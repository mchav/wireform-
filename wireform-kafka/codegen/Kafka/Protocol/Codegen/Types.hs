{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{-|
Module      : Kafka.Protocol.Codegen.Types
Description : Internal types for Kafka protocol code generation
Copyright   : (c) 2025
License     : BSD-3-Clause

Internal representation of Kafka protocol schemas parsed from JSON.
These types mirror the structure of Kafka's protocol definition files.
-}
module Kafka.Protocol.Codegen.Types
  ( -- * Protocol Schema Types
    ProtocolSchema(..)
  , FieldSpec(..)
  , TypeSpec(..)
  , VersionSpec(..)
    -- * Version Utilities
  , parseVersionSpec
  , inVersionRange
  , expandVersionSpec
  ) where

import Data.Aeson
import Data.Int
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Top-level protocol schema for a request or response message.
data ProtocolSchema = ProtocolSchema
  { schemaApiKey          :: !(Maybe Int16)
    -- ^ Numeric API key identifier (optional for headers and internal types)
  , schemaType            :: !Text
    -- ^ "request", "response", or "header"
  , schemaName            :: !Text
    -- ^ Name of the message type (e.g., "MetadataRequest")
  , schemaValidVersions   :: !Text
    -- ^ Version range string (e.g., "0-12", "5+", "3")
  , schemaFlexibleVersions :: !Text
    -- ^ Version range where flexible encoding is used (e.g., "9+")
  , schemaFields          :: ![FieldSpec]
    -- ^ Field specifications for this message
  , schemaCommonStructs   :: ![FieldSpec]
    -- ^ Common struct definitions shared across the message
  , schemaAbout           :: !(Maybe Text)
    -- ^ Documentation comment for this message
  } deriving (Eq, Show, Generic)

instance FromJSON ProtocolSchema where
  parseJSON = withObject "ProtocolSchema" $ \v -> ProtocolSchema
    <$> v .:? "apiKey"
    <*> v .: "type"
    <*> v .: "name"
    <*> v .: "validVersions"
    <*> v .:? "flexibleVersions" .!= "none"
    <*> v .:? "fields" .!= []
    <*> v .:? "commonStructs" .!= []
    <*> v .:? "about"

-- | Specification for a single field in a protocol message.
data FieldSpec = FieldSpec
  { fieldName            :: !Text
    -- ^ Field name (PascalCase in JSON, will be converted to camelCase)
  , fieldType            :: !TypeSpec
    -- ^ Type of the field
  , fieldVersions        :: !Text
    -- ^ Version range where this field is present (e.g., "0+", "4-10")
  , fieldTag             :: !(Maybe Int)
    -- ^ Tag number for tagged fields (in flexible versions)
  , fieldTaggedVersions  :: !(Maybe Text)
    -- ^ Version range where this field is tagged (for flexible messages)
  , fieldNullableVersions :: !(Maybe Text)
    -- ^ Version range where this field can be null
  , fieldFlexibleVersions :: !(Maybe Text)
    -- ^ Per-field flexible-version override. The Kafka spec lets
    -- a field set its own @flexibleVersions@ that supersedes the
    -- message-level value: most commonly @"none"@, used to keep
    -- a specific field non-flexible (i.e. encoded as the
    -- old-style INT16-prefixed string / array) even when the
    -- message body itself is flexible.
    --
    -- The canonical example is the request header's @ClientId@
    -- field: the Kafka spec marks it
    -- @\"flexibleVersions\": \"none\"@ so the broker can parse the
    -- header before knowing which message-body version the
    -- client is using. Without this override the codegen would
    -- emit @toCompactString@ for v2 (the flexible request
    -- header) and brokers would mis-read the header.
  , fieldDefault         :: !(Maybe Value)
    -- ^ Default value for this field (JSON value)
  , fieldIgnorable       :: !Bool
    -- ^ Whether unknown versions of this field can be safely ignored
  , fieldEntityType      :: !(Maybe Text)
    -- ^ Semantic type hint (e.g., "topicName", "groupId")
  , fieldAbout           :: !(Maybe Text)
    -- ^ Documentation comment for this field
  , fieldFields          :: !(Maybe [FieldSpec])
    -- ^ Nested fields (for struct types)
  } deriving (Eq, Show, Generic)

instance FromJSON FieldSpec where
  parseJSON = withObject "FieldSpec" $ \v -> do
    name <- v .: "name"
    -- If "type" is missing but "fields" is present, use the field name as the type
    maybeType <- v .:? "type"
    maybeFields <- v .:? "fields"
    fieldType <- case (maybeType, maybeFields) of
      (Just t, _) -> pure t
      (Nothing, Just _) -> pure $ StructType name
      (Nothing, Nothing) -> fail "Field must have either 'type' or 'fields'"
    -- The Apache Kafka schema set ships @"tag"@ as either a JSON
    -- number (older trunk style) or a numeric string (Kafka 4.0.0+
    -- consistently quotes it). Accept both so the codegen is
    -- agnostic to the upstream encoding choice.
    rawTag <- v .:? "tag"
    tagParsed <- case rawTag of
      Nothing -> pure Nothing
      Just (Number n) -> case toBoundedInteger n :: Maybe Int of
        Just i  -> pure (Just i)
        Nothing -> fail $ "tag is not an Int: " <> show n
      Just (String s) -> case reads (T.unpack s) of
        [(i, "")] -> pure (Just i)
        _         -> fail $ "tag string is not numeric: " <> T.unpack s
      Just other -> fail $ "tag must be a number or numeric string, got: " <> show other
    FieldSpec name fieldType
      <$> v .: "versions"
      <*> pure tagParsed
      <*> v .:? "taggedVersions"
      <*> v .:? "nullableVersions"
      <*> v .:? "flexibleVersions"
      <*> v .:? "default"
      <*> v .:? "ignorable" .!= False
      <*> v .:? "entityType"
      <*> v .:? "about"
      <*> pure maybeFields

-- | Type specification for a field.
data TypeSpec
  = PrimitiveType !Text
    -- ^ Basic types: "bool", "int8", "int16", "int32", "int64", "uint32", 
    -- "string", "bytes", "uuid", "float64"
  | ArrayType !TypeSpec
    -- ^ Array of elements: "[]ElementType"
  | StructType !Text
    -- ^ Named struct type (typically defined inline with nested fields)
  deriving (Eq, Show, Generic)

instance FromJSON TypeSpec where
  parseJSON = withText "TypeSpec" $ \t ->
    case T.unpack t of
      '[':']':rest -> ArrayType <$> parseJSON (String $ T.pack rest)
      _ -> return $ case t of
        "bool"    -> PrimitiveType "bool"
        "int8"    -> PrimitiveType "int8"
        "int16"   -> PrimitiveType "int16"
        "int32"   -> PrimitiveType "int32"
        "int64"   -> PrimitiveType "int64"
        "uint16"  -> PrimitiveType "uint16"
        "uint32"  -> PrimitiveType "uint32"
        "string"  -> PrimitiveType "string"
        "bytes"   -> PrimitiveType "bytes"
        "records" -> PrimitiveType "bytes"  -- records are encoded as bytes
        "uuid"    -> PrimitiveType "uuid"
        "float64" -> PrimitiveType "float64"
        other     -> StructType other

-- | Parsed version specification.
data VersionSpec
  = ExactVersion !Int16
    -- ^ Exact version number (e.g., "5")
  | VersionRange !Int16 !Int16
    -- ^ Version range inclusive (e.g., "0-12")
  | VersionFrom !Int16
    -- ^ Version and above (e.g., "9+")
  | NoVersions
    -- ^ No versions (used for "none")
  deriving (Eq, Show)

-- | Parse a version specification string.
--
-- Examples:
--
-- > parseVersionSpec "5"      == Right (ExactVersion 5)
-- > parseVersionSpec "0-12"   == Right (VersionRange 0 12)
-- > parseVersionSpec "9+"     == Right (VersionFrom 9)
-- > parseVersionSpec "none"   == Right NoVersions
parseVersionSpec :: Text -> Either String VersionSpec
parseVersionSpec t = case T.unpack t of
  "none" -> Right NoVersions
  s | '+' `elem` s ->
      case reads (takeWhile (/= '+') s) of
        [(n, "")] -> Right (VersionFrom n)
        _ -> Left $ "Invalid version spec: " ++ s
    | '-' `elem` s ->
      case break (== '-') s of
        (minS, '-':maxS) ->
          case (reads minS, reads maxS) of
            ([(minV, "")], [(maxV, "")]) -> Right (VersionRange minV maxV)
            _ -> Left $ "Invalid version range: " ++ s
        _ -> Left $ "Invalid version range: " ++ s
    | otherwise ->
      case reads s of
        [(n, "")] -> Right (ExactVersion n)
        _ -> Left $ "Invalid version spec: " ++ s

-- | Check if a version is within a version specification.
inVersionRange :: Int16 -> VersionSpec -> Bool
inVersionRange _ NoVersions = False
inVersionRange v (ExactVersion n) = v == n
inVersionRange v (VersionRange minV maxV) = v >= minV && v <= maxV
inVersionRange v (VersionFrom minV) = v >= minV

-- | Expand a version specification to a list of concrete versions.
-- For open-ended specs (VersionFrom), uses a reasonable maximum (20).
expandVersionSpec :: VersionSpec -> [Int16]
expandVersionSpec NoVersions = []
expandVersionSpec (ExactVersion n) = [n]
expandVersionSpec (VersionRange minV maxV) = [minV..maxV]
expandVersionSpec (VersionFrom minV) = [minV..20]  -- Reasonable upper bound

