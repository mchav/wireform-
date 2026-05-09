{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Protocol.Codegen.Generator
Description : Generate Haskell code from Kafka protocol definitions
Copyright   : (c) 2025
License     : BSD-3-Clause

This module generates Haskell source code from parsed Kafka protocol schemas.
It uses the prettyprinter library to produce human-readable, well-formatted code
with proper Haddock documentation.

Generated code includes:

* Data type definitions with strict fields
* Version-aware encode/decode functions with range-based dispatch
* Support for flexible message formats and tagged fields
* Comprehensive Haddock documentation extracted from protocol comments
* Proper handling of nullable fields and defaults
-}
module Kafka.Protocol.Codegen.Generator
  ( -- * Code Generation
    generateMessageModule
  , generateMessage
  , generateModuleHeader
    -- * Field Generation
  , generateDataType
  , generateEncodeFunction
  , generateDecodeFunction
    -- * Naming
  , toHaskellTypeName
  , toHaskellFieldName
  , toHaskellModuleName
    -- * Inventory Generation
  , generateMessageInventory
  ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (isUpper, toLower, toUpper)
import Data.Int
import Data.List (intercalate, nub, sort, groupBy, sortBy)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Kafka.Protocol.Codegen.Types
import Prettyprinter
import Prettyprinter.Render.Text

-- | Generate a complete Haskell module for a protocol message.
generateMessageModule :: ProtocolSchema -> Doc ann
generateMessageModule schema =
  let 
    -- Collect all type names that need to be exported (recursively)
    nestedTypeNames = concatMap (getNestedTypeNames (schemaName schema)) (schemaFields schema)
    -- Also recursively collect types from common structs
    commonTypeNames = concatMap (\field -> 
      case fieldType field of
        StructType name -> name : case fieldFields field of
          Just nestedFields -> concatMap (getNestedTypeNames name) nestedFields
          Nothing -> []
        _ -> []) (schemaCommonStructs schema)
    allTypes = nub $ filter (not . T.null) $ [schemaName schema] ++ nestedTypeNames ++ commonTypeNames
    typeExports = map (\t -> pretty t <> "(..)") allTypes
    funcExports = ["encode" <> pretty (schemaName schema), "decode" <> pretty (schemaName schema)]
    -- Add max version export
    maxVersionExport = "max" <> pretty (schemaName schema) <> "Version"
    allExports = typeExports ++ funcExports ++ [maxVersionExport]
    exportList = vsep $ punctuate "," allExports
  in
  vsep
    [ generateModuleHeader schema
    , ""
    , "module" <+> pretty (toHaskellModuleName $ schemaName schema)
    , indent 2 "("
    , indent 4 exportList
    , indent 2 ") where"
    , ""
    , generateImports
    , ""
    , generateMessage schema
    ]

-- | Get nested type names from a field spec (recursively)
getNestedTypeNames :: Text -> FieldSpec -> [Text]
getNestedTypeNames _ field =
  case fieldType field of
    ArrayType (StructType name) -> 
      -- Include this type and recursively get nested types from its fields
      name : case fieldFields field of
        Just nestedFields -> concatMap (getNestedTypeNames name) nestedFields
        Nothing -> []
    StructType name -> 
      -- Include this type and recursively get nested types from its fields
      name : case fieldFields field of
        Just nestedFields -> concatMap (getNestedTypeNames name) nestedFields
        Nothing -> []
    _ -> []

-- | Generate the module header with language extensions and documentation.
generateModuleHeader :: ProtocolSchema -> Doc ann
generateModuleHeader schema =
  vsep
    [ "{-# LANGUAGE DeriveGeneric #-}"
    , "{-# LANGUAGE StrictData #-}"
    , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , ""
    , "{-|"
    , "Module      :" <+> pretty (toHaskellModuleName $ schemaName schema)
    , "Description : Kafka" <+> pretty (schemaName schema) <+> "message"
    , "Copyright   : (c) 2025"
    , "License     : BSD-3-Clause"
    , ""
    , case schemaApiKey schema of
        Just apiKey -> "Kafka" <+> pretty (schemaType schema) <+> "for API key" <+> pretty apiKey <> "."
        Nothing -> "Kafka" <+> pretty (schemaType schema) <+> "(no API key)."
    , ""
    , case schemaAbout schema of
        Just about -> vsep (map (("  " <>) . pretty) (T.lines about))
        Nothing -> ""
    , ""
    , "Valid versions:" <+> pretty (schemaValidVersions schema)
    , "Flexible versions:" <+> pretty (schemaFlexibleVersions schema)
    , ""
    , "This code is auto-generated from Kafka protocol definitions."
    , "-}"
    ]

-- | Generate import statements needed for the generated code.
-- Uses qualified imports to avoid naming conflicts.
generateImports :: Doc ann
generateImports =
  vsep
    [ "import Control.Monad (when)"
    , "import Data.Bytes.Get (MonadGet)"
    , "import Data.Bytes.Put (MonadPut)"
    , "import Data.Bytes.Serial (Serial(..), serialize, deserialize)"
    , "import Data.Int (Int8, Int16, Int32, Int64)"
    , "import Data.Word (Word16, Word32)"
    , "import GHC.Generics (Generic)"
    , "import qualified Data.Vector as V"
    , "import qualified Data.ByteString as BS"
    , "import qualified Kafka.Protocol.Primitives as P"
    , "import Kafka.Protocol.Primitives"
    , "  ( VarInt(..), VarLong(..), UVarInt(..)"
    , "  , KafkaString, KafkaBytes, KafkaArray, KafkaUuid"
    , "  , CompactString, CompactBytes, CompactArray"
    , "  , TaggedFields, emptyTaggedFields, Nullable(..)"
    , "  , toCompactString, toCompactBytes, toCompactArray"
    , "  )"
    , "import qualified Kafka.Protocol.Encoding as E"
    , "import Kafka.Protocol.Message (KafkaMessage(..))"
    ]

-- | Generate code for a complete message (data type + encode/decode functions).
generateMessage :: ProtocolSchema -> Doc ann
generateMessage schema =
  let 
    flexibleVersion = case parseVersionSpec (schemaFlexibleVersions schema) of
      Right (VersionFrom v) -> Just v
      Right (VersionRange minV _) -> Just minV  -- Use minimum version as threshold
      Right (ExactVersion v) -> Just v
      _ -> Nothing
    
    validVersions = parseVersionSpec (schemaValidVersions schema)
    
    -- Calculate max version
    maxVersion = case validVersions of
      Right (VersionFrom v) -> Just 20  -- Use reasonable upper bound for open-ended
      Right (VersionRange _ v) -> Just v
      Right (ExactVersion v) -> Just v
      _ -> Nothing
    
    -- Generate nested types from fields
    nestedTypes = concatMap (generateNestedTypes (schemaName schema) flexibleVersion) (schemaFields schema)
    
    -- Generate common struct types
    commonTypes = concatMap (generateCommonStruct flexibleVersion) (schemaCommonStructs schema)
  in
  vsep
    [ -- Generate common struct types
      vsep (map (<> line) commonTypes)
    , -- Generate nested structure types
      vsep (map (<> line) nestedTypes)
    , generateDataType (schemaName schema) (schemaFields schema) (schemaAbout schema)
    , ""
    , generateMaxVersionConstant (schemaName schema) maxVersion
    , ""
    , generateKafkaMessageInstance schema
    , ""
    , generateEncodeFunction schema flexibleVersion validVersions
    , ""
    , generateDecodeFunction schema flexibleVersion validVersions
    ]

-- | Check if a structure has version-dependent fields (fields not present in all versions).
hasVersionDependentFields :: [FieldSpec] -> Bool
hasVersionDependentFields fields = any (not . isAlwaysPresent) fields
  where
    isAlwaysPresent :: FieldSpec -> Bool
    isAlwaysPresent field = 
      case parseVersionSpec (fieldVersions field) of
        Right (VersionFrom 0) -> True  -- "0+" means always present
        _ -> False  -- Any other version spec means version-dependent

-- | Check if a field is a tagged field (appears in tagged fields section for given version)
isTaggedField :: Int16 -> FieldSpec -> Bool
isTaggedField version field =
  case (fieldTag field, fieldTaggedVersions field) of
    (Just _, Just taggedVers) ->
      -- This field has a tag and tagged versions - check if this version is in range
      case parseVersionSpec taggedVers of
        Right spec -> inVersionRange version spec
        Left _ -> False
    _ -> False

-- | Generate a common struct type definition.
-- Common structs are top-level struct definitions in the schema.
generateCommonStruct :: Maybe Int16 -> FieldSpec -> [Doc ann]
generateCommonStruct flexibleVersion field =
  case (fieldType field, fieldFields field) of
    (StructType structName, Just nestedFields) ->
      let structDoc = generateDataType structName nestedFields (fieldAbout field)
          -- Never generate Serial instances for nested structures - they're always version-aware
          -- Generate version-aware encode/decode functions instead
          encodeFn = generateNestedEncodeFunction structName nestedFields flexibleVersion
          decodeFn = generateNestedDecodeFunction structName nestedFields flexibleVersion
          -- Recursively generate nested types within this struct
          deeperNested = concatMap (generateNestedTypes structName flexibleVersion) nestedFields
      in deeperNested ++ [structDoc, encodeFn, decodeFn]
    _ -> []

-- | Generate nested structure types from inline field definitions
generateNestedTypes :: Text -> Maybe Int16 -> FieldSpec -> [Doc ann]
generateNestedTypes parentName flexibleVersion field = 
  case fieldType field of
    ArrayType (StructType structName) ->
      case fieldFields field of
        Just nestedFields ->
          let structDoc = generateDataType structName nestedFields (fieldAbout field)
              -- Never generate Serial instances for nested structures - always use version-aware functions
              encodeFn = generateNestedEncodeFunction structName nestedFields flexibleVersion
              decodeFn = generateNestedDecodeFunction structName nestedFields flexibleVersion
              -- Recursively generate nested types within this struct
              deeperNested = concatMap (generateNestedTypes structName flexibleVersion) nestedFields
          in deeperNested ++ [structDoc, encodeFn, decodeFn]
        Nothing -> []
    StructType structName ->
      case fieldFields field of
        Just nestedFields ->
          let structDoc = generateDataType structName nestedFields (fieldAbout field)
              -- Never generate Serial instances for nested structures - always use version-aware functions
              encodeFn = generateNestedEncodeFunction structName nestedFields flexibleVersion
              decodeFn = generateNestedDecodeFunction structName nestedFields flexibleVersion
              deeperNested = concatMap (generateNestedTypes structName flexibleVersion) nestedFields
          in deeperNested ++ [structDoc, encodeFn, decodeFn]
        Nothing -> []
    _ -> []

-- | Generate a constant for the maximum supported version.
generateMaxVersionConstant :: Text -> Maybe Int16 -> Doc ann
generateMaxVersionConstant typeName maxVer =
  let constantName = "max" <> pretty typeName <> "Version"
      versionValue = case maxVer of
        Just v -> pretty v
        Nothing -> "-1 -- No valid versions"
  in vsep
    [ "-- | Maximum supported version for" <+> pretty typeName <> "."
    , constantName <+> ":: Int16"
    , constantName <+> "=" <+> versionValue
    ]

-- | Generate a KafkaMessage instance for messages with API keys.
-- Messages without an API key (e.g., headers, internal types) won't have an instance.
generateKafkaMessageInstance :: ProtocolSchema -> Doc ann
generateKafkaMessageInstance schema =
  case schemaApiKey schema of
    Nothing -> mempty  -- No instance for messages without API keys
    Just apiKey ->
      let typeName = schemaName schema
          validVersions = parseVersionSpec (schemaValidVersions schema)
          flexibleVersions = parseVersionSpec (schemaFlexibleVersions schema)
          
          -- Calculate min version
          minVer = case validVersions of
            Right (ExactVersion v) -> v
            Right (VersionRange v _) -> v
            Right (VersionFrom v) -> v
            _ -> 0
          
          -- Calculate max version  
          maxVer = case validVersions of
            Right (ExactVersion v) -> v
            Right (VersionRange _ v) -> v
            Right (VersionFrom _) -> 20  -- Use reasonable upper bound for open-ended
            _ -> 0
          
          -- Calculate first flexible version
          flexVer = case flexibleVersions of
            Right (ExactVersion v) -> "Just" <+> pretty v
            Right (VersionRange v _) -> "Just" <+> pretty v
            Right (VersionFrom v) -> "Just" <+> pretty v
            _ -> "Nothing"
          
      in vsep
        [ "-- | KafkaMessage instance for" <+> pretty typeName <> "."
        , "instance KafkaMessage" <+> pretty typeName <+> "where"
        , indent 2 $ "messageApiKey = " <> pretty apiKey
        , indent 2 $ "messageMinVersion = " <> pretty minVer
        , indent 2 $ "messageMaxVersion = " <> pretty maxVer
        , indent 2 $ "messageFlexibleVersion = " <> flexVer
        ]

-- | Generate a Haskell data type from field specifications.
generateDataType :: Text -> [FieldSpec] -> Maybe Text -> Doc ann
generateDataType typeName fields maybeDoc =
  vsep $
    [ case maybeDoc of
        Just doc -> vsep
          [ "-- |" <+> pretty (T.take 200 $ T.replace "\n" " " doc)
          ]
        Nothing -> ""
    , "data" <+> pretty typeName <+> "=" <+> pretty typeName
    , indent 2 "{"
    ]
    ++ punctuate (line <> ",") (map generateField fields)
    ++
    [ line <> indent 2 "}"
    , indent 2 "deriving (Eq, Show, Generic)"
    ]
  where
    generateField :: FieldSpec -> Doc ann
    generateField field =
      let fieldDoc = case fieldAbout field of
            Just doc -> line <> indent 2 ("-- |" <+> pretty (T.take 100 $ T.replace "\n" " " doc))
            Nothing -> mempty
          fieldVersionInfo = line <> indent 2 ("-- Versions:" <+> pretty (fieldVersions field))
      in vsep
        [ fieldDoc
        , fieldVersionInfo
        , indent 2 $ pretty (toHaskellFieldName typeName (fieldName field))
            <+> ":: !" <> parens (pretty (fieldToHaskellType field))
        ]

-- | Generate a Serial instance for a nested struct type.
generateSerialInstance :: Text -> [FieldSpec] -> Doc ann
generateSerialInstance typeName fields =
  let serializeFields = map (\f -> "serialize" <+> parens (pretty (toHaskellFieldName typeName (fieldName f)) <+> "x")) fields
      deserializeFields = map (\f -> 
        let var = makeLowerFieldVar (fieldName f)
        in pretty var <+> "<- deserialize") fields
      recordFields = map (\f ->
        let fieldRec = toHaskellFieldName typeName (fieldName f)
            fieldVar = makeLowerFieldVar (fieldName f)
        in pretty fieldRec <+> "=" <+> pretty fieldVar) fields
  in vsep
    [ "instance Serial" <+> pretty typeName <+> "where"
    , indent 2 "serialize x = do"
    , indent 4 $ vsep serializeFields
    , indent 2 "deserialize = do"
    , indent 4 $ vsep deserializeFields
    , indent 4 $ "pure" <+> pretty typeName
    , indent 6 "{"
    , indent 6 $ vsep $ punctuate (line <> ",") recordFields
    , indent 6 "}"
    , ""
    ]
  where
    makeLowerFieldVar :: Text -> Text
    makeLowerFieldVar fname = 
      let base = T.pack $ map toLower $ T.unpack fname
      in "field" <> base

-- | Generate a version-aware encode function for a nested struct with version-dependent fields.
generateNestedEncodeFunction :: Text -> [FieldSpec] -> Maybe Int16 -> Doc ann
generateNestedEncodeFunction typeName fields flexibleVersion =
  let lowerTypeName = T.toLower $ T.take 1 typeName
      funName = "encode" <> pretty typeName
      varName = lowerTypeName <> "msg"
      -- Check if version is actually used (for nested structs or version conditions)
      versionUsed = any (needsVersionForEncode) fields || isJust flexibleVersion
      versionPattern :: Text = if versionUsed then "version" else "_version"
      -- Generate conditional encoding for each field based on its version range
      generateFieldEncodeConditional :: FieldSpec -> Doc ann
      generateFieldEncodeConditional field =
        let accessor = parens $ pretty (toHaskellFieldName typeName (fieldName field)) <+> pretty varName
            isNullable = isJust (fieldNullableVersions field)
            -- Check if this field should use flexible encoding based on version
            -- We need to generate code that checks at runtime if version >= flexibleVersion
            encodeExpr = case flexibleVersion of
              Just flexVer -> 
                -- Generate conditional code: if version >= flexVer, use compact format
                generateTypeEncodeVersionAware flexVer field accessor isNullable flexibleVersion
              Nothing ->
                -- No flexible versions, always use non-compact
                generateTypeEncode False field accessor isNullable flexibleVersion
            condition = generateVersionCondition (fieldVersions field)
        in case condition of
          Just cond -> vsep
            [ "when" <+> parens cond <+> "$"
            , indent 2 encodeExpr
            ]
          Nothing -> encodeExpr
      encodeStmts = map generateFieldEncodeConditional fields
      -- For flexible versions, nested structures need to write tagged fields
      tagFieldStmt = case flexibleVersion of
        Just flexVer ->
          ["when" <+> parens ("version >=" <+> pretty flexVer) <+> "$ serialize (emptyTaggedFields :: TaggedFields)"]
        Nothing -> []
      allStmts = encodeStmts ++ tagFieldStmt
  in vsep
    [ ""
    , "-- | Encode" <+> pretty typeName <+> "with version-aware field handling."
    , funName <+> ":: MonadPut m => E.ApiVersion ->" <+> pretty typeName <+> "-> m ()"
    , funName <+> pretty versionPattern <+> pretty varName <+> "="
    , indent 2 "do"
    , indent 4 $ vsep allStmts
    , ""
    ]
  where
    generateVersionCondition :: Text -> Maybe (Doc ann)
    generateVersionCondition versionSpec =
      case parseVersionSpec versionSpec of
        Right (VersionFrom 0) -> Nothing  -- Always present, no condition needed
        Right (VersionFrom v) -> Just $ "version >=" <+> pretty v
        Right (VersionRange minV maxV) -> Just $ "version >=" <+> pretty minV <+> "&&" <+> "version <=" <+> pretty maxV
        Right (ExactVersion v) -> Just $ "version ==" <+> pretty v
        _ -> Nothing
    needsVersionForEncode :: FieldSpec -> Bool
    needsVersionForEncode field =
      case fieldType field of
        StructType _ -> True  -- Nested structs need version
        ArrayType (StructType _) -> True  -- Arrays of structs need version
        _ -> case parseVersionSpec (fieldVersions field) of
          Right (VersionFrom 0) -> False  -- Always present, no version check needed
          Right _ -> True  -- Version-dependent field
          Left _ -> False

-- | Generate a version-aware decode function for a nested struct with version-dependent fields.
generateNestedDecodeFunction :: Text -> [FieldSpec] -> Maybe Int16 -> Doc ann
generateNestedDecodeFunction typeName fields flexibleVersion =
  let funName = "decode" <> pretty typeName
      -- Generate conditional decoding for each field based on its version range
      generateFieldDecodeConditional :: FieldSpec -> Doc ann
      generateFieldDecodeConditional field =
        let var = makeLowerFieldVar (fieldName field)
            -- Use version-aware decode expression for fields in flexible contexts
            decodeExpr = case flexibleVersion of
              Just flexVer ->
                generateFieldDecodeExprVersionAware flexVer field flexibleVersion
              Nothing ->
                generateFieldDecodeExpr field flexibleVersion
            condition = generateVersionCondition (fieldVersions field)
        in case condition of
          Just cond -> vsep
            [ indent 4 $ pretty var <+> "<- if" <+> cond
            , indent 6 $ "then" <+> decodeExpr
            , indent 6 $ "else pure" <+> parens (generateFieldDefault field)
            ]
          Nothing -> indent 4 $ pretty var <+> "<-" <+> decodeExpr
      decodeStmts = map generateFieldDecodeConditional fields
      recordFields = map (\f ->
        let fieldRec = toHaskellFieldName typeName (fieldName f)
            fieldVar = makeLowerFieldVar (fieldName f)
        in pretty fieldRec <+> "=" <+> pretty fieldVar) fields
      -- Check if version is actually used (for nested structs or version conditions)
      versionUsed = any (needsVersionForDecode) fields || isJust flexibleVersion
      versionPattern :: Text = if versionUsed then "version" else "_version"
      -- For flexible versions, nested structures need to read tagged fields
      tagFieldStmt = case flexibleVersion of
        Just flexVer ->
          [indent 4 $ "_ <- if version >=" <+> pretty flexVer <+> "then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields"]
        Nothing -> []
      allStmts = decodeStmts ++ tagFieldStmt
  in vsep
    [ "-- | Decode" <+> pretty typeName <+> "with version-aware field handling."
    , funName <+> ":: MonadGet m => E.ApiVersion -> m" <+> pretty typeName
    , funName <+> pretty versionPattern <+> "="
    , indent 2 "do"
    , vsep allStmts
    , indent 4 $ "pure" <+> pretty typeName
    , indent 6 "{"
    , indent 6 $ vsep $ punctuate (line <> ",") recordFields
    , indent 6 "}"
    , ""
    ]
  where
    makeLowerFieldVar :: Text -> Text
    makeLowerFieldVar fname = 
      let base = T.pack $ map toLower $ T.unpack fname
      in "field" <> base
    generateVersionCondition :: Text -> Maybe (Doc ann)
    generateVersionCondition versionSpec =
      case parseVersionSpec versionSpec of
        Right (VersionFrom 0) -> Nothing  -- Always present, no condition needed
        Right (VersionFrom v) -> Just $ "version >=" <+> pretty v
        Right (VersionRange minV maxV) -> Just $ "version >=" <+> pretty minV <+> "&&" <+> "version <=" <+> pretty maxV
        Right (ExactVersion v) -> Just $ "version ==" <+> pretty v
        _ -> Nothing
    needsVersionForDecode :: FieldSpec -> Bool
    needsVersionForDecode field =
      case fieldType field of
        StructType _ -> True  -- Nested structs need version
        ArrayType (StructType _) -> True  -- Arrays of structs need version
        _ -> case parseVersionSpec (fieldVersions field) of
          Right (VersionFrom 0) -> False  -- Always present, no version check needed
          Right _ -> True  -- Version-dependent field
          Left _ -> False

-- | Convert a field spec to a Haskell type, considering nullable versions.
fieldToHaskellType :: FieldSpec -> Text
fieldToHaskellType field =
  let baseType = typeSpecToHaskellType (fieldType field) (fieldFields field)
      isNullable = isJust (fieldNullableVersions field)
      -- Don't wrap already-nullable types (KafkaString, KafkaBytes, KafkaArray)
      -- These types already handle null encoding internally
      alreadyNullable = case fieldType field of
        PrimitiveType "string" -> True
        PrimitiveType "bytes" -> True
        ArrayType _ -> True
        _ -> False
  in if isNullable && not alreadyNullable
       then "Nullable (" <> baseType <> ")"
       else baseType

-- | Convert a TypeSpec to the base Haskell type.
typeSpecToHaskellType :: TypeSpec -> Maybe [FieldSpec] -> Text
typeSpecToHaskellType (PrimitiveType "bool") _ = "Bool"
typeSpecToHaskellType (PrimitiveType "int8") _ = "Int8"
typeSpecToHaskellType (PrimitiveType "int16") _ = "Int16"
typeSpecToHaskellType (PrimitiveType "int32") _ = "Int32"
typeSpecToHaskellType (PrimitiveType "int64") _ = "Int64"
typeSpecToHaskellType (PrimitiveType "uint16") _ = "Word16"
typeSpecToHaskellType (PrimitiveType "uint32") _ = "Word32"
typeSpecToHaskellType (PrimitiveType "string") _ = "KafkaString"
typeSpecToHaskellType (PrimitiveType "bytes") _ = "KafkaBytes"
typeSpecToHaskellType (PrimitiveType "uuid") _ = "KafkaUuid"
typeSpecToHaskellType (PrimitiveType "float64") _ = "Double"
typeSpecToHaskellType (PrimitiveType other) _ = other
typeSpecToHaskellType (ArrayType elemType) fields = 
  "KafkaArray (" <> typeSpecToHaskellType elemType fields <> ")"
typeSpecToHaskellType (StructType name) _ = name

-- | Group fields by their version ranges for efficient encoding.
data FieldGroup = FieldGroup
  { fgMinVersion :: Int16
  , fgMaxVersion :: Maybe Int16  -- Nothing means "to end"
  , fgFields :: [FieldSpec]
  } deriving (Show)

-- | Parse field version ranges and group them.
groupFieldsByVersion :: [FieldSpec] -> [FieldGroup]
groupFieldsByVersion fields =
  let fieldWithVersions = map parseFieldVersions fields
  in buildGroups fieldWithVersions
  where
    parseFieldVersions :: FieldSpec -> (FieldSpec, VersionSpec)
    parseFieldVersions f = 
      case parseVersionSpec (fieldVersions f) of
        Right spec -> (f, spec)
        Left _ -> (f, NoVersions)
    
    buildGroups :: [(FieldSpec, VersionSpec)] -> [FieldGroup]
    buildGroups [] = []
    buildGroups fvs = 
      -- Simplified: just create groups based on min version
      let sorted = sortBy (comparing (minVer . snd)) fvs
          grouped = groupBy (\a b -> minVer (snd a) == minVer (snd b)) sorted
      in map makeGroup grouped
    
    makeGroup :: [(FieldSpec, VersionSpec)] -> FieldGroup
    makeGroup [] = FieldGroup 0 Nothing []
    makeGroup fvs@((_, spec):_) = FieldGroup (minVer spec) (maxVer spec) (map fst fvs)
    
    minVer :: VersionSpec -> Int16
    minVer (ExactVersion v) = v
    minVer (VersionRange minV _) = minV
    minVer (VersionFrom minV) = minV
    minVer NoVersions = 0
    
    maxVer :: VersionSpec -> Maybe Int16
    maxVer (ExactVersion v) = Just v
    maxVer (VersionRange _ maxV) = Just maxV
    maxVer (VersionFrom _) = Nothing
    maxVer NoVersions = Nothing

-- | Generate encoding function with version dispatch.
generateEncodeFunction :: ProtocolSchema -> Maybe Int16 -> Either String VersionSpec -> Doc ann
generateEncodeFunction schema flexibleVersion validVersions =
  let typeName = schemaName schema
      functionName = "encode" <> pretty typeName
      versions = case validVersions of
        Right spec -> expandVersionSpec spec
        Left _ -> []
      guards = generateVersionDispatch versions flexibleVersion "encode" typeName (schemaFields schema)
      hasValidVersions = not (null versions)
  in vsep
    [ "-- | Encode" <+> pretty typeName <+> "with the given API version."
    , functionName <+> ":: MonadPut m => E.ApiVersion ->" <+> pretty typeName <+> "-> m ()"
    , functionName <+> "version msg"
    , vsep guards
    , if hasValidVersions 
        then "  | otherwise = error $ \"Unsupported version: \" ++ show version"
        else mempty
    ]

-- | Generate version dispatch for encode/decode functions.
-- Groups versions by their field sets rather than just consecutive numbers.
generateVersionDispatch :: [Int16] -> Maybe Int16 -> Text -> Text -> [FieldSpec] -> [Doc ann]
generateVersionDispatch [] _ funcType _ _ = 
  if funcType == "encode"
    then ["  = error \"No valid versions\""]
    else ["  = fail \"No valid versions\""]
generateVersionDispatch versions flexibleVer funcType typeName fields =
  let ranges = groupVersionsByFieldSet versions fields flexibleVer
  in punctuate line $ map (generateVersionCase flexibleVer funcType typeName fields) ranges

-- | Group versions by which fields are present, not just consecutive numbers.
-- This ensures each version range has the same set of fields available.
-- Sorts ranges so that more specific (smaller) ranges come first to avoid overlaps.
-- IMPORTANT: Also splits ranges at flexible version boundaries to ensure proper TaggedFields handling.
groupVersionsByFieldSet :: [Int16] -> [FieldSpec] -> Maybe Int16 -> [(Int16, Int16)]
groupVersionsByFieldSet [] _ _ = []
groupVersionsByFieldSet versions fields flexibleVer =
  let -- For each version, compute which fields are present AND whether it's flexible
      -- We need to track flexible vs non-flexible as part of the "field set"
      versionsWithFields = [(v, (fieldsInVersion v, isVersionFlexible v)) | v <- versions]
      -- Sort by field set (including flexibility), then by version
      sorted = sortBy (comparing snd <> comparing fst) versionsWithFields
      -- Group by field set (including flexibility)
      grouped = groupBy (\(_, f1) (_, f2) -> f1 == f2) sorted
      -- Convert each group to a version range (only consecutive versions)
      toRanges :: [(Int16, ([Text], Bool))] -> [(Int16, Int16)]
      toRanges [] = []
      toRanges grp = 
        let vs = map fst grp
            consecutiveRanges = groupConsecutive vs
        in consecutiveRanges
      
      groupConsecutive :: [Int16] -> [(Int16, Int16)]
      groupConsecutive [] = []
      groupConsecutive (v:vs) = go v v vs
        where
          go start end [] = [(start, end)]
          go start end (x:xs)
            | x == end + 1 = go start x xs
            | otherwise = (start, end) : go x x xs
      
      ranges = concatMap toRanges grouped
      -- Sort ranges: exact versions first, then by range size (smaller first), then by min version
      sortedRanges = sortBy compareRanges ranges
      
      compareRanges :: (Int16, Int16) -> (Int16, Int16) -> Ordering
      compareRanges (min1, max1) (min2, max2)
        -- Exact versions (min == max) come first
        | min1 == max1 && min2 /= max2 = LT
        | min1 /= max1 && min2 == max2 = GT
        -- Among ranges, smaller ranges come first
        | size1 /= size2 = compare size1 size2
        -- For same size, earlier versions come first
        | otherwise = compare min1 min2
        where
          size1 = max1 - min1
          size2 = max2 - min2
  in sortedRanges
  where
    fieldsInVersion :: Int16 -> [Text]
    fieldsInVersion v = 
      [fieldName f | f <- fields, fieldInVersionRange v v f]
    
    -- Check if a version is flexible (requires tagged fields)
    isVersionFlexible :: Int16 -> Bool
    isVersionFlexible v = maybe False (<= v) flexibleVer

-- | Group consecutive version numbers into ranges (OLD - keeping for reference).
groupConsecutiveVersions :: [Int16] -> [(Int16, Int16)]
groupConsecutiveVersions [] = []
groupConsecutiveVersions (v:vs) = go v v vs
  where
    go start end [] = [(start, end)]
    go start end (x:xs)
      | x == end + 1 = go start x xs
      | otherwise = (start, end) : go x x xs

-- | Generate a version case for encoding/decoding.
generateVersionCase :: Maybe Int16 -> Text -> Text -> [FieldSpec] -> (Int16, Int16) -> Doc ann
generateVersionCase flexibleVer funcType typeName fields (minV, maxV)
  | minV == maxV = 
      let guard = "  | version ==" <+> pretty minV <+> "="
          body = generateVersionBody flexibleVer funcType typeName fields minV maxV
      in vsep [guard, indent 4 body]
  | otherwise = 
      let guard = "  | version >=" <+> pretty minV <+> "&& version <=" <+> pretty maxV <+> "="
          body = generateVersionBody flexibleVer funcType typeName fields minV maxV
      in vsep [guard, indent 4 body]

-- | Generate the body of a version-specific encode/decode function.
generateVersionBody :: Maybe Int16 -> Text -> Text -> [FieldSpec] -> Int16 -> Int16 -> Doc ann
generateVersionBody flexibleVer "encode" typeName fields minV maxV =
  let isFlexible = maybe False (<= minV) flexibleVer
      -- Filter out fields that are tagged in this version range
      -- Tagged fields go into the TaggedFields structure, not regular field encoding
      regularFields = filter (\f -> fieldInVersionRange minV maxV f && not (isTaggedField minV f)) fields
      hasFields = not (null regularFields)
      fieldEncodes = map (\f -> generateFieldEncode typeName isFlexible f flexibleVer) regularFields
  in if hasFields || isFlexible
     then vsep 
       [ "do"
       , indent 2 $ vsep fieldEncodes
       , if isFlexible
           then indent 2 "serialize (emptyTaggedFields :: TaggedFields)"
           else mempty
       ]
     else "pure ()"

generateVersionBody flexibleVer "decode" typeName fields minV maxV =
  let isFlexible = maybe False (<= minV) flexibleVer
      -- Filter out fields that are tagged in this version range
      -- Tagged fields are decoded from the TaggedFields structure
      regularFieldsInVersion = filter (\f -> fieldInVersionRange minV maxV f && not (isTaggedField minV f)) fields
      allFields = fields  -- Need to include all fields for record construction
      hasFields = not (null regularFieldsInVersion)
      -- Generate lowercase variable names for pattern matching
      decodeStmts = map (\f -> 
        let var = makeLowerFieldVar (fieldName f)
            -- Use version-aware decode for fields in flexible version ranges
            decodeExpr = case flexibleVer of
              Just flexVer | minV >= flexVer -> 
                -- This version range is in flexible territory, use version-aware decode
                generateFieldDecodeExprVersionAware flexVer f flexibleVer
              _ -> 
                -- Non-flexible range, use regular decode
                generateFieldDecodeExpr f flexibleVer
        in indent 2 $ pretty var <+> "<-" <+> decodeExpr) regularFieldsInVersion
      -- Map fields to their variables
      fieldVarMap = map (\f -> (f, makeLowerFieldVar (fieldName f))) regularFieldsInVersion
      recordFields = map (\f ->
        let fieldRec = toHaskellFieldName typeName (fieldName f)
            -- Find the variable for this field if it was decoded
            fieldVal = case lookup f fieldVarMap of
              Just var -> pretty var
              Nothing -> generateFieldDefault f
        in pretty fieldRec <+> "=" <+> fieldVal
        ) allFields
      tagFieldStmt = if isFlexible
                     then [indent 2 "_ <- (deserialize :: MonadGet m => m TaggedFields)"]
                     else []
      allStmts = decodeStmts ++ tagFieldStmt
  in vsep
    [ "do"
    , vsep allStmts
    , indent 2 $ "pure" <+> pretty typeName
    , indent 4 "{"
    , indent 4 $ vsep $ punctuate (line <> ",") recordFields
    , indent 4 "}"
    ]
  where
    -- Helper to create a lowercase variable name from a field name
    makeLowerFieldVar :: Text -> Text
    makeLowerFieldVar fname = 
      let base = T.pack $ map toLower $ T.unpack fname
      in "field" <> base
    
    -- Helper for lookup that compares fields by name
    lookup :: FieldSpec -> [(FieldSpec, Text)] -> Maybe Text
    lookup target pairs = 
      case filter (\(f, _) -> fieldName f == fieldName target) pairs of
        ((_, v):_) -> Just v
        [] -> Nothing

-- | Check if a field is present in a given version range.
-- | Check if a field is present in ALL versions in the given range.
-- A field should only be serialized/deserialized if it's present in every version of the range.
fieldInVersionRange :: Int16 -> Int16 -> FieldSpec -> Bool
fieldInVersionRange minV maxV field =
  case parseVersionSpec (fieldVersions field) of
    Right spec -> all (\v -> inVersionRange v spec) [minV..maxV]
    Left _ -> False

-- | Generate decoding expression for a field, with version-aware handling for complex types.
-- | Whether a field opts /out/ of flexible (compact-string /
-- compact-bytes / compact-array) encoding via a per-field
-- @flexibleVersions@ override. The Kafka spec lets each field
-- carry its own @flexibleVersions@ that supersedes the
-- message-level value; the canonical example is the request
-- header's @ClientId@ field, which is marked
-- @"flexibleVersions": "none"@ so it stays as the old-style
-- INT16-prefixed string even when the request header itself is
-- v2 (flexible).
--
-- Only @"none"@ is honoured here. The (rarely used) variant of
-- a field setting its own @"X+"@ threshold isn't seen in the
-- 3.7 protocol surface and would require a runtime check rather
-- than a compile-time decision; if it ever shows up we can
-- thread @msgVersion@ in the same way 'generateTypeEncodeVersionAware'
-- already does.
fieldOptsOutOfFlexible :: FieldSpec -> Bool
fieldOptsOutOfFlexible f = fieldFlexibleVersions f == Just "none"

-- | Generate version-aware decode expression for a field in a flexible context.
-- This generates conditional code that checks at runtime if version >= flexibleVer.
-- Note: Only strings and bytes need version-aware decoding, not arrays or other primitives.
generateFieldDecodeExprVersionAware :: Int16 -> FieldSpec -> Maybe Int16 -> Doc ann
generateFieldDecodeExprVersionAware flexibleVer field flexibleVersion
  -- Per-field override: this field stays non-compact even on
  -- flexible message versions, so just use the regular decoder.
  | fieldOptsOutOfFlexible field =
      generateFieldDecodeExpr field flexibleVersion
generateFieldDecodeExprVersionAware flexibleVer field flexibleVersion =
  case fieldType field of
    PrimitiveType "string" ->
      "if version >=" <+> pretty flexibleVer <+>
      "then P.fromCompactString <$> deserialize " <>
      "else deserialize"
    PrimitiveType "bytes" ->
      "if version >=" <+> pretty flexibleVer <+>
      "then P.fromCompactBytes <$> deserialize " <>
      "else deserialize"
    -- For everything else (arrays, structs, primitives), use the regular decode
    -- Arrays are handled by encodeVersionedArray/decodeVersionedArray which manages compact format
    _ -> generateFieldDecodeExpr field flexibleVersion

generateFieldDecodeExpr :: FieldSpec -> Maybe Int16 -> Doc ann
generateFieldDecodeExpr field flexibleVersion =
  let isNullable = isJust (fieldNullableVersions field)
  in case fieldType field of
    StructType structName | isNullable ->
      -- Nullable nested structure: need to decode nullable marker and conditionally decode struct
      let decodeFun = "decode" <> pretty structName
      in "do { flag <- deserialize :: (MonadGet m) => m Int8; case flag of { 0 -> pure P.Null; 1 -> P.NotNull <$>" <+> decodeFun <+> "version; _ -> fail \"Invalid nullable flag\" } }"
    StructType structName ->
      -- Single nested structure fields: call version-aware decode function
      let decodeFun = "decode" <> pretty structName
      in decodeFun <+> "version"
    ArrayType (StructType structName) ->
      -- Arrays of nested structures: use version-aware array decoding
      let decodeFun = "decode" <> pretty structName
          isNullable = isJust (fieldNullableVersions field)
          thresholdDoc = maybe "999" pretty flexibleVersion  -- Use 999 if no flexible versions
      in if isNullable
         then "E.decodeVersionedNullableArray" <+> "version" <+> thresholdDoc <+> decodeFun
         else "P.mkKafkaArray" <+> "<$>" <+> "E.decodeVersionedArray" <+> "version" <+> thresholdDoc <+> decodeFun
    ArrayType (PrimitiveType "string") ->
      -- Arrays of strings: use version-aware array decoding with string element decoder
      let isNullable = isJust (fieldNullableVersions field)
          thresholdDoc = maybe "999" pretty flexibleVersion
          stringDecoder = "(\\v -> if v >=" <+> thresholdDoc <+> "then P.fromCompactString <$> deserialize else deserialize)"
      in if isNullable
         then "E.decodeVersionedNullableArray" <+> "version" <+> thresholdDoc <+> stringDecoder
         else "P.mkKafkaArray" <+> "<$>" <+> "E.decodeVersionedArray" <+> "version" <+> thresholdDoc <+> stringDecoder
    ArrayType (PrimitiveType "bytes") ->
      -- Arrays of bytes: use version-aware array decoding with bytes element decoder
      let isNullable = isJust (fieldNullableVersions field)
          thresholdDoc = maybe "999" pretty flexibleVersion
          bytesDecoder = "(\\v -> if v >=" <+> thresholdDoc <+> "then P.fromCompactBytes <$> deserialize else deserialize)"
      in if isNullable
         then "E.decodeVersionedNullableArray" <+> "version" <+> thresholdDoc <+> bytesDecoder
         else "P.mkKafkaArray" <+> "<$>" <+> "E.decodeVersionedArray" <+> "version" <+> thresholdDoc <+> bytesDecoder
    ArrayType otherType ->
      -- Arrays of other primitive types (int32, bool, etc)
      -- Only use version-aware decoding if flexible versions are actually used
      let isNullable = isJust (fieldNullableVersions field)
          primitiveDecoder = "(\\_ -> deserialize)"  -- Version-agnostic decoder for primitives
          thresholdDoc = maybe "999" pretty flexibleVersion
      in case flexibleVersion of
           Nothing -> "deserialize"  -- No flexible versions - use simple deserialization
           Just _ -> if isNullable
                     then "E.decodeVersionedNullableArray" <+> "version" <+> thresholdDoc <+> primitiveDecoder
                     else "P.mkKafkaArray" <+> "<$>" <+> "E.decodeVersionedArray" <+> "version" <+> thresholdDoc <+> primitiveDecoder
    _ -> "deserialize"

-- | Generate encoding code for a single field.
generateFieldEncode :: Text -> Bool -> FieldSpec -> Maybe Int16 -> Doc ann
generateFieldEncode typeName isFlexible field flexibleVersion =
  let fieldAccessor = parens $ pretty (toHaskellFieldName typeName (fieldName field)) <+> "msg"
      isNullable = isJust (fieldNullableVersions field)
  in generateTypeEncode isFlexible field fieldAccessor isNullable flexibleVersion

-- | Generate encoding code based on type, with version-aware flexible format handling.
-- This generates conditional code that checks at runtime if version >= flexibleVer.
-- Note: Only strings and bytes need version-aware encoding at the primitive level.
-- Arrays are handled by encodeVersionedArray/decodeVersionedArray.
generateTypeEncodeVersionAware :: Int16 -> FieldSpec -> Doc ann -> Bool -> Maybe Int16 -> Doc ann
generateTypeEncodeVersionAware flexibleVer field accessor isNullable flexibleVersion
  -- Per-field override: emit the non-compact serializer regardless
  -- of the message-level flexibility flag.
  | fieldOptsOutOfFlexible field =
      generateTypeEncode False field accessor isNullable flexibleVersion
generateTypeEncodeVersionAware flexibleVer field accessor isNullable flexibleVersion =
  case fieldType field of
    PrimitiveType "string" ->
      "if version >=" <+> pretty flexibleVer <+> 
      "then serialize (toCompactString" <+> accessor <> ") " <>
      "else serialize" <+> accessor
    PrimitiveType "bytes" ->
      "if version >=" <+> pretty flexibleVer <+> 
      "then serialize (toCompactBytes" <+> accessor <> ") " <>
      "else serialize" <+> accessor
    -- For everything else (arrays, structs, primitives), use the regular encode
    -- Arrays are handled by encodeVersionedArray which manages compact format
    _ -> generateTypeEncode False field accessor isNullable flexibleVersion

-- | Generate encoding code based on type (non-version-aware, used when flexibility is static).
generateTypeEncode :: Bool -> FieldSpec -> Doc ann -> Bool -> Maybe Int16 -> Doc ann
generateTypeEncode isFlexibleArg field accessor isNullable flexibleVersion =
  -- Honour the per-field flexibility opt-out: if the field has
  -- @flexibleVersions: none@ in the spec, never use the compact
  -- variant even on flexible message versions.
  let isFlexible = isFlexibleArg && not (fieldOptsOutOfFlexible field)
  in case fieldType field of
    PrimitiveType "string" ->
      if isFlexible
        then "serialize (toCompactString" <+> accessor <> ")"
        else "serialize" <+> accessor
    PrimitiveType "bytes" ->
      if isFlexible
        then "serialize (toCompactBytes" <+> accessor <> ")"
        else "serialize" <+> accessor
    StructType structName | isNullable ->
      -- Nullable nested structure: need to handle Null case
      let encodeFun = "encode" <> pretty structName
      in "case" <+> accessor <+> "of { P.Null -> serialize (0 :: Int8); P.NotNull val -> do { serialize (1 :: Int8);" <+> encodeFun <+> "version val } }"
    StructType structName ->
      -- Single nested structure fields: call version-aware encode function
      let encodeFun = "encode" <> pretty structName
      in encodeFun <+> "version" <+> accessor
    ArrayType (StructType structName) ->
      -- Arrays of nested structures: use version-aware array encoding
      let encodeFun = "encode" <> pretty structName
          isNullable = isJust (fieldNullableVersions field)
          thresholdDoc = maybe "999" pretty flexibleVersion
      in if isNullable
         then "E.encodeVersionedNullableArray" <+> "version" <+> thresholdDoc <+> encodeFun <+> accessor
         else "E.encodeVersionedArray" <+> "version" <+> thresholdDoc <+> encodeFun <+> parens ("case P.unKafkaArray" <+> accessor <+> "of { P.NotNull v -> v; P.Null -> V.empty }")
    ArrayType (PrimitiveType "string") ->
      -- Arrays of strings: use version-aware array encoding with string element encoder
      let isNullable = isJust (fieldNullableVersions field)
          thresholdDoc = maybe "999" pretty flexibleVersion
          stringEncoder = "(\\v s -> if v >=" <+> thresholdDoc <+> "then serialize (toCompactString s) else serialize s)"
      in if isNullable
         then "E.encodeVersionedNullableArray" <+> "version" <+> thresholdDoc <+> stringEncoder <+> accessor
         else "E.encodeVersionedArray" <+> "version" <+> thresholdDoc <+> stringEncoder <+> parens ("case P.unKafkaArray" <+> accessor <+> "of { P.NotNull v -> v; P.Null -> V.empty }")
    ArrayType (PrimitiveType "bytes") ->
      -- Arrays of bytes: use version-aware array encoding with bytes element encoder
      let isNullable = isJust (fieldNullableVersions field)
          thresholdDoc = maybe "999" pretty flexibleVersion
          bytesEncoder = "(\\v b -> if v >=" <+> thresholdDoc <+> "then serialize (toCompactBytes b) else serialize b)"
      in if isNullable
         then "E.encodeVersionedNullableArray" <+> "version" <+> thresholdDoc <+> bytesEncoder <+> accessor
         else "E.encodeVersionedArray" <+> "version" <+> thresholdDoc <+> bytesEncoder <+> parens ("case P.unKafkaArray" <+> accessor <+> "of { P.NotNull v -> v; P.Null -> V.empty }")
    ArrayType otherType ->
      -- Arrays of other primitive types (int32, bool, etc)
      -- Only use version-aware encoding if flexible versions are actually used
      let debugMsg = " -- ArrayType: " <> pretty (show otherType)
          primitiveEncoder = "(\\_ x -> serialize x)"  -- Version-agnostic encoder for primitives
          thresholdDoc = maybe "999" pretty flexibleVersion
      in case flexibleVersion of
           Nothing -> "serialize" <+> accessor <> debugMsg  -- No flexible versions - use simple serialization
           Just _ -> if isNullable
                     then "E.encodeVersionedNullableArray" <+> "version" <+> thresholdDoc <+> primitiveEncoder <+> accessor <> debugMsg
                     else "E.encodeVersionedArray" <+> "version" <+> thresholdDoc <+> primitiveEncoder <+> parens ("case P.unKafkaArray" <+> accessor <+> "of { P.NotNull v -> v; P.Null -> V.empty }") <> debugMsg
    _ -> "serialize" <+> accessor

-- | Helper to convert types for flexible encoding
generateFlexibleConversion :: TypeSpec -> Doc ann
generateFlexibleConversion (PrimitiveType "string") = "toCompactString"
generateFlexibleConversion (PrimitiveType "bytes") = "toCompactBytes"
generateFlexibleConversion (ArrayType _) = "toCompactArray"
generateFlexibleConversion _ = "id"

-- | Generate a default constructor for a struct type.
-- Constructs the struct with all fields set to their default values.
-- Generates a single-line expression suitable for use in parens.
generateStructDefault :: Text -> Maybe [FieldSpec] -> Doc ann
generateStructDefault structName Nothing = 
  -- No field information available, use error with message
  "error" <+> dquotes ("No default available for struct" <+> pretty structName)
generateStructDefault structName (Just fields) =
  -- Construct the struct with all fields defaulted on a single line
  let recordFields = map generateStructFieldDefault fields
      fieldsDoc = hsep $ punctuate "," recordFields
  in pretty structName <+> "{" <+> fieldsDoc <+> "}"
  where
    generateStructFieldDefault :: FieldSpec -> Doc ann
    generateStructFieldDefault f =
      let fieldName' = toHaskellFieldName structName (fieldName f)
      in pretty fieldName' <+> "=" <+> generateFieldDefault f

-- | Generate a default value for a field not present in a version.
generateFieldDefault :: FieldSpec -> Doc ann
generateFieldDefault field =
  case fieldDefault field of
    Just (Aeson.Bool True) -> "True"
    Just (Aeson.Bool False) -> "False"
    Just (Aeson.Number n) -> formatNumber (fieldType field) n
    -- String defaults in JSON represent Haskell code - interpret based on field type
    Just (Aeson.String s) -> interpretStringDefault (fieldType field) s
    Just Aeson.Null -> "Null"
    Nothing -> 
      -- Default based on type
      let isNullable = isJust (fieldNullableVersions field)
      in case fieldType field of
        PrimitiveType "bool" -> "False"
        PrimitiveType t | T.isPrefixOf "int" t || t == "uint16" || t == "uint32" -> "0"
        PrimitiveType "float64" -> "0.0"
        PrimitiveType "string" -> "P.KafkaString Null"  -- Wrap Null in KafkaString constructor
        PrimitiveType "bytes" -> "P.KafkaBytes Null"   -- Wrap Null in KafkaBytes constructor
        PrimitiveType "uuid" -> "P.nullUuid"
        ArrayType _ -> if isNullable 
                       then "P.KafkaArray P.Null"  -- Nullable arrays default to null
                       else "P.mkKafkaArray V.empty"  -- Non-nullable arrays default to empty
        StructType structName -> generateStructDefault structName (fieldFields field)
        _ -> "undefined"
  where
    -- Format a number, converting to integer if needed and wrapping negatives in parens
    formatNumber :: TypeSpec -> Scientific -> Doc ann
    formatNumber (PrimitiveType t) n
      | T.isPrefixOf "int" t || t == "uint16" || t == "uint32" =
          -- Convert to integer for int types
          let intVal = Scientific.coefficient n
              numStr = show intVal
          in if head numStr == '-'
             then parens (pretty numStr)
             else pretty numStr
      | t == "float64" =
          let numStr = show n
          in if head numStr == '-'
             then parens (pretty numStr)
             else pretty numStr
    formatNumber _ n =
      let numStr = show n
      in if head numStr == '-'
         then parens (pretty numStr)
         else pretty numStr
    
    -- Use Doc's IsString instance by using 'pretty' on Text directly, not String
    -- This outputs the text content without quotes
    interpretStringDefault :: TypeSpec -> Text -> Doc ann
    interpretStringDefault (PrimitiveType "bool") "true" = "True"
    interpretStringDefault (PrimitiveType "bool") "false" = "False"
    interpretStringDefault (PrimitiveType t) s 
      | T.isPrefixOf "int" t || t == "uint16" || t == "uint32" =
          if s == "null" then "Null" 
          else if not (T.null s) && T.head s == '-' then parens (pretty s)  -- Wrap negative numbers
          else pretty s
    interpretStringDefault (PrimitiveType "float64") s =
      if s == "null" then "Null"
      else if not (T.null s) && T.head s == '-' then parens (pretty s)  -- Wrap negative numbers
      else pretty s
    interpretStringDefault (PrimitiveType "string") "null" = "P.KafkaString Null"
    interpretStringDefault (PrimitiveType "string") "" = "P.KafkaString Null"  -- Empty string default means null
    interpretStringDefault (PrimitiveType "string") s = "P.mkKafkaString" <+> dquotes (pretty s)
    interpretStringDefault (PrimitiveType "bytes") "null" = "P.KafkaBytes Null"
    interpretStringDefault _ "null" = "Null"  -- For other types, raw Null
    interpretStringDefault _ s = 
      if not (T.null s) && T.head s == '-' then parens (pretty s)  -- Wrap negative numbers
      else pretty s

-- | Generate decoding function with version dispatch.
generateDecodeFunction :: ProtocolSchema -> Maybe Int16 -> Either String VersionSpec -> Doc ann
generateDecodeFunction schema flexibleVersion validVersions =
  let typeName = schemaName schema
      functionName = "decode" <> pretty typeName
      versions = case validVersions of
        Right spec -> expandVersionSpec spec
        Left _ -> []
      guards = generateVersionDispatch versions flexibleVersion "decode" typeName (schemaFields schema)
      hasValidVersions = not (null versions)
  in vsep
    [ "-- | Decode" <+> pretty typeName <+> "with the given API version."
    , functionName <+> ":: MonadGet m => E.ApiVersion -> m" <+> pretty typeName
    , functionName <+> "version"
    , vsep guards
    , if hasValidVersions 
        then "  | otherwise = fail $ \"Unsupported version: \" ++ show version"
        else mempty
    ]

-- | Convert a protocol name to a Haskell type name (PascalCase).
toHaskellTypeName :: Text -> Text
toHaskellTypeName = T.pack . ensureUpper . T.unpack
  where
    ensureUpper [] = []
    ensureUpper (x:xs) = toUpper x : xs

-- | Convert a protocol field name to a Haskell record field name.
-- Prepends the type name in camelCase to avoid field name conflicts.
toHaskellFieldName :: Text -> Text -> Text
toHaskellFieldName typeName fieldName =
  let typePrefix = if T.null typeName 
                     then ""
                     else T.pack $ toCamelCase $ T.unpack typeName
      fieldSuffix = T.pack $ T.unpack fieldName
  in typePrefix <> fieldSuffix
  where
    toCamelCase [] = []
    toCamelCase (x:xs) = toLower x : xs

-- | Convert a protocol name to a Haskell module name.
toHaskellModuleName :: Text -> Text
toHaskellModuleName name = "Kafka.Protocol.Generated." <> name

-- | Render a Doc to Text.
renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

-- | Generate a JSON inventory of all messages for test generation.
-- This produces a JSON array with metadata about each message type.
generateMessageInventory :: [ProtocolSchema] -> Text
generateMessageInventory schemas =
  let inventoryItems = map schemaToInventoryItem schemas
      jsonValue = Aeson.toJSON inventoryItems
  in TL.toStrict $ TL.decodeUtf8 $ Aeson.encode jsonValue

-- | Convert a ProtocolSchema to an inventory item.
schemaToInventoryItem :: ProtocolSchema -> Aeson.Value
schemaToInventoryItem schema =
  let validVersions = parseVersionSpec (schemaValidVersions schema)
      flexibleVersions = parseVersionSpec (schemaFlexibleVersions schema)
      
      -- Determine versions to test
      allVersions = case validVersions of
        Right spec -> expandVersionSpec spec
        _ -> []
      
      firstFlexibleVersion = case flexibleVersions of
        Right (VersionFrom v) -> Just v
        Right (VersionRange v _) -> Just v
        Right (ExactVersion v) -> Just v
        _ -> Nothing
      
      -- Select key versions: v0, first flexible, max, and a few intermediate
      versionsToTest = selectTestVersions allVersions firstFlexibleVersion
      
  in Aeson.object
    [ "name" Aeson..= schemaName schema
    , "type" Aeson..= schemaType schema
    , "apiKey" Aeson..= schemaApiKey schema
    , "validVersions" Aeson..= schemaValidVersions schema
    , "flexibleVersions" Aeson..= schemaFlexibleVersions schema
    , "allVersions" Aeson..= allVersions
    , "versionsToTest" Aeson..= versionsToTest
    , "about" Aeson..= schemaAbout schema
    ]

-- | Select a representative set of versions to test.
-- Includes: v0, first flexible version, max version, and some intermediate versions.
selectTestVersions :: [Int16] -> Maybe Int16 -> [Int16]
selectTestVersions [] _ = []
selectTestVersions versions maybeFlexible =
  let v0 = if 0 `elem` versions then [0] else []
      maxV = [maximum versions]
      flexV = case maybeFlexible of
        Just fv | fv `elem` versions && fv /= 0 && fv /= maximum versions -> [fv]
        _ -> []
      
      -- Add a few intermediate versions (roughly every 3-4 versions)
      intermediate = if length versions > 4
                     then let step = max 1 (length versions `div` 4)
                              indices = [step, step * 2, step * 3]
                              vs = [versions !! i | i <- indices, i < length versions]
                          in filter (\v -> v `notElem` (v0 ++ flexV ++ maxV)) vs
                     else []
      
  in nub $ sort $ v0 ++ flexV ++ intermediate ++ maxV
