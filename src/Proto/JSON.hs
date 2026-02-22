{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Proto3 canonical JSON encoding and decoding via aeson.
--
-- This module re-exports the aeson types and provides helpers for
-- proto-canonical JSON representations:
--
-- * 64-bit integers encoded as JSON strings (JavaScript precision limits)
-- * bytes as base64 strings
-- * float\/double with Infinity\/NaN as string sentinels
-- * enum values as their proto name strings
module Proto.JSON
  ( -- * Re-exports from aeson
    Aeson.Value (..)
  , Aeson.ToJSON (..)
  , Aeson.FromJSON (..)
  , Aeson.object
  , (Aeson..=)

    -- * Object construction helpers for generated code
  , jsonObject
  , jsonField
  , protoObject
  , (.=:)
  , parseField
  , parseFieldMaybe

    -- * Proto-specific scalar JSON encoding
  , protoInt64ToJSON
  , protoWord64ToJSON
  , protoFloatToJSON
  , protoDoubleToJSON
  , protoBytesToJSON
  , protoBytesFromJSON
  , protoInt64FromJSON
  , protoWord64FromJSON
  , protoDoubleFromJSON
  , protoFloatFromJSON
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Int (Int64)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Word (Word64)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Int (Int32)
import Data.Word (Word32)

-- | Build a JSON object from key-value pairs (for generated code).
jsonObject :: [(Text, Aeson.Value)] -> Aeson.Value
jsonObject = Aeson.Object . AesonKM.fromList . fmap (\(k, v) -> (AesonKey.fromText k, v))

-- | Create a JSON field pair.
jsonField :: Text -> Aeson.Value -> (Text, Aeson.Value)
jsonField = (,)

-- | Build a proto JSON object with field-skipping for default values.
protoObject :: [(Text, Aeson.Value)] -> Aeson.Value
protoObject = jsonObject

-- | Build a JSON field pair using 'ToJSON'.
(.=:) :: Aeson.ToJSON a => Text -> a -> (Text, Aeson.Value)
key .=: val = (key, Aeson.toJSON val)

-- | Parse a required field from a JSON object (for generated FromJSON).
parseField :: Aeson.FromJSON a => Aeson.Object -> Text -> Aeson.Parser a
parseField obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> fail ("Missing field: " <> T.unpack key)
  Just v  -> Aeson.parseJSON v

-- | Parse an optional field from a JSON object (for generated FromJSON).
parseFieldMaybe :: Aeson.FromJSON a => Aeson.Object -> Text -> Aeson.Parser (Maybe a)
parseFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing        -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v         -> Just <$> Aeson.parseJSON v

-- Proto3 canonical JSON: 64-bit integers are encoded as strings.

protoInt64ToJSON :: Int64 -> Aeson.Value
protoInt64ToJSON n = Aeson.String (int64ToText n)

protoWord64ToJSON :: Word64 -> Aeson.Value
protoWord64ToJSON n = Aeson.String (word64ToText n)

protoInt64FromJSON :: Aeson.Value -> Aeson.Parser Int64
protoInt64FromJSON (Aeson.String s) = case TR.signed TR.decimal s of
  Right (n, rest) | T.null rest -> pure n
  _ -> fail "Invalid int64 string"
protoInt64FromJSON (Aeson.Number n) = pure (round n)
protoInt64FromJSON _ = fail "Expected int64 string or number"

protoWord64FromJSON :: Aeson.Value -> Aeson.Parser Word64
protoWord64FromJSON (Aeson.String s) = case TR.decimal s of
  Right (n, rest) | T.null rest -> pure n
  _ -> fail "Invalid uint64 string"
protoWord64FromJSON (Aeson.Number n) = pure (round n)
protoWord64FromJSON _ = fail "Expected uint64 string or number"

-- Proto3 canonical JSON: floats with NaN/Infinity as strings.

protoDoubleToJSON :: Double -> Aeson.Value
protoDoubleToJSON d
  | isNaN d      = Aeson.String "NaN"
  | isInfinite d = Aeson.String (if d > 0 then "Infinity" else "-Infinity")
  | otherwise    = Aeson.Number (fromFloatDigits d)

protoFloatToJSON :: Float -> Aeson.Value
protoFloatToJSON = protoDoubleToJSON . realToFrac

protoDoubleFromJSON :: Aeson.Value -> Aeson.Parser Double
protoDoubleFromJSON (Aeson.Number n) = pure (toRealFloat n)
protoDoubleFromJSON (Aeson.String "NaN") = pure (0/0)
protoDoubleFromJSON (Aeson.String "Infinity") = pure (1/0)
protoDoubleFromJSON (Aeson.String "-Infinity") = pure (negate (1/0))
protoDoubleFromJSON _ = fail "Expected number or special float string"

protoFloatFromJSON :: Aeson.Value -> Aeson.Parser Float
protoFloatFromJSON v = realToFrac <$> protoDoubleFromJSON v

-- Proto3 canonical JSON: bytes as base64.

protoBytesToJSON :: ByteString -> Aeson.Value
protoBytesToJSON bs = Aeson.String (TE.decodeUtf8 (Base64.encode bs))

protoBytesFromJSON :: Aeson.Value -> Aeson.Parser ByteString
protoBytesFromJSON (Aeson.String s) = case Base64.decode (TE.encodeUtf8 s) of
  Right bs -> pure bs
  Left err -> fail ("Invalid base64 bytes: " <> err)
protoBytesFromJSON _ = fail "Expected base64 string for bytes"

-- Orphan instances for proto3 canonical JSON encoding of ByteString.
-- Proto3 spec: bytes fields are base64-encoded strings in JSON.
instance Aeson.ToJSON ByteString where
  toJSON = protoBytesToJSON

instance Aeson.FromJSON ByteString where
  parseJSON = protoBytesFromJSON

int64ToText :: Int64 -> Text
int64ToText n
  | n < 0     = "-" <> word64ToText (fromIntegral (negate n))
  | otherwise = word64ToText (fromIntegral n)

word64ToText :: Word64 -> Text
word64ToText 0 = "0"
word64ToText n = go T.empty n
  where
    go !acc 0 = acc
    go !acc v = let (!q, !r) = v `quotRem` 10
                in go (T.cons (toEnum (fromIntegral r + 48)) acc) q
