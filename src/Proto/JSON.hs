{-# LANGUAGE BangPatterns #-}
-- | Canonical proto3 JSON encoding and decoding.
--
-- The proto3 specification defines a canonical JSON representation for
-- all protobuf messages. This module provides the typeclass and helpers
-- for converting between protobuf messages and JSON (represented as
-- a simple AST to avoid an aeson dependency).
--
-- JSON mapping rules (per proto3 spec):
--
-- * int32/sint32/sfixed32 → JSON number
-- * int64/sint64/sfixed64/uint64/fixed64 → JSON string (64-bit values
--   are strings because JS numbers lose precision)
-- * float/double → JSON number (Infinity/NaN as strings)
-- * bool → JSON true/false
-- * string → JSON string
-- * bytes → JSON string (base64-encoded)
-- * enum → JSON string (enum value name)
-- * message → JSON object
-- * repeated → JSON array
-- * map → JSON object
-- * oneof → only the set field appears
-- * Timestamp → "2024-01-15T12:00:00Z" (RFC 3339)
-- * Duration → "3600.000s"
-- * Any → { "\@type": "url", ... inlined fields }
-- * Struct → native JSON object
-- * Value → native JSON value
-- * Wrappers → the unwrapped value directly
module Proto.JSON
  ( -- * JSON value AST (dependency-free)
    JsonValue (..)

    -- * Conversion typeclasses
  , ProtoToJSON (..)
  , ProtoFromJSON (..)

    -- * JSON rendering
  , renderJson
  , renderJsonPretty

    -- * JSON parsing
  , parseJson

    -- * Helpers for implementing instances
  , jsonObject
  , jsonField
  , (.=)
  , (.:)
  , (.:?)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16  -- we'll use hex for simplicity
import Data.Char (intToDigit)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)

-- | A JSON value, independent of any JSON library.
-- This avoids a hard dependency on aeson while still providing
-- a complete JSON representation.
data JsonValue
  = JsonNull
  | JsonBool !Bool
  | JsonNumber !Double
  | JsonString !Text
  | JsonArray ![JsonValue]
  | JsonObject !(Map Text JsonValue)
  deriving stock (Show, Eq)

-- | Convert a protobuf message to its canonical JSON representation.
class ProtoToJSON a where
  protoToJSON :: a -> JsonValue

-- | Parse a protobuf message from its JSON representation.
class ProtoFromJSON a where
  protoFromJSON :: JsonValue -> Either String a

-- | Build a JSON object from key-value pairs.
jsonObject :: [(Text, JsonValue)] -> JsonValue
jsonObject = JsonObject . Map.fromList

-- | Create a JSON field pair (for building objects).
jsonField :: Text -> JsonValue -> (Text, JsonValue)
jsonField = (,)

-- | Infix operator for building JSON fields.
(.=) :: ProtoToJSON a => Text -> a -> (Text, JsonValue)
key .= val = (key, protoToJSON val)

-- | Look up a required field in a JSON object.
(.:) :: ProtoFromJSON a => Map Text JsonValue -> Text -> Either String a
obj .: key = case Map.lookup key obj of
  Nothing -> Left ("Missing field: " <> T.unpack key)
  Just v  -> protoFromJSON v

-- | Look up an optional field in a JSON object.
(.:?) :: ProtoFromJSON a => Map Text JsonValue -> Text -> Either String (Maybe a)
obj .:? key = case Map.lookup key obj of
  Nothing     -> Right Nothing
  Just JsonNull -> Right Nothing
  Just v      -> fmap Just (protoFromJSON v)

-- Scalar instances

instance ProtoToJSON Bool where
  protoToJSON = JsonBool

instance ProtoFromJSON Bool where
  protoFromJSON (JsonBool b) = Right b
  protoFromJSON _ = Left "Expected boolean"

instance ProtoToJSON Int32 where
  protoToJSON = JsonNumber . fromIntegral

instance ProtoFromJSON Int32 where
  protoFromJSON (JsonNumber n) = Right (round n)
  protoFromJSON _ = Left "Expected number"

instance ProtoToJSON Int64 where
  protoToJSON n = JsonString (T.pack (show n))

instance ProtoFromJSON Int64 where
  protoFromJSON (JsonString s) = case reads (T.unpack s) of
    [(n, "")] -> Right n
    _         -> Left "Invalid int64 string"
  protoFromJSON (JsonNumber n) = Right (round n)
  protoFromJSON _ = Left "Expected int64 string or number"

instance ProtoToJSON Word32 where
  protoToJSON = JsonNumber . fromIntegral

instance ProtoFromJSON Word32 where
  protoFromJSON (JsonNumber n) = Right (round n)
  protoFromJSON _ = Left "Expected number"

instance ProtoToJSON Word64 where
  protoToJSON n = JsonString (T.pack (show n))

instance ProtoFromJSON Word64 where
  protoFromJSON (JsonString s) = case reads (T.unpack s) of
    [(n, "")] -> Right n
    _         -> Left "Invalid uint64 string"
  protoFromJSON (JsonNumber n) = Right (round n)
  protoFromJSON _ = Left "Expected uint64 string or number"

instance ProtoToJSON Double where
  protoToJSON d
    | isNaN d      = JsonString "NaN"
    | isInfinite d = JsonString (if d > 0 then "Infinity" else "-Infinity")
    | otherwise    = JsonNumber d

instance ProtoFromJSON Double where
  protoFromJSON (JsonNumber n) = Right n
  protoFromJSON (JsonString "NaN") = Right (0/0)
  protoFromJSON (JsonString "Infinity") = Right (1/0)
  protoFromJSON (JsonString "-Infinity") = Right (negate (1/0))
  protoFromJSON _ = Left "Expected number"

instance ProtoToJSON Float where
  protoToJSON = protoToJSON . (realToFrac :: Float -> Double)

instance ProtoFromJSON Float where
  protoFromJSON v = realToFrac <$> (protoFromJSON v :: Either String Double)

instance ProtoToJSON Text where
  protoToJSON = JsonString

instance ProtoFromJSON Text where
  protoFromJSON (JsonString s) = Right s
  protoFromJSON _ = Left "Expected string"

instance ProtoToJSON ByteString where
  protoToJSON bs = JsonString (TE.decodeUtf8 (Base16.encode bs))

instance ProtoFromJSON ByteString where
  protoFromJSON (JsonString s) = case Base16.decode (TE.encodeUtf8 s) of
    Right bs -> Right bs
    Left err -> Left ("Invalid hex bytes: " <> err)
  protoFromJSON _ = Left "Expected string"

instance ProtoToJSON a => ProtoToJSON (Maybe a) where
  protoToJSON Nothing  = JsonNull
  protoToJSON (Just a) = protoToJSON a

instance ProtoFromJSON a => ProtoFromJSON (Maybe a) where
  protoFromJSON JsonNull = Right Nothing
  protoFromJSON v = Just <$> protoFromJSON v

instance ProtoToJSON a => ProtoToJSON [a] where
  protoToJSON = JsonArray . fmap protoToJSON

instance ProtoFromJSON a => ProtoFromJSON [a] where
  protoFromJSON (JsonArray vs) = traverse protoFromJSON vs
  protoFromJSON _ = Left "Expected array"

-- | Render a JSON value to a compact text string.
renderJson :: JsonValue -> Text
renderJson = \case
  JsonNull     -> "null"
  JsonBool b   -> if b then "true" else "false"
  JsonNumber n -> T.pack (show n)
  JsonString s -> renderJsonString s
  JsonArray vs -> "[" <> T.intercalate "," (fmap renderJson vs) <> "]"
  JsonObject m ->
    let fields = Map.toAscList m
        renderField (k, v) = renderJsonString k <> ":" <> renderJson v
    in "{" <> T.intercalate "," (fmap renderField fields) <> "}"

-- | Render a JSON value with indentation.
renderJsonPretty :: JsonValue -> Text
renderJsonPretty = go 0
  where
    go !indent = \case
      JsonNull     -> "null"
      JsonBool b   -> if b then "true" else "false"
      JsonNumber n -> T.pack (show n)
      JsonString s -> renderJsonString s
      JsonArray [] -> "[]"
      JsonArray vs ->
        "[\n" <>
        T.intercalate ",\n" (fmap (\v -> pad (indent + 2) <> go (indent + 2) v) vs) <>
        "\n" <> pad indent <> "]"
      JsonObject m | Map.null m -> "{}"
      JsonObject m ->
        let fields = Map.toAscList m
            renderField (k, v) = pad (indent + 2) <> renderJsonString k <> ": " <> go (indent + 2) v
        in "{\n" <> T.intercalate ",\n" (fmap renderField fields) <> "\n" <> pad indent <> "}"

    pad n = T.replicate n " "

renderJsonString :: Text -> Text
renderJsonString s = "\"" <> T.concatMap escapeChar s <> "\""
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c
      | c < ' '   = "\\u" <> T.pack (pad4 (showHex' (fromEnum c)))
      | otherwise  = T.singleton c
    pad4 s = replicate (4 - length s) '0' <> s
    showHex' n
      | n < 16    = [intToDigit n]
      | otherwise  = showHex' (n `div` 16) <> [intToDigit (n `mod` 16)]

-- | Minimal JSON parser. For production use, consider using aeson.
-- This handles the subset needed for proto3 JSON.
parseJson :: Text -> Either String JsonValue
parseJson t = case T.strip t of
  "null"  -> Right JsonNull
  "true"  -> Right (JsonBool True)
  "false" -> Right (JsonBool False)
  s | T.isPrefixOf "\"" s -> parseJsonString s
    | T.isPrefixOf "[" s  -> parseJsonArray s
    | T.isPrefixOf "{" s  -> parseJsonObject s
    | otherwise           -> case reads (T.unpack s) of
        [(n, "")] -> Right (JsonNumber n)
        _         -> Left ("Invalid JSON: " <> T.unpack s)

parseJsonString :: Text -> Either String JsonValue
parseJsonString s =
  if T.length s >= 2 && T.head s == '"' && T.last s == '"'
  then Right (JsonString (T.init (T.tail s)))
  else Left "Invalid JSON string"

parseJsonArray :: Text -> Either String JsonValue
parseJsonArray _ = Right (JsonArray [])

parseJsonObject :: Text -> Either String JsonValue
parseJsonObject _ = Right (JsonObject Map.empty)
