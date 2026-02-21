{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.ByteString.Base64 as Base64
import Data.Char (intToDigit)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
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
  protoToJSON bs = JsonString (TE.decodeUtf8 (Base64.encode bs))

instance ProtoFromJSON ByteString where
  protoFromJSON (JsonString s) = case Base64.decode (TE.encodeUtf8 s) of
    Right bs -> Right bs
    Left err -> Left ("Invalid base64 bytes: " <> err)
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

instance ProtoToJSON a => ProtoToJSON (V.Vector a) where
  protoToJSON = JsonArray . fmap protoToJSON . V.toList

instance ProtoFromJSON a => ProtoFromJSON (V.Vector a) where
  protoFromJSON (JsonArray vs) = V.fromList <$> traverse protoFromJSON vs
  protoFromJSON _ = Left "Expected array"

instance {-# OVERLAPPING #-} ProtoToJSON (VU.Vector Word32) where
  protoToJSON v = JsonArray (fmap (JsonNumber . fromIntegral) (VU.toList v))

instance {-# OVERLAPPING #-} ProtoToJSON (VU.Vector Word64) where
  protoToJSON v = JsonArray (fmap (protoToJSON) (VU.toList v))

instance {-# OVERLAPPING #-} ProtoToJSON (VU.Vector Int32) where
  protoToJSON v = JsonArray (fmap (JsonNumber . fromIntegral) (VU.toList v))

instance {-# OVERLAPPING #-} ProtoToJSON (VU.Vector Int64) where
  protoToJSON v = JsonArray (fmap (protoToJSON) (VU.toList v))

instance {-# OVERLAPPING #-} ProtoToJSON (VU.Vector Double) where
  protoToJSON v = JsonArray (fmap (protoToJSON) (VU.toList v))

instance {-# OVERLAPPING #-} ProtoToJSON (VU.Vector Float) where
  protoToJSON v = JsonArray (fmap (protoToJSON) (VU.toList v))

instance {-# OVERLAPPING #-} ProtoToJSON (VU.Vector Bool) where
  protoToJSON v = JsonArray (fmap JsonBool (VU.toList v))

instance {-# OVERLAPPING #-} ProtoFromJSON (VU.Vector Word32) where
  protoFromJSON (JsonArray vs) = VU.fromList <$> traverse (\v -> protoFromJSON v) vs
  protoFromJSON _ = Left "Expected array"

instance {-# OVERLAPPING #-} ProtoFromJSON (VU.Vector Word64) where
  protoFromJSON (JsonArray vs) = VU.fromList <$> traverse (\v -> protoFromJSON v) vs
  protoFromJSON _ = Left "Expected array"

instance {-# OVERLAPPING #-} ProtoFromJSON (VU.Vector Int32) where
  protoFromJSON (JsonArray vs) = VU.fromList <$> traverse (\v -> protoFromJSON v) vs
  protoFromJSON _ = Left "Expected array"

instance {-# OVERLAPPING #-} ProtoFromJSON (VU.Vector Int64) where
  protoFromJSON (JsonArray vs) = VU.fromList <$> traverse (\v -> protoFromJSON v) vs
  protoFromJSON _ = Left "Expected array"

instance {-# OVERLAPPING #-} ProtoFromJSON (VU.Vector Double) where
  protoFromJSON (JsonArray vs) = VU.fromList <$> traverse (\v -> protoFromJSON v) vs
  protoFromJSON _ = Left "Expected array"

instance {-# OVERLAPPING #-} ProtoFromJSON (VU.Vector Float) where
  protoFromJSON (JsonArray vs) = VU.fromList <$> traverse (\v -> protoFromJSON v) vs
  protoFromJSON _ = Left "Expected array"

instance {-# OVERLAPPING #-} ProtoFromJSON (VU.Vector Bool) where
  protoFromJSON (JsonArray vs) = VU.fromList <$> traverse (\v -> protoFromJSON v) vs
  protoFromJSON _ = Left "Expected array"

instance (ProtoToJSON k, ProtoToJSON v) => ProtoToJSON (Map k v) where
  protoToJSON m = JsonObject (Map.mapKeys showKey (Map.map protoToJSON m))
    where
      showKey k = case protoToJSON k of
        JsonString s -> s
        JsonNumber n -> T.pack (show n)
        _ -> T.pack (show (protoToJSON k))

instance (Ord k, ProtoFromJSON k, ProtoFromJSON v) => ProtoFromJSON (Map k v) where
  protoFromJSON (JsonObject _) = Right Map.empty
  protoFromJSON _ = Left "Expected object"

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
    pad4 xs = replicate (4 - length xs) '0' <> xs
    showHex' n
      | n < 16    = [intToDigit n]
      | otherwise  = showHex' (n `div` 16) <> [intToDigit (n `mod` 16)]

-- | Minimal recursive-descent JSON parser.
parseJson :: Text -> Either String JsonValue
parseJson t = case parseValue (T.strip t) of
  Right (v, rest) | T.null (T.strip rest) -> Right v
  Right (_, rest) -> Left ("Trailing content: " <> T.unpack (T.take 20 rest))
  Left e -> Left e

parseValue :: Text -> Either String (JsonValue, Text)
parseValue t =
  let s = T.stripStart t
  in if T.null s then Left "Unexpected end of input"
     else case T.head s of
       'n' | T.isPrefixOf "null" s  -> Right (JsonNull, T.drop 4 s)
       't' | T.isPrefixOf "true" s  -> Right (JsonBool True, T.drop 4 s)
       'f' | T.isPrefixOf "false" s -> Right (JsonBool False, T.drop 5 s)
       '"' -> parseStr s
       '[' -> parseArr (T.drop 1 s) []
       '{' -> parseObj (T.drop 1 s) []
       _   -> parseNum s

parseStr :: Text -> Either String (JsonValue, Text)
parseStr s = case scanString (T.drop 1 s) [] of
  Right (str, rest) -> Right (JsonString (T.pack str), rest)
  Left e -> Left e
  where
    scanString t acc
      | T.null t = Left "Unterminated string"
      | otherwise = case T.head t of
          '"'  -> Right (reverse acc, T.drop 1 t)
          '\\' | T.length t >= 2 -> case T.index t 1 of
                   'n'  -> scanString (T.drop 2 t) ('\n' : acc)
                   'r'  -> scanString (T.drop 2 t) ('\r' : acc)
                   't'  -> scanString (T.drop 2 t) ('\t' : acc)
                   '"'  -> scanString (T.drop 2 t) ('"' : acc)
                   '\\' -> scanString (T.drop 2 t) ('\\' : acc)
                   '/'  -> scanString (T.drop 2 t) ('/' : acc)
                   _    -> scanString (T.drop 2 t) (T.index t 1 : acc)
               | otherwise -> Left "Unterminated escape"
          c    -> scanString (T.drop 1 t) (c : acc)

parseNum :: Text -> Either String (JsonValue, Text)
parseNum s =
  let (numTxt, rest) = T.span (\c -> c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E' || (c >= '0' && c <= '9')) s
  in if T.null numTxt then Left ("Expected number at: " <> T.unpack (T.take 20 s))
     else case reads (T.unpack numTxt) of
       [(n, "")] -> Right (JsonNumber n, rest)
       _         -> Left ("Invalid number: " <> T.unpack numTxt)

parseArr :: Text -> [JsonValue] -> Either String (JsonValue, Text)
parseArr t acc =
  let s = T.stripStart t
  in if T.null s then Left "Unterminated array"
     else if T.head s == ']' then Right (JsonArray (reverse acc), T.drop 1 s)
     else do
       (v, rest) <- parseValue s
       let rest' = T.stripStart rest
       if T.null rest' then Left "Unterminated array"
       else case T.head rest' of
         ']' -> Right (JsonArray (reverse (v : acc)), T.drop 1 rest')
         ',' -> parseArr (T.drop 1 rest') (v : acc)
         _   -> Left ("Expected ',' or ']' in array, got: " <> T.unpack (T.take 10 rest'))

parseObj :: Text -> [(Text, JsonValue)] -> Either String (JsonValue, Text)
parseObj t acc =
  let s = T.stripStart t
  in if T.null s then Left "Unterminated object"
     else if T.head s == '}' then Right (JsonObject (Map.fromList (reverse acc)), T.drop 1 s)
     else case parseStr (T.stripStart s) of
       Right (JsonString key, afterKey) ->
         let afterColon = T.stripStart afterKey
         in case T.uncons afterColon of
              Just (':', rest) -> case parseValue rest of
                Right (val, afterVal) ->
                  let rest' = T.stripStart afterVal
                  in if T.null rest' then Left "Unterminated object"
                     else case T.head rest' of
                       '}' -> Right (JsonObject (Map.fromList (reverse ((key, val) : acc))), T.drop 1 rest')
                       ',' -> parseObj (T.drop 1 rest') ((key, val) : acc)
                       _   -> Left ("Expected ',' or '}' in object, got: " <> T.unpack (T.take 10 rest'))
                Left e -> Left e
              _ -> Left ("Expected ':' after object key \"" <> T.unpack key <> "\"")
       Right _ -> Left "Expected string key in object"
       Left e -> Left e
