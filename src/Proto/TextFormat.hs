-- | Protobuf text format (pbtxt) serialization and deserialization.
--
-- The text format is a human-readable representation of protobuf messages,
-- used for configuration files, test fixtures, and debugging.
--
-- Example text format:
--
-- @
-- name: "John Doe"
-- id: 1234
-- email: "jdoe\@example.com"
-- phones {
--   number: "555-4321"
--   type: HOME
-- }
-- @
module Proto.TextFormat
  ( -- * Rendering
    dynamicToText
  , dynamicToTextPretty

    -- * Parsing
  , textToDynamic

    -- * Text format value type
  , TextValue (..)
  , TextField (..)
  ) where

import Data.Char (isDigit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Int (Int64)
import Data.Word (Word64)

import Proto.Dynamic

-- | A text format field.
data TextField = TextField
  { tfName  :: !Text
  , tfValue :: !TextValue
  } deriving stock (Show, Eq)

-- | A text format value.
data TextValue
  = TVString  !Text
  | TVNumber  !Double
  | TVInteger !Integer
  | TVBool    !Bool
  | TVIdent   !Text
  | TVMessage ![TextField]
  deriving stock (Show, Eq)

-- | Render a dynamic message in text format (compact).
dynamicToText :: DynamicMessage -> Text
dynamicToText = renderDyn 0 False

-- | Render a dynamic message in text format (pretty-printed).
dynamicToTextPretty :: DynamicMessage -> Text
dynamicToTextPretty = renderDyn 0 True

renderDyn :: Int -> Bool -> DynamicMessage -> Text
renderDyn depth pretty (DynamicMessage fs _) =
  let sep = if pretty then "\n" else " "
      fieldTexts = Map.foldlWithKey' (\acc fn val ->
        acc <> renderDynField depth pretty (intToText fn) val <> sep
        ) "" fs
  in fieldTexts

renderDynField :: Int -> Bool -> Text -> DynamicValue -> Text
renderDynField depth pretty name val =
  let ind = if pretty then T.replicate (depth * 2) " " else ""
  in case val of
    DynMessage m ->
      ind <> name <> " {" <>
      (if pretty then "\n" else " ") <>
      renderDyn (depth + 1) pretty m <>
      (if pretty then T.replicate (depth * 2) " " else "") <> "}"
    DynRepeated vs ->
      T.concat (fmap (\v -> renderDynField depth pretty name v <>
        (if pretty then "\n" else " ")) vs)
    DynString s -> ind <> name <> ": \"" <> escapeText s <> "\""
    DynBytes bs -> ind <> name <> ": \"" <> TE.decodeUtf8 (Base16.encode bs) <> "\""
    DynBool b -> ind <> name <> ": " <> (if b then "true" else "false")
    DynVarint v -> ind <> name <> ": " <> word64ToText v
    DynSVarint v -> ind <> name <> ": " <> int64ToText v
    DynFixed32 v -> ind <> name <> ": " <> word64ToText (fromIntegral v)
    DynFixed64 v -> ind <> name <> ": " <> word64ToText v
    DynFloat v -> ind <> name <> ": " <> T.pack (show v)
    DynDouble v -> ind <> name <> ": " <> T.pack (show v)
    DynEnum v -> ind <> name <> ": " <> intToText v
    DynMap _ -> ind <> name <> " {}"

escapeText :: Text -> Text
escapeText = T.concatMap $ \case
  '"'  -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c    -> T.singleton c

-- | Parse text format into a dynamic message.
-- This is a simplified parser that handles the common text format subset.
textToDynamic :: Text -> Either String DynamicMessage
textToDynamic t = case parseFields (T.strip t) of
  Right (fs, _) -> Right (fieldsToDynamic fs)
  Left e -> Left e

fieldsToDynamic :: [TextField] -> DynamicMessage
fieldsToDynamic tfs =
  let numbered = fmap (\tf -> case TR.decimal (tfName tf) of
        Right (n, rest) | T.null rest -> (n, textValueToDyn (tfValue tf))
        _ -> (0, textValueToDyn (tfValue tf))) tfs
  in DynamicMessage (Map.fromList numbered) []

textValueToDyn :: TextValue -> DynamicValue
textValueToDyn = \case
  TVString s  -> DynString s
  TVNumber n  -> DynDouble n
  TVInteger n -> DynVarint (fromIntegral n)
  TVBool b    -> DynBool b
  TVIdent t   -> DynString t
  TVMessage fs -> DynMessage (fieldsToDynamic fs)

parseFields :: Text -> Either String ([TextField], Text)
parseFields = go []
  where
    go acc t =
      let s = T.stripStart t
      in if T.null s || T.head s == '}'
         then Right (reverse acc, s)
         else case parseField s of
           Right (f, rest) -> go (f : acc) rest
           Left e -> Left e

parseField :: Text -> Either String (TextField, Text)
parseField t = do
  let s = T.stripStart t
  let (name, rest) = T.span (\c -> c /= ':' && c /= '{' && c /= ' ' && c /= '\n') s
  if T.null name then Left "Expected field name"
  else do
    let rest' = T.stripStart rest
    case T.uncons rest' of
      Just (':', afterColon) -> do
        (val, remaining) <- parseTextValue (T.stripStart afterColon)
        let remaining' = T.stripStart remaining
        let remaining'' = case T.uncons remaining' of
              Just (';', r) -> r
              Just (',', r) -> r
              _ -> remaining'
        Right (TextField name val, remaining'')
      Just ('{', afterBrace) -> do
        (fields, afterFields) <- parseFields afterBrace
        case T.uncons (T.stripStart afterFields) of
          Just ('}', r) -> Right (TextField name (TVMessage fields), r)
          _ -> Left "Expected '}'"
      _ -> Left ("Expected ':' or '{' after field name '" <> T.unpack name <> "'")

parseTextValue :: Text -> Either String (TextValue, Text)
parseTextValue t
  | T.null t = Left "Empty value"
  | T.head t == '"' = parseTextString t
  | T.isPrefixOf "true" t = Right (TVBool True, T.drop 4 t)
  | T.isPrefixOf "false" t = Right (TVBool False, T.drop 5 t)
  | T.head t == '-' || isDigit (T.head t) = parseTextNumber t
  | otherwise =
      let (ident, rest) = T.span (\c -> c /= '\n' && c /= ' ' && c /= ';' && c /= ',' && c /= '}') t
      in Right (TVIdent ident, rest)

parseTextString :: Text -> Either String (TextValue, Text)
parseTextString t = go (T.drop 1 t) []
  where
    go s acc
      | T.null s = Left "Unterminated string"
      | T.head s == '"' = Right (TVString (T.pack (reverse acc)), T.drop 1 s)
      | T.head s == '\\' && T.length s >= 2 =
          case T.index s 1 of
            'n'  -> go (T.drop 2 s) ('\n' : acc)
            'r'  -> go (T.drop 2 s) ('\r' : acc)
            't'  -> go (T.drop 2 s) ('\t' : acc)
            '"'  -> go (T.drop 2 s) ('"' : acc)
            '\\' -> go (T.drop 2 s) ('\\' : acc)
            _    -> go (T.drop 2 s) (T.index s 1 : acc)
      | otherwise = go (T.drop 1 s) (T.head s : acc)

parseTextNumber :: Text -> Either String (TextValue, Text)
parseTextNumber t =
  let (numStr, rest) = T.span (\c -> c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E' || isDigit c) t
  in if T.any (== '.') numStr || T.any (\c -> c == 'e' || c == 'E') numStr
     then case TR.signed TR.double numStr of
       Right (n, leftover) | T.null leftover -> Right (TVNumber n, rest)
       _ -> Left ("Invalid number: " <> T.unpack numStr)
     else case TR.signed TR.decimal numStr of
       Right (n, leftover) | T.null leftover -> Right (TVInteger n, rest)
       _ -> Left ("Invalid integer: " <> T.unpack numStr)

intToText :: Int -> Text
intToText n
  | n < 0     = "-" <> word64ToText (fromIntegral (negate n))
  | otherwise = word64ToText (fromIntegral n)

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
