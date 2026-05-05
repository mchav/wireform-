{-# LANGUAGE BangPatterns #-}
-- | YAML 1.2 emitter.
--
-- Renders a 'YAML.Value.Value' (or a 'Document' / 'Stream') to its
-- block-style YAML representation, choosing flow style only for
-- empty containers and as a fallback. The output is /round-trippable/:
-- any 'Value' goes through 'encode' and back through
-- 'YAML.Decode.decode' to the same 'Value'.
--
-- The emitter follows the YAML 1.2 core schema (so plain scalars that
-- happen to look like a bool / null / int / float are quoted to keep
-- their string identity).
module YAML.Encode
  ( encode
  , encodeBS
  , encodeDocument
  , encodeStream
  ) where

import Data.ByteString (ByteString)
import Data.Char (isControl, isDigit, ord)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder
  ( Builder, fromString, fromText, singleton, toLazyText )
import qualified Data.Text.Lazy.Builder.Int as BI
import qualified Data.Text.Lazy.Builder.RealFloat as BR
import qualified Data.Vector as V
import Numeric (showHex)

import YAML.Value

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

-- | Encode a single document body.
encode :: Value -> Text
encode v = TL.toStrict (toLazyText (buildDoc (Document False False v)))

encodeBS :: Value -> ByteString
encodeBS = TE.encodeUtf8 . encode

encodeDocument :: Document -> Text
encodeDocument = TL.toStrict . toLazyText . buildDoc

encodeStream :: Stream -> Text
encodeStream (Stream docs) =
  TL.toStrict (toLazyText (V.foldl' (\acc d -> acc <> buildDoc d) mempty docs))

-- ---------------------------------------------------------------------------
-- Document framing
-- ---------------------------------------------------------------------------

buildDoc :: Document -> Builder
buildDoc (Document directives explicitEnd body) =
  let !head_ = if directives then fromText "---\n" else mempty
      !body_ = buildTopLevel body
      !end_  = if explicitEnd  then fromText "...\n" else mempty
  in head_ <> body_ <> end_

buildTopLevel :: Value -> Builder
buildTopLevel v = case unwrap v of
  YMap kvs
    | V.null kvs -> fromText "{}\n"
    | otherwise  -> buildBlockMap 0 kvs
  YSeq xs
    | V.null xs  -> fromText "[]\n"
    | otherwise  -> buildBlockSeq 0 xs
  scalar         -> buildScalar scalar <> singleton '\n'

-- ---------------------------------------------------------------------------
-- Block style
-- ---------------------------------------------------------------------------

buildBlockMap :: Int -> V.Vector (Value, Value) -> Builder
buildBlockMap !ind kvs = V.foldl' step mempty kvs
  where
    step !acc (k, v) = acc <> indent ind <> kvLine ind k v

kvLine :: Int -> Value -> Value -> Builder
kvLine !ind k v =
  let !keyB   = buildKey k
      !inner  = unwrap v
  in case inner of
       YMap kvs
         | V.null kvs -> keyB <> fromText ": {}\n"
         | otherwise  ->
             keyB <> fromText ":\n" <> buildBlockMap (ind + 2) kvs
       YSeq xs
         | V.null xs  -> keyB <> fromText ": []\n"
         | otherwise  ->
             keyB <> fromText ":\n" <> buildBlockSeq ind xs
       _ ->
         keyB <> fromText ": " <> buildScalar inner <> singleton '\n'

buildBlockSeq :: Int -> V.Vector Value -> Builder
buildBlockSeq !ind xs = V.foldl' step mempty xs
  where
    step !acc x = acc <> indent ind <> seqLine ind x

seqLine :: Int -> Value -> Builder
seqLine !ind v =
  let !inner = unwrap v
  in case inner of
       YMap kvs
         | V.null kvs -> fromText "- {}\n"
         | otherwise  -> fromText "- " <> firstKvLine (ind + 2) kvs
       YSeq ys
         | V.null ys  -> fromText "- []\n"
         | otherwise  -> fromText "- " <> firstSeqLine (ind + 2) ys
       _ ->
         fromText "- " <> buildScalar inner <> singleton '\n'

-- | Emit the first key-value pair on the same line as @-@, then the
-- rest at the deeper indent.
firstKvLine :: Int -> V.Vector (Value, Value) -> Builder
firstKvLine !ind kvs
  | V.null kvs = mempty
  | otherwise  =
      let (k, v) = V.head kvs
          rest   = V.tail kvs
      in kvLine ind k v <> V.foldl' (\acc (k', v') -> acc <> indent ind <> kvLine ind k' v') mempty rest

firstSeqLine :: Int -> V.Vector Value -> Builder
firstSeqLine !ind xs
  | V.null xs = mempty
  | otherwise =
      let !x = V.head xs
          !rest = V.tail xs
      in seqLine ind x <> V.foldl' (\acc x' -> acc <> indent ind <> seqLine ind x') mempty rest

indent :: Int -> Builder
indent !n
  | n <= 0    = mempty
  | otherwise = fromText (T.replicate n " ")

-- ---------------------------------------------------------------------------
-- Keys
-- ---------------------------------------------------------------------------

-- | Keys are rendered as scalars; complex (sequence / mapping) keys
-- are folded through the flow-style emitter inside an explicit-key
-- @?@ marker.
buildKey :: Value -> Builder
buildKey k = case unwrap k of
  YMap _ -> fromText "? " <> buildFlow k
  YSeq _ -> fromText "? " <> buildFlow k
  _      -> buildScalar (unwrap k)

-- ---------------------------------------------------------------------------
-- Scalar emission
-- ---------------------------------------------------------------------------

buildScalar :: Value -> Builder
buildScalar = \case
  YNull         -> fromText "null"
  YBool True    -> fromText "true"
  YBool False   -> fromText "false"
  YInt n        -> BI.decimal (toInteger (n :: Int64))
  YFloat d
    | isNaN d   -> fromText ".nan"
    | isInfinite d && d > 0 -> fromText ".inf"
    | isInfinite d          -> fromText "-.inf"
    | otherwise -> renderFloat d
  YString t     -> renderString t
  v             -> buildFlow v   -- container fallthrough (shouldn't normally hit)

-- | Render a 'Double' so that decoding produces the same value and
-- it is unambiguously a YAML float (i.e. always contains @.@ or @e@).
renderFloat :: Double -> Builder
renderFloat d =
  let !s   = T.pack (show d)
      hasDot = T.any (\c -> c == '.' || c == 'e' || c == 'E') s
  in if hasDot then fromText s else fromText s <> fromText ".0"
{-# INLINE renderFloat #-}

-- ---------------------------------------------------------------------------
-- Flow style (used for keys + as fallback)
-- ---------------------------------------------------------------------------

buildFlow :: Value -> Builder
buildFlow v = case unwrap v of
  YMap kvs ->
    singleton '{' <> commaSep (V.map kvFlow kvs) <> singleton '}'
  YSeq xs ->
    singleton '[' <> commaSep (V.map buildFlow xs) <> singleton ']'
  scalar -> buildScalar scalar
  where
    kvFlow (k, val) = buildFlow k <> fromText ": " <> buildFlow val

commaSep :: V.Vector Builder -> Builder
commaSep bs
  | V.null bs = mempty
  | otherwise = V.ifoldl' step mempty bs
  where
    step acc 0 b = acc <> b
    step acc _ b = acc <> fromText ", " <> b

-- ---------------------------------------------------------------------------
-- String quoting
-- ---------------------------------------------------------------------------

-- | Strategy: prefer plain when safe, otherwise single-quoted, only
-- fall back to double-quoted for strings containing characters that
-- must be escaped (control characters or single quotes that would
-- require doubling).
renderString :: Text -> Builder
renderString t
  | T.null t              = fromText "''"
  | needsDoubleQuote t    = doubleQuoted t
  | safePlain t           = fromText t
  | otherwise             = singleQuoted t

doubleQuoted :: Text -> Builder
doubleQuoted t = singleton '"' <> T.foldr step (singleton '"') t
  where
    step c acc = escapeDQ c <> acc

escapeDQ :: Char -> Builder
escapeDQ c = case c of
  '"'  -> fromText "\\\""
  '\\' -> fromText "\\\\"
  '\n' -> fromText "\\n"
  '\t' -> fromText "\\t"
  '\r' -> fromText "\\r"
  '\0' -> fromText "\\0"
  '\a' -> fromText "\\a"
  '\b' -> fromText "\\b"
  '\v' -> fromText "\\v"
  '\f' -> fromText "\\f"
  '\x1B' -> fromText "\\e"
  _ | isControl c ->
        let cp = ord c
        in if cp <= 0xFF
             then fromText "\\x" <> hex 2 cp
             else if cp <= 0xFFFF
               then fromText "\\u" <> hex 4 cp
               else fromText "\\U" <> hex 8 cp
    | otherwise -> singleton c

hex :: Int -> Int -> Builder
hex pad n =
  let s = showHex n ""
      need = pad - length s
  in if need > 0
       then fromText (T.replicate need "0") <> fromString s
       else fromString s

singleQuoted :: Text -> Builder
singleQuoted t =
  singleton '\'' <> fromText (T.replace "'" "''" t) <> singleton '\''

-- | A scalar can be emitted plain when:
--
-- * It's not parsed as a non-string scalar by the core schema
--   (true / false / null / number / etc.).
-- * It does not contain any of the YAML indicator characters that
--   would change the parse (@: { } [ ] , & * # ? | > ' " % @ \`@) in
--   places that matter, or whitespace next to colons that would
--   confuse the block scanner.
safePlain :: Text -> Bool
safePlain t
  | T.null t                 = False
  | parsesAsScalar t         = False
  | startBad (T.head t)      = False
  | endsWithSpace t          = False
  | T.any unsafePlainChar t  = False
  | T.any isControl t        = False
  | T.isInfixOf ": " t       = False
  | T.isInfixOf " #" t       = False
  | otherwise                = True
  where
    startBad c = case c of
      '!' -> True; '&' -> True; '*' -> True; '?' -> True; '|' -> True
      '>' -> True; '\'' -> True; '"' -> True; '%' -> True; '@' -> True
      '`' -> True; '#' -> True; ',' -> True; '[' -> True; ']' -> True
      '{' -> True; '}' -> True; ':' -> True; '-' -> True; ' ' -> True
      '\t' -> True
      _    -> False
    endsWithSpace = case T.unsnoc t of
      Just (_, c) -> c == ' ' || c == '\t'
      Nothing     -> False

unsafePlainChar :: Char -> Bool
unsafePlainChar c =
  -- These cause parse ambiguity nearly anywhere in a plain scalar.
  c == ',' || c == '[' || c == ']' || c == '{' || c == '}' ||
  c == '&' || c == '*' || c == '\n' || c == '\r'

-- | Conservative recogniser for strings the core schema would parse
-- as a non-string scalar. We must quote those when they appear as a
-- string value so the round-trip is faithful.
parsesAsScalar :: Text -> Bool
parsesAsScalar t = case T.toLower t of
  "null" -> True
  "~"    -> True
  ""     -> True
  "true" -> True; "false" -> True
  "yes"  -> True; "no"    -> True
  "on"   -> True; "off"   -> True
  ".inf" -> True; "+.inf" -> True; "-.inf" -> True
  ".nan" -> True
  s      ->
    looksLikeInt s || looksLikeFloat s

-- | Conservative integer recogniser matching the YAML 1.2 core
-- schema: optional sign, then decimal / hex (@0x@) / octal (@0o@).
looksLikeInt :: Text -> Bool
looksLikeInt s = case T.uncons s of
  Just ('+', rest) -> goDec rest
  Just ('-', rest) -> goDec rest
  _                -> goDec s
  where
    goDec s'
      | T.isPrefixOf "0x" s' || T.isPrefixOf "0X" s'
          = let r = T.drop 2 s' in not (T.null r) && T.all isHex r
      | T.isPrefixOf "0o" s' || T.isPrefixOf "0O" s'
          = let r = T.drop 2 s' in not (T.null r) && T.all isOct r
      | otherwise = not (T.null s') && T.all isDigit s'
    isHex c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    isOct c = c >= '0' && c <= '7'

looksLikeFloat :: Text -> Bool
looksLikeFloat s =
  let s' = case T.uncons s of
             Just ('+', r) -> r
             Just ('-', r) -> r
             _             -> s
      hasDigit = T.any isDigit s'
      hasMark  = T.any (\c -> c == '.' || c == 'e' || c == 'E') s'
      onlyOK   = T.all (\c -> isDigit c || c == '.' || c == 'e' || c == 'E'
                              || c == '+' || c == '-') s'
  in hasDigit && hasMark && onlyOK

needsDoubleQuote :: Text -> Bool
needsDoubleQuote = T.any badForSingle
  where
    badForSingle c =
      isControl c && c /= '\t'
