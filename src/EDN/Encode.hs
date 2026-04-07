{-# LANGUAGE BangPatterns #-}
-- | EDN (Extensible Data Notation) text encoding.
module EDN.Encode
  ( encode
  , encodeBS
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromString, singleton)
import qualified Data.Vector as V

import qualified EDN.Value as E

-- | Render an EDN 'E.Value' to 'Text'.
encode :: E.Value -> Text
encode = toStrict . toLazyText . buildValue

-- | Render an EDN 'E.Value' to a UTF-8 'ByteString'.
encodeBS :: E.Value -> ByteString
encodeBS = TE.encodeUtf8 . encode

buildValue :: E.Value -> Builder
buildValue = \case
  E.Nil -> fromText "nil"

  E.Bool True  -> fromText "true"
  E.Bool False -> fromText "false"

  E.Integer n -> fromString (show n)

  E.Float d
    | isNaN d               -> fromText "##NaN"
    | isInfinite d && d > 0 -> fromText "##Inf"
    | isInfinite d          -> fromText "##-Inf"
    | otherwise             -> fromString (show d)

  E.String t -> singleton '"' <> escapeString t <> singleton '"'

  E.Char c -> buildChar c

  E.Keyword ns name -> singleton ':' <> buildQualified ns name

  E.Symbol ns name -> buildQualified ns name

  E.List vs -> buildCollection '(' ')' vs

  E.Vector vs -> buildCollection '[' ']' vs

  E.Map pairs ->
    singleton '{' <> buildPairs pairs <> singleton '}'

  E.Set vs ->
    fromText "#{" <> buildElems vs <> singleton '}'

  E.Tagged ns tag val
    | T.null ns -> singleton '#' <> fromText tag <> singleton ' ' <> buildValue val
    | otherwise -> singleton '#' <> fromText ns <> singleton '/' <> fromText tag
                   <> singleton ' ' <> buildValue val

buildQualified :: Maybe Text -> Text -> Builder
buildQualified Nothing  name = fromText name
buildQualified (Just ns) name = fromText ns <> singleton '/' <> fromText name

buildCollection :: Char -> Char -> V.Vector E.Value -> Builder
buildCollection open close vs =
  singleton open <> buildElems vs <> singleton close

buildElems :: V.Vector E.Value -> Builder
buildElems vs
  | V.null vs = mempty
  | otherwise =
      V.ifoldl' (\acc i v ->
        if i == 0
          then acc <> buildValue v
          else acc <> singleton ' ' <> buildValue v
      ) mempty vs

buildPairs :: V.Vector (E.Value, E.Value) -> Builder
buildPairs ps
  | V.null ps = mempty
  | otherwise =
      V.ifoldl' (\acc i (k, v) ->
        let pair = buildValue k <> singleton ' ' <> buildValue v
        in if i == 0
             then acc <> pair
             else acc <> fromText ", " <> pair
      ) mempty ps

escapeString :: Text -> Builder
escapeString = T.foldl' (\acc c -> acc <> escapeChar c) mempty
  where
    escapeChar '"'  = fromText "\\\""
    escapeChar '\\' = fromText "\\\\"
    escapeChar '\n' = fromText "\\n"
    escapeChar '\t' = fromText "\\t"
    escapeChar '\r' = fromText "\\r"
    escapeChar c    = singleton c

buildChar :: Char -> Builder
buildChar '\n' = fromText "\\newline"
buildChar '\r' = fromText "\\return"
buildChar ' '  = fromText "\\space"
buildChar '\t' = fromText "\\tab"
buildChar c    = singleton '\\' <> singleton c
