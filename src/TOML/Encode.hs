{-# LANGUAGE BangPatterns #-}
-- | TOML text encoding.
--
-- Renders a 'TOML.Value.Value' to its TOML text representation.
module TOML.Encode
  ( encode
  , encodeBS
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText, fromString, singleton)
import qualified Data.Vector as V

import qualified TOML.Value as TV

encode :: TV.Value -> Text
encode = TL.toStrict . toLazyText . buildTopLevel []

encodeBS :: TV.Value -> ByteString
encodeBS = TE.encodeUtf8 . encode

buildTopLevel :: [Text] -> TV.Value -> Builder
buildTopLevel path (TV.TTable kvs) =
  let (inline, tables) = V.partition (not . isTable . snd) kvs
      !inlinePart = V.foldl' (\acc (k, v) -> acc <> buildKey k <> fromText " = " <> buildInlineValue v <> singleton '\n') mempty inline
      !tablesPart = V.foldl' (\acc (k, v) -> acc <> buildTableSection (path ++ [k]) v) mempty tables
  in inlinePart <> tablesPart
buildTopLevel _ v = buildInlineValue v

buildTableSection :: [Text] -> TV.Value -> Builder
buildTableSection path (TV.TTable kvs) =
  let !header = singleton '\n' <> singleton '[' <> buildPath path <> singleton ']' <> singleton '\n'
      !body = buildTopLevel path (TV.TTable kvs)
  in header <> body
buildTableSection path (TV.TArray vs)
  | V.all isTable vs =
      V.foldl' (\acc v ->
        acc <> singleton '\n'
            <> fromText "[["
            <> buildPath path
            <> fromText "]]"
            <> singleton '\n'
            <> buildTopLevel path v
      ) mempty vs
buildTableSection path v =
  buildKey (last path) <> fromText " = " <> buildInlineValue v <> singleton '\n'

buildPath :: [Text] -> Builder
buildPath [] = mempty
buildPath [k] = buildKey k
buildPath (k:ks) = buildKey k <> singleton '.' <> buildPath ks

buildKey :: Text -> Builder
buildKey k
  | needsQuoting k = singleton '"' <> escapeString k <> singleton '"'
  | otherwise = fromText k

needsQuoting :: Text -> Bool
needsQuoting t = T.null t || T.any (\c -> c == ' ' || c == '.' || c == '#' || c == '=' || c == '"' || c == '\'' || c == '[' || c == ']') t

isTable :: TV.Value -> Bool
isTable (TV.TTable _) = True
isTable (TV.TArray vs) | not (V.null vs) && V.all isTable vs = True
isTable _ = False

buildInlineValue :: TV.Value -> Builder
buildInlineValue = \case
  TV.TString t -> singleton '"' <> escapeString t <> singleton '"'
  TV.TInteger n -> fromString (show n)
  TV.TFloat d
    | isNaN d -> fromText "nan"
    | isInfinite d && d > 0 -> fromText "inf"
    | isInfinite d -> fromText "-inf"
    | otherwise -> fromString (show d)
  TV.TBool True -> fromText "true"
  TV.TBool False -> fromText "false"
  TV.TDateTime t -> fromText t
  TV.TDate t -> fromText t
  TV.TTime t -> fromText t
  TV.TArray vs ->
    singleton '[' <> buildArrayElems vs <> singleton ']'
  TV.TTable kvs ->
    singleton '{' <> buildInlineTablePairs kvs <> singleton '}'

buildArrayElems :: V.Vector TV.Value -> Builder
buildArrayElems vs
  | V.null vs = mempty
  | otherwise = V.ifoldl' (\acc i v ->
      if i == 0 then acc <> buildInlineValue v
                else acc <> fromText ", " <> buildInlineValue v
    ) mempty vs

buildInlineTablePairs :: V.Vector (Text, TV.Value) -> Builder
buildInlineTablePairs kvs
  | V.null kvs = mempty
  | otherwise = V.ifoldl' (\acc i (k, v) ->
      let !pair = buildKey k <> fromText " = " <> buildInlineValue v
      in if i == 0 then acc <> pair
                   else acc <> fromText ", " <> pair
    ) mempty kvs

escapeString :: Text -> Builder
escapeString = foldMap escChar . T.unpack
  where
    escChar '"'  = fromText "\\\""
    escChar '\\' = fromText "\\\\"
    escChar '\n' = fromText "\\n"
    escChar '\t' = fromText "\\t"
    escChar '\r' = fromText "\\r"
    escChar '\b' = fromText "\\b"
    escChar '\f' = fromText "\\f"
    escChar c    = singleton c
