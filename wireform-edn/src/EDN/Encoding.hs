{-# LANGUAGE BangPatterns #-}

{- | Direct-to-text EDN encoding, mirroring aeson's @toEncoding@ approach.

An 'Encoding' is a 'Data.Text.Lazy.Builder.Builder' that renders to
exactly one EDN value. Encodings compose through 'list', 'vector',
'map_', 'set', and 'tagged' rather than through a 'Monoid' instance.
-}
module EDN.Encoding (
  Encoding (..),
  encodingToBuilder,
  encodingToLazyText,
  encodingToText,
  encodingToByteString,

  -- * Item constructors
  nil,
  bool,
  integer,
  int,
  double,
  float,
  string,
  lazyString,
  char,
  keyword,
  symbol,

  -- * Containers
  list,
  listFromList,
  vector,
  vectorFromList,
  map_,
  mapList,
  set,
  setFromList,
  tagged,
) where

import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import EDN.Encode qualified as EE
import EDN.Value qualified as E


newtype Encoding = Encoding {runEncoding :: Builder}


encodingToBuilder :: Encoding -> Builder
encodingToBuilder = runEncoding


encodingToLazyText :: Encoding -> TL.Text
encodingToLazyText = TLB.toLazyText . runEncoding


encodingToText :: Encoding -> Text
encodingToText = TL.toStrict . encodingToLazyText


encodingToByteString :: Encoding -> ByteString
encodingToByteString = TE.encodeUtf8 . encodingToText


-- Re-uses 'EDN.Encode'\'s string-escape pass for fidelity. Calling
-- through 'EE.encode' on a fully-built 'E.Value' would defeat the
-- point; instead we emit the literal directly.
nil :: Encoding
nil = Encoding (TLB.fromText "nil")


bool :: Bool -> Encoding
bool True = Encoding (TLB.fromText "true")
bool False = Encoding (TLB.fromText "false")


integer :: Integer -> Encoding
integer n = Encoding (TLB.fromString (show n))


int :: Int -> Encoding
int n = Encoding (TLB.fromString (show n))


double :: Double -> Encoding
double d
  | isNaN d = Encoding (TLB.fromText "##NaN")
  | isInfinite d && d > 0 = Encoding (TLB.fromText "##Inf")
  | isInfinite d = Encoding (TLB.fromText "##-Inf")
  | otherwise = Encoding (TLB.fromString (show d))


float :: Float -> Encoding
float = double . realToFrac


{- | Render a 'Text' as an EDN string, with full escaping. We round-trip
through 'EDN.Encode' to share the SIMD-accelerated escape scanner.
-}
string :: Text -> Encoding
string t = Encoding (TLB.fromText (EE.encode (E.String t)))


lazyString :: TL.Text -> Encoding
lazyString = string . TL.toStrict


char :: Char -> Encoding
char c = Encoding (TLB.fromText (EE.encode (E.Char c)))


keyword :: Maybe Text -> Text -> Encoding
keyword ns name = Encoding (TLB.fromText (EE.encode (E.Keyword ns name)))


symbol :: Maybe Text -> Text -> Encoding
symbol ns name = Encoding (TLB.fromText (EE.encode (E.Symbol ns name)))


list :: Foldable f => f Encoding -> Encoding
list xs = wrapped '(' ')' xs


listFromList :: [Encoding] -> Encoding
listFromList = list


vector :: Foldable f => f Encoding -> Encoding
vector xs = wrapped '[' ']' xs


vectorFromList :: [Encoding] -> Encoding
vectorFromList = vector


set :: Foldable f => f Encoding -> Encoding
set xs =
  Encoding
    ( TLB.fromText "#{"
        <> commaSep (fmap runEncoding (toL xs))
        <> TLB.singleton '}'
    )


setFromList :: [Encoding] -> Encoding
setFromList = set


map_ :: Foldable f => f (Encoding, Encoding) -> Encoding
map_ kvs =
  let go acc (k, v) = case acc of
        Nothing -> Just (runEncoding k <> TLB.singleton ' ' <> runEncoding v)
        Just b -> Just (b <> TLB.singleton ' ' <> runEncoding k <> TLB.singleton ' ' <> runEncoding v)
      body = case foldl' go Nothing kvs of
        Nothing -> mempty
        Just b -> b
  in Encoding (TLB.singleton '{' <> body <> TLB.singleton '}')


mapList :: [(Encoding, Encoding)] -> Encoding
mapList = map_


tagged :: Maybe Text -> Text -> Encoding -> Encoding
tagged ns tag inner =
  let prefix = case ns of
        Nothing -> TLB.singleton '#' <> TLB.fromText tag
        Just n -> TLB.singleton '#' <> TLB.fromText n <> TLB.singleton '/' <> TLB.fromText tag
  in Encoding (prefix <> TLB.singleton ' ' <> runEncoding inner)


-- helpers --------------------------------------------------------------

wrapped :: Foldable f => Char -> Char -> f Encoding -> Encoding
wrapped opener closer xs =
  Encoding
    ( TLB.singleton opener
        <> commaSep (fmap runEncoding (toL xs))
        <> TLB.singleton closer
    )


toL :: Foldable f => f a -> [a]
toL = foldr (:) []


-- EDN uses spaces (or commas, treated as whitespace) between
-- collection elements; we use a single space.
commaSep :: [Builder] -> Builder
commaSep [] = mempty
commaSep [b] = b
commaSep (b : bs) = b <> mconcat (fmap (TLB.singleton ' ' <>) bs)
