{-# LANGUAGE BangPatterns #-}

{- | Direct-to-bytes Bencode encoding, mirroring aeson's @toEncoding@
approach.
-}
module Bencode.Encoding (
  Encoding (..),
  encodingToBuilder,
  encodingToLazyByteString,
  encodingToByteString,
  bytes,
  lazyBytes,
  text,
  integer,
  int,
  bool,
  list,
  listFromList,
  dict,
  dictFromList,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (foldl', toList)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Wireform.Builder qualified as BB


newtype Encoding = Encoding {runEncoding :: BB.Builder}


encodingToBuilder :: Encoding -> BB.Builder
encodingToBuilder = runEncoding


encodingToLazyByteString :: Encoding -> BSL.ByteString
encodingToLazyByteString = BB.toLazyByteString . runEncoding


encodingToByteString :: Encoding -> ByteString
encodingToByteString = BB.toStrictByteString . runEncoding


bytes :: ByteString -> Encoding
bytes !bs =
  let !len = BS.length bs
  in Encoding (BB.intDec len <> BB.char7 ':' <> BB.byteString bs)


lazyBytes :: BSL.ByteString -> Encoding
lazyBytes !bs =
  let !len = fromIntegral (BSL.length bs) :: Int
  in Encoding (BB.intDec len <> BB.char7 ':' <> foldMap BB.byteString (BSL.toChunks bs))


text :: Text -> Encoding
text = bytes . TE.encodeUtf8


integer :: Integer -> Encoding
integer n = Encoding (BB.char7 'i' <> BB.byteString (BS8.pack (show n)) <> BB.char7 'e')


int :: Int -> Encoding
int n = Encoding (BB.char7 'i' <> BB.intDec n <> BB.char7 'e')


bool :: Bool -> Encoding
bool True = Encoding (BB.byteString "i1e")
bool False = Encoding (BB.byteString "i0e")


list :: Foldable f => f Encoding -> Encoding
list xs = Encoding (BB.char7 'l' <> foldl' (\b e -> b <> runEncoding e) mempty xs <> BB.char7 'e')


listFromList :: [Encoding] -> Encoding
listFromList xs = Encoding (BB.char7 'l' <> mconcat (fmap runEncoding xs) <> BB.char7 'e')


{- | Bencode dicts must be sorted by raw byte-string key (BEP-3 \xA73).
The pairs are sorted on the way out so callers can pass them in
any order they like.
-}
dict :: Foldable f => f (ByteString, Encoding) -> Encoding
dict = dictFromList . toList


dictFromList :: [(ByteString, Encoding)] -> Encoding
dictFromList kvs =
  let !sorted = sortBy (comparing fst) kvs
  in Encoding
      ( BB.char7 'd'
          <> foldl' (\b (k, v) -> b <> runEncoding (bytes k) <> runEncoding v) mempty sorted
          <> BB.char7 'e'
      )
