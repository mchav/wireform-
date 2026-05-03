{-# LANGUAGE BangPatterns #-}
-- | Direct-to-bytes Bencode encoding, mirroring aeson's @toEncoding@
-- approach.
module Bencode.Encoding
  ( Encoding (..)
  , encodingToBuilder
  , encodingToLazyByteString
  , encodingToByteString
  , bytes
  , lazyBytes
  , text
  , integer
  , int
  , bool
  , list
  , listFromList
  , dict
  , dictFromList
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (foldl')
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

newtype Encoding = Encoding { runEncoding :: BB.Builder }

encodingToBuilder :: Encoding -> BB.Builder
encodingToBuilder = runEncoding

encodingToLazyByteString :: Encoding -> BSL.ByteString
encodingToLazyByteString = BB.toLazyByteString . runEncoding

encodingToByteString :: Encoding -> ByteString
encodingToByteString = BSL.toStrict . encodingToLazyByteString

bytes :: ByteString -> Encoding
bytes !bs =
  let !len = BS.length bs
  in Encoding (BB.intDec len <> BB.char7 ':' <> BB.byteString bs)

lazyBytes :: BSL.ByteString -> Encoding
lazyBytes !bs =
  let !len = fromIntegral (BSL.length bs) :: Int
  in Encoding (BB.intDec len <> BB.char7 ':' <> BB.lazyByteString bs)

text :: Text -> Encoding
text = bytes . TE.encodeUtf8

integer :: Integer -> Encoding
integer n = Encoding (BB.char7 'i' <> BB.byteString (BS8.pack (show n)) <> BB.char7 'e')

int :: Int -> Encoding
int n = Encoding (BB.char7 'i' <> BB.intDec n <> BB.char7 'e')

bool :: Bool -> Encoding
bool True  = Encoding (BB.byteString "i1e")
bool False = Encoding (BB.byteString "i0e")

list :: Foldable f => f Encoding -> Encoding
list xs = Encoding (BB.char7 'l' <> foldl' (\b e -> b <> runEncoding e) mempty xs <> BB.char7 'e')

listFromList :: [Encoding] -> Encoding
listFromList xs = Encoding (BB.char7 'l' <> mconcat (fmap runEncoding xs) <> BB.char7 'e')

-- | Bencode dicts must be sorted by key. The caller is responsible
-- for ensuring the input is sorted.
dict :: Foldable f => f (ByteString, Encoding) -> Encoding
dict kvs =
  Encoding (BB.char7 'd'
              <> foldl' (\b (k, v) -> b <> runEncoding (bytes k) <> runEncoding v) mempty kvs
              <> BB.char7 'e')

dictFromList :: [(ByteString, Encoding)] -> Encoding
dictFromList = dict
