{-# LANGUAGE BangPatterns #-}
-- | ORC stripe footer (protobuf) — stream layout within a stripe.
--
-- Decodes the trailing protobuf @StripeFooter@ of a stripe (the last
-- @siFooterLength@ bytes). Individual stream payloads remain in the stripe
-- slice; use 'streamSlice' to extract bytes for a decoded 'Stream'.
module ORC.Stripe
  ( Stream (..)
  , StripeFooter (..)
  , decodeStripeFooter
  , stripeFooterBytes
  , streamSlice
  , stripeStreamSlices
    -- * Encoding
  , encodeStripeFooter
  , encodeStream
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word64)
import qualified Data.Vector as V

import ORC.Types (StripeInformation (..))

-- | One physical stream inside a stripe (see ORC @Stream@ protobuf).
data Stream = Stream
  { stKind   :: !Word64
  , stColumn :: !Word64
  , stLength :: !Word64
  } deriving stock (Show, Eq)

-- | Parsed @StripeFooter@ (streams only; column encodings omitted for now).
newtype StripeFooter = StripeFooter
  { sfStreams :: V.Vector Stream
  } deriving stock (Show, Eq)

-- | Take the stripe-footer protobuf bytes from a full stripe blob.
stripeFooterBytes :: ByteString -> StripeInformation -> Either String ByteString
stripeFooterBytes stripeBs si =
  let !flen = fromIntegral (siFooterLength si) :: Int
      !n = BS.length stripeBs
  in if flen <= 0 || flen > n
    then Left "ORC.Stripe: invalid stripe footer length"
    else Right $! BS.take flen (BS.drop (n - flen) stripeBs)

-- | Slice @stLength@ bytes for this stream starting at @offset@ within @stripeBs@.
streamSlice :: ByteString -> Word64 -> Word64 -> Either String ByteString
streamSlice stripeBs !offset !len =
  let !o = fromIntegral offset :: Int
      !l = fromIntegral len :: Int
      !n = BS.length stripeBs
  in if o < 0 || l < 0 || o + l > n
    then Left "ORC.Stripe: stream slice out of bounds"
    else Right $! BS.take l (BS.drop o stripeBs)

-- | Walk streams in @StripeFooter@ order and slice each payload from the start
-- of @stripeBs@ (index + data region; caller supplies the full stripe blob).
stripeStreamSlices :: ByteString -> StripeFooter -> Either String (V.Vector (Stream, ByteString))
stripeStreamSlices stripeBs (StripeFooter streams) = go 0 0 V.empty
  where
    go !i !pos !acc
      | i >= V.length streams = Right acc
      | otherwise =
          let st = V.unsafeIndex streams i
              !l = stLength st
          in case streamSlice stripeBs pos l of
            Left e -> Left e
            Right chunk ->
              go (i + 1) (pos + l) (V.snoc acc (st, chunk))

-- | Parse protobuf @StripeFooter@ (field 1: repeated Stream).
decodeStripeFooter :: ByteString -> Either String StripeFooter
decodeStripeFooter bs = go 0 V.empty
  where
    !len = BS.length bs
    go !off !acc
      | off >= len = Right (StripeFooter acc)
      | otherwise = do
          (tag, off1) <- getVarint bs off len
          let !fieldNum = fromIntegral (tag `shiftR` 3) :: Int
              !wireType = tag .&. 7
          case (fieldNum, wireType) of
            (1, 2) -> do
              (chunk, off2) <- getLenDelim bs off1 len
              st <- decodeStream chunk
              go off2 (V.snoc acc st)
            _ -> skipField wireType bs off1 len >>= \off2 -> go off2 acc

decodeStream :: ByteString -> Either String Stream
decodeStream bs = go 0 (Stream 0 0 0)
  where
    !len = BS.length bs
    go !off !st
      | off >= len = Right st
      | otherwise = do
          (tag, off') <- getVarint bs off len
          let !fieldNum = fromIntegral (tag `shiftR` 3) :: Int
              !wireType = tag .&. 7
          case (fieldNum, wireType) of
            (1, 0) -> do (v, off'') <- getVarint bs off' len; go off'' st { stKind = v }
            (2, 0) -> do (v, off'') <- getVarint bs off' len; go off'' st { stColumn = v }
            (3, 0) -> do (v, off'') <- getVarint bs off' len; go off'' st { stLength = v }
            _ -> skipField wireType bs off' len >>= \off'' -> go off'' st

getVarint :: ByteString -> Int -> Int -> Either String (Word64, Int)
getVarint bs !off !len = go off 0 0
  where
    go !pos !val !shift
      | pos >= len = Left "ORC.Stripe: truncated varint"
      | shift >= 64 = Left "ORC.Stripe: varint too long"
      | otherwise =
          let !b = fromIntegral (BSU.unsafeIndex bs pos) :: Word64
              !val' = val .|. ((b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
              then Right (val', pos + 1)
              else go (pos + 1) val' (shift + 7)

getLenDelim :: ByteString -> Int -> Int -> Either String (ByteString, Int)
getLenDelim bs !off !len = do
  (dlen, off') <- getVarint bs off len
  let !dataLen = fromIntegral dlen :: Int
  if off' + dataLen > len
    then Left "ORC.Stripe: length-delimited overflow"
    else Right (BS.take dataLen (BS.drop off' bs), off' + dataLen)

skipField :: Word64 -> ByteString -> Int -> Int -> Either String Int
skipField wireType bs !off !len = case wireType of
  0 -> do (_, off') <- getVarint bs off len; Right off'
  1 -> if off + 8 <= len then Right (off + 8) else Left "ORC.Stripe: skip fixed64"
  2 -> do (_, off') <- getLenDelim bs off len; Right off'
  5 -> if off + 4 <= len then Right (off + 4) else Left "ORC.Stripe: skip fixed32"
  _ -> Left $ "ORC.Stripe: unknown wire type " ++ show wireType

------------------------------------------------------------------------
-- Protobuf encoding
------------------------------------------------------------------------

-- | Encode a 'StripeFooter' as protobuf bytes.
encodeStripeFooter :: StripeFooter -> ByteString
encodeStripeFooter (StripeFooter streams) =
  BL.toStrict $ B.toLazyByteString $
    V.foldl' (\acc s -> acc <> pbLenDelim 1 (encodeStream s)) mempty streams

-- | Encode a single 'Stream' as protobuf bytes.
encodeStream :: Stream -> B.Builder
encodeStream (Stream kind col len) = mconcat
  [ pbVarintField 1 kind
  , pbVarintField 2 col
  , pbVarintField 3 len
  ]

pbVarintField :: Int -> Word64 -> B.Builder
pbVarintField fieldNum val =
  let !tag = fromIntegral fieldNum `shiftL` 3 :: Word64
  in pbVarint tag <> pbVarint val

pbLenDelim :: Int -> B.Builder -> B.Builder
pbLenDelim fieldNum content =
  let !tag = (fromIntegral fieldNum `shiftL` 3) .|. 2 :: Word64
      !encoded = BL.toStrict $ B.toLazyByteString content
      !contentLen = BS.length encoded
  in pbVarint tag <> pbVarint (fromIntegral contentLen) <> B.byteString encoded

pbVarint :: Word64 -> B.Builder
pbVarint = go
  where
    go !v
      | v < 0x80  = B.word8 (fromIntegral v)
      | otherwise = B.word8 (fromIntegral (v .&. 0x7F) .|. 0x80) <> go (v `shiftR` 7)
