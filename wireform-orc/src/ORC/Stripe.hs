{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
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

import ORC.Proto.Schema
import ORC.Types (StripeInformation (..))

-- | One physical stream inside a stripe (see ORC @Stream@ protobuf).
data Stream = Stream
  { stKind   :: !Word64
  , stColumn :: !Word64
  , stLength :: !Word64
  } deriving stock (Show, Eq)

-- | Parsed @StripeFooter@ (streams only; column encodings
-- omitted for now — separate work to wire that through).
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
decodeStripeFooter bs = StripeFooter <$> go 0 V.empty
  where
    !len = BS.length bs
    go !off !acc
      | off >= len = Right acc
      | otherwise = do
          (tag, off1) <- getVarint bs off len
          let !fn = fromIntegral (tag `shiftR` 3) :: Int
              !wt = tag .&. 7
          case (fn, wt) of
            StripeFooter_Streams -> do
              (chunk, off2) <- getLenDelim bs off1 len
              st <- decodeStream chunk
              go off2 (V.snoc acc st)
            _ -> skipField wt bs off1 len >>= \off2 -> go off2 acc

decodeStream :: ByteString -> Either String Stream
decodeStream bs = go 0 (Stream 0 0 0)
  where
    !len = BS.length bs
    go !off !st
      | off >= len = Right st
      | otherwise = do
          (tag, off') <- getVarint bs off len
          let !fn = fromIntegral (tag `shiftR` 3) :: Int
              !wt = tag .&. 7
              readV f = do
                (v, off'') <- getVarint bs off' len
                go off'' (f v)
          case (fn, wt) of
            Stream_Kind   -> readV $ \v -> st { stKind   = v }
            Stream_Column -> readV $ \v -> st { stColumn = v }
            Stream_Length -> readV $ \v -> st { stLength = v }
            _             -> skipField wt bs off' len >>= \off'' -> go off'' st

-- Decoder primitives (getVarint, getLenDelim, skipField) come from
-- "ORC.Proto.Schema"; so do the encoder helpers. Both sides of this
-- module share the same named-field codec.

-- ============================================================
-- Protobuf encoding
-- ============================================================

-- | Encode a 'StripeFooter' as protobuf bytes.
encodeStripeFooter :: StripeFooter -> ByteString
encodeStripeFooter (StripeFooter streams) =
  BL.toStrict $ B.toLazyByteString $
    V.foldl' (\acc s -> acc <> encodeLengthDelim StripeFooter_Streams
                                (encodeStream s))
      mempty streams

-- | Encode a single 'Stream' as protobuf bytes.
encodeStream :: Stream -> B.Builder
encodeStream (Stream kind col len) = mconcat
  [ encodeVarintField Stream_Kind   kind
  , encodeVarintField Stream_Column col
  , encodeVarintField Stream_Length len
  ]
