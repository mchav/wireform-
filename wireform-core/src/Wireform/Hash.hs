{-# LANGUAGE BangPatterns #-}
-- | Shared C/SIMDe-backed hash and Roaring-bitmap kernels used across the
-- wireform format packages.
--
-- - 'murmur3_32' / 'bucketLong' — byte-compatible with Apache Iceberg's
--   @org.apache.iceberg.util.BucketUtil@ (used both by the Iceberg
--   @bucket[N]@ partition transform and by the Parquet bloom filter's
--   feeder hash variants in some integrations).
-- - 'xxh64' — XXH64 with a caller-supplied seed, byte-exact against the
--   upstream xxHash 0.6.x reference. This is the hash function Parquet's
--   split-block bloom filter uses (seed @0@) and that Iceberg manifest
--   tooling uses for content-addressing.
-- - 'roaring*' — portable Roaring 32-bit container kernels used by Iceberg
--   V3 deletion vectors and (potentially) Parquet column-index null pages.
--
-- The implementations live in @cbits/wireform_hash_simd.c@. All entry
-- points are 'unsafePerformIO' / 'unsafeDupablePerformIO' wrapped so
-- callers don't think about IO.
module Wireform.Hash
  ( -- * Murmur3 32-bit (Iceberg @BucketUtil@)
    murmur3_32
  , bucketLong
    -- * XXH64
  , xxh64
    -- * Roaring 32-bit container
  , roaringDecodeArray
  , roaringDecodeBitset
  , roaringContains
  , roaringEncodeArray
  , roaringEncodeBitset
  , RoaringContainerKind(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int32, Int64)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

-- ============================================================
-- C entry points
-- ============================================================

foreign import ccall unsafe "hs_wf_murmur3_32"
  c_murmur3_32 :: Ptr Word8 -> Int32 -> Int32

foreign import ccall unsafe "hs_wf_bucket_long"
  c_bucket_long :: Int64 -> Int32 -> Int32

foreign import ccall unsafe "hs_wf_xxh64"
  c_xxh64 :: Ptr Word8 -> Int64 -> Word64 -> Word64

foreign import ccall unsafe "hs_wf_roaring_decode_array"
  c_roaring_decode_array :: Ptr Word8 -> Int32 -> Word32 -> Ptr Int32 -> Int32

foreign import ccall unsafe "hs_wf_roaring_decode_bitset"
  c_roaring_decode_bitset :: Ptr Word8 -> Word32 -> Ptr Int32 -> Int32

foreign import ccall unsafe "hs_wf_roaring_contains"
  c_roaring_contains :: Int32 -> Ptr Word8 -> Int32 -> Word16 -> Int32

foreign import ccall unsafe "hs_wf_roaring_encode_array"
  c_roaring_encode_array :: Ptr Word16 -> Int32 -> Ptr Word8 -> Int32

foreign import ccall unsafe "hs_wf_roaring_encode_bitset"
  c_roaring_encode_bitset :: Ptr Word16 -> Int32 -> Ptr Word8 -> IO ()

-- ============================================================
-- Public wrappers
-- ============================================================

-- | Murmur3 32-bit hash with seed 0, matching Iceberg's @BucketUtil.hash@.
murmur3_32 :: BS.ByteString -> Int32
murmur3_32 bs = unsafePerformIO $
  unsafeUseAsCStringLen bs $ \(p, len) ->
    pure $! c_murmur3_32 (castPtr p) (fromIntegral len)
{-# INLINE murmur3_32 #-}

-- | Iceberg @bucket[N]@ partition transform on a 64-bit signed integer.
-- The C kernel inlines the 8-byte little-endian serialisation, the
-- murmur3 hash, and the @& Integer.MAX_VALUE % N@ reduction.
bucketLong :: Int -> Int64 -> Int
bucketLong n v
  | n <= 0    = 0
  | otherwise = fromIntegral (c_bucket_long v (fromIntegral n))
{-# INLINE bucketLong #-}

-- | XXH64 with caller-supplied seed.
xxh64 :: Word64 -> BS.ByteString -> Word64
xxh64 seed bs = unsafePerformIO $
  unsafeUseAsCStringLen bs $ \(p, len) ->
    pure $! c_xxh64 (castPtr p) (fromIntegral len) seed
{-# INLINE xxh64 #-}

-- ============================================================
-- Roaring containers
-- ============================================================

data RoaringContainerKind = ArrayContainer | BitsetContainer
  deriving (Show, Eq)

-- | Decode a Roaring ARRAY container payload into a vector of 32-bit
-- positions, OR'd with @hi << 16@.
roaringDecodeArray
  :: BS.ByteString -- ^ Raw payload (cardinality * 2 bytes).
  -> Int           -- ^ Cardinality.
  -> Word32        -- ^ High 32 bits to OR onto each position.
  -> VS.Vector Int32
roaringDecodeArray bs card hi = unsafeDupablePerformIO $ do
  mv <- VSM.unsafeNew card
  VSM.unsafeWith mv $ \dst ->
    unsafeUseAsCStringLen bs $ \(src, _) -> do
      _ <- pure $! c_roaring_decode_array (castPtr src) (fromIntegral card) hi dst
      pure ()
  VS.unsafeFreeze mv
{-# INLINE roaringDecodeArray #-}

-- | Decode a Roaring BITSET container payload (exactly 8192 bytes) into
-- a vector of set positions, OR'd with @hi << 16@.
roaringDecodeBitset
  :: BS.ByteString  -- ^ 8192 raw bytes.
  -> Word32         -- ^ High 32 bits.
  -> VS.Vector Int32
roaringDecodeBitset bs hi = unsafeDupablePerformIO $ do
  mv <- VSM.unsafeNew 65536
  written <- VSM.unsafeWith mv $ \dst ->
    unsafeUseAsCStringLen bs $ \(src, _) ->
      pure $! c_roaring_decode_bitset (castPtr src) hi dst
  let n = fromIntegral written
  VS.unsafeFreeze (VSM.unsafeSlice 0 n mv)
{-# INLINE roaringDecodeBitset #-}

-- | Test whether a 16-bit low value is present in a Roaring container.
roaringContains
  :: RoaringContainerKind
  -> BS.ByteString
  -> Int           -- ^ Cardinality (only used for ARRAY).
  -> Word16
  -> Bool
roaringContains kind bs card v = unsafePerformIO $
  unsafeUseAsCStringLen bs $ \(p, _) ->
    let !k = case kind of ArrayContainer -> 0; BitsetContainer -> 1
    in pure $! c_roaring_contains k (castPtr p) (fromIntegral card) v /= 0
{-# INLINE roaringContains #-}

-- | Encode a sorted vector of 16-bit positions as a Roaring ARRAY
-- container payload.
roaringEncodeArray :: VS.Vector Word16 -> BS.ByteString
roaringEncodeArray vs = unsafePerformIO $ do
  let !len = VS.length vs
      !sz  = len * 2
  fp <- BSI.mallocByteString sz
  withForeignPtr fp $ \dst ->
    VS.unsafeWith vs $ \src ->
      do _ <- pure $! c_roaring_encode_array src (fromIntegral len) dst
         pure ()
  pure (BSI.BS fp sz)
{-# INLINE roaringEncodeArray #-}

-- | Encode a sorted vector of 16-bit positions as a Roaring BITSET
-- container payload (exactly 8192 bytes).
roaringEncodeBitset :: VS.Vector Word16 -> BS.ByteString
roaringEncodeBitset vs = unsafePerformIO $ do
  fp <- BSI.mallocByteString 8192
  withForeignPtr fp $ \dst ->
    VS.unsafeWith vs $ \src ->
      c_roaring_encode_bitset src (fromIntegral (VS.length vs)) dst
  pure (BSI.BS fp 8192)
{-# INLINE roaringEncodeBitset #-}
