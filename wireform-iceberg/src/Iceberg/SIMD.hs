{-# LANGUAGE BangPatterns #-}
-- | C/SIMDe-backed kernels used on Iceberg's hot paths.
--
-- These are byte-for-byte equivalent to the pure-Haskell implementations in
-- "Iceberg.Murmur3" and "Iceberg.DeletionVector"; they exist purely to win
-- back the order-of-magnitude penalty of running tight bit-twiddling loops
-- through GHC's general-purpose runtime.
--
-- All entry points operate on pinned 'ByteString' / 'Storable.Vector' memory
-- and are 'unsafePerformIO' / 'unsafeDupablePerformIO' wrapped so callers
-- need not think about IO. See @cbits/iceberg_simd.c@ for the kernels and
-- "Iceberg.SIMD.Bench" for the criterion comparison.
module Iceberg.SIMD
  ( -- * Murmur3 32-bit (Iceberg @BucketUtil@)
    murmur3_32
  , bucketLong
    -- * XXH64 (Parquet bloom filter, Iceberg manifest hash)
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
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)

-- ============================================================
-- C entry points
-- ============================================================

foreign import ccall unsafe "hs_iceberg_murmur3_32"
  c_murmur3_32 :: Ptr Word8 -> Int32 -> Int32

foreign import ccall unsafe "hs_iceberg_bucket_long"
  c_bucket_long :: Int64 -> Int32 -> Int32

foreign import ccall unsafe "hs_iceberg_xxh64"
  c_xxh64 :: Ptr Word8 -> Int64 -> Word64 -> Word64

foreign import ccall unsafe "hs_iceberg_roaring_decode_array"
  c_roaring_decode_array :: Ptr Word8 -> Int32 -> Word32 -> Ptr Int32 -> Int32

foreign import ccall unsafe "hs_iceberg_roaring_decode_bitset"
  c_roaring_decode_bitset :: Ptr Word8 -> Word32 -> Ptr Int32 -> Int32

foreign import ccall unsafe "hs_iceberg_roaring_contains"
  c_roaring_contains :: Int32 -> Ptr Word8 -> Int32 -> Word16 -> Int32

foreign import ccall unsafe "hs_iceberg_roaring_encode_array"
  c_roaring_encode_array :: Ptr Word16 -> Int32 -> Ptr Word8 -> Int32

foreign import ccall unsafe "hs_iceberg_roaring_encode_bitset"
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

-- | Iceberg @bucket[N]@ partition transform on a 64-bit signed integer
-- (also covers @int@, @date@, and @timestamp@ source types after promotion).
-- This is the function called per-row in writers and per-literal in
-- predicate projection.
bucketLong :: Int -> Int64 -> Int
bucketLong n v
  | n <= 0    = 0
  | otherwise = fromIntegral (c_bucket_long v (fromIntegral n))
{-# INLINE bucketLong #-}

-- | XXH64 with caller-supplied seed. Iceberg uses seed @0@ for manifest
-- hashing; Parquet split-block bloom filters use seed @0@ as well.
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
-- positions, OR'd with @hi << 16@. The input is the raw payload bytes
-- only (no cookie/header).
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

-- | Decode a Roaring BITSET container payload (exactly 8192 bytes) into a
-- vector of set positions, OR'd with @hi << 16@. The result length is the
-- bitmap's popcount; the caller may want to call 'VS.length' to discover it.
roaringDecodeBitset
  :: BS.ByteString  -- ^ 8192 raw bytes.
  -> Word32         -- ^ High 32 bits.
  -> VS.Vector Int32
roaringDecodeBitset bs hi = unsafeDupablePerformIO $ do
  -- Worst case is 65536 set bits (16 bits per chunk). Allocate the upper
  -- bound, then truncate after the kernel reports how many it wrote.
  mv <- VSM.unsafeNew 65536
  written <- VSM.unsafeWith mv $ \dst ->
    unsafeUseAsCStringLen bs $ \(src, _) ->
      pure $! c_roaring_decode_bitset (castPtr src) hi dst
  let n = fromIntegral written
  VS.unsafeFreeze (VSM.unsafeSlice 0 n mv)
{-# INLINE roaringDecodeBitset #-}

-- | Test whether a 16-bit low value is present in a Roaring container of
-- the given kind. ARRAY containers are sorted; the C kernel uses a SIMD
-- linear scan for small cardinalities and binary search for larger ones.
roaringContains
  :: RoaringContainerKind
  -> BS.ByteString -- ^ Container payload.
  -> Int           -- ^ Cardinality (only used for ARRAY).
  -> Word16        -- ^ Position low-16 bits.
  -> Bool
roaringContains kind bs card v = unsafePerformIO $
  unsafeUseAsCStringLen bs $ \(p, _) ->
    let !k = case kind of ArrayContainer -> 0; BitsetContainer -> 1
    in pure $! c_roaring_contains k (castPtr p) (fromIntegral card) v /= 0
{-# INLINE roaringContains #-}

-- | Encode a sorted vector of 16-bit positions as a Roaring ARRAY
-- container payload. Returns the raw bytes (cardinality * 2 long).
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

-- ============================================================
-- (Compile-time checks: keep referenced types alive when imports above
-- are pruned by GHC.)
-- ============================================================

_unused1 :: Ptr a -> Ptr a
_unused1 = (`plusPtr` 0)

_unused2 :: CSize
_unused2 = 0

_unused3 :: Ptr a -> IO a -> IO a
_unused3 _ io = do
  allocaBytes 0 $ \(_ :: Ptr ()) -> pure ()
  with (0 :: Int) $ \p -> do
    _ <- peek p
    poke p 1
  io

_unused4 :: CInt -> CInt
_unused4 = id
