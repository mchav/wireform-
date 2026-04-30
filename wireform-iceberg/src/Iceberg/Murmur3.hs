{-# LANGUAGE BangPatterns #-}
-- | Murmur3 32-bit hash, byte-compatible with Apache Iceberg's
-- @org.apache.iceberg.util.BucketUtil@.
--
-- Iceberg uses a non-standard variant of Murmur3 32-bit: the seed is fixed
-- to @0@, and the hash is computed over a canonical binary representation
-- of the value (e.g. little-endian 8-byte long, UTF-8 string, IEEE 754 long
-- bits for floats). The bucket transform is then @(hash & Integer.MAX_VALUE) % N@.
module Iceberg.Murmur3
  ( murmur3_32
  , bucketHash
  , bucketIndex
    -- * Source-typed helpers
  , bucketInt
  , bucketLong
  , bucketString
  , bucketBytes
    -- * Pure reference implementation (benchmark companion)
  , murmur3_32_pure
  , bucketLong_pure
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word8)

import qualified Iceberg.SIMD as SIMD

-- | Murmur3 32-bit hash, seeded to zero, matching the Iceberg specification.
--
-- Returns a signed 'Int32' so the result can be combined with @& Integer.MAX_VALUE@
-- exactly as the Java @BucketUtil.hash@ does. Backed by the C kernel in
-- @cbits/iceberg_simd.c@ (~10x faster than the pure reference at ~64 bytes
-- per call).
murmur3_32 :: ByteString -> Int32
murmur3_32 = SIMD.murmur3_32
{-# INLINE murmur3_32 #-}

-- | Pure-Haskell reference. Equivalent to 'murmur3_32' but compiled by GHC
-- without any C calls; used by the benchmark suite to measure the speedup.
murmur3_32_pure :: ByteString -> Int32
murmur3_32_pure bs = fromIntegral (go 0 (BS.length bs) 0 :: Word32)
  where
    !c1 = 0xcc9e2d51 :: Word32
    !c2 = 0x1b873593 :: Word32
    !len = BS.length bs

    -- Body: process full 4-byte blocks
    go :: Word32 -> Int -> Int -> Word32
    go !h !remaining !off
      | remaining >= 4 =
          let k = readWord32LE bs off
              k1 = (k * c1) `rotateL32` 15 * c2
              h1 = ((h `xor` k1) `rotateL32` 13) * 5 + 0xe6546b64
          in go h1 (remaining - 4) (off + 4)
      | remaining > 0 =
          let k = case remaining of
                3 -> let !b0 = fromIntegral (BS.index bs off)        :: Word32
                         !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
                         !b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
                     in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
                2 -> let !b0 = fromIntegral (BS.index bs off)        :: Word32
                         !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
                     in b0 .|. (b1 `shiftL` 8)
                1 -> fromIntegral (BS.index bs off)
                _ -> 0
              k1 = ((k * c1) `rotateL32` 15) * c2
          in finalize (h `xor` k1)
      | otherwise = finalize h

    finalize :: Word32 -> Word32
    finalize !h =
      let h1 = h `xor` fromIntegral len
          h2 = h1 `xor` (h1 `shiftR` 16)
          h3 = h2 * 0x85ebca6b
          h4 = h3 `xor` (h3 `shiftR` 13)
          h5 = h4 * 0xc2b2ae35
      in h5 `xor` (h5 `shiftR` 16)

rotateL32 :: Word32 -> Int -> Word32
rotateL32 = rotateL
{-# INLINE rotateL32 #-}

readWord32LE :: ByteString -> Int -> Word32
readWord32LE bs off =
  let !b0 = fromIntegral (BS.index bs off)        :: Word32
      !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
      !b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
      !b3 = fromIntegral (BS.index bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
{-# INLINE readWord32LE #-}

-- | Iceberg's @hash@ helper: seed-zero murmur3 of the canonical binary form.
-- Re-exported as a friendly name for callers serialising values themselves.
bucketHash :: ByteString -> Int32
bucketHash = murmur3_32
{-# INLINE bucketHash #-}

-- | @(hash & Integer.MAX_VALUE) % N@.
bucketIndex :: Int32 -> Int -> Int
bucketIndex h n = fromIntegral (fromIntegral h .&. (0x7FFFFFFF :: Word32))
                  `mod` n
{-# INLINE bucketIndex #-}

-- | Bucket an int (or date) value: hashes its little-endian 8-byte
-- @long@ representation, matching @BucketUtil.hash(int)@.
bucketInt :: Int -> Int32 -> Int
bucketInt n v = SIMD.bucketLong n (fromIntegral v)
{-# INLINE bucketInt #-}

-- | Bucket a long\/timestamp value, delegated to the C kernel which inlines
-- the 8-byte little-endian serialisation, the murmur3 hash, and the
-- @& Integer.MAX_VALUE % N@ reduction.
bucketLong :: Int -> Int64 -> Int
bucketLong = SIMD.bucketLong
{-# INLINE bucketLong #-}

-- | Pure-Haskell reference for 'bucketLong', used by the bench.
bucketLong_pure :: Int -> Int64 -> Int
bucketLong_pure n v = bucketIndex (bucketLongHashPure v) n

-- Utility: 8-byte little-endian encoding for ints/longs.
bucketLongHashPure :: Int64 -> Int32
bucketLongHashPure v =
  let bs = BL.toStrict (BB.toLazyByteString (BB.int64LE v))
  in murmur3_32_pure bs

-- | Bucket a string by hashing its UTF-8 bytes.
bucketString :: Int -> Text -> Int
bucketString n t = bucketIndex (murmur3_32 (TE.encodeUtf8 t)) n

-- | Bucket a binary value by hashing its bytes.
bucketBytes :: Int -> ByteString -> Int
bucketBytes n bs = bucketIndex (murmur3_32 bs) n

-- Helpers (suppress unused warning if BB.flush is removed from imports).
_unused :: Word8 -> Word8
_unused = id

_unusedFlush :: BB.Builder -> BB.Builder
_unusedFlush = id
