{-# LANGUAGE BangPatterns #-}
-- | XXH64 (xxHash) — the hash function used by Parquet's split-block bloom filter.
--
-- Implements the xxHash 0.1.1 algorithm (Yann Collet, 2014):
-- <https://github.com/Cyan4973/xxHash/blob/dev/doc/xxhash_spec.md>.
--
-- Parquet's 'Parquet.BloomFilter' uses 'xxh64' with seed @0@ on a value's
-- @PLAIN@ encoding. The function is byte-exact with reference C / Rust /
-- Java implementations and produces the same digests.
module Parquet.XXH64
  ( xxh64
  , xxh64Seed
    -- * Pure reference implementation (benchmark companion)
  , xxh64_pure
  , xxh64Seed_pure
  ) where

import Data.Bits (rotateL, shiftR, unsafeShiftL, xor, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word32, Word64)

import qualified Wireform.Hash as WH

prime1, prime2, prime3, prime4, prime5 :: Word64
prime1 = 0x9E3779B185EBCA87
prime2 = 0xC2B2AE3D27D4EB4F
prime3 = 0x165667B19E3779F9
prime4 = 0x85EBCA77C2B2AE63
prime5 = 0x27D4EB2F165667C5

-- | XXH64 with the default Parquet seed of @0@. Backed by the C/SIMDe
-- kernel in @wireform-core@ (~3-50x faster than the pure reference
-- depending on input length; see @wireform-iceberg/bench/RESULTS.md@).
{-# INLINE xxh64 #-}
xxh64 :: ByteString -> Word64
xxh64 = WH.xxh64 0

-- | XXH64 with an explicit seed. Same C kernel.
xxh64Seed :: Word64 -> ByteString -> Word64
xxh64Seed = WH.xxh64
{-# INLINE xxh64Seed #-}

-- | Pure-Haskell XXH64 reference. Equivalent to 'xxh64' but compiled by
-- GHC without any C calls; used by the parquet bench to measure speedup.
{-# INLINE xxh64_pure #-}
xxh64_pure :: ByteString -> Word64
xxh64_pure = xxh64Seed_pure 0

-- | Pure-Haskell XXH64 reference with an explicit seed.
xxh64Seed_pure :: Word64 -> ByteString -> Word64
xxh64Seed_pure seed bs =
  let !len = BS.length bs
      !nStripes = len `shiftR` 5
      !tailOff = nStripes `unsafeShiftL` 5
      !h0 = if len >= 32 then bulkPhase seed bs nStripes else seed + prime5
      !h1 = h0 + fromIntegral len
  in finalize h1 tailOff bs len

bulkPhase :: Word64 -> ByteString -> Int -> Word64
bulkPhase seed bs nStripes =
  let !v1_0 = seed + prime1 + prime2
      !v2_0 = seed + prime2
      !v3_0 = seed
      !v4_0 = seed - prime1
      go !i !off !a1 !a2 !a3 !a4
        | i >= nStripes = (a1, a2, a3, a4)
        | otherwise =
            let !w1 = readLE64 bs off
                !w2 = readLE64 bs (off + 8)
                !w3 = readLE64 bs (off + 16)
                !w4 = readLE64 bs (off + 24)
            in go (i + 1) (off + 32)
                  (round_ a1 w1) (round_ a2 w2)
                  (round_ a3 w3) (round_ a4 w4)
      (!fv1, !fv2, !fv3, !fv4) = go 0 0 v1_0 v2_0 v3_0 v4_0
      !merged =
          rotateL fv1 1
        + rotateL fv2 7
        + rotateL fv3 12
        + rotateL fv4 18
      !m1 = mergeAccumulator merged fv1
      !m2 = mergeAccumulator m1 fv2
      !m3 = mergeAccumulator m2 fv3
      !m4 = mergeAccumulator m3 fv4
  in m4

finalize :: Word64 -> Int -> ByteString -> Int -> Word64
finalize !h0 !startOff bs !len = avalanche (go h0 startOff)
  where
    go !h !off
      | off + 8 <= len =
          let !k1raw = readLE64 bs off
              !k1 = round_ 0 k1raw
              !h' = rotateL (h `xor` k1) 27 * prime1 + prime4
          in go h' (off + 8)
      | off + 4 <= len =
          let !w = fromIntegral (readLE32 bs off) :: Word64
              !h' = rotateL (h `xor` (w * prime1)) 23 * prime2 + prime3
          in go h' (off + 4)
      | off < len =
          let !b = fromIntegral (BSU.unsafeIndex bs off) :: Word64
              !h' = rotateL (h `xor` (b * prime5)) 11 * prime1
          in go h' (off + 1)
      | otherwise = h

-- | Final mixing step ("avalanche") after the tail loop.
{-# INLINE avalanche #-}
avalanche :: Word64 -> Word64
avalanche !h0 =
  let !h1 = h0 `xor` (h0 `shiftR` 33)
      !h2 = h1 * prime2
      !h3 = h2 `xor` (h2 `shiftR` 29)
      !h4 = h3 * prime3
      !h5 = h4 `xor` (h4 `shiftR` 32)
  in h5

{-# INLINE round_ #-}
round_ :: Word64 -> Word64 -> Word64
round_ !acc !w = rotateL (acc + w * prime2) 31 * prime1

{-# INLINE mergeAccumulator #-}
mergeAccumulator :: Word64 -> Word64 -> Word64
mergeAccumulator !merged !v =
  let !v' = round_ 0 v
  in (merged `xor` v') * prime1 + prime4

{-# INLINE readLE64 #-}
readLE64 :: ByteString -> Int -> Word64
readLE64 bs !off =
  let rd i = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word64
  in rd 0
   .|. (rd 1 `unsafeShiftL` 8)
   .|. (rd 2 `unsafeShiftL` 16)
   .|. (rd 3 `unsafeShiftL` 24)
   .|. (rd 4 `unsafeShiftL` 32)
   .|. (rd 5 `unsafeShiftL` 40)
   .|. (rd 6 `unsafeShiftL` 48)
   .|. (rd 7 `unsafeShiftL` 56)

{-# INLINE readLE32 #-}
readLE32 :: ByteString -> Int -> Word32
readLE32 bs !off =
  let rd i = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word32
  in rd 0
   .|. (rd 1 `unsafeShiftL` 8)
   .|. (rd 2 `unsafeShiftL` 16)
   .|. (rd 3 `unsafeShiftL` 24)
