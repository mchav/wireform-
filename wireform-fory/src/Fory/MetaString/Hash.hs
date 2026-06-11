{-# LANGUAGE BangPatterns #-}

{- | Pure Haskell port of the hash routines pyfory uses to compute
meta-string @hashcode@ values.

* @hashSmallMetaString@: a custom 64-bit mix-hash applied when
  the encoded data is @<=@ 16 bytes; the low 8 bits carry the
  encoding tag verbatim.

* @murmurHash3X64_128_low64@: the low 64 bits of MurmurHash3's
  x64-128 variant (seed 47), applied when the encoded data is
  @>@ 16 bytes; the low 8 bits are then overwritten with the
  encoding tag.

These match @pyfory.context.hash_small_metastring@ and
@pyfory.lib.mmh3.hash_buffer@ respectively.
-}
module Fory.MetaString.Hash (
  hashSmallMetaString,
  murmur3X64_128,
  metaStringHashcode,
) where

import Data.Bits (rotateL, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word64)


-- ---------------------------------------------------------------------------
-- 64-bit finalizer used by both algorithms
-- ---------------------------------------------------------------------------

fmix64 :: Word64 -> Word64
fmix64 !k0 =
  let !k1 = k0 `xor` (k0 `shiftR` 33)
      !k2 = k1 * 0xff51afd7ed558ccd
      !k3 = k2 `xor` (k2 `shiftR` 33)
      !k4 = k3 * 0xc4ceb9fe1a85ec53
      !k5 = k4 `xor` (k4 `shiftR` 33)
  in k5
{-# INLINE fmix64 #-}


-- ---------------------------------------------------------------------------
-- Small-meta-string hash (custom, used for length <= 16)
-- ---------------------------------------------------------------------------

{- | Hash a small meta string. @v1@ and @v2@ are the first 8 bytes
and bytes [8, 16) of the encoded data, padded with zero bytes
to 16 bytes total. The low 8 bits of the result are the
encoding tag.
-}
hashSmallMetaString
  :: Word64
  -- ^ v1 (first 8 bytes, little-endian, zero-padded)
  -> Word64
  -- ^ v2 (next 8 bytes, little-endian, zero-padded)
  -> Int
  -- ^ length in bytes (0..16)
  -> Word64
  -- ^ encoding tag (0..4)
  -> Word64
hashSmallMetaString v1 v2 len enc =
  let kk = 0x9E3779B97F4A7C15 :: Word64
      x0 = v1 `xor` (v2 * kk)
      x1 = x0 `xor` (fromIntegral len `shiftL` 56)
      h = fmix64 x1
  in (h .&. 0xFFFFFFFFFFFFFF00) .|. (enc .&. 0xFF)


-- | Compute (v1, v2) from up to 16 bytes of data, zero-padded.
unpackSmall :: ByteString -> (Word64, Word64)
unpackSmall bs =
  let bs16 = BS.take 16 bs `BS.append` BS.replicate (16 - min 16 (BS.length bs)) 0
      v1 = readWord64LEUnsafe bs16 0
      v2 = readWord64LEUnsafe bs16 8
  in (v1, v2)


-- ---------------------------------------------------------------------------
-- MurmurHash3_x64_128 (Austin Appleby's reference algorithm,
-- ported from pyfory's vendored implementation)
-- ---------------------------------------------------------------------------

{- | Compute @MurmurHash3_x64_128@ over @bs@ with the given 32-bit
@seed@. Returns @(h1, h2)@ as the two 64-bit halves of the
128-bit hash, in the order pyfory's @mmh3.hash_buffer@ writes
them.
-}
murmur3X64_128 :: ByteString -> Word64 -> (Word64, Word64)
murmur3X64_128 !bs !seed =
  let !len = BS.length bs
      !nblocks = len `quot` 16
      (h1Body, h2Body) = body 0 nblocks seed seed
      (h1Tail, h2Tail) = tailMix bs nblocks h1Body h2Body
      -- Finalisation
      !h1f = h1Tail `xor` fromIntegral len
      !h2f = h2Tail `xor` fromIntegral len
      !h1f1 = h1f + h2f
      !h2f1 = h2f + h1f1
      !h1f2 = fmix64 h1f1
      !h2f2 = fmix64 h2f1
      !h1f3 = h1f2 + h2f2
      !h2f3 = h2f2 + h1f3
  in (h1f3, h2f3)
  where
    !c1 = 0x87c37b91114253d5 :: Word64
    !c2 = 0x4cf5ad432745937f :: Word64

    body :: Int -> Int -> Word64 -> Word64 -> (Word64, Word64)
    body i n h1 h2
      | i >= n = (h1, h2)
      | otherwise =
          let !k1 = readWord64LEUnsafe bs (i * 16)
              !k2 = readWord64LEUnsafe bs (i * 16 + 8)
              !k1m = rotateL (k1 * c1) 31 * c2
              !h1' = (rotateL (h1 `xor` k1m) 27 + h2) * 5 + 0x52dce729
              !k2m = rotateL (k2 * c2) 33 * c1
              !h2' = (rotateL (h2 `xor` k2m) 31 + h1') * 5 + 0x38495ab5
          in body (i + 1) n h1' h2'

    tailMix :: ByteString -> Int -> Word64 -> Word64 -> (Word64, Word64)
    tailMix b nb h1 h2 =
      let !tailLen = BS.length b - nb * 16
          !off = nb * 16
          !k1 = mkTailK1 b off tailLen
          !k2 = mkTailK2 b off tailLen
          !k2m = (rotateL (k2 * c2) 33) * c1
          !h2' = if tailLen > 8 then h2 `xor` k2m else h2
          !k1m = (rotateL (k1 * c1) 31) * c2
          !h1' = if tailLen > 0 then h1 `xor` k1m else h1
      in (h1', h2')


mkTailK1 :: ByteString -> Int -> Int -> Word64
mkTailK1 bs off tailLen =
  let n = min 8 tailLen
  in goK 0 0 n
  where
    goK :: Int -> Word64 -> Int -> Word64
    goK i acc n
      | i >= n = acc
      | otherwise =
          let !b = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word64
              !acc' = acc .|. (b `shiftL` (8 * i))
          in goK (i + 1) acc' n


mkTailK2 :: ByteString -> Int -> Int -> Word64
mkTailK2 bs off tailLen
  | tailLen <= 8 = 0
  | otherwise =
      let n = tailLen - 8
      in goK 0 0 n
  where
    goK :: Int -> Word64 -> Int -> Word64
    goK i acc n
      | i >= n = acc
      | otherwise =
          let !b = fromIntegral (BSU.unsafeIndex bs (off + 8 + i)) :: Word64
              !acc' = acc .|. (b `shiftL` (8 * i))
          in goK (i + 1) acc' n


-- ---------------------------------------------------------------------------
-- Public composite: meta-string hashcode (length-aware)
-- ---------------------------------------------------------------------------

{- | The hashcode pyfory writes alongside a fresh meta string
(after the @(length \<\< 1)@ header) when the encoded length
exceeds 16 bytes. The low 8 bits are overwritten with the
encoding tag.
-}
metaStringHashcode :: ByteString -> Word64 -> Word64
metaStringHashcode !bs !enc
  | BS.length bs <= 16 =
      let (!v1, !v2) = unpackSmall bs
      in hashSmallMetaString v1 v2 (BS.length bs) enc
  | otherwise =
      let (!h1, _h2) = murmur3X64_128 bs 47
      in (h1 .&. 0xFFFFFFFFFFFFFF00) .|. (enc .&. 0xFF)


-- ---------------------------------------------------------------------------
-- ByteString little-endian Word64 reader
-- ---------------------------------------------------------------------------

readWord64LEUnsafe :: ByteString -> Int -> Word64
readWord64LEUnsafe !bs !off =
  let go i acc
        | i >= 8 = acc
        | otherwise =
            let !b = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word64
                !acc' = acc .|. (b `shiftL` (8 * i))
            in go (i + 1) acc'
  in go 0 0
{-# INLINE readWord64LEUnsafe #-}
