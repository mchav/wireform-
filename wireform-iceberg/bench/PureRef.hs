{-# LANGUAGE BangPatterns #-}

{- | Pure Haskell reference implementations of "Iceberg.Murmur3" /
"Iceberg.DeletionVector" used /only/ by the criterion benchmark suite
to measure the speedup of the C/SIMDe kernels.

This module is a benchmark-private artifact and intentionally lives in
the @bench/@ directory rather than @src/@ so it doesn't appear in the
public API.
-}
module PureRef (
  -- * Murmur3 reference
  murmur3_32_pure,
  bucketLong_pure,

  -- * Deletion vector reference
  decodeDV_pure,
) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Word (Word32, Word64)
import Iceberg.DeletionVector qualified as DV
import Wireform.Builder qualified as BB


-- ============================================================
-- Murmur3 reference
-- ============================================================

murmur3_32_pure :: ByteString -> Int32
murmur3_32_pure bs = fromIntegral (go 0 (BS.length bs) 0 :: Word32)
  where
    !c1 = 0xcc9e2d51 :: Word32
    !c2 = 0x1b873593 :: Word32
    !len = BS.length bs

    go :: Word32 -> Int -> Int -> Word32
    go !h !remaining !off
      | remaining >= 4 =
          let k = readW32LE bs off
              k1 = (k * c1) `rotL32` 15 * c2
              h1 = ((h `xor` k1) `rotL32` 13) * 5 + 0xe6546b64
          in go h1 (remaining - 4) (off + 4)
      | remaining > 0 =
          let k = case remaining of
                3 ->
                  let !b0 = fromIntegral (BS.index bs off) :: Word32
                      !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
                      !b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
                  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
                2 ->
                  let !b0 = fromIntegral (BS.index bs off) :: Word32
                      !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
                  in b0 .|. (b1 `shiftL` 8)
                1 -> fromIntegral (BS.index bs off)
                _ -> 0
              k1 = ((k * c1) `rotL32` 15) * c2
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


rotL32 :: Word32 -> Int -> Word32
rotL32 x i = (x `shiftL` i) .|. (x `shiftR` (32 - i))


readW32LE :: ByteString -> Int -> Word32
readW32LE bs off =
  let !b0 = fromIntegral (BS.index bs off) :: Word32
      !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
      !b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
      !b3 = fromIntegral (BS.index bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)


bucketLong_pure :: Int -> Int64 -> Int
bucketLong_pure n v =
  let bs = BL.toStrict (BB.toLazyByteString (BB.int64LE v))
      h = murmur3_32_pure bs
  in fromIntegral (fromIntegral h .&. (0x7FFFFFFF :: Word32)) `mod` n


-- ============================================================
-- Deletion vector reference
-- ============================================================
--
-- A pure-Haskell port of the Roaring 32-bit decode in
-- 'Iceberg.DeletionVector.decodeDV' that doesn't go through the C
-- kernel. Used by the criterion benchmark to measure the
-- C-vs-Haskell speedup; not exported from the library.

decodeDV_pure :: ByteString -> Either String DV.DeletionVector
decodeDV_pure bs0 = do
  (cnt, rest) <- takeW64 bs0
  go (fromIntegral cnt) rest IntMap.empty
  where
    go 0 _ acc = Right (DV.DeletionVector acc)
    go !n bs acc = do
      (hi, bs') <- takeW32 bs
      (set, bs'') <- decodeRoaring32 bs'
      go (n - 1) bs'' (IntMap.insert (fromIntegral hi) set acc)


decodeRoaring32 :: ByteString -> Either String (IntSet, ByteString)
decodeRoaring32 bs = do
  (cookie, r1) <- takeW32 bs
  if cookie .&. 0xFFFF /= 0x3B30
    then Left "Roaring32: bad cookie"
    else pure ()
  (numContainers, r2) <- takeW32 r1
  let n = fromIntegral numContainers
      (keysAndCards, r3) = BS.splitAt (4 * n) r2
      (_offsets, r4) = BS.splitAt (4 * n) r3
      kac = unflatten4 keysAndCards
  decodeContainers kac r4 IntSet.empty
  where
    unflatten4 :: ByteString -> [(Int, Int)]
    unflatten4 b
      | BS.null b = []
      | BS.length b < 4 = []
      | otherwise =
          let key = readW16LE b 0
              card = readW16LE b 2 + 1
          in (fromIntegral key, fromIntegral card) : unflatten4 (BS.drop 4 b)


decodeContainers :: [(Int, Int)] -> ByteString -> IntSet -> Either String (IntSet, ByteString)
decodeContainers [] bs acc = Right (acc, bs)
decodeContainers ((key, card) : rest) bs acc =
  let (data', bs') = BS.splitAt (2 * card) bs
      lows = readW16Vec data'
      adjusted = IntSet.fromList (map (\l -> key * 0x10000 + fromIntegral l) lows)
  in decodeContainers rest bs' (IntSet.union acc adjusted)


readW16Vec :: ByteString -> [Int]
readW16Vec b
  | BS.null b = []
  | BS.length b < 2 = []
  | otherwise = readW16LE b 0 : readW16Vec (BS.drop 2 b)


readW16LE :: ByteString -> Int -> Int
readW16LE bs off =
  let b0 = fromIntegral (BS.index bs off) :: Int
      b1 = fromIntegral (BS.index bs (off + 1)) :: Int
  in b0 .|. (b1 `shiftL` 8)


takeW32 :: ByteString -> Either String (Word32, ByteString)
takeW32 bs
  | BS.length bs < 4 = Left "expected 4 bytes"
  | otherwise =
      let b0 = fromIntegral (BS.index bs 0) :: Word32
          b1 = fromIntegral (BS.index bs 1) :: Word32
          b2 = fromIntegral (BS.index bs 2) :: Word32
          b3 = fromIntegral (BS.index bs 3) :: Word32
          w = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
      in Right (w, BS.drop 4 bs)


takeW64 :: ByteString -> Either String (Word64, ByteString)
takeW64 bs
  | BS.length bs < 8 = Left "expected 8 bytes"
  | otherwise =
      let bn i = fromIntegral (BS.index bs i) :: Word64
          w =
            bn 0
              .|. (bn 1 `shiftL` 8)
              .|. (bn 2 `shiftL` 16)
              .|. (bn 3 `shiftL` 24)
              .|. (bn 4 `shiftL` 32)
              .|. (bn 5 `shiftL` 40)
              .|. (bn 6 `shiftL` 48)
              .|. (bn 7 `shiftL` 56)
      in Right (w, BS.drop 8 bs)
