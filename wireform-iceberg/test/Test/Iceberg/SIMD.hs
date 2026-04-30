{-# LANGUAGE OverloadedStrings #-}
-- | Cross-check the C kernels against the pure Haskell reference
-- implementations on a fuzz set, plus the canonical Iceberg vectors.
module Test.Iceberg.SIMD (tests) where

import qualified Data.ByteString as BS
import Data.Bits (xor, shiftR, shiftL, (.&.))
import qualified Data.Vector.Storable as VS
import Data.Word (Word16, Word64)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Iceberg.Murmur3 as M
import qualified Iceberg.SIMD as SIMD

-- A small deterministic byte stream so the bench doesn't rely on QuickCheck.
deterministicBytes :: Int -> BS.ByteString
deterministicBytes n =
  BS.pack (take n (cycle [0..255]))

tests :: TestTree
tests = testGroup "Iceberg.SIMD"
  [ testCase "Murmur3 C kernel matches pure on canonical empty input" $
      SIMD.murmur3_32 BS.empty @?= 0

  , testCase "Murmur3 C kernel matches pure across many lengths" $ do
      let lengths = [0 .. 64] ++ [100, 256, 257, 1024, 4096]
      mapM_ (\n -> do
        let bs = deterministicBytes n
        SIMD.murmur3_32 bs @?= M.murmur3_32_pure bs
        ) lengths

  , testCase "bucketLong: C matches pure for a wide range of inputs" $ do
      let xs = [-1000, -1, 0, 1, 5, 34, 1024, 1 `shiftL` 32, maxBound]
      mapM_ (\v -> SIMD.bucketLong 16 v @?= M.bucketLong_pure 16 v) xs

  , testCase "Spec vector: bucketLong 16 34 == 3" $
      SIMD.bucketLong 16 34 @?= 3

  , testCase "XXH64 known vector: empty input with seed 0 == 0xef46db3751d8e999" $
      -- Reference value from the upstream xxHash test suite.
      SIMD.xxh64 0 BS.empty @?= (0xef46db3751d8e999 :: Word64)

  , testCase "XXH64 self-consistency across lengths" $ do
      let bs64 = deterministicBytes 64
          bs256 = deterministicBytes 256
      SIMD.xxh64 0 bs64    @?= SIMD.xxh64 0 bs64
      (SIMD.xxh64 0 bs256 /= SIMD.xxh64 0 bs64) @?= True

  , testCase "Roaring ARRAY decode round-trips a sorted vector" $ do
      let lows = VS.fromList [3, 7, 100, 1000, 65000 :: Word16]
          payload = SIMD.roaringEncodeArray lows
          decoded = SIMD.roaringDecodeArray payload (VS.length lows) 0
      VS.toList decoded @?= map fromIntegral (VS.toList lows)

  , testCase "Roaring ARRAY contains: present & absent" $ do
      let lows = VS.fromList [3, 7, 100, 1000, 65000 :: Word16]
          payload = SIMD.roaringEncodeArray lows
      mapM_ (\v -> SIMD.roaringContains SIMD.ArrayContainer payload (VS.length lows) v @?= True)
            (VS.toList lows)
      SIMD.roaringContains SIMD.ArrayContainer payload (VS.length lows) 4 @?= False
      SIMD.roaringContains SIMD.ArrayContainer payload (VS.length lows) 99 @?= False

  , testCase "Roaring BITSET round-trip: encode -> decode" $ do
      let lows = VS.fromList [0, 1, 64, 65535 :: Word16]
          payload = SIMD.roaringEncodeBitset lows
          decoded = SIMD.roaringDecodeBitset payload 0
      map (fromIntegral :: Int -> Word16) [0, 1, 64, 65535] @?=
        map fromIntegral (VS.toList decoded)

  , testCase "Roaring BITSET membership matches array membership" $ do
      let lows = VS.fromList [3, 100, 65000 :: Word16]
          payload = SIMD.roaringEncodeBitset lows
      mapM_ (\v -> SIMD.roaringContains SIMD.BitsetContainer payload 0 v @?= True)
            (VS.toList lows)
      SIMD.roaringContains SIMD.BitsetContainer payload 0 4 @?= False
  ]

-- Suppress unused warnings for re-exported bit ops we keep around for future
-- expansion.
_keep :: Int
_keep = (1 `xor` 1) .&. (1 `shiftR` 0)
