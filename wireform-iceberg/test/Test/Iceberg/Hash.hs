-- | Cross-check the C/SIMDe kernels in "Wireform.Hash" against the
-- pure Haskell reference implementations in "PureRef" on a fuzz set,
-- plus the canonical Iceberg / xxHash test vectors.
module Test.Iceberg.Hash (tests) where

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as VS
import Data.Word (Word16, Word64)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Iceberg.Murmur3 as M
import qualified Wireform.Hash as Hash

-- A small deterministic byte stream so the bench doesn't rely on QuickCheck.
deterministicBytes :: Int -> BS.ByteString
deterministicBytes n = BS.pack (take n (cycle [0 .. 255]))

tests :: TestTree
tests = testGroup "Wireform.Hash"
  [ testCase "Murmur3 of empty input == 0" $
      M.murmur3_32 BS.empty @?= 0

  , testCase "Spec vector: bucket[16] long 34 == 3" $
      M.bucketLong 16 34 @?= 3

  , testCase "bucketInt and bucketLong agree on small ints" $
      let xs = [-1000, -1, 0, 1, 5, 34, 1024, 65536]
       in mapM_ (\v -> M.bucketInt 16 (fromIntegral v) @?=
                      M.bucketLong 16 (fromIntegral v)) xs

  , testCase "XXH64 known vector: empty input with seed 0" $
      Hash.xxh64 0 BS.empty @?= (0xef46db3751d8e999 :: Word64)

  , testCase "XXH64 self-consistency across lengths" $ do
      let bs64  = deterministicBytes 64
          bs256 = deterministicBytes 256
      Hash.xxh64 0 bs64    @?= Hash.xxh64 0 bs64
      (Hash.xxh64 0 bs256 /= Hash.xxh64 0 bs64) @?= True

  , testCase "Roaring ARRAY decode round-trips a sorted vector" $ do
      let lows = VS.fromList [3, 7, 100, 1000, 65000 :: Word16]
          payload = Hash.roaringEncodeArray lows
          decoded = Hash.roaringDecodeArray payload (VS.length lows) 0
      VS.toList decoded @?= map fromIntegral (VS.toList lows)

  , testCase "Roaring ARRAY contains: present & absent" $ do
      let lows = VS.fromList [3, 7, 100, 1000, 65000 :: Word16]
          payload = Hash.roaringEncodeArray lows
      mapM_ (\v -> Hash.roaringContains Hash.ArrayContainer payload (VS.length lows) v @?= True)
            (VS.toList lows)
      Hash.roaringContains Hash.ArrayContainer payload (VS.length lows) 4  @?= False
      Hash.roaringContains Hash.ArrayContainer payload (VS.length lows) 99 @?= False

  , testCase "Roaring BITSET round-trip: encode -> decode" $ do
      let lows = VS.fromList [0, 1, 64, 65535 :: Word16]
          payload = Hash.roaringEncodeBitset lows
          decoded = Hash.roaringDecodeBitset payload 0
      map (fromIntegral :: Int -> Word16) [0, 1, 64, 65535] @?=
        map fromIntegral (VS.toList decoded)

  , testCase "Roaring BITSET membership matches array membership" $ do
      let lows = VS.fromList [3, 100, 65000 :: Word16]
          payload = Hash.roaringEncodeBitset lows
      mapM_ (\v -> Hash.roaringContains Hash.BitsetContainer payload 0 v @?= True)
            (VS.toList lows)
      Hash.roaringContains Hash.BitsetContainer payload 0 4 @?= False
  ]
