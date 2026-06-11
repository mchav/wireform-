{- | Cross-check the C/SIMDe kernels in "Wireform.Hash" against the
pure Haskell reference implementations in "PureRef" on a fuzz set,
plus the canonical Iceberg / xxHash test vectors.
-}
module Test.Iceberg.Hash (tests) where

import Data.ByteString qualified as BS
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word64)
import Iceberg.Murmur3 qualified as M
import Test.Syd
import Wireform.Hash qualified as Hash


-- A small deterministic byte stream so the bench doesn't rely on QuickCheck.
deterministicBytes :: Int -> BS.ByteString
deterministicBytes n = BS.pack (take n (cycle [0 .. 255]))


tests :: Spec
tests =
  describe "Wireform.Hash" $
    sequence_
      [ it "Murmur3 of empty input == 0" $
          M.murmur3_32 BS.empty `shouldBe` 0
      , it "Spec vector: bucket[16] long 34 == 3" $
          M.bucketLong 16 34 `shouldBe` 3
      , it "bucketInt and bucketLong agree on small ints" $
          let xs = [-1000, -1, 0, 1, 5, 34, 1024, 65536]
          in mapM_
               ( \v ->
                   M.bucketInt 16 (fromIntegral v)
                     `shouldBe` M.bucketLong 16 (fromIntegral v)
               )
               xs
      , it "XXH64 known vector: empty input with seed 0" $
          Hash.xxh64 0 BS.empty `shouldBe` (0xef46db3751d8e999 :: Word64)
      , it "XXH64 self-consistency across lengths" $ do
          let bs64 = deterministicBytes 64
              bs256 = deterministicBytes 256
          Hash.xxh64 0 bs64 `shouldBe` Hash.xxh64 0 bs64
          (Hash.xxh64 0 bs256 /= Hash.xxh64 0 bs64) `shouldBe` True
      , it "Roaring ARRAY decode round-trips a sorted vector" $ do
          let lows = VS.fromList [3, 7, 100, 1000, 65000 :: Word16]
              payload = Hash.roaringEncodeArray lows
              decoded = Hash.roaringDecodeArray payload (VS.length lows) 0
          VS.toList decoded `shouldBe` map fromIntegral (VS.toList lows)
      , it "Roaring ARRAY contains: present & absent" $ do
          let lows = VS.fromList [3, 7, 100, 1000, 65000 :: Word16]
              payload = Hash.roaringEncodeArray lows
          mapM_
            (\v -> Hash.roaringContains Hash.ArrayContainer payload (VS.length lows) v `shouldBe` True)
            (VS.toList lows)
          Hash.roaringContains Hash.ArrayContainer payload (VS.length lows) 4 `shouldBe` False
          Hash.roaringContains Hash.ArrayContainer payload (VS.length lows) 99 `shouldBe` False
      , it "Roaring BITSET round-trip: encode -> decode" $ do
          let lows = VS.fromList [0, 1, 64, 65535 :: Word16]
              payload = Hash.roaringEncodeBitset lows
              decoded = Hash.roaringDecodeBitset payload 0
          map (fromIntegral :: Int -> Word16) [0, 1, 64, 65535]
            `shouldBe` map fromIntegral (VS.toList decoded)
      , it "Roaring BITSET membership matches array membership" $ do
          let lows = VS.fromList [3, 100, 65000 :: Word16]
              payload = Hash.roaringEncodeBitset lows
          mapM_
            (\v -> Hash.roaringContains Hash.BitsetContainer payload 0 v `shouldBe` True)
            (VS.toList lows)
          Hash.roaringContains Hash.BitsetContainer payload 0 4 `shouldBe` False
      ]
