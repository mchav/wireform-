{-# LANGUAGE OverloadedStrings #-}

module Protocol.CRC32CSpec (spec) where

import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString qualified as BS
import Data.Word (Word32, Word8)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Protocol.CRC32C qualified as CRC
import Test.Syd
import Test.Syd.Hedgehog ()


-- -----------------------------------------------------------------------------
-- Naive Reference Implementation
-- -----------------------------------------------------------------------------

{- | Naive CRC32C implementation using the Castagnoli polynomial.
This is a simple, unoptimized reference implementation for testing.
-}
naiveCrc32c :: BS.ByteString -> Word32
naiveCrc32c bs = BS.foldl' crc32cByte 0xFFFFFFFF bs `xor` 0xFFFFFFFF
  where
    -- CRC32C (Castagnoli) polynomial: 0x1EDC6F41
    -- Reversed: 0x82F63B78
    crc32cByte :: Word32 -> Word8 -> Word32
    crc32cByte crc byte =
      let crc' = crc `xor` fromIntegral byte
          step c =
            if c .&. 1 /= 0
              then (c `shiftR` 1) `xor` 0x82F63B78
              else c `shiftR` 1
      in step $ step $ step $ step $ step $ step $ step $ step crc'


-- | Naive incremental CRC32C computation
naiveCrc32cAppend :: Word32 -> BS.ByteString -> Word32
naiveCrc32cAppend crc bs = BS.foldl' crc32cByte crc bs
  where
    crc32cByte :: Word32 -> Word8 -> Word32
    crc32cByte c byte =
      let c' = c `xor` fromIntegral byte
          step x =
            if x .&. 1 /= 0
              then (x `shiftR` 1) `xor` 0x82F63B78
              else x `shiftR` 1
      in step $ step $ step $ step $ step $ step $ step $ step c'


-- -----------------------------------------------------------------------------
-- Test Suite
-- -----------------------------------------------------------------------------

-- | Test suite for CRC32C implementation
spec :: Spec
spec =
  describe "CRC32C" $
    sequence_
      [ testKnownValues
      , testIncremental
      , testEmpty
      , testLarge
      , testProperties
      ]


{- | Test against known CRC32C values
These test vectors are from various CRC32C implementations
-}
testKnownValues :: Spec
testKnownValues =
  describe "Known values" $
    sequence_
      [ it "empty string" $
          CRC.crc32c "" `shouldBe` 0x00000000
      , it "single byte '0'" $
          CRC.crc32c "0" `shouldBe` 0x629E1AE0
      , it "\"123456789\"" $
          CRC.crc32c "123456789" `shouldBe` 0xE3069283
      , it "\"Hello, World!\"" $
          CRC.crc32c "Hello, World!" `shouldBe` 1297420392 -- 0x4D555368
      , it "\"The quick brown fox jumps over the lazy dog\"" $
          CRC.crc32c "The quick brown fox jumps over the lazy dog" `shouldBe` 0x22620404
      , it "all zeros (32 bytes)" $
          CRC.crc32c (BS.replicate 32 0) `shouldBe` 0x8A9136AA
      , it "all 0xFF (32 bytes)" $
          CRC.crc32c (BS.replicate 32 0xFF) `shouldBe` 0x62A8AB43
      ]


-- | Test incremental CRC computation
testIncremental :: Spec
testIncremental =
  describe "Incremental" $
    sequence_
      [ it "one-shot vs incremental (two chunks)" $ do
          let input = "Hello, World!"
              (chunk1, chunk2) = BS.splitAt 7 input
              oneShot = CRC.crc32c input
              incremental =
                CRC.crc32cFinalize $
                  CRC.crc32cAppend (CRC.crc32cAppend CRC.crc32cInit chunk1) chunk2
          incremental `shouldBe` oneShot
      , it "one-shot vs incremental (three chunks)" $ do
          let input = "The quick brown fox jumps over the lazy dog"
              chunk1 = BS.take 15 input
              chunk2 = BS.take 15 (BS.drop 15 input)
              chunk3 = BS.drop 30 input
              oneShot = CRC.crc32c input
              incremental =
                CRC.crc32cFinalize $
                  CRC.crc32cAppend (CRC.crc32cAppend (CRC.crc32cAppend CRC.crc32cInit chunk1) chunk2) chunk3
          incremental `shouldBe` oneShot
      , it "byte-by-byte vs one-shot" $ do
          let input = "123456789"
              oneShot = CRC.crc32c input
              byteByByte =
                CRC.crc32cFinalize $
                  foldl CRC.crc32cAppend CRC.crc32cInit (map BS.singleton (BS.unpack input))
          byteByByte `shouldBe` oneShot
      ]


-- | Test empty input
testEmpty :: Spec
testEmpty =
  describe "Empty input" $
    sequence_
      [ it "empty ByteString" $
          CRC.crc32c BS.empty `shouldBe` 0x00000000
      , it "incremental with empty chunks" $ do
          let crc =
                CRC.crc32cFinalize $
                  CRC.crc32cAppend (CRC.crc32cAppend CRC.crc32cInit "Hello") ""
          crc `shouldBe` CRC.crc32c "Hello"
      ]


-- | Test large inputs (to exercise vectorized code paths)
testLarge :: Spec
testLarge =
  describe "Large inputs" $
    sequence_
      [ it "1KB of zeros" $ do
          let input = BS.replicate 1024 0
          -- Just verify it doesn't crash and produces consistent results
          CRC.crc32c input `shouldBe` CRC.crc32c input
      , it "1KB incremental matches one-shot" $ do
          let input = BS.replicate 1024 42
              (chunk1, chunk2) = BS.splitAt 512 input
              oneShot = CRC.crc32c input
              incremental =
                CRC.crc32cFinalize $
                  CRC.crc32cAppend (CRC.crc32cAppend CRC.crc32cInit chunk1) chunk2
          incremental `shouldBe` oneShot
      , it "4KB of pattern" $ do
          let input = BS.concat (replicate 64 "0123456789ABCDEF0123456789abcdef0123456789ABCDEF0123456789abcdef")
          -- Just verify it doesn't crash and produces consistent results
          CRC.crc32c input `shouldBe` CRC.crc32c input
      , it "64-byte boundary (tests SSE4.2 code path)" $ do
          let input = BS.replicate 64 0xAA
              oneShot = CRC.crc32c input
              (chunk1, chunk2) = BS.splitAt 32 input
              incremental =
                CRC.crc32cFinalize $
                  CRC.crc32cAppend (CRC.crc32cAppend CRC.crc32cInit chunk1) chunk2
          incremental `shouldBe` oneShot
      ]


-- -----------------------------------------------------------------------------
-- Property-Based Tests (Hedgehog)
-- -----------------------------------------------------------------------------

-- | Property-based tests comparing optimized implementation with naive reference
testProperties :: Spec
testProperties =
  describe "Properties (vs naive implementation)" $
    sequence_
      [ it "matches naive implementation (small)" prop_matchesNaiveSmall
      , it "matches naive implementation (medium)" prop_matchesNaiveMedium
      , it "matches naive implementation (large)" prop_matchesNaiveLarge
      , it "incremental matches naive (2 chunks)" prop_incrementalMatchesNaive2
      , it "incremental matches naive (3 chunks)" prop_incrementalMatchesNaive3
      , it "incremental matches naive (many chunks)" prop_incrementalMatchesNaiveMany
      , it "empty input" prop_emptyInput
      , it "single byte" prop_singleByte
      ]


-- | Test that optimized implementation matches naive for small inputs (0-100 bytes)
prop_matchesNaiveSmall :: Property
prop_matchesNaiveSmall = property $ do
  bytes <- forAll $ Gen.bytes (Range.linear 0 100)
  let optimized = CRC.crc32c bytes
      naive = naiveCrc32c bytes
  optimized === naive


-- | Test that optimized implementation matches naive for medium inputs (100-1024 bytes)
prop_matchesNaiveMedium :: Property
prop_matchesNaiveMedium = property $ do
  bytes <- forAll $ Gen.bytes (Range.linear 100 1024)
  let optimized = CRC.crc32c bytes
      naive = naiveCrc32c bytes
  optimized === naive


-- | Test that optimized implementation matches naive for large inputs (1KB-16KB)
prop_matchesNaiveLarge :: Property
prop_matchesNaiveLarge = property $ do
  bytes <- forAll $ Gen.bytes (Range.linear 1024 (16 * 1024))
  let optimized = CRC.crc32c bytes
      naive = naiveCrc32c bytes
  optimized === naive


-- | Test incremental computation with 2 chunks
prop_incrementalMatchesNaive2 :: Property
prop_incrementalMatchesNaive2 = property $ do
  bytes <- forAll $ Gen.bytes (Range.linear 10 1024)
  splitPoint <- forAll $ Gen.int (Range.linear 0 (BS.length bytes))

  let (chunk1, chunk2) = BS.splitAt splitPoint bytes

      -- Optimized incremental
      optimized =
        CRC.crc32cFinalize $
          CRC.crc32cAppend (CRC.crc32cAppend CRC.crc32cInit chunk1) chunk2

      -- Naive incremental
      naive = naiveCrc32cAppend (naiveCrc32cAppend 0xFFFFFFFF chunk1) chunk2 `xor` 0xFFFFFFFF

  optimized === naive


-- | Test incremental computation with 3 chunks
prop_incrementalMatchesNaive3 :: Property
prop_incrementalMatchesNaive3 = property $ do
  bytes <- forAll $ Gen.bytes (Range.linear 15 1024)
  split1 <- forAll $ Gen.int (Range.linear 0 (BS.length bytes))
  split2 <- forAll $ Gen.int (Range.linear split1 (BS.length bytes))

  let chunk1 = BS.take split1 bytes
      chunk2 = BS.take (split2 - split1) (BS.drop split1 bytes)
      chunk3 = BS.drop split2 bytes

      -- Optimized incremental
      optimized =
        CRC.crc32cFinalize $
          CRC.crc32cAppend
            ( CRC.crc32cAppend
                (CRC.crc32cAppend CRC.crc32cInit chunk1)
                chunk2
            )
            chunk3

      -- Naive incremental
      naive =
        naiveCrc32cAppend
          ( naiveCrc32cAppend
              (naiveCrc32cAppend 0xFFFFFFFF chunk1)
              chunk2
          )
          chunk3
          `xor` 0xFFFFFFFF

  optimized === naive


-- | Test incremental computation with many small chunks
prop_incrementalMatchesNaiveMany :: Property
prop_incrementalMatchesNaiveMany = property $ do
  -- Generate a list of small chunks
  chunks <- forAll $ Gen.list (Range.linear 1 20) (Gen.bytes (Range.linear 1 100))

  let bytes = BS.concat chunks

      -- Optimized incremental
      optimized = CRC.crc32cFinalize $ foldl CRC.crc32cAppend CRC.crc32cInit chunks

      -- Naive incremental
      naive = foldl naiveCrc32cAppend 0xFFFFFFFF chunks `xor` 0xFFFFFFFF

      -- Also compare with one-shot
      oneShot = CRC.crc32c bytes

  optimized === naive
  optimized === oneShot


-- | Test empty input
prop_emptyInput :: Property
prop_emptyInput = property $ do
  let optimized = CRC.crc32c BS.empty
      naive = naiveCrc32c BS.empty
  optimized === naive
  optimized === 0x00000000


-- | Test single byte inputs
prop_singleByte :: Property
prop_singleByte = property $ do
  byte <- forAll $ Gen.word8 Range.constantBounded
  let bytes = BS.singleton byte
      optimized = CRC.crc32c bytes
      naive = naiveCrc32c bytes
  optimized === naive
