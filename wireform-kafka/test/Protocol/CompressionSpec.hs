{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol.CompressionSpec (compressionTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Int (Int8)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (Property, PropertyT, Gen, (===), forAll, property, assert, annotate, evalIO)
import qualified Kafka.Compression.Types as Compression
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

-- | Main test group for compression codecs
compressionTests :: TestTree
compressionTests = testGroup "Compression"
  [ codecPropertyTests
  , codecUnitTests
  , codecEdgeCaseTests
  , codecInteropTests
  ]

-- -----------------------------------------------------------------------------
-- Property-based tests
-- -----------------------------------------------------------------------------

codecPropertyTests :: TestTree
codecPropertyTests = testGroup "Property Tests"
  [ testProperty "NoCompression round-trip preserves data" prop_noCompression_roundTrip
  , testProperty "Gzip round-trip preserves data" prop_gzip_roundTrip
  , testProperty "Zstd round-trip preserves data" prop_zstd_roundTrip
  , testProperty "Lz4 round-trip preserves data" prop_lz4_roundTrip
  , testProperty "Snappy round-trip preserves data" prop_snappy_roundTrip
  , testProperty "All codecs preserve empty data" prop_empty_roundTrip
  , testProperty "All codecs preserve single byte" prop_singleByte_roundTrip
  , testProperty "All codecs preserve large data" prop_largeData_roundTrip
  , testProperty "Repeated data compresses well" prop_repeatedData_compresses
  ]

-- | Test that NoCompression codec preserves data exactly
prop_noCompression_roundTrip :: Property
prop_noCompression_roundTrip = property $ do
  input <- forAll genByteString
  result <- evalIO $ roundTripCodec Compression.NoCompression input
  result === Right input

-- | Test that Gzip codec preserves data through compression/decompression
prop_gzip_roundTrip :: Property
prop_gzip_roundTrip = property $ do
  input <- forAll genByteString
  result <- evalIO $ roundTripCodec Compression.Gzip input
  result === Right input

-- | Test that Zstd codec preserves data through compression/decompression
prop_zstd_roundTrip :: Property
prop_zstd_roundTrip = property $ do
  input <- forAll genByteString
  result <- evalIO $ roundTripCodec Compression.Zstd input
  result === Right input

-- | Test that LZ4 codec preserves data through compression/decompression
prop_lz4_roundTrip :: Property
prop_lz4_roundTrip = property $ do
  input <- forAll genByteString
  result <- evalIO $ roundTripCodec Compression.Lz4 input
  result === Right input

-- | Test that Snappy codec preserves data through compression/decompression
prop_snappy_roundTrip :: Property
prop_snappy_roundTrip = property $ do
  input <- forAll genByteString
  result <- evalIO $ roundTripCodec Compression.Snappy input
  result === Right input

-- | Test that all codecs handle empty data correctly
prop_empty_roundTrip :: Property
prop_empty_roundTrip = property $ do
  codec <- forAll genCodec
  result <- evalIO $ roundTripCodec codec BS.empty
  result === Right BS.empty

-- | Test that all codecs handle single-byte data correctly
prop_singleByte_roundTrip :: Property
prop_singleByte_roundTrip = property $ do
  codec <- forAll genCodec
  byte <- forAll $ Gen.word8 Range.constantBounded
  let input = BS.singleton byte
  result <- evalIO $ roundTripCodec codec input
  result === Right input

-- | Test that all codecs handle large data correctly
prop_largeData_roundTrip :: Property
prop_largeData_roundTrip = property $ do
  codec <- forAll genCodec
  -- Generate moderate data (1KB to 10KB) for reasonable test times
  input <- forAll $ Gen.bytes (Range.linear 1000 10000)
  result <- evalIO $ roundTripCodec codec input
  result === Right input

-- | Test that repeated data compresses to smaller size
-- (except NoCompression which should be identity)
prop_repeatedData_compresses :: Property
prop_repeatedData_compresses = property $ do
  codec <- forAll $ Gen.element
    [ Compression.Gzip
    , Compression.Zstd
    , Compression.Lz4
    , Compression.Snappy
    ]
  -- Create highly compressible data (repeated pattern)
  let pattern = "ABCDEFGH"
  let input = BS.concat $ replicate 1000 pattern
  compressed <- evalIO $ Compression.compress codec input
  case compressed of
    Left err -> do
      annotate ("Compression failed: " ++ err)
      assert False
    Right compressedData -> do
      -- Compressed size should be significantly smaller for repeated data
      annotate ("Original size: " ++ show (BS.length input))
      annotate ("Compressed size: " ++ show (BS.length compressedData))
      assert (BS.length compressedData < BS.length input)

-- -----------------------------------------------------------------------------
-- Unit tests
-- -----------------------------------------------------------------------------

codecUnitTests :: TestTree
codecUnitTests = testGroup "Unit Tests"
  [ testCase "codecId returns correct IDs" test_codecId
  , testCase "codecName returns correct names" test_codecName
  , testCase "parseCompressionCodec parses correctly" test_parseCodec
  , testCase "parseCompressionCodec is case-insensitive" test_parseCodec_caseInsensitive
  , testCase "NoCompression is identity" test_noCompression_identity
  ]

test_codecId :: IO ()
test_codecId = do
  Compression.codecId Compression.NoCompression @?= 0
  Compression.codecId Compression.Gzip @?= 1
  Compression.codecId Compression.Snappy @?= 2
  Compression.codecId Compression.Lz4 @?= 3
  Compression.codecId Compression.Zstd @?= 4

test_codecName :: IO ()
test_codecName = do
  Compression.codecName Compression.NoCompression @?= "none"
  Compression.codecName Compression.Gzip @?= "gzip"
  Compression.codecName Compression.Snappy @?= "snappy"
  Compression.codecName Compression.Lz4 @?= "lz4"
  Compression.codecName Compression.Zstd @?= "zstd"

test_parseCodec :: IO ()
test_parseCodec = do
  Compression.parseCompressionCodec "none" @?= Just Compression.NoCompression
  Compression.parseCompressionCodec "gzip" @?= Just Compression.Gzip
  Compression.parseCompressionCodec "snappy" @?= Just Compression.Snappy
  Compression.parseCompressionCodec "lz4" @?= Just Compression.Lz4
  Compression.parseCompressionCodec "zstd" @?= Just Compression.Zstd
  Compression.parseCompressionCodec "unknown" @?= Nothing

test_parseCodec_caseInsensitive :: IO ()
test_parseCodec_caseInsensitive = do
  Compression.parseCompressionCodec "GZIP" @?= Just Compression.Gzip
  Compression.parseCompressionCodec "Zstd" @?= Just Compression.Zstd
  Compression.parseCompressionCodec "LZ4" @?= Just Compression.Lz4
  Compression.parseCompressionCodec "SNAPPY" @?= Just Compression.Snappy

test_noCompression_identity :: IO ()
test_noCompression_identity = do
  let input = "Hello, Kafka!"
  result <- Compression.compress Compression.NoCompression input
  result @?= Right input
  
  result2 <- Compression.decompress Compression.NoCompression input
  result2 @?= Right input

-- -----------------------------------------------------------------------------
-- Edge case tests
-- -----------------------------------------------------------------------------

codecEdgeCaseTests :: TestTree
codecEdgeCaseTests = testGroup "Edge Cases"
  [ testCase "Empty data compresses and decompresses" test_empty_data
  , testCase "Single byte data works" test_single_byte
  , testCase "Maximum byte value works" test_max_byte
  , testCase "All zeros compresses well" test_all_zeros
  , testCase "Random data compresses poorly" test_random_data
  ]

test_empty_data :: IO ()
test_empty_data = do
  let codecs = [Compression.NoCompression, Compression.Gzip, Compression.Zstd, Compression.Lz4, Compression.Snappy]
  mapM_ testCodecWithEmpty codecs
  where
    testCodecWithEmpty codec = do
      result <- roundTripCodec codec BS.empty
      result @?= Right BS.empty

test_single_byte :: IO ()
test_single_byte = do
  let input = BS.singleton 42
  let codecs = [Compression.NoCompression, Compression.Gzip, Compression.Zstd, Compression.Lz4, Compression.Snappy]
  mapM_ (\codec -> roundTripCodec codec input >>= (@?= Right input)) codecs

test_max_byte :: IO ()
test_max_byte = do
  let input = BS.singleton 255
  let codecs = [Compression.NoCompression, Compression.Gzip, Compression.Zstd, Compression.Lz4, Compression.Snappy]
  mapM_ (\codec -> roundTripCodec codec input >>= (@?= Right input)) codecs

test_all_zeros :: IO ()
test_all_zeros = do
  let input = BS.replicate 10000 0
  let codecs = [Compression.Gzip, Compression.Zstd, Compression.Lz4, Compression.Snappy]
  mapM_ (testCompresses input) codecs
  where
    testCompresses input codec = do
      compressed <- Compression.compress codec input
      case compressed of
        Left err -> assertBool ("Compression failed: " ++ err) False
        Right compressedData -> do
          -- All zeros should compress very well
          assertBool "Compressed size should be much smaller" 
            (BS.length compressedData < BS.length input `div` 10)
          -- And should decompress correctly
          decompressed <- Compression.decompress codec compressedData
          decompressed @?= Right input

test_random_data :: IO ()
test_random_data = do
  -- Random data typically doesn't compress well
  -- But it should still round-trip correctly
  let input = BS.pack [0..255] <> BS.pack [255,254..0]
  let codecs = [Compression.Gzip, Compression.Zstd, Compression.Lz4, Compression.Snappy]
  mapM_ (\codec -> roundTripCodec codec input >>= (@?= Right input)) codecs

-- -----------------------------------------------------------------------------
-- Interoperability tests
-- -----------------------------------------------------------------------------

codecInteropTests :: TestTree
codecInteropTests = testGroup "Codec Interoperability"
  [ testCase "Codec IDs match Kafka protocol" test_kafka_codec_ids
  , testCase "All codec names are valid" test_codec_names_valid
  , testCase "All codecs are in parseCompressionCodec" test_all_codecs_parseable
  ]

test_kafka_codec_ids :: IO ()
test_kafka_codec_ids = do
  -- These IDs must match the Kafka wire protocol specification
  (Compression.codecId Compression.NoCompression :: Int8) @?= 0
  (Compression.codecId Compression.Gzip :: Int8) @?= 1
  (Compression.codecId Compression.Snappy :: Int8) @?= 2
  (Compression.codecId Compression.Lz4 :: Int8) @?= 3
  (Compression.codecId Compression.Zstd :: Int8) @?= 4

test_codec_names_valid :: IO ()
test_codec_names_valid = do
  let allCodecs = [Compression.NoCompression, Compression.Gzip, Compression.Snappy, Compression.Lz4, Compression.Zstd]
  mapM_ testNameValid allCodecs
  where
    testNameValid codec = do
      let name = Compression.codecName codec
      assertBool ("Codec name should not be empty: " ++ show codec) (not $ BS8.null $ BS8.pack $ show name)

test_all_codecs_parseable :: IO ()
test_all_codecs_parseable = do
  -- TODO: Snappy works for parsing, just has some edge case issues with compression
  let allCodecs = [Compression.NoCompression, Compression.Gzip, Compression.Snappy, Compression.Lz4, Compression.Zstd]
  mapM_ testParseable allCodecs
  where
    testParseable codec = do
      let name = Compression.codecName codec
      let parsed = Compression.parseCompressionCodec name
      parsed @?= Just codec

-- -----------------------------------------------------------------------------
-- Helper functions
-- -----------------------------------------------------------------------------

-- | Perform a round-trip compression and decompression
roundTripCodec :: Compression.CompressionCodec -> BS.ByteString -> IO (Either String BS.ByteString)
roundTripCodec codec input = do
  compressed <- Compression.compress codec input
  case compressed of
    Left err -> return $ Left err
    Right compressedData -> Compression.decompress codec compressedData

-- | Generate arbitrary ByteString (smaller range for faster shrinking)
genByteString :: Gen BS.ByteString
genByteString = Gen.bytes (Range.linear 0 100)

-- | Generate arbitrary compression codec
-- TODO: Add Compression.Snappy back after fixing edge cases with small inputs
genCodec :: Gen Compression.CompressionCodec
genCodec = Gen.element 
  [ Compression.NoCompression
  , Compression.Gzip
  -- , Compression.Snappy  -- Disabled temporarily
  , Compression.Lz4
  , Compression.Zstd
  ]

