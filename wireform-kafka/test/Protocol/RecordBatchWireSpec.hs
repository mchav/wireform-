{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cross-codec equivalence tests for the new direct-poke
-- 'Kafka.Protocol.RecordBatchWire.encodeRecordBatchWire' against
-- the legacy 'Kafka.Protocol.RecordBatch.encodeRecordBatch'.
--
-- Both must produce byte-identical output for any record batch.
-- Plus the round-trip @decodeRecordBatch . encodeRecordBatchWire
-- == Right@ has to hold so the new encoder is consumable by the
-- existing decoder (and, by extension, by every Kafka broker).
module Protocol.RecordBatchWireSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB
import qualified Kafka.Protocol.RecordBatchWire as RBW

-- 'BS' kept as a placeholder import for future compressed-bytes
-- assertions; remove the qualifier alias if unused.
_keepBS :: BS.ByteString
_keepBS = BS.empty

tests :: TestTree
tests = testGroup "RecordBatchWire vs RecordBatch (legacy)"
  [ testCase "empty batch: bytes match"
      empty_match
  , testCase "1 record: bytes match"
      single_match
  , testProperty "any batch: bytes match (cross-codec equivalence)"
      prop_cross_codec_eq
  , testProperty "any batch: round-trip via legacy decoder"
      prop_round_trip_via_legacy_decoder
  , testProperty "any batch: round-trip via wire decoder"
      prop_round_trip_via_wire_decoder
  , testProperty "wire decoder accepts legacy-encoded bytes"
      prop_wire_decoder_accepts_legacy
  , testGroup "compressed encoder"
      [ testProperty "gzip: wire bytes == legacy bytes"
          (prop_compressed_match Compression.Gzip)
      , testProperty "lz4: wire bytes == legacy bytes"
          (prop_compressed_match Compression.Lz4)
      , testProperty "zstd: wire bytes == legacy bytes"
          (prop_compressed_match Compression.Zstd)
      , testProperty "gzip: round-trip via decompressing decoder"
          (prop_compressed_round_trip Compression.Gzip)
      ]
  ]

empty_match :: IO ()
empty_match = do
  let !b = mkBatch []
  RBW.encodeRecordBatchWire b @?= RB.encodeRecordBatch b

single_match :: IO ()
single_match = do
  let !b = mkBatch [mkRecord 0 (Just "k") "v"]
  RBW.encodeRecordBatchWire b @?= RB.encodeRecordBatch b

prop_cross_codec_eq :: Property
prop_cross_codec_eq = property $ do
  records <- forAll (Gen.list (Range.linear 0 16) genRecord)
  let !b = mkBatch records
  RBW.encodeRecordBatchWire b === RB.encodeRecordBatch b

prop_round_trip_via_legacy_decoder :: Property
prop_round_trip_via_legacy_decoder = property $ do
  records <- forAll (Gen.list (Range.linear 0 16) genRecord)
  let !b   = mkBatch records
      !bs  = RBW.encodeRecordBatchWire b
  case RB.decodeRecordBatch bs of
    Left err -> annotate err >> failure
    Right b' -> b' === b

prop_round_trip_via_wire_decoder :: Property
prop_round_trip_via_wire_decoder = property $ do
  records <- forAll (Gen.list (Range.linear 0 16) genRecord)
  let !b  = mkBatch records
      !bs = RBW.encodeRecordBatchWire b
  case RBW.decodeRecordBatchWire bs of
    Left err -> annotate err >> failure
    Right b' -> b' === b

prop_wire_decoder_accepts_legacy :: Property
prop_wire_decoder_accepts_legacy = property $ do
  records <- forAll (Gen.list (Range.linear 0 16) genRecord)
  let !b  = mkBatch records
      !bs = RB.encodeRecordBatch b
  case RBW.decodeRecordBatchWire bs of
    Left err -> annotate err >> failure
    Right b' -> b' === b

-- | The Wire compressed encoder must be byte-identical with the
-- legacy 'encodeRecordBatchWithCompression' for every codec we
-- ship. Using the same inputs through both encoders should yield
-- the same wire bytes (the codec-side compression is
-- deterministic for our chosen levels).
prop_compressed_match :: Compression.CompressionCodec -> Property
prop_compressed_match codec = property $ do
  records <- forAll (Gen.list (Range.linear 1 16) genRecord)
  let !b = mkCompressedBatch codec records
  legacy <- evalIO (RB.encodeRecordBatchWithCompression b)
  wire   <- evalIO (RBW.encodeRecordBatchWireCompressed   b)
  case (legacy, wire) of
    (Right lbs, Right wbs) -> lbs === wbs
    _ -> do
      annotate ("legacy=" <> show legacy <> " wire=" <> show wire)
      failure

-- | The Wire compressed encoder's output must round-trip
-- through the existing decompressing decoder back to the
-- original batch.
prop_compressed_round_trip :: Compression.CompressionCodec -> Property
prop_compressed_round_trip codec = property $ do
  records <- forAll (Gen.list (Range.linear 1 16) genRecord)
  let !b = mkCompressedBatch codec records
  wire <- evalIO (RBW.encodeRecordBatchWireCompressed b)
  case wire of
    Left err -> annotate err >> failure
    Right wbs -> do
      back <- evalIO (RB.decodeRecordBatchWithDecompression wbs)
      case back of
        Left err -> annotate err >> failure
        Right b' -> b' === b

mkCompressedBatch :: Compression.CompressionCodec -> [RB.Record] -> RB.RecordBatch
mkCompressedBatch codec records =
  let attrs = RB.defaultAttributes { RB.attrCompressionType = codec }
  in RB.mkRecordBatch
       0
       RB.noPartitionLeaderEpoch
       attrs
       0
       RB.noProducerId
       RB.noProducerEpoch
       RB.noSequence
       (V.fromList records)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

mkBatch :: [RB.Record] -> RB.RecordBatch
mkBatch records = RB.mkRecordBatch
  0                      -- baseOffset
  RB.noPartitionLeaderEpoch
  RB.defaultAttributes
  0                      -- baseTimestamp
  RB.noProducerId
  RB.noProducerEpoch
  RB.noSequence
  (V.fromList records)

mkRecord :: Int -> Maybe BS.ByteString -> BS.ByteString -> RB.Record
mkRecord ix k v = RB.Record
  { RB.recordTimestampDelta = 0
  , RB.recordOffsetDelta    = fromIntegral ix
  , RB.recordKey            = k
  , RB.recordValue          = v
  , RB.recordHeaders        = []
  }

genRecord :: Gen RB.Record
genRecord = do
  ofs <- Gen.int32 (Range.linear (-1000) 1000)
  ts  <- Gen.int64 (Range.linear (-1000) 1000)
  k   <- Gen.maybe (Gen.bytes (Range.linear 0 32))
  v   <- Gen.bytes (Range.linear 0 256)
  hs  <- Gen.list (Range.linear 0 4) genHeader
  pure RB.Record
    { RB.recordTimestampDelta = ts
    , RB.recordOffsetDelta    = ofs
    , RB.recordKey            = k
    , RB.recordValue          = v
    , RB.recordHeaders        = hs
    }

genHeader :: Gen RB.RecordHeader
genHeader = do
  k <- Gen.bytes (Range.linear 0 16)
  v <- Gen.maybe (Gen.bytes (Range.linear 0 64))
  pure RB.RecordHeader { RB.headerKey = k, RB.headerValue = v }
