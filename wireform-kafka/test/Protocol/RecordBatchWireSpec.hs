{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Round-trip tests for the direct-poke
-- 'Kafka.Protocol.RecordBatchWire' codec — the production path for
-- record batches in both produce and consume after the no-Serial
-- migration.
--
-- Before the migration these tests doubled as cross-codec parity
-- (Wire output == legacy Serial output). Now that Serial is gone
-- from the production path, the suite reduces to what actually
-- matters at the runtime layer: every encode round-trips through
-- the matching Wire decoder, with and without compression.
module Protocol.RecordBatchWireSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase)

import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB
import qualified Kafka.Protocol.RecordBatchWire as RBW

tests :: TestTree
tests = testGroup "RecordBatchWire round-trips"
  [ testCase "empty batch round-trips"
      empty_round_trip
  , testCase "single-record batch round-trips"
      single_round_trip
  , testProperty "any uncompressed batch round-trips"
      prop_round_trip_uncompressed
  , testGroup "compressed encoder"
      [ testProperty "gzip: encode . decode == id"
          (prop_round_trip_compressed Compression.Gzip)
      , testProperty "lz4: encode . decode == id"
          (prop_round_trip_compressed Compression.Lz4)
      , testProperty "zstd: encode . decode == id"
          (prop_round_trip_compressed Compression.Zstd)
      ]
  ]

empty_round_trip :: IO ()
empty_round_trip = do
  let !b   = mkBatch []
      !bs  = RBW.encodeRecordBatchWire b
  case RBW.decodeRecordBatchWire bs of
    Left err -> error err
    Right b' -> if b == b' then pure () else error "mismatch"

single_round_trip :: IO ()
single_round_trip = do
  let !b   = mkBatch [mkRecord 0 (Just "k") "v"]
      !bs  = RBW.encodeRecordBatchWire b
  case RBW.decodeRecordBatchWire bs of
    Left err -> error err
    Right b' -> if b == b' then pure () else error "mismatch"

prop_round_trip_uncompressed :: Property
prop_round_trip_uncompressed = property $ do
  records <- forAll (Gen.list (Range.linear 0 16) genRecord)
  let !b  = mkBatch records
      !bs = RBW.encodeRecordBatchWire b
  case RBW.decodeRecordBatchWire bs of
    Left err -> annotate err >> failure
    Right b' -> b' === b

-- | Compressed encode + decompress + decode round-trips through the
-- Wire codec end-to-end.
prop_round_trip_compressed :: Compression.CompressionCodec -> Property
prop_round_trip_compressed codec = property $ do
  records <- forAll (Gen.list (Range.linear 1 16) genRecord)
  let !b  = mkCompressedBatch codec records
  wire <- evalIO (RBW.encodeRecordBatchWireCompressed b)
  case wire of
    Left err  -> annotate err >> failure
    Right wbs -> do
      back <- evalIO (RBW.decodeRecordBatchWireWithDecompression wbs)
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
