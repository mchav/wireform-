{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Benchmarks.HotPath
Description : Producer + consumer hot paths against the in-memory mock cluster

Measures what every Kafka client spends its CPU on in steady state:

  * Producer-side
    - 'BA.appendRecordStamped' (STM accumulator).
    - 'Sender.buildRecordBatch' (per-batch wire shape).
    - 'RB.encodeRecordBatch' (the actual byte writer).
    - 'MockProducer.sendMockH' (full mock-cluster round-trip,
      no network).
  * Consumer-side
    - 'RB.decodeRecordBatch' (the per-batch wire reader).
    - 'MockConsumer.pollMC' (full mock-cluster round-trip).

Setup that creates STM-/IORef-backed handles is shared across runs
via top-level @NOINLINE@ + 'unsafePerformIO' (the standard
benchmark-fixture idiom). Each measurement therefore times only
the operation under test, not the cluster construction.

No broker / Docker required; everything runs in-process.
-}
module Benchmarks.HotPath (benchmarks) where

import Criterion (Benchmark, bench, bgroup, nf, whnfIO)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import GHC.IO (unsafePerformIO)

import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Client.Internal.ProducerSender as Sender
import qualified Kafka.Client.Mock.Cluster as MC
import qualified Kafka.Client.Mock.Consumer as MC
import qualified Kafka.Client.Mock.Fault as MC
import qualified Kafka.Client.Mock.Producer as MP
import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB
import qualified Kafka.Protocol.RecordBatchWire as RBW

----------------------------------------------------------------------
-- Producer hot paths
----------------------------------------------------------------------

producerBench :: Benchmark
producerBench = bgroup "Producer"
  [ bgroup "MockProducer.sendMockH (full mock round-trip, no network)"
      [ bench "1 record / 100B value" $ whnfIO $
          MP.sendMockH sharedMockProducer "topic" 0 (Just "key")
            payload100 0 []
      , bench "1 record / 1 KiB value" $ whnfIO $
          MP.sendMockH sharedMockProducer "topic" 0 (Just "key")
            payload1k 0 []
      , bench "1000 records / 100B value (sequential)" $ whnfIO $
          sendNTimes sharedMockProducer 1000 payload100
      , bench "10000 records / 100B value (sequential)" $ whnfIO $
          sendNTimes sharedMockProducer 10000 payload100
      ]
  , bgroup "BatchAccumulator.appendRecordStamped (STM hot path)"
      [ bench "single append (no-stamp)" $ whnfIO $
          BA.appendRecordStamped sharedAccumulator
            (BA.TopicPartition "topic" 0)
            sampleRecord (\_ -> pure ()) BA.noStamp
      , bench "100 appends" $ whnfIO $
          appendNRecords sharedAccumulator 100
      , bench "1000 appends" $ whnfIO $
          appendNRecords sharedAccumulator 1000
      ]
  , bgroup "RecordBatch encode (the wire serializer)"
      [ bench "buildRecordBatch (1 record)" $
          nf (BS.length . RB.encodeRecordBatch
                . Sender.buildRecordBatch) (sampleBatch 1)
      , bench "buildRecordBatch (10 records)" $
          nf (BS.length . RB.encodeRecordBatch
                . Sender.buildRecordBatch) (sampleBatch 10)
      , bench "buildRecordBatch (100 records)" $
          nf (BS.length . RB.encodeRecordBatch
                . Sender.buildRecordBatch) (sampleBatch 100)
      , bench "encodeRecordBatch only (1 record, prebuilt batch)" $
          nf (BS.length . RB.encodeRecordBatch) builtBatch1
      , bench "encodeRecordBatch only (10 records, prebuilt)" $
          nf (BS.length . RB.encodeRecordBatch) builtBatch10
      , bench "encodeRecordBatch only (100 records, prebuilt)" $
          nf (BS.length . RB.encodeRecordBatch) builtBatch100
      ]
  , bgroup "RecordBatch encode: Wire vs legacy (head-to-head)"
      [ bench "legacy   (1   record)" $
          nf (BS.length . RB.encodeRecordBatch) builtBatch1
      , bench "wire     (1   record)" $
          nf (BS.length . RBW.encodeRecordBatchWire) builtBatch1
      , bench "legacy   (10  records)" $
          nf (BS.length . RB.encodeRecordBatch) builtBatch10
      , bench "wire     (10  records)" $
          nf (BS.length . RBW.encodeRecordBatchWire) builtBatch10
      , bench "legacy   (100 records)" $
          nf (BS.length . RB.encodeRecordBatch) builtBatch100
      , bench "wire     (100 records)" $
          nf (BS.length . RBW.encodeRecordBatchWire) builtBatch100
      ]
  ]

----------------------------------------------------------------------
-- Consumer hot paths
----------------------------------------------------------------------

consumerBench :: Benchmark
consumerBench = bgroup "Consumer"
  [ bgroup "MockConsumer.pollMC (full mock round-trip)"
      [ bench "1 record waiting" $ whnfIO $
          MC.pollMC sharedConsumer1
      , bench "100 records waiting" $ whnfIO $
          MC.pollMC sharedConsumer100
      ]
  , bgroup "RecordBatch decode (the wire reader)"
      [ bench "decodeRecordBatch (1 record)" $
          nf decodeOrDie encoded1
      , bench "decodeRecordBatch (10 records)" $
          nf decodeOrDie encoded10
      , bench "decodeRecordBatch (100 records)" $
          nf decodeOrDie encoded100
      ]
  , bgroup "RecordBatch decode: Wire vs legacy (head-to-head)"
      [ bench "legacy (1   record)"  $ nf decodeLegacy encoded1
      , bench "wire   (1   record)"  $ nf decodeWire   encoded1
      , bench "legacy (10  records)" $ nf decodeLegacy encoded10
      , bench "wire   (10  records)" $ nf decodeWire   encoded10
      , bench "legacy (100 records)" $ nf decodeLegacy encoded100
      , bench "wire   (100 records)" $ nf decodeWire   encoded100
      ]
  ]

----------------------------------------------------------------------
-- Public group
----------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "HotPath"
  [ producerBench
  , consumerBench
  ]

----------------------------------------------------------------------
-- Shared setup (one cluster / accumulator / consumer per run)
----------------------------------------------------------------------

{-# NOINLINE sharedMockProducer #-}
sharedMockProducer :: MP.MockProducer
sharedMockProducer = unsafePerformIO $ do
  c  <- MC.newMockCluster 1
  MC.createTopic c "topic" 1
  fp <- MC.noFaults
  MP.newMockProducer c fp Nothing

{-# NOINLINE sharedAccumulator #-}
sharedAccumulator :: BA.BatchAccumulator
sharedAccumulator = unsafePerformIO $
  BA.createBatchAccumulator
    (16 * 1024 * 1024)  -- 16 MiB ceiling so we never split mid-bench
    100_000             -- linger 100 s — never roll over by time
    Compression.NoCompression
    (Compression.defaultLevel Compression.NoCompression)

{-# NOINLINE sharedConsumer1 #-}
sharedConsumer1 :: MC.MockConsumer
sharedConsumer1 = unsafePerformIO (mkConsumer 1)

{-# NOINLINE sharedConsumer100 #-}
sharedConsumer100 :: MC.MockConsumer
sharedConsumer100 = unsafePerformIO (mkConsumer 100)

mkConsumer :: Int -> IO MC.MockConsumer
mkConsumer n = do
  c <- MC.newMockCluster 1
  MC.createTopic c "topic" 1
  -- Seed n records into partition 0 so the first poll has something
  -- to drain. We re-seed inside the benchmark loop because pollMC
  -- consumes the records; for steady-state numbers see the
  -- 'pollMC' bench in the dedicated subgroup above.
  let !payload = BS.replicate 100 0x41
  mapM_ (\i -> MC.appendToPartition c "topic" 0 (Just "k") payload
                  (fromIntegral i) [] Nothing)
        [0 .. n - 1]
  fp <- MC.noFaults
  cons <- MC.newMockConsumer c fp (MC.GroupId "g") MC.ReadUncommitted (max 1 n)
  MC.subscribeMC cons ["topic"]
  pure cons

----------------------------------------------------------------------
-- Sample data
----------------------------------------------------------------------

payload100, payload1k :: BS.ByteString
payload100 = BS.replicate 100  0x41
payload1k  = BS.replicate 1024 0x41

sampleRecord :: RB.Record
sampleRecord = RB.Record
  { RB.recordTimestampDelta = 0
  , RB.recordOffsetDelta    = 0
  , RB.recordKey            = Just "k"
  , RB.recordValue          = payload100
  , RB.recordHeaders        = []
  }

sampleBatch :: Int -> BA.ProducerBatch
sampleBatch n = BA.ProducerBatch
  { BA.batchTopicPartition = BA.TopicPartition "topic" 0
  , BA.batchRecords        = Seq.fromList
      [ RB.Record 0 (fromIntegral i) (Just "k") payload100 []
      | i <- [0 .. n - 1]
      ]
  , BA.batchSizeBytes      = n * 120
  , BA.batchCreateTime     = 0
  , BA.batchBaseTimestamp  = 0
  , BA.batchState          = BA.Ready
  , BA.batchCompression    = Compression.NoCompression
  , BA.batchCompressionLevel =
      Compression.defaultLevel Compression.NoCompression
  , BA.batchCallbacks      = replicate n (\_ -> pure ())
  , BA.batchAttempts       = 0
  , BA.batchProducerId     = RB.noProducerId
  , BA.batchProducerEpoch  = RB.noProducerEpoch
  , BA.batchBaseSequence   = RB.noSequence
  , BA.batchIsTransactional = False
  }

builtBatch1, builtBatch10, builtBatch100 :: RB.RecordBatch
builtBatch1   = Sender.buildRecordBatch (sampleBatch 1)
builtBatch10  = Sender.buildRecordBatch (sampleBatch 10)
builtBatch100 = Sender.buildRecordBatch (sampleBatch 100)

encoded1, encoded10, encoded100 :: BS.ByteString
encoded1   = RB.encodeRecordBatch builtBatch1
encoded10  = RB.encodeRecordBatch builtBatch10
encoded100 = RB.encodeRecordBatch builtBatch100

decodeOrDie :: BS.ByteString -> Int
decodeOrDie bs = case RB.decodeRecordBatch bs of
  Left err -> error err
  Right rb -> length (RB.batchRecords rb)
                   -- forces every record (Vector elements are
                   -- already strict by construction).

decodeLegacy :: BS.ByteString -> Int
decodeLegacy bs = case RB.decodeRecordBatch bs of
  Left e   -> error e
  Right rb -> length (RB.batchRecords rb)

decodeWire :: BS.ByteString -> Int
decodeWire bs = case RBW.decodeRecordBatchWire bs of
  Left e   -> error e
  Right rb -> length (RB.batchRecords rb)

----------------------------------------------------------------------
-- Loops
----------------------------------------------------------------------

sendNTimes :: MP.MockProducer -> Int -> BS.ByteString -> IO ()
sendNTimes p !n !val = go n
  where
    go 0 = pure ()
    go !k = do
      _ <- MP.sendMockH p "topic" 0 (Just "k") val 0 []
      go (k - 1)

appendNRecords :: BA.BatchAccumulator -> Int -> IO ()
appendNRecords acc !n = go n
  where
    go 0 = pure ()
    go !k = do
      _ <- BA.appendRecordStamped acc (BA.TopicPartition "topic" 0)
              sampleRecord (\_ -> pure ()) BA.noStamp
      go (k - 1)
