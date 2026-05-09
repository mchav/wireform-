{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Benchmarks.HwKafkaComparison
Description : End-to-end producer comparison against the hw-kafka (librdkafka) bindings

Run only when @WIREFORM_KAFKA_BROKER=host:port@ is set (and points at
a live Kafka broker; the @test-integration/docker-compose.yml@
fixture is the canonical setup). Measures records/sec for an
identical workload through both clients:

  * wireform-kafka  (this package)
  * hw-kafka-client (librdkafka bindings)

The comparison is the most direct way to verify the per-record
CPU envelope claim — full encode + queue + flush + ack round-trip
end to end.

Both producers run with the same config (acks=1, no compression,
16 KB batches, 5 ms linger, 1 partition). The benchmark target
times producing 100 000 records into a fresh topic and divides
by total wall time to get records/sec.

If the env var isn't set, the benchmark group reports an empty
"skipped" group so 'criterion' doesn't fail the run.

== When this is wired in

This module isn't currently registered in 'bench/Main.hs' — adding
it pulls in the hw-kafka-client cabal dep and the libpq /
librdkafka native libs, which raises the cost of a "cabal build"
on a VM that doesn't need them. Add the line

    , HwKafkaComparison.benchmarks

to 'bench/Main.hs' and the 'hw-kafka-client' build-depends to the
benchmark stanza in @wireform-kafka.cabal@ before running.
-}
module Benchmarks.HwKafkaComparison
  ( benchmarks
  ) where

import Criterion (Benchmark, bench, bgroup, nfIO)
import qualified Data.ByteString as BS
import System.Environment (lookupEnv)
import GHC.IO (unsafePerformIO)

----------------------------------------------------------------------
-- Public group
----------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "HwKafkaComparison"
  [ case envBroker of
      Nothing ->
        bench "skipped (set WIREFORM_KAFKA_BROKER=host:port to enable)" $
          nfIO (pure ())
      Just _broker ->
        bgroup "1 partition, 100 KB total"
          [ bench "wireform-kafka producer" $ nfIO produceWireform
          , bench "hw-kafka       producer" $ nfIO produceHwKafka
          ]
  ]

----------------------------------------------------------------------
-- Workload knobs
----------------------------------------------------------------------

-- | Total records the bench will publish per iteration.
batchSize :: Int
batchSize = 100_000

-- | Per-record value size.
valueSize :: Int
valueSize = 200

-- | Topic the bench will produce into. Must already exist on the
-- broker (the integration fixture creates it on startup).
topicName :: String
topicName = "wireform-bench-cmp"

----------------------------------------------------------------------
-- Implementations
----------------------------------------------------------------------

-- | Saturate the wireform-kafka producer for one batch's worth of
-- records.
--
-- Implementation note: this is a stub. The full implementation
-- mirrors the producer setup in
-- @test-integration/Integration/TransactionalSpec.hs@ but with
-- @producerTransactional = Nothing@ and the workload above.
-- Wire it up when the cabal dep is enabled.
produceWireform :: IO ()
produceWireform = pure ()

-- | Saturate the hw-kafka producer for the same workload.
--
-- Pseudo-code (uncomment when hw-kafka-client is added to the
-- benchmark deps):
--
-- @
-- import qualified Kafka.Producer as HW
-- import           Kafka.Producer.Types
--
-- produceHwKafka = do
--   let !broker = unsafePerformIO (lookupEnv "WIREFORM_KAFKA_BROKER")
--   p <- either (error . show) pure =<< HW.newProducer (props broker)
--   let !payload = BS.replicate valueSize 0x41
--   forM_ [0 .. batchSize - 1] $ \i -> do
--     _ <- HW.produceMessage p (msg payload (showBS i))
--     pure ()
--   HW.flushProducer p
--   HW.closeProducer p
--   where
--     props b = HW.brokersList [HW.BrokerAddress (T.pack b)]
--             <> HW.compression HW.NoCompression
--             <> HW.sendTimeout (HW.Timeout 30000)
--     msg payload key =
--       HW.ProducerRecord
--         { HW.prTopic     = HW.TopicName (T.pack topicName)
--         , HW.prPartition = HW.UnassignedPartition
--         , HW.prKey       = Just key
--         , HW.prValue     = Just payload
--         , HW.prHeaders   = mempty
--         }
-- @
produceHwKafka :: IO ()
produceHwKafka = pure ()

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{-# NOINLINE envBroker #-}
envBroker :: Maybe String
envBroker = unsafePerformIO (lookupEnv "WIREFORM_KAFKA_BROKER")
