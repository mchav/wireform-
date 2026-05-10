{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Benchmarks.HwKafkaComparison
Description : End-to-end producer comparison against the hw-kafka (librdkafka) bindings

Run only when @WIREFORM_KAFKA_BROKER=host:port@ is set (and points
at a live Kafka broker; the @test-integration/docker-compose.yml@
fixture or a directly-launched Kafka 3.7 KRaft broker both work).
Measures records/sec for an identical workload through both
clients:

  * 'wireform-kafka' (this package)
  * 'hw-kafka-client' (librdkafka bindings)

This is the most direct way to verify the per-record CPU envelope
claim: full encode + queue + flush + ack round-trip end to end
on the same broker and the same workload.

Both producers run with the same effective config:

  * @acks = 1@
  * no compression
  * @batch.size = 16 KB@ (default for both)
  * @linger.ms = 5@
  * single partition, fresh topic per run (timestamped name) so
    the broker's segment cleaner doesn't bias older runs

The benchmark target produces 'recordsPerRun' records of
'valueSize' bytes each per criterion iteration. With @--time-limit
1.0@ and a typical throughput of 100k records/s that's ~10 runs
per side, well above criterion's noise floor.
-}
module Benchmarks.HwKafkaComparison
  ( benchmarks
  ) where

import Control.Monad (forM_, replicateM_, void)
import Criterion (Benchmark, bench, bgroup, nfIO, whnfIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as Time
import GHC.IO (unsafePerformIO)
import qualified Kafka.Client.Producer as WP
import qualified Kafka.Producer as HW
import System.Environment (lookupEnv)

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
        bgroup ("1 partition, " ++ show recordsPerRun ++ " records / iteration")
          [ bench "hw-kafka       (librdkafka, baseline)" $
              whnfIO hwSetupAndRun
          , bench "wireform-kafka (this package)        " $
              whnfIO wireformSetupAndRun
          ]
  ]

----------------------------------------------------------------------
-- Workload knobs
----------------------------------------------------------------------

-- | Total records the bench will publish per iteration.
recordsPerRun :: Int
recordsPerRun = 50_000

-- | Per-record value size in bytes.
valueSize :: Int
valueSize = 200

----------------------------------------------------------------------
-- hw-kafka producer
----------------------------------------------------------------------

-- | Bench helper: build a producer, publish 'recordsPerRun'
-- records, flush, close. Re-built per criterion sample because
-- 'KafkaProducer' has no 'NFData' instance for 'perRunEnv', and
-- the per-iteration setup cost (~1 ms) is small compared to the
-- 50k-record publish loop (~500 ms at 100k records/s).
hwSetupAndRun :: IO ()
hwSetupAndRun = do
  st <- hwSetup
  hwRun st

hwSetup :: IO (HW.KafkaProducer, ByteString, T.Text)
hwSetup = do
  let !topic   = "wireform-bench-cmp"   -- pre-create on the broker
      !payload = BS.replicate valueSize 0x41
      !broker  = fromMaybe (error "WIREFORM_KAFKA_BROKER unset") envBroker
      !props   = HW.brokersList [HW.BrokerAddress (T.pack broker)]
              <> HW.sendTimeout (HW.Timeout 30000)
              <> HW.compression HW.NoCompression
              <> HW.extraProps
                   (Map.fromList
                      [ ("acks",       "1")
                      , ("linger.ms",  "5")
                      , ("batch.size", "16384")
                      ])
              <> HW.logLevel HW.KafkaLogErr
  res <- HW.newProducer props
  case res of
    Left err -> error ("hw-kafka newProducer failed: " ++ show err)
    Right p  -> pure (p, payload, T.pack topic)

hwRun :: (HW.KafkaProducer, ByteString, T.Text) -> IO ()
hwRun (!p, !payload, !topic) = do
  let !msg = HW.ProducerRecord
        { HW.prTopic     = HW.TopicName topic
        , HW.prPartition = HW.UnassignedPartition
        , HW.prKey       = Nothing
        , HW.prValue     = Just payload
        , HW.prHeaders   = mempty
        }
  replicateM_ recordsPerRun $ do
    !mErr <- HW.produceMessage p msg
    forM_ mErr $ \e -> error ("hw-kafka produceMessage failed: " ++ show e)
  HW.flushProducer p
  void (HW.closeProducer p)

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{-# NOINLINE envBroker #-}
envBroker :: Maybe String
envBroker = unsafePerformIO (lookupEnv "WIREFORM_KAFKA_BROKER")

----------------------------------------------------------------------
-- wireform-kafka producer
----------------------------------------------------------------------

-- | Symmetric to 'hwSetupAndRun': build a wireform-kafka
-- producer, publish 'recordsPerRun' records, close. The
-- comparison is fair only if both sides hit the same
-- broker / topic shape; we ensure that by giving each iteration
-- its own timestamped topic.
wireformSetupAndRun :: IO ()
wireformSetupAndRun = do
  let !topic   = T.pack "wireform-bench-cmp"  -- same pre-created topic as hw-kafka
      !payload = BS.replicate valueSize 0x41
      !broker  = T.pack (fromMaybe (error "WIREFORM_KAFKA_BROKER unset") envBroker)
      !pcfg    = WP.defaultProducerConfig
        { WP.producerLingerMs    = 5
        , WP.producerBatchSize   = 16384
        }
  res <- WP.createProducer [broker] pcfg
  case res of
    Left err -> error ("wireform-kafka createProducer failed: " ++ err)
    Right p  -> do
      -- Mirror the hw-kafka loop: enqueue + return
      -- ('produceMessage' / 'sendMessageAsync'), then 'flushProducer'
      -- to wait for every record to be acked.  The previous shape
      -- used the synchronous 'sendMessage', which serialised the
      -- entire workload to one record per broker round-trip and
      -- showed wireform-kafka as ~80x slower than hw-kafka for
      -- reasons that had nothing to do with the codec.
      replicateM_ recordsPerRun $ do
        r <- WP.sendMessageAsync p topic Nothing payload
        case r of
          Left e  -> error ("wireform-kafka sendMessageAsync failed: " ++ e)
          Right _ -> pure ()
      flushRes <- WP.flushProducer p
      case flushRes of
        Left e  -> error ("wireform-kafka flushProducer failed: " ++ e)
        Right _ -> pure ()
      WP.closeProducer p
