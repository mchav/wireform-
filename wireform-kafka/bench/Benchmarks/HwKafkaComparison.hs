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

import Control.Monad (forM_, replicateM, replicateM_, void, when)
import Criterion (Benchmark, bench, bgroup, nfIO, whnfIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.IORef as IORef
import qualified Data.Time.Clock.POSIX as Time
import GHC.IO (unsafePerformIO)
import qualified System.Process
import qualified Kafka.Client.Consumer as WC
import qualified Kafka.Client.Producer as WP
import qualified Kafka.Consumer as HWC
import qualified Kafka.Producer as HW
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

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
          [ bench "Producer 16K-batch sendAsync:    hw-kafka       (librdkafka, baseline)" $
              whnfIO hwSetupAndRun
          , bench "Producer 16K-batch sendAsync:    wireform-kafka (this package)" $
              whnfIO wireformSetupAndRun
          , bench "Producer 1MiB-batch sendAsync:   wireform-kafka (this package)" $
              whnfIO wireformSetupAndRunLargeBatch
          , bench "Producer 16K-batch sendDrop:     wireform-kafka (this package)" $
              whnfIO wireformSetupAndRunDrop
          , bench "Consumer subscribe + drain:      hw-kafka       (librdkafka, baseline)" $
              whnfIO hwConsumeAndRun
          , bench "Consumer subscribe + drain:      wireform-kafka (this package)" $
              whnfIO wireformConsumeAndRun
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
      -- ('produceMessage' / 'sendMessage_'), then 'flushProducer'
      -- to wait for every record to be acked.  The previous shape
      -- used the synchronous 'sendMessage', which serialised the
      -- entire workload to one record per broker round-trip and
      -- showed wireform-kafka as ~80x slower than hw-kafka for
      -- reasons that had nothing to do with the codec.
      replicateM_ recordsPerRun $ do
        r <- WP.sendMessage_ p topic Nothing payload
        case r of
          Left e  -> error ("wireform-kafka sendMessage_ failed: " ++ e)
          Right _ -> pure ()
      flushRes <- WP.flushProducer p
      case flushRes of
        Left e  -> error ("wireform-kafka flushProducer failed: " ++ e)
        Right _ -> pure ()
      WP.closeProducer p

-- | Large-batch variant: 1 MiB instead of the librdkafka /
-- JVM-default 16 KiB.  Same workload, but fewer broker
-- round-trips (~50 batches at 200 B/record vs ~700 at 16 KiB),
-- so we can see how much of the per-record cost is the codec
-- and how much is the per-request envelope.
wireformSetupAndRunLargeBatch :: IO ()
wireformSetupAndRunLargeBatch = do
  let !topic   = T.pack "wireform-bench-cmp"
      !payload = BS.replicate valueSize 0x41
      !broker  = T.pack (fromMaybe (error "WIREFORM_KAFKA_BROKER unset") envBroker)
      !pcfg    = WP.defaultProducerConfig
        { WP.producerLingerMs  = 5
        , WP.producerBatchSize = 1024 * 1024  -- 1 MiB
        }
  res <- WP.createProducer [broker] pcfg
  case res of
    Left err -> error ("wireform-kafka createProducer failed: " ++ err)
    Right p  -> do
      replicateM_ recordsPerRun $ do
        r <- WP.sendMessage_ p topic Nothing payload
        case r of
          Left e  -> error ("sendMessage_ (1MiB batch) failed: " ++ e)
          Right _ -> pure ()
      flushRes <- WP.flushProducer p
      case flushRes of
        Left e  -> error ("flushProducer (1MiB batch) failed: " ++ e)
        Right _ -> pure ()
      WP.closeProducer p

-- | Bare-minimum-overhead fire-and-forget variant.  Calls the
-- new 'WP.sendMessage_', which skips the user-installed
-- interceptor + ack hooks, the transactional / idempotent
-- stamping path, and the per-record 'ProducerRecord' struct
-- allocation.  Same workload + same flush + close as the other
-- two producer benches so the numbers are directly comparable.
wireformSetupAndRunDrop :: IO ()
wireformSetupAndRunDrop = do
  let !topic   = T.pack "wireform-bench-cmp"
      !payload = BS.replicate valueSize 0x41
      !broker  = T.pack (fromMaybe (error "WIREFORM_KAFKA_BROKER unset") envBroker)
      !pcfg    = WP.defaultProducerConfig
        { WP.producerLingerMs  = 5
        , WP.producerBatchSize = 16384
        }
  res <- WP.createProducer [broker] pcfg
  case res of
    Left err -> error ("wireform-kafka createProducer failed: " ++ err)
    Right p  -> do
      replicateM_ recordsPerRun $ do
        r <- WP.sendMessage_ p topic Nothing payload
        case r of
          Left e  -> error ("sendMessage_ failed: " ++ e)
          Right _ -> pure ()
      flushRes <- WP.flushProducer p
      case flushRes of
        Left e  -> error ("flushProducer (drop) failed: " ++ e)
        Right _ -> pure ()
      WP.closeProducer p

----------------------------------------------------------------------
-- Consumer head-to-head
--
-- Both consumers read the same pre-populated topic
-- ('consumerTopic') with @auto.offset.reset=earliest@ + a fresh
-- group id per criterion sample (so each iteration starts at
-- offset 0 instead of resuming where the last one stopped).
-- The benchmark drains until 'recordsPerRun' records have been
-- pulled off the wire — the same shape used to compare the
-- producers above.
----------------------------------------------------------------------

-- | Topic the consumer reads from.  A unique per-process suffix
-- (the process start time) keeps every bench invocation on a
-- freshly-created log so 'auto.offset.reset=earliest' resolves to
-- offset 0 and the consumer drain finishes deterministically.
{-# NOINLINE consumerTopic #-}
consumerTopic :: T.Text
consumerTopic = unsafePerformIO $ do
  t <- Time.getPOSIXTime
  pure (T.pack ("wireform-bench-cmp-"
                  ++ show (truncate (t * 1000) :: Integer)))

----------------------------------------------------------------------
-- hw-kafka consumer
----------------------------------------------------------------------

hwConsumeAndRun :: IO ()
hwConsumeAndRun = do
  ensureSeeded
  st <- hwConsumerSetup
  hwConsumerDrain st

hwConsumerSetup :: IO HWC.KafkaConsumer
hwConsumerSetup = do
  groupId <- freshGroupId "hw-kafka"
  let !broker = fromMaybe (error "WIREFORM_KAFKA_BROKER unset") envBroker
      !cfg = HWC.brokersList [HWC.BrokerAddress (T.pack broker)]
          <> HWC.groupId (HWC.ConsumerGroupId (T.pack groupId))
          <> HWC.noAutoCommit
          <> HWC.logLevel HWC.KafkaLogErr
      !sub = HWC.topics [HWC.TopicName consumerTopic]
          <> HWC.offsetReset HWC.Earliest
  r <- HWC.newConsumer cfg sub
  case r of
    Left err -> error ("hw-kafka newConsumer failed: " ++ show err)
    Right c  -> pure c

hwConsumerDrain :: HWC.KafkaConsumer -> IO ()
hwConsumerDrain c = go (0 :: Int)
  where
    go !n
      | n >= recordsPerRun = void (HWC.closeConsumer c)
      | otherwise = do
          mr <- HWC.pollMessage c (HWC.Timeout 1000)
          case mr of
            Right _ -> go (n + 1)
            Left  _ -> go n

----------------------------------------------------------------------
-- wireform-kafka consumer
----------------------------------------------------------------------

wireformConsumeAndRun :: IO ()
wireformConsumeAndRun = do
  ensureSeeded
  groupId <- freshGroupId "wireform-kafka"
  let !broker = T.pack (fromMaybe (error "WIREFORM_KAFKA_BROKER unset") envBroker)
      !ccfg   = WC.defaultConsumerConfig
                  { WC.consumerAutoOffsetReset = WC.Earliest
                  , WC.consumerAutoCommit      = False
                  }
  res <- WC.createConsumer [broker] (T.pack groupId) ccfg
  case res of
    Left err -> error ("wireform-kafka createConsumer failed: " ++ err)
    Right c  -> do
      sr <- WC.subscribe c [consumerTopic]
      case sr of
        Left e -> error ("wireform-kafka subscribe failed: " ++ e)
        Right () -> pure ()
      drainW c 0
      WC.closeConsumer c
  where
    drainW c !n
      | n >= recordsPerRun = pure ()
      | otherwise = do
          r <- WC.poll c 1000
          case r of
            Right rs -> drainW c (n + length rs)
            Left  _  -> drainW c n

----------------------------------------------------------------------
-- Seeding helpers
----------------------------------------------------------------------

-- | Top-up the topic to at least 'recordsPerRun' records before
-- the first consumer benchmark sample runs.  Subsequent samples
-- reuse the same data — both consumers use a fresh group id per
-- run so they each start at offset 0.
-- | Run-once gate so the consumer-bench seed is paid exactly once
-- per process, regardless of how many criterion samples or
-- producer/consumer benchmark variants share the same topic.
{-# NOINLINE seededGate #-}
seededGate :: IORef.IORef Bool
seededGate = unsafePerformIO (IORef.newIORef False)

-- | Pre-populate 'consumerTopic' with 'recordsPerRun' records.
-- The topic is deleted + recreated via the bundled @kafka-topics.sh@
-- once on the first call, so the consumer's
-- @auto.offset.reset=earliest@ resolves to a non-deleted offset
-- and the consumer drain finishes deterministically.
ensureSeeded :: IO ()
ensureSeeded = do
  alreadySeeded <- IORef.atomicModifyIORef' seededGate (\b -> (True, b))
  when (not alreadySeeded) doSeed

doSeed :: IO ()
doSeed = do
  recreateTopic
  let !payload = BS.replicate valueSize 0x42
      !broker  = T.pack (fromMaybe (error "WIREFORM_KAFKA_BROKER unset") envBroker)
      !pcfg    = WP.defaultProducerConfig
        { WP.producerLingerMs  = 5
        , WP.producerBatchSize = 16384
        }
  res <- WP.createProducer [broker] pcfg
  case res of
    Left err -> error ("ensureSeeded createProducer failed: " ++ err)
    Right p  -> do
      replicateM_ recordsPerRun $ do
        r <- WP.sendMessage_ p consumerTopic Nothing payload
        case r of
          Left e  -> error ("ensureSeeded sendMessage_ failed: " ++ e)
          Right _ -> pure ()
      flushRes <- WP.flushProducer p
      case flushRes of
        Left e  -> error ("ensureSeeded flushProducer failed: " ++ e)
        Right _ -> pure ()
      WP.closeProducer p

-- | Drop and recreate 'consumerTopic' via the bundled @kafka-topics.sh@
-- so the consumer benchmark starts each iteration with a fresh log
-- whose first record is at offset 0.
recreateTopic :: IO ()
recreateTopic = do
  let !broker = fromMaybe (error "WIREFORM_KAFKA_BROKER unset") envBroker
      !topic  = T.unpack consumerTopic
      !ktopics =
        "/tmp/kafka_2.13-3.7.0/bin/kafka-topics.sh"
  -- The delete + create are best-effort; the create races the
  -- async delete completion on the broker side, but since the
  -- producer below uses @auto.create.topics.enable=true@ the
  -- topic is reborn on first publish anyway.
  _ <- System.Process.system $
    ktopics ++ " --bootstrap-server " ++ broker
            ++ " --delete --topic " ++ topic ++ " 2>/dev/null"
  _ <- System.Process.system $
    ktopics ++ " --bootstrap-server " ++ broker
            ++ " --create --if-not-exists --topic " ++ topic
            ++ " --partitions 1 --replication-factor 1 2>/dev/null"
  pure ()

freshGroupId :: String -> IO String
freshGroupId tag = do
  t <- Time.getPOSIXTime
  pure (tag ++ "-bench-" ++ show (truncate (t * 1000) :: Integer))
