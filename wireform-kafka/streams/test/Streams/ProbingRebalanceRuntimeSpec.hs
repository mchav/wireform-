{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | KIP-441 probing-rebalance: runtime-side wiring.
--
-- Verifies that the streams runtime invokes
-- 'sdRequestProbingRebalance' on the mock driver when:
--
--   * the probing interval has elapsed since the last probe,
--   * AND at least one warmup replica is "ready" (lag <=
--     acceptable.recovery.lag).
--
-- The driver's count is observable via 'mockDriverProbeRequests'.
module Streams.ProbingRebalanceRuntimeSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

import qualified Kafka.Client.Consumer as KC
import Kafka.Streams
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.NativeDriver

tests :: TestTree
tests = testGroup "Probing rebalance runtime wiring (KIP-441)"
  [ ready_warmup_with_zero_interval_does_not_probe
  , no_warmups_means_no_probe
  , ready_warmup_with_short_interval_eventually_probes
  ]

mkRec :: Text -> Text -> KC.ConsumerRecord
mkRec k v = KC.ConsumerRecord
  { KC.crTopic = "in"
  , KC.crPartition = 0
  , KC.crOffset = 0
  , KC.crTimestamp = 0
  , KC.crKey = Just (BSC.pack (T.unpack k))
  , KC.crValue = BSC.pack (T.unpack v)
  , KC.crHeaders = []
  }

buildPassthroughTopo :: IO TopologyValid
buildPassthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

cfgWith :: Int -> StreamsConfig
cfgWith intervalMs = defaultStreamsConfig
  { applicationId    = "probe-runtime"
  , bootstrapServers = ["mock:0"]
  , numStreamThreads = 1
  , pollMs           = 0
  , probingRebalanceIntervalMs = intervalMs
  , acceptableRecoveryLag      = 100
  }

----------------------------------------------------------------------
-- 1. interval = 0 disables probing
----------------------------------------------------------------------

ready_warmup_with_zero_interval_does_not_probe :: TestTree
ready_warmup_with_zero_interval_does_not_probe =
  testCase "probingRebalanceIntervalMs=0 disables probing even when warmups are ready" $ do
    topo <- buildPassthroughTopo
    ks <- newKafkaStreams (cfgWith 0) topo
    (drv, h) <- newMockDriver
    -- Mark a warmup as fully caught up.
    reportWarmupLag ks (TaskId 0 0) 0
    mockDriverInjectPoll h [mkRec "k" "v"]
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning
    _ <- awaitTicks ks 3
    probes <- mockDriverProbeRequests h
    probes @?= 0
    closeKafkaStreams ks
    awaitState ks StreamsClosed

----------------------------------------------------------------------
-- 2. No warmups -> no probe
----------------------------------------------------------------------

no_warmups_means_no_probe :: TestTree
no_warmups_means_no_probe =
  testCase "no registered warmups: probe never fires" $ do
    topo <- buildPassthroughTopo
    ks <- newKafkaStreams (cfgWith 1) topo  -- 1 ms — trivially elapsed
    (drv, h) <- newMockDriver
    mockDriverInjectPoll h [mkRec "k" "v"]
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning
    _ <- awaitTicks ks 5
    probes <- mockDriverProbeRequests h
    probes @?= 0
    closeKafkaStreams ks
    awaitState ks StreamsClosed

----------------------------------------------------------------------
-- 3. Ready warmup + short interval -> at least one probe
----------------------------------------------------------------------

ready_warmup_with_short_interval_eventually_probes :: TestTree
ready_warmup_with_short_interval_eventually_probes =
  testCase "ready warmup + short interval: probe fires at least once" $ do
    topo <- buildPassthroughTopo
    ks <- newKafkaStreams (cfgWith 1) topo
    (drv, h) <- newMockDriver
    -- Mark task (0, 0) as caught up (lag <= acceptableRecoveryLag).
    reportWarmupLag ks (TaskId 0 0) 0
    -- Drive a few poll cycles so the event-loop runs the probe
    -- check multiple times.
    mockDriverInjectPoll h [mkRec "k1" "v"]
    mockDriverInjectPoll h [mkRec "k2" "v"]
    mockDriverInjectPoll h [mkRec "k3" "v"]
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning
    _ <- awaitTicks ks 5
    probes <- mockDriverProbeRequests h
    assertBool
      ("expected >= 1 probe; got " <> show probes)
      (probes >= 1)
    closeKafkaStreams ks
    awaitState ks StreamsClosed
