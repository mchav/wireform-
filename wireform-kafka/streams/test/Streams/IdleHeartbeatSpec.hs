{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.IdleHeartbeatSpec
-- Description : Regression: runtime reports poll cycles to the coordinator
--
-- Pin down the wiring added in 'Kafka.Streams.Internal.Engine.reportPollCycle':
-- the runtime poll loop must call it once per cycle so the
-- 'Kafka.Streams.Watermark.WatermarkCoordinator' can:
--
--   1. Advance its wall-clock so 'IdlenessConfig.idleTimeout'
--      has fresh timing.
--   2. Mark sources that didn't see records this cycle as idle.
--   3. Mark sources that DID see records as active (clearing
--      any prior idle flag).
--
-- These tests drive 'reportPollCycle' directly against an engine
-- that has a coordinator attached, mirroring the call the
-- 'eventLoop' / 'multiEventLoop' do per poll cycle.
module Streams.IdleHeartbeatSpec (tests) where

import qualified Data.HashSet as HashSet
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams.Imperative
import qualified Kafka.Streams.Consumed as Consumed
import Kafka.Streams.Driver (driverEngine)
import Kafka.Streams.Internal.Engine
  ( attachWatermarkCoordinator
  , reportPollCycle
  )
import Kafka.Streams.Time (millis, minTimestamp)
import Kafka.Streams.Watermark
  ( IdleTimeout (..)
  , IdlenessConfig (..)
  , currentEffectiveWatermark
  , monotonicAscending
  , newWatermarkCoordinator
  , perSourceWatermarks
  , withIdleness
  )

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

tests :: TestTree
tests = testGroup "Watermark idle-source heartbeat (Riffle §5)"
  [ unit_active_then_idle_unblocks_effective_watermark
  , unit_active_source_clears_prior_idle
  , unit_no_coordinator_is_noop
  ]

----------------------------------------------------------------------
-- 1. Idle source no longer blocks the effective watermark
----------------------------------------------------------------------

unit_active_then_idle_unblocks_effective_watermark :: TestTree
unit_active_then_idle_unblocks_effective_watermark =
  testCase "an idle source stops dragging the effective watermark down" $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 100))
    -- Two sources sharing a coordinator; both monotonic, both
    -- with the same short idle threshold so the test can
    -- exercise the gate deterministically.
    let strat = withIdleness (IdleAfter (millis 100)) monotonicAscending
    b <- newStreamsBuilder
    a_ <- streamFromTopic b (topicName "a")
             (Consumed.withWatermarkStrategy strat
                (consumed textSerde textSerde))
    bStream <- streamFromTopic b (topicName "b")
             (Consumed.withWatermarkStrategy strat
                (consumed textSerde textSerde))
    -- The streams aren't wired to a sink in this test; we only
    -- care about the source-side reporting. Tie both to a
    -- foreach so the builder considers them "used".
    foreachStream (\_ -> pure ()) a_
    foreachStream (\_ -> pure ()) bStream
    topo <- buildTopology b

    driver <- newDriver topo "idle-heartbeat-1"
    attachWatermarkCoordinator (driverEngine driver) coord

    -- Push one record per source.
    pipeInput driver (topicName "a") (Just (bytes "k"))
              (bytes "v") (Timestamp 100) 0
    pipeInput driver (topicName "b") (Just (bytes "k"))
              (bytes "v") (Timestamp 50) 0

    wmStart <- currentEffectiveWatermark coord
    -- Effective watermark = min(100, 50) = 50, both live.
    wmStart @?= Timestamp 50

    -- Source 'a' continues to fire; 'b' goes silent. Two poll
    -- cycles below the idle threshold should keep 'b' as a
    -- live source dragging the watermark down to 50.
    reportPollCycle (driverEngine driver) (HashSet.fromList [topicName "a"])
    -- Stretch the coordinator's wall-clock past the idle
    -- threshold (100 ms). Real runtime does this via the
    -- second 'reportPollCycle' call below; we accelerate by
    -- driving the coordinator's clock forward directly.
    advanceWallClockTime driver 200
    reportPollCycle (driverEngine driver) (HashSet.fromList [topicName "a"])

    wmEnd <- currentEffectiveWatermark coord
    -- With 'b' filtered out as idle, the effective watermark
    -- should now reflect only 'a' (Timestamp 100).
    assertBool
      ("expected wm >= 100 after b went idle; got " <> show wmEnd)
      (wmEnd >= Timestamp 100)
    closeDriver driver

----------------------------------------------------------------------
-- 2. An active poll on a previously idle source clears the flag
----------------------------------------------------------------------

unit_active_source_clears_prior_idle :: TestTree
unit_active_source_clears_prior_idle =
  testCase "markActive flips an idle source back to live" $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 100))
    let strat = withIdleness (IdleAfter (millis 100)) monotonicAscending
    b <- newStreamsBuilder
    a_ <- streamFromTopic b (topicName "a")
             (Consumed.withWatermarkStrategy strat
                (consumed textSerde textSerde))
    foreachStream (\_ -> pure ()) a_
    topo <- buildTopology b
    driver <- newDriver topo "idle-heartbeat-2"
    attachWatermarkCoordinator (driverEngine driver) coord

    -- Initial record. Coordinator's per-source watermark for
    -- 'a' is 50.
    pipeInput driver (topicName "a") Nothing (bytes "v") (Timestamp 50) 0

    -- Empty poll. Source flips to idle (after the threshold).
    advanceWallClockTime driver 200
    reportPollCycle (driverEngine driver) HashSet.empty

    -- Now an active poll: source should be re-marked active.
    reportPollCycle (driverEngine driver)
      (HashSet.singleton (topicName "a"))

    pairs <- perSourceWatermarks coord
    -- The source survived through the active call: still
    -- carrying its 'Timestamp 50' watermark even though it had
    -- spent a cycle marked idle.
    length pairs @?= 1
    wm <- currentEffectiveWatermark coord
    -- Source is back to live; effective wm = the source's
    -- own wm, i.e. 50.
    wm @?= Timestamp 50
    closeDriver driver

----------------------------------------------------------------------
-- 3. Without a coordinator, reportPollCycle is a no-op
----------------------------------------------------------------------

unit_no_coordinator_is_noop :: TestTree
unit_no_coordinator_is_noop =
  testCase "reportPollCycle is a no-op when no coordinator is attached" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in")
           (consumed textSerde textSerde)
    foreachStream (\_ -> pure ()) s
    topo <- buildTopology b
    driver <- newDriver topo "idle-heartbeat-3"
    -- Don't attach any coordinator.
    -- Should not throw or block.
    reportPollCycle (driverEngine driver) HashSet.empty
    reportPollCycle (driverEngine driver) (HashSet.singleton (topicName "in"))
    closeDriver driver
