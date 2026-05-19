{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streams.Properties.WatermarkWiringSpec
-- Description : Engine-side wiring of the watermark coordinator
--
-- Property + unit tests that exercise:
--
--   1. A source built with @consumed `withWatermarkStrategy` s@
--      surfaces 's' through the topology graph's
--      'sourceWatermarkStrategy' field.
--   2. After 'attachWatermarkCoordinator' wires a coordinator to
--      an engine, every record pushed through that source
--      arrives at the coordinator's per-source watermark.
--   3. Sources without a strategy do NOT report to the
--      coordinator (legacy path).
module Streams.Properties.WatermarkWiringSpec (tests) where

import Control.Monad (forM_)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Imperative
  ( Timestamp (..)
  , buildTopology
  , closeDriver
  , consumed
  , newDriver
  , newStreamsBuilder
  , pipeInput
  , streamFromTopic
  , textSerde
  , topologyValidGraph
  , validateTopology
  )
import qualified Kafka.Streams.Consumed as Consumed
import Kafka.Streams.Driver (driverEngine)
import Kafka.Streams.Internal.Engine (attachWatermarkCoordinator)
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Time (millis, minTimestamp)
import Kafka.Streams.Watermark
import qualified Data.ByteString.Char8 as BSC

----------------------------------------------------------------------
-- Test topology
----------------------------------------------------------------------

bytes :: String -> BSC.ByteString
bytes = BSC.pack

----------------------------------------------------------------------
-- 1. Strategy round-trip through SourceSpec
----------------------------------------------------------------------

unit_consumed_carries_strategy :: TestTree
unit_consumed_carries_strategy =
  testCase "consumed.withWatermarkStrategy: round-trips into SourceSpec" $ do
    b <- newStreamsBuilder
    let c = Consumed.withWatermarkStrategy monotonicAscending
              (consumed textSerde textSerde)
    _ <- streamFromTopic b "in" c
    topo <- buildTopology b
    case validateTopology topo of
      Left err -> error (show err)
      Right v  -> do
        let g = topologyValidGraph v
        case find ((== "in") . head . Topo.sourceTopics)
                  (Map.elems (Topo.topoSources g)) of
          Nothing -> error "no source named 'in'"
          Just spec ->
            wsName <$> Topo.sourceWatermarkStrategy spec
              @?= Just "monotonic-ascending"

----------------------------------------------------------------------
-- 2. Engine routes records to the coordinator
----------------------------------------------------------------------

unit_engine_reports_to_coordinator :: TestTree
unit_engine_reports_to_coordinator =
  testCase "attachWatermarkCoordinator + pipeInput: per-source wm advances" $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 60_000))
    b <- newStreamsBuilder
    let c = Consumed.withWatermarkStrategy monotonicAscending
              (consumed textSerde textSerde)
    _ <- streamFromTopic b "in" c
    topo <- buildTopology b
    driver <- newDriver topo "wm-wire-app"
    attachWatermarkCoordinator (driverEngine driver) coord

    forM_ [Timestamp 100, Timestamp 50, Timestamp 200] $ \ts ->
      pipeInput driver "in" (Just (bytes "k")) (bytes "v") ts 0

    eff <- currentEffectiveWatermark coord
    eff @?= Timestamp 200
    closeDriver driver

----------------------------------------------------------------------
-- 3. Sources WITHOUT a strategy do not report
----------------------------------------------------------------------

unit_no_strategy_no_report :: TestTree
unit_no_strategy_no_report =
  testCase "source without strategy does not register with coordinator" $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 60_000))
    b <- newStreamsBuilder
    -- Plain consumed; no withWatermarkStrategy.
    _ <- streamFromTopic b "in" (consumed textSerde textSerde)
    topo <- buildTopology b
    driver <- newDriver topo "wm-wire-noop"
    attachWatermarkCoordinator (driverEngine driver) coord

    forM_ [Timestamp 100, Timestamp 200, Timestamp 300] $ \ts ->
      pipeInput driver "in" (Just (bytes "k")) (bytes "v") ts 0

    eff <- currentEffectiveWatermark coord
    -- The coordinator has no registered sources, so its
    -- effective watermark stays at the empty-set sentinel.
    eff @?= minTimestamp
    closeDriver driver

----------------------------------------------------------------------
-- 4. Alignment group survives the round-trip
----------------------------------------------------------------------

unit_strategy_alignment_round_trips :: TestTree
unit_strategy_alignment_round_trips =
  testCase "withAlignment survives into SourceSpec and registers" $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 60_000))
    let strat = withAlignment (AlignmentGroupId "g1") monotonicAscending
    b <- newStreamsBuilder
    let c = Consumed.withWatermarkStrategy strat
              (consumed textSerde textSerde)
    _ <- streamFromTopic b "in" c
    topo <- buildTopology b
    driver <- newDriver topo "wm-wire-align"
    attachWatermarkCoordinator (driverEngine driver) coord
    pipeInput driver "in" (Just (bytes "k")) (bytes "v")
      (Timestamp 100) 0
    -- The source is in 'g1'. perSourceWatermarks should
    -- surface that.
    snap <- perSourceWatermarks coord
    case snap of
      [(_, _, _, mg)] -> mg @?= Just "g1"
      _ -> error ("unexpected perSourceWatermarks: " <> show snap)
    closeDriver driver

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Watermark wiring (engine integration)"
  [ unit_consumed_carries_strategy
  , unit_engine_reports_to_coordinator
  , unit_no_strategy_no_report
  , unit_strategy_alignment_round_trips
  ]
