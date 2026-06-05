{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Streams.TopologyStatsSpec
-- Description : Tests for topology structural statistics
module Streams.TopologyStatsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams.Imperative
import qualified Kafka.Streams.State.Store as Store
import qualified Kafka.Streams.Topology as Topo

import Kafka.Streams.Observability.TopologyStats

-- | One-source one-sink passthrough.
passthrough :: IO Topo.Topology
passthrough = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  buildTopology b

-- | A counting topology with one logged KV store.
counting :: IO Topo.Topology
counting = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde int64Serde)
  let g  = grouped textSerde int64Serde
      ks = groupByKey g s
  _ <- countStream (materializedAs (Store.storeName "counts")) ks
  buildTopology b

tests :: TestTree
tests = testGroup "TopologyStats"
  [ passthrough_counts
  , counting_has_logged_store
  ]

passthrough_counts :: TestTree
passthrough_counts =
  testCase "passthrough has one source, one sink, one edge, depth two" $ do
    st <- topologyStats <$> passthrough
    statSources st      @?= 1
    statSinks st        @?= 1
    statStores st       @?= 0
    statSourceTopics st @?= 1
    statSinkTopics st   @?= 1
    statEdges st        @?= 1
    statMaxDepth st     @?= 2

counting_has_logged_store :: TestTree
counting_has_logged_store =
  testCase "counting topology reports a logged store" $ do
    st <- topologyStats <$> counting
    assertBool "at least one store" (statStores st >= 1)
    assertBool "at least one logged store" (statLoggedStores st >= 1)
    assertBool "at least one source topic" (statSourceTopics st >= 1)
    assertBool "depth grows past the source" (statMaxDepth st >= 2)
