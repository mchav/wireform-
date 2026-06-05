{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Streams.TopologyStatsSpec
-- Description : Tests for topology structural statistics
module Streams.TopologyStatsSpec (tests) where

import Test.Syd

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

tests :: Spec
tests = describe "TopologyStats" $ sequence_
  [ passthrough_counts
  , counting_has_logged_store
  ]

passthrough_counts :: Spec
passthrough_counts =
  it "passthrough has one source, one sink, one edge, depth two" $ do
    st <- topologyStats <$> passthrough
    statSources st      `shouldBe` 1
    statSinks st        `shouldBe` 1
    statStores st       `shouldBe` 0
    statSourceTopics st `shouldBe` 1
    statSinkTopics st   `shouldBe` 1
    statEdges st        `shouldBe` 1
    statMaxDepth st     `shouldBe` 2

counting_has_logged_store :: Spec
counting_has_logged_store =
  it "counting topology reports a logged store" $ do
    st <- topologyStats <$> counting
    (statStores st >= 1) `shouldBe` True
    (statLoggedStores st >= 1) `shouldBe` True
    (statSourceTopics st >= 1) `shouldBe` True
    (statMaxDepth st >= 2) `shouldBe` True
