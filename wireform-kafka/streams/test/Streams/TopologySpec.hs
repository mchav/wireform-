{-# LANGUAGE OverloadedStrings #-}

-- | Tests focused on the low-level Topology / validation surface.
module Streams.TopologySpec (tests) where

import Data.Foldable qualified as Foldable
import Data.Map.Strict qualified as Map
import Kafka.Streams.Processor (Processor (..), processorName)
import Kafka.Streams.Serde (textSerde)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStoreBuilder)
import Kafka.Streams.State.Store (storeName)
import Kafka.Streams.Time (recordTimestampExtractor)
import Kafka.Streams.Topology (
  NodeName (..),
  addProcessor,
  addSink,
  addSource,
  addStateStoreKV,
  childrenOf,
  emptyTopology,
  parentsOf,
  topoStores,
  validateTopology,
 )
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (topicName)
import Test.Syd


tests :: Spec
tests =
  describe "Topology" $
    sequence_
      [ valid_minimal_topology
      , children_index_correct
      , parents_of_processor
      , unknown_parent_caught_by_validation
      , no_sources_caught_by_validation
      , empty_source_topics_caught
      , state_store_added_to_processor
      , topology_round_trip_in_order
      ]


mkPassthroughProc :: IO (Processor () ())
mkPassthroughProc =
  pure
    Processor
      { procName = processorName "PT"
      , procInit = \_ -> pure ()
      , procClose = pure ()
      , procProcess = \_ -> pure ()
      }


valid_minimal_topology :: Spec
valid_minimal_topology =
  it "source -> processor -> sink validates" $ do
    let t =
          addSource
            (NodeName "src")
            [topicName "in"]
            textSerde
            textSerde
            recordTimestampExtractor
            $ addProcessor (NodeName "proc") [NodeName "src"] mkPassthroughProc
            $ addSink
              (NodeName "snk")
              (topicName "out")
              textSerde
              textSerde
              [NodeName "proc"]
            $ emptyTopology
    case validateTopology t of
      Right _ -> pure ()
      Left err -> expectationFailure ("validation failed: " <> show err)


children_index_correct :: Spec
children_index_correct =
  it "childrenOf reflects the wired topology" $ do
    let t =
          addSource
            (NodeName "src")
            [topicName "in"]
            textSerde
            textSerde
            recordTimestampExtractor
            $ addProcessor (NodeName "proc") [NodeName "src"] mkPassthroughProc
            $ addSink
              (NodeName "snk")
              (topicName "out")
              textSerde
              textSerde
              [NodeName "proc"]
            $ emptyTopology
    childrenOf t (NodeName "src") `shouldBe` [NodeName "proc"]
    childrenOf t (NodeName "proc") `shouldBe` [NodeName "snk"]
    childrenOf t (NodeName "snk") `shouldBe` []


parents_of_processor :: Spec
parents_of_processor =
  it "parentsOf returns the right set" $ do
    let t =
          addSource
            (NodeName "src")
            [topicName "in"]
            textSerde
            textSerde
            recordTimestampExtractor
            $ addProcessor (NodeName "proc") [NodeName "src"] mkPassthroughProc
            $ emptyTopology
    parentsOf t (NodeName "proc") `shouldBe` [NodeName "src"]


unknown_parent_caught_by_validation :: Spec
unknown_parent_caught_by_validation =
  it "unknown parent is rejected by validation" $ do
    let t =
          addSource
            (NodeName "src")
            [topicName "in"]
            textSerde
            textSerde
            recordTimestampExtractor
            $ addProcessor (NodeName "proc") [NodeName "missing"] mkPassthroughProc
            $ emptyTopology
    case validateTopology t of
      Right _ -> expectationFailure "expected validation failure"
      Left e ->
        ( if ( case e of
                 Topo.UnknownParent _ _ -> True
                 _ -> False
             )
            then pure ()
            else expectationFailure ("got " <> show e)
        )


no_sources_caught_by_validation :: Spec
no_sources_caught_by_validation =
  it "topology without sources is rejected" $ do
    let t = emptyTopology
    case validateTopology t of
      Right _ -> expectationFailure "expected NoSources"
      Left Topo.NoSources -> pure ()
      Left other -> expectationFailure ("expected NoSources, got " <> show other)


empty_source_topics_caught :: Spec
empty_source_topics_caught =
  it "source with empty topic list is rejected" $ do
    let t =
          addSource
            (NodeName "src")
            []
            textSerde
            textSerde
            recordTimestampExtractor
            $ emptyTopology
    case validateTopology t of
      Right _ -> expectationFailure "expected EmptySourceTopics"
      Left (Topo.EmptySourceTopics _) -> pure ()
      Left other -> expectationFailure ("expected EmptySourceTopics, got " <> show other)


state_store_added_to_processor :: Spec
state_store_added_to_processor =
  it "state stores attach to declared owners" $ do
    let sb = inMemoryKeyValueStoreBuilder @Int @Int (storeName "kv")
        t =
          addSource
            (NodeName "src")
            [topicName "in"]
            textSerde
            textSerde
            recordTimestampExtractor
            $ addProcessor (NodeName "proc") [NodeName "src"] mkPassthroughProc
            $ addStateStoreKV sb [NodeName "proc"]
            $ emptyTopology
    Map.size (topoStores t) `shouldBe` 1


topology_round_trip_in_order :: Spec
topology_round_trip_in_order =
  it "addSource/addProcessor/addSink record insertion order" $ do
    let t =
          addSink
            (NodeName "c")
            (topicName "out")
            textSerde
            textSerde
            [NodeName "b"]
            $ addProcessor (NodeName "b") [NodeName "a"] mkPassthroughProc
            $ addSource
              (NodeName "a")
              [topicName "in"]
              textSerde
              textSerde
              recordTimestampExtractor
            $ emptyTopology
    Foldable.toList (Topo.topoOrder t)
      `shouldBe` [NodeName "a", NodeName "b", NodeName "c"]
