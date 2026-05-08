{-# LANGUAGE OverloadedStrings #-}

-- | Tests focused on the low-level Topology / validation surface.
module Streams.TopologySpec (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams.Time (recordTimestampExtractor)
import Kafka.Streams.Serde (textSerde)
import Kafka.Streams.Topology
  ( NodeName (..)
  , addProcessor
  , addSink
  , addSource
  , addStateStoreKV
  , childrenOf
  , emptyTopology
  , parentsOf
  , topoStores
  , validateTopology
  )
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (topicName)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStoreBuilder)
import Kafka.Streams.State.Store (storeName)
import Kafka.Streams.Processor (Processor (..), processorName)

tests :: TestTree
tests = testGroup "Topology"
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
mkPassthroughProc = pure Processor
  { procName    = processorName "PT"
  , procInit    = \_ -> pure ()
  , procClose   = pure ()
  , procProcess = \_ -> pure ()
  }

valid_minimal_topology :: TestTree
valid_minimal_topology =
  testCase "source -> processor -> sink validates" $ do
    let t = addSource (NodeName "src") [topicName "in"]
              textSerde textSerde recordTimestampExtractor
          $ addProcessor (NodeName "proc") [NodeName "src"] mkPassthroughProc
          $ addSink (NodeName "snk") (topicName "out")
              textSerde textSerde [NodeName "proc"]
          $ emptyTopology
    case validateTopology t of
      Right _   -> pure ()
      Left  err -> error ("validation failed: " <> show err)

children_index_correct :: TestTree
children_index_correct =
  testCase "childrenOf reflects the wired topology" $ do
    let t = addSource (NodeName "src") [topicName "in"]
              textSerde textSerde recordTimestampExtractor
          $ addProcessor (NodeName "proc") [NodeName "src"] mkPassthroughProc
          $ addSink (NodeName "snk") (topicName "out")
              textSerde textSerde [NodeName "proc"]
          $ emptyTopology
    childrenOf t (NodeName "src")  @?= [NodeName "proc"]
    childrenOf t (NodeName "proc") @?= [NodeName "snk"]
    childrenOf t (NodeName "snk")  @?= []

parents_of_processor :: TestTree
parents_of_processor =
  testCase "parentsOf returns the right set" $ do
    let t = addSource (NodeName "src") [topicName "in"]
              textSerde textSerde recordTimestampExtractor
          $ addProcessor (NodeName "proc") [NodeName "src"] mkPassthroughProc
          $ emptyTopology
    parentsOf t (NodeName "proc") @?= [NodeName "src"]

unknown_parent_caught_by_validation :: TestTree
unknown_parent_caught_by_validation =
  testCase "unknown parent is rejected by validation" $ do
    let t = addSource (NodeName "src") [topicName "in"]
              textSerde textSerde recordTimestampExtractor
          $ addProcessor (NodeName "proc") [NodeName "missing"] mkPassthroughProc
          $ emptyTopology
    case validateTopology t of
      Right _ -> error "expected validation failure"
      Left  e ->
        assertBool ("got " <> show e) $
          case e of
            Topo.UnknownParent _ _ -> True
            _                       -> False

no_sources_caught_by_validation :: TestTree
no_sources_caught_by_validation =
  testCase "topology without sources is rejected" $ do
    let t = emptyTopology
    case validateTopology t of
      Right _ -> error "expected NoSources"
      Left Topo.NoSources -> pure ()
      Left other          -> error ("expected NoSources, got " <> show other)

empty_source_topics_caught :: TestTree
empty_source_topics_caught =
  testCase "source with empty topic list is rejected" $ do
    let t = addSource (NodeName "src") []
              textSerde textSerde recordTimestampExtractor
          $ emptyTopology
    case validateTopology t of
      Right _ -> error "expected EmptySourceTopics"
      Left (Topo.EmptySourceTopics _) -> pure ()
      Left other -> error ("expected EmptySourceTopics, got " <> show other)

state_store_added_to_processor :: TestTree
state_store_added_to_processor =
  testCase "state stores attach to declared owners" $ do
    let sb = inMemoryKeyValueStoreBuilder @Int @Int (storeName "kv")
        t  = addSource (NodeName "src") [topicName "in"]
              textSerde textSerde recordTimestampExtractor
           $ addProcessor (NodeName "proc") [NodeName "src"] mkPassthroughProc
           $ addStateStoreKV sb [NodeName "proc"]
           $ emptyTopology
    Map.size (topoStores t) @?= 1

topology_round_trip_in_order :: TestTree
topology_round_trip_in_order =
  testCase "addSource/addProcessor/addSink record insertion order" $ do
    let t = addSink (NodeName "c") (topicName "out")
              textSerde textSerde [NodeName "b"]
          $ addProcessor (NodeName "b") [NodeName "a"] mkPassthroughProc
          $ addSource (NodeName "a") [topicName "in"]
              textSerde textSerde recordTimestampExtractor
          $ emptyTopology
    Topo.topoOrder t @?= [NodeName "a", NodeName "b", NodeName "c"]
