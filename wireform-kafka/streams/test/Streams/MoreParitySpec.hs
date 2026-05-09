{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the latest parity batch:
-- reverse iterators (KIP-617), pause / resume (KIP-834),
-- Topology.connectProcessorAndStateStores.
module Streams.MoreParitySpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.Store (kvIteratorToList)

tests :: TestTree
tests = testGroup "MoreParity"
  [ reverse_all_descending
  , reverse_range_descending
  , pause_then_resume_state
  ]

reverse_all_descending :: TestTree
reverse_all_descending =
  testCase "kvsReverseAll yields entries in descending key order" $ do
    s <- inMemoryKeyValueStore @Int @Int (storeName "rev")
    mapM_ (\n -> kvsPut s n (n * 10)) [1, 2, 3, 4, 5]
    it <- kvsReverseAll s
    xs <- kvIteratorToList it
    map fst xs @?= [5, 4, 3, 2, 1]

reverse_range_descending :: TestTree
reverse_range_descending =
  testCase "kvsReverseRange yields the inclusive [lo, hi] slice in descending order" $ do
    s <- inMemoryKeyValueStore @Int @Int (storeName "rev")
    mapM_ (\n -> kvsPut s n (n * 10)) [1, 2, 3, 4, 5]
    it <- kvsReverseRange s 2 4
    xs <- kvIteratorToList it
    map fst xs @?= [4, 3, 2]

pause_then_resume_state :: TestTree
pause_then_resume_state =
  testCase "pauseKafkaStreams flips isPausedKafkaStreams to True; resume flips it back" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    toTopic (topicName "out") (produced textSerde textSerde) s
    topo <- buildTopology b
    case validateTopology topo of
      Left err -> error (show err)
      Right v  -> do
        ks <- newKafkaStreams defaultStreamsConfig
                { applicationId    = "pr-app"
                , bootstrapServers = ["mock:0"]
                } v
        isPausedKafkaStreams ks >>= (@?= False)
        pauseKafkaStreams ks
        isPausedKafkaStreams ks >>= (@?= True)
        resumeKafkaStreams ks
        isPausedKafkaStreams ks >>= (@?= False)
        closeKafkaStreams ks
