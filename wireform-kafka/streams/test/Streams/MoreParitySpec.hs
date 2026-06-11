{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for the latest parity batch:
reverse iterators (KIP-617), pause / resume (KIP-834),
Topology.connectProcessorAndStateStores.
-}
module Streams.MoreParitySpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.Store (kvIteratorToList)
import Test.Syd


tests :: Spec
tests =
  describe "MoreParity" $
    sequence_
      [ reverse_all_descending
      , reverse_range_descending
      , pause_then_resume_state
      ]


reverse_all_descending :: Spec
reverse_all_descending =
  it "kvsReverseAll yields entries in descending key order" $ do
    s <- inMemoryKeyValueStore @Int @Int (storeName "rev")
    mapM_ (\n -> kvsPut s n (n * 10)) [1, 2, 3, 4, 5]
    it <- kvsReverseAll s
    xs <- kvIteratorToList it
    map fst xs `shouldBe` [5, 4, 3, 2, 1]


reverse_range_descending :: Spec
reverse_range_descending =
  it "kvsReverseRange yields the inclusive [lo, hi] slice in descending order" $ do
    s <- inMemoryKeyValueStore @Int @Int (storeName "rev")
    mapM_ (\n -> kvsPut s n (n * 10)) [1, 2, 3, 4, 5]
    it <- kvsReverseRange s 2 4
    xs <- kvIteratorToList it
    map fst xs `shouldBe` [4, 3, 2]


pause_then_resume_state :: Spec
pause_then_resume_state =
  it "pauseKafkaStreams flips isPausedKafkaStreams to True; resume flips it back" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    toTopic (topicName "out") (produced textSerde textSerde) s
    topo <- buildTopology b
    case validateTopology topo of
      Left err -> error (show err)
      Right v -> do
        ks <-
          newKafkaStreams
            defaultStreamsConfig
              { applicationId = "pr-app"
              , bootstrapServers = ["mock:0"]
              }
            v
        isPausedKafkaStreams ks >>= (`shouldBe` False)
        pauseKafkaStreams ks
        isPausedKafkaStreams ks >>= (`shouldBe` True)
        resumeKafkaStreams ks
        isPausedKafkaStreams ks >>= (`shouldBe` False)
        closeKafkaStreams ks
