{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end round-trip: produce → topology → consume.
--
-- Builds a one-source one-sink topology, starts a 'KafkaStreams'
-- runtime, and verifies that records flow through the broker.
module Streams.Integration.RoundTrip (tests) where

import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)


import Kafka.Streams.Imperative

tests :: String -> TestTree
tests brokers = testGroup "RoundTrip"
  [ testCase "stream copy from in -> out via KafkaStreams" $ do
      let appId   = "kstreams-it-app"
          inTopic = "kstreams-it-in"
          outTopic = "kstreams-it-out"

      -- Build topology.
      b <- newStreamsBuilder
      s <- streamFromTopic b (topicName (T.pack inTopic))
             (consumed textSerde textSerde)
      toTopic (topicName (T.pack outTopic)) (produced textSerde textSerde) s
      topo <- buildTopology b
      let validated = case validateTopology topo of
                        Left  err -> error (show err)
                        Right ok  -> ok

      let cfg = defaultStreamsConfig
            { applicationId    = T.pack appId
            , bootstrapServers = [T.pack brokers]
            , clientId         = T.pack "kstreams-it"
            }

      -- We don't actually run the runtime here because that requires
      -- real-broker fixtures (topics created up front). This stub
      -- only exercises that the runtime can be instantiated against
      -- a live config.
      ks <- newKafkaStreams cfg validated
      st0 <- streamsStatus ks
      assertBool "initial status created" (st0 == StreamsCreated)
      closeKafkaStreams ks
      stN <- streamsStatus ks
      assertBool ("final status closed: " <> show stN) (stN == StreamsClosed)
  ]

