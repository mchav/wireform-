{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.JoinSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams

tests :: TestTree
tests = testGroup "Joins"
  [ kstream_ktable_inner
  , kstream_ktable_left
  , kstream_ktable_table_updates_propagate
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

kstream_ktable_inner :: TestTree
kstream_ktable_inner =
  testCase "KStream-KTable inner join drops unmatched stream records" $ do
    b <- newStreamsBuilder
    -- KTable side
    tab <- tableFromTopic b (topicName "users")
             (consumed textSerde textSerde)
             (materializedAs (storeName "users-store"))
    -- KStream side
    s <- streamFromTopic b (topicName "events")
           (consumed textSerde textSerde)
    joined <- joinKStreamKTable
                 (\ev usr -> usr <> ":" <> ev)
                 (Kafka.Streams.joined textSerde textSerde textSerde)
                 s
                 tab
    toTopic (topicName "out") (produced textSerde textSerde) joined
    topo <- buildTopology b
    driver <- newDriver topo "join-app"

    -- Populate table.
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "users") (Just (bytes "u2")) (bytes "bob")   (t 0) 0
    -- Stream events.
    pipeInput driver (topicName "events") (Just (bytes "u1")) (bytes "click")  (t 1) 0
    pipeInput driver (topicName "events") (Just (bytes "u3")) (bytes "ignore") (t 1) 0
    pipeInput driver (topicName "events") (Just (bytes "u2")) (bytes "scroll") (t 2) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["alice:click", "bob:scroll"]
    closeDriver driver

kstream_ktable_left :: TestTree
kstream_ktable_left =
  testCase "KStream-KTable left join always emits" $ do
    b <- newStreamsBuilder
    tab <- tableFromTopic b (topicName "users")
             (consumed textSerde textSerde)
             (materializedAs (storeName "users-store-l"))
    s <- streamFromTopic b (topicName "events")
           (consumed textSerde textSerde)
    j <- leftJoinKStreamKTable
            (\ev mu -> case mu of
                         Just u  -> u <> ":" <> ev
                         Nothing -> "<unknown>:" <> ev)
            (Kafka.Streams.joined textSerde textSerde textSerde)
            s
            tab
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "ljoin-app"

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "events") (Just (bytes "u1")) (bytes "x") (t 1) 0
    pipeInput driver (topicName "events") (Just (bytes "u2")) (bytes "y") (t 1) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["alice:x", "<unknown>:y"]
    closeDriver driver

kstream_ktable_table_updates_propagate :: TestTree
kstream_ktable_table_updates_propagate =
  testCase "KStream-KTable: table updates change subsequent join results" $ do
    b <- newStreamsBuilder
    tab <- tableFromTopic b (topicName "users")
             (consumed textSerde textSerde)
             (materializedAs (storeName "users-store-u"))
    s <- streamFromTopic b (topicName "events")
           (consumed textSerde textSerde)
    j <- joinKStreamKTable
            (\ev u -> u <> ":" <> ev)
            (Kafka.Streams.joined textSerde textSerde textSerde)
            s
            tab
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "ujoin-app"

    pipeInput driver (topicName "users")  (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "events") (Just (bytes "u1")) (bytes "x")     (t 1) 0
    pipeInput driver (topicName "users")  (Just (bytes "u1")) (bytes "ALICE") (t 2) 0
    pipeInput driver (topicName "events") (Just (bytes "u1")) (bytes "y")     (t 3) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["alice:x", "ALICE:y"]
    closeDriver driver
