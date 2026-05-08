{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.JoinSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams
import Kafka.Streams.DSL.Joined (symmetricJoinWindows)

tests :: TestTree
tests = testGroup "Joins"
  [ kstream_ktable_inner
  , kstream_ktable_left
  , kstream_ktable_table_updates_propagate
    -- Stream-stream window joins
  , kstream_kstream_inner_within_window
  , kstream_kstream_inner_outside_window
  , kstream_kstream_left_unmatched_emits_nothing
  , kstream_kstream_outer_emits_both_sides
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

----------------------------------------------------------------------
-- KStream-KStream window join tests
----------------------------------------------------------------------

-- A stream-stream join with both sides under the same key, where the
-- window covers the gap.
kstream_kstream_inner_within_window :: TestTree
kstream_kstream_inner_within_window =
  testCase "KStream-KStream inner join: matches within window" $ do
    b <- newStreamsBuilder
    sl <- streamFromTopic b (topicName "left")
            (consumed textSerde textSerde)
    sr <- streamFromTopic b (topicName "right")
            (consumed textSerde textSerde)
    j <- joinKStreamKStream
            (\l r -> l <> "+" <> r)
            (symmetricJoinWindows (millis 100))
            (Kafka.Streams.joined textSerde textSerde textSerde)
            sl
            sr
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "kskj-app"

    pipeInput driver (topicName "left")  (Just (bytes "k")) (bytes "L1") (t 100) 0
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 150) 0
    -- left at 200 should still match R1 (within +/-100 of 150, and 100 too)
    pipeInput driver (topicName "left")  (Just (bytes "k")) (bytes "L2") (t 200) 0

    out <- readOutput driver (topicName "out")
    -- Order: L1 buffers left-side; R1 arrives, scans left, finds L1
    -- → "L1+R1"; L2 arrives, scans right, finds R1 → "L2+R1".
    map (unbytes . crValue) out @?= ["L1+R1", "L2+R1"]
    closeDriver driver

-- Records outside the window must NOT match.
kstream_kstream_inner_outside_window :: TestTree
kstream_kstream_inner_outside_window =
  testCase "KStream-KStream inner join: drops matches outside window" $ do
    b <- newStreamsBuilder
    sl <- streamFromTopic b (topicName "left")
            (consumed textSerde textSerde)
    sr <- streamFromTopic b (topicName "right")
            (consumed textSerde textSerde)
    j <- joinKStreamKStream
            (\l r -> l <> "+" <> r)
            (symmetricJoinWindows (millis 50))
            (Kafka.Streams.joined textSerde textSerde textSerde)
            sl
            sr
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "kskj-app"

    pipeInput driver (topicName "left")  (Just (bytes "k")) (bytes "L1") (t 0)   0
    -- Right at 200 is way outside the 50ms window of L1.
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 200) 0

    out <- readOutput driver (topicName "out")
    length out @?= 0
    closeDriver driver

kstream_kstream_left_unmatched_emits_nothing :: TestTree
kstream_kstream_left_unmatched_emits_nothing =
  testCase "KStream-KStream left join: unmatched left records emit Nothing" $ do
    b <- newStreamsBuilder
    sl <- streamFromTopic b (topicName "left")
            (consumed textSerde textSerde)
    sr <- streamFromTopic b (topicName "right")
            (consumed textSerde textSerde)
    j <- leftJoinKStreamKStream
            (\l mr -> case mr of
                        Just r -> l <> "+" <> r
                        Nothing -> l <> "+<none>")
            (symmetricJoinWindows (millis 50))
            (Kafka.Streams.joined textSerde textSerde textSerde)
            sl
            sr
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "kskj-app"

    -- L1 has no right match yet → emits "L1+<none>".
    pipeInput driver (topicName "left")  (Just (bytes "k")) (bytes "L1") (t 0)   0
    -- R1 within window → finds L1 in the left store, emits "L1+R1".
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 20)  0
    -- L2 within window of R1 → emits "L2+R1".
    pipeInput driver (topicName "left")  (Just (bytes "k")) (bytes "L2") (t 30)  0
    -- L3 outside window of R1 → emits "L3+<none>".
    pipeInput driver (topicName "left")  (Just (bytes "k")) (bytes "L3") (t 200) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?=
      ["L1+<none>", "L1+R1", "L2+R1", "L3+<none>"]
    closeDriver driver

kstream_kstream_outer_emits_both_sides :: TestTree
kstream_kstream_outer_emits_both_sides =
  testCase "KStream-KStream outer join: unmatched on either side emits Nothing" $ do
    b <- newStreamsBuilder
    sl <- streamFromTopic b (topicName "left")
            (consumed textSerde textSerde)
    sr <- streamFromTopic b (topicName "right")
            (consumed textSerde textSerde)
    j <- outerJoinKStreamKStream
            (\ml mr ->
                let l = maybe "<>" id ml
                    r = maybe "<>" id mr
                 in l <> "/" <> r)
            (symmetricJoinWindows (millis 50))
            (Kafka.Streams.joined textSerde textSerde textSerde)
            sl
            sr
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "ksko-app"

    pipeInput driver (topicName "left")  (Just (bytes "k")) (bytes "L1") (t 0)   0
    -- Right at t=200: outside L1's 50ms window AND L1 already buffered.
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 200) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["L1/<>", "<>/R1"]
    closeDriver driver
