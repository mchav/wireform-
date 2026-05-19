{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.SuppressSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Imperative

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

tests :: TestTree
tests = testGroup "Suppress"
  [ suppress_time_limit_debounces
  , suppress_time_limit_emits_after_limit
  , suppress_time_limit_first_seen_is_sticky
  ]

suppress_time_limit_debounces :: TestTree
suppress_time_limit_debounces =
  testCase "suppressUntilTimeLimit holds updates within the window" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    suppressed <- suppressUntilTimeLimit (millis 100) src
    toTopic (topicName "out") (produced textSerde textSerde) suppressed
    topo <- buildTopology b
    driver <- newDriver topo "supp-app"

    -- Three updates inside 100ms: only the latest survives, but
    -- nothing is emitted yet (we're still inside the limit window).
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (t 0)  0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (t 50) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v3") (t 99) 0

    out0 <- readOutput driver (topicName "out")
    -- Within the 100ms window from t=0, no emission.
    map (T.pack . BSC.unpack . crValue) out0 @?= []
    closeDriver driver

suppress_time_limit_emits_after_limit :: TestTree
suppress_time_limit_emits_after_limit =
  testCase "suppressUntilTimeLimit emits once stream-time crosses the limit" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    suppressed <- suppressUntilTimeLimit (millis 100) src
    toTopic (topicName "out") (produced textSerde textSerde) suppressed
    topo <- buildTopology b
    driver <- newDriver topo "supp-app"

    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "first")  (t 0)   0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "second") (t 50)  0
    -- This record is at t=200, well past the t=100 limit; the
    -- buffered "second" should flush.
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "third")  (t 200) 0

    out <- readOutput driver (topicName "out")
    map (T.pack . BSC.unpack . crValue) out @?= ["second"]
    closeDriver driver

suppress_time_limit_first_seen_is_sticky :: TestTree
suppress_time_limit_first_seen_is_sticky =
  testCase "first-seen timestamp anchors the per-key debounce window" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    suppressed <- suppressUntilTimeLimit (millis 200) src
    toTopic (topicName "out") (produced textSerde textSerde) suppressed
    topo <- buildTopology b
    driver <- newDriver topo "supp-app"

    -- Key A's first record is at t=0; B's first is at t=50. The
    -- limit is 200ms, so A flushes at t>=200 and B flushes at
    -- t>=250.
    pipeInput driver (topicName "in") (Just (bytes "A")) (bytes "a1") (t 0)   0
    pipeInput driver (topicName "in") (Just (bytes "B")) (bytes "b1") (t 50)  0
    pipeInput driver (topicName "in") (Just (bytes "A")) (bytes "a2") (t 100) 0
    pipeInput driver (topicName "in") (Just (bytes "B")) (bytes "b2") (t 150) 0
    -- Trigger evaluation at t=210: A should flush "a2", B not yet.
    pipeInput driver (topicName "in") (Just (bytes "A")) (bytes "a3") (t 210) 0
    out1 <- readOutput driver (topicName "out")
    map (T.pack . BSC.unpack . crValue) out1 @?= ["a2"]

    -- Trigger at t=300: B flushes "b2", A's new debounce holds "a3".
    pipeInput driver (topicName "in") (Just (bytes "B")) (bytes "b3") (t 300) 0
    out2 <- readOutput driver (topicName "out")
    map (T.pack . BSC.unpack . crValue) out2 @?= ["b2"]
    closeDriver driver
