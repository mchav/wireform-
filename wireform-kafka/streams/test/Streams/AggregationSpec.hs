{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.AggregationSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams

tests :: TestTree
tests = testGroup "Aggregation"
  [ count_per_key
  , reduce_per_key
  , aggregate_with_init
  , windowed_count
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

i64Bytes :: Int64 -> BSC.ByteString
i64Bytes = serialize int64Serde

t :: Int64 -> Timestamp
t = Timestamp

count_per_key :: TestTree
count_per_key = testCase "count per key writes monotone counts" $ do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = grouped textSerde textSerde
  let kgs = groupByKey g src
  table <- countStream materialized kgs
  -- Drive until the test driver ends.
  topo <- buildTopology b
  driver <- newDriver topo "agg-app"

  pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "1") (t 0) 0
  pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "2") (t 1) 0
  pipeInput driver (topicName "in") (Just (bytes "b")) (bytes "1") (t 2) 0
  pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "3") (t 3) 0

  mStore <- getKeyValueStore @Text @Int64 driver (ctlStore table)
  case mStore of
    Nothing  -> error "store missing"
    Just kvs -> do
      ca <- kvsGet kvs "a"
      cb <- kvsGet kvs "b"
      ca @?= Just 3
      cb @?= Just 1
  closeDriver driver

reduce_per_key :: TestTree
reduce_per_key = testCase "reduce sums values per key" $ do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "in")
           (consumed textSerde int64Serde)
  let g = grouped textSerde int64Serde
      kgs = groupByKey g src
  table <- reduceStream (+) materialized kgs
  topo <- buildTopology b
  driver <- newDriver topo "reduce-app"

  pipeInput driver (topicName "in") (Just (bytes "a")) (i64Bytes 5)  (t 0) 0
  pipeInput driver (topicName "in") (Just (bytes "a")) (i64Bytes 10) (t 1) 0
  pipeInput driver (topicName "in") (Just (bytes "b")) (i64Bytes 1)  (t 2) 0
  pipeInput driver (topicName "in") (Just (bytes "a")) (i64Bytes 2)  (t 3) 0
  pipeInput driver (topicName "in") (Just (bytes "b")) (i64Bytes 100) (t 4) 0

  mStore <- getKeyValueStore @Text @Int64 driver (ctlStore table)
  case mStore of
    Nothing  -> error "store missing"
    Just kvs -> do
      kvsGet kvs "a" >>= (@?= Just 17)
      kvsGet kvs "b" >>= (@?= Just 101)
  closeDriver driver

aggregate_with_init :: TestTree
aggregate_with_init = testCase "aggregate with non-trivial init" $ do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "in")
           (consumed textSerde int64Serde)
  let g = grouped textSerde int64Serde
      kgs = groupByKey g src
  table <- aggregateStream
             (pure (1 :: Int64))   -- start at 1, multiply by each value
             (\_ v acc -> acc * v)
             materialized
             kgs
  topo <- buildTopology b
  driver <- newDriver topo "agg-init-app"

  pipeInput driver (topicName "in") (Just (bytes "x")) (i64Bytes 2) (t 0) 0
  pipeInput driver (topicName "in") (Just (bytes "x")) (i64Bytes 3) (t 1) 0
  pipeInput driver (topicName "in") (Just (bytes "x")) (i64Bytes 5) (t 2) 0

  mStore <- getKeyValueStore @Text @Int64 driver (ctlStore table)
  case mStore of
    Nothing  -> error "store missing"
    Just kvs -> kvsGet kvs "x" >>= (@?= Just 30)
  closeDriver driver

windowed_count :: TestTree
windowed_count = testCase "tumbling windowed count" $ do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = grouped textSerde textSerde
      kgs = groupByKey g src
      twks = windowedByTime (tumblingWindows (millis 100)) kgs
  table <- countWindowed materialized twks
  topo <- buildTopology b
  driver <- newDriver topo "wcount-app"

  -- 3 records in window [0, 100)
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (t 10) 0
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (t 50) 0
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v3") (t 99) 0
  -- 2 records in window [100, 200)
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v4") (t 150) 0
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v5") (t 199) 0

  mStore <- getWindowStore @Text @Int64 driver (wthStore table)
  case mStore of
    Nothing  -> error "window store missing"
    Just ws  -> do
      wsFetch ws "k" (Timestamp 0)   >>= (@?= Just 3)
      wsFetch ws "k" (Timestamp 100) >>= (@?= Just 2)
  closeDriver driver
