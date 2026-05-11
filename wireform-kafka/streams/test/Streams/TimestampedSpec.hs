{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.TimestampedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.KeyValue.Timestamped
  ( ValueAndTimestamp (..)
  , getT
  , latestTimestamp
  , putT
  , timestampedFromKV
  )
import Kafka.Streams.State.Store (KeyValueStore (..), storeName)
import Kafka.Streams.Time (Timestamp (..))

tests :: TestTree
tests = testGroup "TimestampedKeyValueStore"
  [ tskv_put_get_round_trip
  , tskv_latest_timestamp
  , tskv_overwrites_keep_latest
  ]

tskv_put_get_round_trip :: TestTree
tskv_put_get_round_trip =
  testCase "putT then getT round-trips both value and timestamp" $ do
    base <- inMemoryKeyValueStore @String @(ValueAndTimestamp Int)
              (storeName "ts")
    let ts = timestampedFromKV base
    putT ts "k" 42 (Timestamp 1234)
    getT ts "k" >>= (@?= Just (42, Timestamp 1234))

tskv_latest_timestamp :: TestTree
tskv_latest_timestamp =
  testCase "latestTimestamp returns Nothing for missing key" $ do
    base <- inMemoryKeyValueStore @String @(ValueAndTimestamp Int)
              (storeName "ts")
    let ts = timestampedFromKV base
    latestTimestamp ts "missing" >>= (@?= Nothing)
    putT ts "k" 1 (Timestamp 100)
    latestTimestamp ts "k" >>= (@?= Just (Timestamp 100))

tskv_overwrites_keep_latest :: TestTree
tskv_overwrites_keep_latest =
  testCase "putT overwrites both value and timestamp" $ do
    base <- inMemoryKeyValueStore @String @(ValueAndTimestamp Int)
              (storeName "ts")
    let ts = timestampedFromKV base
    putT ts "k" 1 (Timestamp 100)
    putT ts "k" 2 (Timestamp 200)
    getT ts "k" >>= (@?= Just (2, Timestamp 200))
