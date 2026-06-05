{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.TimestampedSpec (tests) where

import Test.Syd

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

tests :: Spec
tests = describe "TimestampedKeyValueStore" $ sequence_
  [ tskv_put_get_round_trip
  , tskv_latest_timestamp
  , tskv_overwrites_keep_latest
  ]

tskv_put_get_round_trip :: Spec
tskv_put_get_round_trip =
  it "putT then getT round-trips both value and timestamp" $ do
    base <- inMemoryKeyValueStore @String @(ValueAndTimestamp Int)
              (storeName "ts")
    let ts = timestampedFromKV base
    putT ts "k" 42 (Timestamp 1234)
    getT ts "k" >>= (`shouldBe` Just (42, Timestamp 1234))

tskv_latest_timestamp :: Spec
tskv_latest_timestamp =
  it "latestTimestamp returns Nothing for missing key" $ do
    base <- inMemoryKeyValueStore @String @(ValueAndTimestamp Int)
              (storeName "ts")
    let ts = timestampedFromKV base
    latestTimestamp ts "missing" >>= (`shouldBe` Nothing)
    putT ts "k" 1 (Timestamp 100)
    latestTimestamp ts "k" >>= (`shouldBe` Just (Timestamp 100))

tskv_overwrites_keep_latest :: Spec
tskv_overwrites_keep_latest =
  it "putT overwrites both value and timestamp" $ do
    base <- inMemoryKeyValueStore @String @(ValueAndTimestamp Int)
              (storeName "ts")
    let ts = timestampedFromKV base
    putT ts "k" 1 (Timestamp 100)
    putT ts "k" 2 (Timestamp 200)
    getT ts "k" >>= (`shouldBe` Just (2, Timestamp 200))
