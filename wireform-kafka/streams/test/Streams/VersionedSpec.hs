{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.VersionedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.State.KeyValue.Versioned
  ( VersionedConfig (..)
  , VersionedRecord (..)
  , defaultVersionedConfig
  , getAsOf
  , getHistory
  , getLatest
  , inMemoryVersionedKeyValueStore
  , putV
  )
import Kafka.Streams.State.Store (storeName)
import Kafka.Streams.Time (Timestamp (..))

tests :: TestTree
tests = testGroup "VersionedKeyValueStore"
  [ versioned_keeps_history
  , versioned_get_latest_returns_most_recent
  , versioned_get_as_of_floor
  , versioned_history_band
  , versioned_retention_drops_old_versions
  ]

versioned_keeps_history :: TestTree
versioned_keeps_history =
  testCase "putV keeps each (key, ts) pair" $ do
    s <- inMemoryVersionedKeyValueStore @String @Int
           (storeName "v") defaultVersionedConfig
    putV s "k" 1 (Timestamp 100)
    putV s "k" 2 (Timestamp 200)
    putV s "k" 3 (Timestamp 300)
    hs <- getHistory s "k" (Timestamp 0) (Timestamp 1000)
    map vrValue hs @?= [1, 2, 3]
    map vrValidFromTs hs @?= [Timestamp 100, Timestamp 200, Timestamp 300]

versioned_get_latest_returns_most_recent :: TestTree
versioned_get_latest_returns_most_recent =
  testCase "getLatest returns the highest-timestamped version" $ do
    s <- inMemoryVersionedKeyValueStore @String @Int
           (storeName "v") defaultVersionedConfig
    putV s "k" 1 (Timestamp 100)
    putV s "k" 2 (Timestamp 200)
    putV s "k" 999 (Timestamp 50)   -- older than current latest
    Just (VersionedRecord v ts) <- getLatest s "k"
    v @?= 2
    ts @?= Timestamp 200

versioned_get_as_of_floor :: TestTree
versioned_get_as_of_floor =
  testCase "getAsOf returns the largest version with ts <= asof" $ do
    s <- inMemoryVersionedKeyValueStore @String @Int
           (storeName "v") defaultVersionedConfig
    putV s "k" 1 (Timestamp 100)
    putV s "k" 2 (Timestamp 200)
    putV s "k" 3 (Timestamp 300)

    Just (VersionedRecord v1 _) <- getAsOf s "k" (Timestamp 150)
    v1 @?= 1

    Just (VersionedRecord v2 _) <- getAsOf s "k" (Timestamp 250)
    v2 @?= 2

    Just (VersionedRecord v3 _) <- getAsOf s "k" (Timestamp 999)
    v3 @?= 3

    -- Before any version → Nothing.
    getAsOf s "k" (Timestamp 50) >>= (@?= Nothing)

versioned_history_band :: TestTree
versioned_history_band =
  testCase "getHistory returns the inclusive [from, to] slice" $ do
    s <- inMemoryVersionedKeyValueStore @String @Int
           (storeName "v") defaultVersionedConfig
    mapM_ (\n -> putV s "k" n (Timestamp (fromIntegral n * 100))) [1..5]
    hs <- getHistory s "k" (Timestamp 200) (Timestamp 400)
    map vrValue hs @?= [2, 3, 4]

versioned_retention_drops_old_versions :: TestTree
versioned_retention_drops_old_versions =
  testCase "putV beyond retention drops old versions" $ do
    -- 100ms retention.
    let cfg = VersionedConfig { historyRetention = 100 }
    s <- inMemoryVersionedKeyValueStore @String @Int (storeName "v") cfg
    putV s "k" 1 (Timestamp 0)
    putV s "k" 2 (Timestamp 50)
    -- This put bumps observed time to 500; cutoff = 500 - 100 = 400.
    -- So versions at 0 and 50 should be pruned.
    putV s "k" 3 (Timestamp 500)
    hs <- getHistory s "k" (Timestamp 0) (Timestamp 1000)
    map vrValue hs @?= [3]
