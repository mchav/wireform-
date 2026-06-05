{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.VersionedSpec (tests) where

import Test.Syd

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

tests :: Spec
tests = describe "VersionedKeyValueStore" $ sequence_
  [ versioned_keeps_history
  , versioned_get_latest_returns_most_recent
  , versioned_get_as_of_floor
  , versioned_history_band
  , versioned_retention_drops_old_versions
  ]

versioned_keeps_history :: Spec
versioned_keeps_history =
  it "putV keeps each (key, ts) pair" $ do
    s <- inMemoryVersionedKeyValueStore @String @Int
           (storeName "v") defaultVersionedConfig
    putV s "k" 1 (Timestamp 100)
    putV s "k" 2 (Timestamp 200)
    putV s "k" 3 (Timestamp 300)
    hs <- getHistory s "k" (Timestamp 0) (Timestamp 1000)
    map vrValue hs `shouldBe` [1, 2, 3]
    map vrValidFromTs hs `shouldBe` [Timestamp 100, Timestamp 200, Timestamp 300]

versioned_get_latest_returns_most_recent :: Spec
versioned_get_latest_returns_most_recent =
  it "getLatest returns the highest-timestamped version" $ do
    s <- inMemoryVersionedKeyValueStore @String @Int
           (storeName "v") defaultVersionedConfig
    putV s "k" 1 (Timestamp 100)
    putV s "k" 2 (Timestamp 200)
    putV s "k" 999 (Timestamp 50)   -- older than current latest
    Just (VersionedRecord v ts) <- getLatest s "k"
    v `shouldBe` 2
    ts `shouldBe` Timestamp 200

versioned_get_as_of_floor :: Spec
versioned_get_as_of_floor =
  it "getAsOf returns the largest version with ts <= asof" $ do
    s <- inMemoryVersionedKeyValueStore @String @Int
           (storeName "v") defaultVersionedConfig
    putV s "k" 1 (Timestamp 100)
    putV s "k" 2 (Timestamp 200)
    putV s "k" 3 (Timestamp 300)

    Just (VersionedRecord v1 _) <- getAsOf s "k" (Timestamp 150)
    v1 `shouldBe` 1

    Just (VersionedRecord v2 _) <- getAsOf s "k" (Timestamp 250)
    v2 `shouldBe` 2

    Just (VersionedRecord v3 _) <- getAsOf s "k" (Timestamp 999)
    v3 `shouldBe` 3

    -- Before any version → Nothing.
    getAsOf s "k" (Timestamp 50) >>= (`shouldBe` Nothing)

versioned_history_band :: Spec
versioned_history_band =
  it "getHistory returns the inclusive [from, to] slice" $ do
    s <- inMemoryVersionedKeyValueStore @String @Int
           (storeName "v") defaultVersionedConfig
    mapM_ (\n -> putV s "k" n (Timestamp (fromIntegral n * 100))) [1..5]
    hs <- getHistory s "k" (Timestamp 200) (Timestamp 400)
    map vrValue hs `shouldBe` [2, 3, 4]

versioned_retention_drops_old_versions :: Spec
versioned_retention_drops_old_versions =
  it "putV beyond retention drops old versions" $ do
    -- 100ms retention.
    let cfg = VersionedConfig { historyRetention = 100 }
    s <- inMemoryVersionedKeyValueStore @String @Int (storeName "v") cfg
    putV s "k" 1 (Timestamp 0)
    putV s "k" 2 (Timestamp 50)
    -- This put bumps observed time to 500; cutoff = 500 - 100 = 400.
    -- So versions at 0 and 50 should be pruned.
    putV s "k" 3 (Timestamp 500)
    hs <- getHistory s "k" (Timestamp 0) (Timestamp 1000)
    map vrValue hs `shouldBe` [3]
