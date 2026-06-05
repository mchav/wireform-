{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.BackfillSpec
-- Description : Tests for state backfill (changelog / snapshot / CDC)
module Streams.BackfillSpec (tests) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)

import Test.Syd

import Kafka.Streams (int64Serde, storeName, textSerde)
import Kafka.Streams.Serde (serialize)
import Kafka.Streams.State.Store (KeyValueStore (..), StoreName)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.Runtime.ObjectStore (inMemoryObjectStore)
import Kafka.Streams.Runtime.Standby
  ( newInMemoryChangelogTopic
  , publishEntry
  )
import Kafka.Streams.State.KeyValue.Snapshot
  ( SnapshotId (..)
  , snapshotStore
  )
import Kafka.Streams.Sources.CDC
  ( CDCEvent (..)
  , CDCOp (..)
  , inMemoryCDCSource
  , pushCDC
  )
import Kafka.Streams.Time (Timestamp (..))

import Kafka.Streams.Backfill

newStore :: IO (KeyValueStore Text Int64)
newStore = inMemoryKeyValueStore (storeName "s")

storeNm :: StoreName
storeNm = storeName "s"

kb :: Text -> Maybe ByteString
kb = Just . serialize textSerde

vb :: Int64 -> Maybe ByteString
vb = Just . serialize int64Serde

tests :: Spec
tests = describe "Backfill" $ sequence_
  [ changelog_rebuilds_store
  , changelog_from_offset
  , snapshot_then_tail
  , cdc_drains_into_store
  ]

changelog_rebuilds_store :: Spec
changelog_rebuilds_store =
  it "backfillFromChangelog applies puts and tombstones" $ do
    topic <- newInMemoryChangelogTopic
    _ <- publishEntry topic storeNm (kb "k1") (vb 10)
    _ <- publishEntry topic storeNm (kb "k2") (vb 20)
    _ <- publishEntry topic storeNm (kb "k1") Nothing       -- tombstone
    store <- newStore
    res <- backfillFromChangelog store storeNm textSerde int64Serde topic 0
    backfillApplied res    `shouldBe` 3
    backfillFromOffset res `shouldBe` 0
    backfillToOffset res   `shouldBe` 3
    v1 <- kvsGet store "k1"
    v2 <- kvsGet store "k2"
    v1 `shouldBe` Nothing
    v2 `shouldBe` Just 20

changelog_from_offset :: Spec
changelog_from_offset =
  it "backfillFromChangelog starts at the given offset" $ do
    topic <- newInMemoryChangelogTopic
    _ <- publishEntry topic storeNm (kb "a") (vb 1)   -- offset 0
    _ <- publishEntry topic storeNm (kb "b") (vb 2)   -- offset 1
    store <- newStore
    res <- backfillFromChangelog store storeNm textSerde int64Serde topic 1
    backfillApplied res `shouldBe` 1
    va <- kvsGet store "a"
    vb' <- kvsGet store "b"
    va  `shouldBe` Nothing       -- offset 0 skipped
    vb' `shouldBe` Just 2

snapshot_then_tail :: Spec
snapshot_then_tail =
  it "backfillFromSnapshot restores blob then replays the tail" $ do
    os <- inMemoryObjectStore "obj"
    -- Build & snapshot a source store at advancedTo = 1.
    src <- newStore
    kvsPut src "a" 1
    kvsPut src "b" 2
    snapR <- snapshotStore os storeNm (SnapshotId 1) 1
               (serialize textSerde) (serialize int64Serde) src
    snapR `shouldBe` Right ()
    -- Changelog: offset 0 should be ignored (before advancedTo),
    -- offset 1 replayed.
    topic <- newInMemoryChangelogTopic
    _ <- publishEntry topic storeNm (kb "a") (vb 99)  -- offset 0 (skipped)
    _ <- publishEntry topic storeNm (kb "c") (vb 3)   -- offset 1 (applied)
    dest <- newStore
    res <- backfillFromSnapshot os storeNm dest textSerde int64Serde topic
    case res of
      Left e   -> expectationFailure ("backfill failed: " <> show e)
      Right br -> do
        backfillFromOffset br `shouldBe` 1
        va <- kvsGet dest "a"
        vb' <- kvsGet dest "b"
        vc <- kvsGet dest "c"
        va  `shouldBe` Just 1     -- from snapshot, NOT 99 (offset 0 skipped)
        vb' `shouldBe` Just 2     -- from snapshot
        vc  `shouldBe` Just 3     -- from changelog tail

cdc_drains_into_store :: Spec
cdc_drains_into_store =
  it "cdcBackfill applies compacted CDC events to the store" $ do
    (src, h) <- inMemoryCDCSource "t"
    store <- newStore
    pushCDC h (CDCEvent CDCInsert "a" Nothing (Just 1) 0 (Timestamp 0))
    pushCDC h (CDCEvent CDCInsert "b" Nothing (Just 2) 1 (Timestamp 0))
    pushCDC h (CDCEvent CDCUpdate "a" (Just 1) (Just 9) 2 (Timestamp 0))
    res <- cdcBackfill src store
    cdcApplied res `shouldBe` 2          -- compacted to last-per-key
    va <- kvsGet store "a"
    vb' <- kvsGet store "b"
    va  `shouldBe` Just 9
    vb' `shouldBe` Just 2
