{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Examples.Ops.Backfill
-- Description : Demo of state backfill (changelog / snapshot / CDC)
--
-- Shows the @Kafka.Streams.Backfill@ utilities bootstrapping a state
-- store three ways, without reprocessing topology input:
--
--   1. from a changelog log;
--   2. from a snapshot blob plus the changelog tail;
--   3. from a change-data-capture source.
module Kafka.Streams.Examples.Ops.Backfill
  ( runDemo
  ) where

import Data.Int (Int64)
import Data.Text (Text)

import Kafka.Streams (int64Serde, storeName, textSerde)
import Kafka.Streams.Serde (serialize)
import Kafka.Streams.State.Store
  ( KeyValueStore
  , kvIteratorToList
  , kvsAll
  , kvsPut
  )
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.Runtime.ObjectStore (inMemoryObjectStore)
import Kafka.Streams.Runtime.Standby
  ( newInMemoryChangelogTopic
  , publishEntry
  )
import Kafka.Streams.State.KeyValue.Snapshot (SnapshotId (..), snapshotStore)
import Kafka.Streams.Sources.CDC
  ( CDCEvent (..)
  , CDCOp (..)
  , inMemoryCDCSource
  , pushCDC
  )
import Kafka.Streams.Time (Timestamp (..))

import Kafka.Streams.Examples.Ops.Helpers (bullet, section)
import Kafka.Streams.Backfill

newStore :: IO (KeyValueStore Text Int64)
newStore = inMemoryKeyValueStore (storeName "s")

dump :: KeyValueStore Text Int64 -> IO [(Text, Int64)]
dump store = kvsAll store >>= kvIteratorToList

runDemo :: IO ()
runDemo = do
  section "BackfillDemo"
  let sn = storeName "s"

  -- (1) Changelog rebuild -------------------------------------------
  topic <- newInMemoryChangelogTopic
  _ <- publishEntry topic sn (Just (serialize textSerde "users"))   (Just (serialize int64Serde 3))
  _ <- publishEntry topic sn (Just (serialize textSerde "orders"))  (Just (serialize int64Serde 7))
  _ <- publishEntry topic sn (Just (serialize textSerde "users"))   (Just (serialize int64Serde 4))
  store1 <- newStore
  res1 <- backfillFromChangelog store1 sn textSerde int64Serde topic 0
  bullet ("Changelog rebuild: applied " <> show (backfillApplied res1)
            <> " entries over offsets [" <> show (backfillFromOffset res1)
            <> "," <> show (backfillToOffset res1) <> ")")
  entries1 <- dump store1
  mapM_ (\(k, v) -> bullet ("    " <> show k <> " = " <> show v)) entries1

  -- (2) Snapshot + changelog tail -----------------------------------
  os <- inMemoryObjectStore "obj"
  src <- newStore
  kvsPut src "users" 100
  kvsPut src "orders" 200
  _ <- snapshotStore os sn (SnapshotId 1) 1
         (serialize textSerde) (serialize int64Serde) src
  tail' <- newInMemoryChangelogTopic
  _ <- publishEntry tail' sn (Just (serialize textSerde "users"))  (Just (serialize int64Serde 999))  -- offset 0, skipped
  _ <- publishEntry tail' sn (Just (serialize textSerde "carts"))  (Just (serialize int64Serde 5))     -- offset 1, applied
  dest <- newStore
  res2 <- backfillFromSnapshot os sn dest textSerde int64Serde tail'
  case res2 of
    Left err -> bullet ("Snapshot backfill failed: " <> show err)
    Right br -> do
      bullet ("Snapshot + tail: restored snapshot, replayed tail from offset "
                <> show (backfillFromOffset br))
      entries2 <- dump dest
      mapM_ (\(k, v) -> bullet ("    " <> show k <> " = " <> show v)) entries2

  -- (3) CDC drain ---------------------------------------------------
  (cdcSrc, h) <- inMemoryCDCSource "orders-cdc"
  store3 <- newStore
  pushCDC h (CDCEvent CDCInsert "a" Nothing (Just 1) 0 (Timestamp 0))
  pushCDC h (CDCEvent CDCInsert "b" Nothing (Just 2) 1 (Timestamp 0))
  pushCDC h (CDCEvent CDCUpdate "a" (Just 1) (Just 9) 2 (Timestamp 0))
  pushCDC h (CDCEvent CDCDelete "b" (Just 2) Nothing 3 (Timestamp 0))
  res3 <- cdcBackfill cdcSrc store3
  bullet ("CDC drain: applied " <> show (cdcApplied res3)
            <> " compacted events in " <> show (cdcSteps res3)
            <> " step(s), final phase " <> show (cdcFinalPhase res3))
  entries3 <- dump store3
  mapM_ (\(k, v) -> bullet ("    " <> show k <> " = " <> show v)) entries3
