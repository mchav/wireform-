{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Backfill
-- Description : Bootstrap state stores without reprocessing input
--
-- Where "Kafka.Streams.Replay" reprocesses /input records/ through a
-- whole topology, this module bootstraps a single state store
-- directly from its persisted history. Three sources:
--
--   * __Changelog__ ('backfillFromChangelog') — replay a store's
--     changelog log into the store. This is what a standby task does
--     to stay warm; exposed here as a one-shot bootstrap.
--   * __Snapshot + changelog tail__ ('backfillFromSnapshot') — restore
--     the latest snapshot blob, then replay only the changelog tail
--     from the snapshot's @advancedTo@ offset. This is the bounded
--     recovery contract (recovery time @O(time-since-snapshot)@, not
--     @O(state-size)@) wrapped in a single call.
--   * __CDC__ ('cdcBackfill') — drain a change-data-capture source's
--     snapshot/streaming events into the store (the
--     materialise-a-KTable-from-a-DB-snapshot path).
--
-- All three are broker-free and operate on in-memory primitives
-- ('Kafka.Streams.Runtime.Standby', 'Kafka.Streams.State.KeyValue.Snapshot',
-- 'Kafka.Streams.Sources.CDC'); a broker-backed runtime swaps the
-- backing transports without changing these signatures.
module Kafka.Streams.Backfill
  ( -- * Result
    BackfillResult (..)

    -- * Changelog
  , backfillFromChangelog

    -- * Snapshot + changelog tail
  , backfillFromSnapshot

    -- * CDC
  , CDCBackfillResult (..)
  , cdcBackfill
  ) where

import Data.Int (Int64)
import Data.IORef (writeIORef)
import Data.Text (Text)

import Kafka.Streams.Runtime.ObjectStore (ObjectStoreClient)
import Kafka.Streams.Runtime.Standby
  ( ChangelogTopic
  , StandbyTask (sbOffset)
  , advanceStandby
  , currentChangelogOffset
  , newStandbyTask
  )
import Kafka.Streams.Serde (Serde, deserialize)
import Kafka.Streams.Sources.CDC
  ( CDCPhase (SnapshotPhase)
  , CDCSource
  , SchemaChange
  , cdcToKTableStep
  )
import Kafka.Streams.State.KeyValue.Snapshot
  ( manifestAdvancedTo
  , restoreFromSnapshot
  )
import Kafka.Streams.State.Store (KeyValueStore, StoreName)

----------------------------------------------------------------------
-- Changelog / snapshot backfill
----------------------------------------------------------------------

-- | Outcome of a store backfill.
data BackfillResult = BackfillResult
  { backfillApplied    :: !Int
    -- ^ Changelog entries applied to the store.
  , backfillFromOffset :: !Int64
    -- ^ Changelog offset replay started from (0 for a full rebuild,
    -- or the snapshot's @advancedTo@ for snapshot-based recovery).
  , backfillToOffset   :: !Int64
    -- ^ Changelog end offset after replay.
  } deriving stock (Eq, Show)

-- | Rebuild a store by replaying its changelog from @startOffset@.
-- Pass @0@ for a full bootstrap. Returns how many entries were
-- applied and the offset range covered.
backfillFromChangelog
  :: KeyValueStore k v
  -> StoreName
  -> Serde k
  -> Serde v
  -> ChangelogTopic
  -> Int64                   -- ^ start offset (0 = from the beginning)
  -> IO BackfillResult
backfillFromChangelog store sn ks vs topic startOffset = do
  task <- newStandbyTask store topic sn ks vs
  writeIORef (sbOffset task) startOffset
  !applied <- advanceStandby task
  endOff <- currentChangelogOffset topic
  pure BackfillResult
    { backfillApplied    = applied
    , backfillFromOffset = startOffset
    , backfillToOffset   = endOff
    }

-- | Restore the latest snapshot for a store, then replay only the
-- changelog tail from the snapshot's @advancedTo@ offset. With no
-- snapshot present this degrades to a full changelog replay from 0.
backfillFromSnapshot
  :: ObjectStoreClient
  -> StoreName
  -> KeyValueStore k v
  -> Serde k
  -> Serde v
  -> ChangelogTopic
  -> IO (Either Text BackfillResult)
backfillFromSnapshot os sn store ks vs topic = do
  restored <-
    restoreFromSnapshot os sn (deserialize ks) (deserialize vs) store
  case restored of
    Left err -> pure (Left err)
    Right mManifest -> do
      let startOffset = maybe 0 manifestAdvancedTo mManifest
      Right <$> backfillFromChangelog store sn ks vs topic startOffset

----------------------------------------------------------------------
-- CDC backfill
----------------------------------------------------------------------

-- | Outcome of draining a CDC source into a store.
data CDCBackfillResult = CDCBackfillResult
  { cdcApplied       :: !Int
    -- ^ Events applied to the store (after per-key compaction).
  , cdcSteps         :: !Int
    -- ^ Productive poll iterations.
  , cdcSchemaChanges :: ![SchemaChange]
    -- ^ Schema-change announcements observed during the drain.
  , cdcFinalPhase    :: !CDCPhase
    -- ^ Phase reported by the last poll (snapshot vs streaming).
  } deriving stock (Show)

-- | Drain every currently-available event from a CDC source into the
-- store, applying the standard Insert/Update → put, Delete → delete
-- mapping with per-key compaction. Stops at the first empty poll (no
-- events and no schema changes), bounded against a pathological
-- never-empty source.
cdcBackfill
  :: Ord k => CDCSource k v -> KeyValueStore k v -> IO CDCBackfillResult
cdcBackfill src store = loop 0 0 [] SnapshotPhase maxSteps
  where
    maxSteps = 100000 :: Int
    loop !applied !steps scs phase budget
      | budget <= 0 = pure (CDCBackfillResult applied steps scs phase)
      | otherwise = do
          (n, newScs, phase') <- cdcToKTableStep src store
          if n == 0 && null newScs
            then pure (CDCBackfillResult applied steps scs phase')
            else loop (applied + n) (steps + 1) (scs <> newScs)
                      phase' (budget - 1)
