{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Runtime.Snapshot
Description : Snapshot lifecycle (publish + recovery)
              for Riffle \xc2\xa71 snapshot-aware stores

Owns the lifecycle thread that decides /when/ to publish
snapshots and the recovery helper that boots a store from
the latest snapshot manifest before changelog replay
starts.

The actual snapshot byte production + restore lives in
'Kafka.Streams.State.KeyValue.Snapshot'; this module is the
runtime glue (a thread that consults 'SnapshotPlan' on each
commit cycle and a 'recoverStore' helper for the boot path).
-}
module Kafka.Streams.Runtime.Snapshot (
  -- * Lifecycle policy
  SnapshotTrigger (..),
  shouldSnapshot,

  -- * Driver loop
  SnapshotState,
  newSnapshotState,
  publishIfDue,

  -- * Recovery
  recoverStore,

  -- * Pruning
  pruneOldSnapshots,
) where

import Control.Concurrent.STM
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Kafka.Streams.Runtime.ObjectStore (
  ObjectStoreClient,
  osDelete,
 )
import Kafka.Streams.State.KeyValue.Snapshot (
  SnapshotId (..),
  SnapshotManifest (..),
  SnapshotPlan (..),
  listSnapshots,
  readLatestManifest,
  restoreFromSnapshot,
  snapshotStore,
  storeSnapshotKey,
 )
import Kafka.Streams.State.Store (
  KeyValueStore,
  StoreName,
 )
import Kafka.Streams.Time (
  Duration,
  Timestamp (..),
  addDuration,
 )


----------------------------------------------------------------------
-- Trigger policy
----------------------------------------------------------------------

{- | Reason the lifecycle thread fired a snapshot. Surface in
metrics; the runtime ignores this except for diagnostics.
-}
data SnapshotTrigger
  = TriggerInterval
  | TriggerRecordCount
  | TriggerOnDemand
  deriving stock (Eq, Show)


{- | Decide whether the next commit cycle should publish a
snapshot under the given plan.

  * 'TriggerInterval' fires when @now \>= lastSnapshotAt +
    spInterval@.
  * 'TriggerRecordCount' fires when @recordsSince \>=
    spMaxRecordsBetween@ (if set).
  * Otherwise no snapshot.
-}
shouldSnapshot
  :: SnapshotPlan
  -> Timestamp
  -- ^ wall-clock now
  -> Timestamp
  -- ^ last snapshot at
  -> Int64
  -- ^ records consumed since last snapshot
  -> Maybe SnapshotTrigger
shouldSnapshot plan now lastAt recordsSince
  | now >= addDuration lastAt (spInterval plan) = Just TriggerInterval
  | Just lim <- spMaxRecordsBetween plan
  , recordsSince >= lim =
      Just TriggerRecordCount
  | otherwise = Nothing


----------------------------------------------------------------------
-- Lifecycle state
----------------------------------------------------------------------

{- | Per-store snapshot state held by the runtime. The
lifecycle thread / commit cycle reads + updates this in STM.
-}
data SnapshotState = SnapshotState
  { ssLastAt :: !(TVar Timestamp)
  , ssLastSnapshotId :: !(TVar (Maybe SnapshotId))
  , ssRecordsSince :: !(TVar Int64)
  }


newSnapshotState :: Timestamp -> IO SnapshotState
newSnapshotState now = do
  ssLastAt' <- newTVarIO now
  ssLastSnapshotId' <- newTVarIO Nothing
  ssRecordsSince' <- newTVarIO 0
  pure
    SnapshotState
      { ssLastAt = ssLastAt'
      , ssLastSnapshotId = ssLastSnapshotId'
      , ssRecordsSince = ssRecordsSince'
      }


----------------------------------------------------------------------
-- Publish if due
----------------------------------------------------------------------

{- | Take a snapshot if the policy says it's time. Called by the
runtime at the end of each commit cycle. Returns the trigger
that fired (or 'Nothing' if no snapshot was taken).

The caller supplies the current changelog offset (used as the
snapshot's @advancedTo@) and byte encoders for the store's
key / value types.
-}
publishIfDue
  :: forall k v
   . SnapshotPlan
  -> SnapshotState
  -> ObjectStoreClient
  -> StoreName
  -> Timestamp
  -- ^ now (wall-clock)
  -> Int64
  -- ^ current changelog offset
  -> (k -> ByteString)
  -> (v -> ByteString)
  -> KeyValueStore k v
  -> IO (Maybe (SnapshotTrigger, Either Text SnapshotId))
publishIfDue plan st os sn now changelogOff encK encV kvs = do
  lastAt <- readTVarIO (ssLastAt st)
  recs <- readTVarIO (ssRecordsSince st)
  case shouldSnapshot plan now lastAt recs of
    Nothing -> pure Nothing
    Just trigger -> do
      let !sid = SnapshotId changelogOff
      r <- snapshotStore os sn sid changelogOff encK encV kvs
      case r of
        Left e -> pure (Just (trigger, Left e))
        Right () -> do
          atomically $ do
            writeTVar (ssLastAt st) now
            writeTVar (ssLastSnapshotId st) (Just sid)
            writeTVar (ssRecordsSince st) 0
          -- Best-effort retention prune.
          _ <- pruneOldSnapshots os sn (spRetention plan)
          pure (Just (trigger, Right sid))


----------------------------------------------------------------------
-- Recovery
----------------------------------------------------------------------

{- | Boot-time recovery: restore the underlying store from the
latest snapshot manifest and return the changelog offset the
caller should start replaying from. 'Nothing' means there is
no snapshot — the caller falls back to full changelog replay
as today.
-}
recoverStore
  :: forall k v
   . ObjectStoreClient
  -> StoreName
  -> (ByteString -> Either Text k)
  -> (ByteString -> Either Text v)
  -> KeyValueStore k v
  -> IO (Either Text (Maybe Int64))
recoverStore os sn decK decV kvs = do
  r <- restoreFromSnapshot os sn decK decV kvs
  case r of
    Left e -> pure (Left e)
    Right Nothing -> pure (Right Nothing)
    Right (Just mf) -> pure (Right (Just (manifestAdvancedTo mf)))


----------------------------------------------------------------------
-- Retention
----------------------------------------------------------------------

{- | Keep the @keep@ most recent snapshots; delete the rest.
Idempotent and tolerant of partial failures.
-}
pruneOldSnapshots
  :: ObjectStoreClient
  -> StoreName
  -> Int
  -- ^ keep this many
  -> IO (Either Text Int)
  -- ^ deleted count
pruneOldSnapshots os sn keep
  | keep <= 0 = pure (Right 0)
  | otherwise = do
      r <- listSnapshots os sn
      case r of
        Left e -> pure (Left e)
        Right ids -> do
          let sorted = List.reverse (List.sort ids)
              tooOld = drop keep sorted
          forM_ tooOld $ \sid ->
            () <$ osDelete os (storeSnapshotKey sn sid)
          pure (Right (length tooOld))


-- Silence -Wunused for 'when' which we may want later.
_unused :: ()
_unused = let _ = (BS.length :: ByteString -> Int) in ()
