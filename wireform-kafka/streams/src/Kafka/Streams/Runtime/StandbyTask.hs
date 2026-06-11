{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Kafka.Streams.Runtime.StandbyTask
Description : Standby-task replay machinery

A /standby task/ shadows an active task by replaying the
active's changelog topic into a local copy of the state
store. When a rebalance moves the active task off the
current host, the standby is promoted in place — avoiding
the cost of restoring the state store from scratch.

Mirrors @org.apache.kafka.streams.processor.internals.StandbyTask@.

== Current scope

  * 'StandbyTask' — per-task state: changelog @(topic,
    partition)@, replay offset, owned store, last-reported
    lag.
  * 'standbyReplay' — fold a batch of changelog records into
    the owned store and report progress to the KIP-441
    warmup-lag map.
  * 'newStandbyManager' / 'addStandbyTask' /
    'removeStandbyTask' — registry the runtime consults
    during rebalance + assignment.

== Deferred

A live native driver doesn't yet plug a /second/ Kafka
consumer in to pull changelog records on the side; the
replay function here is fed by either an in-memory test
harness or a user-driven loop. The integration piece (a
per-task @ChangelogConsumer@ that polls the broker
independently from the main 'StreamDriver') is tracked in
the live-broker integration suite; the API below is the
contract.
-}
module Kafka.Streams.Runtime.StandbyTask (
  -- * Tasks
  StandbyTask (..),
  StandbyMode (..),
  newStandbyTask,
  newSnapshotPointerStandby,

  -- * Manager
  StandbyManager,
  newStandbyManager,
  addStandbyTask,
  removeStandbyTask,
  listStandbyTasks,

  -- * Replay
  ChangelogRecord (..),
  standbyReplay,

  -- * Snapshot-pointer mode (Riffle \xc2\xa71)
  standbyAdvancedTo,
  standbyLagSnapshotMode,
  bumpSnapshotPointer,
) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  StoreName,
 )


----------------------------------------------------------------------
-- Tasks
----------------------------------------------------------------------

-- | What replication model a 'StandbyTask' is running in.
data StandbyMode
  = {- | Original mode: mirror every changelog record into a
    local copy of the state store. The active is promoted by
    swapping pointers; the standby's local bytes are the new
    source of truth.
    -}
    ReplayBytes
  | {- | Riffle \xc2\xa71 mode: don't mirror bytes at all. The standby
    only tracks @(latestSnapshotId, advancedToOffset)@ as it
    observes the active publishing snapshots. Promotion = the
    new owner fetches the snapshot from the object store and
    replays the changelog tail. Storage cost is O(1) per
    standby; warmup latency is O(snapshot size) at promotion
    time, which is the trade-off the snapshot machinery
    explicitly bounds.
    -}
    SnapshotPointer
  deriving stock (Eq, Show, Generic)


{- | One standby task. Holds the metadata the runtime needs to
track replication for one (taskId, changelog-topic, partition)
triple.
-}
data StandbyTask = StandbyTask
  { taskId :: !TaskId
  , changelogTopic :: !Text
  , partition :: !Int32
  , storeName :: !StoreName
  , mode :: !StandbyMode
  {- ^ Riffle \xc2\xa71: which replication model this standby uses.
  Defaults to 'ReplayBytes' for back-compat;
  'SnapshotPointer' is built via 'newSnapshotPointerStandby'.
  -}
  , replayOffset :: !(TVar Int64)
  {- ^ /Bytes mode:/ next changelog offset to read.
  /Pointer mode:/ the changelog offset baked into the
  most recently observed snapshot (i.e. @advancedTo@).
  -}
  , endOffset :: !(TVar Int64)
  {- ^ Last-seen high-water mark for the changelog
  partition. Updated by the changelog consumer (mock
  harness or native driver). @endOffset - replayOffset@
  is the current lag.
  -}
  , snapshotPtr :: !(TVar (Maybe Int64))
  {- ^ Riffle \xc2\xa71: in 'SnapshotPointer' mode, the latest
  snapshot id this standby has observed. 'Nothing' until
  the first 'bumpSnapshotPointer'. 'ReplayBytes' standbys
  leave this 'Nothing' for ever.
  -}
  }


{- | Allocate a fresh 'StandbyTask' in the bytes-replication
mode. Replay offset starts at 0; the runtime advances it as
it consumes the changelog.
-}
newStandbyTask
  :: TaskId -> Text -> Int32 -> StoreName -> IO StandbyTask
newStandbyTask tid topic part sn = do
  replay <- newTVarIO 0
  endOff <- newTVarIO 0
  snapPtr <- newTVarIO Nothing
  pure
    StandbyTask
      { taskId = tid
      , changelogTopic = topic
      , partition = part
      , storeName = sn
      , mode = ReplayBytes
      , replayOffset = replay
      , endOffset = endOff
      , snapshotPtr = snapPtr
      }


{- | Allocate a 'StandbyTask' in the Riffle \xc2\xa71
'SnapshotPointer' mode. Holds no local replica; only tracks
the snapshot pointer + changelog offset. The runtime calls
'bumpSnapshotPointer' each time it observes a new snapshot
being published.
-}
newSnapshotPointerStandby
  :: TaskId -> Text -> Int32 -> StoreName -> IO StandbyTask
newSnapshotPointerStandby tid topic part sn = do
  replay <- newTVarIO 0
  endOff <- newTVarIO 0
  snapPtr <- newTVarIO Nothing
  pure
    StandbyTask
      { taskId = tid
      , changelogTopic = topic
      , partition = part
      , storeName = sn
      , mode = SnapshotPointer
      , replayOffset = replay
      , endOffset = endOff
      , snapshotPtr = snapPtr
      }


----------------------------------------------------------------------
-- Manager
----------------------------------------------------------------------

{- | Registry of every standby task running on this instance.
Keyed by @(taskId, changelog-topic, partition)@ so the same
TaskId can hold standbys for different stores (the JVM
@TaskId@ doesn't distinguish them either; we encode the
(topic, partition) explicitly).
-}
newtype StandbyManager = StandbyManager
  { tasks :: TVar (Map (TaskId, Text, Int32) StandbyTask)
  }


newStandbyManager :: IO StandbyManager
newStandbyManager = StandbyManager <$> newTVarIO Map.empty


addStandbyTask :: StandbyManager -> StandbyTask -> IO ()
addStandbyTask (StandbyManager tv) st =
  atomically $
    modifyTVar' tv $
      Map.insert (st.taskId, st.changelogTopic, st.partition) st


removeStandbyTask
  :: StandbyManager -> TaskId -> Text -> Int32 -> IO ()
removeStandbyTask (StandbyManager tv) tid topic part =
  atomically $ modifyTVar' tv (Map.delete (tid, topic, part))


listStandbyTasks :: StandbyManager -> IO [StandbyTask]
listStandbyTasks (StandbyManager tv) =
  Map.elems <$> readTVarIO tv


----------------------------------------------------------------------
-- Replay
----------------------------------------------------------------------

{- | One changelog record. The runtime applies this to the
standby's local store via 'kvsPut' (delete is encoded as
'Nothing' value, matching Kafka's tombstone convention).
-}
data ChangelogRecord = ChangelogRecord
  { offset :: !Int64
  , key :: !ByteString
  , value :: !(Maybe ByteString)
  , end :: !Int64
  {- ^ End-of-log marker the consumer saw at fetch time;
  used to refresh the standby's end-offset so
  'reportWarmupLag' reflects the real lag.
  -}
  }
  deriving stock (Eq, Show, Generic)


{- | Fold a batch of changelog records into the standby's
local store. The store is treated as @KeyValueStore
ByteString (Maybe ByteString)@ because the changelog wire
format is bytes — typed access happens through the store's
normal user-facing serdes.

Returns the updated lag (end-offset minus next replay
offset), which the caller typically forwards to
'reportWarmupLag' in the streams runtime so the
KIP-441 probing-rebalance machinery can decide whether to
promote.
-}
standbyReplay
  :: StandbyTask
  -> KeyValueStore ByteString (Maybe ByteString)
  -> [ChangelogRecord]
  -> IO Int64
standbyReplay st kvs batch = do
  mapM_ apply batch
  case batch of
    [] -> currentLag
    rs -> do
      let !lastOff = (last rs).offset
          !lastEnd = (last rs).end
      atomically $ do
        writeTVar st.replayOffset (lastOff + 1)
        writeTVar st.endOffset (max lastEnd (lastOff + 1))
      currentLag
  where
    apply r = case r.value of
      Just v -> kvsPut kvs r.key (Just v)
      Nothing ->
        -- Tombstone: kvsPut with Nothing keeps the JVM
        -- changelog convention (the user-side serde wraps
        -- the actual delete).
        kvsPut kvs r.key Nothing
    currentLag = do
      next <- readTVarIO st.replayOffset
      e <- readTVarIO st.endOffset
      pure (max 0 (e - next))


----------------------------------------------------------------------
-- Snapshot-pointer mode (Riffle \xc2\xa71)
----------------------------------------------------------------------

{- | In 'SnapshotPointer' mode, the active task publishes
snapshots with a known @advancedTo@ changelog offset; each
such publish should be reflected on every standby via this
call. The standby's 'replayOffset' jumps to the new
@advancedTo@, the snapshot pointer is updated, and the lag
shrinks accordingly.

In 'ReplayBytes' mode this is a no-op; the standby continues
consuming the changelog normally.
-}
bumpSnapshotPointer
  :: StandbyTask
  -> Int64
  -- ^ snapshot id
  -> Int64
  {- ^ advancedTo (changelog offset the snapshot
  was taken at)
  -}
  -> Int64
  -- ^ current end-of-changelog
  -> IO ()
bumpSnapshotPointer st sid advTo endOff_ = atomically $ case st.mode of
  ReplayBytes -> pure ()
  SnapshotPointer -> do
    writeTVar st.snapshotPtr (Just sid)
    writeTVar st.replayOffset advTo
    writeTVar st.endOffset (max endOff_ advTo)


{- | The most recently snapshotted offset the standby has caught
up to. For a 'ReplayBytes' standby this is identical to its
'replayOffset' (the next-to-replay marker). For a
'SnapshotPointer' standby this is the @advancedTo@ baked into
the latest snapshot it has observed.
-}
standbyAdvancedTo :: StandbyTask -> IO Int64
standbyAdvancedTo st = readTVarIO st.replayOffset


{- | Current snapshot-mode lag: @endOffset - replayOffset@.
Identical to what 'standbyReplay' returns for the bytes
variant, but exposed as a separate name so the runtime's
KIP-441 probing-rebalance logic can read it without
triggering a replay call.
-}
standbyLagSnapshotMode :: StandbyTask -> IO Int64
standbyLagSnapshotMode st = do
  next <- readTVarIO st.replayOffset
  e <- readTVarIO st.endOffset
  pure (max 0 (e - next))
