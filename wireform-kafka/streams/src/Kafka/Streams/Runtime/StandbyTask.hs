{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Runtime.StandbyTask
-- Description : Standby-task replay machinery
--
-- A /standby task/ shadows an active task by replaying the
-- active's changelog topic into a local copy of the state
-- store. When a rebalance moves the active task off the
-- current host, the standby is promoted in place — avoiding
-- the cost of restoring the state store from scratch.
--
-- Mirrors @org.apache.kafka.streams.processor.internals.StandbyTask@.
--
-- == Current scope
--
--   * 'StandbyTask' — per-task state: changelog @(topic,
--     partition)@, replay offset, owned store, last-reported
--     lag.
--   * 'standbyReplay' — fold a batch of changelog records into
--     the owned store and report progress to the KIP-441
--     warmup-lag map.
--   * 'newStandbyManager' / 'addStandbyTask' /
--     'removeStandbyTask' — registry the runtime consults
--     during rebalance + assignment.
--
-- == Deferred
--
-- A live native driver doesn't yet plug a /second/ Kafka
-- consumer in to pull changelog records on the side; the
-- replay function here is fed by either an in-memory test
-- harness or a user-driven loop. The integration piece (a
-- per-task @ChangelogConsumer@ that polls the broker
-- independently from the main 'StreamDriver') is tracked in
-- the live-broker integration suite; the API below is the
-- contract.
module Kafka.Streams.Runtime.StandbyTask
  ( -- * Tasks
    StandbyTask (..)
  , newStandbyTask
    -- * Manager
  , StandbyManager
  , newStandbyManager
  , addStandbyTask
  , removeStandbyTask
  , listStandbyTasks
    -- * Replay
  , ChangelogRecord (..)
  , standbyReplay
  ) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , StoreName
  )

----------------------------------------------------------------------
-- Tasks
----------------------------------------------------------------------

-- | One standby task. Holds the metadata the runtime needs to
-- pull from the changelog topic and apply each record to the
-- local store.
data StandbyTask = StandbyTask
  { taskId         :: !TaskId
  , changelogTopic :: !Text
  , partition      :: !Int32
  , storeName      :: !StoreName
  , replayOffset   :: !(TVar Int64)
    -- ^ Next changelog offset to read.
  , endOffset      :: !(TVar Int64)
    -- ^ Last-seen high-water mark for the changelog
    --   partition. Updated by the changelog consumer (mock
    --   harness or native driver). @endOffset - replayOffset@
    --   is the current lag.
  }

-- | Allocate a fresh 'StandbyTask' with replay offset 0 and
-- end-offset 0.
newStandbyTask
  :: TaskId -> Text -> Int32 -> StoreName -> IO StandbyTask
newStandbyTask tid topic part sn = do
  replay <- newTVarIO 0
  endOff <- newTVarIO 0
  pure StandbyTask
    { taskId         = tid
    , changelogTopic = topic
    , partition      = part
    , storeName      = sn
    , replayOffset   = replay
    , endOffset      = endOff
    }

----------------------------------------------------------------------
-- Manager
----------------------------------------------------------------------

-- | Registry of every standby task running on this instance.
-- Keyed by @(taskId, changelog-topic, partition)@ so the same
-- TaskId can hold standbys for different stores (the JVM
-- @TaskId@ doesn't distinguish them either; we encode the
-- (topic, partition) explicitly).
newtype StandbyManager = StandbyManager
  { tasks :: TVar (Map (TaskId, Text, Int32) StandbyTask)
  }

newStandbyManager :: IO StandbyManager
newStandbyManager = StandbyManager <$> newTVarIO Map.empty

addStandbyTask :: StandbyManager -> StandbyTask -> IO ()
addStandbyTask (StandbyManager tv) st =
  atomically $ modifyTVar' tv $
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

-- | One changelog record. The runtime applies this to the
-- standby's local store via 'kvsPut' (delete is encoded as
-- 'Nothing' value, matching Kafka's tombstone convention).
data ChangelogRecord = ChangelogRecord
  { offset :: !Int64
  , key    :: !ByteString
  , value  :: !(Maybe ByteString)
  , end    :: !Int64
    -- ^ End-of-log marker the consumer saw at fetch time;
    --   used to refresh the standby's end-offset so
    --   'reportWarmupLag' reflects the real lag.
  }
  deriving stock (Eq, Show, Generic)

-- | Fold a batch of changelog records into the standby's
-- local store. The store is treated as @KeyValueStore
-- ByteString (Maybe ByteString)@ because the changelog wire
-- format is bytes — typed access happens through the store's
-- normal user-facing serdes.
--
-- Returns the updated lag (end-offset minus next replay
-- offset), which the caller typically forwards to
-- 'reportWarmupLag' in the streams runtime so the
-- KIP-441 probing-rebalance machinery can decide whether to
-- promote.
standbyReplay
  :: StandbyTask
  -> KeyValueStore ByteString (Maybe ByteString)
  -> [ChangelogRecord]
  -> IO Int64
standbyReplay st kvs batch = do
  mapM_ apply batch
  case batch of
    []   -> currentLag
    rs   -> do
      let !lastOff = (last rs).offset
          !lastEnd = (last rs).end
      atomically $ do
        writeTVar st.replayOffset (lastOff + 1)
        writeTVar st.endOffset    (max lastEnd (lastOff + 1))
      currentLag
  where
    apply r = case r.value of
      Just v  -> kvsPut kvs r.key (Just v)
      Nothing ->
        -- Tombstone: kvsPut with Nothing keeps the JVM
        -- changelog convention (the user-side serde wraps
        -- the actual delete).
        kvsPut kvs r.key Nothing
    currentLag = do
      next <- readTVarIO st.replayOffset
      e    <- readTVarIO st.endOffset
      pure (max 0 (e - next))
