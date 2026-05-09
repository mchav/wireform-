{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime.WorkerPool
-- Description : numStreamThreads worker pool
--
-- Mirrors Java's @StreamThread@ pool: each worker owns its own
-- engine, its own slice of partitions, and a private input queue.
-- The pool fans records out to the correct worker by partition; a
-- dedicated 'async' thread per worker drains its queue and drives
-- the engine.
--
-- == Usage
--
-- @
-- pool <- newWorkerPool topo "app-id" perWorkerOwnership
-- mapM_ (submitRecord pool ...) records
-- waitForQuiescence pool
-- closeWorkerPool pool
-- @
--
-- The single-task 'TopologyTestDriver' is unchanged. The pool is a
-- separate, multi-thread-aware /driver/ used by tests and by the
-- broker-backed runtime when @numStreamThreads > 1@.
module Kafka.Streams.Runtime.WorkerPool
  ( -- * Pool
    WorkerPool
  , Worker (..)
  , newWorkerPool
  , poolWorkers
  , submitRecord
  , waitForQuiescence
  , commitAllWorkers
  , closeWorkerPool
    -- * Per-worker state
  , workerProcessedCount
  , workerEngine
  , workerCollector
  ) where

import Control.Concurrent.Async
  ( Async
  , async
  , cancel
  , waitCatch
  )
import Control.Concurrent.STM
import Control.Monad (forM, forM_, unless)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Text (Text)

import Kafka.Streams.Errors (logAndContinue)
import Kafka.Streams.Internal.Engine
  ( Engine
  , buildEngine
  , closeEngine
  , commitEngine
  , feedSource
  )
import Kafka.Streams.Internal.RecordCollector
  ( RecordCollector
  , inMemoryCollector
  )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Time (Timestamp)
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (TopicName)

----------------------------------------------------------------------
-- Worker
----------------------------------------------------------------------

-- | One worker. Each worker owns:
--
--   * an 'Engine' built from the same topology as every other
--     worker, but operating on its own state (its own stores,
--     its own collector);
--   * a 'Set' of (topic, partition) pairs it processes;
--   * a private 'TQueue' inbox the pool writes to.
data Worker = Worker
  { workerId        :: !Int
  , workerEngine    :: !Engine
  , workerCollector :: !RecordCollector
  , workerOwned     :: !(HashSet (TopicName, Int32))
  , workerInbox     :: !(TQueue WorkRecord)
  , workerStop      :: !(TVar Bool)
  , workerProcessed :: !(TVar Int64)
  , workerSubmitted :: !(TVar Int64)
    -- ^ Tracks how many records have been submitted to this worker
    -- (via 'submitRecord'). 'waitForQuiescence' blocks until
    -- 'workerProcessed' catches up to this number.
  , workerThread    :: !(IORef (Maybe (Async ())))
  }

data WorkRecord = WorkRecord
  { wrTopic     :: !TopicName
  , wrKey       :: !(Maybe ByteString)
  , wrValue     :: !ByteString
  , wrTimestamp :: !Timestamp
  , wrPartition :: !Int
  , wrOffset    :: !Int64
  }

----------------------------------------------------------------------
-- Pool
----------------------------------------------------------------------

data WorkerPool = WorkerPool
  { poolWorkers       :: ![Worker]
  , poolWorkersByIdx  :: !(HashMap.HashMap Int Worker)
    -- ^ Cache of 'poolWorkers' keyed by 'workerId' so the hot
    -- 'submitRecord' lookup is O(1) instead of an O(n) walk over
    -- the list. Built once in 'newWorkerPool'; the worker list
    -- is fixed for the pool's lifetime.
  , poolRouting       :: !(TVar (HashMap.HashMap (TopicName, Int32) Int))
    -- ^ 'HashMap' (not 'Data.Map') for the routing table — every
    -- 'submitRecord' looks up by '(TopicName, Int32)' which has a
    -- 'Hashable' instance.
  , poolNextOff       :: !(IORef Int64)
  }

-- | Build a pool of @length perWorkerOwnership@ workers. Each entry
-- in @perWorkerOwnership@ is the set of (topic, partition) pairs
-- that worker handles. The runtime's partition assignor decides
-- this in production; tests pass it in directly.
newWorkerPool
  :: Topo.TopologyValid
  -> Text                                 -- ^ application id
  -> [HashSet (TopicName, Int32)]
  -> IO WorkerPool
newWorkerPool topo appId perWorkerOwnership = do
  off <- newIORef 0
  routing <- newTVarIO (buildRouting perWorkerOwnership)
  workers <- forM (zip [0 ..] perWorkerOwnership) $ \(idx, owned) -> do
    inbox     <- newTQueueIO
    stop      <- newTVarIO False
    processed <- newTVarIO 0
    submitted <- newTVarIO 0
    coll      <- inMemoryCollector
    eng       <- buildEngine topo (TaskId 0 (fromIntegral idx))
                   appId coll logAndContinue
    threadRef <- newIORef Nothing
    let !w = Worker
          { workerId        = idx
          , workerEngine    = eng
          , workerCollector = coll
          , workerOwned     = owned
          , workerInbox     = inbox
          , workerStop      = stop
          , workerProcessed = processed
          , workerSubmitted = submitted
          , workerThread    = threadRef
          }
    th <- async (workerLoop w)
    -- Park the Async handle so closeWorkerPool can cancel it.
    atomicModifyIORef' threadRef (\_ -> (Just th, ()))
    pure w
  pure WorkerPool
    { poolWorkers      = workers
    , poolWorkersByIdx = HashMap.fromList [(workerId w, w) | w <- workers]
    , poolRouting      = routing
    , poolNextOff      = off
    }

buildRouting :: [HashSet (TopicName, Int32)] -> HashMap.HashMap (TopicName, Int32) Int
buildRouting perWorker =
  HashMap.fromList
    [ (tp, idx)
    | (idx, owned) <- zip [0 ..] perWorker
    , tp <- HashSet.toList owned
    ]

----------------------------------------------------------------------
-- Submit
----------------------------------------------------------------------

-- | Enqueue a record onto the worker that owns its (topic,
-- partition). Records for unassigned partitions are silently
-- dropped (matches the broker's behaviour after a rebalance).
submitRecord
  :: WorkerPool
  -> TopicName
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> Int
  -> IO ()
submitRecord pool topic key val ts part = do
  off <- atomicModifyIORef' (poolNextOff pool) (\n -> (n + 1, n))
  routing <- readTVarIO (poolRouting pool)
  case HashMap.lookup (topic, fromIntegral part) routing of
    Nothing  -> pure ()
    Just idx ->
      -- O(1) lookup against the cached index; the previous shape
      -- walked '[(workerId w, w) | w <- poolWorkers pool]' on
      -- every record (O(numWorkers)).
      case HashMap.lookup idx (poolWorkersByIdx pool) of
        Nothing -> pure ()
        Just w  ->
          atomically $ do
            writeTQueue (workerInbox w)
              (WorkRecord topic key val ts part off)
            modifyTVar' (workerSubmitted w) (+ 1)

-- | Block until every record submitted so far has finished
-- processing on the right worker.  Coordinated via per-worker
-- @workerSubmitted@ / @workerProcessed@ counters; no thread sleeps.
waitForQuiescence :: WorkerPool -> IO ()
waitForQuiescence pool =
  atomically $
    forM_ (poolWorkers pool) $ \w -> do
      sub  <- readTVar (workerSubmitted w)
      done <- readTVar (workerProcessed w)
      unless (done >= sub) retry

-- | Force a commit on every worker's engine.
commitAllWorkers :: WorkerPool -> IO ()
commitAllWorkers pool =
  forM_ (poolWorkers pool) (commitEngine . workerEngine)

closeWorkerPool :: WorkerPool -> IO ()
closeWorkerPool pool = do
  -- Drain in-flight records first.
  waitForQuiescence pool
  forM_ (poolWorkers pool) $ \w ->
    atomically (writeTVar (workerStop w) True)
  -- Cancel each worker thread.
  forM_ (poolWorkers pool) $ \w -> do
    mTh <- readIORef (workerThread w)
    case mTh of
      Just th -> do
        cancel th
        _ <- waitCatch th
        pure ()
      Nothing -> pure ()
  -- Close engines.
  forM_ (poolWorkers pool) (closeEngine . workerEngine)

----------------------------------------------------------------------
-- Worker loop
----------------------------------------------------------------------

workerLoop :: Worker -> IO ()
workerLoop w = loop
  where
    loop = do
      next <- atomically $ do
        s <- readTVar (workerStop w)
        if s
          then pure Nothing
          else Just <$> readTQueue (workerInbox w)
      case next of
        Nothing -> pure ()
        Just r  -> do
          -- Filter on ownership for safety; the routing index
          -- shouldn't have sent us anything we don't own.
          let !tp = (wrTopic r, fromIntegral (wrPartition r))
          if HashSet.member tp (workerOwned w)
            then do
              feedSource (workerEngine w) (wrTopic r)
                (wrKey r) (wrValue r) (wrTimestamp r)
                (wrPartition r) (wrOffset r)
              atomically $ modifyTVar' (workerProcessed w) (+ 1)
            else pure ()
          loop

workerProcessedCount :: Worker -> IO Int64
workerProcessedCount = readTVarIO . workerProcessed