{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime.Task
-- Description : Task / TaskManager abstractions
--
-- A 'Task' is a single instance of the topology engine bound to a
-- specific @TaskId@ (i.e. a (subtopology, partition) pair). The
-- 'TaskManager' owns many tasks and dispatches records to the right
-- one based on the source partition.
--
-- This is the building block for multi-task scheduling: a
-- 'Kafka.Streams.Runtime.KafkaStreams' instance with
-- @numStreamThreads > 0@ allocates a 'TaskManager' per worker
-- thread, each owning the slice of partitions assigned to that
-- thread by the broker's group coordinator.
--
-- The single-task 'TopologyTestDriver' continues to work — it just
-- uses a 'TaskManager' with exactly one task at @TaskId 0 0@.
module Kafka.Streams.Runtime.Task
  ( -- * Tasks
    Task (..)
  , newTask
  , feedTask
  , commitTask
  , closeTask
    -- * Task manager
  , TaskManager
  , newTaskManager
  , addTask
  , removeTask
  , routeByPartition
  , tasks
  , taskCount
  , commitAllTasks
  , closeAllTasks
  , pipeInputAcrossPartitions
  ) where

import Control.Concurrent.STM
import Control.Monad (forM_, void)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

import Kafka.Streams.Errors (DeserializationHandler)
import Kafka.Streams.Internal.Engine
  ( Engine
  , buildEngine
  , closeEngine
  , commitEngine
  , feedSource
  )
import Kafka.Streams.Internal.RecordCollector (RecordCollector)
import Kafka.Streams.Processor (TaskId (..))
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Time (Timestamp)
import Kafka.Streams.Types (TopicName)

----------------------------------------------------------------------
-- Task
----------------------------------------------------------------------

data Task = Task
  { taskTaskId   :: !TaskId
  , taskEngine   :: !Engine
  , taskOwnsPart :: !(Int32 -> Bool)
    -- ^ Returns 'True' if this task owns the given partition. The
    -- runtime usually fixes this to a closure over a 'Set Int32'.
  }

newTask
  :: Topo.TopologyValid
  -> TaskId
  -> Text                                -- application id
  -> RecordCollector
  -> DeserializationHandler
  -> (Int32 -> Bool)                     -- ownership predicate
  -> IO Task
newTask topo tid appId coll handler ownership = do
  engine <- buildEngine topo tid appId coll handler
  pure Task
    { taskTaskId   = tid
    , taskEngine   = engine
    , taskOwnsPart = ownership
    }

feedTask
  :: Task
  -> TopicName
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> Int                                 -- partition
  -> Int64                               -- offset
  -> IO ()
feedTask t topic key val ts part off
  | taskOwnsPart t (fromIntegral part) =
      feedSource (taskEngine t) topic key val ts part off
  | otherwise = pure ()

commitTask :: Task -> IO ()
commitTask = commitEngine . taskEngine

closeTask :: Task -> IO ()
closeTask = closeEngine . taskEngine

----------------------------------------------------------------------
-- TaskManager
----------------------------------------------------------------------

data TaskManager = TaskManager
  { tmTasks  :: !(TVar (Map TaskId Task))
  , tmByPart :: !(TVar (Map (TopicName, Int32) TaskId))
  }

newTaskManager :: IO TaskManager
newTaskManager = do
  ts <- newTVarIO Map.empty
  ip <- newTVarIO Map.empty
  pure TaskManager { tmTasks = ts, tmByPart = ip }

-- | Register a task and the (topic, partition) pairs it owns.
addTask
  :: TaskManager
  -> Task
  -> [(TopicName, Int32)]
  -> IO ()
addTask tm t parts = atomically $ do
  modifyTVar' (tmTasks tm) (Map.insert (taskTaskId t) t)
  forM_ parts $ \tp ->
    modifyTVar' (tmByPart tm) (Map.insert tp (taskTaskId t))

-- | Drop a task from the manager. The caller is responsible for
-- 'closeTask' separately if it wants to release the engine's
-- resources.
removeTask :: TaskManager -> TaskId -> IO ()
removeTask tm tid = atomically $ do
  modifyTVar' (tmTasks tm) (Map.delete tid)
  modifyTVar' (tmByPart tm) $
    Map.filter (/= tid)

-- | Find the task that owns a given (topic, partition) pair.
routeByPartition
  :: TaskManager
  -> TopicName
  -> Int32
  -> IO (Maybe Task)
routeByPartition tm topic part = atomically $ do
  byPart <- readTVar (tmByPart tm)
  case Map.lookup (topic, part) byPart of
    Nothing  -> pure Nothing
    Just tid -> do
      ts <- readTVar (tmTasks tm)
      pure (Map.lookup tid ts)

tasks :: TaskManager -> IO [Task]
tasks tm = Map.elems <$> readTVarIO (tmTasks tm)

taskCount :: TaskManager -> IO Int
taskCount tm = Map.size <$> readTVarIO (tmTasks tm)

commitAllTasks :: TaskManager -> IO ()
commitAllTasks tm = do
  ts <- tasks tm
  mapM_ commitTask ts

closeAllTasks :: TaskManager -> IO ()
closeAllTasks tm = do
  ts <- tasks tm
  mapM_ closeTask ts

-- | Convenience for tests: feed records that already include
-- partition info, route each one to the correct task.
pipeInputAcrossPartitions
  :: TaskManager
  -> [(TopicName, Maybe ByteString, ByteString, Timestamp, Int, Int64)]
  -> IO ()
pipeInputAcrossPartitions tm = mapM_ go
  where
    go (topic, k, v, ts, part, off) = do
      mt <- routeByPartition tm topic (fromIntegral part)
      case mt of
        Nothing -> pure ()
        Just t  -> void (feedTask t topic k v ts part off)