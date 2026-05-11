{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime.StandbyDriver
-- Description : Native-side changelog consumer for standby tasks
--
-- A standby task is a replica that shadows an active task by
-- reading the active's changelog topic and replaying records
-- into its own local copy of the state store. This module
-- spawns the second 'Kafka.Client.Consumer' the streams
-- runtime needs to do that — separate from the main
-- consumer that drives the active tasks.
--
-- == Lifecycle
--
--   * 'newStandbyDriver' allocates the driver but doesn't
--     start polling.
--   * 'attachStandbyConsumer' parks a live consumer + the
--     KV store to replay into. The consumer must already be
--     subscribed to the changelog topics of every standby in
--     'StandbyManager'.
--   * 'startStandbyDriver' forks the poll-loop async; it
--     periodically polls, dispatches each record into the
--     'StandbyTask' whose '(changelogTopic, partition)'
--     matches, and calls 'standbyReplay'.
--   * 'reportWarmupLagCallback' is fired after every replay
--     so the parent runtime can call
--     'Kafka.Streams.Runtime.reportWarmupLag'.
--   * 'stopStandbyDriver' cancels the async and lets the
--     caller close the consumer.
--
-- == What lives on the parent runtime side
--
-- The driver carries the consumer + the changelog-poll loop;
-- the 'StandbyManager' + 'StandbyTask' bookkeeping +
-- warmup-lag reporting all stay in
-- 'Kafka.Streams.Runtime'. Callers wire the two via the
-- 'reportWarmupLagCallback' constructor argument.
--
-- == Driver-agnostic
--
-- 'StandbyDriver' accepts an opaque /poll/ action so it works
-- against either a real 'Kafka.Client.Consumer' or a mock
-- driver. Tests use the mock path for deterministic replay;
-- production wires in a live consumer.
module Kafka.Streams.Runtime.StandbyDriver
  ( -- * Driver
    StandbyDriver
  , StandbyPollFn
  , StandbyStoreLookup
  , newStandbyDriver
  , startStandbyDriver
  , stopStandbyDriver
    -- * Driver tick (called once per poll cycle)
  , standbyDriverTick
  ) where

import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

import Kafka.Streams.Processor (TaskId)
import Kafka.Streams.Runtime.StandbyTask
  ( ChangelogRecord (..)
  , StandbyManager
  , StandbyTask (..)
  , listStandbyTasks
  , standbyReplay
  )
import Kafka.Streams.State.Store
  ( KeyValueStore
  , StoreName
  )

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

-- | One batch of changelog records, keyed by @(topic,
-- partition)@. The driver dispatches each batch to the
-- 'StandbyTask' whose @(topic, partition)@ matches.
type StandbyPollFn =
  Int                            -- ^ poll timeout (ms)
  -> IO [( (Text, Int32)
         , [ChangelogRecord]
         )]

-- | Resolve a 'StoreName' to the live KV store on the
-- instance. The parent runtime supplies this so the driver
-- doesn't need to know about engine internals.
type StandbyStoreLookup =
  StoreName -> IO (Maybe (KeyValueStore ByteString (Maybe ByteString)))

data StandbyDriver = StandbyDriver
  { sdMgr      :: !StandbyManager
  , sdPoll     :: !StandbyPollFn
  , sdStoreOf  :: !StandbyStoreLookup
  , sdReport   :: !(TaskId -> Int64 -> IO ())
  , sdPollMs   :: !Int
  , sdRunning  :: !(TVar Bool)
  , sdThread   :: !(IORef (Maybe (Async ())))
  }

----------------------------------------------------------------------
-- Construction
----------------------------------------------------------------------

newStandbyDriver
  :: StandbyManager
  -> StandbyPollFn
  -> StandbyStoreLookup
  -> (TaskId -> Int64 -> IO ())   -- ^ warmup-lag reporter
  -> Int                          -- ^ poll timeout (ms)
  -> IO StandbyDriver
newStandbyDriver mgr pollFn lookupFn reportFn pollMs = do
  run    <- newTVarIO True
  thread <- newIORef Nothing
  pure StandbyDriver
    { sdMgr     = mgr
    , sdPoll    = pollFn
    , sdStoreOf = lookupFn
    , sdReport  = reportFn
    , sdPollMs  = pollMs
    , sdRunning = run
    , sdThread  = thread
    }

----------------------------------------------------------------------
-- Lifecycle
----------------------------------------------------------------------

startStandbyDriver :: StandbyDriver -> IO ()
startStandbyDriver drv = do
  existing <- readIORef (sdThread drv)
  case existing of
    Just _ -> pure ()
    Nothing -> do
      th <- async (driverLoop drv)
      writeIORef (sdThread drv) (Just th)

stopStandbyDriver :: StandbyDriver -> IO ()
stopStandbyDriver drv = do
  atomically (writeTVar (sdRunning drv) False)
  m <- readIORef (sdThread drv)
  case m of
    Nothing -> pure ()
    Just th -> do
      cancel th
      _ <- waitCatch th
      writeIORef (sdThread drv) Nothing

driverLoop :: StandbyDriver -> IO ()
driverLoop drv = loop
  where
    loop = do
      keepGoing <- readTVarIO (sdRunning drv)
      if not keepGoing
        then pure ()
        else do
          r <- try (standbyDriverTick drv)
            :: IO (Either SomeException ())
          case r of
            Right () -> pure ()
            Left _   ->
              -- A transient failure (broker hiccup, store
              -- unavailable) shouldn't kill the standby
              -- driver — the next tick retries. The user's
              -- KIP-988 StandbyUpdateListener would normally
              -- be the place to surface the error; this
              -- driver doesn't hold one directly to keep
              -- coupling low.
              pure ()
          loop

----------------------------------------------------------------------
-- Tick (exposed so tests can drive it deterministically)
----------------------------------------------------------------------

-- | One poll-and-dispatch cycle. Public so tests can drive
-- the loop without spawning the async.
standbyDriverTick :: StandbyDriver -> IO ()
standbyDriverTick drv = do
  batches <- sdPoll drv (sdPollMs drv)
  -- Index the active standbys by (topic, partition) so the
  -- dispatch is O(1) per batch.
  tasks <- listStandbyTasks (sdMgr drv)
  let taskIx :: Map (Text, Int32) StandbyTask
      taskIx = Map.fromList
        [ ((stChangelogTopic t, stPartition t), t) | t <- tasks ]
  mapM_ (apply taskIx) batches
  where
    apply taskIx (key, records) = case Map.lookup key taskIx of
      Nothing -> pure ()          -- record for a partition we
                                   -- don't standby — drop.
      Just task -> do
        mStore <- sdStoreOf drv (stStoreName task)
        case mStore of
          Nothing -> pure ()      -- store isn't materialised
                                   -- yet on this instance.
          Just kvs -> do
            lag <- standbyReplay task kvs records
            sdReport drv (stTaskId task) lag

