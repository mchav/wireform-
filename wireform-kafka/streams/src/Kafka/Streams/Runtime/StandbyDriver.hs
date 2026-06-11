{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Runtime.StandbyDriver
Description : Native-side changelog consumer for standby tasks

A standby task is a replica that shadows an active task by
reading the active's changelog topic and replaying records
into its own local copy of the state store. This module
spawns the second 'Kafka.Client.Consumer' the streams
runtime needs to do that — separate from the main
consumer that drives the active tasks.

== Lifecycle

  * 'newStandbyDriver' allocates the driver but doesn't
    start polling.
  * 'attachStandbyConsumer' parks a live consumer + the
    KV store to replay into. The consumer must already be
    subscribed to the changelog topics of every standby in
    'StandbyManager'.
  * 'startStandbyDriver' forks the poll-loop async; it
    periodically polls, dispatches each record into the
    'StandbyTask' whose '(changelogTopic, partition)'
    matches, and calls 'standbyReplay'.
  * 'reportWarmupLagCallback' is fired after every replay
    so the parent runtime can call
    'Kafka.Streams.Runtime.reportWarmupLag'.
  * 'stopStandbyDriver' cancels the async and lets the
    caller close the consumer.

== What lives on the parent runtime side

The driver carries the consumer + the changelog-poll loop;
the 'StandbyManager' + 'StandbyTask' bookkeeping +
warmup-lag reporting all stay in
'Kafka.Streams.Runtime'. Callers wire the two via the
'reportWarmupLagCallback' constructor argument.

== Driver-agnostic

'StandbyDriver' accepts an opaque /poll/ action so it works
against either a real 'Kafka.Client.Consumer' or a mock
driver. Tests use the mock path for deterministic replay;
production wires in a live consumer.
-}
module Kafka.Streams.Runtime.StandbyDriver (
  -- * Driver
  StandbyDriver,
  StandbyPollFn,
  StandbyStoreLookup,
  newStandbyDriver,
  startStandbyDriver,
  stopStandbyDriver,

  -- * Driver tick (called once per poll cycle)
  standbyDriverTick,
) where

import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Kafka.Streams.Processor (TaskId)
import Kafka.Streams.Runtime.StandbyTask (
  ChangelogRecord (..),
  StandbyManager,
  StandbyTask (..),
  listStandbyTasks,
  standbyReplay,
 )
import Kafka.Streams.State.Store (
  KeyValueStore,
  StoreName,
 )


----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

{- | One batch of changelog records, keyed by @(topic,
partition)@. The driver dispatches each batch to the
'StandbyTask' whose @(topic, partition)@ matches.
-}
type StandbyPollFn =
  Int
  -- ^ poll timeout (ms)
  -> IO
       [ ( (Text, Int32)
         , [ChangelogRecord]
         )
       ]


{- | Resolve a 'StoreName' to the live KV store on the
instance. The parent runtime supplies this so the driver
doesn't need to know about engine internals.
-}
type StandbyStoreLookup =
  StoreName -> IO (Maybe (KeyValueStore ByteString (Maybe ByteString)))


data StandbyDriver = StandbyDriver
  { manager :: !StandbyManager
  , poll :: !StandbyPollFn
  , storeOf :: !StandbyStoreLookup
  , report :: !(TaskId -> Int64 -> IO ())
  , pollMs :: !Int
  , running :: !(TVar Bool)
  , thread :: !(IORef (Maybe (Async ())))
  }


----------------------------------------------------------------------
-- Construction
----------------------------------------------------------------------

newStandbyDriver
  :: StandbyManager
  -> StandbyPollFn
  -> StandbyStoreLookup
  -> (TaskId -> Int64 -> IO ())
  -- ^ warmup-lag reporter
  -> Int
  -- ^ poll timeout (ms)
  -> IO StandbyDriver
newStandbyDriver mgr pollFn lookupFn reportFn pollMsArg = do
  run <- newTVarIO True
  thrd <- newIORef Nothing
  pure
    StandbyDriver
      { manager = mgr
      , poll = pollFn
      , storeOf = lookupFn
      , report = reportFn
      , pollMs = pollMsArg
      , running = run
      , thread = thrd
      }


----------------------------------------------------------------------
-- Lifecycle
----------------------------------------------------------------------

startStandbyDriver :: StandbyDriver -> IO ()
startStandbyDriver drv = do
  existing <- readIORef drv.thread
  case existing of
    Just _ -> pure ()
    Nothing -> do
      th <- async (driverLoop drv)
      writeIORef drv.thread (Just th)


stopStandbyDriver :: StandbyDriver -> IO ()
stopStandbyDriver drv = do
  atomically (writeTVar drv.running False)
  m <- readIORef drv.thread
  case m of
    Nothing -> pure ()
    Just th -> do
      cancel th
      _ <- waitCatch th
      writeIORef drv.thread Nothing


driverLoop :: StandbyDriver -> IO ()
driverLoop drv = loop
  where
    loop = do
      keepGoing <- readTVarIO drv.running
      if not keepGoing
        then pure ()
        else do
          r <-
            try (standbyDriverTick drv)
              :: IO (Either SomeException ())
          case r of
            Right () -> pure ()
            Left _ ->
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

{- | One poll-and-dispatch cycle. Public so tests can drive
the loop without spawning the async.
-}
standbyDriverTick :: StandbyDriver -> IO ()
standbyDriverTick drv = do
  batches <- drv.poll drv.pollMs
  -- Index the active standbys by (topic, partition) so the
  -- dispatch is O(1) per batch.
  tasks <- listStandbyTasks drv.manager
  let taskIx :: Map (Text, Int32) StandbyTask
      taskIx =
        Map.fromList
          [((t.changelogTopic, t.partition), t) | t <- tasks]
  mapM_ (apply taskIx) batches
  where
    apply taskIx (key, records) = case Map.lookup key taskIx of
      Nothing -> pure () -- record for a partition we
      -- don't standby — drop.
      Just task -> do
        mStore <- drv.storeOf task.storeName
        case mStore of
          Nothing -> pure () -- store isn't materialised
          -- yet on this instance.
          Just kvs -> do
            lag <- standbyReplay task kvs records
            drv.report task.taskId lag
