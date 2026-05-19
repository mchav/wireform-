{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Examples.Ops.DynamicThreads
-- Description : Add and remove stream-threads inside one process
--
-- This is the /intra-process/ analogue of cluster scaling:
-- 'Kafka.Streams.Runtime.WorkerPool' models the
-- @num.stream.threads@ pool. We start with two workers, submit
-- a batch, then add two more workers under load (KIP-663-style
-- dynamic scale-up), submit another batch, and finally remove
-- one worker.
--
-- The 'submitRecordHashed' path keeps the routing table
-- consistent across worker churn: records for the same partition
-- index always land on the same worker as long as the routing
-- table is stable; adding\/removing workers re-hashes only the
-- affected entries.
module Kafka.Streams.Examples.Ops.DynamicThreads
  ( runDemo
  ) where

import Control.Monad (forM_, replicateM_, void)
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.WorkerPool

import Kafka.Streams.Examples.Ops.Helpers

runDemo :: IO ()
runDemo = do
  section "DynamicThreadsDemo"
  topo <- passthroughTopo

  pool <- newWorkerPoolHashed topo "ops-pool" 2
  c0 <- poolWorkerCount pool
  bullet ("Starting workers: " <> show c0)

  -- Each batch uses a disjoint partition range so the hashed
  -- routing table sees fresh partitions after each scale event
  -- (existing entries in the routing cache are sticky -- that's
  -- the whole point of state-store consistency under scaling, see
  -- 'submitRecordHashed'). With fresh partitions per batch the
  -- balance after scale-up actually shows up in the per-worker
  -- counts at the end.
  bullet "Batch A (partitions 0..15, 2 workers)"
  submitBatchRange pool "A"   0 16
  waitForQuiescence pool

  bullet "Scaling up: + 2 workers"
  replicateM_ 2 (void (addPoolWorker pool))
  c1 <- poolWorkerCount pool
  bullet ("Workers now: " <> show c1)

  bullet "Batch B (partitions 100..115, 4 workers)"
  submitBatchRange pool "B" 100 16
  waitForQuiescence pool

  bullet "Scaling down: - 1 worker"
  _ <- removePoolWorker pool
  c2 <- poolWorkerCount pool
  bullet ("Workers now: " <> show c2)

  bullet "Batch C (partitions 200..215, 3 workers)"
  submitBatchRange pool "C" 200 16
  waitForQuiescence pool

  snapshot <- poolWorkersSnapshot pool
  bullet "Per-worker processed counts after final scale-down:"
  forM_ snapshot $ \w -> do
    n <- workerProcessedCount w
    bullet ("    worker " <> show (workerId w) <> " -> " <> show n)
  closeWorkerPool pool

submitBatchRange
  :: WorkerPool
  -> String         -- label
  -> Int            -- partition base
  -> Int            -- batch size
  -> IO ()
submitBatchRange pool label base n =
  forM_ [0 .. n - 1] $ \i -> do
    let v :: Text
        v = T.pack (label <> "-" <> show i)
    submitRecordHashed pool (topicName "in")
      (Just (bytes (T.pack ("k" <> show i))))
      (bytes v) ts0 (base + i)
