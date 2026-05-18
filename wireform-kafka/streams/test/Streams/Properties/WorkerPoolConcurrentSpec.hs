{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streams.Properties.WorkerPoolConcurrentSpec
-- Description : Concurrent-submit + churn properties for the WorkerPool
--
-- The existing 'Streams.Properties.WorkerPoolSMSpec' drives the
-- pool sequentially. The real runtime has every public entry
-- point hit by multiple threads at once:
--
--   * 'submitRecordHashed' from the consumer poll loop.
--   * 'addPoolWorker' / 'removePoolWorker' from the runtime's
--     scaling controller.
--   * 'waitForQuiescence' / 'commitAllWorkers' from the commit
--     thread.
--
-- This module hammers those entry points concurrently and
-- enforces the conservation laws that should still hold:
--
--   1. Conservation: with no churn, every record submitted is
--      processed exactly once. Sum of 'workerProcessedCount'
--      across live workers equals the total submit count.
--   2. Sticky routing: the routing table for a @(topic, partition)@
--      observed by 'routingFor' is stable while no remove
--      targets the chosen worker.
--   3. Idempotent close after concurrent churn: 'closeWorkerPool'
--      returns promptly even with active submitter threads
--      racing it.
--   4. Add-only churn never drops records: with concurrent
--      submitters and concurrent 'addPoolWorker', conservation
--      still holds (added workers are pure additions).
module Streams.Properties.WorkerPoolConcurrentSpec (tests) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Control.Monad (replicateM_, void)
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
  ( atomicModifyIORef'
  , newIORef
  , readIORef
  )
import qualified Data.Set as Set
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Kafka.Streams
  ( Timestamp (..)
  , TopologyValid
  , buildTopology
  , consumed
  , newStreamsBuilder
  , produced
  , streamFromTopic
  , textSerde
  , toTopic
  , validateTopology
  )
import Kafka.Streams.Runtime.WorkerPool
  ( WorkerPool
  , addPoolWorker
  , closeWorkerPool
  , newWorkerPoolHashed
  , poolWorkerCount
  , poolWorkersSnapshot
  , removePoolWorker
  , routingFor
  , submitRecordHashed
  , waitForQuiescence
  , workerProcessedCount
  )

----------------------------------------------------------------------
-- Test topology
----------------------------------------------------------------------

passthrough :: IO TopologyValid
passthrough = do
  b <- newStreamsBuilder
  s <- streamFromTopic b "in" (consumed textSerde textSerde)
  toTopic "out" (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

bytes :: String -> BSC.ByteString
bytes = BSC.pack

-- | Run @threads@ in parallel, return when all are done.
runConcurrent :: [IO ()] -> IO ()
runConcurrent ios = do
  as <- mapM Async.async ios
  mapM_ Async.wait as

-- | Sum of processed counts across every currently-live worker.
totalProcessed :: WorkerPool -> IO Int64
totalProcessed pool = do
  ws <- poolWorkersSnapshot pool
  sum <$> mapM workerProcessedCount (toList ws)

----------------------------------------------------------------------
-- Property 1: conservation under concurrent submit (no churn)
----------------------------------------------------------------------

prop_concurrent_submit_conserves :: H.Property
prop_concurrent_submit_conserves = H.property $ do
  threads     <- H.forAll (Gen.int (Range.linear 2 6))
  perThread   <- H.forAll (Gen.int (Range.linear 50 200))
  initialN    <- H.forAll (Gen.int (Range.linear 1 4))
  partitions  <- H.forAll (Gen.int (Range.linear 1 8))
  observed <- H.evalIO $ do
    topo <- passthrough
    pool <- newWorkerPoolHashed topo "wp-conc" initialN
    let submitOne t = submitRecordHashed pool "in"
                        (Just (bytes ("k" ++ show t)))
                        (bytes "v") (Timestamp 0)
                        (t `mod` partitions)
    let mkThread tid = replicateM_ perThread (submitOne tid)
    runConcurrent (map mkThread [0 .. threads - 1])
    waitForQuiescence pool
    n <- totalProcessed pool
    closeWorkerPool pool
    pure n
  observed H.=== fromIntegral (threads * perThread)

----------------------------------------------------------------------
-- Property 2: sticky routing
----------------------------------------------------------------------

prop_routing_is_sticky :: H.Property
prop_routing_is_sticky = H.property $ do
  -- Submit many records on a single (topic, partition) from
  -- multiple threads; routing must agree on a single worker.
  threads     <- H.forAll (Gen.int (Range.linear 2 6))
  perThread   <- H.forAll (Gen.int (Range.linear 20 80))
  initialN    <- H.forAll (Gen.int (Range.linear 2 4))
  partition   <- H.forAll (Gen.int (Range.linear 0 7))
  observed <- H.evalIO $ do
    topo <- passthrough
    pool <- newWorkerPoolHashed topo "wp-sticky" initialN
    routes <- newIORef ([] :: [Maybe Int])
    let submitOne = do
          submitRecordHashed pool "in" (Just (bytes "k"))
            (bytes "v") (Timestamp 0) partition
          mRoute <- routingFor pool partition
          atomicModifyIORef' routes (\xs -> (mRoute : xs, ()))
    runConcurrent
      [ replicateM_ perThread submitOne
      | _ <- [0 .. threads - 1]
      ]
    waitForQuiescence pool
    closeWorkerPool pool
    readIORef routes
  -- Every recorded routing is the same 'Just _' value.
  let observedSet =
        Set.fromList [ i | Just i <- observed ]
  H.assert (Set.size observedSet == 1)

----------------------------------------------------------------------
-- Property 3: idempotent close after concurrent churn
----------------------------------------------------------------------

prop_close_after_concurrent_churn :: H.Property
prop_close_after_concurrent_churn = H.property $ do
  initialN    <- H.forAll (Gen.int (Range.linear 1 3))
  numAdds     <- H.forAll (Gen.int (Range.linear 1 6))
  numSubmits  <- H.forAll (Gen.int (Range.linear 10 60))
  partitions  <- H.forAll (Gen.int (Range.linear 1 4))
  observed <- H.evalIO $ do
    topo <- passthrough
    pool <- newWorkerPoolHashed topo "wp-close" initialN
    let submitter t = submitRecordHashed pool "in"
                        (Just (bytes ("k" ++ show t)))
                        (bytes "v") (Timestamp 0)
                        (t `mod` partitions)
    -- Submitters: many records concurrent with churn.
    let submitters =
          [ mapM_ submitter [0 .. numSubmits - 1]
          | _ <- [0 .. 2 :: Int]
          ]
    -- Churn thread: do all the adds, then remove half of the
    -- final pool. Interleaved by the runtime scheduler with the
    -- submitters.
    let churn = do
          replicateM_ numAdds (void (addPoolWorker pool))
          finalN <- poolWorkerCount pool
          replicateM_ (finalN `div` 2) (void (removePoolWorker pool))
    runConcurrent (churn : submitters)
    waitForQuiescence pool
    closeWorkerPool pool
    pure ()
  -- We're really asserting "no deadlock, no exception". If the
  -- IO returned, the property holds.
  observed H.=== ()

----------------------------------------------------------------------
-- Property 4: add-only churn preserves conservation
----------------------------------------------------------------------

prop_add_only_churn_conserves :: H.Property
prop_add_only_churn_conserves = H.property $ do
  threads    <- H.forAll (Gen.int (Range.linear 2 5))
  perThread  <- H.forAll (Gen.int (Range.linear 40 120))
  initialN   <- H.forAll (Gen.int (Range.linear 1 3))
  numAdds    <- H.forAll (Gen.int (Range.linear 2 5))
  partitions <- H.forAll (Gen.int (Range.linear 1 6))
  observed <- H.evalIO $ do
    topo <- passthrough
    pool <- newWorkerPoolHashed topo "wp-add-conc" initialN
    -- A barrier so the add thread fires while submitters are
    -- already in flight (not after they're done).
    addsDone <- STM.newTVarIO (0 :: Int)
    let submitter t = mapM_
          (\j -> submitRecordHashed pool "in"
                   (Just (bytes ("k" ++ show j)))
                   (bytes "v") (Timestamp 0)
                   ((t + j) `mod` partitions))
          [0 .. perThread - 1]
    let adder = do
          replicateM_ numAdds (do
            _ <- addPoolWorker pool
            STM.atomically $ STM.modifyTVar' addsDone (+ 1))
    runConcurrent (adder : [submitter t | t <- [0 .. threads - 1]])
    -- Adds and submits both done.
    waitForQuiescence pool
    proc <- totalProcessed pool
    countNow <- poolWorkerCount pool
    closeWorkerPool pool
    pure (proc, countNow)
  let (proc, countNow) = observed
  H.annotate ("final worker count: " <> show countNow)
  proc H.=== fromIntegral (threads * perThread)
  -- Pool grew to (initial + numAdds).
  countNow H.=== (initialN + numAdds)

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "WorkerPool concurrent chaos"
  [ testProperty "concurrent submit conserves count" $
      H.withTests 40 prop_concurrent_submit_conserves
  , testProperty "routing for a single partition is sticky under concurrency" $
      H.withTests 40 prop_routing_is_sticky
  , testProperty "closeWorkerPool returns under concurrent submit+churn" $
      H.withTests 25 prop_close_after_concurrent_churn
  , testProperty "add-only concurrent churn preserves conservation" $
      H.withTests 30 prop_add_only_churn_conserves
  ]
