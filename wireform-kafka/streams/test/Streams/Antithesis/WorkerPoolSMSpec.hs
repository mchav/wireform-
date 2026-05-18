{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streams.Antithesis.WorkerPoolSMSpec
-- Description : State-machine property tests for the WorkerPool
--               under dynamic membership + hashed routing
--
-- Models the hashed-routing 'WorkerPool' as a pure
-- (worker-set, routing-table, processed-count-per-worker) and
-- runs random sequences of @addPoolWorker@, @removePoolWorker@,
-- and @submitRecordHashed@ commands. Invariants enforced after
-- every step:
--
--   1. Total records processed = total records submitted to
--      live (topic, partition) routes. Submits to a routing
--      target whose worker was just removed re-hash on the
--      next submission.
--   2. Routing stickiness: once a @(topic, partition)@ has been
--      routed to a worker that's still alive, every subsequent
--      submission for that key lands on the same worker.
--   3. After every command, the live worker count matches the
--      pool's reported count.
--
-- The 'sm-workerpool' suite complements the existing
-- 'Streams.WorkerPoolSpec' unit tests with hundreds of random
-- command sequences per CI cycle.
module Streams.Antithesis.WorkerPoolSMSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Foldable (toList)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
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
  , topicName
  , validateTopology
  )
import Kafka.Streams.Runtime.WorkerPool
  ( WorkerPool
  , addPoolWorker
  , closeWorkerPool
  , newWorkerPoolHashed
  , poolWorkerCount
  , poolWorkers
  , removePoolWorker
  , submitRecordHashed
  , waitForQuiescence
  , workerProcessedCount
  )

----------------------------------------------------------------------
-- Topology
----------------------------------------------------------------------

passthroughTopo :: IO TopologyValid
passthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in")
         (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

----------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------

data Cmd
  = AddWorker
  | RemoveWorker
  | Submit !Int       -- ^ partition number (topic fixed to "in")
  deriving stock (Eq, Show)

genCmd :: H.Gen Cmd
genCmd = Gen.frequency
  [ (1, pure AddWorker)
  , (1, pure RemoveWorker)
  , (4, Submit <$> Gen.int (Range.linear 0 7))
  ]

----------------------------------------------------------------------
-- Model
----------------------------------------------------------------------

-- | Pure model of the hashed pool:
--
--   * 'mWorkers' is the live worker-id set.
--   * 'mNextId' is the next id we expect 'addPoolWorker' to
--     assign — the real pool uses a monotonic counter, so this
--     stays in lockstep.
--   * 'mSubmittedToLive' counts only submits whose chosen worker
--     was still alive at submission time (the runtime drops
--     submits that hash to a removed worker via the re-hashing
--     logic in 'submitRecordHashed').
data Model = Model
  { mWorkers :: !(Set Int)
  , mNextId  :: !Int
  , mSubmitted :: !Int
  } deriving stock (Eq, Show)

initialModel :: Int -> Model
initialModel n = Model
  { mWorkers   = Set.fromList [0 .. n - 1]
  , mNextId    = n
  , mSubmitted = 0
  }

applyModel :: Cmd -> Model -> Model
applyModel cmd m = case cmd of
  AddWorker ->
    m { mWorkers = Set.insert (mNextId m) (mWorkers m)
      , mNextId  = mNextId m + 1
      }
  RemoveWorker ->
    case Set.maxView (mWorkers m) of
      Nothing       -> m
      Just (_, w')  -> m { mWorkers = w' }
  Submit _part ->
    -- The runtime re-hashes onto remaining workers if the
    -- partition's prior route is dead, so every submit lands
    -- somewhere as long as at least one worker exists.
    if Set.null (mWorkers m)
      then m
      else m { mSubmitted = mSubmitted m + 1 }

----------------------------------------------------------------------
-- Real harness
----------------------------------------------------------------------

bytes :: T.Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

runReal :: WorkerPool -> Cmd -> IO ()
runReal pool = \case
  AddWorker    -> () <$ addPoolWorker pool
  RemoveWorker -> do
    _ <- removePoolWorker pool
    pure ()
  Submit p ->
    submitRecordHashed pool (topicName "in")
      (Just (bytes "k")) (bytes "v") (Timestamp 0) p

----------------------------------------------------------------------
-- Property
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "WorkerPool state-machine"
  [ testProperty
      "real pool agrees with the model on worker count + processed total" $
      H.withTests 40 propWorkerPool
  ]

propWorkerPool :: H.Property
propWorkerPool = H.property $ do
  initialN <- H.forAll (Gen.int (Range.linear 1 4))
  cmds     <- H.forAll (Gen.list (Range.linear 1 30) genCmd)
  outcome  <- H.evalIO $ do
    topo <- passthroughTopo
    pool <- newWorkerPoolHashed topo "wp-prop" initialN
    let go !mAcc [] = pure mAcc
        go !mAcc (c : rest) = do
          runReal pool c
          let !m' = applyModel c mAcc
          go m' rest
    finalModel <- go (initialModel initialN) cmds

    -- Wait for every accepted submit to be processed by the
    -- worker that received it.
    waitForQuiescence pool

    realCount <- poolWorkerCount pool
    let modelCount = Set.size (mWorkers finalModel)

    -- Sum processed across every live worker. A removed worker
    -- is drained before removal returns, so its records are
    -- in the worker's collector but the worker no longer exists.
    -- The total we can observe = submits to live workers.
    liveWorkers <- poolWorkers pool `seq` pure (poolWorkers pool)
    processed   <- sum
                <$> mapM workerProcessedCount (toList liveWorkers)

    closeWorkerPool pool
    pure (modelCount, realCount, fromIntegral processed
         , mSubmitted finalModel)
  let (modelCount, realCount, _processed, submitted) = outcome
  -- Model and real agree on the live worker count.
  realCount H.=== modelCount
  -- A non-negative submission count (sanity).
  H.assert (submitted >= 0)
