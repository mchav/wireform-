{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the Riffle async-I\/O operator family
-- ('Kafka.Streams.AsyncIO'). Exercised through the in-process
-- 'TopologyTestDriver' against the imperative-DSL smart constructors;
-- the AST-level @Prim@ wiring is tested separately in
-- 'Streams.TopologyFreeSpec' once it lands.
--
-- Coordination is via STM TVars — never 'threadDelay' — so the
-- tests are deterministic against the bounded worker pool.
module Streams.AsyncIOSpec (tests) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
  ( atomically
  , isEmptyTMVar
  , modifyTVar'
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTVar
  , retry
  , takeTMVar
  )
import qualified Control.Category as Cat
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import qualified Kafka.Streams.Topology.Free as F

import Kafka.Streams
import Kafka.Streams.AsyncIO
  ( AsyncDrainTrigger (..)
  , AsyncFailurePolicy (..)
  , AsyncIOConfig (..)
  , AsyncOutputMode (..)
  , asyncMapValues
  , defaultAsyncIOConfig
  )
import qualified Kafka.Streams.Time as Time

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

t0 :: Timestamp
t0 = Timestamp 0

----------------------------------------------------------------------
-- Test harness
----------------------------------------------------------------------

-- | Build a topology @in -> asyncMapValues cfg f -> out@ wired with
-- text serdes. Returns the driver plus a @waitForDeposits :: Int -> IO ()@
-- coordination helper. The helper uses an 'aioOnDeposit'-bumped
-- 'TVar' so the test can wait for /durable/ deposits without
-- racing the user IO.
buildAsyncDriver
  :: AsyncIOConfig
  -> (Text -> IO Text)
  -> IO (TopologyTestDriver, Int -> IO ())
buildAsyncDriver cfg0 f = do
  deposits <- newTVarIO (0 :: Int)
  let cfg = cfg0 { aioOnDeposit = atomically (modifyTVar' deposits (+ 1)) }
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  s' <- asyncMapValues cfg f s
  toTopic (topicName "out") (produced textSerde textSerde) s'
  topo <- buildTopology b
  driver <- newDriver topo "asyncio-test"
  let waitForDeposits n = atomically $ do
        c <- readTVar deposits
        if c >= n then pure () else retry
  pure (driver, waitForDeposits)

-- | Tight wall-clock advance that exceeds the default punctuator
-- interval. The drain-on-punctuator fires when the engine sees
-- the wall-clock cross the next-fire boundary.
fireDrainPunctuator :: TopologyTestDriver -> IO ()
fireDrainPunctuator d = advanceWallClockTime d 250

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "AsyncIO"
  [ ordered_preserves_input_order
  , drain_via_punctuator
  , drop_and_continue_skips_failure
  , log_and_continue_skips_failure
  , fail_task_surfaces_exception
  , unordered_emits_in_completion_order
  , topology_free_async_map_values_smoke
  , fusion_hoists_pure_into_async
  , commit_drains_in_flight_work
  , commit_waits_for_slow_user_io
  , commit_drains_unordered
  ]

----------------------------------------------------------------------
-- Ordered: 5 records, completes in input order
----------------------------------------------------------------------

ordered_preserves_input_order :: TestTree
ordered_preserves_input_order =
  testCase "ordered output preserves input order across the worker pool" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 8
          , aioWorkers        = 4
          , aioOutputMode     = OrderedOutput
          }
        f v = pure (T.toUpper v)
    (driver, waitFor) <- buildAsyncDriver cfg f

    let inputs = ["alpha", "bravo", "charlie", "delta", "echo"]
    mapM_ (\v -> pipeInput driver (topicName "in")
                  Nothing (bytes v) t0 0) inputs

    waitFor (length inputs)
    fireDrainPunctuator driver

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= map T.toUpper inputs
    closeDriver driver

----------------------------------------------------------------------
-- Drain via punctuator (no more input arrives)
----------------------------------------------------------------------

drain_via_punctuator :: TestTree
drain_via_punctuator =
  testCase "drain-on-punctuator emits completed work without further input" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntryAndPunctuator (Time.millis 25)
          }
        f v = pure (T.toUpper v)
    (driver, waitFor) <- buildAsyncDriver cfg f

    -- One input; without the punctuator fire, the single completed
    -- result would stay in the reorder buffer until the next
    -- pipeInput.
    pipeInput driver (topicName "in") Nothing (bytes "once") t0 0
    waitFor 1

    fireDrainPunctuator driver

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["ONCE"]
    closeDriver driver

----------------------------------------------------------------------
-- DropAndContinue: failing record is skipped silently
----------------------------------------------------------------------

drop_and_continue_skips_failure :: TestTree
drop_and_continue_skips_failure =
  testCase "DropAndContinue skips the failing slot, keeps the rest" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 8
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = DropAndContinue
          }
        f v
          | v == "boom" = Exception.throwIO (Exception.ErrorCall "synthetic")
          | otherwise   = pure (T.toUpper v)
    (driver, waitFor) <- buildAsyncDriver cfg f

    let inputs = ["a", "boom", "b", "c"]
    mapM_ (\v -> pipeInput driver (topicName "in")
                  Nothing (bytes v) t0 0) inputs
    waitFor (length inputs)

    fireDrainPunctuator driver

    out <- readOutput driver (topicName "out")
    -- "boom" drops, the surviving three preserve input order.
    map (unbytes . crValue) out @?= ["A", "B", "C"]
    closeDriver driver

----------------------------------------------------------------------
-- LogAndContinue: same as DropAndContinue but logged
----------------------------------------------------------------------

log_and_continue_skips_failure :: TestTree
log_and_continue_skips_failure =
  testCase "LogAndContinue behaves like DropAndContinue downstream" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = LogAndContinue
          }
        f v
          | v == "x"  = Exception.throwIO (Exception.ErrorCall "synthetic-log")
          | otherwise = pure (T.toUpper v)
    (driver, waitFor) <- buildAsyncDriver cfg f

    mapM_ (\v -> pipeInput driver (topicName "in")
                  Nothing (bytes v) t0 0) ["k", "x", "q"]
    waitFor 3
    fireDrainPunctuator driver

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["K", "Q"]
    closeDriver driver

----------------------------------------------------------------------
-- FailTask: exception surfaces on the next drain
----------------------------------------------------------------------

fail_task_surfaces_exception :: TestTree
fail_task_surfaces_exception =
  testCase "FailTask re-throws on the stream thread at the next drain" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = FailTask
          }
        f v = Exception.throwIO (Exception.ErrorCall ("fail:" <> T.unpack v))
    (driver, waitFor) <- buildAsyncDriver cfg f

    -- First pipeInput enqueues; its drain on entry has nothing to
    -- flush. The worker fails and sets the failure TVar (the
    -- failure path also fires the deposit hook).
    pipeInput driver (topicName "in") Nothing (bytes "die") t0 0
    waitFor 1

    -- The next drain (whether from a pipeInput or the punctuator)
    -- re-throws on the stream thread.
    result <- Exception.try (fireDrainPunctuator driver)
                :: IO (Either Exception.SomeException ())
    case result of
      Left _  -> pure ()
      Right _ ->
        assertBool "expected FailTask to re-throw on drain" False

    closeDriver driver

----------------------------------------------------------------------
-- Unordered: results emitted in completion order
----------------------------------------------------------------------

-- | Block each worker on a per-record MVar. The test signals them
-- in reverse to force out-of-order completion, then verifies the
-- output ordering matches the signal order.
unordered_emits_in_completion_order :: TestTree
unordered_emits_in_completion_order =
  testCase "unordered output emits in completion order" $ do
    gateA <- newEmptyMVar
    gateB <- newEmptyMVar
    gateC <- newEmptyMVar

    let pickGate "a" = gateA
        pickGate "b" = gateB
        pickGate "c" = gateC
        pickGate _   = error "unexpected key"

        cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 8
          , aioWorkers        = 3  -- one worker per record
          , aioOutputMode     = UnorderedOutput
          }
        f v = do
          takeMVar (pickGate v)
          pure (T.toUpper v)

    (driver, waitFor) <- buildAsyncDriver cfg f

    mapM_ (\v -> pipeInput driver (topicName "in")
                  Nothing (bytes v) t0 0) ["a", "b", "c"]

    -- Release in c, a, b order. The deposit hook observes the
    -- /durable/ enqueue onto the unordered queue, so by the
    -- time waitFor returns we know the deposit landed before
    -- the next gate release.
    putMVar gateC ()
    waitFor 1
    putMVar gateA ()
    waitFor 2
    putMVar gateB ()
    waitFor 3

    fireDrainPunctuator driver

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["C", "A", "B"]
    closeDriver driver

----------------------------------------------------------------------
-- Topology.Free AST: AsyncMapValues Prim compiles + runs
----------------------------------------------------------------------

topology_free_async_map_values_smoke :: TestTree
topology_free_async_map_values_smoke =
  testCase "Topology.Free asyncMapValues compiles and forwards" $ do
    deposits <- newTVarIO (0 :: Int)
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioOnDeposit      = atomically (modifyTVar' deposits (+ 1))
          }
        topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            Cat.>>> F.asyncMapValues cfg pure
            Cat.>>> F.mapValues T.toUpper
            Cat.>>> F.sink "out" textSerde textSerde

    (_, topo) <- F.compile topology
    driver <- newDriver topo "asyncio-free-smoke"

    mapM_ (\v -> pipeInput driver (topicName "in")
                   Nothing (bytes v) t0 0) ["x", "y", "z"]
    atomically $ do
      n <- readTVar deposits
      if n >= 3 then pure () else retry
    advanceWallClockTime driver 250

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["X", "Y", "Z"]
    closeDriver driver

----------------------------------------------------------------------
-- AST fusion: a pure MapValues feeding an AsyncMapValues collapses
-- into a single AsyncMapValues with the composed function.
----------------------------------------------------------------------

fusion_hoists_pure_into_async :: TestTree
fusion_hoists_pure_into_async =
  testCase "optFuseSyncIntoAsync collapses MapValues >>> AsyncMapValues" $ do
    deposits <- newTVarIO (0 :: Int)
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioOnDeposit      = atomically (modifyTVar' deposits (+ 1))
          }
        topology :: F.Topology Void ()
        topology =
          F.source "in" textSerde textSerde
            Cat.>>> F.mapValues T.toUpper          -- pure prefix
            Cat.>>> F.asyncMapValues cfg pure       -- async tail
            Cat.>>> F.sink "out" textSerde textSerde

    -- With the rule enabled the optimiser should drop one node
    -- (the pure 'MapValues' folds into the async). We assert on
    -- the node-count delta to make the fusion observable
    -- without parsing the AST structure.
    let optimised   = F.optimize topology
        unoptimised = F.optimizeWith F.noOptimization topology
        nFused      = F.countNodes optimised
        nRaw        = F.countNodes unoptimised
    assertBool
      ("expected fused topology to have fewer nodes; raw="
         <> show nRaw <> " fused=" <> show nFused)
      (nFused < nRaw)

    -- And the fused topology still runs to the same output.
    (_, topo) <- F.compile topology
    driver <- newDriver topo "asyncio-fusion"
    mapM_ (\v -> pipeInput driver (topicName "in")
                  Nothing (bytes v) t0 0) ["alpha", "bravo"]
    atomically $ do
      n <- readTVar deposits
      if n >= 2 then pure () else retry
    advanceWallClockTime driver 250
    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["ALPHA", "BRAVO"]
    closeDriver driver

----------------------------------------------------------------------
-- EOS pre-commit drain: commitDriver drains in-flight work before
-- it returns, without relying on the wall-clock punctuator.
----------------------------------------------------------------------

commit_drains_in_flight_work :: TestTree
commit_drains_in_flight_work =
  testCase "commitDriver drains async work even with no punctuator" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
            -- ^ Critical: no punctuator. The only mechanism that
            -- can drain the reorder buffer is the pre-commit hook
            -- the engine fires from commitEngine.
          }
        f v = pure (T.toUpper v)
    (driver, _waitFor) <- buildAsyncDriver cfg f

    let inputs = ["alpha", "bravo", "charlie"]
    mapM_ (\v -> pipeInput driver (topicName "in")
                  Nothing (bytes v) t0 0) inputs

    -- Don't advance wall clock. commitDriver MUST drain on its
    -- own via the engine's drainPreCommit hook.
    commitDriver driver

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= map T.toUpper inputs
    closeDriver driver

-- | The user IO blocks until the test releases its MVar. Without
-- the EOS drain, commitDriver would return immediately and the
-- post-commit readOutput would miss records that the worker pool
-- hasn't deposited yet. With the drain, commitDriver blocks
-- until every in-flight result is deposited and forwarded.
--
-- Coordination is fully STM: a @started@ counter bumps /before/
-- each worker blocks on its gate, so we can wait until both
-- workers are parked before we schedule the background commit.
-- That removes any race between the test thread and the runtime
-- scheduler.
commit_waits_for_slow_user_io :: TestTree
commit_waits_for_slow_user_io =
  testCase "commitDriver blocks until slow async IO has completed" $ do
    gateA   <- newEmptyMVar
    gateB   <- newEmptyMVar
    started <- newTVarIO (0 :: Int)
    let pickGate "a" = gateA
        pickGate "b" = gateB
        pickGate _   = error "unexpected input"
        cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = do
          atomically (modifyTVar' started (+ 1))
          takeMVar (pickGate v)
          pure (T.toUpper v)
    (driver, _waitFor) <- buildAsyncDriver cfg f

    pipeInput driver (topicName "in") Nothing (bytes "a") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "b") t0 0

    -- Wait until both workers are parked on their gate. After
    -- this barrier we know: submitted == 2, deposited == 0, two
    -- workers are blocked. The drain MUST park.
    atomically $ do
      s <- readTVar started
      if s == 2 then pure () else retry

    -- Run commitDriver in the background; it must block until
    -- both gates are released.
    commitDone <- newEmptyTMVarIO
    _ <- Async.async $ do
      commitDriver driver
      atomically (putTMVar commitDone ())

    -- Confirm via STM that the commit hasn't completed: with
    -- both workers parked, the EOS drain has to be parked too.
    -- If the drain were skipped, commitDriver would return and
    -- this snapshot would observe a filled TMVar.
    notDoneYet <- atomically (isEmptyTMVar commitDone)
    assertBool
      "expected commitDriver to be blocked on in-flight IO"
      notDoneYet

    -- Releasing the gates lets the workers complete; commit
    -- should now finish and forward both records.
    putMVar gateA ()
    putMVar gateB ()
    atomically (takeTMVar commitDone)

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["A", "B"]
    closeDriver driver

-- | UnorderedOutput: the drain forwards the completed unordered
-- queue regardless of input ordering.
commit_drains_unordered :: TestTree
commit_drains_unordered =
  testCase "commit drains UnorderedOutput too" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = UnorderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = pure (T.toUpper v)
    (driver, _waitFor) <- buildAsyncDriver cfg f

    let inputs = ["x", "y", "z"]
    mapM_ (\v -> pipeInput driver (topicName "in")
                  Nothing (bytes v) t0 0) inputs

    commitDriver driver

    out <- readOutput driver (topicName "out")
    -- Unordered: assert membership, not order. The drain
    -- guarantees all submitted records show up.
    map (unbytes . crValue) out
      `shouldContainAll` ["X", "Y", "Z"]
    closeDriver driver
  where
    shouldContainAll got expected =
      assertBool
        ("expected " <> show expected
           <> " to be a permutation of " <> show got)
        (length got == length expected
         && all (`elem` got) expected)
