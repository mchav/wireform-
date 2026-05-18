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

import Control.Concurrent.MVar
import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , retry
  )
import qualified Control.Exception as Exception
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

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
