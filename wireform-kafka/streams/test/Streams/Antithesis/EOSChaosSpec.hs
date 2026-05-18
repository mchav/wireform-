{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streams.Antithesis.EOSChaosSpec
-- Description : Chaos / fault-injection property for the EOSCoordinator
--
-- The EOS-v2 commit cycle in 'Kafka.Streams.Runtime.EOS.runCommitCycle'
-- is a six-step protocol:
--
-- @
-- beginTxn → flushBody → commitOffsets → commitTxn → storeCommit
-- @
--
-- (plus 'abortTxn' + 'storeAbort' on the recovery path).
--
-- This test injects a fault at every step independently and asserts
-- the resulting 'CommitOutcome' matches a pure state-machine model
-- of the cycle, plus that the abort callbacks fire on every
-- recoverable failure and only on those.
module Streams.Antithesis.EOSChaosSpec (tests) where

import Control.Exception (throwIO, ErrorCall (..))
import Data.IORef
  ( atomicModifyIORef'
  , newIORef
  , readIORef
  )
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Kafka.Streams.Runtime.EOS
  ( CommitOutcome (..)
  , EOSCoordinator (..)
  , runCommitCycle
  )

import qualified Hedgehog.Range as Range

----------------------------------------------------------------------
-- Step outcomes
----------------------------------------------------------------------

data StepOutcome
  = StepOK
  | StepLeft       -- ^ callback returns @Left ...@
  | StepThrow      -- ^ flush body throws (only valid for flush step)
  deriving stock (Eq, Show)

genResult :: H.Gen StepOutcome
genResult = Gen.element [StepOK, StepLeft]

genFlushResult :: H.Gen StepOutcome
genFlushResult = Gen.element [StepOK, StepThrow]

-- | One schedule = the outcome for each of the five callback
-- decision points in 'runCommitCycle'. 'flushBody' is special: it
-- can additionally throw, and that's the only place a thrown
-- sync exception is caught.
data Schedule = Schedule
  { schBegin         :: !StepOutcome
  , schFlush         :: !StepOutcome
  , schCommitOffsets :: !StepOutcome
  , schCommitTxn     :: !StepOutcome
  , schStoreCommit   :: !StepOutcome
  } deriving stock (Eq, Show)

genSchedule :: H.Gen Schedule
genSchedule = do
  -- "StepLeft" represents Left; for the flush step we also
  -- allow throwing.
  Schedule
    <$> genResult
    <*> genFlushResult
    <*> genResult
    <*> genResult
    <*> genResult

----------------------------------------------------------------------
-- Pure model
----------------------------------------------------------------------

-- | The model is a direct translation of 'runCommitCycle'. Each
-- predicted outcome must come with the matching trace of
-- coordinator-side callbacks the cycle should perform.
--
-- Note: the flush body is the caller's responsibility (it's not a
-- coordinator callback), so 'TFlush' is /not/ part of the trace
-- we model — the trace records what 'runCommitCycle' calls /on
-- the coordinator/, which is exactly what an external EOS
-- harness can observe.
data CallTag
  = TBegin
  | TCommitOffsets
  | TCommitTxn
  | TStoreCommit
  | TAbortTxn
  | TStoreAbort
  deriving stock (Eq, Show)

-- | Predict the outcome + trace for a 'Schedule'.
predict :: Schedule -> (CommitOutcome, [CallTag])
predict s = case schBegin s of
  StepLeft -> (CommitFatal "begin: left", [TBegin])
  StepThrow -> error "predict: begin doesn't throw"
  StepOK    -> case schFlush s of
    StepThrow -> ( CommitAborted "flush: <synthetic>"
                 , [TBegin, TAbortTxn, TStoreAbort])
    StepLeft  -> error "predict: flush only uses OK or Throw"
    StepOK    -> case schCommitOffsets s of
      StepLeft  ->
        ( CommitAborted "commitOffsets: left"
        , [TBegin, TCommitOffsets, TAbortTxn, TStoreAbort])
      StepThrow -> error "predict: commitOffsets doesn't throw"
      StepOK    -> case schCommitTxn s of
        StepLeft  ->
          ( CommitAborted "commit: left"
          , [TBegin, TCommitOffsets, TCommitTxn, TAbortTxn, TStoreAbort])
        StepThrow -> error "predict: commitTxn doesn't throw"
        StepOK    -> case schStoreCommit s of
          StepLeft  ->
            ( CommitFatal "storeCommit: left"
            , [TBegin, TCommitOffsets, TCommitTxn, TStoreCommit])
          StepThrow -> error "predict: storeCommit doesn't throw"
          StepOK    ->
            ( CommitSucceeded
            , [TBegin, TCommitOffsets, TCommitTxn, TStoreCommit])

----------------------------------------------------------------------
-- Instrumented coordinator
----------------------------------------------------------------------

-- | Build an 'EOSCoordinator' that records every callback it
-- receives plus the side-channel 'Schedule' decisions.
mkCoord :: Schedule -> IO (EOSCoordinator, IO [CallTag])
mkCoord s = do
  traceRef <- newIORef ([] :: [CallTag])
  let bump tag = atomicModifyIORef' traceRef (\xs -> (tag : xs, ()))
      stepEither tag out = do
        bump tag
        case out of
          StepOK    -> pure (Right ())
          StepLeft  -> pure (Left "left")
          StepThrow ->
            error "step: stepEither cannot represent Throw — flushBody special-cased"
  let coord = EOSCoordinator
        { initTxn       = pure (Right ())
        , beginTxn      = stepEither TBegin (schBegin s)
        , commitTxn     = stepEither TCommitTxn (schCommitTxn s)
        , abortTxn      = bump TAbortTxn >> pure (Right ())
        , commitOffsets = \_gid _offs ->
            stepEither TCommitOffsets (schCommitOffsets s)
        , storeCommit   = stepEither TStoreCommit (schStoreCommit s)
        , storeAbort    = bump TStoreAbort >> pure (Right ())
        }
      readTrace = reverse <$> readIORef traceRef
  pure (coord, readTrace)

----------------------------------------------------------------------
-- Property
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "EOS chaos"
  [ testProperty
      "runCommitCycle outcome + trace matches the state-machine model" $
      H.withTests 200 propMatchesModel
  , testProperty
      "getOffsets throwing aborts cleanly (never escapes runCommitCycle)"
      $ H.withTests 100 propGetOffsetsThrows
  , testProperty
      "abortTxn returning Left does not destabilise the cycle outcome"
      $ H.withTests 100 propAbortTxnLeftIsTolerated
  , testProperty
      "storeAbort returning Left does not destabilise the cycle outcome"
      $ H.withTests 80 propStoreAbortLeftIsTolerated
  ]

propMatchesModel :: H.Property
propMatchesModel = H.property $ do
        sched <- H.forAll genSchedule
        let (expectedOutcome, expectedTrace) = predict sched
        -- For 'flushBody' we wrap a separate body that respects
        -- the schedule's flush decision.
        let flushBody = case schFlush sched of
              StepOK    -> pure ()
              StepLeft  -> error "predict cannot get here"
              StepThrow -> throwIO (ErrorCall "<synthetic>")
        (observedOutcome, trace) <- H.evalIO $ do
          (coord, readTrace) <- mkCoord sched
          outcome <- runCommitCycle coord
                       "group" (pure HM.empty) flushBody
          tr <- readTrace
          pure (outcome, tr)
        H.annotate ("schedule: " <> show sched)
        H.annotate ("expected outcome: " <> show expectedOutcome)
        H.annotate ("actual outcome:   " <> show observedOutcome)
        H.annotate ("expected trace:   " <> show expectedTrace)
        H.annotate ("actual trace:     " <> show trace)
        -- The actual outcome string varies in detail (exception
        -- show), so we compare on the outcome /tag/ + matching
        -- prefix of the reason.
        outcomesMatch expectedOutcome observedOutcome
        trace H.=== expectedTrace

----------------------------------------------------------------------
-- Property: getOffsets throwing aborts cleanly
----------------------------------------------------------------------

-- | 'getOffsets' is supplied by the engine; in production a closed
-- consumer, in-flight rebalance, or metadata failure can make it
-- throw. The cycle must catch the exception, run the usual abort
-- path, and return 'CommitAborted' instead of letting the
-- exception escape and tear the engine down.
propGetOffsetsThrows :: H.Property
propGetOffsetsThrows = H.property $ do
  -- Drive a tiny schedule variation just so Hedgehog shrinks
  -- something. The coordinator is fixed to "everything before
  -- getOffsets succeeds, everything after must never fire".
  _ <- H.forAll (Gen.int (Range.linear 0 0))
  outcome <- H.evalIO $ do
    traceRef <- newIORef ([] :: [CallTag])
    let bump tag = atomicModifyIORef' traceRef (\xs -> (tag : xs, ()))
        coord = EOSCoordinator
          { initTxn       = pure (Right ())
          , beginTxn      = bump TBegin >> pure (Right ())
          , commitTxn     = bump TCommitTxn >> pure (Right ())
          , abortTxn      = bump TAbortTxn >> pure (Right ())
          , commitOffsets = \_ _ -> do
              bump TCommitOffsets
              pure (Right ())
          , storeCommit   = bump TStoreCommit >> pure (Right ())
          , storeAbort    = bump TStoreAbort >> pure (Right ())
          }
        oops = throwIO (ErrorCall "<getOffsets boom>")
    o <- runCommitCycle coord "group" oops (pure ())
    tr <- reverse <$> readIORef traceRef
    pure (o, tr)
  let (o, tr) = outcome
  H.annotate ("outcome: " <> show o)
  H.annotate ("trace:   " <> show tr)
  case o of
    CommitAborted reason
      | "getOffsets" `T.isPrefixOf` reason -> pure ()
      | otherwise -> do
          H.annotate "expected CommitAborted prefixed with `getOffsets`"
          H.failure
    _ -> H.failure
  -- The cycle reached the abort path, which calls both abortTxn
  -- and storeAbort. 'commitOffsets' / 'commitTxn' / 'storeCommit'
  -- must NOT have fired.
  tr H.=== [TBegin, TAbortTxn, TStoreAbort]

----------------------------------------------------------------------
-- Property: abortTxn returning Left is tolerated
----------------------------------------------------------------------

-- | The current 'runCommitCycle' contract: 'doAbort' fires both
-- 'abortTxn' and 'storeAbort' and discards their return values.
-- We assert that a 'Left' from 'abortTxn' does not change the
-- outcome of a flush failure — i.e. a recovery callback failing
-- doesn't promote the cycle from 'CommitAborted' to 'CommitFatal'.
propAbortTxnLeftIsTolerated :: H.Property
propAbortTxnLeftIsTolerated = H.property $ do
  -- We trigger abort via a flush exception; the schedule's
  -- post-flush slots are irrelevant for this case.
  rest <- H.forAll genSchedule
  outcome <- H.evalIO $ do
    traceRef <- newIORef ([] :: [CallTag])
    let bump tag = atomicModifyIORef' traceRef (\xs -> (tag : xs, ()))
        coord = EOSCoordinator
          { initTxn       = pure (Right ())
          , beginTxn      = bump TBegin >> pure (Right ())
          , commitTxn     = bump TCommitTxn >> pure (Right ())
          , abortTxn      = do
              bump TAbortTxn
              pure (Left "abort-failed")
          , commitOffsets = \_ _ -> do
              bump TCommitOffsets
              pure (Right ())
          , storeCommit   = bump TStoreCommit >> pure (Right ())
          , storeAbort    = bump TStoreAbort >> pure (Right ())
          }
    o <- runCommitCycle coord "g" (pure HM.empty)
           (throwIO (ErrorCall "<flush boom>"))
    tr <- reverse <$> readIORef traceRef
    pure (rest, o, tr)
  let (_, o, tr) = outcome
  H.annotate ("outcome: " <> show o)
  H.annotate ("trace:   " <> show tr)
  case o of
    CommitAborted r
      | "flush" `T.isPrefixOf` r -> pure ()
      | otherwise -> do
          H.annotate "expected CommitAborted prefixed with `flush`"
          H.failure
    _ -> H.failure
  -- abortTxn + storeAbort fire even when abortTxn fails.
  tr H.=== [TBegin, TAbortTxn, TStoreAbort]

----------------------------------------------------------------------
-- Property: storeAbort returning Left is tolerated
----------------------------------------------------------------------

propStoreAbortLeftIsTolerated :: H.Property
propStoreAbortLeftIsTolerated = H.property $ do
  _ <- H.forAll (Gen.int (Range.linear 0 0))
  outcome <- H.evalIO $ do
    traceRef <- newIORef ([] :: [CallTag])
    let bump tag = atomicModifyIORef' traceRef (\xs -> (tag : xs, ()))
        coord = EOSCoordinator
          { initTxn       = pure (Right ())
          , beginTxn      = bump TBegin >> pure (Right ())
          , commitTxn     = bump TCommitTxn >> pure (Right ())
          , abortTxn      = bump TAbortTxn >> pure (Right ())
          , commitOffsets = \_ _ -> do
              bump TCommitOffsets
              pure (Right ())
          , storeCommit   = bump TStoreCommit >> pure (Right ())
          , storeAbort    = do
              bump TStoreAbort
              pure (Left "store-abort-failed")
          }
    o <- runCommitCycle coord "g" (pure HM.empty)
           (throwIO (ErrorCall "<flush boom>"))
    tr <- reverse <$> readIORef traceRef
    pure (o, tr)
  let (o, tr) = outcome
  H.annotate ("outcome: " <> show o)
  H.annotate ("trace:   " <> show tr)
  case o of
    CommitAborted r
      | "flush" `T.isPrefixOf` r -> pure ()
      | otherwise -> do
          H.annotate "expected CommitAborted prefixed with `flush`"
          H.failure
    _ -> H.failure
  tr H.=== [TBegin, TAbortTxn, TStoreAbort]

outcomesMatch :: CommitOutcome -> CommitOutcome -> H.PropertyT IO ()
outcomesMatch e a = case (e, a) of
  (CommitSucceeded, CommitSucceeded) -> pure ()
  (CommitAborted re, CommitAborted ra)
    | prefix re ra -> pure ()
    | otherwise    -> do
        H.annotate ("aborted reason prefix mismatch: expected `"
                     <> T.unpack re <> "`, got `" <> T.unpack ra <> "`")
        H.failure
  (CommitFatal re, CommitFatal ra)
    | prefix re ra -> pure ()
    | otherwise    -> do
        H.annotate ("fatal reason prefix mismatch: expected `"
                     <> T.unpack re <> "`, got `" <> T.unpack ra <> "`")
        H.failure
  _ -> H.failure
  where
    prefix pe pa = T.isPrefixOf (T.takeWhile (/= ' ') pe) pa
                || T.takeWhile (/= ' ') pe == T.takeWhile (/= ' ') pa

