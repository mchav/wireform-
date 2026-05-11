{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.EOSSpec (tests) where

import Data.IORef
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Runtime.EOS

-- | A coordinator that records each call into an 'IORef'.
recordingCoordinator
  :: IO (EOSCoordinator, IO [Text])
recordingCoordinator = do
  buf <- newIORef ([] :: [Text])
  let log_ s = modifyIORef' buf (s :)
      coord = EOSCoordinator
        { eosInit          = log_ "init"   *> pure (Right ())
        , eosBegin         = log_ "begin"  *> pure (Right ())
        , eosCommit        = log_ "commit" *> pure (Right ())
        , eosAbort         = log_ "abort"  *> pure (Right ())
        , eosCommitOffsets = \_ _ ->
            log_ "commitOffsets" *> pure (Right ())
        }
  pure (coord, reverse <$> readIORef buf)

-- | A coordinator that fails at a chosen step.
failingAt :: Text -> IO (EOSCoordinator, IO [Text])
failingAt failStep = do
  buf <- newIORef ([] :: [Text])
  let log_ s = modifyIORef' buf (s :)
      step name action
        | name == failStep = log_ name *> pure (Left ("forced-fail-" <> name))
        | otherwise        = log_ name *> action
      coord = EOSCoordinator
        { eosInit  = step "init" (pure (Right ()))
        , eosBegin = step "begin" (pure (Right ()))
        , eosCommit = step "commit" (pure (Right ()))
        , eosAbort = step "abort" (pure (Right ()))
        , eosCommitOffsets = \_ _ ->
            step "commitOffsets" (pure (Right ()))
        }
  pure (coord, reverse <$> readIORef buf)

tests :: TestTree
tests = testGroup "EOS"
  [ eos_happy_path_order
  , eos_flush_failure_aborts
  , eos_commit_offsets_failure_aborts
  , eos_begin_failure_is_fatal
  , eos_noop_returns_succeeded
  ]

eos_happy_path_order :: TestTree
eos_happy_path_order =
  testCase "happy-path commit cycle: begin → flush → commitOffsets → commit" $ do
    (coord, drain) <- recordingCoordinator
    flushed <- newIORef False
    let getOffsets = pure Map.empty
        flushBody  = writeIORef flushed True
    outcome <- runCommitCycle coord "g" getOffsets flushBody
    outcome @?= CommitSucceeded
    flushFlag <- readIORef flushed
    flushFlag @?= True
    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "commit"]

eos_flush_failure_aborts :: TestTree
eos_flush_failure_aborts =
  testCase "exception during flush -> abort, return CommitAborted" $ do
    (coord, drain) <- recordingCoordinator
    let flushBody = error "boom"
    outcome <- runCommitCycle coord "g" (pure Map.empty) flushBody
    case outcome of
      CommitAborted msg -> assertContains "boom" msg
      _                  -> error ("unexpected outcome: " <> show outcome)
    log_ <- drain
    log_ @?= ["begin", "abort"]

eos_commit_offsets_failure_aborts :: TestTree
eos_commit_offsets_failure_aborts =
  testCase "commitOffsets fail -> abort" $ do
    (coord, drain) <- failingAt "commitOffsets"
    outcome <- runCommitCycle coord "g" (pure Map.empty) (pure ())
    case outcome of
      CommitAborted msg -> assertContains "forced-fail-commitOffsets" msg
      _                  -> error ("unexpected outcome: " <> show outcome)
    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "abort"]

eos_begin_failure_is_fatal :: TestTree
eos_begin_failure_is_fatal =
  testCase "begin fail -> CommitFatal, abort NOT called (we never began)" $ do
    (coord, drain) <- failingAt "begin"
    outcome <- runCommitCycle coord "g" (pure Map.empty) (pure ())
    case outcome of
      CommitFatal msg -> assertContains "forced-fail-begin" msg
      _                -> error ("unexpected outcome: " <> show outcome)
    log_ <- drain
    log_ @?= ["begin"]

eos_noop_returns_succeeded :: TestTree
eos_noop_returns_succeeded =
  testCase "noopEOSCoordinator: every commit succeeds with no protocol traffic" $ do
    outcome <- runCommitCycle noopEOSCoordinator "g" (pure Map.empty) (pure ())
    outcome @?= CommitSucceeded

assertContains :: Text -> Text -> IO ()
assertContains needle hay
  | T.isInfixOf needle hay = pure ()
  | otherwise = error
      ("expected " <> show needle <> " in " <> show hay)