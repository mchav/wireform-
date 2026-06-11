{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.EOSSpec (tests) where

import Data.HashMap.Strict qualified as Map
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Runtime.EOS
import Kafka.Streams.State.KeyValue.InMemory qualified as Mem
import Kafka.Streams.State.Store qualified as Store
import Kafka.Streams.State.Transactional qualified as TX
import Test.Syd


-- | A coordinator that records each call into an 'IORef'.
recordingCoordinator
  :: IO (EOSCoordinator, IO [Text])
recordingCoordinator = do
  buf <- newIORef ([] :: [Text])
  let log_ s = modifyIORef' buf (s :)
      coord =
        EOSCoordinator
          { initTxn = log_ "init" *> pure (Right ())
          , beginTxn = log_ "begin" *> pure (Right ())
          , commitTxn = log_ "commit" *> pure (Right ())
          , abortTxn = log_ "abort" *> pure (Right ())
          , commitOffsets = \_ _ ->
              log_ "commitOffsets" *> pure (Right ())
          , storeCommit = log_ "storeCommit" *> pure (Right ())
          , storeAbort = log_ "storeAbort" *> pure (Right ())
          , preCommit2PC = pure (Right ())
          , commit2PC = pure (Right ())
          , abort2PC = pure ()
          }
  pure (coord, reverse <$> readIORef buf)


-- | A coordinator that fails at a chosen step.
failingAt :: Text -> IO (EOSCoordinator, IO [Text])
failingAt failStep = do
  buf <- newIORef ([] :: [Text])
  let log_ s = modifyIORef' buf (s :)
      step name action
        | name == failStep = log_ name *> pure (Left ("forced-fail-" <> name))
        | otherwise = log_ name *> action
      coord =
        EOSCoordinator
          { initTxn = step "init" (pure (Right ()))
          , beginTxn = step "begin" (pure (Right ()))
          , commitTxn = step "commit" (pure (Right ()))
          , abortTxn = step "abort" (pure (Right ()))
          , commitOffsets = \_ _ ->
              step "commitOffsets" (pure (Right ()))
          , storeCommit = step "storeCommit" (pure (Right ()))
          , storeAbort = step "storeAbort" (pure (Right ()))
          , preCommit2PC = pure (Right ())
          , commit2PC = pure (Right ())
          , abort2PC = pure ()
          }
  pure (coord, reverse <$> readIORef buf)


tests :: Spec
tests =
  describe "EOS" $
    sequence_
      [ eos_happy_path_order
      , eos_flush_failure_aborts
      , eos_commit_offsets_failure_aborts
      , eos_begin_failure_is_fatal
      , eos_noop_returns_succeeded
      , eos_transactional_stores_drain_on_commit
      , eos_transactional_stores_revert_on_abort
      ]


eos_happy_path_order :: Spec
eos_happy_path_order =
  it "happy-path commit cycle: begin → flush → commitOffsets → commit" $ do
    (coord, drain) <- recordingCoordinator
    flushed <- newIORef False
    let getOffsets = pure Map.empty
        flushBody = writeIORef flushed True
    outcome <- runCommitCycle coord "g" getOffsets flushBody
    outcome `shouldBe` CommitSucceeded
    flushFlag <- readIORef flushed
    flushFlag `shouldBe` True
    log_ <- drain
    log_ `shouldBe` ["begin", "commitOffsets", "commit", "storeCommit"]


eos_flush_failure_aborts :: Spec
eos_flush_failure_aborts =
  it "exception during flush -> abort, return CommitAborted" $ do
    (coord, drain) <- recordingCoordinator
    let flushBody = error "boom"
    outcome <- runCommitCycle coord "g" (pure Map.empty) flushBody
    case outcome of
      CommitAborted msg -> assertContains "boom" msg
      _ -> error ("unexpected outcome: " <> show outcome)
    log_ <- drain
    log_ `shouldBe` ["begin", "abort", "storeAbort"]


eos_commit_offsets_failure_aborts :: Spec
eos_commit_offsets_failure_aborts =
  it "commitOffsets fail -> abort" $ do
    (coord, drain) <- failingAt "commitOffsets"
    outcome <- runCommitCycle coord "g" (pure Map.empty) (pure ())
    case outcome of
      CommitAborted msg -> assertContains "forced-fail-commitOffsets" msg
      _ -> error ("unexpected outcome: " <> show outcome)
    log_ <- drain
    log_ `shouldBe` ["begin", "commitOffsets", "abort", "storeAbort"]


eos_begin_failure_is_fatal :: Spec
eos_begin_failure_is_fatal =
  it "begin fail -> CommitFatal, abort NOT called (we never began)" $ do
    (coord, drain) <- failingAt "begin"
    outcome <- runCommitCycle coord "g" (pure Map.empty) (pure ())
    case outcome of
      CommitFatal msg -> assertContains "forced-fail-begin" msg
      _ -> error ("unexpected outcome: " <> show outcome)
    log_ <- drain
    log_ `shouldBe` ["begin"]


eos_noop_returns_succeeded :: Spec
eos_noop_returns_succeeded =
  it "noopEOSCoordinator: every commit succeeds with no protocol traffic" $ do
    outcome <- runCommitCycle noopEOSCoordinator "g" (pure Map.empty) (pure ())
    outcome `shouldBe` CommitSucceeded


{- | KIP-892: when the producer commit succeeds, every
'TransactionalStore' that was wired in via
'withTransactionalStores' gets its buffer drained onto the
underlying store. Verifies the order: producer commit
FIRST, store commit SECOND.
-}
eos_transactional_stores_drain_on_commit :: Spec
eos_transactional_stores_drain_on_commit =
  it "withTransactionalStores: producer commit -> store commit, in order" $ do
    underlying <-
      Mem.inMemoryKeyValueStore @Text @Text
        (Store.storeName "x")
    ts <- TX.newTransactionalStore underlying
    let kvs = TX.txnStore ts
    Store.kvsPut kvs "k" "v"
    -- Pre-commit: underlying is empty (matches the
    -- TransactionalStore contract).
    Store.kvsGet underlying "k" >>= (`shouldBe` Nothing)
    let coord =
          withTransactionalStores
            noopEOSCoordinator
            [TX.txnCommit ts]
            [TX.txnAbort ts]
    outcome <- runCommitCycle coord "g" (pure Map.empty) (pure ())
    outcome `shouldBe` CommitSucceeded
    -- Post-commit: underlying now has the buffered write.
    Store.kvsGet underlying "k" >>= (`shouldBe` Just "v")
    TX.txnPendingCount ts >>= (`shouldBe` 0)


{- | KIP-892: when the producer commit aborts, the
TransactionalStore's buffer is discarded.
-}
eos_transactional_stores_revert_on_abort :: Spec
eos_transactional_stores_revert_on_abort =
  it "withTransactionalStores: abort path runs storeAbort" $ do
    underlying <-
      Mem.inMemoryKeyValueStore @Text @Text
        (Store.storeName "x2")
    ts <- TX.newTransactionalStore underlying
    let kvs = TX.txnStore ts
    Store.kvsPut kvs "k" "v"
    let coord =
          withTransactionalStores
            noopEOSCoordinator
              { commitTxn = pure (Left "forced-fail")
              }
            [TX.txnCommit ts]
            [TX.txnAbort ts]
    outcome <- runCommitCycle coord "g" (pure Map.empty) (pure ())
    case outcome of
      CommitAborted _ -> pure ()
      _ -> error ("unexpected outcome: " <> show outcome)
    -- Underlying never received the put; pending was
    -- discarded by storeAbort.
    Store.kvsGet underlying "k" >>= (`shouldBe` Nothing)
    TX.txnPendingCount ts >>= (`shouldBe` 0)


assertContains :: Text -> Text -> IO ()
assertContains needle hay
  | T.isInfixOf needle hay = pure ()
  | otherwise =
      error
        ("expected " <> show needle <> " in " <> show hay)
