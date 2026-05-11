{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Runtime.EOS
-- Description : Exactly-once-v2 commit orchestration for the Streams runtime
--
-- This module provides the /orchestration/ layer that drives the
-- transactional producer through the EOS-v2 commit cycle:
--
--   1. On startup, the runtime calls 'eosInit' once.  It maps to
--      @InitProducerId@ on the broker via the underlying
--      'Kafka.Client.Transaction'.
--   2. On every commit interval, the runtime calls 'eosBegin' →
--      flushes the engine (writes records through the transactional
--      producer) → 'eosCommitOffsets' (sends @TxnOffsetCommit@ for
--      the consumer-group offsets it consumed since the previous
--      commit) → 'eosCommit'.
--   3. On any unrecoverable error, the runtime calls 'eosAbort'
--      and either rejoins the consumer group or shuts down,
--      depending on the error.
--
-- == Why a separate module
--
-- The orchestration is defined against an 'EOSCoordinator' record
-- rather than a hard dependency on 'Kafka.Client.Transaction', so
-- tests can inject a mock and verify the call sequence.
-- 'newRealEOSCoordinator' wires it to the real client.
module Kafka.Streams.Runtime.EOS
  ( EOSCoordinator (..)
  , noopEOSCoordinator
  , runCommitCycle
  , CommitOutcome (..)
  , newRealEOSCoordinator
  , withTransactionalStores
  ) where

import Control.Exception (SomeException, try)
import Data.Int (Int64)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Kafka.Client.Consumer as KC
import qualified Kafka.Client.Transaction as KT

----------------------------------------------------------------------
-- Coordinator interface
----------------------------------------------------------------------

-- | Pluggable EOS coordinator. The Streams runtime calls these
-- callbacks; production wires them to 'Kafka.Client.Transaction',
-- tests wire them to a recorder.
data EOSCoordinator = EOSCoordinator
  { initTxn        :: !(IO (Either Text ()))
  , beginTxn       :: !(IO (Either Text ()))
  , commitTxn      :: !(IO (Either Text ()))
  , abortTxn       :: !(IO (Either Text ()))
  , commitOffsets  :: !(Text                                  -- consumer group id
                       -> HashMap KC.TopicPartition Int64
                       -> IO (Either Text ()))
  , storeCommit    :: !(IO (Either Text ()))
    -- ^ KIP-892: drain every transactional state store onto its
    --   underlying store. Called by 'runCommitCycle' AFTER the
    --   producer transaction commit succeeds, so the store
    --   write happens iff the wire-side commit was durable.
    --   Default ('noopEOSCoordinator'): pure (Right ()).
  , storeAbort     :: !(IO (Either Text ()))
    -- ^ KIP-892: discard every transactional state store's
    --   buffered writes. Called when the producer transaction
    --   aborts so the store and the broker-side log stay
    --   consistent.
  }

-- | The do-nothing coordinator: every step succeeds without side
-- effects. Used by the at-least-once code path.
noopEOSCoordinator :: EOSCoordinator
noopEOSCoordinator = EOSCoordinator
  { initTxn       = pure (Right ())
  , beginTxn      = pure (Right ())
  , commitTxn     = pure (Right ())
  , abortTxn      = pure (Right ())
  , commitOffsets = \_ _ -> pure (Right ())
  , storeCommit   = pure (Right ())
  , storeAbort    = pure (Right ())
  }

-- | Result of one commit cycle.
data CommitOutcome
  = CommitSucceeded
  | CommitAborted    !Text   -- ^ Aborted due to an error; runtime should rebalance
  | CommitFatal      !Text   -- ^ Fatal error; runtime should fail-fast
  deriving stock (Eq, Show)

-- | Drive a single commit cycle. The supplied @flushBody@ is run
-- between 'eosBegin' and 'eosCommit'; it should drive the engine
-- and, if EOS is on, send all produced records via the transactional
-- producer.
runCommitCycle
  :: EOSCoordinator
  -> Text                                            -- consumer group id
  -> IO (HashMap KC.TopicPartition Int64)            -- offsets to commit
  -> IO ()                                           -- flush body
  -> IO CommitOutcome
runCommitCycle coord groupId getOffsets flushBody = do
  step1 <- coord.beginTxn
  case step1 of
    Left err -> pure (CommitFatal ("begin: " <> err))
    Right () -> do
      bodyR <- try flushBody :: IO (Either SomeException ())
      case bodyR of
        Left e -> doAbort ("flush: " <> T.pack (show e))
        Right () -> do
          offs <- getOffsets
          step2 <- coord.commitOffsets groupId offs
          case step2 of
            Left err -> doAbort ("commitOffsets: " <> err)
            Right () -> do
              step3 <- coord.commitTxn
              case step3 of
                Left err -> doAbort ("commit: " <> err)
                Right () -> do
                  -- KIP-892: the producer commit succeeded, so
                  -- the changelog records are durable; only now
                  -- is it safe to drain the per-task
                  -- TransactionalStore buffers onto their
                  -- underlying stores.
                  step4 <- coord.storeCommit
                  case step4 of
                    Left err ->
                      -- The wire commit succeeded but the
                      -- store commit failed: log the runtime
                      -- as fatal (no clean recovery — the
                      -- store and the log are now permanently
                      -- inconsistent for this task).
                      pure (CommitFatal ("storeCommit: " <> err))
                    Right () -> pure CommitSucceeded
  where
    doAbort reason = do
      _ <- coord.abortTxn
      -- KIP-892: matching abort on the store side discards
      -- buffered writes so a retry starts from a clean slate.
      _ <- coord.storeAbort
      pure (CommitAborted reason)

----------------------------------------------------------------------
-- Real-broker coordinator
----------------------------------------------------------------------

-- | Wire the coordinator to a real 'KT.Transaction'. Translates the
-- transaction client's 'TransactionError' into 'Text' for the
-- orchestrator.
newRealEOSCoordinator :: KT.Transaction -> EOSCoordinator
newRealEOSCoordinator txn = EOSCoordinator
  { initTxn  = wrapTE <$> KT.initTransactions txn
  , beginTxn = wrapTE <$> KT.beginTransaction txn
  , commitTxn = wrapTE <$> KT.commitTransaction txn
  , abortTxn = wrapTE <$> KT.abortTransaction txn
  , commitOffsets = \gid offs ->
      wrapTE <$> KT.commitOffsetsInTransaction txn gid offs
  , storeCommit = pure (Right ())
    -- No transactional stores registered with this coordinator
    -- by default. Callers that materialise stores wrap the
    -- coordinator with 'withTransactionalStores' below.
  , storeAbort  = pure (Right ())
  }
  where
    wrapTE = either (Left . T.pack . show) Right

-- | KIP-892 wiring helper: take an existing coordinator and a
-- list of 'TransactionalStore'-like commit/abort actions and
-- thread them through 'eosStoreCommit' / 'eosStoreAbort'. The
-- callbacks are run in declaration order on commit; the first
-- 'Left' short-circuits and the runtime promotes the cycle to
-- 'CommitFatal'.
--
-- Callers typically build the list once at engine
-- construction time and reuse the coordinator across every
-- commit cycle.
withTransactionalStores
  :: EOSCoordinator
  -> [IO ()]                  -- per-store commit actions
  -> [IO ()]                  -- per-store abort actions
  -> EOSCoordinator
withTransactionalStores base commits aborts = base
  { storeCommit = runActions commits "store-commit"
  , storeAbort  = runActions aborts  "store-abort"
  }
  where
    runActions xs label = do
      r <- try (sequence_ xs) :: IO (Either SomeException ())
      case r of
        Right () -> pure (Right ())
        Left e   -> pure (Left (label <> ": " <> T.pack (show e)))
