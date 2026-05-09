{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
  ) where

import Control.Exception (SomeException, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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
  { eosInit            :: !(IO (Either Text ()))
  , eosBegin           :: !(IO (Either Text ()))
  , eosCommit          :: !(IO (Either Text ()))
  , eosAbort           :: !(IO (Either Text ()))
  , eosCommitOffsets   :: !(Text                              -- consumer group id
                            -> Map KC.TopicPartition Int64
                            -> IO (Either Text ()))
  }

-- | The do-nothing coordinator: every step succeeds without side
-- effects. Used by the at-least-once code path.
noopEOSCoordinator :: EOSCoordinator
noopEOSCoordinator = EOSCoordinator
  { eosInit          = pure (Right ())
  , eosBegin         = pure (Right ())
  , eosCommit        = pure (Right ())
  , eosAbort         = pure (Right ())
  , eosCommitOffsets = \_ _ -> pure (Right ())
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
  -> Text                                       -- consumer group id
  -> IO (Map KC.TopicPartition Int64)            -- offsets to commit
  -> IO ()                                      -- flush body
  -> IO CommitOutcome
runCommitCycle coord groupId getOffsets flushBody = do
  step1 <- eosBegin coord
  case step1 of
    Left err -> pure (CommitFatal ("begin: " <> err))
    Right () -> do
      bodyR <- try flushBody :: IO (Either SomeException ())
      case bodyR of
        Left e -> doAbort ("flush: " <> T.pack (show e))
        Right () -> do
          offs <- getOffsets
          step2 <- eosCommitOffsets coord groupId offs
          case step2 of
            Left err -> doAbort ("commitOffsets: " <> err)
            Right () -> do
              step3 <- eosCommit coord
              case step3 of
                Left err -> doAbort ("commit: " <> err)
                Right () -> pure CommitSucceeded
  where
    doAbort reason = do
      _ <- eosAbort coord
      pure (CommitAborted reason)

----------------------------------------------------------------------
-- Real-broker coordinator
----------------------------------------------------------------------

-- | Wire the coordinator to a real 'KT.Transaction'. Translates the
-- transaction client's 'TransactionError' into 'Text' for the
-- orchestrator.
newRealEOSCoordinator :: KT.Transaction -> EOSCoordinator
newRealEOSCoordinator txn = EOSCoordinator
  { eosInit  = wrapTE <$> KT.initTransactions txn
  , eosBegin = wrapTE <$> KT.beginTransaction txn
  , eosCommit = wrapTE <$> KT.commitTransaction txn
  , eosAbort = wrapTE <$> KT.abortTransaction txn
  , eosCommitOffsets = \gid offs ->
      wrapTE <$> KT.commitOffsetsInTransaction txn gid offs
  }
  where
    wrapTE = either (Left . T.pack . show) Right

-- 'IORef' / 'readIORef' / 'writeIORef' / 'newIORef' kept here as
-- they're commonly used by tests that attach a recording
-- coordinator (see 'Streams.EOSSpec' in the test suite).
_keepIO :: IORef Int -> IO Int
_keepIO r = do
  _ <- newIORef (0 :: Int)
  v <- readIORef r
  writeIORef r v
  pure v