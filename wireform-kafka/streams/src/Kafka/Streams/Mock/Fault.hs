{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Mock.Fault
-- Description : Fault injection for the mock cluster
--
-- Modelled on librdkafka's @rd_kafka_mock_push_request_errors@
-- machinery: a fault policy produces an error (or 'Nothing' meaning
-- "no fault, run normally") for each operation. Tests drive
-- specific failure modes by pre-loading the policy with a queue of
-- errors per (topic, partition) — the first call gets the head of
-- the queue, the second the next, and so on.
module Kafka.Streams.Mock.Fault
  ( -- * Errors
    MockError (..)
  , isRetriable
  , isFatal
  , kafkaErrorText
    -- * Fault policy
  , FaultPolicy
  , noFaults
  , queueProduceErrors
  , queueFetchErrors
  , queueCommitErrors
  , queueTxnErrors
  , queueTxnBeginErrors
  , queueTxnCommitErrors
  , queueTxnAbortErrors
  , addProduceFault
  , addFetchFault
  , addCommitFault
  , addTxnFault
  , addTxnBeginFault
  , addTxnCommitFault
  , addTxnAbortFault
  , TxnOp (..)
  , clearFaults
    -- * Querying / popping
  , takeProduceFault
  , takeFetchFault
  , takeCommitFault
  , takeTxnFault
  , takeTxnFaultFor
    -- * Permanent (sticky) faults
  , setStickyProduce
  , setStickyFetch
  , clearStickyProduce
  , clearStickyFetch
  ) where

import Control.Concurrent.STM
import Control.Monad (forM_)
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Streams.Mock.Cluster (GroupId, TxnId)
import Kafka.Streams.Types (TopicName)

----------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------

-- | The error variants the mock cluster can inject. The names mirror
-- @org.apache.kafka.common.errors.*@ / @rd_kafka_resp_err_t@ so
-- failure-mode tests read like a Java client trace.
data MockError
  = ErrCorruptMessage              -- !(retriable) data check failed
  | ErrUnknownServerError          -- !(retriable) generic
  | ErrLeaderNotAvailable          -- !(retriable) leader election in flight
  | ErrNotLeaderForPartition       -- !(retriable) reroute to new leader
  | ErrRequestTimedOut             -- !(retriable) request timeout
  | ErrNetworkException            -- !(retriable) connection died
  | ErrCoordinatorNotAvailable     -- !(retriable) group coordinator down
  | ErrCoordinatorLoadInProgress   -- !(retriable) group rejoining
  | ErrNotCoordinator              -- !(retriable) wrong coordinator
  | ErrOffsetOutOfRange            -- !(retriable but special) consumer rewinds
  | ErrInvalidProducerEpoch        -- !(fatal) txn fenced
  | ErrTransactionalIdAuthorizationFailed  -- !(fatal)
  | ErrInvalidTxnState             -- !(fatal) txn state machine wrong
  | ErrUnknownTopicOrPartition     -- !(fatal) caller bug or topic deleted
  | ErrAuthorizationFailed         -- !(fatal) ACL denied
  | ErrInvalidRequiredAcks         -- !(fatal) bad config
  | ErrRecordTooLarge              -- !(fatal) over max-message-bytes
  | ErrPolicyViolation             -- !(fatal)
  | ErrMessageTooLarge             -- !(fatal)
  | ErrCustom !Text                -- escape hatch with a custom message
  deriving stock (Eq, Show, Generic)

-- | Errors that the client is expected to retry on (the runtime
-- backs off, refreshes metadata, and resumes).
isRetriable :: MockError -> Bool
isRetriable = \case
  ErrCorruptMessage              -> True
  ErrUnknownServerError          -> True
  ErrLeaderNotAvailable          -> True
  ErrNotLeaderForPartition       -> True
  ErrRequestTimedOut             -> True
  ErrNetworkException            -> True
  ErrCoordinatorNotAvailable     -> True
  ErrCoordinatorLoadInProgress   -> True
  ErrNotCoordinator              -> True
  ErrOffsetOutOfRange            -> True
  _                              -> False

isFatal :: MockError -> Bool
isFatal = not . isRetriable

-- | Render a 'MockError' the way a real broker would in its
-- @errorMessage@ field (used by 'Kafka.Streams.Errors').
kafkaErrorText :: MockError -> Text
kafkaErrorText = \case
  ErrCorruptMessage              -> "CORRUPT_MESSAGE"
  ErrUnknownServerError          -> "UNKNOWN_SERVER_ERROR"
  ErrLeaderNotAvailable          -> "LEADER_NOT_AVAILABLE"
  ErrNotLeaderForPartition       -> "NOT_LEADER_FOR_PARTITION"
  ErrRequestTimedOut             -> "REQUEST_TIMED_OUT"
  ErrNetworkException            -> "NETWORK_EXCEPTION"
  ErrCoordinatorNotAvailable     -> "COORDINATOR_NOT_AVAILABLE"
  ErrCoordinatorLoadInProgress   -> "COORDINATOR_LOAD_IN_PROGRESS"
  ErrNotCoordinator              -> "NOT_COORDINATOR"
  ErrOffsetOutOfRange            -> "OFFSET_OUT_OF_RANGE"
  ErrInvalidProducerEpoch        -> "INVALID_PRODUCER_EPOCH"
  ErrTransactionalIdAuthorizationFailed
                                 -> "TRANSACTIONAL_ID_AUTHORIZATION_FAILED"
  ErrInvalidTxnState             -> "INVALID_TXN_STATE"
  ErrUnknownTopicOrPartition     -> "UNKNOWN_TOPIC_OR_PARTITION"
  ErrAuthorizationFailed         -> "TOPIC_AUTHORIZATION_FAILED"
  ErrInvalidRequiredAcks         -> "INVALID_REQUIRED_ACKS"
  ErrRecordTooLarge              -> "RECORD_TOO_LARGE"
  ErrPolicyViolation             -> "POLICY_VIOLATION"
  ErrMessageTooLarge             -> "MESSAGE_TOO_LARGE"
  ErrCustom t                    -> t

----------------------------------------------------------------------
-- Policy (queues + sticky overrides)
----------------------------------------------------------------------

-- | The fault policy is itself a small in-memory database. Each
-- (operation, key) pair has:
--
--   * a /queue/ of errors consumed in FIFO order on each call;
--   * an optional /sticky/ override that fires forever until the
--     test calls @clearSticky*@.
--
-- Sticky takes precedence over the queue when both are present.
-- | Which transactional operation a fault should target.
data TxnOp = TxnBegin | TxnCommit | TxnAbort
  deriving (Eq, Ord, Show)

data FaultPolicy = FaultPolicy
  { fpProduceQ  :: !(TVar (Map (TopicName, Int32) (Seq MockError)))
  , fpFetchQ    :: !(TVar (Map (TopicName, Int32) (Seq MockError)))
  , fpCommitQ   :: !(TVar (Map GroupId (Seq MockError)))
  , fpTxnQ      :: !(TVar (Map (TxnId, TxnOp) (Seq MockError)))
  , fpStickyP   :: !(TVar (Map (TopicName, Int32) MockError))
  , fpStickyF   :: !(TVar (Map (TopicName, Int32) MockError))
  }

noFaults :: IO FaultPolicy
noFaults = atomically $ do
  p <- newTVar Map.empty
  f <- newTVar Map.empty
  c <- newTVar Map.empty
  t <- newTVar Map.empty
  sp <- newTVar Map.empty
  sf <- newTVar Map.empty
  pure FaultPolicy
    { fpProduceQ = p
    , fpFetchQ   = f
    , fpCommitQ  = c
    , fpTxnQ     = t
    , fpStickyP  = sp
    , fpStickyF  = sf
    }

----------------------------------------------------------------------
-- Queue ops
----------------------------------------------------------------------

queueProduceErrors
  :: FaultPolicy -> TopicName -> Int32 -> [MockError] -> IO ()
queueProduceErrors fp t p errs = atomically $
  modifyTVar' (fpProduceQ fp) (Map.insertWith (Seq.><) (t, p) (Seq.fromList errs))

queueFetchErrors
  :: FaultPolicy -> TopicName -> Int32 -> [MockError] -> IO ()
queueFetchErrors fp t p errs = atomically $
  modifyTVar' (fpFetchQ fp) (Map.insertWith (Seq.><) (t, p) (Seq.fromList errs))

queueCommitErrors :: FaultPolicy -> GroupId -> [MockError] -> IO ()
queueCommitErrors fp g errs = atomically $
  modifyTVar' (fpCommitQ fp) (Map.insertWith (Seq.><) g (Seq.fromList errs))

-- | Queue errors for /every/ transactional op against a txn id.
-- Faults fire in this order: 'TxnBegin' first, then 'TxnCommit',
-- then 'TxnAbort' — the order in which a typical client makes
-- those calls. Use 'queueTxnBeginErrors' / 'queueTxnCommitErrors'
-- / 'queueTxnAbortErrors' for op-specific control.
queueTxnErrors :: FaultPolicy -> TxnId -> [MockError] -> IO ()
queueTxnErrors fp t errs = atomically $ do
  forM_ [TxnBegin, TxnCommit, TxnAbort] $ \op ->
    modifyTVar' (fpTxnQ fp)
      (Map.insertWith (Seq.><) (t, op) (Seq.fromList errs))

queueTxnBeginErrors :: FaultPolicy -> TxnId -> [MockError] -> IO ()
queueTxnBeginErrors = queueTxnOpErrors TxnBegin

queueTxnCommitErrors :: FaultPolicy -> TxnId -> [MockError] -> IO ()
queueTxnCommitErrors = queueTxnOpErrors TxnCommit

queueTxnAbortErrors :: FaultPolicy -> TxnId -> [MockError] -> IO ()
queueTxnAbortErrors = queueTxnOpErrors TxnAbort

queueTxnOpErrors
  :: TxnOp -> FaultPolicy -> TxnId -> [MockError] -> IO ()
queueTxnOpErrors op fp t errs = atomically $
  modifyTVar' (fpTxnQ fp)
    (Map.insertWith (Seq.><) (t, op) (Seq.fromList errs))

addProduceFault :: FaultPolicy -> TopicName -> Int32 -> MockError -> IO ()
addProduceFault fp t p e = queueProduceErrors fp t p [e]

addFetchFault :: FaultPolicy -> TopicName -> Int32 -> MockError -> IO ()
addFetchFault fp t p e = queueFetchErrors fp t p [e]

addCommitFault :: FaultPolicy -> GroupId -> MockError -> IO ()
addCommitFault fp g e = queueCommitErrors fp g [e]

-- | Queue a single error against /every/ txn op for this txn id.
-- See 'queueTxnErrors' for the per-op variants.
addTxnFault :: FaultPolicy -> TxnId -> MockError -> IO ()
addTxnFault fp t e = queueTxnErrors fp t [e]

addTxnBeginFault, addTxnCommitFault, addTxnAbortFault
  :: FaultPolicy -> TxnId -> MockError -> IO ()
addTxnBeginFault  fp t e = queueTxnBeginErrors  fp t [e]
addTxnCommitFault fp t e = queueTxnCommitErrors fp t [e]
addTxnAbortFault  fp t e = queueTxnAbortErrors  fp t [e]

-- | Drain every fault out of every queue. Sticky overrides are
-- left in place (call 'clearStickyProduce'/'clearStickyFetch'
-- separately). Useful between test phases.
clearFaults :: FaultPolicy -> IO ()
clearFaults fp = atomically $ do
  forM_ [fpProduceQ fp, fpFetchQ fp] (\v -> writeTVar v Map.empty)
  writeTVar (fpCommitQ fp) Map.empty
  writeTVar (fpTxnQ fp)    Map.empty

----------------------------------------------------------------------
-- Sticky (permanent until cleared)
----------------------------------------------------------------------

setStickyProduce :: FaultPolicy -> TopicName -> Int32 -> MockError -> IO ()
setStickyProduce fp t p e = atomically $
  modifyTVar' (fpStickyP fp) (Map.insert (t, p) e)

setStickyFetch :: FaultPolicy -> TopicName -> Int32 -> MockError -> IO ()
setStickyFetch fp t p e = atomically $
  modifyTVar' (fpStickyF fp) (Map.insert (t, p) e)

clearStickyProduce :: FaultPolicy -> TopicName -> Int32 -> IO ()
clearStickyProduce fp t p = atomically $
  modifyTVar' (fpStickyP fp) (Map.delete (t, p))

clearStickyFetch :: FaultPolicy -> TopicName -> Int32 -> IO ()
clearStickyFetch fp t p = atomically $
  modifyTVar' (fpStickyF fp) (Map.delete (t, p))

----------------------------------------------------------------------
-- Pop helpers
----------------------------------------------------------------------

popHead :: Ord k => TVar (Map k (Seq a)) -> k -> STM (Maybe a)
popHead ref k = do
  m <- readTVar ref
  case Map.lookup k m of
    Nothing -> pure Nothing
    Just sq -> case Seq.viewl sq of
      Seq.EmptyL    -> pure Nothing
      e Seq.:< rest -> do
        writeTVar ref (Map.insert k rest m)
        pure (Just e)

-- | Pop a fault for the next produce on (topic, partition). Sticky
-- overrides win when set. Returns 'Nothing' if no fault should fire.
takeProduceFault
  :: FaultPolicy -> TopicName -> Int32 -> IO (Maybe MockError)
takeProduceFault fp t p = do
  sticky <- readTVarIO (fpStickyP fp)
  case Map.lookup (t, p) sticky of
    Just e  -> pure (Just e)
    Nothing -> atomically (popHead (fpProduceQ fp) (t, p))

takeFetchFault
  :: FaultPolicy -> TopicName -> Int32 -> IO (Maybe MockError)
takeFetchFault fp t p = do
  sticky <- readTVarIO (fpStickyF fp)
  case Map.lookup (t, p) sticky of
    Just e  -> pure (Just e)
    Nothing -> atomically (popHead (fpFetchQ fp) (t, p))

takeCommitFault :: FaultPolicy -> GroupId -> IO (Maybe MockError)
takeCommitFault fp g = atomically (popHead (fpCommitQ fp) g)

-- | Op-specific take. Use this from inside producer code so a
-- queued begin-fault doesn't accidentally fire on a commit and
-- vice-versa.
takeTxnFaultFor :: FaultPolicy -> TxnId -> TxnOp -> IO (Maybe MockError)
takeTxnFaultFor fp t op = atomically (popHead (fpTxnQ fp) (t, op))

-- | Backwards-compatible: pop a fault for any op (tries begin
-- first, then commit, then abort).
takeTxnFault :: FaultPolicy -> TxnId -> IO (Maybe MockError)
takeTxnFault fp t = do
  -- Caller doesn't know which op is firing; default to TxnCommit
  -- which is the most common test target. Internal producers
  -- should call 'takeTxnFaultFor' with the right op.
  takeTxnFaultFor fp t TxnCommit
