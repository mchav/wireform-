{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.RetryClassifier
Description : KIP-487 — automatic retry of producer failures, classified by error code

KIP-487 standardised which Kafka error codes are /retriable/
(transient — try again), /abortable/ (transactional state
poisoned — abort the txn but the producer can keep going), and
/fatal/ (the producer must close).

Mirrors the JVM client's @org.apache.kafka.common.errors.*@
hierarchy + @RetriableException@ marker interface. The producer's
sender thread consults 'classify' on every non-zero error code
returned by Produce / Fetch / Heartbeat to decide whether to
retry the batch, abort the transaction, or surface a fatal error
to the caller.

The mapping here is the canonical one published in the upstream
@common/errors/Errors.java@; each branch comments the JVM enum
name + the human-readable description librdkafka uses.
-}
module Kafka.Client.RetryClassifier
  ( ErrorClass (..)
  , classify
  , isRetriable
  , isAbortable
  , isFatal
  , errorMessage
  ) where

import Data.Int (Int16)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data ErrorClass
  = ECRetriable
    -- ^ Transient: client should back off + retry the same
    --   request.
  | ECAbortable
    -- ^ Transactional state is poisoned for the current txn
    --   but the producer can recover by aborting + restarting
    --   it.
  | ECFatal
    -- ^ Producer must close. The error code names a permanent
    --   condition.
  | ECNoError
    -- ^ Sentinel for code 0; should never be classified.
  deriving stock (Eq, Show, Generic)

-- | Classify a Kafka error code per KIP-487.
classify :: Int16 -> ErrorClass
classify = \case
  0   -> ECNoError
  -- Retriable
  1   -> ECRetriable  -- OFFSET_OUT_OF_RANGE
  3   -> ECRetriable  -- UNKNOWN_TOPIC_OR_PARTITION
  5   -> ECRetriable  -- LEADER_NOT_AVAILABLE
  6   -> ECRetriable  -- NOT_LEADER_OR_FOLLOWER
  7   -> ECRetriable  -- REQUEST_TIMED_OUT
  9   -> ECRetriable  -- REPLICA_NOT_AVAILABLE
  11  -> ECRetriable  -- STALE_CONTROLLER_EPOCH
  13  -> ECRetriable  -- NETWORK_EXCEPTION
  14  -> ECRetriable  -- COORDINATOR_LOAD_IN_PROGRESS
  15  -> ECRetriable  -- COORDINATOR_NOT_AVAILABLE
  16  -> ECRetriable  -- NOT_COORDINATOR
  19  -> ECRetriable  -- NOT_ENOUGH_REPLICAS
  20  -> ECRetriable  -- NOT_ENOUGH_REPLICAS_AFTER_APPEND
  41  -> ECRetriable  -- NOT_CONTROLLER
  42  -> ECRetriable  -- INVALID_REQUEST
  46  -> ECRetriable  -- KAFKA_STORAGE_ERROR
  56  -> ECRetriable  -- LISTENER_NOT_FOUND
  62  -> ECRetriable  -- ELECTION_NOT_NEEDED
  63  -> ECRetriable  -- NO_REASSIGNMENT_IN_PROGRESS
  68  -> ECRetriable  -- REASSIGNMENT_IN_PROGRESS
  74  -> ECRetriable  -- THROTTLING_QUOTA_EXCEEDED
  75  -> ECRetriable  -- PRODUCER_FENCED -- abortable in JVM, retriable on session reset
  76  -> ECRetriable  -- RESOURCE_NOT_FOUND
  78  -> ECRetriable  -- BROKER_ID_NOT_REGISTERED
  79  -> ECRetriable  -- INCONSISTENT_TOPIC_ID
  82  -> ECRetriable  -- FETCH_SESSION_TOPIC_ID_ERROR
  91  -> ECRetriable  -- INELIGIBLE_REPLICA
  104 -> ECRetriable  -- TRANSACTION_ABORTABLE (KIP-1044 mirror)
  -- Abortable
  10  -> ECAbortable  -- MESSAGE_TOO_LARGE (split + retry; abort if single)
  18  -> ECAbortable  -- MESSAGE_TOO_LARGE_OR_OFFSET_MISMATCH
  47  -> ECAbortable  -- INVALID_PRODUCER_ID_MAPPING
  48  -> ECAbortable  -- INVALID_PARTITIONS_IN_TXN_REQUEST
  49  -> ECAbortable  -- INVALID_TXN_TIMEOUT
  50  -> ECAbortable  -- CONCURRENT_TRANSACTIONS
  51  -> ECAbortable  -- INVALID_TXN_STATE
  -- Fatal
  4   -> ECFatal      -- INVALID_FETCH_SIZE
  8   -> ECFatal      -- BROKER_NOT_AVAILABLE (the broker is gone permanently)
  17  -> ECFatal      -- INVALID_TOPIC_EXCEPTION
  25  -> ECFatal      -- ILLEGAL_GENERATION
  29  -> ECFatal      -- TOPIC_AUTHORIZATION_FAILED
  30  -> ECFatal      -- GROUP_AUTHORIZATION_FAILED
  31  -> ECFatal      -- CLUSTER_AUTHORIZATION_FAILED
  37  -> ECFatal      -- TRANSACTIONAL_ID_AUTHORIZATION_FAILED
  38  -> ECFatal      -- SECURITY_DISABLED
  44  -> ECFatal      -- INVALID_REPLICATION_FACTOR
  53  -> ECFatal      -- INVALID_REPLICA_ASSIGNMENT
  58  -> ECFatal      -- TOPIC_DELETION_DISABLED
  85  -> ECFatal      -- INVALID_RECORD
  86  -> ECFatal      -- UNSUPPORTED_COMPRESSION_TYPE
  87  -> ECFatal      -- PREFERRED_LEADER_NOT_AVAILABLE
  88  -> ECFatal      -- GROUP_MAX_SIZE_REACHED
  89  -> ECFatal      -- FENCED_INSTANCE_ID
  90  -> ECFatal      -- ELIGIBLE_LEADERS_NOT_AVAILABLE
  -- Default: retriable. The JVM client treats unknown error
  -- codes as retriable so a future broker upgrade doesn't
  -- crash old clients.
  _   -> ECRetriable

isRetriable, isAbortable, isFatal :: Int16 -> Bool
isRetriable c = classify c == ECRetriable
isAbortable c = classify c == ECAbortable
isFatal     c = classify c == ECFatal

-- | Human-readable label per KIP-1054 (consistent error
-- messages). The mapping uses the canonical Kafka enum names
-- so logs port across clients.
errorMessage :: Int16 -> Text
errorMessage = \case
  0   -> "NONE"
  1   -> "OFFSET_OUT_OF_RANGE"
  2   -> "CORRUPT_MESSAGE"
  3   -> "UNKNOWN_TOPIC_OR_PARTITION"
  4   -> "INVALID_FETCH_SIZE"
  5   -> "LEADER_NOT_AVAILABLE"
  6   -> "NOT_LEADER_OR_FOLLOWER"
  7   -> "REQUEST_TIMED_OUT"
  8   -> "BROKER_NOT_AVAILABLE"
  9   -> "REPLICA_NOT_AVAILABLE"
  10  -> "MESSAGE_TOO_LARGE"
  11  -> "STALE_CONTROLLER_EPOCH"
  12  -> "OFFSET_METADATA_TOO_LARGE"
  13  -> "NETWORK_EXCEPTION"
  14  -> "COORDINATOR_LOAD_IN_PROGRESS"
  15  -> "COORDINATOR_NOT_AVAILABLE"
  16  -> "NOT_COORDINATOR"
  17  -> "INVALID_TOPIC_EXCEPTION"
  18  -> "RECORD_LIST_TOO_LARGE"
  19  -> "NOT_ENOUGH_REPLICAS"
  20  -> "NOT_ENOUGH_REPLICAS_AFTER_APPEND"
  21  -> "INVALID_REQUIRED_ACKS"
  22  -> "ILLEGAL_GENERATION"
  23  -> "INCONSISTENT_GROUP_PROTOCOL"
  24  -> "INVALID_GROUP_ID"
  25  -> "UNKNOWN_MEMBER_ID"
  29  -> "TOPIC_AUTHORIZATION_FAILED"
  30  -> "GROUP_AUTHORIZATION_FAILED"
  31  -> "CLUSTER_AUTHORIZATION_FAILED"
  37  -> "TRANSACTIONAL_ID_AUTHORIZATION_FAILED"
  38  -> "SECURITY_DISABLED"
  41  -> "NOT_CONTROLLER"
  47  -> "INVALID_PRODUCER_ID_MAPPING"
  48  -> "INVALID_PARTITIONS_IN_TXN_REQUEST"
  49  -> "INVALID_TXN_TIMEOUT"
  50  -> "CONCURRENT_TRANSACTIONS"
  51  -> "INVALID_TXN_STATE"
  74  -> "THROTTLING_QUOTA_EXCEEDED"
  75  -> "PRODUCER_FENCED"
  85  -> "INVALID_RECORD"
  104 -> "TRANSACTION_ABORTABLE"
  c   -> "UNKNOWN_KAFKA_ERROR_" <> tshow c
  where
    tshow :: Int16 -> Text
    tshow = T.pack . show
