{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
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
module Kafka.Client.RetryClassifier (
  ErrorClass (..),
  classify,
  isRetriable,
  isAbortable,
  isFatal,
  errorMessage,
) where

import Data.Int (Int16)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)


data ErrorClass
  = {- | Transient: client should back off + retry the same
    request.
    -}
    ECRetriable
  | {- | Transactional state is poisoned for the current txn
    but the producer can recover by aborting + restarting
    it.
    -}
    ECAbortable
  | {- | Producer must close. The error code names a permanent
    condition.
    -}
    ECFatal
  | -- | Sentinel for code 0; should never be classified.
    ECNoError
  deriving stock (Eq, Show, Generic)


{- | Classify a Kafka error code per KIP-487.

Error codes + class assignments are sourced from the canonical
Apache Kafka 3.7+ @org.apache.kafka.common.protocol.Errors@ enum
(see @clients/src/main/java/org/apache/kafka/common/protocol/Errors.java@).
The JVM client uses @RetriableException@ as a marker interface;
everything else is fatal unless the producer specifically knows
how to recover from it (the abortable bucket below).
-}
classify :: Int16 -> ErrorClass
classify = \case
  0 -> ECNoError
  -- Retriable: transient broker / network / coordinator state.
  1 -> ECRetriable -- OFFSET_OUT_OF_RANGE
  3 -> ECRetriable -- UNKNOWN_TOPIC_OR_PARTITION
  5 -> ECRetriable -- LEADER_NOT_AVAILABLE
  6 -> ECRetriable -- NOT_LEADER_OR_FOLLOWER
  7 -> ECRetriable -- REQUEST_TIMED_OUT
  9 -> ECRetriable -- REPLICA_NOT_AVAILABLE
  11 -> ECRetriable -- STALE_CONTROLLER_EPOCH
  13 -> ECRetriable -- NETWORK_EXCEPTION
  14 -> ECRetriable -- COORDINATOR_LOAD_IN_PROGRESS
  15 -> ECRetriable -- COORDINATOR_NOT_AVAILABLE
  16 -> ECRetriable -- NOT_COORDINATOR
  19 -> ECRetriable -- NOT_ENOUGH_REPLICAS
  20 -> ECRetriable -- NOT_ENOUGH_REPLICAS_AFTER_APPEND
  22 -> ECRetriable -- ILLEGAL_GENERATION (rejoin to refresh)
  41 -> ECRetriable -- NOT_CONTROLLER
  42 -> ECRetriable -- INVALID_REQUEST (broker bug or schema drift)
  56 -> ECRetriable -- KAFKA_STORAGE_ERROR
  60 -> ECRetriable -- REASSIGNMENT_IN_PROGRESS
  72 -> ECRetriable -- LISTENER_NOT_FOUND
  74 -> ECRetriable -- FENCED_LEADER_EPOCH (refresh metadata + retry)
  75 -> ECRetriable -- UNKNOWN_LEADER_EPOCH (retry; broker hasn't caught up)
  78 -> ECRetriable -- OFFSET_NOT_AVAILABLE (during leader transition)
  79 -> ECRetriable -- MEMBER_ID_REQUIRED (rejoin with empty id then retry)
  84 -> ECRetriable -- ELECTION_NOT_NEEDED
  85 -> ECRetriable -- NO_REASSIGNMENT_IN_PROGRESS
  88 -> ECRetriable -- UNSTABLE_OFFSET_COMMIT (KIP-447, retry the fetch)
  89 -> ECRetriable -- THROTTLING_QUOTA_EXCEEDED (KIP-599)
  91 -> ECRetriable -- RESOURCE_NOT_FOUND
  102 -> ECRetriable -- BROKER_ID_NOT_REGISTERED
  103 -> ECRetriable -- INCONSISTENT_TOPIC_ID (refresh metadata + retry)
  106 -> ECRetriable -- FETCH_SESSION_TOPIC_ID_ERROR
  107 -> ECRetriable -- INELIGIBLE_REPLICA
  -- Abortable: transactional state is poisoned for the current
  -- txn but the producer can recover by aborting + restarting it.
  10 -> ECAbortable -- MESSAGE_TOO_LARGE (split + retry; abort if single)
  18 -> ECAbortable -- RECORD_LIST_TOO_LARGE
  47 -> ECAbortable -- INVALID_PRODUCER_EPOCH
  48 -> ECAbortable -- INVALID_TXN_STATE
  49 -> ECAbortable -- INVALID_PRODUCER_ID_MAPPING
  50 -> ECAbortable -- INVALID_TRANSACTION_TIMEOUT
  51 -> ECAbortable -- CONCURRENT_TRANSACTIONS
  52 -> ECAbortable -- TRANSACTION_COORDINATOR_FENCED
  -- TRANSACTION_ABORTABLE was 119 in 3.7-trunk but reserved
  -- under different numbers in earlier branches; treat the
  -- canonical 3.7 code 104 (INCONSISTENT_CLUSTER_ID) as fatal
  -- per Errors.java.
  -- Fatal: producer must close. Names match the upstream enum.
  4 -> ECFatal -- INVALID_FETCH_SIZE
  8 -> ECFatal -- BROKER_NOT_AVAILABLE
  17 -> ECFatal -- INVALID_TOPIC_EXCEPTION
  29 -> ECFatal -- TOPIC_AUTHORIZATION_FAILED
  30 -> ECFatal -- GROUP_AUTHORIZATION_FAILED
  31 -> ECFatal -- CLUSTER_AUTHORIZATION_FAILED
  38 -> ECFatal -- INVALID_REPLICATION_FACTOR
  39 -> ECFatal -- INVALID_REPLICA_ASSIGNMENT
  40 -> ECFatal -- INVALID_CONFIG
  43 -> ECFatal -- UNSUPPORTED_FOR_MESSAGE_FORMAT
  44 -> ECFatal -- POLICY_VIOLATION
  45 -> ECFatal -- OUT_OF_ORDER_SEQUENCE_NUMBER
  46 -> ECFatal -- DUPLICATE_SEQUENCE_NUMBER
  53 -> ECFatal -- TRANSACTIONAL_ID_AUTHORIZATION_FAILED
  54 -> ECFatal -- SECURITY_DISABLED
  73 -> ECFatal -- TOPIC_DELETION_DISABLED
  76 -> ECFatal -- UNSUPPORTED_COMPRESSION_TYPE
  80 -> ECFatal -- PREFERRED_LEADER_NOT_AVAILABLE
  81 -> ECFatal -- GROUP_MAX_SIZE_REACHED
  82 -> ECFatal -- FENCED_INSTANCE_ID (KIP-345)
  83 -> ECFatal -- ELIGIBLE_LEADERS_NOT_AVAILABLE
  87 -> ECFatal -- INVALID_RECORD
  90 -> ECFatal -- PRODUCER_FENCED (KIP-360)
  -- Default: retriable. The JVM client treats unknown error
  -- codes as retriable so a future broker upgrade doesn't
  -- crash old clients.
  _ -> ECRetriable


isRetriable, isAbortable, isFatal :: Int16 -> Bool
isRetriable c = classify c == ECRetriable
isAbortable c = classify c == ECAbortable
isFatal c = classify c == ECFatal


{- | Human-readable label per KIP-1054 (consistent error
messages). The mapping uses the canonical Kafka enum names
so logs port across clients.
-}
errorMessage :: Int16 -> Text
errorMessage = \case
  0 -> "NONE"
  1 -> "OFFSET_OUT_OF_RANGE"
  2 -> "CORRUPT_MESSAGE"
  3 -> "UNKNOWN_TOPIC_OR_PARTITION"
  4 -> "INVALID_FETCH_SIZE"
  5 -> "LEADER_NOT_AVAILABLE"
  6 -> "NOT_LEADER_OR_FOLLOWER"
  7 -> "REQUEST_TIMED_OUT"
  8 -> "BROKER_NOT_AVAILABLE"
  9 -> "REPLICA_NOT_AVAILABLE"
  10 -> "MESSAGE_TOO_LARGE"
  11 -> "STALE_CONTROLLER_EPOCH"
  12 -> "OFFSET_METADATA_TOO_LARGE"
  13 -> "NETWORK_EXCEPTION"
  14 -> "COORDINATOR_LOAD_IN_PROGRESS"
  15 -> "COORDINATOR_NOT_AVAILABLE"
  16 -> "NOT_COORDINATOR"
  17 -> "INVALID_TOPIC_EXCEPTION"
  18 -> "RECORD_LIST_TOO_LARGE"
  19 -> "NOT_ENOUGH_REPLICAS"
  20 -> "NOT_ENOUGH_REPLICAS_AFTER_APPEND"
  21 -> "INVALID_REQUIRED_ACKS"
  22 -> "ILLEGAL_GENERATION"
  23 -> "INCONSISTENT_GROUP_PROTOCOL"
  24 -> "INVALID_GROUP_ID"
  25 -> "UNKNOWN_MEMBER_ID"
  26 -> "INVALID_SESSION_TIMEOUT"
  27 -> "REBALANCE_IN_PROGRESS"
  28 -> "INVALID_COMMIT_OFFSET_SIZE"
  29 -> "TOPIC_AUTHORIZATION_FAILED"
  30 -> "GROUP_AUTHORIZATION_FAILED"
  31 -> "CLUSTER_AUTHORIZATION_FAILED"
  32 -> "INVALID_TIMESTAMP"
  33 -> "UNSUPPORTED_SASL_MECHANISM"
  34 -> "ILLEGAL_SASL_STATE"
  35 -> "UNSUPPORTED_VERSION"
  36 -> "TOPIC_ALREADY_EXISTS"
  37 -> "INVALID_PARTITIONS"
  38 -> "INVALID_REPLICATION_FACTOR"
  39 -> "INVALID_REPLICA_ASSIGNMENT"
  40 -> "INVALID_CONFIG"
  41 -> "NOT_CONTROLLER"
  42 -> "INVALID_REQUEST"
  43 -> "UNSUPPORTED_FOR_MESSAGE_FORMAT"
  44 -> "POLICY_VIOLATION"
  45 -> "OUT_OF_ORDER_SEQUENCE_NUMBER"
  46 -> "DUPLICATE_SEQUENCE_NUMBER"
  47 -> "INVALID_PRODUCER_EPOCH"
  48 -> "INVALID_TXN_STATE"
  49 -> "INVALID_PRODUCER_ID_MAPPING"
  50 -> "INVALID_TRANSACTION_TIMEOUT"
  51 -> "CONCURRENT_TRANSACTIONS"
  52 -> "TRANSACTION_COORDINATOR_FENCED"
  53 -> "TRANSACTIONAL_ID_AUTHORIZATION_FAILED"
  54 -> "SECURITY_DISABLED"
  55 -> "OPERATION_NOT_ATTEMPTED"
  56 -> "KAFKA_STORAGE_ERROR"
  57 -> "LOG_DIR_NOT_FOUND"
  58 -> "SASL_AUTHENTICATION_FAILED"
  59 -> "UNKNOWN_PRODUCER_ID"
  60 -> "REASSIGNMENT_IN_PROGRESS"
  72 -> "LISTENER_NOT_FOUND"
  73 -> "TOPIC_DELETION_DISABLED"
  74 -> "FENCED_LEADER_EPOCH"
  75 -> "UNKNOWN_LEADER_EPOCH"
  76 -> "UNSUPPORTED_COMPRESSION_TYPE"
  78 -> "OFFSET_NOT_AVAILABLE"
  79 -> "MEMBER_ID_REQUIRED"
  80 -> "PREFERRED_LEADER_NOT_AVAILABLE"
  81 -> "GROUP_MAX_SIZE_REACHED"
  82 -> "FENCED_INSTANCE_ID"
  83 -> "ELIGIBLE_LEADERS_NOT_AVAILABLE"
  84 -> "ELECTION_NOT_NEEDED"
  85 -> "NO_REASSIGNMENT_IN_PROGRESS"
  86 -> "GROUP_SUBSCRIBED_TO_TOPIC"
  87 -> "INVALID_RECORD"
  88 -> "UNSTABLE_OFFSET_COMMIT"
  89 -> "THROTTLING_QUOTA_EXCEEDED"
  90 -> "PRODUCER_FENCED"
  91 -> "RESOURCE_NOT_FOUND"
  102 -> "BROKER_ID_NOT_REGISTERED"
  103 -> "INCONSISTENT_TOPIC_ID"
  105 -> "TRANSACTIONAL_ID_NOT_FOUND"
  106 -> "FETCH_SESSION_TOPIC_ID_ERROR"
  107 -> "INELIGIBLE_REPLICA"
  c -> "UNKNOWN_KAFKA_ERROR_" <> tshow c
  where
    tshow :: Int16 -> Text
    tshow = T.pack . show
