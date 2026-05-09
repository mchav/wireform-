{-# LANGUAGE OverloadedStrings #-}

module Client.RetryClassifierSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.RetryClassifier as RC

tests :: TestTree
tests = testGroup "Retry classifier (KIP-487 / 1054)"
  [ testCase "code 0 -> ECNoError"
      no_error
  , testCase "transient codes are retriable"
      retriable
  , testCase "transactional / payload codes are abortable"
      abortable
  , testCase "auth + invalid-record codes are fatal"
      fatal
  , testCase "unknown codes default to retriable (forward-compat)"
      unknown_retriable
  , testCase "errorMessage uses canonical Kafka enum names"
      messages
  ]

no_error :: IO ()
no_error = RC.classify 0 @?= RC.ECNoError

-- | Codes the JVM client treats as transient broker / coordinator
-- state. Sourced from
-- @org.apache.kafka.common.protocol.Errors@ + the @RetriableException@
-- hierarchy (see KIP-487).
retriable :: IO ()
retriable = mapM_ (\c -> RC.classify c @?= RC.ECRetriable)
  [ 1   -- OFFSET_OUT_OF_RANGE
  , 3   -- UNKNOWN_TOPIC_OR_PARTITION
  , 5   -- LEADER_NOT_AVAILABLE
  , 6   -- NOT_LEADER_OR_FOLLOWER
  , 7   -- REQUEST_TIMED_OUT
  , 13  -- NETWORK_EXCEPTION
  , 14  -- COORDINATOR_LOAD_IN_PROGRESS
  , 15  -- COORDINATOR_NOT_AVAILABLE
  , 16  -- NOT_COORDINATOR
  , 19  -- NOT_ENOUGH_REPLICAS
  , 41  -- NOT_CONTROLLER
  , 74  -- FENCED_LEADER_EPOCH
  , 75  -- UNKNOWN_LEADER_EPOCH
  , 89  -- THROTTLING_QUOTA_EXCEEDED (KIP-599)
  , 106 -- FETCH_SESSION_TOPIC_ID_ERROR
  ]

-- | Codes that poison the current transaction but allow recovery
-- via abort + restart.
abortable :: IO ()
abortable = mapM_ (\c -> RC.classify c @?= RC.ECAbortable)
  [ 10 -- MESSAGE_TOO_LARGE
  , 47 -- INVALID_PRODUCER_EPOCH
  , 48 -- INVALID_TXN_STATE
  , 49 -- INVALID_PRODUCER_ID_MAPPING
  , 50 -- INVALID_TRANSACTION_TIMEOUT
  , 51 -- CONCURRENT_TRANSACTIONS
  , 52 -- TRANSACTION_COORDINATOR_FENCED
  ]

-- | Codes that require closing the producer.
fatal :: IO ()
fatal = mapM_ (\c -> RC.classify c @?= RC.ECFatal)
  [ 4  -- INVALID_FETCH_SIZE
  , 17 -- INVALID_TOPIC_EXCEPTION
  , 29 -- TOPIC_AUTHORIZATION_FAILED
  , 30 -- GROUP_AUTHORIZATION_FAILED
  , 31 -- CLUSTER_AUTHORIZATION_FAILED
  , 53 -- TRANSACTIONAL_ID_AUTHORIZATION_FAILED (was 37 in older Kafka)
  , 38 -- INVALID_REPLICATION_FACTOR
  , 82 -- FENCED_INSTANCE_ID (KIP-345)
  , 87 -- INVALID_RECORD (was 85 in older Kafka)
  , 90 -- PRODUCER_FENCED (KIP-360)
  ]

unknown_retriable :: IO ()
unknown_retriable = RC.classify 9999 @?= RC.ECRetriable

messages :: IO ()
messages = do
  RC.errorMessage 0  @?= "NONE"
  RC.errorMessage 7  @?= "REQUEST_TIMED_OUT"
  -- Codes 47-53 are the transactional cluster; the previous test
  -- asserted the pre-3.0 layout (where 51 was INVALID_TXN_STATE
  -- and 75 was PRODUCER_FENCED). Apache Kafka 3.7+ moved them to
  -- the layout asserted here.
  RC.errorMessage 47 @?= "INVALID_PRODUCER_EPOCH"
  RC.errorMessage 48 @?= "INVALID_TXN_STATE"
  RC.errorMessage 51 @?= "CONCURRENT_TRANSACTIONS"
  RC.errorMessage 82 @?= "FENCED_INSTANCE_ID"
  RC.errorMessage 87 @?= "INVALID_RECORD"
  RC.errorMessage 89 @?= "THROTTLING_QUOTA_EXCEEDED"
  RC.errorMessage 90 @?= "PRODUCER_FENCED"
  RC.errorMessage 106 @?= "FETCH_SESSION_TOPIC_ID_ERROR"
