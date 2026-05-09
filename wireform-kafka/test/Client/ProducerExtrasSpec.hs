{-# LANGUAGE OverloadedStrings #-}

module Client.ProducerExtrasSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.ProducerExtras as PE

tests :: TestTree
tests = testGroup "ProducerExtras"
  [ testCase "transactionalIdOptional override wins"
      txn_override
  , testCase "transactionalIdOptional defaults to prefix-suffix"
      txn_default
  , testCase "classifyTxnError: retriable code -> Retry"
      classify_retry
  , testCase "classifyTxnError: abortable code -> Abort"
      classify_abort
  , testCase "classifyTxnError: fatal code -> Fatal"
      classify_fatal
  , testCase "effectiveTxnDeadlineMs: explicit deadline wins"
      explicit_deadline
  , testCase "effectiveTxnDeadlineMs: producer default fallback"
      default_deadline
  ]

txn_override :: IO ()
txn_override = PE.transactionalIdOptional (Just "myid") "app" "host1@1" @?= "myid"

txn_default :: IO ()
txn_default = PE.transactionalIdOptional Nothing "app" "host1@1" @?= "app-host1@1"

classify_retry :: IO ()
classify_retry = PE.classifyTxnError 7 @?= PE.TxnRecoverByRetry  -- REQUEST_TIMED_OUT

classify_abort :: IO ()
classify_abort = PE.classifyTxnError 51 @?= PE.TxnRecoverByAbort -- INVALID_TXN_STATE

classify_fatal :: IO ()
classify_fatal = PE.classifyTxnError 37 @?= PE.TxnRecoverFatal   -- TRANSACTIONAL_ID_AUTHORIZATION_FAILED

explicit_deadline :: IO ()
explicit_deadline = PE.effectiveTxnDeadlineMs 1000 60_000 (PE.TxnDeadlineMs 5000) @?= 6000

default_deadline :: IO ()
default_deadline = PE.effectiveTxnDeadlineMs 1000 60_000 PE.TxnUseProducerDefault @?= 61_000
