{-# LANGUAGE OverloadedStrings #-}

module Client.TransactionHelpersSpec (tests) where

import Test.Syd

import qualified Kafka.Client.Transaction as PE

tests :: Spec
tests = describe "Transaction helpers" $ sequence_
  [ it "transactionalIdOptional override wins"
      txn_override
  , it "transactionalIdOptional defaults to prefix-suffix"
      txn_default
  , it "classifyTxnError: retriable code -> Retry"
      classify_retry
  , it "classifyTxnError: abortable code -> Abort"
      classify_abort
  , it "classifyTxnError: fatal code -> Fatal"
      classify_fatal
  , it "effectiveTxnDeadlineMs: explicit deadline wins"
      explicit_deadline
  , it "effectiveTxnDeadlineMs: producer default fallback"
      default_deadline
  ]

txn_override :: IO ()
txn_override = PE.transactionalIdOptional (Just "myid") "app" "host1@1" `shouldBe` "myid"

txn_default :: IO ()
txn_default = PE.transactionalIdOptional Nothing "app" "host1@1" `shouldBe` "app-host1@1"

classify_retry :: IO ()
classify_retry = PE.classifyTxnError 7 `shouldBe` PE.TxnRecoverByRetry  -- REQUEST_TIMED_OUT

classify_abort :: IO ()
classify_abort = PE.classifyTxnError 51 `shouldBe` PE.TxnRecoverByAbort -- CONCURRENT_TRANSACTIONS

classify_fatal :: IO ()
classify_fatal = PE.classifyTxnError 53 `shouldBe` PE.TxnRecoverFatal
  -- TRANSACTIONAL_ID_AUTHORIZATION_FAILED. Apache Kafka 3.7+ moved
  -- this code from 37 (where INVALID_PARTITIONS now lives) to 53.

explicit_deadline :: IO ()
explicit_deadline = PE.effectiveTxnDeadlineMs 1000 60_000 (PE.TxnDeadlineMs 5000) `shouldBe` 6000

default_deadline :: IO ()
default_deadline = PE.effectiveTxnDeadlineMs 1000 60_000 PE.TxnUseProducerDefault `shouldBe` 61_000
