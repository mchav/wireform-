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

retriable :: IO ()
retriable = mapM_ (\c -> RC.classify c @?= RC.ECRetriable)
  [1, 3, 5, 6, 7, 11, 13, 14, 15, 16, 19, 41, 74]

abortable :: IO ()
abortable = mapM_ (\c -> RC.classify c @?= RC.ECAbortable)
  [10, 47, 48, 49, 50, 51]

fatal :: IO ()
fatal = mapM_ (\c -> RC.classify c @?= RC.ECFatal)
  [4, 17, 29, 30, 31, 37, 38, 85]

unknown_retriable :: IO ()
unknown_retriable = RC.classify 9999 @?= RC.ECRetriable

messages :: IO ()
messages = do
  RC.errorMessage 0  @?= "NONE"
  RC.errorMessage 7  @?= "REQUEST_TIMED_OUT"
  RC.errorMessage 51 @?= "INVALID_TXN_STATE"
  RC.errorMessage 75 @?= "PRODUCER_FENCED"
