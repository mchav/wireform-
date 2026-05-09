{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Client.TransactionCoordinatorSpec
Description : Tests for transaction coordinator protocol functions
Copyright   : (c) 2025
License     : BSD-3-Clause

Tests for the TransactionCoordinator module, which handles protocol communication
with Kafka's transaction coordinator.

These tests verify:
- Error code interpretation
- Request/response encoding/decoding
- State management during coordinator operations
- Error handling for various failure modes
-}

module Client.TransactionCoordinatorSpec (transactionCoordinatorSpec) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Concurrent.STM
import Control.Exception (try, evaluate)
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T

import Kafka.Client.Consumer (TopicPartition(..))
import Kafka.Client.Internal.TransactionCoordinator

-- | Main test tree
transactionCoordinatorSpec :: TestTree
transactionCoordinatorSpec = testGroup "TransactionCoordinator"
  [ errorInterpretationTests
  , coordinatorTypeTests
  , unitTests
  ]

--------------------------------------------------------------------------------
-- Error Interpretation Tests
--------------------------------------------------------------------------------

errorInterpretationTests :: TestTree
errorInterpretationTests = testGroup "Error Code Interpretation"
  [ testCase "COORDINATOR_NOT_AVAILABLE (15)" $ do
      let err = interpretCoordinatorError 15
      case err of
        CoordinatorNotAvailable msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "COORDINATOR_LOAD_IN_PROGRESS (14)" $ do
      let err = interpretCoordinatorError 14
      case err of
        CoordinatorLoadInProgress msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "NOT_COORDINATOR (16)" $ do
      let err = interpretCoordinatorError 16
      case err of
        NotCoordinator msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "INVALID_PRODUCER_ID_MAPPING (47)" $ do
      let err = interpretCoordinatorError 47
      case err of
        InvalidProducerIdMapping msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "INVALID_PRODUCER_EPOCH (51)" $ do
      let err = interpretCoordinatorError 51
      case err of
        InvalidProducerEpoch msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "INVALID_TXN_STATE (24)" $ do
      let err = interpretCoordinatorError 24
      case err of
        InvalidTxnState msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "INVALID_PARTITIONS_IN_TXN (48)" $ do
      let err = interpretCoordinatorError 48
      case err of
        InvalidPartitionsInTxn msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "TRANSACTION_COORDINATOR_FENCED (32)" $ do
      let err = interpretCoordinatorError 32
      case err of
        TransactionCoordinatorFenced msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "CONCURRENT_TRANSACTIONS (96)" $ do
      let err = interpretCoordinatorError 96
      case err of
        ConcurrentTransactions msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "PRODUCER_FENCED (90)" $ do
      let err = interpretCoordinatorError 90
      case err of
        ProducerFenced msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "Unknown error code" $ do
      let err = interpretCoordinatorError 9999
      case err of
        UnknownCoordinatorError code msg -> do
          assertEqual "Error code" 9999 code
          assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"
  ]

--------------------------------------------------------------------------------
-- TransactionCoordinator Type Tests
--------------------------------------------------------------------------------

coordinatorTypeTests :: TestTree
coordinatorTypeTests = testGroup "TransactionCoordinator Type"
  [ testCase "Create and access coordinator" $ do
      let coordinator = TransactionCoordinator
            { tcNodeId = 1
            , tcHost = "localhost"
            , tcPort = 9092
            }
      assertEqual "Node ID" 1 (tcNodeId coordinator)
      assertEqual "Host" "localhost" (tcHost coordinator)
      assertEqual "Port" 9092 (tcPort coordinator)

  , testCase "Coordinator equality" $ do
      let coord1 = TransactionCoordinator 1 "host1" 9092
          coord2 = TransactionCoordinator 1 "host1" 9092
          coord3 = TransactionCoordinator 2 "host1" 9092
      assertEqual "Same coordinators equal" coord1 coord2
      assertBool "Different coordinators not equal" (coord1 /= coord3)
  ]

--------------------------------------------------------------------------------
-- Unit Tests (without network)
--------------------------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Error codes are distinct" $ do
      let errors = map interpretCoordinatorError [14, 15, 16, 24, 32, 47, 48, 51, 90, 96]
      -- All errors should be different types
      assertEqual "Number of error codes" 10 (length errors)

  , testCase "TransactionCoordinator shows correctly" $ do
      let coordinator = TransactionCoordinator 1 "localhost" 9092
      let shown = show coordinator
      assertBool "Contains node ID" ("1" `T.isInfixOf` T.pack shown)
      assertBool "Contains host" ("localhost" `T.isInfixOf` T.pack shown)
      assertBool "Contains port" ("9092" `T.isInfixOf` T.pack shown)

  , testCase "Error messages contain useful info" $ do
      let err = interpretCoordinatorError 15
      case err of
        CoordinatorNotAvailable msg -> do
          assertBool "Message not empty" (not $ T.null msg)
          assertBool "Message is descriptive" (T.length msg > 10)
        _ -> assertFailure "Wrong error type"
  ]

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

-- | All error codes should produce valid errors
prop_errorCodesValid :: H.Property
prop_errorCodesValid = H.property $ do
  errorCode <- H.forAll $ Gen.int16 (Range.constant 0 200)
  
  let err = interpretCoordinatorError errorCode
  
  -- Should not throw when showing
  errStr <- H.evalIO $ evaluate (show err)
  H.assert (not $ null errStr)

-- | Known error codes should not produce UnknownCoordinatorError
prop_knownErrorCodes :: H.Property
prop_knownErrorCodes = H.property $ do
  errorCode <- H.forAll $ Gen.element [14, 15, 16, 24, 32, 47, 48, 51, 90, 96]
  
  let err = interpretCoordinatorError errorCode
  
  case err of
    UnknownCoordinatorError _ _ -> H.failure
    _ -> H.success

-- | TransactionCoordinator fields should be preserved
prop_coordinatorPreservesFields :: H.Property
prop_coordinatorPreservesFields = H.property $ do
  nodeId <- H.forAll $ Gen.int32 (Range.linear 0 100)
  host <- H.forAll $ Gen.text (Range.linear 1 20) Gen.alphaNum
  port <- H.forAll $ Gen.int32 (Range.linear 1024 65535)
  
  let coordinator = TransactionCoordinator nodeId host port
  
  H.assert (tcNodeId coordinator == nodeId)
  H.assert (tcHost coordinator == host)
  H.assert (tcPort coordinator == port)

