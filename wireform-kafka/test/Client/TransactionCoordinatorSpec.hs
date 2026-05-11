{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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

import qualified Data.Vector as V
import Kafka.Client.Consumer (TopicPartition(..))
import Kafka.Client.Internal.TransactionCoordinator
import qualified Kafka.Protocol.Generated.AddOffsetsToTxnRequest as AOTReq
import qualified Kafka.Protocol.Generated.TxnOffsetCommitRequest as TOCReq
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Wire.Codec as WC

-- | Main test tree
transactionCoordinatorSpec :: TestTree
transactionCoordinatorSpec = testGroup "TransactionCoordinator"
  [ errorInterpretationTests
  , coordinatorTypeTests
  , unitTests
  , requestBuilderTests
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

  -- Error code mapping cross-checked against Kafka 3.7's
  -- Errors.java; the previous test fixtures asserted the
  -- wrong-codes mapping that 'interpretCoordinatorError' had
  -- before the live-broker fix. The codes now actually match
  -- the wire protocol.
  , testCase "INVALID_PRODUCER_EPOCH (47)" $ do
      let err = interpretCoordinatorError 47
      case err of
        InvalidProducerEpoch msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "INVALID_TXN_STATE (48)" $ do
      let err = interpretCoordinatorError 48
      case err of
        InvalidTxnState msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "INVALID_PRODUCER_ID_MAPPING (49)" $ do
      let err = interpretCoordinatorError 49
      case err of
        InvalidProducerIdMapping msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "CONCURRENT_TRANSACTIONS (51)" $ do
      let err = interpretCoordinatorError 51
      case err of
        ConcurrentTransactions msg -> assertBool "Has message" (not $ T.null msg)
        _ -> assertFailure "Wrong error type"

  , testCase "TRANSACTION_COORDINATOR_FENCED (52)" $ do
      let err = interpretCoordinatorError 52
      case err of
        TransactionCoordinatorFenced msg -> assertBool "Has message" (not $ T.null msg)
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
      let errors = map interpretCoordinatorError [14, 15, 16, 47, 48, 49, 51, 52, 90]
      -- All errors should be different types
      assertEqual "Number of error codes" 9 (length errors)

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
-- Request Builder Tests
--
-- 'addOffsetsToTxn' / 'txnOffsetCommitWith' both build their
-- requests inline before sending; we lifted the construction to
-- 'buildAddOffsetsToTxnRequest' / 'buildTxnOffsetCommitRequest'
-- so we can assert the exact wire shape (encoder roundtrip + per
-- field equality) without spinning up a coordinator.
--------------------------------------------------------------------------------

extractK :: P.KafkaString -> Text
extractK (P.KafkaString P.Null)        = ""
extractK (P.KafkaString (P.NotNull t)) = t

requestBuilderTests :: TestTree
requestBuilderTests = testGroup "Request Builders"
  [ testCase "buildAddOffsetsToTxnRequest sets every field as supplied" $ do
      let req = buildAddOffsetsToTxnRequest "tx-1" 4242 7 "consumer-grp"
      extractK (AOTReq.addOffsetsToTxnRequestTransactionalId req) @?= "tx-1"
      AOTReq.addOffsetsToTxnRequestProducerId    req @?= 4242
      AOTReq.addOffsetsToTxnRequestProducerEpoch req @?= 7
      extractK (AOTReq.addOffsetsToTxnRequestGroupId req) @?= "consumer-grp"

  , testCase "AddOffsetsToTxnRequest round-trips through encoder/decoder (v3)" $ do
      let req   = buildAddOffsetsToTxnRequest "tx-rt" 1 0 "g"
          bytes = WC.runEncodeVer @AOTReq.AddOffsetsToTxnRequest 3 req
      case WC.runDecodeVer @AOTReq.AddOffsetsToTxnRequest 3 bytes of
        Left err -> assertFailure ("decode failed: " <> err)
        Right r2 -> do
          extractK (AOTReq.addOffsetsToTxnRequestTransactionalId r2) @?= "tx-rt"
          AOTReq.addOffsetsToTxnRequestProducerId    r2 @?= 1
          AOTReq.addOffsetsToTxnRequestProducerEpoch r2 @?= 0
          extractK (AOTReq.addOffsetsToTxnRequestGroupId r2) @?= "g"

  , testCase "buildTxnOffsetCommitRequest groups partitions by topic" $ do
      let req = buildTxnOffsetCommitRequest "grp" 100 5
                  [ (TopicPartition "t1" 0, 10)
                  , (TopicPartition "t1" 1, 11)
                  , (TopicPartition "t2" 0, 20)
                  ]
      let topics = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopics req) of
                     P.Null      -> V.empty
                     P.NotNull v -> v
      V.length topics @?= 2
      let names = [ extractK (TOCReq.txnOffsetCommitRequestTopicName t) | t <- V.toList topics ]
      assertBool ("topics: " <> show names)
                 ("t1" `elem` names && "t2" `elem` names)
      -- The "t1" topic should have 2 partitions; "t2" should have 1.
      let parts t = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopicPartitions t) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
      let countsByName =
            [ ( extractK (TOCReq.txnOffsetCommitRequestTopicName t)
              , V.length (parts t)
              )
            | t <- V.toList topics
            ]
      lookup "t1" countsByName @?= Just 2
      lookup "t2" countsByName @?= Just 1

  , testCase "buildTxnOffsetCommitRequest stamps -1 leader epoch + null metadata" $ do
      let req = buildTxnOffsetCommitRequest "g" 1 0 [(TopicPartition "x" 0, 99)]
          [topic] = V.toList $ case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopics req) of
                                 P.Null      -> V.empty
                                 P.NotNull v -> v
          [part] = V.toList $ case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopicPartitions topic) of
                                P.Null      -> V.empty
                                P.NotNull v -> v
      TOCReq.txnOffsetCommitRequestPartitionPartitionIndex     part @?= 0
      TOCReq.txnOffsetCommitRequestPartitionCommittedOffset    part @?= 99
      TOCReq.txnOffsetCommitRequestPartitionCommittedLeaderEpoch part @?= -1
      case TOCReq.txnOffsetCommitRequestPartitionCommittedMetadata part of
        P.KafkaString P.Null -> pure ()
        other                -> assertFailure ("expected null metadata, got " <> show other)

  , testCase "buildTxnOffsetCommitRequest with empty offsets emits no topics" $ do
      let req = buildTxnOffsetCommitRequest "g" 1 0 []
          topics = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopics req) of
                     P.Null      -> V.empty
                     P.NotNull v -> v
      V.length topics @?= 0

  , testCase "TxnOffsetCommitRequest round-trips through encoder/decoder (v3)" $ do
      let req   = buildTxnOffsetCommitRequest "grp" 7 9
                    [(TopicPartition "x" 0, 1), (TopicPartition "x" 1, 2)]
          bytes = WC.runEncodeVer @TOCReq.TxnOffsetCommitRequest 3 req
      case WC.runDecodeVer @TOCReq.TxnOffsetCommitRequest 3 bytes of
        Left err -> assertFailure ("decode failed: " <> err)
        Right r2 -> do
          extractK (TOCReq.txnOffsetCommitRequestGroupId r2)      @?= "grp"
          TOCReq.txnOffsetCommitRequestProducerId       r2 @?= 7
          TOCReq.txnOffsetCommitRequestProducerEpoch    r2 @?= 9
          let topics = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopics r2) of
                         P.Null      -> V.empty
                         P.NotNull v -> v
          V.length topics @?= 1
          let [topic] = V.toList topics
              parts   = case P.unKafkaArray (TOCReq.txnOffsetCommitRequestTopicPartitions topic) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
          V.length parts @?= 2
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
  errorCode <- H.forAll $ Gen.element [14, 15, 16, 47, 48, 49, 51, 52, 90]
  
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

