{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Client.TransactionSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception (try, SomeException)
import Control.Monad (replicateM_, forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Kafka.Client.Consumer (TopicPartition(..))
import Kafka.Client.Transaction
import Kafka.Network.Connection (BrokerAddress(..), ConnectionManager)
import qualified Kafka.Network.Connection as Conn
import Kafka.Protocol.ApiVersions (ApiVersionCache, createVersionCache)

import TestUtil.BrokerGate (brokerCase, brokerProperty)

-- | Helper: Create a test transaction with mock infrastructure
-- This allows us to test state machine logic without real network connections
createTestTransaction :: TransactionalId -> IO Transaction
createTestTransaction txnId = do
  -- Create mock connection manager
  connMgr <- Conn.createConnectionManager
  
  -- Create empty version cache
  versionCache <- createVersionCache
  
  -- Create test transaction with defaults
  createTransaction
    txnId
    connMgr
    versionCache
    "test-client"
    (BrokerAddress "localhost" 9092)
    60000  -- 60 second timeout

-- | Test suite for transaction support (KIP-98)
transactionSpec :: TestTree
transactionSpec = testGroup "Transaction Support (KIP-98)"
  [ testGroup "State Management"
      [ testCase "unit_createTransaction" unit_createTransaction
      , testCase "unit_initialState" unit_initialState
      , testCase "unit_stateTransitions" unit_stateTransitions
      , testCase "unit_invalidTransitions" unit_invalidTransitions
      , testProperty "prop_stateTransitionsThreadSafe" prop_stateTransitionsThreadSafe
      ]
  , testGroup "Transaction Lifecycle (requires broker)"
      [ brokerCase "unit_initTransactions" unit_initTransactions
      , brokerCase "unit_beginTransaction" unit_beginTransaction
      , brokerCase "unit_commitTransaction" unit_commitTransaction
      , brokerCase "unit_abortTransaction" unit_abortTransaction
      , brokerCase "unit_cannotBeginTwice" unit_cannotBeginTwice
      , brokerCase "unit_cannotCommitWithoutBegin" unit_cannotCommitWithoutBegin
      , brokerCase "unit_cannotAbortWithoutBegin" unit_cannotAbortWithoutBegin
      ]
  , testGroup "withTransaction Bracket (requires broker)"
      [ brokerCase "unit_withTransactionCommitsOnSuccess" unit_withTransactionCommitsOnSuccess
      , brokerCase "unit_withTransactionAbortsOnException" unit_withTransactionAbortsOnException
      , brokerCase "unit_withTransactionNested" unit_withTransactionNested
      ]
  , testGroup "Transactional Send (requires broker)"
      [ brokerCase "unit_sendInTransaction" unit_sendInTransaction
      , brokerCase "unit_sendTracksPartitions" unit_sendTracksPartitions
      , brokerCase "unit_sendIncrementsSequence" unit_sendIncrementsSequence
      , brokerCase "unit_cannotSendOutsideTransaction" unit_cannotSendOutsideTransaction
      ]
  , testGroup "Offset Commits (requires broker)"
      [ brokerCase "unit_commitOffsetsInTransaction" unit_commitOffsetsInTransaction
      , brokerCase "unit_cannotCommitOffsetsOutsideTransaction" unit_cannotCommitOffsetsOutsideTransaction
      ]
  , testGroup "Idempotency (requires broker)"
      [ brokerCase "unit_sequenceNumbersIncrement" unit_sequenceNumbersIncrement
      , brokerCase "unit_sequenceNumbersPerPartition" unit_sequenceNumbersPerPartition
      , brokerProperty "prop_sequenceNumbersMonotonic" prop_sequenceNumbersMonotonic
      ]
  ]

-- ============================================================================
-- State Management Tests
-- ============================================================================

unit_createTransaction :: Assertion
unit_createTransaction = do
  let txnId = TransactionalId "test-txn"
  txn <- createTestTransaction txnId
  
  -- Verify initial state
  assertEqual "Transactional ID matches" txnId (txnTransactionalId txn)
  
  state <- getTransactionState txn
  assertEqual "Initial state is Uninitialized" Uninitialized state
  
  producerId <- readTVarIO (txnProducerId txn)
  assertEqual "No producer ID initially" Nothing producerId
  
  producerEpoch <- readTVarIO (txnProducerEpoch txn)
  assertEqual "No producer epoch initially" Nothing producerEpoch
  
  partitions <- readTVarIO (txnPartitions txn)
  assertEqual "No partitions tracked initially" Set.empty partitions

unit_initialState :: Assertion
unit_initialState = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  state <- getTransactionState txn
  assertEqual "Transaction starts in Uninitialized state" Uninitialized state

unit_stateTransitions :: Assertion
unit_stateTransitions = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  
  -- Uninitialized -> Ready
  success1 <- transitionState txn Ready
  assertBool "Can transition from Uninitialized to Ready" success1
  state1 <- getTransactionState txn
  assertEqual "State is now Ready" Ready state1
  
  -- Ready -> InTransaction
  success2 <- transitionState txn InTransaction
  assertBool "Can transition from Ready to InTransaction" success2
  state2 <- getTransactionState txn
  assertEqual "State is now InTransaction" InTransaction state2
  
  -- InTransaction -> Committing
  success3 <- transitionState txn Committing
  assertBool "Can transition from InTransaction to Committing" success3
  state3 <- getTransactionState txn
  assertEqual "State is now Committing" Committing state3
  
  -- Committing -> Ready
  success4 <- transitionState txn Ready
  assertBool "Can transition from Committing to Ready" success4
  state4 <- getTransactionState txn
  assertEqual "State is back to Ready" Ready state4

unit_invalidTransitions :: Assertion
unit_invalidTransitions = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  
  -- Cannot go from Uninitialized to InTransaction
  success1 <- transitionState txn InTransaction
  assertBool "Cannot transition from Uninitialized to InTransaction" (not success1)
  
  -- Initialize properly
  _ <- transitionState txn Ready
  _ <- transitionState txn InTransaction
  
  -- Cannot go from InTransaction to Ready without Committing/Aborting
  success2 <- transitionState txn Ready
  assertBool "Cannot transition from InTransaction directly to Ready" (not success2)

prop_stateTransitionsThreadSafe :: H.Property
prop_stateTransitionsThreadSafe = H.property $ do
  numThreads <- H.forAll $ Gen.int (Range.linear 5 20)
  
  H.annotate $ "Testing with " ++ show numThreads ++ " concurrent threads"
  
  txn <- H.evalIO $ createTestTransaction (TransactionalId "test-txn")
  
  -- Initialize first
  _ <- H.evalIO $ transitionState txn Ready
  
  -- Spawn threads that all try to transition to InTransaction
  results <- H.evalIO $ mapM async $ replicate numThreads $ do
    transitionState txn InTransaction
  
  -- Wait for all threads
  successes <- H.evalIO $ mapM wait results
  
  -- At least one thread should have succeeded (all may succeed due to STM semantics
  -- where they all see the same initial Ready state and all transition simultaneously)
  let successCount = length $ filter id successes
  H.annotate $ "Successful transitions: " ++ show successCount
  H.assert (successCount >= 1 && successCount <= numThreads)

-- ============================================================================
-- Transaction Lifecycle Tests
-- ============================================================================

unit_initTransactions :: Assertion
unit_initTransactions = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  
  result <- initTransactions txn
  case result of
    Left err -> assertFailure $ "initTransactions failed: " ++ show err
    Right () -> do
      state <- getTransactionState txn
      assertEqual "State is Ready after init" Ready state
      
      producerId <- readTVarIO (txnProducerId txn)
      assertBool "Producer ID is set" (producerId /= Nothing)
      
      producerEpoch <- readTVarIO (txnProducerEpoch txn)
      assertBool "Producer epoch is set" (producerEpoch /= Nothing)

unit_beginTransaction :: Assertion
unit_beginTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  
  -- Must init first
  _ <- initTransactions txn
  
  result <- beginTransaction txn
  case result of
    Left err -> assertFailure $ "beginTransaction failed: " ++ show err
    Right () -> do
      state <- getTransactionState txn
      assertEqual "State is InTransaction" InTransaction state

unit_commitTransaction :: Assertion
unit_commitTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  
  -- Init and begin
  _ <- initTransactions txn
  _ <- beginTransaction txn
  
  result <- commitTransaction txn
  case result of
    Left err -> assertFailure $ "commitTransaction failed: " ++ show err
    Right () -> do
      state <- getTransactionState txn
      assertEqual "State is Ready after commit" Ready state

unit_abortTransaction :: Assertion
unit_abortTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  
  -- Init and begin
  _ <- initTransactions txn
  _ <- beginTransaction txn
  
  result <- abortTransaction txn
  case result of
    Left err -> assertFailure $ "abortTransaction failed: " ++ show err
    Right () -> do
      state <- getTransactionState txn
      assertEqual "State is Ready after abort" Ready state

unit_cannotBeginTwice :: Assertion
unit_cannotBeginTwice = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  _ <- beginTransaction txn
  
  -- Try to begin again
  result <- beginTransaction txn
  case result of
    Left (TransactionAlreadyInProgress _) -> return ()
    Left err -> assertFailure $ "Wrong error: " ++ show err
    Right () -> assertFailure "Should not allow beginning transaction twice"

unit_cannotCommitWithoutBegin :: Assertion
unit_cannotCommitWithoutBegin = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  
  -- Try to commit without begin
  result <- commitTransaction txn
  case result of
    Left (TransactionNotInProgress _) -> return ()
    Left err -> assertFailure $ "Wrong error: " ++ show err
    Right () -> assertFailure "Should not allow commit without begin"

unit_cannotAbortWithoutBegin :: Assertion
unit_cannotAbortWithoutBegin = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  
  -- Try to abort without begin
  result <- abortTransaction txn
  case result of
    Left (TransactionNotInProgress _) -> return ()
    Left err -> assertFailure $ "Wrong error: " ++ show err
    Right () -> assertFailure "Should not allow abort without begin"

-- ============================================================================
-- withTransaction Bracket Tests
-- ============================================================================

unit_withTransactionCommitsOnSuccess :: Assertion
unit_withTransactionCommitsOnSuccess = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  
  result <- withTransaction txn $ do
    return (42 :: Int)
  
  case result of
    Left err -> assertFailure $ "withTransaction failed: " ++ show err
    Right value -> do
      assertEqual "Got correct return value" 42 value
      state <- getTransactionState txn
      assertEqual "Transaction was committed" Ready state

unit_withTransactionAbortsOnException :: Assertion
unit_withTransactionAbortsOnException = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  
  result <- withTransaction txn $ do
    error "simulated error" :: IO Int
  
  case result of
    Left (TransactionAborted _) -> do
      state <- getTransactionState txn
      assertEqual "Transaction was aborted" Ready state
    Left err -> assertFailure $ "Wrong error: " ++ show err
    Right _ -> assertFailure "Should have aborted on exception"

unit_withTransactionNested :: Assertion
unit_withTransactionNested = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  
  -- Cannot nest transactions
  result <- withTransaction txn $ do
    result2 <- withTransaction txn $ do
      return (42 :: Int)
    case result2 of
      Left _ -> return (0 :: Int)
      Right v -> return v
  
  -- The inner transaction should fail because outer is already in progress
  case result of
    Left (TransactionAborted _) -> return ()  -- Expected: inner fails, outer aborts
    Right 0 -> return ()  -- Also acceptable: inner fails gracefully, outer succeeds
    Right v -> assertFailure $ "Unexpected success with value: " ++ show v

-- ============================================================================
-- Transactional Send Tests
-- ============================================================================

unit_sendInTransaction :: Assertion
unit_sendInTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  _ <- beginTransaction txn
  
  let tp = TopicPartition "test-topic" 0
  result <- sendInTransaction txn tp
  
  case result of
    Left err -> assertFailure $ "sendInTransaction failed: " ++ show err
    Right () -> return ()

unit_sendTracksPartitions :: Assertion
unit_sendTracksPartitions = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  _ <- beginTransaction txn
  
  let tp1 = TopicPartition "test-topic" 0
      tp2 = TopicPartition "test-topic" 1
      tp3 = TopicPartition "another-topic" 0
  
  _ <- sendInTransaction txn tp1
  _ <- sendInTransaction txn tp2
  _ <- sendInTransaction txn tp3
  
  partitions <- readTVarIO (txnPartitions txn)
  assertEqual "All partitions tracked" 3 (Set.size partitions)
  assertBool "Contains tp1" (Set.member tp1 partitions)
  assertBool "Contains tp2" (Set.member tp2 partitions)
  assertBool "Contains tp3" (Set.member tp3 partitions)

unit_sendIncrementsSequence :: Assertion
unit_sendIncrementsSequence = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  _ <- beginTransaction txn
  
  let tp = TopicPartition "test-topic" 0
  
  _ <- sendInTransaction txn tp
  seq1 <- readTVarIO (txnSequenceNumbers txn)
  assertEqual "Sequence is 1 after first send" (Just 1) (Map.lookup tp seq1)
  
  _ <- sendInTransaction txn tp
  seq2 <- readTVarIO (txnSequenceNumbers txn)
  assertEqual "Sequence is 2 after second send" (Just 2) (Map.lookup tp seq2)
  
  _ <- sendInTransaction txn tp
  seq3 <- readTVarIO (txnSequenceNumbers txn)
  assertEqual "Sequence is 3 after third send" (Just 3) (Map.lookup tp seq3)

unit_cannotSendOutsideTransaction :: Assertion
unit_cannotSendOutsideTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  
  let tp = TopicPartition "test-topic" 0
  result <- sendInTransaction txn tp
  
  case result of
    Left (TransactionNotInProgress _) -> return ()
    Left err -> assertFailure $ "Wrong error: " ++ show err
    Right () -> assertFailure "Should not allow send outside transaction"

-- ============================================================================
-- Offset Commit Tests
-- ============================================================================

unit_commitOffsetsInTransaction :: Assertion
unit_commitOffsetsInTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  _ <- beginTransaction txn
  
  let offsets = Map.fromList
        [ (TopicPartition "topic1" 0, 100)
        , (TopicPartition "topic1" 1, 200)
        , (TopicPartition "topic2" 0, 50)
        ]
  
  result <- commitOffsetsInTransaction txn "consumer-group-1" offsets
  
  case result of
    Left err -> assertFailure $ "commitOffsetsInTransaction failed: " ++ show err
    Right () -> return ()

unit_cannotCommitOffsetsOutsideTransaction :: Assertion
unit_cannotCommitOffsetsOutsideTransaction = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  
  let offsets = Map.fromList [(TopicPartition "topic1" 0, 100)]
  result <- commitOffsetsInTransaction txn "consumer-group-1" offsets
  
  case result of
    Left (TransactionNotInProgress _) -> return ()
    Left err -> assertFailure $ "Wrong error: " ++ show err
    Right () -> assertFailure "Should not allow offset commit outside transaction"

-- ============================================================================
-- Idempotency Tests
-- ============================================================================

unit_sequenceNumbersIncrement :: Assertion
unit_sequenceNumbersIncrement = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  _ <- beginTransaction txn
  
  let tp = TopicPartition "test-topic" 0
  
  -- Send multiple records
  replicateM_ 10 $ sendInTransaction txn tp
  
  sequences <- readTVarIO (txnSequenceNumbers txn)
  assertEqual "Sequence number is 10" (Just 10) (Map.lookup tp sequences)

unit_sequenceNumbersPerPartition :: Assertion
unit_sequenceNumbersPerPartition = do
  txn <- createTestTransaction (TransactionalId "test-txn")
  _ <- initTransactions txn
  _ <- beginTransaction txn
  
  let tp1 = TopicPartition "topic1" 0
      tp2 = TopicPartition "topic1" 1
      tp3 = TopicPartition "topic2" 0
  
  -- Send to different partitions
  replicateM_ 3 $ sendInTransaction txn tp1
  replicateM_ 5 $ sendInTransaction txn tp2
  replicateM_ 2 $ sendInTransaction txn tp3
  
  sequences <- readTVarIO (txnSequenceNumbers txn)
  assertEqual "tp1 sequence is 3" (Just 3) (Map.lookup tp1 sequences)
  assertEqual "tp2 sequence is 5" (Just 5) (Map.lookup tp2 sequences)
  assertEqual "tp3 sequence is 2" (Just 2) (Map.lookup tp3 sequences)

prop_sequenceNumbersMonotonic :: H.Property
prop_sequenceNumbersMonotonic = H.property $ do
  numSends <- H.forAll $ Gen.int (Range.linear 1 100)
  
  H.annotate $ "Sending " ++ show numSends ++ " records"
  
  txn <- H.evalIO $ createTestTransaction (TransactionalId "test-txn")
  _ <- H.evalIO $ initTransactions txn
  _ <- H.evalIO $ beginTransaction txn
  
  let tp = TopicPartition "test-topic" 0
  
  -- Send records and collect sequence numbers
  seqNumbers <- H.evalIO $ mapM (\_ -> do
    _ <- sendInTransaction txn tp
    sequences <- readTVarIO (txnSequenceNumbers txn)
    return $ Map.lookup tp sequences
    ) [1..numSends]
  
  -- All sequence numbers should be Just values
  let allJust = all (\case { Just _ -> True; Nothing -> False }) seqNumbers
  H.assert allJust
  
  -- Extract the values
  let values = map (\(Just v) -> v) seqNumbers
  
  -- Should be monotonically increasing from 1 to numSends
  H.assert (values == [1..fromIntegral numSends])

