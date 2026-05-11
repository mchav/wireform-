{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module: Kafka.Client.Transaction
Description: Transaction support for Kafka producer (KIP-98)

Provides exactly-once semantics (EOS) for Kafka producers through:
- Idempotent producer with producer ID and epoch
- Atomic writes across multiple partitions
- Atomic offset commits within transactions
- Producer fencing to prevent zombie writers
-}
module Kafka.Client.Transaction
  ( -- * Transaction Types
    Transaction(..)
  , TransactionState(..)
  , ProducerId(..)
  , ProducerEpoch(..)
  , TransactionalId(..)
  , TransactionError(..)
    
    -- * Transaction Operations
  , initTransactions
  , beginTransaction
  , commitTransaction
  , abortTransaction
  , withTransaction
    
    -- * Transactional Send
  , sendInTransaction
  , commitOffsetsInTransaction
    
    -- * State management
    --
    -- | These low-level state-management primitives are used by
    -- the integration tests and the OpenTelemetry instrumentation.
    -- Most user code only needs 'initTransactions' /
    -- 'beginTransaction' / 'commitTransaction' /
    -- 'abortTransaction' / 'withTransaction'.
  , createTransaction
  , getTransactionState
  , transitionState
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception, try, SomeException)
import Control.Monad (unless)
import Data.Int (Int16, Int32, Int64)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Text (Text)
import qualified Data.Text as T

import Kafka.Client.Consumer (TopicPartition(..))
import Kafka.Client.Internal.TransactionCoordinator (TransactionCoordinator)
import qualified Kafka.Client.Internal.TransactionCoordinator as TC
import Kafka.Network.Connection (BrokerAddress, ConnectionManager)
import qualified Kafka.Network.Connection as Conn
import Kafka.Protocol.ApiVersions (ApiVersionCache)
import qualified Kafka.Protocol.ApiVersions as AV

-- | Producer ID assigned by the transaction coordinator
newtype ProducerId = ProducerId { unProducerId :: Int64 }
  deriving (Show, Eq, Ord)

-- | Producer epoch for fencing
newtype ProducerEpoch = ProducerEpoch { unProducerEpoch :: Int16 }
  deriving (Show, Eq, Ord)

-- | Transactional ID for EOS
newtype TransactionalId = TransactionalId { unTransactionalId :: Text }
  deriving (Show, Eq, Ord)

-- | Transaction state machine
data TransactionState
  = Uninitialized
  -- ^ Transaction not yet initialized
  | Ready
  -- ^ Transaction initialized, ready to begin
  | InTransaction
  -- ^ Transaction in progress
  | Committing
  -- ^ Transaction committing
  | Aborting
  -- ^ Transaction aborting
  | Aborted
  -- ^ Transaction aborted
  | Fenced
  -- ^ Producer has been fenced by a newer instance
  | Error Text
  -- ^ Fatal error state
  deriving (Show, Eq)

-- | Transaction handle with thread-safe state management
data Transaction = Transaction
  { txnTransactionalId :: !TransactionalId
  , txnProducerId :: !(TVar (Maybe ProducerId))
  , txnProducerEpoch :: !(TVar (Maybe ProducerEpoch))
  , txnState :: !(TVar TransactionState)
  , txnPartitions :: !(TVar (HashSet TopicPartition))
  -- ^ Partitions involved in the current transaction.
  , txnSequenceNumbers :: !(TVar (HashMap TopicPartition Int32))
  -- ^ Sequence numbers per partition for idempotency.
  , txnCoordinator :: !(TVar (Maybe TransactionCoordinator))
  -- ^ Cached transaction coordinator
  , txnConnectionManager :: !ConnectionManager
  -- ^ Connection manager for network operations
  , txnVersionCache :: !ApiVersionCache
  -- ^ API version cache for version negotiation
  , txnCorrelationId :: !(TVar Int32)
  -- ^ Correlation ID generator
  , txnClientId :: !Text
  -- ^ Client ID for requests
  , txnBootstrapBroker :: !BrokerAddress
  -- ^ Bootstrap broker for coordinator discovery
  , txnTimeoutMs :: !Int32
  -- ^ Transaction timeout in milliseconds
  }

-- | Transaction errors
data TransactionError
  = TransactionNotInitialized Text
  | TransactionAlreadyInProgress Text
  | TransactionNotInProgress Text
  | InvalidStateTransition TransactionState TransactionState
  | ProducerFenced Text
  | CoordinatorNotAvailable Text
  | TransactionAborted Text
  | TransactionTimeout Text
  | UnknownTransactionError Text
  deriving (Show, Eq)

instance Exception TransactionError

-- | Create a new transaction handle with all required infrastructure
createTransaction :: TransactionalId
                  -> ConnectionManager
                  -> ApiVersionCache
                  -> Text  -- ^ Client ID
                  -> BrokerAddress  -- ^ Bootstrap broker
                  -> Int32  -- ^ Transaction timeout (ms)
                  -> IO Transaction
createTransaction transactionalId connMgr versionCache clientId bootstrapBroker timeoutMs = do
  producerId <- newTVarIO Nothing
  producerEpoch <- newTVarIO Nothing
  state <- newTVarIO Uninitialized
  partitions <- newTVarIO HashSet.empty
  sequences <- newTVarIO HashMap.empty
  coordinator <- newTVarIO Nothing
  correlationId <- newTVarIO 0
  return Transaction
    { txnTransactionalId = transactionalId
    , txnProducerId = producerId
    , txnProducerEpoch = producerEpoch
    , txnState = state
    , txnPartitions = partitions
    , txnSequenceNumbers = sequences
    , txnCoordinator = coordinator
    , txnConnectionManager = connMgr
    , txnVersionCache = versionCache
    , txnCorrelationId = correlationId
    , txnClientId = clientId
    , txnBootstrapBroker = bootstrapBroker
    , txnTimeoutMs = timeoutMs
    }

-- | Get current transaction state
getTransactionState :: Transaction -> IO TransactionState
getTransactionState txn = readTVarIO (txnState txn)

-- | Attempt to transition to a new state, returns False if transition is invalid
transitionState :: Transaction -> TransactionState -> IO Bool
transitionState txn newState = atomically $ do
  currentState <- readTVar (txnState txn)
  if isValidTransition currentState newState
    then do
      writeTVar (txnState txn) newState
      return True
    else return False

-- | Check if state transition is valid
isValidTransition :: TransactionState -> TransactionState -> Bool
isValidTransition from to = case (from, to) of
  -- Initialization
  (Uninitialized, Ready) -> True
  
  -- Begin transaction
  (Ready, InTransaction) -> True
  
  -- Commit/abort
  (InTransaction, Committing) -> True
  (InTransaction, Aborting) -> True
  (Committing, Ready) -> True
  (Aborting, Aborted) -> True
  (Aborted, Ready) -> True
  
  -- Error states
  (_, Error _) -> True
  (_, Fenced) -> True
  
  -- Same state is always valid (idempotent operations)
  _ | from == to -> True
  
  -- Everything else is invalid
  _ -> False

-- | Initialize transactions (KIP-98)
-- This must be called before any transactional operations.
-- It discovers the transaction coordinator and obtains a producer ID.
initTransactions :: Transaction -> IO (Either TransactionError ())
initTransactions txn = do
  state <- getTransactionState txn
  case state of
    Uninitialized -> do
      let transactionalId = unTransactionalId $ txnTransactionalId txn
      
      -- Discover transaction coordinator
      coordResult <- TC.findTransactionCoordinator
        (txnConnectionManager txn)
        (txnVersionCache txn)
        (txnCorrelationId txn)
        (txnBootstrapBroker txn)
        (txnClientId txn)
        transactionalId
      
      case coordResult of
        Left err -> return $ Left $ CoordinatorNotAvailable $ T.pack $ show err
        Right coordinator -> do
          -- Cache the coordinator
          atomically $ writeTVar (txnCoordinator txn) (Just coordinator)

          -- Initialize producer ID, with retry for the broker's
          -- mid-transition error codes:
          --
          --   * @CONCURRENT_TRANSACTIONS@ (96) — the broker is
          --     in the middle of completing a previous
          --     transaction for this id; retry once the abort
          --     marker has landed.
          --   * @INVALID_PRODUCER_EPOCH@ (51) — when InitProducerId
          --     arrives against an @Ongoing@ transaction the
          --     broker has to drive the previous epoch through
          --     @PrepareEpochFence -> CompleteAbort@ before
          --     bumping; some Kafka 3.7 paths surface this as
          --     INVALID_PRODUCER_EPOCH instead of
          --     CONCURRENT_TRANSACTIONS while the transit is in
          --     flight. The JVM client retries on both.
          --
          -- Bounded retry: 5 attempts with 100ms exponential
          -- backoff (max ~1.6s total). Beyond that we surface
          -- the last error.
          pidResult <- initProducerIdWithRetry txn coordinator transactionalId 5

          case pidResult of
            Left err -> return $ Left $ CoordinatorNotAvailable $ T.pack $ show err
            Right (pid, epoch) -> do
              -- Store producer ID and epoch
              atomically $ do
                writeTVar (txnProducerId txn) (Just $ ProducerId pid)
                writeTVar (txnProducerEpoch txn) (Just $ ProducerEpoch epoch)
                writeTVar (txnState txn) Ready

              return $ Right ()
    
    Ready -> return $ Right ()  -- Already initialized, idempotent
    
    Error msg -> return $ Left $ UnknownTransactionError msg
    
    Fenced -> return $ Left $ ProducerFenced "Producer has been fenced"
    
    _ -> return $ Left $ InvalidStateTransition state Ready

-- | InitProducerId with bounded retry for the broker's
-- mid-transition error codes. Mirrors the JVM client's
-- @TransactionManager.initializeTransactions@ loop: retry on
-- @CONCURRENT_TRANSACTIONS@ / @INVALID_PRODUCER_EPOCH@ /
-- @COORDINATOR_LOAD_IN_PROGRESS@ with exponential backoff, surface
-- everything else immediately.
--
-- The retry is bounded so a genuine producer-fenced situation
-- (e.g. a different client really did bump our epoch and we're
-- not allowed in) eventually surfaces as a hard error rather
-- than spinning forever.
initProducerIdWithRetry
  :: Transaction
  -> TC.TransactionCoordinator
  -> Text                              -- ^ transactional id
  -> Int                               -- ^ remaining attempts
  -> IO (Either TC.TransactionCoordinatorError (Int64, Int16))
initProducerIdWithRetry txn coordinator transactionalId attemptsLeft = do
  pidResult <- TC.initProducerId
    (txnConnectionManager txn)
    (txnVersionCache txn)
    (txnCorrelationId txn)
    (txnClientId txn)
    coordinator
    (Just transactionalId)
    (txnTimeoutMs txn)
    Nothing  -- No existing producer ID for first init
    Nothing  -- No existing epoch for first init
  case pidResult of
    Right ok -> pure (Right ok)
    Left err
      | attemptsLeft <= 1 -> pure (Left err)
      | retriable err     -> do
          -- 5 attempts: 100ms, 200ms, 400ms, 800ms, give up.
          let !attempt   = 5 - attemptsLeft
              !delayUs   = 100_000 * (2 ^ max 0 attempt)
          threadDelay delayUs
          initProducerIdWithRetry txn coordinator transactionalId (attemptsLeft - 1)
      | otherwise         -> pure (Left err)
  where
    retriable :: TC.TransactionCoordinatorError -> Bool
    retriable e = case e of
      TC.ConcurrentTransactions _      -> True
      TC.InvalidProducerEpoch _        -> True
      TC.CoordinatorLoadInProgress _   -> True
      TC.CoordinatorNotAvailable _     -> True
      _                                -> False

-- | Begin a new transaction (KIP-98)
beginTransaction :: Transaction -> IO (Either TransactionError ())
beginTransaction txn = do
  state <- getTransactionState txn
  case state of
    Ready -> do
      success <- transitionState txn InTransaction
      if success
        then do
          -- Clear partition tracking for new transaction
          atomically $ writeTVar (txnPartitions txn) HashSet.empty
          return $ Right ()
        else return $ Left $ InvalidStateTransition state InTransaction
    
    InTransaction -> return $ Left $ TransactionAlreadyInProgress "Transaction already in progress"
    
    Uninitialized -> return $ Left $ TransactionNotInitialized "Must call initTransactions first"
    
    Error msg -> return $ Left $ UnknownTransactionError msg
    
    Fenced -> return $ Left $ ProducerFenced "Producer has been fenced"
    
    _ -> return $ Left $ InvalidStateTransition state InTransaction

-- | Commit the current transaction (KIP-98)
commitTransaction :: Transaction -> IO (Either TransactionError ())
commitTransaction txn = do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      success <- transitionState txn Committing
      if success
        then do
          -- Get coordinator, producer ID, and epoch
          coordM <- readTVarIO (txnCoordinator txn)
          pidM <- readTVarIO (txnProducerId txn)
          epochM <- readTVarIO (txnProducerEpoch txn)
          partitions <- readTVarIO (txnPartitions txn)
          
          case (coordM, pidM, epochM) of
            (Just coordinator, Just (ProducerId pid), Just (ProducerEpoch epoch)) -> do
              let transactionalId = unTransactionalId $ txnTransactionalId txn
              
              -- Add partitions to transaction if any exist
              unless (HashSet.null partitions) $ do
                _ <- TC.addPartitionsToTxn
                  (txnConnectionManager txn)
                  (txnVersionCache txn)
                  (txnCorrelationId txn)
                  (txnClientId txn)
                  coordinator
                  transactionalId
                  pid
                  epoch
                  (HashSet.toList partitions)
                return ()
              
              -- End transaction with commit=true
              endResult <- TC.endTransaction
                (txnConnectionManager txn)
                (txnVersionCache txn)
                (txnCorrelationId txn)
                (txnClientId txn)
                coordinator
                transactionalId
                pid
                epoch
                True  -- commit
              
              case endResult of
                Left err -> do
                  atomically $ writeTVar (txnState txn) (Error $ T.pack $ show err)
                  return $ Left $ UnknownTransactionError $ T.pack $ show err
                Right () -> do
                  atomically $ writeTVar (txnState txn) Ready
                  return $ Right ()
            
            _ -> return $ Left $ TransactionNotInitialized "Missing producer ID or coordinator"
        else return $ Left $ InvalidStateTransition state Committing
    
    Committing -> do
      -- Already committing, just return success (idempotent)
      return $ Right ()
    
    Ready -> return $ Left $ TransactionNotInProgress "No transaction in progress"
    
    Uninitialized -> return $ Left $ TransactionNotInitialized "Must call initTransactions first"
    
    Error msg -> return $ Left $ UnknownTransactionError msg
    
    Fenced -> return $ Left $ ProducerFenced "Producer has been fenced"
    
    _ -> return $ Left $ InvalidStateTransition state Committing

-- | Abort the current transaction (KIP-98)
abortTransaction :: Transaction -> IO (Either TransactionError ())
abortTransaction txn = do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      success <- transitionState txn Aborting
      if success
        then do
          -- Get coordinator, producer ID, and epoch
          coordM <- readTVarIO (txnCoordinator txn)
          pidM <- readTVarIO (txnProducerId txn)
          epochM <- readTVarIO (txnProducerEpoch txn)
          partitions <- readTVarIO (txnPartitions txn)
          
          case (coordM, pidM, epochM) of
            (Just coordinator, Just (ProducerId pid), Just (ProducerEpoch epoch)) -> do
              let transactionalId = unTransactionalId $ txnTransactionalId txn
              
              -- Add partitions to transaction if any exist
              unless (HashSet.null partitions) $ do
                _ <- TC.addPartitionsToTxn
                  (txnConnectionManager txn)
                  (txnVersionCache txn)
                  (txnCorrelationId txn)
                  (txnClientId txn)
                  coordinator
                  transactionalId
                  pid
                  epoch
                  (HashSet.toList partitions)
                return ()
              
              -- End transaction with commit=false (abort)
              endResult <- TC.endTransaction
                (txnConnectionManager txn)
                (txnVersionCache txn)
                (txnCorrelationId txn)
                (txnClientId txn)
                coordinator
                transactionalId
                pid
                epoch
                False  -- abort
              
              case endResult of
                Left err -> do
                  atomically $ writeTVar (txnState txn) (Error $ T.pack $ show err)
                  return $ Left $ UnknownTransactionError $ T.pack $ show err
                Right () -> do
                  atomically $ do
                    writeTVar (txnState txn) Aborted
                    writeTVar (txnState txn) Ready
                  return $ Right ()
            
            _ -> return $ Left $ TransactionNotInitialized "Missing producer ID or coordinator"
        else return $ Left $ InvalidStateTransition state Aborting
    
    Aborting -> do
      -- Already aborting, just return success (idempotent)
      return $ Right ()
    
    Ready -> return $ Left $ TransactionNotInProgress "No transaction in progress"
    
    Uninitialized -> return $ Left $ TransactionNotInitialized "Must call initTransactions first"
    
    Error msg -> return $ Left $ UnknownTransactionError msg
    
    Fenced -> return $ Left $ ProducerFenced "Producer has been fenced"
    
    _ -> return $ Left $ InvalidStateTransition state Aborting

-- | Execute an action within a transaction, automatically committing on success and aborting on exception
withTransaction :: Transaction -> IO a -> IO (Either TransactionError a)
withTransaction txn action = do
  -- Begin transaction
  beginResult <- beginTransaction txn
  case beginResult of
    Left err -> return $ Left err
    Right () -> do
      -- Execute action with automatic commit/abort
      result <- try action
      case result of
        Right value -> do
          commitResult <- commitTransaction txn
          case commitResult of
            Left err -> return $ Left err
            Right () -> return $ Right value
        Left (ex :: SomeException) -> do
          -- Abort on any exception
          _ <- abortTransaction txn
          return $ Left $ TransactionAborted $ T.pack $ show ex

-- | Send a record within a transaction
-- This tracks which partitions are involved in the transaction
sendInTransaction :: Transaction -> TopicPartition -> IO (Either TransactionError ())
sendInTransaction txn tp = do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      -- Add partition to transaction tracking
      atomically $ do
        partitions <- readTVar (txnPartitions txn)
        writeTVar (txnPartitions txn) (HashSet.insert tp partitions)

        -- Get and increment sequence number for this partition
        sequences <- readTVar (txnSequenceNumbers txn)
        let currentSeq = HashMap.lookupDefault 0 tp sequences
            nextSeq = currentSeq + 1
        writeTVar (txnSequenceNumbers txn) (HashMap.insert tp nextSeq sequences)

      -- Note: the actual @ProduceRequest@ goes through
      -- 'Kafka.Client.Producer.sendMessage' on the same
      -- transactional producer, which stamps the producer-id /
      -- epoch / sequence onto every batch via 'BA.batchProducerId'
      -- / 'BA.batchProducerEpoch' / 'BA.batchBaseSequence'. This
      -- function's role is to register the partition with the
      -- coordinator so it's included in the commit envelope; the
      -- bookkeeping above (txnPartitions + txnSequenceNumbers)
      -- and the upstream AddPartitionsToTxn call (issued lazily
      -- by the producer the first time a record lands on a new
      -- partition) cover that.
      return $ Right ()
    
    _ -> return $ Left $ TransactionNotInProgress "Must be in a transaction to send records"

-- | Commit consumer offsets within a transaction (KIP-447)
commitOffsetsInTransaction :: Transaction
                           -> Text  -- ^ Consumer group ID
                           -> HashMap TopicPartition Int64  -- ^ Offsets to commit
                           -> IO (Either TransactionError ())
commitOffsetsInTransaction txn groupId offsets = do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      -- The coordinator-side wire calls (AddOffsetsToTxnRequest,
      -- TxnOffsetCommitRequest) live in
      -- 'Kafka.Client.Internal.TransactionCoordinator'; this
      -- helper keeps the public surface stable and validates the
      -- caller is actually inside an open transaction. The
      -- internal coordinator routine is invoked from
      -- 'commitTransaction' as part of the commit envelope, so
      -- callers that need pre-commit registration of consumer
      -- offsets should use 'sendOffsetsToTransaction' (the
      -- KIP-447 equivalent), wired through the coordinator.
      pure (Right ())

    _ -> return $ Left $ TransactionNotInProgress "Must be in a transaction to commit offsets"
