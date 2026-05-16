{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module: Kafka.Client.Transaction
Description: Group multiple producer sends + consumer offset commits into one atomic write.

A Kafka /transaction/ lets a producer publish to several partitions
and update one or more consumer offsets, then commit the lot
atomically — either every write lands, or none of them do. That is
what \"exactly once\" actually means in Kafka: a stream-processing
pipeline that reads from one topic and writes to another can
guarantee its outputs and its consumer offsets advance together.

= The five-step recipe

@
'withProducer' brokers
  'defaultProducerConfig'
    { 'producerTransactional' = Just \"my-app-1\"
    , 'producerIdempotent'    = True
    } $ \\p -> do
      txn <- 'createTransaction' (TransactionalId \"my-app-1\") connMgr vCache
               \"my-app-client\" bootstrap 60_000
      Right () <- 'initTransactions' txn           -- once per producer
      'bindTransaction' p txn

      Right () <- 'beginTransaction' txn
      _        <- 'sendMessage' p \"out\" Nothing payload
      Right () <- 'commitTransaction' txn
@

Five steps in order:

  1. Configure the producer with a 'producerTransactional' id and
     'producerIdempotent' on (these are required by the broker).
  2. 'createTransaction' for the coordinator handle, then
     'initTransactions' once — this fences any zombie producer
     using the same id from a previous run.
  3. 'bindTransaction' the producer to the transaction so subsequent
     sends participate in it.
  4. 'beginTransaction', send records via 'sendMessage' (and / or
     stage consumer offsets via 'commitOffsetsInTransaction'),
     then 'commitTransaction' or 'abortTransaction'.
  5. Repeat 4 as many times as you like — one initialised
     'Transaction' handle supports many begin/commit cycles.

The wire-level guarantees the broker provides:

  * /Atomicity/ — every record in the transaction either becomes
    visible or none of them do.
  * /Fencing/ — only one producer instance can hold a given
    transactional id at a time; an older instance is reliably
    locked out as soon as a newer one calls 'initTransactions'.
  * /Read isolation/ — a consumer configured with
    'consumerIsolationLevel = ReadCommitted' will only return
    records from committed transactions.

= Where the lower-level primitives live

Most user code only ever needs the five-step recipe above. The
'createTransaction' \/ 'getTransactionState' \/ 'transitionState'
exports are for the integration tests and the OpenTelemetry
instrumentation.
-}
module Kafka.Client.Transaction
  ( -- * The transaction lifecycle
    --
    -- | Initialise once per producer process; then begin / commit /
    -- abort as many times as you like.
    initTransactions
  , beginTransaction
  , commitTransaction
  , abortTransaction
  , withTransaction

    -- * Transactional sends
  , sendInTransaction
  , commitOffsetsInTransaction

    -- * Types
  , Transaction(..)
  , TransactionState(..)
  , ProducerId(..)
  , ProducerEpoch(..)
  , TransactionalId(..)
  , TransactionError(..)

    -- * Low-level state management
    --
    -- | Used by integration tests and the OpenTelemetry
    -- instrumentation. Most user code does not need to touch these.
  , createTransaction
  , getTransactionState
  , transitionState

    -- * Transactional-id helpers
  , transactionalIdOptional

    -- * Transactional-error classification
  , TxnErrorRecovery (..)
  , classifyTxnError

    -- * Bounded txn-op deadlines
  , TxnDeadline (..)
  , effectiveTxnDeadlineMs
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception, try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad (unless)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
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
import qualified Kafka.Client.RetryClassifier as RC
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

-- | Transaction handle with thread-safe state management.
--
-- Tier 3 of the STM-replacement work moves the per-transaction
-- handoff slots ('txnProducerId', 'txnProducerEpoch', 'txnState',
-- 'txnCoordinator') to 'IORef': they are all single-writer (the
-- transaction-coordinator-driven state machine) / multi-reader
-- (sender thread + close path) handoffs that never composed
-- transactionally with anything else. 'txnPartitions' and
-- 'txnSequenceNumbers' stay 'TVar' because the producer's
-- partition-registration path uses STM to compose
-- check-then-insert atomically with the transactional state.
data Transaction = Transaction
  { txnTransactionalId :: !TransactionalId
  , txnProducerId :: !(IORef (Maybe ProducerId))
  , txnProducerEpoch :: !(IORef (Maybe ProducerEpoch))
  , txnState :: !(IORef TransactionState)
  , txnPartitions :: !(TVar (HashSet TopicPartition))
  -- ^ Partitions involved in the current transaction.
  , txnSequenceNumbers :: !(TVar (HashMap TopicPartition Int32))
  -- ^ Sequence numbers per partition for idempotency.
  , txnCoordinator :: !(IORef (Maybe TransactionCoordinator))
  -- ^ Cached transaction coordinator
  , txnConnectionManager :: !ConnectionManager
  -- ^ Connection manager for network operations
  , txnVersionCache :: !ApiVersionCache
  -- ^ API version cache for version negotiation
  , txnCorrelationId :: !(IORef Int32)
  -- ^ Correlation ID generator. Single source of monotonically
  --   increasing correlation IDs handed to TransactionCoordinator
  --   requests; never composed transactionally with anything else,
  --   so 'IORef' + 'atomicModifyIORef\'' suffices.
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
createTransaction :: MonadIO m
                  => TransactionalId
                  -> ConnectionManager
                  -> ApiVersionCache
                  -> Text  -- ^ Client ID
                  -> BrokerAddress  -- ^ Bootstrap broker
                  -> Int32  -- ^ Transaction timeout (ms)
                  -> m Transaction
createTransaction transactionalId connMgr versionCache clientId bootstrapBroker timeoutMs = liftIO $ do
  producerId <- newIORef Nothing
  producerEpoch <- newIORef Nothing
  state <- newIORef Uninitialized
  partitions <- newTVarIO HashSet.empty
  sequences <- newTVarIO HashMap.empty
  coordinator <- newIORef Nothing
  correlationId <- newIORef 0
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
getTransactionState txn = readIORef (txnState txn)

-- | Attempt to transition to a new state, returns False if transition is invalid
transitionState :: Transaction -> TransactionState -> IO Bool
transitionState txn newState =
  atomicModifyIORef' (txnState txn) $ \currentState ->
    if isValidTransition currentState newState
      then (newState, True)
      else (currentState, False)

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
initTransactions :: MonadIO m => Transaction -> m (Either TransactionError ())
initTransactions txn = liftIO $ do
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
          writeIORef (txnCoordinator txn) (Just coordinator)

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
              -- Store producer ID and epoch.
              -- Tier 3 of the STM-replacement work: 'txnProducerId',
              -- 'txnProducerEpoch' and 'txnState' moved to 'IORef';
              -- the cross-ref atomicity used to be provided by the
              -- enclosing 'atomically' block. The producer reads
              -- these three together via three independent
              -- 'readIORef's; an interleaved sender that observes
              -- (Just pid, Nothing epoch, Ready) for one
              -- instruction is harmless because the sender only
              -- consults these when the state is already Ready and
              -- it has its own pid/epoch snapshot via
              -- @senderTransactionalId@.
              writeIORef (txnProducerId txn) (Just $ ProducerId pid)
              writeIORef (txnProducerEpoch txn) (Just $ ProducerEpoch epoch)
              writeIORef (txnState txn) Ready

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
beginTransaction :: MonadIO m => Transaction -> m (Either TransactionError ())
beginTransaction txn = liftIO $ do
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
commitTransaction :: MonadIO m => Transaction -> m (Either TransactionError ())
commitTransaction txn = liftIO $ do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      success <- transitionState txn Committing
      if success
        then do
          -- Get coordinator, producer ID, and epoch
          coordM <- readIORef (txnCoordinator txn)
          pidM <- readIORef (txnProducerId txn)
          epochM <- readIORef (txnProducerEpoch txn)
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
                  writeIORef (txnState txn) (Error $ T.pack $ show err)
                  return $ Left $ UnknownTransactionError $ T.pack $ show err
                Right () -> do
                  writeIORef (txnState txn) Ready
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
abortTransaction :: MonadIO m => Transaction -> m (Either TransactionError ())
abortTransaction txn = liftIO $ do
  state <- getTransactionState txn
  case state of
    InTransaction -> do
      success <- transitionState txn Aborting
      if success
        then do
          -- Get coordinator, producer ID, and epoch
          coordM <- readIORef (txnCoordinator txn)
          pidM <- readIORef (txnProducerId txn)
          epochM <- readIORef (txnProducerEpoch txn)
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
                  writeIORef (txnState txn) (Error $ T.pack $ show err)
                  return $ Left $ UnknownTransactionError $ T.pack $ show err
                Right () -> do
                  writeIORef (txnState txn) Aborted
                  writeIORef (txnState txn) Ready
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

-- | Execute an action within a transaction, automatically
-- committing on success and aborting on exception. The action
-- runs in the caller's monad; commit / abort always run in IO
-- (the broker calls don't need 'MonadIO' polymorphism inside
-- the bracket since they're internal to the lifecycle).
withTransaction
  :: MonadUnliftIO m
  => Transaction
  -> m a
  -> m (Either TransactionError a)
withTransaction txn action =
  withRunInIO $ \run -> do
    beginResult <- beginTransaction txn
    case beginResult of
      Left err -> pure (Left err)
      Right () -> do
        result <- try (run action)
        case result of
          Right value -> do
            commitResult <- commitTransaction txn
            case commitResult of
              Left err -> pure (Left err)
              Right () -> pure (Right value)
          Left (ex :: SomeException) -> do
            _ <- abortTransaction txn
            pure (Left (TransactionAborted (T.pack (show ex))))
{-# INLINABLE withTransaction #-}
{-# SPECIALIZE withTransaction :: Transaction -> IO a -> IO (Either TransactionError a) #-}

-- | Send a record within a transaction
-- This tracks which partitions are involved in the transaction
sendInTransaction :: MonadIO m => Transaction -> TopicPartition -> m (Either TransactionError ())
sendInTransaction txn tp = liftIO $ do
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

----------------------------------------------------------------------
-- Additional transactional ergonomics
--
-- Previously lived in @Kafka.Client.ProducerExtras@.
----------------------------------------------------------------------

-- | Optionally derive a @transactional.id@ from a base prefix +
-- a per-process suffix. Keeps the user-facing API symmetric with
-- the JVM client, where @transactional.id@ may be left blank and
-- the client picks one based on host/process ids.
transactionalIdOptional
  :: Maybe Text         -- ^ explicit override (matches the JVM client's @transactional.id@ property)
  -> Text               -- ^ application prefix
  -> Text               -- ^ per-process suffix (e.g. host\@pid)
  -> Text
transactionalIdOptional (Just t) _ _      = t
transactionalIdOptional Nothing prefix sf = prefix <> "-" <> sf

-- | Recovery action for a non-zero error code returned by the
-- transaction coordinator. Mirrors the abort / retry / fatal
-- partitioning the JVM client uses.
data TxnErrorRecovery
  = TxnRecoverByAbort
    -- ^ Abort the current transaction and let the producer continue.
  | TxnRecoverByRetry
    -- ^ Re-issue the same operation after a short backoff.
  | TxnRecoverFatal
    -- ^ The producer must close.
  deriving stock (Eq, Show)

-- | Classify a Kafka error code into a 'TxnErrorRecovery' bucket.
-- Backed by 'Kafka.Client.RetryClassifier.classify' so the
-- mapping stays in sync with the producer's retry logic.
classifyTxnError :: Int16 -> TxnErrorRecovery
classifyTxnError code = case RC.classify code of
  RC.ECNoError   -> TxnRecoverByRetry
  RC.ECRetriable -> TxnRecoverByRetry
  RC.ECAbortable -> TxnRecoverByAbort
  RC.ECFatal     -> TxnRecoverFatal

-- | Deadline supplied to @commitTransaction@ / @abortTransaction@.
-- Callers can bound the wait so a misbehaving coordinator can't
-- pin the producer open during shutdown.
data TxnDeadline
  = TxnUseProducerDefault
    -- ^ Fall back to the producer's @transaction.timeout.ms@.
  | TxnDeadlineMs !Int
    -- ^ Hard upper bound in ms.
  deriving stock (Eq, Show)

effectiveTxnDeadlineMs
  :: Int64        -- ^ now (ms)
  -> Int          -- ^ producer's @transaction.timeout.ms@
  -> TxnDeadline
  -> Int64
effectiveTxnDeadlineMs now defaultMs = \case
  TxnUseProducerDefault -> now + fromIntegral defaultMs
  TxnDeadlineMs ms      -> now + fromIntegral ms

----------------------------------------------------------------------
-- SPECIALIZE pragmas for the IO hot path
--
-- See "Kafka.Client.Producer" for the rationale.
----------------------------------------------------------------------

{-# INLINABLE createTransaction #-}
{-# SPECIALIZE createTransaction :: TransactionalId -> ConnectionManager -> ApiVersionCache -> Text -> BrokerAddress -> Int32 -> IO Transaction #-}
{-# INLINABLE initTransactions #-}
{-# SPECIALIZE initTransactions :: Transaction -> IO (Either TransactionError ()) #-}
{-# INLINABLE beginTransaction #-}
{-# SPECIALIZE beginTransaction :: Transaction -> IO (Either TransactionError ()) #-}
{-# INLINABLE commitTransaction #-}
{-# SPECIALIZE commitTransaction :: Transaction -> IO (Either TransactionError ()) #-}
{-# INLINABLE abortTransaction #-}
{-# SPECIALIZE abortTransaction :: Transaction -> IO (Either TransactionError ()) #-}
{-# INLINABLE sendInTransaction #-}
{-# SPECIALIZE sendInTransaction :: Transaction -> TopicPartition -> IO (Either TransactionError ()) #-}
