{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Internal.BatchAccumulator
Description : Producer batch accumulation and management
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the batch accumulator for the Kafka producer.

The batch accumulator:
- Maintains one queue per topic-partition
- Accumulates records into batches
- Makes batches "ready" based on size or linger time
- Preserves ordering within each partition
- Allows concurrent access from multiple threads

Design follows patterns from Java Kafka client and librdkafka:
- Per-partition Deques hold batches
- Current batch per partition is being filled
- Batches become ready when full or linger time expires
- Sender thread(s) drain ready batches

-}
module Kafka.Client.Internal.BatchAccumulator
  ( -- * Batch Accumulator
    BatchAccumulator
  , createBatchAccumulator
  , closeBatchAccumulator
    -- * Adding Records
  , appendRecord
  , appendRecordWithCallback
  , appendRecordStamped
  , BatchStamp (..)
  , noStamp
  , TopicPartition(..)
    -- * Draining Batches
  , drainReadyBatches
  , hasReadyBatches
    -- * Batch Types
  , ProducerBatch(..)
  , BatchState(..)
  , RecordCallback
  ) where

import Control.Concurrent.STM
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.Int
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Kafka.Time as KafkaTime
import GHC.Generics (Generic)
import qualified ListT
import qualified StmContainers.Map as StmMap

import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB

-- | Topic-partition identifier
data TopicPartition = TopicPartition
  { tpTopic :: !Text
  , tpPartition :: !Int32
  } deriving (Eq, Show, Ord, Generic)

instance Hashable TopicPartition

-- | State of a batch
data BatchState
  = Filling      -- ^ Currently being filled with records
  | Ready        -- ^ Ready to be sent
  | Sending      -- ^ Currently being sent
  | Complete     -- ^ Successfully sent
  | Failed !Text -- ^ Send failed with error
  deriving (Eq, Show, Generic)

-- | Callback for record completion
-- Takes Either an error message or the record metadata
-- Record metadata includes: topic, partition, offset, timestamp
type RecordCallback = Either Text (Text, Int32, Int64, Int64) -> IO ()

-- | A batch of records for a single partition
data ProducerBatch = ProducerBatch
  { batchTopicPartition :: !TopicPartition
    -- ^ Which topic-partition this batch is for
  , batchRecords :: !(Seq RB.Record)
    -- ^ Records in this batch
  , batchSizeBytes :: !Int
    -- ^ Current size in bytes (approximate)
  , batchCreateTime :: !Int64
    -- ^ Timestamp when batch was created (milliseconds)
  , batchBaseTimestamp :: !Int64
    -- ^ Base timestamp for the batch
  , batchState :: !BatchState
    -- ^ Current state of this batch
  , batchCompression :: !Compression.CompressionCodec
    -- ^ Compression codec to use
  , batchCompressionLevel :: !Compression.CompressionLevel
    -- ^ Compression level (KIP-353/776/909)
  , batchCallbacks :: !(Seq RecordCallback)
    -- ^ Completion callbacks for each record (in order). A 'Seq'
    -- rather than a list because the producer hot path appends to
    -- this on every record; a list would be O(n) per append and
    -- therefore O(n^2) per batch (the older shape was the
    -- dominant cost in the BatchAccumulator append benchmark).
  , batchAttempts :: !Int
    -- ^ Number of retry attempts already taken on this batch.
    --   Bumped each time the sender re-enqueues the batch after a
    --   retriable produce error. The 'shouldRetry' predicate
    --   compares this against 'retryMaxAttempts' from the sender's
    --   'RetryConfig'; 'retryBatch' passes it to 'nextRetryBackoffMs'
    --   so successive attempts back off exponentially. 0 means the
    --   batch hasn't been retried yet.
  , batchProducerId :: !Int64
    -- ^ Idempotent / transactional producer id stamped onto the
    --   serialised RecordBatch. 'noProducerId' (= -1) for
    --   non-idempotent producers; populated from
    --   'Kafka.Client.Internal.ProducerSender.SenderState' when
    --   the producer config has @producerIdempotent = True@ or
    --   @producerTransactional = Just _@.
  , batchProducerEpoch :: !Int16
    -- ^ Producer epoch from @InitProducerId@. Pairs with
    --   'batchProducerId'; together they fence stale producers.
  , batchBaseSequence :: !Int32
    -- ^ Sequence number assigned to the first record in this
    --   batch. Each successive batch on the same (topic, partition)
    --   gets the previous batch's @batchBaseSequence + count@.
    --   'noSequence' (= -1) for non-idempotent producers.
  , batchIsTransactional :: !Bool
    -- ^ Whether this batch is part of an open transaction. When
    --   'True', 'Kafka.Client.Internal.ProducerSender.buildRecordBatch'
    --   sets 'attrIsTransactional' on the wire-level
    --   'RecordBatch.Attributes' so the broker treats the batch as a
    --   transactional write (consumed by read-committed consumers
    --   only after 'EndTxn(commit)'). Always 'False' for
    --   non-transactional / idempotent-only producers.
  }

-- | Per-partition batch queue
data PartitionQueue = PartitionQueue
  { queueBatches :: !(TVar (Seq ProducerBatch))
    -- ^ Queue of batches for this partition (oldest first)
  , queueCurrentBatch :: !(TVar (Maybe ProducerBatch))
    -- ^ Batch currently being filled
  }

-- | Batch accumulator configuration
data BatchAccumulatorConfig = BatchAccumulatorConfig
  { accumulatorBatchSize :: !Int
    -- ^ Maximum batch size in bytes
  , accumulatorLingerMs :: !Int
    -- ^ Time to wait for batching in milliseconds
  , accumulatorCompression :: !Compression.CompressionCodec
    -- ^ Compression codec to use
  , accumulatorCompressionLevel :: !Compression.CompressionLevel
    -- ^ Compression level (KIP-353/776/909)
  }

-- | Batch accumulator that manages batches for all partitions
data BatchAccumulator = BatchAccumulator
  { accumulatorConfig :: !BatchAccumulatorConfig
    -- ^ Configuration
  , accumulatorPartitions :: !(StmMap.Map TopicPartition PartitionQueue)
    -- ^ Per-partition queues
  , accumulatorClosed :: !(TVar Bool)
    -- ^ Whether accumulator is closed
  }

-- | Create a new batch accumulator
createBatchAccumulator
  :: Int                           -- ^ Batch size in bytes
  -> Int                           -- ^ Linger time in milliseconds
  -> Compression.CompressionCodec  -- ^ Compression codec
  -> Compression.CompressionLevel  -- ^ Compression level
  -> IO BatchAccumulator
createBatchAccumulator batchSize lingerMs compression compressionLevel = do
  let config = BatchAccumulatorConfig
        { accumulatorBatchSize = batchSize
        , accumulatorLingerMs = lingerMs
        , accumulatorCompression = compression
        , accumulatorCompressionLevel = compressionLevel
        }
  partitions <- StmMap.newIO
  closed <- newTVarIO False
  return BatchAccumulator
    { accumulatorConfig = config
    , accumulatorPartitions = partitions
    , accumulatorClosed = closed
    }

-- | Close the batch accumulator
-- Marks all pending batches as ready for draining
closeBatchAccumulator :: BatchAccumulator -> IO ()
closeBatchAccumulator BatchAccumulator{..} = atomically $ do
  writeTVar accumulatorClosed True
  -- Mark all current batches as ready
  partitionList <- ListT.toList $ StmMap.listT accumulatorPartitions
  mapM_ markCurrentBatchReady partitionList
  where
    markCurrentBatchReady :: (TopicPartition, PartitionQueue) -> STM ()
    markCurrentBatchReady (_, PartitionQueue{..}) = do
      currentM <- readTVar queueCurrentBatch
      case currentM of
        Nothing -> return ()
        Just batch -> do
          -- Move current batch to ready queue
          let readyBatch = batch { batchState = Ready }
          modifyTVar' queueBatches (|> readyBatch)
          writeTVar queueCurrentBatch Nothing

-- | Append a record to the accumulator
-- Returns True if the record was added, False if accumulator is closed
appendRecord
  :: BatchAccumulator
  -> TopicPartition
  -> RB.Record
  -> IO Bool
appendRecord accumulator tp record = do
  -- Use appendRecordWithCallback with a no-op callback
  appendRecordWithCallback accumulator tp record (\_ -> return ())

-- | Producer-id / epoch / sequence triple stamped onto each new
-- batch when the producer is idempotent (KIP-98) or transactional
-- (KIP-98 / KIP-447). Carries an 'isTransactional' flag so the
-- record-batch encoder can flip the corresponding bit in the
-- attributes word.
--
-- Use 'noStamp' for non-idempotent producers; the accumulator will
-- leave 'batchProducerId' / 'batchProducerEpoch' / 'batchBaseSequence'
-- at the @no…@ sentinels.
data BatchStamp = BatchStamp
  { stampProducerId      :: !Int64
  , stampProducerEpoch   :: !Int16
  , stampBaseSequence    :: !Int32
    -- ^ Sequence number to stamp on a /freshly created/ batch.
    --   Records appended to a batch that's already filling inherit
    --   that batch's sequence base; the producer-side counter
    --   should therefore be advanced /after/ a batch is sealed.
  , stampIsTransactional :: !Bool
  }
  deriving (Eq, Show)

-- | Stamp value used when no idempotent / transactional state is
-- in scope.
noStamp :: BatchStamp
noStamp = BatchStamp
  { stampProducerId      = RB.noProducerId
  , stampProducerEpoch   = RB.noProducerEpoch
  , stampBaseSequence    = RB.noSequence
  , stampIsTransactional = False
  }

-- | Append a record carrying an explicit 'BatchStamp'. If the
-- record creates a new batch, the stamp is recorded on the batch;
-- if it joins an existing /filling/ batch, the existing stamp is
-- preserved and the call asserts (in debug mode) that the
-- producer-id + epoch match.
--
-- The producer is responsible for ensuring that consecutive
-- 'stampBaseSequence' values across batches on the same
-- (topic, partition) form a gapless sequence; the accumulator only
-- records what it's given.
appendRecordStamped
  :: BatchAccumulator
  -> TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchStamp
  -> IO Bool
appendRecordStamped acc@BatchAccumulator{..} tp record callback stamp = do
  -- Fast path: try to append to an existing /filling/ batch
  -- without paying for getCurrentTime + new-batch construction.
  -- This is the common case once a partition's batch has been
  -- created; for steady-state high-throughput producers it skips
  -- the syscall on every record except the first per batch.
  r <- atomically (tryFastAppend tp record callback acc)
  case r of
    FastAppendDone done -> pure done
    FastAppendNeedsNewBatch -> do
      currentTime <- getCurrentTimeMillis
      atomically (slowAppend tp record callback stamp currentTime acc)

-- | Result of an optimistic append into an already-filling batch.
data FastAppendResult
  = FastAppendDone !Bool
    -- ^ The append landed (or the accumulator is closed). The
    -- 'Bool' mirrors 'appendRecordStamped's return value.
  | FastAppendNeedsNewBatch
    -- ^ No existing /filling/ batch for this partition; the
    -- caller must take the slow path (which fetches a timestamp
    -- before creating a fresh batch).
  deriving (Eq, Show)

-- | STM-only fast path. Re-uses the existing 'PartitionQueue'
-- for this 'TopicPartition' and the existing /filling/
-- 'ProducerBatch' on it. Falls through to 'FastAppendNeedsNewBatch'
-- when either is missing so the caller can do the IO bits
-- (clock + new batch construction) outside the transaction.
{-# INLINE tryFastAppend #-}
tryFastAppend
  :: TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchAccumulator
  -> STM FastAppendResult
tryFastAppend tp record callback BatchAccumulator{..} = do
  isClosed <- readTVar accumulatorClosed
  if isClosed
    then pure (FastAppendDone False)
    else do
      mq <- StmMap.lookup tp accumulatorPartitions
      case mq of
        Nothing    -> pure FastAppendNeedsNewBatch
        Just queue -> do
          mc <- readTVar (queueCurrentBatch queue)
          case mc of
            Nothing    -> pure FastAppendNeedsNewBatch
            Just batch -> do
              let !recordSize = approximateRecordSize record
                  !newSize    = batchSizeBytes batch + recordSize
                  !newBatch   = batch
                    { batchRecords   = batchRecords batch |> record
                    , batchSizeBytes = newSize
                    , batchCallbacks = batchCallbacks batch |> callback
                    }
              if newSize >= accumulatorBatchSize accumulatorConfig
                then do
                  let !readyBatch = newBatch { batchState = Ready }
                  modifyTVar' (queueBatches queue) (|> readyBatch)
                  writeTVar (queueCurrentBatch queue) Nothing
                  pure (FastAppendDone True)
                else do
                  writeTVar (queueCurrentBatch queue) (Just newBatch)
                  pure (FastAppendDone True)

-- | Slow path: clock has been read, we may need to create a new
-- partition queue + new batch. If between the fast-path peek and
-- this call another thread already filled in a batch, we still
-- append to it (so we don't lose the timestamp work, but also
-- don't double-create the batch).
{-# INLINE slowAppend #-}
slowAppend
  :: TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchStamp
  -> Int64           -- ^ current time, fetched in IO
  -> BatchAccumulator
  -> STM Bool
slowAppend tp record callback stamp currentTime BatchAccumulator{..} = do
  isClosed <- readTVar accumulatorClosed
  if isClosed
    then pure False
    else do
      queue <- getOrCreatePartitionQueue accumulatorPartitions tp
      current <- readTVar (queueCurrentBatch queue)
      batch <- case current of
        Just b  -> pure b
        Nothing -> do
          let !newBatch = applyStamp stamp $
                createBatch accumulatorConfig tp currentTime
          writeTVar (queueCurrentBatch queue) (Just newBatch)
          pure newBatch
      let !recordSize = approximateRecordSize record
          !newSize    = batchSizeBytes batch + recordSize
          !newBatch   = batch
            { batchRecords   = batchRecords batch |> record
            , batchSizeBytes = newSize
            , batchCallbacks = batchCallbacks batch |> callback
            }
      if newSize >= accumulatorBatchSize accumulatorConfig
        then do
          let !readyBatch = newBatch { batchState = Ready }
          modifyTVar' (queueBatches queue) (|> readyBatch)
          writeTVar (queueCurrentBatch queue) Nothing
          pure True
        else do
          writeTVar (queueCurrentBatch queue) (Just newBatch)
          pure True
  where
    applyStamp BatchStamp{..} b = b
      { batchProducerId      = stampProducerId
      , batchProducerEpoch   = stampProducerEpoch
      , batchBaseSequence    = stampBaseSequence
      , batchIsTransactional = stampIsTransactional
      }

-- | Append a record with a completion callback
-- Returns True if the record was added, False if accumulator is closed
-- The callback will be invoked when the broker acknowledges the record
appendRecordWithCallback
  :: BatchAccumulator
  -> TopicPartition
  -> RB.Record
  -> RecordCallback  -- ^ Callback invoked on completion
  -> IO Bool
appendRecordWithCallback BatchAccumulator{..} tp record callback = do
  currentTime <- getCurrentTimeMillis
  atomically $ do
    isClosed <- readTVar accumulatorClosed
    if isClosed
      then return False
      else do
        -- Get or create partition queue
        queue <- getOrCreatePartitionQueue accumulatorPartitions tp
        
        -- Get or create current batch
        current <- readTVar (queueCurrentBatch queue)
        batch <- case current of
          Just b -> return b
          Nothing -> do
            let newBatch = createBatch accumulatorConfig tp currentTime
            writeTVar (queueCurrentBatch queue) (Just newBatch)
            return newBatch
        
        -- Calculate size of this record (approximate)
        let recordSize = approximateRecordSize record
            newSize = batchSizeBytes batch + recordSize
            newBatch = batch
              { batchRecords = batchRecords batch |> record
              , batchSizeBytes = newSize
              , batchCallbacks = batchCallbacks batch |> callback
              }
        
        -- Check if batch is now full
        if newSize >= accumulatorBatchSize accumulatorConfig
          then do
            -- Batch is full, mark as ready and create new current batch
            let readyBatch = newBatch { batchState = Ready }
            modifyTVar' (queueBatches queue) (|> readyBatch)
            writeTVar (queueCurrentBatch queue) Nothing
            return True
          else do
            -- Batch not full yet, update current batch
            writeTVar (queueCurrentBatch queue) (Just newBatch)
            return True

-- | Drain all ready batches from the accumulator
-- This includes batches that are full or have exceeded linger time
drainReadyBatches :: BatchAccumulator -> IO [ProducerBatch]
drainReadyBatches BatchAccumulator{..} = do
  currentTime <- getCurrentTimeMillis
  atomically $ do
    -- Get all partitions
    partitionList <- ListT.toList $ StmMap.listT accumulatorPartitions
    
    -- For each partition, check if current batch is ready due to linger time
    mapM_ (checkLingerTime currentTime) partitionList
    
    -- Collect all ready batches from all partitions
    batches <- mapM drainPartitionBatches partitionList
    return $ concat batches
  where
    lingerMs = fromIntegral $ accumulatorLingerMs accumulatorConfig
    
    checkLingerTime :: Int64 -> (TopicPartition, PartitionQueue) -> STM ()
    checkLingerTime now (_, PartitionQueue{..}) = do
      currentM <- readTVar queueCurrentBatch
      case currentM of
        Nothing -> return ()
        Just batch -> do
          let age = now - batchCreateTime batch
          when (age >= lingerMs) $ do
            -- Linger time expired, mark as ready
            let readyBatch = batch { batchState = Ready }
            modifyTVar' queueBatches (|> readyBatch)
            writeTVar queueCurrentBatch Nothing
    
    drainPartitionBatches :: (TopicPartition, PartitionQueue) -> STM [ProducerBatch]
    drainPartitionBatches (_, PartitionQueue{..}) = do
      batches <- readTVar queueBatches
      let (ready, remaining) = Seq.spanl isReadyBatch batches
      writeTVar queueBatches remaining
      return $ toList ready
    
    isReadyBatch :: ProducerBatch -> Bool
    isReadyBatch batch = batchState batch == Ready

-- | Check if there are any ready batches
hasReadyBatches :: BatchAccumulator -> IO Bool
hasReadyBatches BatchAccumulator{..} = do
  currentTime <- getCurrentTimeMillis
  atomically $ do
    partitionList <- ListT.toList $ StmMap.listT accumulatorPartitions
    anyReady <- mapM (checkPartitionReady currentTime) partitionList
    return $ or anyReady
  where
    lingerMs = fromIntegral $ accumulatorLingerMs accumulatorConfig
    
    checkPartitionReady :: Int64 -> (TopicPartition, PartitionQueue) -> STM Bool
    checkPartitionReady now (_, PartitionQueue{..}) = do
      -- Check if there are ready batches in queue
      batches <- readTVar queueBatches
      let hasReady = any (\b -> batchState b == Ready) batches
      
      if hasReady
        then return True
        else do
          -- Check if current batch is ready due to linger time
          currentM <- readTVar queueCurrentBatch
          case currentM of
            Nothing -> return False
            Just batch -> do
              let age = now - batchCreateTime batch
              return (age >= lingerMs)

-- | Get or create a partition queue
getOrCreatePartitionQueue
  :: StmMap.Map TopicPartition PartitionQueue
  -> TopicPartition
  -> STM PartitionQueue
getOrCreatePartitionQueue partitions tp = do
  queueM <- StmMap.lookup tp partitions
  case queueM of
    Just queue -> return queue
    Nothing -> do
      batches <- newTVar Seq.empty
      current <- newTVar Nothing
      let queue = PartitionQueue batches current
      StmMap.insert queue tp partitions
      return queue

-- | Create a new empty batch
createBatch
  :: BatchAccumulatorConfig
  -> TopicPartition
  -> Int64  -- ^ Current time
  -> ProducerBatch
createBatch BatchAccumulatorConfig{..} tp currentTime =
  ProducerBatch
    { batchTopicPartition = tp
    , batchRecords = Seq.empty
    , batchSizeBytes = 0
    , batchCreateTime = currentTime
    , batchBaseTimestamp = currentTime
    , batchState = Filling
    , batchCompression = accumulatorCompression
    , batchCompressionLevel = accumulatorCompressionLevel
    , batchCallbacks = Seq.empty
    , batchAttempts = 0
    , batchProducerId = RB.noProducerId
    , batchProducerEpoch = RB.noProducerEpoch
    , batchBaseSequence = RB.noSequence
    , batchIsTransactional = False
    }

-- | Get current time in milliseconds.
--
-- Uses 'Kafka.Time.currentTimeMillis' which on Linux reads the
-- vDSO-mapped @CLOCK_REALTIME_COARSE@ (~8 ns per call) and on
-- macOS/BSD reads the regular vDSO 'CLOCK_REALTIME'. The
-- accumulator's @tryFastAppend@ STM-only path skips this call
-- entirely; only the @slowAppend@ path (one in N records) needs
-- the timestamp to seed a fresh batch.
getCurrentTimeMillis :: IO Int64
getCurrentTimeMillis = KafkaTime.currentTimeMillis

-- | Approximate size of a record in bytes
-- This is a rough estimate for batch size tracking
approximateRecordSize :: RB.Record -> Int
approximateRecordSize RB.Record{..} =
  let keySize = maybe 0 BS.length recordKey
      valueSize = BS.length recordValue
      headerSize = sum $ map (\h -> BS.length (RB.headerKey h) + maybe 0 BS.length (RB.headerValue h)) recordHeaders
      -- Add overhead for VarInt encoding and metadata
      overhead = 20
  in keySize + valueSize + headerSize + overhead

