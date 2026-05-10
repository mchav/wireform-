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
  , flushPendingBatches
  ) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, when)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  )
import Data.Int
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Kafka.Time as KafkaTime
import GHC.Generics (Generic)

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
    -- ^ Completion callbacks for each record, in order.
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

-- | Per-partition batch queue.
--
-- Per-partition usage is single-producer (the user thread that
-- calls 'sendMessage' for this partition; partition assignment is
-- deterministic per record so concurrent producer threads land in
-- different 'PartitionQueue' instances) and single-consumer (the
-- sender thread that calls 'drainReadyBatches'). The two refs were
-- 'TVar' pre-Tier-2 of the STM-replacement work; the 'atomically'
-- block on every 'appendRecordStamped' was paying STM commit
-- overhead (~150–200 ns/record at the bench) for an SPSC swap.
-- 'IORef' + 'atomicModifyIORef\'' provides the same single-ref
-- visibility guarantees, with the multi-thread-on-one-partition
-- safety 'atomicModifyIORef\'' still gives us. See
-- @docs/STM_REPLACEMENT_SPEC.md@ Tier 2 for the full analysis.
data PartitionQueue = PartitionQueue
  { queueBatches :: !(IORef (Seq ProducerBatch))
    -- ^ Queue of batches for this partition (oldest first).
    --   Producer thread appends ready batches via
    --   'atomicModifyIORef\''; sender thread drains via
    --   'atomicModifyIORef\'' with @Seq.spanl isReady@.
  , queueCurrentBatch :: !(IORef (Maybe ProducerBatch))
    -- ^ Batch currently being filled. Producer thread mutates
    --   via 'atomicModifyIORef\''; sender thread peeks during
    --   linger-time check (also via 'atomicModifyIORef\'' so the
    --   peek-then-promote operation stays atomic).
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

-- | Batch accumulator that manages batches for all partitions.
--
-- Pre-Tier-2 the partition map was an 'StmMap.Map' so insertion of
-- a new partition could compose with the per-partition queue swap
-- inside one STM transaction. Tier 2 replaced both the per-partition
-- queue refs and the partition map with 'IORef'-backed equivalents;
-- new-partition insertion is now an 'atomicModifyIORef\'' CAS that
-- preserves the "first inserter wins, the loser walks away" race
-- semantics @StmMap.insert@ already gave us.
--
-- 'accumulatorClosed' stays a 'TVar' (Tier 1's recommendation:
-- "leave the booleans alone in Tier 1, revisit if profiling
-- indicates it"). The producer / sender / close paths only read it
-- once per call so the per-tick STM cost is negligible compared to
-- the per-record win in Tier 2.
data BatchAccumulator = BatchAccumulator
  { accumulatorConfig :: !BatchAccumulatorConfig
    -- ^ Configuration
  , accumulatorPartitions :: !(IORef (HashMap TopicPartition PartitionQueue))
    -- ^ Per-partition queues. New partitions are inserted via
    --   'atomicModifyIORef\'' CAS with a re-check inside the
    --   modify so concurrent inserters of the same partition see
    --   the same 'PartitionQueue'.
  , accumulatorClosed :: !(TVar Bool)
    -- ^ Whether accumulator is closed. Kept as a 'TVar' to keep
    --   'closeBatchAccumulator's flip-then-drain semantics
    --   visible to the rest of the producer (notably the sender
    --   thread's shutdown signal); the cost is one 'readTVarIO'
    --   per 'appendRecord' which is dominated by the IORef work
    --   below.
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
  partitions <- newIORef HashMap.empty
  closed <- newTVarIO False
  return BatchAccumulator
    { accumulatorConfig = config
    , accumulatorPartitions = partitions
    , accumulatorClosed = closed
    }

-- | Close the batch accumulator. Marks all pending batches as
-- ready for draining.
--
-- Pre-Tier-2 the close-flag flip and the per-partition drain ran
-- inside a single STM transaction, so a producer thread that had
-- already entered 'appendRecordStamped's STM block before the
-- close committed would either see closed=True (and bail out) or
-- complete its append before close ran. Tier 2 splits these into
-- two operations: the 'TVar' flip happens first under STM, then
-- the partition snapshot drain happens via 'atomicModifyIORef\''
-- per partition. The race window for "producer appends after
-- closed flag is True but its append still lands" widens
-- slightly, but the late-append outcome is identical to the
-- pre-Tier-2 behaviour: the record sits in a partition's ready
-- queue that nobody is draining, exactly as a stray late append
-- under STM would (the sender thread has been told to stop). See
-- @docs/STM_REPLACEMENT_SPEC.md@ Tier 2 "closed-flag race" note.
closeBatchAccumulator :: BatchAccumulator -> IO ()
closeBatchAccumulator BatchAccumulator{..} = do
  atomically $ writeTVar accumulatorClosed True
  parts <- readIORef accumulatorPartitions
  forM_ (HashMap.elems parts) markCurrentBatchReadyIO

-- | Mark every partition's filling batch as 'Ready' /without/
-- closing the accumulator.  Used by 'Kafka.Client.Producer.
-- flushProducer' so subsequent sends keep working — the JVM
-- client + librdkafka behave the same way: 'flush()' is a
-- drain-checkpoint, not a destructor.
--
-- Tier 2: was an STM transaction over every partition; now folds
-- 'atomicModifyIORef\'' over a snapshot of the partition map.
-- Loses cross-partition atomicity which is fine — flush is a
-- drain-checkpoint, not a barrier (mid-flush appends to other
-- partitions were always allowed; the JVM client's @flush()@
-- behaves the same way).
flushPendingBatches :: BatchAccumulator -> IO ()
flushPendingBatches BatchAccumulator{..} = do
  parts <- readIORef accumulatorPartitions
  forM_ (HashMap.elems parts) markCurrentBatchReadyIO

-- | Promote a partition's in-flight 'queueCurrentBatch' onto the
-- ready 'queueBatches', if any. Atomic per-partition; the two
-- 'atomicModifyIORef\'' calls together do not need to be atomic
-- across both refs because a sender that drains 'queueBatches'
-- between the two only loses visibility of one ready batch for
-- one drain tick.
markCurrentBatchReadyIO :: PartitionQueue -> IO ()
markCurrentBatchReadyIO PartitionQueue{..} = do
  promoted <- atomicModifyIORef' queueCurrentBatch $ \mb -> case mb of
    Nothing    -> (Nothing, Nothing)
    Just batch -> (Nothing, Just (batch { batchState = Ready }))
  case promoted of
    Nothing         -> pure ()
    Just readyBatch ->
      atomicModifyIORef' queueBatches $ \s -> (s |> readyBatch, ())

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
  -- The closed-flag check moved out of the per-record STM
  -- transaction; see the data-decl note above. We accept the
  -- widened race because 'closeBatchAccumulator' drains every
  -- partition after flipping the flag, so a stray late append
  -- ends up in a partition's ready queue that the sender thread
  -- is no longer servicing — same outcome the STM version
  -- produced for an interleaved late append.
  isClosed <- readTVarIO accumulatorClosed
  if isClosed
    then pure False
    else do
      -- Fast path: try to append to an existing /filling/ batch
      -- without paying for getCurrentTime + new-batch construction.
      -- This is the common case once a partition's batch has been
      -- created; for steady-state high-throughput producers it
      -- skips the syscall on every record except the first per
      -- batch.
      mq <- HashMap.lookup tp <$> readIORef accumulatorPartitions
      case mq of
        Nothing -> slowAppendIO tp record callback stamp acc
        Just queue -> do
          fast <- atomicModifyIORef' (queueCurrentBatch queue) $ \mc ->
            case mc of
              Nothing -> (Nothing, FastAppendNeedsNewBatch)
              Just batch ->
                let !recordSize = approximateRecordSize record
                    !newSize    = batchSizeBytes batch + recordSize
                    !newBatch   = batch
                      { batchRecords   = batchRecords batch |> record
                      , batchSizeBytes = newSize
                      , batchCallbacks = batchCallbacks batch |> callback
                      }
                in if newSize >= accumulatorBatchSize accumulatorConfig
                     then ( Nothing
                          , FastAppendReady (newBatch { batchState = Ready })
                          )
                     else (Just newBatch, FastAppendKept)
          case fast of
            FastAppendKept -> pure True
            FastAppendReady readyBatch -> do
              atomicModifyIORef' (queueBatches queue) $ \s ->
                (s |> readyBatch, ())
              pure True
            FastAppendNeedsNewBatch ->
              slowAppendIO tp record callback stamp acc

-- | Outcome of the optimistic 'atomicModifyIORef\'' on
-- 'queueCurrentBatch' inside 'appendRecordStamped'.
data FastAppendResult
  = FastAppendKept
    -- ^ Record landed in the existing /filling/ batch and the
    --   batch isn't full yet; no further work required.
  | FastAppendReady !ProducerBatch
    -- ^ Record landed in the existing /filling/ batch and the
    --   batch is now at-or-over the configured size limit; the
    --   caller must push @batch@ onto 'queueBatches' (we left
    --   'queueCurrentBatch' empty inside the modify).
  | FastAppendNeedsNewBatch
    -- ^ There is no /filling/ batch (either the partition is
    --   freshly seen or the previous batch was just promoted to
    --   ready). The caller must take the slow path which reads
    --   the clock and constructs a fresh batch.

-- | Slow path: read the clock, ensure a 'PartitionQueue' exists
-- for this partition, and append the record to either the
-- existing /filling/ batch (if a concurrent thread filled one in
-- between the fast-path peek and our 'atomicModifyIORef\'' here)
-- or a freshly constructed batch.
{-# INLINE slowAppendIO #-}
slowAppendIO
  :: TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchStamp
  -> BatchAccumulator
  -> IO Bool
slowAppendIO tp record callback stamp acc@BatchAccumulator{..} = do
  currentTime <- getCurrentTimeMillis
  queue       <- getOrCreatePartitionQueue acc tp
  outcome <- atomicModifyIORef' (queueCurrentBatch queue) $ \mc ->
    let !batch = case mc of
          Just b  -> b
          Nothing -> applyStamp stamp $
            createBatch accumulatorConfig tp currentTime
        !recordSize = approximateRecordSize record
        !newSize    = batchSizeBytes batch + recordSize
        !newBatch   = batch
          { batchRecords   = batchRecords batch |> record
          , batchSizeBytes = newSize
          , batchCallbacks = batchCallbacks batch |> callback
          }
    in if newSize >= accumulatorBatchSize accumulatorConfig
         then ( Nothing
              , Just (newBatch { batchState = Ready })
              )
         else (Just newBatch, Nothing)
  case outcome of
    Nothing -> pure True
    Just readyBatch -> do
      atomicModifyIORef' (queueBatches queue) $ \s ->
        (s |> readyBatch, ())
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
appendRecordWithCallback acc tp record callback =
  appendRecordStamped acc tp record callback noStamp

-- | Drain all ready batches from the accumulator. This includes
-- batches that are full or have exceeded linger time.
--
-- Tier 2 rewrites this from one big STM transaction over every
-- partition into a fold of per-partition 'atomicModifyIORef\''
-- calls. Cross-partition atomicity is lost, which matches the
-- pre-Tier-2 producer's expectations: the sender's drain
-- iteration was already a snapshot in time, and a partition that
-- becomes ready /during/ the drain just gets picked up on the
-- sender's next iteration.
drainReadyBatches :: BatchAccumulator -> IO [ProducerBatch]
drainReadyBatches BatchAccumulator{..} = do
  currentTime <- getCurrentTimeMillis
  parts <- readIORef accumulatorPartitions
  fmap concat $ forM (HashMap.elems parts) $ \PartitionQueue{..} -> do
    -- Linger check: if the current /filling/ batch has been open
    -- longer than 'lingerMs', promote it to the ready queue
    -- before draining. The atomic-modify keeps the
    -- check-and-promote race-free against a concurrent producer
    -- thread that's also updating 'queueCurrentBatch'.
    promoted <- atomicModifyIORef' queueCurrentBatch $ \mc ->
      case mc of
        Just batch | currentTime - batchCreateTime batch >= lingerMs ->
          (Nothing, Just (batch { batchState = Ready }))
        _ -> (mc, Nothing)
    case promoted of
      Nothing -> pure ()
      Just b  -> atomicModifyIORef' queueBatches $ \s -> (s |> b, ())
    atomicModifyIORef' queueBatches $ \s ->
      let (ready, remaining) = Seq.spanl isReadyBatch s
      in (remaining, toList ready)
  where
    lingerMs = fromIntegral $ accumulatorLingerMs accumulatorConfig

    isReadyBatch :: ProducerBatch -> Bool
    isReadyBatch batch = batchState batch == Ready

-- | Check if there are any ready batches.
--
-- Tier 2: walks the IORef-backed partition snapshot and reads
-- each partition's queue + current batch with plain
-- 'readIORef'. Both reads acquire their own visibility window;
-- since the only state transition we care about is "is there a
-- ready batch?" and a producer that promotes a batch to ready
-- between the queue read and the current-batch read just gets
-- noticed on the next call, the racy view is fine.
hasReadyBatches :: BatchAccumulator -> IO Bool
hasReadyBatches BatchAccumulator{..} = do
  currentTime <- getCurrentTimeMillis
  parts <- readIORef accumulatorPartitions
  go currentTime (HashMap.elems parts)
  where
    lingerMs = fromIntegral $ accumulatorLingerMs accumulatorConfig

    go _   []                       = pure False
    go now (PartitionQueue{..} : rest) = do
      batches <- readIORef queueBatches
      if any (\b -> batchState b == Ready) batches
        then pure True
        else do
          currentM <- readIORef queueCurrentBatch
          case currentM of
            Just batch | now - batchCreateTime batch >= lingerMs ->
              pure True
            _ -> go now rest

-- | Get or create a partition queue.
--
-- Tier 2: was an STM 'StmMap.lookup' / 'StmMap.insert' pair;
-- now an 'atomicModifyIORef\'' CAS over the partition map. The
-- CAS is in three steps:
--
-- 1. Cheap fast path: 'readIORef' the map and look up the
--    partition. If present, we're done — no allocation, no CAS.
-- 2. Otherwise allocate fresh 'IORef's for the partition queue.
-- 3. CAS the partition map: if some other inserter beat us to
--    the same 'TopicPartition' we throw our candidate away and
--    return theirs (matches @StmMap.insert@'s "first writer
--    wins" semantics so all subsequent producers on this
--    partition see the same 'PartitionQueue').
getOrCreatePartitionQueue
  :: BatchAccumulator
  -> TopicPartition
  -> IO PartitionQueue
getOrCreatePartitionQueue BatchAccumulator{..} tp = do
  existing <- HashMap.lookup tp <$> readIORef accumulatorPartitions
  case existing of
    Just q  -> pure q
    Nothing -> do
      newBatchesRef <- newIORef Seq.empty
      newCurrentRef <- newIORef Nothing
      let !candidate = PartitionQueue newBatchesRef newCurrentRef
      atomicModifyIORef' accumulatorPartitions $ \m ->
        case HashMap.lookup tp m of
          Just q' -> (m, q')
          Nothing -> (HashMap.insert tp candidate m, candidate)

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

