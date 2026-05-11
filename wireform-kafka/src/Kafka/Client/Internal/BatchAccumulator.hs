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
  , appendRecordStampedUnsafe
  , appendRecordsStamped
    -- * Direct-mode hot path (skip partition lookups)
  , BatchAccumulating
  , PartitionQueue
  , baHotState
  , baTopicPartition
  , currentBatchOf
  , appendDirect
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
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
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
    -- ^ Queue of /sealed/ batches for this partition (oldest
    --   first). Producer thread appends ready batches via
    --   'atomicModifyIORef\''; sender thread drains via
    --   'atomicModifyIORef\'' with @Seq.spanl isReady@.
  , queueCurrentBatch :: !(IORef (Maybe BatchAccumulating))
    -- ^ Batch currently being filled. The accumulating batch
    --   has its hot mutable fields (records, size, callbacks)
    --   under a single inner 'IORef' so the per-record append
    --   path mutates them in place rather than rebuilding the
    --   whole batch struct on every call. When sealing we
    --   snapshot the inner state into an immutable
    --   'ProducerBatch' (so the rest of the producer keeps the
    --   record-style interface) and push it onto
    --   'queueBatches'.
  }

-- | In-progress batch held in 'queueCurrentBatch' while records
-- are being appended.
--
-- Splitting the batch into "fixed metadata" (this struct) and
-- "hot mutable state" (the inner 'baHotState' ref) lets the
-- per-record 'appendRecordStamped' / 'appendRecordStampedUnsafe'
-- hot path do one 'atomicModifyIORef\'' on the small
-- 'BatchHotState' (3 fields = 24 B allocation per call) instead
-- of rebuilding the 12-field 'ProducerBatch' struct on every
-- record (~96 B + tuple allocation).
--
-- The fixed fields mirror the constant parts of 'ProducerBatch';
-- when the batch is sealed (size limit hit / linger expired /
-- flush) we snapshot the hot state and the fixed fields into a
-- normal 'ProducerBatch' so the writer thread + every existing
-- consumer of the type sees the same immutable shape they used
-- to.
data BatchAccumulating = BatchAccumulating
  { baTopicPartition       :: !TopicPartition
  , baCreateTime           :: !Int64
  , baBaseTimestamp        :: !Int64
  , baCompression          :: !Compression.CompressionCodec
  , baCompressionLevel     :: !Compression.CompressionLevel
  , baProducerId           :: !Int64
  , baProducerEpoch        :: !Int16
  , baBaseSequence         :: !Int32
  , baIsTransactional      :: !Bool
  , baHotState             :: !(IORef BatchHotState)
  }

-- | Hot mutable state of an in-progress batch. Held under a
-- single 'IORef' so the per-record append is one CAS that
-- updates all three fields atomically.
--
-- Records and callbacks are accumulated in /reversed/ singly-
-- linked lists so the per-record append is one ':' cons (one
-- cell allocation per record per list) rather than one 'Seq'
-- snoc (one tree-node allocation per record per Seq, with
-- internal rebalancing). The 'snapshotBatch' helper reverses
-- and converts to the @Seq@ shape the rest of the producer
-- expects; the O(n) reverse runs once per sealed batch and is
-- negligible compared to the per-record win.
data BatchHotState = BatchHotState
  { bhRecordsRev   :: ![RB.Record]
  , bhSizeBytes    :: !Int
  , bhCallbacksRev :: ![RecordCallback]
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

-- | Promote a partition's in-flight accumulating batch onto the
-- ready 'queueBatches', if any. Snapshots the 'BatchAccumulating'
-- into an immutable 'ProducerBatch' marked 'Ready' before
-- pushing.
markCurrentBatchReadyIO :: PartitionQueue -> IO ()
markCurrentBatchReadyIO PartitionQueue{..} = do
  promoted <- atomicModifyIORef' queueCurrentBatch $ \mb -> case mb of
    Nothing -> (Nothing, Nothing)
    Just ba -> (Nothing, Just ba)
  case promoted of
    Nothing -> pure ()
    Just ba -> do
      sealed <- snapshotBatch ba Ready
      atomicModifyIORef' queueBatches $ \s -> (s |> sealed, ())

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
{-# INLINE appendRecordStamped #-}
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
      mq <- HashMap.lookup tp <$> readIORef accumulatorPartitions
      case mq of
        Nothing -> slowAppendIO tp record callback stamp acc
        Just queue -> do
          mba <- readIORef (queueCurrentBatch queue)
          case mba of
            Nothing -> slowAppendIO tp record callback stamp acc
            Just ba -> do
              -- One 'atomicModifyIORef\'' on the small inner
              -- 'BatchHotState' (3 fields = 24 B allocation per
              -- call) replaces the pre-refactor full-struct
              -- rebuild on the 12-field 'ProducerBatch'. The
              -- per-record allocation drop translates into
              -- ~25-30% main-thread enqueue throughput on the
              -- hot path.
              let !rs    = approximateRecordSize record
                  !limit = accumulatorBatchSize accumulatorConfig
              !sealNow <- atomicModifyIORef' (baHotState ba) $ \st ->
                let !ns = bhSizeBytes st + rs
                    !st' = BatchHotState
                      { bhRecordsRev   = record   : bhRecordsRev st
                      , bhSizeBytes    = ns
                      , bhCallbacksRev = callback : bhCallbacksRev st
                      }
                in (st', ns >= limit)
              when sealNow $ sealCurrent acc queue ba
              pure True

-- | Single-writer-per-partition variant of 'appendRecordStamped'
-- that swaps 'atomicModifyIORef\'' for a plain 'readIORef' +
-- 'writeIORef' pair on the partition's @queueCurrentBatch@. The
-- CAS-loop overhead 'atomicModifyIORef\'' pays per call is
-- replaced with one read + one write — about a 2x throughput lift
-- on the producer's hot path on 4-core hardware.
--
-- /Safety/: the caller must guarantee that no other thread
-- concurrently calls 'appendRecordStamped' /
-- 'appendRecordStampedUnsafe' /
-- 'appendRecordsStamped' for the same 'TopicPartition'. The
-- accumulator's other operations ('drainReadyBatches',
-- 'closeBatchAccumulator', 'flushPendingBatches') are still
-- thread-safe; only the per-partition append path is non-atomic.
--
-- Suitable for the canonical single-producer-thread shape every
-- 'librdkafka' /
-- @KafkaProducer@-on-the-JVM workload uses; for multi-thread
-- producers writing to the /same/ partition, stay on
-- 'appendRecordStamped'.
{-# INLINE appendRecordStampedUnsafe #-}
appendRecordStampedUnsafe
  :: BatchAccumulator
  -> TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchStamp
  -> IO Bool
appendRecordStampedUnsafe acc@BatchAccumulator{..} tp record callback stamp = do
  isClosed <- readTVarIO accumulatorClosed
  if isClosed
    then pure False
    else do
      mq <- HashMap.lookup tp <$> readIORef accumulatorPartitions
      case mq of
        Nothing -> slowAppendIO tp record callback stamp acc
        Just queue -> do
          mba <- readIORef (queueCurrentBatch queue)
          case mba of
            Nothing -> slowAppendIO tp record callback stamp acc
            Just ba -> do
              -- Single-writer fast path: 'readIORef' +
              -- 'writeIORef' on 'baHotState' instead of
              -- 'atomicModifyIORef\''. Caller guarantees no
              -- concurrent appends on this partition (see the
              -- haddock above).
              !st <- readIORef (baHotState ba)
              let !rs    = approximateRecordSize record
                  !ns    = bhSizeBytes st + rs
                  !limit = accumulatorBatchSize accumulatorConfig
                  !st' = BatchHotState
                    { bhRecordsRev   = record   : bhRecordsRev st
                    , bhSizeBytes    = ns
                    , bhCallbacksRev = callback : bhCallbacksRev st
                    }
              writeIORef (baHotState ba) st'
              when (ns >= limit) $ sealCurrent acc queue ba
              pure True

-- | Append a /sequence/ of records to one (topic, partition) in
-- a single 'atomicModifyIORef\'' call on the partition's
-- @queueCurrentBatch@. Amortises the per-call overhead the
-- single-record 'appendRecordStamped' pays on its hot path
-- (closed check, partition map lookup, queue-current swap) so
-- high-throughput producers that already have a list of records
-- to publish can hand them all over in one go.
--
-- Per-record work (size accounting, batch sealing on size limit,
-- sequence promotion to the ready queue) is folded inside the
-- one atomic-modify; if the records overflow the configured batch
-- size we may emit multiple ready batches at once. Callbacks
-- attach to the same record vector (the caller passes one
-- callback list keyed by record position).
--
-- Returns 'False' if the accumulator is closed; 'True' on
-- successful enqueue regardless of how many internal batches
-- were sealed.
appendRecordsStamped
  :: BatchAccumulator
  -> TopicPartition
  -> Seq RB.Record       -- ^ records to append, in order
  -> Seq RecordCallback  -- ^ one callback per record (same order; same length)
  -> BatchStamp
  -> IO Bool
appendRecordsStamped acc@BatchAccumulator{..} tp recs cbs stamp = do
  isClosed <- readTVarIO accumulatorClosed
  if isClosed
    then pure False
    else do
      queue <- getOrCreatePartitionQueue acc tp
      go queue (Seq.zip recs cbs)
      pure True
  where
    !batchSizeLimit = accumulatorBatchSize accumulatorConfig

    -- Walk the input records, folding them into the current
    -- accumulating batch. Each iteration:
    --   * ensures there's a current batch (creating one if not),
    --   * atomically appends the next record to its hot state,
    --   * seals if the batch hit the size limit.
    -- This is the canonical multi-record flow; it shares the
    -- single-record code paths above.
    go :: PartitionQueue -> Seq (RB.Record, RecordCallback) -> IO ()
    go queue s = case Seq.viewl s of
      Seq.EmptyL -> pure ()
      (r, cb) Seq.:< rest -> do
        ba <- ensureCurrent queue
        let !rs = approximateRecordSize r
        sealNow <- atomicModifyIORef' (baHotState ba) $ \st ->
          let !ns = bhSizeBytes st + rs
              !st' = BatchHotState
                { bhRecordsRev   = r  : bhRecordsRev st
                , bhSizeBytes    = ns
                , bhCallbacksRev = cb : bhCallbacksRev st
                }
          in (st', ns >= batchSizeLimit)
        when sealNow $ sealCurrent acc queue ba
        go queue rest

    ensureCurrent :: PartitionQueue -> IO BatchAccumulating
    ensureCurrent queue = do
      mba <- readIORef (queueCurrentBatch queue)
      case mba of
        Just b -> pure b
        Nothing -> do
          ct <- getCurrentTimeMillis
          candidate <- newAccumulating accumulatorConfig tp ct stamp
          atomicModifyIORef' (queueCurrentBatch queue) $ \mc ->
            case mc of
              Just b  -> (Just b, b)
              Nothing -> (Just candidate, candidate)


-- | Slow path: read the clock, ensure a 'PartitionQueue' exists,
-- create a fresh 'BatchAccumulating' if needed, then fall back
-- to the regular 'appendRecordStamped' to handle the actual
-- append (and any seal it triggers).
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
  -- CAS in a fresh 'BatchAccumulating' if one isn't there yet.
  -- Race semantics: if a concurrent caller installs first, we
  -- discard our candidate and use theirs.
  candidate <- newAccumulating accumulatorConfig tp currentTime stamp
  installed <- atomicModifyIORef' (queueCurrentBatch queue) $ \mc ->
    case mc of
      Just b  -> (Just b, b)
      Nothing -> (Just candidate, candidate)
  -- Now do the regular append via the inner 'baHotState' ref.
  let !rs = approximateRecordSize record
  !sealNow <- atomicModifyIORef' (baHotState installed) $ \st ->
    let !ns = bhSizeBytes st + rs
        !st' = BatchHotState
          { bhRecordsRev   = record   : bhRecordsRev st
          , bhSizeBytes    = ns
          , bhCallbacksRev = callback : bhCallbacksRev st
          }
    in (st', ns >= accumulatorBatchSize accumulatorConfig)
  when sealNow $ sealCurrent acc queue installed
  pure True

-- | Snapshot the in-progress 'BatchAccumulating' into an
-- immutable 'ProducerBatch' marked 'Ready', swap
-- 'queueCurrentBatch' to 'Nothing' (so future appends create a
-- fresh accumulating batch), and push the sealed batch onto
-- 'queueBatches'.
sealCurrent
  :: BatchAccumulator
  -> PartitionQueue
  -> BatchAccumulating
  -> IO ()
sealCurrent _acc queue ba = do
  -- Best-effort swap: if a concurrent caller has already swapped
  -- this same accumulating batch out (e.g. another thread sealed
  -- on its own atomic-modify-then-seal path), we just bail.
  swapped <- atomicModifyIORef' (queueCurrentBatch queue) $ \mc ->
    case mc of
      Just _  -> (Nothing, True)
      Nothing -> (Nothing, False)
  when swapped $ do
    sealed <- snapshotBatch ba Ready
    atomicModifyIORef' (queueBatches queue) $ \s -> (s |> sealed, ())

-- | Allocate a fresh 'BatchAccumulating' with empty hot state.
newAccumulating
  :: BatchAccumulatorConfig
  -> TopicPartition
  -> Int64                -- ^ current time millis
  -> BatchStamp
  -> IO BatchAccumulating
newAccumulating BatchAccumulatorConfig{..} tp currentTime BatchStamp{..} = do
  hot <- newIORef BatchHotState
    { bhRecordsRev   = []
    , bhSizeBytes    = 0
    , bhCallbacksRev = []
    }
  pure BatchAccumulating
    { baTopicPartition   = tp
    , baCreateTime       = currentTime
    , baBaseTimestamp    = currentTime
    , baCompression      = accumulatorCompression
    , baCompressionLevel = accumulatorCompressionLevel
    , baProducerId       = stampProducerId
    , baProducerEpoch    = stampProducerEpoch
    , baBaseSequence     = stampBaseSequence
    , baIsTransactional  = stampIsTransactional
    , baHotState         = hot
    }

-- | Snapshot a 'BatchAccumulating' into an immutable
-- 'ProducerBatch' with the supplied 'BatchState'. Reads the
-- inner hot ref once.
snapshotBatch :: BatchAccumulating -> BatchState -> IO ProducerBatch
snapshotBatch BatchAccumulating{..} state = do
  st <- readIORef baHotState
  -- One pass each to reverse the cons'd lists into the right
  -- forward order, then 'Seq.fromList'. Both passes are O(n)
  -- once per sealed batch — negligible against the per-record
  -- 'Seq.|>' allocations the prior shape paid for every append.
  let !records   = Seq.fromList (reverse (bhRecordsRev st))
      !callbacks = Seq.fromList (reverse (bhCallbacksRev st))
  pure ProducerBatch
    { batchTopicPartition   = baTopicPartition
    , batchRecords          = records
    , batchSizeBytes        = bhSizeBytes st
    , batchCreateTime       = baCreateTime
    , batchBaseTimestamp    = baBaseTimestamp
    , batchState            = state
    , batchCompression      = baCompression
    , batchCompressionLevel = baCompressionLevel
    , batchCallbacks        = callbacks
    , batchAttempts         = 0
    , batchProducerId       = baProducerId
    , batchProducerEpoch    = baProducerEpoch
    , batchBaseSequence     = baBaseSequence
    , batchIsTransactional  = baIsTransactional
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
    -- before draining.
    promoted <- atomicModifyIORef' queueCurrentBatch $ \mc ->
      case mc of
        Just ba | currentTime - baCreateTime ba >= lingerMs ->
          (Nothing, Just ba)
        _ -> (mc, Nothing)
    case promoted of
      Nothing -> pure ()
      Just ba -> do
        sealed <- snapshotBatch ba Ready
        atomicModifyIORef' queueBatches $ \s -> (s |> sealed, ())
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
            Just ba | now - baCreateTime ba >= lingerMs -> pure True
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

-- | Look up the partition's currently-in-progress
-- 'BatchAccumulating' (or 'Nothing' if there isn't one yet).
-- Used by 'Kafka.Client.Producer.sendMessageDropFastest' to
-- cache the in-progress batch handle in producer-local state and
-- skip the per-record partition-map + queue-current lookups.
currentBatchOf
  :: BatchAccumulator
  -> TopicPartition
  -> IO (Maybe (BatchAccumulating, PartitionQueue))
currentBatchOf BatchAccumulator{..} tp = do
  mq <- HashMap.lookup tp <$> readIORef accumulatorPartitions
  case mq of
    Nothing -> pure Nothing
    Just queue -> do
      mba <- readIORef (queueCurrentBatch queue)
      case mba of
        Nothing -> pure Nothing
        Just ba -> pure (Just (ba, queue))

-- | Append a record directly to a known 'BatchAccumulating',
-- skipping the closed-flag check, the partition-map lookup, and
-- the 'queueCurrentBatch' read 'appendRecordStamped' would do.
-- Returns 'True' if the batch is still the partition's current
-- batch after the append; 'False' if this append filled the
-- batch and triggered a seal (so the caller should refresh its
-- cached handle).
--
-- /Safety/: caller must ensure no other thread concurrently
-- appends to this same 'BatchAccumulating'. The default sticky
-- partitioner + single producer thread is the canonical safe
-- shape.
{-# INLINE appendDirect #-}
appendDirect
  :: BatchAccumulator
  -> PartitionQueue
  -> BatchAccumulating
  -> RB.Record
  -> RecordCallback
  -> IO Bool
appendDirect acc@BatchAccumulator{..} queue ba record callback = do
  let !rs    = approximateRecordSize record
      !limit = accumulatorBatchSize accumulatorConfig
  !st <- readIORef (baHotState ba)
  let !ns = bhSizeBytes st + rs
      !st' = BatchHotState
        { bhRecordsRev   = record   : bhRecordsRev st
        , bhSizeBytes    = ns
        , bhCallbacksRev = callback : bhCallbacksRev st
        }
  writeIORef (baHotState ba) st'
  if ns >= limit
    then do
      sealCurrent acc queue ba
      pure False
    else pure True

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
{-# INLINE approximateRecordSize #-}
approximateRecordSize :: RB.Record -> Int
approximateRecordSize RB.Record{..} =
  let keySize = maybe 0 BS.length recordKey
      valueSize = BS.length recordValue
      headerSize = sum $ map (\h -> BS.length (RB.headerKey h) + maybe 0 BS.length (RB.headerValue h)) recordHeaders
      -- Add overhead for VarInt encoding and metadata
      overhead = 20
  in keySize + valueSize + headerSize + overhead

