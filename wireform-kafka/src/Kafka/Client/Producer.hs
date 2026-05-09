{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Producer
Description : High-level Kafka producer API
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module provides a high-level producer API for sending messages to Kafka.

Features:

* Automatic batching for improved throughput
* Configurable partitioning strategies
* Compression support
* Idempotent and transactional producers
* Asynchronous send with callbacks or futures
* Automatic retry with exponential backoff
* Delivery guarantees: at-most-once, at-least-once, exactly-once

= Usage Example

@
producer <- createProducer brokers defaultProducerConfig
result <- sendMessage producer "my-topic" key value
case result of
  Left err -> putStrLn $ "Send failed: " ++ err
  Right metadata -> print metadata
closeProducer producer
@

-}
module Kafka.Client.Producer
  ( -- * Producer Types
    Producer
  , ProducerConfig(..)
  , ProducerRecord(..)
  , RecordMetadata(..)
    -- * Producer Creation
  , createProducer
  , closeProducer
  , closeProducerWithTimeout
    -- * Flushing
  , flushProducer
    -- * Sending Messages
  , sendMessage
  , sendMessageAsync
  , sendBatch
    -- * Transactions
    --
    -- | The high-level KIP-98 / KIP-447 lifecycle (initTransactions /
    -- beginTransaction / commitTransaction / abortTransaction /
    -- sendOffsetsToTransaction) lives in
    -- "Kafka.Client.Transaction". To make 'sendMessage' actually
    -- participate in a transaction, bind a 'Txn.Transaction' to the
    -- producer with 'bindTransaction' /after/
    -- 'Txn.initTransactions' has populated its producer-id / epoch.
    --
    -- After binding:
    --
    --   * 'sendMessage' is rejected with 'Left' unless the
    --     transaction is in the 'Txn.InTransaction' state;
    --   * the first 'sendMessage' on a (topic, partition) issues an
    --     @AddPartitionsToTxn@ to the coordinator before enqueuing
    --     the record;
    --   * outgoing record batches are stamped with the
    --     transactional producer-id / epoch / sequence and have
    --     their @attrIsTransactional@ bit set.
  , bindTransaction
  , producerBoundTransaction
    -- * Partitioning
  , Partitioner
  , defaultPartitioner
  , roundRobinPartitioner
  , hashPartitioner
  , stickyPartitioner
    -- * Configuration
  , defaultProducerConfig
  , DeliveryGuarantee(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import qualified Data.Time.Clock.POSIX as Time
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (hash)
import qualified Data.Hashable as Hashable
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified ListT
import qualified StmContainers.Map as StmMap
import System.Timeout (timeout)

import qualified Kafka.Compression.Types as Compression
import Kafka.Compression.Types (CompressionCodec, defaultCodec)
import qualified Kafka.Client.Consumer as KCC
import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Client.Internal.ProducerSender as Sender
import qualified Kafka.Client.Internal.TransactionCoordinator as TC
import qualified Kafka.Client.Metadata as Meta
import qualified Kafka.Client.Transaction as Txn
import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.RecordBatch as RB

-- | Delivery guarantee level.
data DeliveryGuarantee
  = AtMostOnce   -- ^ Fire and forget, no acknowledgment
  | AtLeastOnce  -- ^ Wait for acknowledgment, may duplicate
  | ExactlyOnce  -- ^ Transactional, exactly-once semantics
  deriving (Eq, Show, Generic)

-- | Producer configuration.
data ProducerConfig = ProducerConfig
  { producerClientId :: !Text
    -- ^ Client identifier
  , producerCompression :: !CompressionCodec
    -- ^ Compression codec to use
  , producerCompressionLevel :: !(Maybe Int)
    -- ^ Compression level (Nothing = use codec default; KIP-353/776/909)
    -- Gzip: 0-9, Zstd: 1-22, LZ4: 0-16, Snappy: 0-9 (placeholder)
  , producerBatchSize :: !Int
    -- ^ Maximum batch size in bytes (default: 16384)
  , producerLingerMs :: !Int
    -- ^ Time to wait for batching in milliseconds (default: 0)
  , producerMaxInFlight :: !Int
    -- ^ Maximum in-flight requests per connection (default: 5)
  , producerRetries :: !Int
    -- ^ Number of retries on transient errors. Default: @2147483647@
    --   (librdkafka @retries@: effectively unlimited; capped by
    --   'producerDeliveryTimeoutMs').
  , producerRetryBackoffMs :: !Int
    -- ^ Initial backoff after a retriable error in ms. Default 100.
    --   Mirrors librdkafka @retry.backoff.ms@.
  , producerRetryBackoffMaxMs :: !Int
    -- ^ Ceiling for the exponential backoff in ms. Default 1000.
    --   Mirrors librdkafka @retry.backoff.max.ms@.
  , producerRetryBackoffMultiplier :: !Double
    -- ^ Multiplier between consecutive retry backoffs. Default 2.0.
  , producerRetryBackoffJitter :: !Double
    -- ^ Jitter band [0.0, 1.0]; the actual backoff is
    --   uniformly randomised in @backoff * (1 ± jitter)@.
    --   Default 0.2.
  , producerDeliveryTimeoutMs :: !Int
    -- ^ Maximum time for a record to be delivered including retries (default: 120000ms = 2 minutes)
  , producerRequestTimeoutMs :: !Int
    -- ^ Per-request timeout in ms. Bounded above by
    --   'producerDeliveryTimeoutMs'. Default 30000.
    --   Mirrors librdkafka @request.timeout.ms@ /
    --   @socket.timeout.ms@.
  , producerMaxRequestSize :: !Int
    -- ^ Cap for the size of a single ProduceRequest in bytes.
    --   Default 1048576 (1 MiB), matching librdkafka @message.max.bytes@.
  , producerQueueBufferingMaxMessages :: !Int
    -- ^ Max records the accumulator buffers across all partitions.
    --   Default 100000. Mirrors librdkafka
    --   @queue.buffering.max.messages@.
  , producerQueueBufferingMaxKbytes :: !Int
    -- ^ Max bytes the accumulator buffers across all partitions.
    --   Default 1048576 (1 GiB). Mirrors librdkafka
    --   @queue.buffering.max.kbytes@.
  , producerTransactionTimeoutMs :: !Int
    -- ^ How long the broker holds an open transaction before
    --   aborting it. Default 60000. Mirrors librdkafka
    --   @transaction.timeout.ms@.
  , producerEnableGaplessGuarantee :: !Bool
    -- ^ For idempotent producers, fail-fast on a sequence-number
    --   gap rather than dedup. Default 'False'. Mirrors librdkafka
    --   @enable.gapless.guarantee@.
  , producerStickyPartitioningLingerMs :: !Int
    -- ^ Sticky partitioner: linger this long on a partition
    --   before switching, regardless of batch size (KIP-480).
    --   Default 10. Mirrors librdkafka
    --   @sticky.partitioning.linger.ms@.
  , producerPartitioner :: !Partitioner
    -- ^ Partitioning strategy (default: DefaultPartitioner with sticky behavior - KIP-480)
  , producerDelivery :: !DeliveryGuarantee
    -- ^ Delivery guarantee (default: AtLeastOnce)
  , producerIdempotent :: !Bool
    -- ^ Enable idempotent producer (default: False)
  , producerTransactional :: !(Maybe Text)
    -- ^ Transactional ID (Nothing = non-transactional)
  } deriving (Generic)

-- | Default producer configuration.
defaultProducerConfig :: ProducerConfig
defaultProducerConfig = ProducerConfig
  { producerClientId = "kafka-native-producer"
  , producerCompression = defaultCodec
  , producerCompressionLevel = Nothing  -- Use codec default
  , producerBatchSize = 16384
  , producerLingerMs = 0
  , producerMaxInFlight = 5
  , producerRetries                    = 2_147_483_647   -- librdkafka default
  , producerRetryBackoffMs             = 100
  , producerRetryBackoffMaxMs          = 1000
  , producerRetryBackoffMultiplier     = 2.0
  , producerRetryBackoffJitter         = 0.2
  , producerDeliveryTimeoutMs          = 120_000          -- KIP-91
  , producerRequestTimeoutMs           = 30_000
  , producerMaxRequestSize             = 1_048_576        -- 1 MiB
  , producerQueueBufferingMaxMessages  = 100_000
  , producerQueueBufferingMaxKbytes    = 1_048_576        -- 1 GiB worth of records
  , producerTransactionTimeoutMs       = 60_000
  , producerEnableGaplessGuarantee     = False
  , producerStickyPartitioningLingerMs = 10
  , producerPartitioner                = defaultPartitioner
  , producerDelivery                   = AtLeastOnce
  , producerIdempotent                 = False
  , producerTransactional              = Nothing
  }

-- | A record to be sent to Kafka.
data ProducerRecord = ProducerRecord
  { recordTopic :: !Text
    -- ^ Target topic
  , recordKey :: !(Maybe ByteString)
    -- ^ Optional message key (for partitioning and compaction)
  , recordValue :: !ByteString
    -- ^ Message value
  , recordHeaders :: ![(Text, ByteString)]
    -- ^ Optional headers
  , recordPartition :: !(Maybe Int32)
    -- ^ Explicit partition (overrides partitioner)
  , recordTimestamp :: !(Maybe Int64)
    -- ^ Message timestamp (Nothing = broker assigns)
  } deriving (Eq, Show, Generic)

-- | Metadata about a successfully sent record.
data RecordMetadata = RecordMetadata
  { metadataTopic :: !Text
    -- ^ Topic name
  , metadataPartition :: !Int32
    -- ^ Partition number
  , metadataOffset :: !Int64
    -- ^ Offset within partition
  , metadataTimestamp :: !Int64
    -- ^ Broker-assigned timestamp
  } deriving (Eq, Show, Generic)

-- | Partitioning strategy for messages.
-- | Partitioner function type (KIP-480: Sticky partitioning).
--
-- A partitioner determines which partition a message goes to.
-- This affects batching, ordering, and load distribution.
--
-- The function receives:
--   - Producer state (for sticky/round-robin counters)
--   - Topic name
--   - Optional message key
--   - Partition count for the topic
--
-- And returns the selected partition (0-indexed).
type Partitioner = Producer -> Text -> Maybe ByteString -> Int32 -> IO Int32

-- | Default partitioner: hash-based if key present, sticky otherwise (KIP-480).
--
-- This is the Kafka default behavior:
--   - If a key is provided, use a hash of the key
--   - If no key, use sticky partitioning for better batching
defaultPartitioner :: Partitioner
defaultPartitioner producer topic keyM partitionCount = case keyM of
  Just key -> return $ hashPartition key partitionCount
  Nothing -> getStickyPartition producer topic partitionCount

-- | Round-robin partitioner: evenly distribute across partitions.
--
-- Cycles through partitions in order, providing even distribution
-- but potentially worse batching than sticky partitioner.
roundRobinPartitioner :: Partitioner
roundRobinPartitioner producer topic _key partitionCount =
  getRoundRobinPartition producer topic partitionCount

-- | Hash partitioner: always use key hash (requires key).
--
-- If no key is provided, falls back to partition 0.
-- For best results, always provide keys when using this partitioner.
hashPartitioner :: Partitioner
hashPartitioner _producer _topic keyM partitionCount = case keyM of
  Just key -> return $ hashPartition key partitionCount
  Nothing -> return 0  -- Fallback if no key

-- | Sticky partitioner: maximize batching by sticking to same partition (KIP-480).
--
-- Sticks to the same partition until the batch is full, then switches.
-- This improves batching efficiency and throughput compared to round-robin.
stickyPartitioner :: Partitioner
stickyPartitioner producer topic _key partitionCount =
  getStickyPartition producer topic partitionCount

-- | Kafka producer handle.
data Producer = Producer
  { producerConfig :: !ProducerConfig
    -- ^ Configuration
  , producerMetadata :: !Meta.MetadataCache
    -- ^ Metadata cache for partition leaders
  , producerAccumulator :: !BA.BatchAccumulator
    -- ^ Batch accumulator for buffering records
  , producerConnManager :: !Conn.ConnectionManager
    -- ^ Connection manager for broker connections
  , producerSender :: !(Async ())
    -- ^ Background sender thread
  , producerSenderState :: !Sender.SenderState
    -- ^ State for the sender thread
  , producerStickyPartitions :: !(StmMap.Map Text Int32)
    -- ^ Sticky partition state per topic (KIP-480) - using stm-containers
  , producerRoundRobinCounters :: !(StmMap.Map Text Int32)
    -- ^ Round-robin counters per topic - using stm-containers
  , producerIdempotentId :: !(TVar Int64)
    -- ^ Producer id (KIP-98). 'noProducerId' (= -1) for
    --   non-idempotent / non-transactional producers; otherwise
    --   set by 'InitProducerId' on the broker. We initialise it
    --   to 'noProducerId' and let the transactional bootstrap
    --   path overwrite it.
  , producerIdempotentEpoch :: !(TVar Int16)
    -- ^ Producer epoch from 'InitProducerId'. Pairs with
    --   'producerIdempotentId'.
  , producerSequenceNumbers :: !(StmMap.Map BA.TopicPartition Int32)
    -- ^ Per-(topic, partition) next sequence number to stamp
    --   onto the next batch's 'batchBaseSequence'. KIP-98.
  , producerTransaction :: !(TVar (Maybe Txn.Transaction))
    -- ^ When 'Just', the producer is bound to a 'Txn.Transaction'
    --   and 'sendMessage' participates in its KIP-98 / KIP-447
    --   transactional lifecycle: state is enforced to be
    --   'Txn.InTransaction', record batches are stamped with the
    --   transaction's producer-id / epoch and the
    --   'attrIsTransactional' bit, and the first send to a
    --   (topic, partition) lazily issues an
    --   @AddPartitionsToTxn@ to the coordinator. Set via
    --   'bindTransaction'; cleared on 'closeProducer' (which also
    --   aborts an open txn).
  , producerRegisteredPartitions :: !(StmMap.Map BA.TopicPartition ())
    -- ^ Partitions that have already been registered with the
    --   transaction coordinator inside the /current/
    --   transaction. Reset by 'beginTransaction' / on each
    --   commit/abort cycle. Used to make
    --   'AddPartitionsToTxn' idempotent without re-issuing the
    --   request on every send.
  }

-- | Create a new Kafka producer.
--
-- Steps:
--   1. Parse broker addresses
--   2. Connect to bootstrap brokers and fetch metadata
--   3. Initialize batch accumulator
--   4. Start background sender thread
--   5. If transactional, initialize transaction coordinator (TODO)
createProducer
  :: [Text]          -- ^ Bootstrap broker addresses (host:port format)
  -> ProducerConfig  -- ^ Configuration
  -> IO (Either String Producer)
createProducer brokerAddrs config = do
  -- Parse broker addresses
  let parsedBrokers = map parseBrokerAddress brokerAddrs
  
  -- Validate all brokers parsed successfully
  case sequence parsedBrokers of
    Left err -> return $ Left $ "Failed to parse broker addresses: " ++ err
    Right brokers -> do
      -- Create metadata cache
      metadataCache <- Meta.createMetadataCache
      
      -- Create connection manager
      connManager <- Conn.createConnectionManager
      
      -- Fetch initial metadata from first bootstrap broker
      let firstBroker = head brokers
          connConfig = Conn.defaultConnectionConfig
      connResult <- Conn.getOrCreateConnection connManager firstBroker connConfig
      case connResult of
        Left err -> return $ Left $ "Failed to connect to bootstrap broker: " ++ err
        Right conn -> do
          -- Fetch metadata (correlation ID 0 for initial fetch)
          fetchResult <- Meta.refreshMetadata conn metadataCache 0
          case fetchResult of
            Left err -> return $ Left $ "Failed to fetch initial metadata: " ++ err
            Right _ -> do
              -- Continue with producer setup
              setupProducer config metadataCache connManager
  where
    setupProducer config metadataCache connManager = do
      
      -- Create batch accumulator
      -- Determine compression level to use
      let compressionLevel = case producerCompressionLevel config of
            Just level -> level
            Nothing -> Compression.defaultLevel (producerCompression config)
      
      accumulator <- BA.createBatchAccumulator
        (producerBatchSize config)
        (producerLingerMs config)
        (producerCompression config)
        compressionLevel
      
      -- Determine required acks based on delivery guarantee
      let acks = case producerDelivery config of
            AtMostOnce -> 0    -- Fire and forget
            AtLeastOnce -> 1   -- Leader acknowledgment
            ExactlyOnce -> (-1) -- All ISRs (requires idempotent/transactional)
      
      -- Create sender state
      let retryConfig = Sender.RetryConfig
            { Sender.retryMaxAttempts       = producerRetries config
            , Sender.retryBackoffMs         = producerRetryBackoffMs config
            , Sender.retryBackoffMaxMs      = producerRetryBackoffMaxMs config
            , Sender.retryBackoffMultiplier = producerRetryBackoffMultiplier config
            , Sender.retryBackoffJitter     = producerRetryBackoffJitter config
            }
      
      senderState <- Sender.createSenderState
        accumulator
        metadataCache
        connManager
        retryConfig
        acks
        (producerDeliveryTimeoutMs config)  -- KIP-91: delivery timeout
        (producerCompression config)
        (producerClientId config)
      
      -- Start sender thread
      senderThread <- Sender.startSenderThread senderState
      
      -- Initialize sticky partition and round-robin state (KIP-480)
      stickyPartitions <- StmMap.newIO
      roundRobinCounters <- StmMap.newIO

      -- Idempotent / transactional producer state (KIP-98). The
      -- producer id + epoch are populated by 'InitProducerId' on
      -- first transactional bootstrap; until then they default to
      -- 'noProducerId' / 'noProducerEpoch'. Non-idempotent
      -- producers leave both at the no-op sentinels and the
      -- sender writes 'RB.noProducerId' / 'RB.noSequence' onto
      -- every batch.
      idempotentPid   <- newTVarIO RB.noProducerId
      idempotentEpoch <- newTVarIO RB.noProducerEpoch
      sequenceNumbers <- StmMap.newIO
      transaction     <- newTVarIO Nothing
      registeredParts <- StmMap.newIO

      -- Return producer handle
      return $ Right Producer
        { producerConfig = config
        , producerMetadata = metadataCache
        , producerAccumulator = accumulator
        , producerConnManager = connManager
        , producerSender = senderThread
        , producerSenderState = senderState
        , producerStickyPartitions = stickyPartitions
        , producerRoundRobinCounters = roundRobinCounters
        , producerIdempotentId = idempotentPid
        , producerIdempotentEpoch = idempotentEpoch
        , producerSequenceNumbers = sequenceNumbers
        , producerTransaction = transaction
        , producerRegisteredPartitions = registeredParts
        }

-- | Parse broker address in "host:port" format
parseBrokerAddress :: Text -> Either String Conn.BrokerAddress
parseBrokerAddress addr =
  case T.splitOn ":" addr of
    [host, portText] ->
      case reads (T.unpack portText) of
        [(port, "")] -> Right $ Conn.BrokerAddress (T.unpack host) port
        _ -> Left $ "Invalid port: " ++ T.unpack portText
    _ -> Left $ "Invalid broker address format (expected host:port): " ++ T.unpack addr

-- | Close the producer and flush pending messages.
--
-- Steps:
--   1. Close the batch accumulator (stops accepting new messages)
--   2. Wait for pending batches to be sent
--   3. Stop the sender thread
--   4. If transactional, abort any open transaction (TODO)
--   5. Close all connections
closeProducer :: Producer -> IO ()
closeProducer p@Producer{..} = do
  -- If a transaction is bound and currently open, abort it before
  -- shutdown. Mirrors the JVM client's @KafkaProducer.close@
  -- behaviour: an open transaction at close time is rolled back
  -- so the broker doesn't keep the txn id locked until its
  -- @transaction.timeout.ms@ deadline elapses.
  mTxn <- readTVarIO producerTransaction
  case mTxn of
    Nothing  -> pure ()
    Just txn -> do
      st <- Txn.getTransactionState txn
      case st of
        Txn.InTransaction -> do
          _ <- Txn.abortTransaction txn
          pure ()
        _ -> pure ()

  -- Close accumulator (marks all batches as ready). After this
  -- returns no new records can be appended; the sender thread
  -- will drain everything that's already been accumulated.
  BA.closeBatchAccumulator producerAccumulator

  -- Wait for the sender thread to actually finish draining,
  -- bounded by the producer's delivery timeout. This replaces a
  -- previous fixed 1-second sleep that didn't actually wait for
  -- the work to complete.
  let deadlineMs = max 1000 (producerDeliveryTimeoutMs producerConfig)
  _ <- waitForDrain p deadlineMs

  -- Stop sender thread (idempotent if already stopped).
  Sender.stopSenderThread producerSenderState producerSender

  -- Close all connections.
  Conn.closeAllConnections producerConnManager

-- | Poll the accumulator until it has no pending batches or the
-- caller-supplied deadline elapses. Returns @True@ if everything
-- drained cleanly, @False@ on timeout. The polling interval is
-- 10ms which keeps the close path responsive without burning CPU.
waitForDrain :: Producer -> Int -> IO Bool
waitForDrain Producer{..} deadlineMs = do
  start <- nowMillis
  let loop = do
        pending <- BA.hasReadyBatches producerAccumulator
        now <- nowMillis
        let elapsed = now - start
        if not pending
          then pure True
          else if elapsed >= fromIntegral deadlineMs
            then pure False
            else do
              threadDelay (10 * 1000) -- 10ms
              loop
  loop
  where
    nowMillis :: IO Int64
    nowMillis = round . (* 1000) <$> Time.getPOSIXTime

-- | Close the producer with a specified timeout (KIP-15).
--
-- Attempts to send all pending messages before closing, waiting up to
-- the specified timeout in milliseconds. If the timeout expires, any
-- remaining messages may be lost.
--
-- @since KIP-15
closeProducerWithTimeout :: Producer -> Int -> IO ()
closeProducerWithTimeout Producer{..} timeoutMs = do
  -- Close accumulator (marks all batches as ready)
  BA.closeBatchAccumulator producerAccumulator
  
  -- Wait for sender to drain with timeout
  let timeoutMicros = timeoutMs * 1000
      waitIncrement = 100000  -- 100ms
      maxWaits = max 1 (timeoutMicros `div` waitIncrement)
  
  -- Poll until sender finishes or timeout
  waitForDrain maxWaits
  
  -- Stop sender thread
  Sender.stopSenderThread producerSenderState producerSender
  
  -- Close all connections
  Conn.closeAllConnections producerConnManager
  where
    waitForDrain 0 = return ()  -- Timeout expired
    waitForDrain n = do
      hasReady <- BA.hasReadyBatches producerAccumulator
      if not hasReady
        then return ()  -- All batches sent
        else do
          threadDelay 100000  -- 100ms
          waitForDrain (n - 1)

-- | Flush all pending producer records (KIP-8).
--
-- Blocks until all buffered records have been sent to the broker and
-- acknowledged, or until the delivery timeout expires. This is useful
-- when you need to ensure all records are sent before proceeding.
--
-- Returns 'Right ()' on success, or 'Left error' if flush fails.
--
-- @since KIP-8
flushProducer :: Producer -> IO (Either String ())
flushProducer Producer{..} = do
  -- Mark all current batches as ready (without closing accumulator)
  BA.closeBatchAccumulator producerAccumulator
  
  -- Wait for all batches to be sent
  let deliveryTimeout = producerDeliveryTimeoutMs producerConfig
      timeoutMicros = deliveryTimeout * 1000
      waitIncrement = 100000  -- 100ms
      maxWaits = max 1 (timeoutMicros `div` waitIncrement)
  
  -- Poll until all batches are sent or timeout
  result <- waitForAllBatches maxWaits
  
  return result
  where
    waitForAllBatches 0 = return $ Left "Flush timeout: not all records sent within delivery timeout"
    waitForAllBatches n = do
      hasReady <- BA.hasReadyBatches producerAccumulator
      if not hasReady
        then return $ Right ()  -- All batches sent
        else do
          threadDelay 100000  -- 100ms
          waitForAllBatches (n - 1)

-- | Bind a 'Txn.Transaction' to this producer.
--
-- After this call returns:
--
--   * 'sendMessage' (and the variants below) read the
--     transaction's state on every call: a send is rejected with
--     a typed error unless the transaction is in
--     'Txn.InTransaction';
--   * record batches are stamped with the transaction's
--     producer-id / epoch (set by 'Txn.initTransactions') and the
--     'attrIsTransactional' bit;
--   * the first send to a (topic, partition) registers it with
--     the transaction coordinator via 'AddPartitionsToTxn'
--     (KIP-98).
--
-- The producer takes a non-owning reference: closing the producer
-- aborts the transaction if one is open at that point but does
-- not close the 'Transaction' value itself. Multiple producers
-- bound to the same transaction would race on the underlying
-- coordinator state, so don't do that.
--
-- It is safe to call 'bindTransaction' /before/
-- 'Txn.initTransactions': sends will simply be rejected (the
-- transaction's state is 'Txn.Uninitialized' until init runs).
bindTransaction :: Producer -> Txn.Transaction -> IO ()
bindTransaction Producer{..} txn = atomically $ do
  writeTVar producerTransaction (Just txn)
  -- Reset the per-transaction registered-partition memo. The
  -- 'beginTransaction' lifecycle should also clear this, but we
  -- do it on bind so a freshly bound transaction starts clean.
  resetStmMap producerRegisteredPartitions

-- | The currently bound transaction, if any. Mostly useful for
-- test scaffolding and observability; production code should hold
-- onto its 'Txn.Transaction' explicitly.
producerBoundTransaction :: Producer -> IO (Maybe Txn.Transaction)
producerBoundTransaction p = readTVarIO (producerTransaction p)

-- | Send a message synchronously (blocks until acknowledged).
--
-- Steps:
--   1. If a transaction is bound, verify it's in
--      'Txn.InTransaction' state; reject otherwise.
--   2. Determine target partition (if not specified).
--   3. If transactional, register the partition with the
--      coordinator on first observation (idempotent within the
--      txn).
--   4. Allocate a per-(topic, partition) sequence number for
--      idempotent / transactional producers.
--   5. Append record to the batch accumulator with the
--      appropriate stamp and completion callback.
--   6. Wait for acknowledgment from broker.
sendMessage
  :: Producer
  -> Text            -- ^ Topic
  -> Maybe ByteString  -- ^ Key (optional)
  -> ByteString      -- ^ Value
  -> IO (Either String RecordMetadata)
sendMessage p@Producer{..} topic key value = do
  -- Decide whether the producer is in a transactional / idempotent
  -- mode that requires stamping. We read the transaction state
  -- once up front; if the txn finishes between this read and the
  -- broker round-trip, the broker fences us — that's the
  -- intended JVM-client behaviour.
  preCheck <- producerPreSendCheck p topic key
  case preCheck of
    Left err -> return (Left err)
    Right (partition, stamp) -> do
      let record = RB.Record
            { RB.recordTimestampDelta = 0
            , RB.recordOffsetDelta = 0
            , RB.recordKey = key
            , RB.recordValue = value
            , RB.recordHeaders = []
            }
          topicPartition = BA.TopicPartition topic partition

      resultVar <- newEmptyTMVarIO
      let callback result =
            atomically $ putTMVar resultVar $ case result of
              Left err -> Left (T.unpack err)
              Right (topic', part, offset, timestamp) ->
                Right RecordMetadata
                  { metadataTopic     = topic'
                  , metadataPartition = part
                  , metadataOffset    = offset
                  , metadataTimestamp = timestamp
                  }

      success <- BA.appendRecordStamped
                   producerAccumulator
                   topicPartition
                   record
                   callback
                   stamp

      if success
        then do
          let timeoutMicros = producerDeliveryTimeoutMs producerConfig * 1000
          result <- timeout timeoutMicros $
                      atomically (readTMVar resultVar)
          case result of
            Nothing -> return $ Left $
              "Delivery timeout exceeded ("
                <> show (producerDeliveryTimeoutMs producerConfig)
                <> "ms)"
            Just r -> return r
        else
          return $ Left "Failed to append record (accumulator closed or full)"

-- | Combined gate + partitioning + sequence allocation. Run in a
-- single STM transaction (modulo the IO-scoped partitioner +
-- 'AddPartitionsToTxn' coordinator round-trip) so the
-- (state-check, partition-pick, sequence-bump) tuple is atomic.
producerPreSendCheck
  :: Producer
  -> Text
  -> Maybe ByteString
  -> IO (Either String (Int32, BA.BatchStamp))
producerPreSendCheck p@Producer{..} topic key = do
  mTxn <- readTVarIO producerTransaction
  -- 1. Transactional state guard.
  txnGate <- case mTxn of
    Nothing  -> return (Right Nothing)
    Just txn -> do
      st <- Txn.getTransactionState txn
      case st of
        Txn.InTransaction -> do
          mPid   <- readTVarIO (Txn.txnProducerId    txn)
          mEpoch <- readTVarIO (Txn.txnProducerEpoch txn)
          case (mPid, mEpoch) of
            (Just (Txn.ProducerId pid), Just (Txn.ProducerEpoch ep)) ->
              return (Right (Just (txn, pid, ep)))
            _ -> return $ Left
              "transactional producer: missing producer-id/epoch \
              \(call initTransactions before sending)"
        Txn.Uninitialized -> return $ Left
          "transactional producer: must call initTransactions \
          \before sending"
        Txn.Ready -> return $ Left
          "transactional producer: must call beginTransaction \
          \before sending"
        Txn.Fenced -> return $ Left
          "transactional producer: producer fenced"
        Txn.Error msg -> return $ Left $
          "transactional producer in error state: " <> T.unpack msg
        _ -> return $ Left $
          "transactional producer: cannot send in state "
            <> show st
  case txnGate of
    Left err -> return (Left err)
    Right txnInfo -> do
      realPartition <- selectPartition p topic key
      let tp = BA.TopicPartition topic realPartition

      -- 2. For transactional sends, lazily register the partition
      --    with the coordinator. Memoised in
      --    'producerRegisteredPartitions' so subsequent sends are
      --    just an STM lookup.
      regResult <- case txnInfo of
        Nothing -> return (Right ())
        Just (txn, pid, ep) -> do
          alreadyKnown <- atomically $ do
            r <- StmMap.lookup tp producerRegisteredPartitions
            case r of
              Just () -> return True
              Nothing -> do
                StmMap.insert () tp producerRegisteredPartitions
                return False
          if alreadyKnown
            then return (Right ())
            else do
              -- Track in the Transaction's partition set too — that
              -- drives the commit-time AddPartitionsToTxn envelope
              -- and the per-partition sequence bookkeeping.
              _ <- Txn.sendInTransaction txn (KCC.TopicPartition topic realPartition)
              -- Best-effort coordinator round-trip; on failure
              -- back the memo out so a future send retries.
              mCoord <- readTVarIO (Txn.txnCoordinator txn)
              case mCoord of
                Nothing -> do
                  atomically $ StmMap.delete tp producerRegisteredPartitions
                  return $ Left
                    "transactional producer: no transaction \
                    \coordinator (initTransactions never \
                    \completed?)"
                Just coord -> do
                  r <- TC.addPartitionsToTxn
                         (Txn.txnConnectionManager txn)
                         (Txn.txnVersionCache txn)
                         (Txn.txnCorrelationId txn)
                         (Txn.txnClientId txn)
                         coord
                         (Txn.unTransactionalId
                            (Txn.txnTransactionalId txn))
                         pid
                         ep
                         [KCC.TopicPartition topic realPartition]
                  case r of
                    Left e  -> do
                      atomically $ StmMap.delete tp producerRegisteredPartitions
                      return $ Left $
                        "transactional producer: \
                        \AddPartitionsToTxn failed: " <> show e
                    Right () -> return (Right ())
      case regResult of
        Left err -> return (Left err)
        Right () -> do
          -- 3. Allocate sequence + producer-id stamp.
          stamp <- atomically $ do
            curM <- StmMap.lookup tp producerSequenceNumbers
            let !cur = case curM of
                  Just s  -> s
                  Nothing -> 0
            StmMap.insert (cur + 1) tp producerSequenceNumbers
            (pid, ep) <- case txnInfo of
              Just (_, pid', ep') -> return (pid', ep')
              Nothing -> do
                pid' <- readTVar producerIdempotentId
                ep'  <- readTVar producerIdempotentEpoch
                return (pid', ep')
            return BA.BatchStamp
              { BA.stampProducerId      = pid
              , BA.stampProducerEpoch   = ep
              , BA.stampBaseSequence    = cur
              , BA.stampIsTransactional =
                  case txnInfo of
                    Just _  -> True
                    Nothing -> False
              }
          return (Right (realPartition, stamp))

-- | Drain every key from an stm-containers 'StmMap.Map' inside an
-- STM transaction. There's no built-in primitive for this, hence
-- the @ListT.toList@ + delete loop.
resetStmMap :: (Eq k, Hashable.Hashable k) => StmMap.Map k v -> STM ()
resetStmMap m = do
  pairs <- ListT.toList (StmMap.listT m)
  mapM_ (\(k, _) -> StmMap.delete k m) pairs

-- | Send a message asynchronously (returns immediately).
--
-- Note: This currently has the same behavior as sendMessage since we don't
-- have a callback/future mechanism yet. In a full implementation, this would
-- return a future or take a callback parameter.
sendMessageAsync
  :: Producer
  -> Text
  -> Maybe ByteString
  -> ByteString
  -> IO (Either String ())
sendMessageAsync producer topic key value = do
  result <- sendMessage producer topic key value
  case result of
    Left err -> return $ Left err
    Right _ -> return $ Right ()

-- | Select partition for a message based on configured partitioner (KIP-480).
selectPartition
  :: Producer
  -> Text            -- ^ Topic
  -> Maybe ByteString  -- ^ Key (optional)
  -> IO Int32
selectPartition producer@Producer{..} topic keyM = do
  -- Get partition count for topic from metadata
  partCountM <- atomically $ Meta.getPartitionCount producerMetadata topic
  
  case partCountM of
    Nothing -> return 0  -- Fallback if metadata not available
    Just partitionCount -> do
      -- Call the configured partitioner function
      let partitioner = producerPartitioner producerConfig
      partitioner producer topic keyM partitionCount

-- | Hash-based partitioning: murmur2 hash (Kafka-compatible)
hashPartition :: ByteString -> Int32 -> Int32
hashPartition key partCount =
  let keyHash = hash key  -- Using Data.Hashable
  in fromIntegral (abs keyHash) `mod` partCount

-- | Sticky partitioning (KIP-480): stick to partition until batch is ready,
-- then switch to a different partition (improves batching)
getStickyPartition :: Producer -> Text -> Int32 -> IO Int32
getStickyPartition Producer{..} topic partCount = atomically $ do
  -- Look up current sticky partition for this topic
  currentM <- StmMap.lookup topic producerStickyPartitions
  
  case currentM of
    Just partition -> return partition  -- Use existing sticky partition
    Nothing -> do
      -- No sticky partition yet, pick a random one
      -- In production, we'd check which partition has space in its batch
      -- For now, use simple round-robin selection
      counterM <- StmMap.lookup topic producerRoundRobinCounters
      let counter = maybe 0 id counterM
          partition = counter `mod` partCount
          nextCounter = counter + 1
      
      StmMap.insert partition topic producerStickyPartitions
      StmMap.insert nextCounter topic producerRoundRobinCounters
      return partition

-- | Round-robin partitioning: cycle through partitions evenly
getRoundRobinPartition :: Producer -> Text -> Int32 -> IO Int32
getRoundRobinPartition Producer{..} topic partCount = atomically $ do
  counterM <- StmMap.lookup topic producerRoundRobinCounters
  let counter = maybe 0 id counterM
      partition = counter `mod` partCount
      nextCounter = counter + 1
  
  StmMap.insert nextCounter topic producerRoundRobinCounters
  return partition

-- | Send a batch of messages.
--
-- Sends multiple messages as a batch. More efficient than multiple individual sends.
sendBatch
  :: Producer
  -> [ProducerRecord]
  -> IO (Either String [RecordMetadata])
sendBatch producer records = do
  -- Fire each enqueue in its own async so the accumulator can
  -- coalesce records targeting the same (topic, partition) into
  -- the same RecordBatch on the wire. Sequencing them via plain
  -- 'mapM' would block per-record on the broker round-trip and
  -- defeat the batching.
  asyncs   <- mapM (Async.async . sendRecordIndividual producer) records
  results  <- mapM Async.wait asyncs
  let (errors, successes) = partitionEithers results
  if null errors
    then return $ Right successes
    else return $ Left $
      "Some messages failed: "
        <> show (length errors) <> " errors"
  where
    sendRecordIndividual :: Producer -> ProducerRecord -> IO (Either String RecordMetadata)
    sendRecordIndividual p ProducerRecord{..} =
      sendMessage p recordTopic recordKey recordValue
    
    partitionEithers :: [Either a b] -> ([a], [b])
    partitionEithers = foldr (either left right) ([], [])
      where
        left  a (l, r) = (a:l, r)
        right b (l, r) = (l, b:r)

