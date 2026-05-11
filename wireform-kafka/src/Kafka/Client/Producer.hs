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
  , sendMessageDrop
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
  , producerTxnGate
    -- * Partitioning
  , Partitioner
  , defaultPartitioner
  , roundRobinPartitioner
  , hashPartitioner
  , stickyPartitioner
    -- * Configuration
  , defaultProducerConfig
  , DeliveryGuarantee(..)
    -- * Configuration validation (KIP-360)
  , validateProducerConfig
    -- * Cluster info (KIP-78)
  , producerClusterId
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
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
import Kafka.Client.ConfigValidation (ConfigError, renderConfigErrors)
import qualified Kafka.Client.ConfigValidation as CV
import qualified Kafka.Client.Consumer as KCC
import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Client.Internal.Murmur2 as Murmur2
import qualified Kafka.Client.Internal.ProducerSender as Sender
import qualified Kafka.Client.Internal.TransactionCoordinator as TC
import qualified Kafka.Client.Metadata as Meta
import qualified Kafka.Client.Transaction as Txn
import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.RecordBatch as RB
import qualified Kafka.Protocol.VersionNegotiation as VN
import qualified Kafka.Time as KafkaTime

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
  , producerInterceptor :: !(ProducerRecord -> IO ProducerRecord)
    -- ^ Pre-send interceptor. Mirrors
    --   @org.apache.kafka.clients.producer.ProducerInterceptor.onSend@.
    --   Applied to every record /before/ partition selection +
    --   batch accumulation. Defaults to 'pure', i.e. the record
    --   is forwarded unchanged. Exceptions in the interceptor
    --   are propagated to the caller of 'sendMessage' (the JVM
    --   client logs + skips, but a typed exception is more
    --   useful in Haskell).
  , producerOnAcknowledgement
      :: !(ProducerRecord -> Either String RecordMetadata -> IO ())
    -- ^ Per-record acknowledgement callback. Mirrors
    --   @ProducerInterceptor.onAcknowledgement@. Called from the
    --   sender thread when the broker ACK arrives (success path)
    --   or when the record's delivery times out / fails. Exceptions
    --   are caught and dropped so an interceptor bug can't take
    --   down the sender loop.
  , producerCompressionDictionary :: !(Maybe ByteString)
    -- ^ Optional zstd dictionary used by the configured
    --   'producerCompression' codec (currently only honoured for
    --   'Kafka.Compression.Zstd'). Mirrors librdkafka's
    --   @compression.dictionary@. Default 'Nothing' (no dict).
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
  , producerInterceptor                = pure
  , producerOnAcknowledgement          = \_ _ -> pure ()
  , producerCompressionDictionary      = Nothing
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
  , producerStickyPartitions :: !(IORef (HashMap.HashMap Text Int32))
    -- ^ Sticky partition state per topic (KIP-480). Consulted on
    --   every 'sendMessageAsync' / 'sendMessageDrop' call, so
    --   moving from 'StmMap' to 'IORef HashMap' (single
    --   'readIORef' per call instead of an STM transaction) is
    --   worth a measurable ~80-100 ns per record on the
    --   sender-side hot path.
  , producerRoundRobinCounters :: !(IORef (HashMap.HashMap Text Int32))
    -- ^ Round-robin counters per topic. Same hot-path
    --   conversion as 'producerStickyPartitions'.
  , producerPartitionCount :: !(IORef (HashMap.HashMap Text Int32))
    -- ^ Per-topic cached partition count. Populated lazily on
    --   first send to a topic from the metadata cache. Without
    --   this cache, every 'selectPartition' call paid an STM
    --   transaction against 'producerMetadata' just to read a
    --   value that almost never changes. The cache is a strict
    --   superset of the sticky-partition state; the sticky map
    --   only kicks in for the partitioner's choice.
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
-- | Pure config-validation rules (KIP-360). Each rule mirrors a
-- check the JVM client performs in
-- @org.apache.kafka.clients.producer.ProducerConfig@; we run them
-- before opening any socket so an obviously broken config (e.g.
-- @delivery.timeout.ms < request.timeout.ms@) fails fast with a
-- clear message instead of being deferred to the broker as an
-- opaque @POLICY_VIOLATION@ much later.
--
-- Returns the empty list when the config is acceptable.
validateProducerConfig :: ProducerConfig -> [ConfigError]
validateProducerConfig ProducerConfig{..} = concat
  [ CV.check (T.null producerClientId)
      "client.id" "must be non-empty"
  , CV.check (producerBatchSize < 0)
      "batch.size" "must be >= 0"
  , CV.check (producerLingerMs < 0)
      "linger.ms" "must be >= 0"
  , CV.check (producerMaxInFlight < 1)
      "max.in.flight.requests.per.connection" "must be >= 1"
  , CV.check (producerRetries < 0)
      "retries" "must be >= 0"
  , CV.check (producerRetryBackoffMs < 0)
      "retry.backoff.ms" "must be >= 0"
  , CV.check (producerRetryBackoffMaxMs < producerRetryBackoffMs)
      "retry.backoff.max.ms" "must be >= retry.backoff.ms"
  , CV.check (producerRetryBackoffMultiplier < 1.0)
      "retry.backoff.multiplier" "must be >= 1.0 (otherwise backoff shrinks)"
  , CV.check (producerRetryBackoffJitter < 0.0 || producerRetryBackoffJitter > 1.0)
      "retry.backoff.jitter" "must be in [0.0, 1.0]"
  , CV.check (producerRequestTimeoutMs <= 0)
      "request.timeout.ms" "must be > 0"
  , CV.check (producerDeliveryTimeoutMs < producerRequestTimeoutMs + producerLingerMs)
      "delivery.timeout.ms"
      "must be >= request.timeout.ms + linger.ms (KIP-91 invariant)"
  , CV.check (producerMaxRequestSize <= 0)
      "message.max.bytes" "must be > 0"
  , CV.check (producerQueueBufferingMaxMessages <= 0)
      "queue.buffering.max.messages" "must be > 0"
  , CV.check (producerQueueBufferingMaxKbytes <= 0)
      "queue.buffering.max.kbytes" "must be > 0"
  , CV.check (producerTransactionTimeoutMs <= 0)
      "transaction.timeout.ms" "must be > 0"
  , CV.check (producerStickyPartitioningLingerMs < 0)
      "sticky.partitioning.linger.ms" "must be >= 0"
  -- Idempotence / EOS coupling. KIP-679 hard-cap of in-flight=5
  -- for the idempotent producer to preserve sequence ordering.
  , CV.check (producerIdempotent && producerMaxInFlight > 5)
      "max.in.flight.requests.per.connection"
      "must be <= 5 when enable.idempotence=true (KIP-679)"
  , CV.check (producerIdempotent && producerDelivery == AtMostOnce)
      "acks"
      "must be all/-1 (ExactlyOnce/AtLeastOnce) when enable.idempotence=true"
  , CV.check (isJustNonEmpty producerTransactional && not producerIdempotent)
      "enable.idempotence"
      "transactional.id requires enable.idempotence=true (KIP-98)"
  , CV.check (isJustNonEmpty producerTransactional
              && producerDelivery /= ExactlyOnce)
      "acks"
      "transactional producers require acks=all (ExactlyOnce delivery guarantee)"
  ]
  where
    isJustNonEmpty (Just t) = not (T.null t)
    isJustNonEmpty Nothing  = False

createProducer
  :: [Text]          -- ^ Bootstrap broker addresses (host:port format)
  -> ProducerConfig  -- ^ Configuration
  -> IO (Either String Producer)
createProducer brokerAddrs config
  | configErrs <- validateProducerConfig config
  , not (null configErrs)
  = return $ Left $ renderConfigErrors configErrs
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
          -- Per-producer API version cache (shared with the
          -- Sender below). We negotiate against the bootstrap
          -- broker right after connect so the sender's first
          -- request finds a populated cache; brokers that
          -- don't speak ApiVersions silently leave the cache
          -- empty and the sender falls back to its compiled-in
          -- defaults.
          versionCache <- AV.createVersionCache
          handshakeCorrId <- newIORef 0
          let nextHandshakeCid = atomicModifyIORef' handshakeCorrId $ \cid -> (cid + 1, cid)
          _ <- VN.ensureVersionsNegotiated
                 conn firstBroker versionCache nextHandshakeCid

          fetchResult <- Meta.refreshMetadata conn metadataCache 0
          case fetchResult of
            Left err -> return $ Left $ "Failed to fetch initial metadata: " ++ err
            Right _ ->
              setupProducer config metadataCache connManager versionCache
  where
    setupProducer config metadataCache connManager versionCache = do
      
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
        versionCache
        (producerMaxInFlight config)
      
      -- Start sender thread
      senderThread <- Sender.startSenderThread senderState
      
      -- Initialize sticky partition and round-robin state (KIP-480)
      stickyPartitions <- newIORef HashMap.empty
      roundRobinCounters <- newIORef HashMap.empty
      partitionCounts <- newIORef HashMap.empty

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
        , producerPartitionCount = partitionCounts
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

-- | Poll the accumulator until it has no pending batches /and/
-- the sender thread is not currently mid-iteration on a drained
-- batch (i.e. 'senderBusy' is False), or the caller-supplied
-- deadline elapses. Returns @True@ if everything drained cleanly,
-- @False@ on timeout. The polling interval is 10ms which keeps
-- the close path responsive without burning CPU.
--
-- Checking the busy flag in addition to 'hasReadyBatches' is
-- required for correctness: 'BA.drainReadyBatches' removes
-- batches from the accumulator queue /before/ the sender has
-- received the broker's 'ProduceResponse' for them. A poller
-- that watched 'hasReadyBatches' alone would race the in-flight
-- window and a 'closeProducer' that fires immediately after
-- 'flushProducer' returned could lose every record on the wire.
waitForDrain :: Producer -> Int -> IO Bool
waitForDrain Producer{..} deadlineMs = do
  start <- nowMillis
  let loop = do
        pending  <- BA.hasReadyBatches producerAccumulator
        busy     <- readIORef (Sender.senderBusy producerSenderState)
        inFlight <- Sender.senderTotalInFlight producerSenderState
        now <- nowMillis
        let elapsed = now - start
        if not pending && not busy && inFlight == 0
          then pure True
          else if elapsed >= fromIntegral deadlineMs
            then pure False
            else do
              threadDelay (10 * 1000) -- 10ms
              loop
  loop
  where
    nowMillis :: IO Int64
    nowMillis = KafkaTime.currentTimeMillis

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
      busy     <- readIORef (Sender.senderBusy producerSenderState)
      inFlight <- Sender.senderTotalInFlight producerSenderState
      if not hasReady && not busy && inFlight == 0
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
  -- Mark all current batches as ready /without/ closing the
  -- accumulator: this is a drain-checkpoint and the producer
  -- must remain usable for subsequent sends.  (The pre-fix code
  -- called 'closeBatchAccumulator' here, which silently turned
  -- every later send into a "Failed to append record
  -- (accumulator closed or full)" — manifested as missing
  -- records in the head-to-head producer benchmark.)
  BA.flushPendingBatches producerAccumulator
  
  -- Wait for all batches to be sent
  let deliveryTimeout = producerDeliveryTimeoutMs producerConfig
      timeoutMicros = deliveryTimeout * 1000
      waitIncrement = 100000  -- 100ms
      maxWaits = max 1 (timeoutMicros `div` waitIncrement)
  
  -- Poll until all batches are sent or timeout. Both
  -- 'hasReadyBatches' (queued in the accumulator) and 'senderBusy'
  -- (drained by the sender but the broker reply hasn't landed)
  -- must be clear before we can claim a record is durable.
  result <- waitForAllBatches maxWaits

  return result
  where
    waitForAllBatches 0 = return $ Left "Flush timeout: not all records sent within delivery timeout"
    waitForAllBatches n = do
      hasReady <- BA.hasReadyBatches producerAccumulator
      busy     <- readIORef (Sender.senderBusy producerSenderState)
      inFlight <- Sender.senderTotalInFlight producerSenderState
      if not hasReady && not busy && inFlight == 0
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
bindTransaction Producer{..} txn = do
  -- 'senderTransactionalId' moved to IORef in Tier 3 of the
  -- STM-replacement work; the previous shape did the txn id
  -- mirror, the producerTransaction swap, and the
  -- registered-partitions reset under one STM transaction. Split
  -- into two: the producerTransaction TVar still composes with the
  -- registered-partitions StmMap reset (those remain STM), and
  -- the senderTransactionalId mirror is a separate IORef write.
  -- A producer that races bindTransaction against a sender's
  -- read of the txn id sees one of the two consistent states
  -- (old or new) — same as before.
  let txnIdText = Txn.unTransactionalId (Txn.txnTransactionalId txn)
  writeIORef (Sender.senderTransactionalId producerSenderState) (Just txnIdText)
  atomically $ do
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
  -- 1. Run the user-supplied interceptor first (KIP-388 / JVM
  --    ProducerInterceptor.onSend). This is allowed to rewrite
  --    the record (e.g. attach trace headers, drop a field, …).
  --    Errors propagate.
  let preInterceptRecord = ProducerRecord
        { recordTopic     = topic
        , recordKey       = key
        , recordValue     = value
        , recordHeaders   = []
        , recordPartition = Nothing
        , recordTimestamp = Nothing
        }
  iceptedRecord <- producerInterceptor producerConfig preInterceptRecord
  let icTopic = recordTopic   iceptedRecord
      icKey   = recordKey     iceptedRecord
      icValue = recordValue   iceptedRecord
      icHdrs  = recordHeaders iceptedRecord
  -- 2. Decide whether the producer is in a transactional /
  --    idempotent mode that requires stamping. We read the
  --    transaction state once up front; if the txn finishes
  --    between this read and the broker round-trip, the broker
  --    fences us — that's the intended JVM-client behaviour.
  preCheck <- producerPreSendCheck p icTopic icKey
  case preCheck of
    Left err -> do
      -- Surface the failure through the ack interceptor /before/
      -- returning to the caller, so observability tools see every
      -- send attempt regardless of where it failed.
      runAckInterceptor producerConfig iceptedRecord (Left err)
      return (Left err)
    Right (partition, stamp) -> do
      let record = RB.Record
            { RB.recordTimestampDelta = 0
            , RB.recordOffsetDelta = 0
            , RB.recordKey = icKey
            , RB.recordValue = icValue
            , RB.recordHeaders =
                map (\(k, v) -> RB.RecordHeader (TE.encodeUtf8 k) (Just v)) icHdrs
            }
          topicPartition = BA.TopicPartition icTopic partition

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
            Nothing -> do
              let err = "Delivery timeout exceeded ("
                          <> show (producerDeliveryTimeoutMs producerConfig)
                          <> "ms)"
              runAckInterceptor producerConfig iceptedRecord (Left err)
              return $ Left err
            Just r -> do
              runAckInterceptor producerConfig iceptedRecord r
              return r
        else do
          let err = "Failed to append record (accumulator closed or full)"
          runAckInterceptor producerConfig iceptedRecord (Left err)
          return (Left err)

-- | KIP-78: read the broker-supplied cluster id off this
-- producer's metadata cache. Returns 'Nothing' until the first
-- successful metadata refresh; afterwards reflects whatever the
-- broker set in its @MetadataResponse@.
producerClusterId :: Producer -> IO (Maybe Text)
producerClusterId Producer{..} =
  atomically (Meta.getClusterId producerMetadata)

-- | Best-effort dispatch of the ack interceptor. Wraps in 'try' so
-- a buggy interceptor can't take down the sender thread / caller.
runAckInterceptor
  :: ProducerConfig
  -> ProducerRecord
  -> Either String RecordMetadata
  -> IO ()
runAckInterceptor cfg rec_ outcome = do
  r <- try (producerOnAcknowledgement cfg rec_ outcome)
       :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left  _  -> pure ()

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
  -- Fast path for the common-case non-transactional /
  -- non-idempotent producer: skip the entire STM transaction
  -- below for sequence tracking and just call the partitioner +
  -- return the no-stamp sentinel. This is what 'sendMessageDrop'
  -- does inline and what 'sendMessageAsync' should do too —
  -- pre-fix the per-record STM commit was the dominant
  -- 'sendMessageAsync' cost (~150 ns / record on the bench).
  case mTxn of
    Nothing -> do
      pid   <- readTVarIO producerIdempotentId
      epoch <- readTVarIO producerIdempotentEpoch
      if pid == RB.noProducerId && epoch == RB.noProducerEpoch
        then do
          partition <- selectPartition p topic key
          pure (Right (partition, BA.noStamp))
        else fullPath mTxn
    _ -> fullPath mTxn
  where
    fullPath mTxn = fullPreSendCheck p topic key mTxn

fullPreSendCheck
  :: Producer
  -> Text
  -> Maybe ByteString
  -> Maybe Txn.Transaction
  -> IO (Either String (Int32, BA.BatchStamp))
fullPreSendCheck p@Producer{..} topic key mTxn = do
  -- 1. Transactional state guard.
  txnGate <- case mTxn of
    Nothing  -> return (Right Nothing)
    Just txn -> do
      st <- Txn.getTransactionState txn
      mPid   <- readIORef (Txn.txnProducerId    txn)
      mEpoch <- readIORef (Txn.txnProducerEpoch txn)
      case producerTxnGate st mPid mEpoch of
        Left err           -> return (Left err)
        Right (pid, epoch) -> return (Right (Just (txn, pid, epoch)))
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
              mCoord <- readIORef (Txn.txnCoordinator txn)
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

-- | Pure transactional state gate. Given the current
-- 'Txn.TransactionState' plus the (possibly populated) producer-id
-- / epoch from the bound 'Txn.Transaction', either:
--
--   * @Right (pid, epoch)@ — the producer is in 'Txn.InTransaction'
--     with both values populated, so the caller may proceed and
--     stamp the outgoing batch with @(pid, epoch)@; or
--   * @Left reason@ — surface a typed-string error matching what
--     'sendMessage' reports to the application.
--
-- Exposed so unit tests can drive every branch without spinning up
-- a real 'Producer' or transaction-coordinator round-trip.
producerTxnGate
  :: Txn.TransactionState
  -> Maybe Txn.ProducerId
  -> Maybe Txn.ProducerEpoch
  -> Either String (Int64, Int16)
producerTxnGate st mPid mEpoch = case st of
  Txn.InTransaction -> case (mPid, mEpoch) of
    (Just (Txn.ProducerId pid), Just (Txn.ProducerEpoch ep)) ->
      Right (pid, ep)
    _ -> Left
      "transactional producer: missing producer-id/epoch \
      \(call initTransactions before sending)"
  Txn.Uninitialized -> Left
    "transactional producer: must call initTransactions before sending"
  Txn.Ready -> Left
    "transactional producer: must call beginTransaction before sending"
  Txn.Fenced -> Left
    "transactional producer: producer fenced"
  Txn.Error msg -> Left $
    "transactional producer in error state: " <> T.unpack msg
  _ -> Left $
    "transactional producer: cannot send in state " <> show st

-- | Send a message asynchronously: enqueue into the batch
-- accumulator and return immediately, /without/ waiting for the
-- broker ack.  Mirrors librdkafka's @rd_kafka_produce@ +
-- 'Kafka.Producer.produceMessage' from @hw-kafka-client@: the
-- record sits in the in-memory queue and is flushed by the
-- background sender thread on the next batch round-trip.
--
-- Returns 'Left' only for /pre-enqueue/ failures (transactional
-- state checks, partition-selection errors, accumulator closed
-- or full).  Per-record broker errors surface through
-- 'producerOnAcknowledgement', not through this 'IO ()' result.
--
-- For at-least-once semantics, pair with 'flushProducer' before
-- 'closeProducer' so the queue drains.
sendMessageAsync
  :: Producer
  -> Text
  -> Maybe ByteString
  -> ByteString
  -> IO (Either String ())
sendMessageAsync p@Producer{..} topic key value = do
  let preInterceptRecord = ProducerRecord
        { recordTopic     = topic
        , recordKey       = key
        , recordValue     = value
        , recordHeaders   = []
        , recordPartition = Nothing
        , recordTimestamp = Nothing
        }
  iceptedRecord <- producerInterceptor producerConfig preInterceptRecord
  let icTopic = recordTopic   iceptedRecord
      icKey   = recordKey     iceptedRecord
      icValue = recordValue   iceptedRecord
      icHdrs  = recordHeaders iceptedRecord
  preCheck <- producerPreSendCheck p icTopic icKey
  case preCheck of
    Left err -> do
      runAckInterceptor producerConfig iceptedRecord (Left err)
      return (Left err)
    Right (partition, stamp) -> do
      let record = RB.Record
            { RB.recordTimestampDelta = 0
            , RB.recordOffsetDelta    = 0
            , RB.recordKey            = icKey
            , RB.recordValue          = icValue
            , RB.recordHeaders =
                map (\(k, v) -> RB.RecordHeader (TE.encodeUtf8 k) (Just v)) icHdrs
            }
          topicPartition = BA.TopicPartition icTopic partition
          -- The async path doesn't need a result-bearing TMVar:
          -- we just hand the broker outcome straight to the
          -- caller's ack interceptor.  This is the usual
          -- librdkafka shape — synchronous backpressure happens
          -- at 'flushProducer', not at the per-record send.
          callback result = do
            let outcome = case result of
                  Left err -> Left (T.unpack err)
                  Right (topic', part, offset, timestamp) ->
                    Right RecordMetadata
                      { metadataTopic     = topic'
                      , metadataPartition = part
                      , metadataOffset    = offset
                      , metadataTimestamp = timestamp
                      }
            runAckInterceptor producerConfig iceptedRecord outcome

      success <- BA.appendRecordStamped
                   producerAccumulator
                   topicPartition
                   record
                   callback
                   stamp

      if success
        then return (Right ())
        else do
          let err = "Failed to append record (accumulator closed or full)"
          runAckInterceptor producerConfig iceptedRecord (Left err)
          return (Left err)

-- | Bare-minimum-overhead async send: skips the user-installed
-- 'producerInterceptor' / 'producerOnAcknowledgement' hooks, the
-- transactional / idempotent stamping path
-- ('producerPreSendCheck'), and the per-record 'ProducerRecord'
-- struct allocation.
--
-- Suitable for high-throughput non-transactional producers that
-- don't need per-record ack callbacks (logs / telemetry / fire-
-- and-forget event streams).  Per-record CPU work is roughly:
--
--   * 'selectPartition' (one TVar read for sticky / no work for
--     hash partitioner);
--   * one 'RB.Record' allocation;
--   * one 'BA.appendRecordStamped' call (the STM hot path inside
--     'BatchAccumulator', ~250 ns).
--
-- Returns 'Left' only when the accumulator rejects the record
-- (closed / over-capacity); the caller is expected to back off
-- + retry.  Per-record broker errors are /silently dropped/ —
-- the user signed up for fire-and-forget by calling this.
--
-- For at-most-one-loss semantics, pair with 'flushProducer'
-- before 'closeProducer' so the in-memory queue drains.
--
-- This is the path the producer benchmark uses for the head-to-
-- head against librdkafka's @rd_kafka_produce@ + flush combo.
sendMessageDrop
  :: Producer
  -> Text             -- ^ Topic
  -> Maybe ByteString -- ^ Optional key (used by hash partitioner)
  -> ByteString       -- ^ Value
  -> IO (Either String ())
sendMessageDrop p@Producer{..} topic key value = do
  partition <- selectPartition p topic key
  let !record = RB.Record
        { RB.recordTimestampDelta = 0
        , RB.recordOffsetDelta    = 0
        , RB.recordKey            = key
        , RB.recordValue          = value
        , RB.recordHeaders        = []
        }
      !tp = BA.TopicPartition topic partition
  success <- BA.appendRecordStamped
               producerAccumulator
               tp
               record
               (\_ -> pure ())
               BA.noStamp
  if success
    then return (Right ())
    else return (Left "Failed to append record (accumulator closed or full)")

-- | Select partition for a message based on configured partitioner (KIP-480).
selectPartition
  :: Producer
  -> Text            -- ^ Topic
  -> Maybe ByteString  -- ^ Key (optional)
  -> IO Int32
selectPartition producer@Producer{..} topic keyM = do
  -- Fast path: per-topic partition count cached in
  -- 'producerPartitionCount'. The previous shape did
  -- @atomically $ Meta.getPartitionCount@ on /every/ record
  -- send, paying a full STM transaction just to read a value
  -- that effectively never changes. Cache miss falls through
  -- to the metadata cache + refresh path.
  cache <- readIORef producerPartitionCount
  partCount <- case HashMap.lookup topic cache of
    Just n  -> pure (Just n)
    Nothing -> do
      partCountM <- atomically $ Meta.getPartitionCount producerMetadata topic
      mn <- case partCountM of
        Just n  -> pure (Just n)
        Nothing -> do
          refreshTopicOnDemand producer topic
          atomically $ Meta.getPartitionCount producerMetadata topic
      case mn of
        Just n -> do
          atomicModifyIORef' producerPartitionCount $ \m ->
            (HashMap.insertWith (\_ old -> old) topic n m, ())
          pure (Just n)
        Nothing -> pure Nothing
  case partCount of
    Nothing -> return 0  -- Refresh didn't help; sender will error.
    Just partitionCount -> do
      let partitioner = producerPartitioner producerConfig
      partitioner producer topic keyM partitionCount

-- | Synchronous metadata refresh for a single topic. Picks any
-- broker the producer already knows about (from the
-- bootstrap-time refresh in 'createProducer') and issues a
-- targeted MetadataRequest. Best-effort: errors are swallowed
-- because the caller falls back to partition 0 + sender-side
-- retry on failure.
refreshTopicOnDemand :: Producer -> Text -> IO ()
refreshTopicOnDemand Producer{..} topic = do
  brokersM <- atomically (Meta.getAllBrokers producerMetadata)
  case brokersM of
    Just (b : _) -> do
      let addr = Meta.brokerMetaAddress b
      connRes <- Conn.getOrCreateConnection
                   producerConnManager addr Conn.defaultConnectionConfig
      case connRes of
        Right conn -> do
          -- Reuse the sender's correlation-id source so we
          -- don't tread on its in-flight numbering.
          cid <- atomicModifyIORef' (Sender.senderCorrelationId producerSenderState) $ \n -> (n + 1, n)
          _ <- Meta.refreshTopicMetadata conn producerMetadata
                 (Just [topic]) cid
          pure ()
        Left _ -> pure ()
    _ -> pure ()

-- | Hash-based partitioning using Kafka's murmur2 variant.
--
-- This is the SAME hash + selection rule the JVM client's
-- @DefaultPartitioner@ uses (via
-- @org.apache.kafka.common.utils.Utils.murmur2(byte[])@):
-- @(murmur2(key) & 0x7FFFFFFF) % numPartitions@. Every official
-- Kafka client (JVM, librdkafka, kafka-go, …) computes the same
-- result, so a record with @key=\"foo\"@ produced from any
-- language lands on the same partition.
hashPartition :: ByteString -> Int32 -> Int32
hashPartition = Murmur2.partitionForKey

-- | Sticky partitioning (KIP-480): stick to partition until batch is ready,
-- then switch to a different partition (improves batching)
getStickyPartition :: Producer -> Text -> Int32 -> IO Int32
getStickyPartition Producer{..} topic partCount = do
  -- Fast path: 'producerStickyPartitions' is consulted on every
  -- send (one-record-per-call interface), so the common case
  -- has to be a single 'readIORef' + 'HashMap.lookup' rather
  -- than an STM transaction.
  m <- readIORef producerStickyPartitions
  case HashMap.lookup topic m of
    Just partition -> pure partition
    Nothing -> do
      -- First record for this topic: allocate a partition by
      -- bumping the round-robin counter, then persist the
      -- sticky pick. Two 'atomicModifyIORef\'' calls — race
      -- against a concurrent first-record-for-this-topic call
      -- is harmless: both see the same partCount, both compute
      -- the same partition, the second 'atomicModifyIORef\''
      -- with 'insertWith (\_ old -> old)' preserves whichever
      -- caller landed first.
      partition <- atomicModifyIORef' producerRoundRobinCounters $ \rrm ->
        let !counter = HashMap.lookupDefault 0 topic rrm
            !p      = counter `mod` partCount
        in (HashMap.insert topic (counter + 1) rrm, p)
      atomicModifyIORef' producerStickyPartitions $ \sm ->
        case HashMap.lookup topic sm of
          Just existing -> (sm, existing)
          Nothing       -> (HashMap.insert topic partition sm, partition)

-- | Round-robin partitioning: cycle through partitions evenly.
-- Single 'atomicModifyIORef\'' bumps the per-topic counter and
-- returns the new partition; matches the pre-IORef STM
-- semantics.
getRoundRobinPartition :: Producer -> Text -> Int32 -> IO Int32
getRoundRobinPartition Producer{..} topic partCount =
  atomicModifyIORef' producerRoundRobinCounters $ \rrm ->
    let !counter = HashMap.lookupDefault 0 topic rrm
        !partition = counter `mod` partCount
    in (HashMap.insert topic (counter + 1) rrm, partition)

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

