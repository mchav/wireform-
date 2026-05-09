{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Internal.ProducerSender
Description : Producer background sender thread with retry logic
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the background sender thread for the Kafka producer.

The sender thread:
- Continuously drains ready batches from the accumulator
- Groups batches by broker
- Sends ProduceRequests to each broker
- Handles retries with exponential backoff
- Tracks in-flight requests
- Updates batch states and notifies waiting threads

Design patterns:
- Dedicated sender thread per producer (similar to Java Kafka client)
- In-flight request tracking with correlation IDs
- Exponential backoff for retries
- Graceful shutdown support
-}
module Kafka.Client.Internal.ProducerSender
  ( -- * Sender Thread
    SenderState(..)
  , createSenderState
  , startSenderThread
  , stopSenderThread
    -- * Batch Sending
  , sendBatches
  , retryBatch
  , bumpBatchAttempts
  , batchBackoffMs
  , shouldRetry
    -- * Pure batch construction (exposed for testing)
  , buildRecordBatch
    -- * Timeout Checking (KIP-91)
  , isBatchTimedOut
    -- * Configuration
  , RetryConfig(..)
  , defaultRetryConfig
  , nextRetryBackoffMs
    -- * Structured logger
  , LogLevel(..)
  , Logger
  , defaultLogger
  , silentLogger
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.Async (Async, async, wait, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, try, bracket)
import Control.Monad (forever, when, unless, forM, forM_)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.Int
import qualified Data.Sequence as Seq
import Data.List (groupBy, sortBy, partition)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.Connection (Connection)
import System.IO (hPutStrLn, stderr)
import qualified Kafka.Time as KafkaTime

import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Client.Metadata as Meta
import Kafka.Compression.Types (CompressionCodec (NoCompression))
import qualified Kafka.Network.Connection as Conn
import Kafka.Network.Connection (BrokerAddress(..))
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.VersionNegotiation as VN
import qualified Kafka.Protocol.Encoding as E
import qualified Kafka.Protocol.Generated.ProduceRequest as PR
import qualified Kafka.Protocol.Generated.ProduceResponse as PResp
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.RecordBatch as RB
import qualified Kafka.Protocol.RecordBatchWire as RBW
import qualified Kafka.Protocol.Wire.Codec as WC

-- | Retry configuration for failed sends. Mirrors librdkafka's
-- @retries@ + @retry.backoff.ms@ + @retry.backoff.max.ms@ +
-- @retry.backoff.multiplier@ knobs. The producer threads a
-- 'RetryConfig' through 'sendMessage' / 'sendMessageAsync' so a
-- per-batch retry loop can compute its own next-backoff via
-- 'nextRetryBackoffMs'.
data RetryConfig = RetryConfig
  { retryMaxAttempts       :: !Int
    -- ^ Maximum number of retry attempts (default: 2147483647).
  , retryBackoffMs         :: !Int
    -- ^ Initial backoff in ms (default: 100).
  , retryBackoffMaxMs      :: !Int
    -- ^ Ceiling for the exponential progression in ms (default: 1000).
  , retryBackoffMultiplier :: !Double
    -- ^ Multiplier between consecutive backoffs (default: 2.0).
  , retryBackoffJitter     :: !Double
    -- ^ Jitter band in [0.0, 1.0]; the actual backoff is
    --   @backoff * (1 ± jitter)@ randomised uniformly. Default 0.2.
  } deriving (Eq, Show)

-- | Default retry configuration. Matches the producer-side
-- @defaultProducerConfig@.
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { retryMaxAttempts       = 2147483647
  , retryBackoffMs         = 100
  , retryBackoffMaxMs      = 1000
  , retryBackoffMultiplier = 2.0
  , retryBackoffJitter     = 0.2
  }

-- | Severity bucket for the structured 'Logger'.
data LogLevel
  = LogDebug
  | LogInfo
  | LogWarn
  | LogError
  deriving (Eq, Ord, Show)

-- | Structured logger callback. Sender threads call this on every
-- retriable produce error / batch send / timeout, so callers can
-- route the events to whatever observability stack they prefer.
-- 'defaultLogger' writes to stderr; 'silentLogger' is a no-op
-- suitable for tests.
type Logger = LogLevel -> Text -> IO ()

defaultLogger :: Logger
defaultLogger lvl msg =
  -- Use 'putStrLn' on stderr so library logs don't pollute stdout
  -- — the producer's record callbacks are the public success
  -- channel.
  hPutStrLn stderr (renderLogLevel lvl <> " " <> T.unpack msg)

renderLogLevel :: LogLevel -> String
renderLogLevel = \case
  LogDebug -> "[debug]"
  LogInfo  -> "[info]"
  LogWarn  -> "[warn]"
  LogError -> "[error]"

silentLogger :: Logger
silentLogger _ _ = pure ()

-- | Compute the next backoff for the given attempt number
-- (0-indexed: attempt 0 returns @retryBackoffMs@). The curve is
-- @backoff_n = min(retryBackoffMaxMs, retryBackoffMs * retryBackoffMultiplier^n)@
-- with deterministic jitter applied as a function of @n@ so two
-- runs of the same test produce identical numbers.
nextRetryBackoffMs :: RetryConfig -> Int -> Int
nextRetryBackoffMs RetryConfig{..} attempt =
  let !raw     = fromIntegral retryBackoffMs
                   * (retryBackoffMultiplier ^ max 0 attempt)
      !capped  = min (fromIntegral retryBackoffMaxMs) raw :: Double
      -- Sin-based jitter: deterministic per attempt, no PRNG, so
      -- tests can reproduce the curve exactly.
      !jit     = sin (fromIntegral attempt) * retryBackoffJitter
   in max 0 (round (capped * (1 + jit)))

-- | State for the sender thread
data SenderState = SenderState
  { senderAccumulator :: !BA.BatchAccumulator
    -- ^ Batch accumulator to drain from
  , senderMetadata :: !Meta.MetadataCache
    -- ^ Metadata cache for broker lookup
  , senderConnManager :: !Conn.ConnectionManager
    -- ^ Connection manager for getting broker connections
  , senderRetryConfig :: !RetryConfig
    -- ^ Retry configuration
  , senderRunning :: !(TVar Bool)
    -- ^ Whether the sender thread should keep running
  , senderAcks :: !Int16
    -- ^ Required acks: 0 = none, 1 = leader, -1 = all ISRs
  , senderTimeoutMs :: !Int32
    -- ^ Request timeout in milliseconds (used in ProduceRequest)
  , senderDeliveryTimeoutMs :: !Int32
    -- ^ Delivery timeout in milliseconds (KIP-91: total time including retries)
  , senderCompression :: !CompressionCodec
    -- ^ Compression codec to use
  , senderClientId :: !Text
    -- ^ Client ID for requests
  , senderCorrelationId :: !(TVar Int32)
    -- ^ Next correlation ID to use
  , senderLogger :: !Logger
    -- ^ Structured logger callback; invoked on retriable / fatal
    --   produce errors. Defaults to 'defaultLogger'.
  , senderVersionCache :: !AV.ApiVersionCache
    -- ^ Per-broker negotiated API version cache. Populated on
    --   first contact with each leader broker via
    --   'VN.ensureVersionsNegotiated' so the Produce-version
    --   selection ('VN.pickApiVersion') matches what the broker
    --   actually accepts (rather than the hardcoded v3 the
    --   sender used to emit unconditionally).
  }

-- | Create a new sender state
createSenderState
  :: BA.BatchAccumulator
  -> Meta.MetadataCache
  -> Conn.ConnectionManager
  -> RetryConfig
  -> Int16        -- ^ Required acks
  -> Int           -- ^ Delivery timeout (ms) - KIP-91
  -> CompressionCodec
  -> Text         -- ^ Client ID
  -> AV.ApiVersionCache
                 -- ^ Per-broker negotiated API-version cache (shared
                 --   with the producer's bootstrap handshake)
  -> IO SenderState
createSenderState accumulator metadata connManager retryConfig acks deliveryTimeoutMs compression clientId versionCache = do
  running <- newTVarIO True
  correlationId <- newTVarIO 0
  -- Use the smaller of delivery timeout and 30 seconds for individual request timeout
  -- The delivery timeout covers all retries, while request timeout is per-request
  let requestTimeoutMs = min 30000 (fromIntegral deliveryTimeoutMs)
  return SenderState
    { senderAccumulator = accumulator
    , senderMetadata = metadata
    , senderConnManager = connManager
    , senderRetryConfig = retryConfig
    , senderRunning = running
    , senderAcks = acks
    , senderTimeoutMs = requestTimeoutMs
    , senderDeliveryTimeoutMs = fromIntegral deliveryTimeoutMs
    , senderCompression = compression
    , senderClientId = clientId
    , senderCorrelationId = correlationId
    , senderLogger = defaultLogger
    , senderVersionCache = versionCache
    }

-- | Start the sender background thread
startSenderThread :: SenderState -> IO (Async ())
startSenderThread state = async $ senderLoop state

-- | Stop the sender thread gracefully
stopSenderThread :: SenderState -> Async () -> IO ()
stopSenderThread state thread = do
  atomically $ writeTVar (senderRunning state) False
  cancel thread

-- | Main sender loop
senderLoop :: SenderState -> IO ()
senderLoop state@SenderState{..} = do
  -- Check if we should continue running
  shouldRun <- atomically $ readTVar senderRunning
  
  if not shouldRun
    then return ()  -- Exit loop
    else do
      -- Check for ready batches
      hasReady <- BA.hasReadyBatches senderAccumulator
      
      if hasReady
        then do
          -- Drain ready batches
          batches <- BA.drainReadyBatches senderAccumulator
          
          -- Group batches by topic-partition, then by leader broker
          result <- try $ sendBatches state batches
          
          case result of
            Left (e :: SomeException) ->
              senderLogger
                LogError
                (T.pack ("Sender loop error: " <> show e))
            Right () -> return ()
          
          -- Continue immediately if there might be more batches
          senderLoop state
        else do
          -- No ready batches, sleep briefly
          threadDelay 10000  -- 10ms
          senderLoop state

-- | Send a collection of batches to their respective brokers
sendBatches :: SenderState -> [BA.ProducerBatch] -> IO ()
sendBatches state@SenderState{..} batches = do
  -- Group batches by broker (based on partition leader)
  batchesByBroker <- groupBatchesByBroker senderMetadata batches
  
  -- Send to each broker
  forM_ (Map.toList batchesByBroker) $ \(broker, brokerBatches) -> do
    result <- try $ sendToBroker state broker brokerBatches
    
    case result of
      Left (e :: SomeException) -> do
        -- Invoke error callbacks for all batches
        forM_ brokerBatches $ \batch -> do
          let callbacks = BA.batchCallbacks batch
              errorMsg = "Exception sending to broker: " <> T.pack (show e)
          forM_ callbacks $ \callback -> callback (Left errorMsg)
      Right () -> return ()

-- | Group batches by their target broker
groupBatchesByBroker
  :: Meta.MetadataCache
  -> [BA.ProducerBatch]
  -> IO (Map Meta.BrokerMetadata [BA.ProducerBatch])
groupBatchesByBroker metadata batches = do
  -- Look up leader for each batch's topic-partition
  batchesWithBroker <- forM batches $ \batch -> do
    let tp = BA.batchTopicPartition batch
        topic = BA.tpTopic tp
        partition = BA.tpPartition tp
    
    brokerM <- atomically $ Meta.getPartitionLeader metadata topic partition
    return (brokerM, batch)
  
  -- Handle batches with unknown leaders by invoking error callbacks
  let unknownLeaderBatches = [batch | (Nothing, batch) <- batchesWithBroker]
  forM_ unknownLeaderBatches $ \batch -> do
    let tp = BA.batchTopicPartition batch
        errorMsg = "Unknown leader for topic " <> BA.tpTopic tp <> 
                   " partition " <> T.pack (show (BA.tpPartition tp))
        callbacks = BA.batchCallbacks batch
    forM_ callbacks $ \callback -> callback (Left errorMsg)
  
  -- Group by broker, filtering out batches with unknown leaders
  let validBatches = [(broker, batch) | (Just broker, batch) <- batchesWithBroker]
      grouped = groupBy (\(b1, _) (b2, _) -> b1 == b2)
              $ sortBy (comparing fst) validBatches
  
  return $ Map.fromList $ map (\g -> (fst (head g), map snd g)) grouped

-- | Check if a batch has exceeded the delivery timeout (KIP-91)
isBatchTimedOut :: Int64 -> Int32 -> BA.ProducerBatch -> Bool
isBatchTimedOut currentTime deliveryTimeoutMs batch =
  let createTime = BA.batchCreateTime batch
      elapsed = currentTime - createTime
  in elapsed > fromIntegral deliveryTimeoutMs

-- | Send batches to a specific broker
sendToBroker :: SenderState -> Meta.BrokerMetadata -> [BA.ProducerBatch] -> IO ()
sendToBroker state@SenderState{..} broker batches = do
  -- Get current time to check delivery timeout (KIP-91).
  -- 'KafkaTime.currentTimeMillis' is the fast vDSO-coarse clock
  -- (~8 ns on Linux), called once per send-loop iteration.
  currentTime <- KafkaTime.currentTimeMillis
  
  -- Partition batches into those that have exceeded delivery timeout and those that haven't
  let (timedOutBatches, validBatches) = partition (isBatchTimedOut currentTime senderDeliveryTimeoutMs) batches
  
  -- Fail timed-out batches immediately
  forM_ timedOutBatches $ \batch -> do
    let callbacks = BA.batchCallbacks batch
        createTime = BA.batchCreateTime batch
        elapsed = currentTime - createTime
        errorMsg = "Delivery timeout exceeded: batch created " <> T.pack (show elapsed) <> 
                   "ms ago, timeout is " <> T.pack (show senderDeliveryTimeoutMs) <> "ms"
    forM_ callbacks $ \callback -> callback (Left errorMsg)
  
  -- Continue with valid batches only
  when (not $ null validBatches) $ do
    -- Get or create connection to the broker
    let brokerAddr = Meta.brokerMetaAddress broker
        nextCid = atomically $ do
          cid <- readTVar senderCorrelationId
          writeTVar senderCorrelationId (cid + 1)
          pure cid
    connResult <- Conn.getOrCreateConnection senderConnManager brokerAddr Conn.defaultConnectionConfig
    case connResult of
      Left err -> do
        senderLogger LogError
          (T.pack ("Failed to connect to broker: " <> err))
        forM_ validBatches $ \batch -> do
          let callbacks = BA.batchCallbacks batch
              errorMsg = "Connection failed: " <> T.pack err
          forM_ callbacks $ \callback -> callback (Left errorMsg)
    
      Right conn -> do
        -- Negotiate ApiVersions for this broker if we haven't yet;
        -- idempotent, single round-trip per (sender, broker) pair.
        _ <- VN.ensureVersionsNegotiated conn brokerAddr senderVersionCache nextCid
        corrId <- nextCid

        -- Build and encode the request (compression is IO).
        -- Threads the metadata cache through so v13+ requests
        -- can populate the KIP-516 TopicId on each
        -- TopicProduceData entry; the cache maintains
        -- 'topicMetaTopicId' from the v10+ MetadataResponse.
        request <- buildProduceRequest senderMetadata senderAcks senderTimeoutMs validBatches
        -- ProduceRequest: codegen handles up to v13. The request
        -- shape is stable from v3 onwards (v3 added the
        -- transactional id, which we send as Null on the
        -- non-transactional path). v9 went flexible (the broker
        -- expects compact strings + tagged-fields trailer); v10+
        -- added per-partition response fields (KIP-467
        -- 'RecordErrors' + 'ErrorMessage') that our decoder
        -- handles via the codegen, and the codegen-flexible-
        -- tagged-string fix in this branch unblocks v12 against
        -- Kafka 3.7.
        --
        -- v13 swapped the per-topic name field for a KIP-516
        -- TopicId; we plumb the id through the metadata cache
        -- ('Meta.getTopicId') and 'buildTopicProduceData' fills
        -- in 'topicProduceDataTopicId' for v13+, or leaves it
        -- nullUuid for v0-v12 (which expect the name).
        --
        -- Cap at v13.
        verR <- VN.pickApiVersion senderVersionCache brokerAddr
                  0  {- API key 0 = Produce -}
                  3 13 3
        let apiVersion = case verR of
              Right v -> v
              Left  _ -> 3
            apiKey = 0
            clientId = P.mkKafkaString senderClientId
            requestBody = WC.runEncodeVer PR.encodeProduceRequest apiVersion request
        
        -- Send the request and receive response
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientId requestBody
        
        case result of
          Left err -> do
            -- Invoke error callbacks
            forM_ validBatches $ \batch -> do
              let callbacks = BA.batchCallbacks batch
                  errorMsg = "Request failed: " <> T.pack err
              forM_ callbacks $ \callback -> callback (Left errorMsg)
          
          Right (responseCorrId, responseBody) -> do
            -- Verify correlation ID matches
            if responseCorrId /= corrId
              then do
                -- Invoke error callbacks
                forM_ validBatches $ \batch -> do
                  let callbacks = BA.batchCallbacks batch
                      errorMsg = "Correlation ID mismatch"
                  forM_ callbacks $ \callback -> callback (Left errorMsg)
              else do
                -- Parse the response
                case WC.runDecodeVer PResp.decodeProduceResponse apiVersion responseBody of
                  Left err -> do
                    -- Invoke error callbacks
                    forM_ validBatches $ \batch -> do
                      let callbacks = BA.batchCallbacks batch
                          errorMsg = "Failed to parse response: " <> T.pack err
                      forM_ callbacks $ \callback -> callback (Left errorMsg)
                  
                  Right response -> do
                    -- Process the response and update batch states
                    processProduceResponse (Just senderMetadata) validBatches response

-- | Build a ProduceRequest from a list of batches.
-- Compression happens here, so this is an IO operation.
-- The metadata cache is consulted per-topic to populate the
-- KIP-516 TopicId field; on pre-v10 brokers (or topics not yet
-- in the cache) the field stays 'P.nullUuid' which is fine for
-- v0-v12 (where the encoder ignores it). v13+ requires a real
-- topic id.
buildProduceRequest
  :: Meta.MetadataCache
  -> Int16
  -> Int32
  -> [BA.ProducerBatch]
  -> IO PR.ProduceRequest
buildProduceRequest metaCache acks timeoutMs batches = do
  -- Group batches by topic
  let batchesByTopic = groupBy (\b1 b2 -> BA.tpTopic (BA.batchTopicPartition b1) == BA.tpTopic (BA.batchTopicPartition b2))
                     $ sortBy (comparing (BA.tpTopic . BA.batchTopicPartition)) batches

  -- Build TopicProduceData for each topic (with compression, so IO)
  topicData <- mapM (buildTopicProduceData metaCache) batchesByTopic

  return $ PR.ProduceRequest
    { PR.produceRequestTransactionalId = P.KafkaString P.Null
    , PR.produceRequestAcks = acks
    , PR.produceRequestTimeoutMs = timeoutMs
    , PR.produceRequestTopicData = P.mkKafkaArray (V.fromList topicData)
    }

-- | Build TopicProduceData for a group of batches from the same topic.
-- Compression happens here, so this is an IO operation. Populates
-- the KIP-516 'topicProduceDataTopicId' from the metadata cache
-- when known; falls back to 'P.nullUuid' otherwise (which is
-- what every Produce version through v12 expects in the field).
buildTopicProduceData
  :: Meta.MetadataCache -> [BA.ProducerBatch] -> IO PR.TopicProduceData
buildTopicProduceData metaCache batches = do
  let topic = BA.tpTopic $ BA.batchTopicPartition $ head batches
  topicIdM <- atomically (Meta.getTopicId metaCache topic)
  partitionData <- mapM buildPartitionProduceData batches
  return $ PR.TopicProduceData
    { PR.topicProduceDataName = P.mkKafkaString topic
    , PR.topicProduceDataTopicId =
        -- 'getTopicId' returns 'Nothing' before the cache is
        -- populated; that path keeps the field at nullUuid
        -- which is what v0-v12 expect anyway.
        case topicIdM of
          Just tid -> tid
          Nothing  -> P.nullUuid
    , PR.topicProduceDataPartitionData = P.mkKafkaArray (V.fromList partitionData)
    }

-- | Build PartitionProduceData for a single batch
-- Applies compression using the batch's compression codec and level
buildPartitionProduceData :: BA.ProducerBatch -> IO PR.PartitionProduceData
buildPartitionProduceData batch = do
  let partition = BA.tpPartition $ BA.batchTopicPartition batch
      recordBatch = buildRecordBatch batch
      compressionLevel = BA.batchCompressionLevel batch
      codec = RB.attrCompressionType (RB.batchAttributes recordBatch)

  -- Fast path for the uncompressed common case: skip the
  -- compression layer entirely (it's a no-op for NoCompression
  -- but still pays a runPutS hop for the records section). The
  -- direct-poke Wire encoder writes the whole batch in a single
  -- pass — ~10x faster than the legacy Builder shape.
  if codec == NoCompression
    then pure $ PR.PartitionProduceData
      { PR.partitionProduceDataIndex   = partition
      , PR.partitionProduceDataRecords = P.mkKafkaBytes (RBW.encodeRecordBatchWire recordBatch)
      }
    else do
      -- Compressed path now goes through the Wire-based
      -- compressed encoder ('encodeRecordBatchWireCompressedWithLevel'):
      -- the records section is built once via 'encodeRecordsWire'
      -- (~10x faster than the legacy Builder-per-record loop)
      -- and the batch envelope is back-patched in place. Bytes
      -- are byte-identical with 'RB.encodeRecordBatchWithCompressionLevel'
      -- (verified by 'Protocol.RecordBatchWireSpec').
      encodeResult <- RBW.encodeRecordBatchWireCompressedWithLevel recordBatch compressionLevel
      case encodeResult of
        Left err -> do
          -- Compression failed; fall back to uncompressed
          -- encoding via the direct-poke Wire encoder. Rare,
          -- so we log to stderr rather than thread a 'Logger'
          -- through this otherwise pure-ish helper.
          hPutStrLn stderr
            ("[warn] producer: compression failed for batch, "
              <> "sending uncompressed: " <> err)
          pure $ PR.PartitionProduceData
            { PR.partitionProduceDataIndex   = partition
            , PR.partitionProduceDataRecords = P.mkKafkaBytes (RBW.encodeRecordBatchWire recordBatch)
            }
        Right recordBytes ->
          pure $ PR.PartitionProduceData
            { PR.partitionProduceDataIndex   = partition
            , PR.partitionProduceDataRecords = P.mkKafkaBytes recordBytes
            }

-- | Build a RecordBatch from a ProducerBatch. Idempotent /
-- transactional state is read off the batch itself
-- ('batchProducerId', 'batchProducerEpoch', 'batchBaseSequence',
-- 'batchIsTransactional'); the producer-side 'sendMessage' /
-- 'sendBatch' path stamps these fields when @producerIdempotent@
-- or @producerTransactional@ is enabled, via 'BA.appendRecordStamped'.
buildRecordBatch :: BA.ProducerBatch -> RB.RecordBatch
buildRecordBatch batch =
  let !recSeq = BA.batchRecords batch
      -- Single-pass Seq -> Vector: ask 'Vector' to fill itself
      -- from the 'Seq' length + indexed access. Avoids the
      -- intermediate list spine 'V.fromList . toList' would
      -- allocate (one cons cell per record).
      !nRec = Seq.length recSeq
      !records = V.generate nRec (Seq.index recSeq)
      attrs = RB.Attributes
        { RB.attrCompressionType = BA.batchCompression batch
        , RB.attrTimestampType = RB.CreateTime
        , RB.attrIsTransactional = BA.batchIsTransactional batch
        , RB.attrIsControl = False
        , RB.attrHasDeleteHorizon = False
        }
      baseTimestamp = BA.batchBaseTimestamp batch
      -- Single fold instead of (V.toList + map + maximum +
      -- intermediate list). 'V.foldl'' walks the vector in one
      -- pass and computes the max delta directly.
      !maxTimestamp =
        if nRec == 0
          then baseTimestamp
          else baseTimestamp
                 + V.foldl' (\acc r -> max acc (RB.recordTimestampDelta r))
                            0 records
  in
    RB.RecordBatch
      { RB.batchBaseOffset = 0  -- Broker will assign
      , RB.batchPartitionLeaderEpoch = RB.noPartitionLeaderEpoch
      , RB.batchAttributes = attrs
      , RB.batchLastOffsetDelta = fromIntegral (V.length records) - 1
      , RB.batchBaseTimestamp = baseTimestamp
      , RB.batchMaxTimestamp = maxTimestamp
      , RB.batchProducerId = BA.batchProducerId batch
      , RB.batchProducerEpoch = BA.batchProducerEpoch batch
      , RB.batchBaseSequence = BA.batchBaseSequence batch
      , RB.batchRecords = records
      }

-- | Determine if a batch should be retried. Compares the batch's
-- accrued 'BA.batchAttempts' against the sender's configured
-- 'retryMaxAttempts'. The sender increments 'batchAttempts' via
-- 'bumpBatchAttempts' before re-enqueuing the batch.
shouldRetry :: SenderState -> BA.ProducerBatch -> Bool
shouldRetry SenderState{..} batch =
  BA.batchAttempts batch < retryMaxAttempts senderRetryConfig

-- | Compute the next backoff for /this specific/ batch's accrued
-- attempt count.
batchBackoffMs :: SenderState -> BA.ProducerBatch -> Int
batchBackoffMs SenderState{..} batch =
  nextRetryBackoffMs senderRetryConfig (BA.batchAttempts batch)

-- | Increment the batch's attempt counter, returning the updated
-- batch. The sender calls this just before re-enqueueing the
-- batch onto the partition queue.
bumpBatchAttempts :: BA.ProducerBatch -> BA.ProducerBatch
bumpBatchAttempts batch =
  batch { BA.batchAttempts = BA.batchAttempts batch + 1 }

-- | Retry a failed batch: wait for this batch's exponential
-- backoff, then bump its attempt counter. The caller is
-- responsible for actually re-enqueueing the returned batch.
retryBatch :: SenderState -> BA.ProducerBatch -> IO BA.ProducerBatch
retryBatch state batch = do
  let backoffMs = batchBackoffMs state batch
  threadDelay (backoffMs * 1000)
  pure (bumpBatchAttempts batch)

-- | Process a ProduceResponse and update batch states. Honours
-- the broker's @ThrottleTimeMs@ (KIP-219): if the broker has
-- requested back-pressure, the sender thread sleeps for that
-- duration /before/ returning to its loop, so the next produce
-- request waits the broker-requested interval. This is the same
-- behaviour the JVM client / librdkafka implement.
processProduceResponse
  :: Maybe Meta.MetadataCache  -- ^ Optional metadata cache for KIP-466 leader-cache patches
  -> [BA.ProducerBatch]
  -> PResp.ProduceResponse
  -> IO ()
processProduceResponse metaCacheM batches response = do
  -- KIP-219 throttle. Negative / zero values are no-ops.
  let throttleMs = PResp.produceResponseThrottleTimeMs response
  when (throttleMs > 0) $
    threadDelay (fromIntegral throttleMs * 1000)

  -- Extract topic responses
  let topics = case P.unKafkaArray (PResp.produceResponseResponses response) of
        P.Null -> V.empty
        P.NotNull vec -> vec

  -- Pre-index outbound batches by their (topic, partition) so
  -- the per-response lookup is O(1) instead of an O(#batches)
  -- 'filter' on every partition row. For the common one-batch-
  -- per-(topic, partition) shape the map values are singleton
  -- lists (HashMap is keyed on 'TopicPartition' which already
  -- has a 'Hashable' instance via 'BA.TopicPartition').
  let batchIndex :: HashMap BA.TopicPartition [BA.ProducerBatch]
      !batchIndex = HashMap.fromListWith (++)
        [ (BA.batchTopicPartition b, [b]) | b <- batches ]

  -- Process each topic response
  V.forM_ topics $ \topicResp -> do
    let topicName = extractText $ PResp.topicProduceResponseName topicResp
        partitions = case P.unKafkaArray (PResp.topicProduceResponsePartitionResponses topicResp) of
          P.Null -> V.empty
          P.NotNull vec -> vec

    -- Process each partition response
    V.forM_ partitions $ \partResp -> do
      let partitionId = PResp.partitionProduceResponseIndex partResp
          errorCode = PResp.partitionProduceResponseErrorCode partResp
          baseOffset = PResp.partitionProduceResponseBaseOffset partResp
          curLeader   = PResp.partitionProduceResponseCurrentLeader partResp
          curLeaderId = PResp.leaderIdAndEpochLeaderId curLeader

      -- KIP-466: if the broker reports a CurrentLeader for this
      -- partition (>= 0), patch the metadata cache so the next
      -- produce goes to that broker without an extra
      -- MetadataRequest round-trip.
      case metaCacheM of
        Just cache | curLeaderId >= 0 ->
          atomically $
            Meta.updatePartitionLeader cache topicName partitionId curLeaderId
        _ -> pure ()

      -- O(1) average lookup into the pre-built index.
      let !key = BA.TopicPartition topicName partitionId
          matchingBatches = HashMap.lookupDefault [] key batchIndex

      -- Update batch state based on error code
      forM_ matchingBatches $ \batch -> do
        if errorCode == 0
          then do
            -- Success: complete each record callback with its
            -- assigned offset. We deliberately don't log on every
            -- success — application code observes deliveries via
            -- the per-record callbacks, which are the public
            -- success channel.
            let callbacks = BA.batchCallbacks batch
            timestamp <- KafkaTime.currentTimeMillis
            -- (Per-record timestamps aren't surfaced in
            -- ProduceResponse; the per-record
            -- 'PartitionProduceResponse.LogAppendTimeMs' is
            -- only set when the topic is in @LogAppendTime@ mode,
            -- and we can't tell here, so we use the wall-clock
            -- timestamp as a reasonable fallback.)
            -- 'callbacks' is a 'Seq', so we walk the sequence
            -- with its index to assign each record's offset.
            forM_ (Seq.mapWithIndex (,) callbacks) $ \(idx, callback) -> do
              let recordOffset = baseOffset + fromIntegral (idx :: Int)
                  metadata = (topicName, partitionId, recordOffset, timestamp)
              callback (Right metadata)
          else do
            hPutStrLn stderr
              ("[error] producer: error sending batch to "
                <> T.unpack topicName <> " partition "
                <> show partitionId
                <> ": error code " <> show errorCode)
            let callbacks = BA.batchCallbacks batch
                errorMsg = "Kafka error: " <> T.pack (show errorCode)
            forM_ callbacks $ \callback -> do
              callback (Left errorMsg)

-- | Extract text from a KafkaString
extractText :: P.KafkaString -> Text
extractText (P.KafkaString P.Null) = ""
extractText (P.KafkaString (P.NotNull t)) = t

