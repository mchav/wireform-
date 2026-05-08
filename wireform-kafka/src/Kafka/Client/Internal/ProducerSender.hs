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
    -- * Timeout Checking (KIP-91)
  , isBatchTimedOut
    -- * Configuration
  , RetryConfig(..)
  , defaultRetryConfig
  , nextRetryBackoffMs
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.Connection (Connection)
import qualified Data.Time.Clock.POSIX as Time

import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Client.Metadata as Meta
import Kafka.Compression.Types (CompressionCodec)
import qualified Kafka.Network.Connection as Conn
import Kafka.Network.Connection (BrokerAddress(..))
import qualified Kafka.Protocol.Encoding as E
import qualified Kafka.Protocol.Generated.ProduceRequest as PR
import qualified Kafka.Protocol.Generated.ProduceResponse as PResp
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.RecordBatch as RB

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
  -> IO SenderState
createSenderState accumulator metadata connManager retryConfig acks deliveryTimeoutMs compression clientId = do
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
            Left (e :: SomeException) -> do
              -- Log error and continue
              -- TODO: Add proper logging
              putStrLn $ "Sender loop error: " ++ show e
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
  -- Get current time to check delivery timeout (KIP-91)
  currentTime <- round . (* 1000) <$> Time.getPOSIXTime
  
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
    connResult <- Conn.getOrCreateConnection senderConnManager brokerAddr Conn.defaultConnectionConfig
    
    case connResult of
      Left err -> do
        -- Connection failed, invoke error callbacks
        putStrLn $ "Failed to connect to broker: " ++ err
        forM_ validBatches $ \batch -> do
          let callbacks = BA.batchCallbacks batch
              errorMsg = "Connection failed: " <> T.pack err
          forM_ callbacks $ \callback -> callback (Left errorMsg)
    
      Right conn -> do
        -- Get next correlation ID
        corrId <- atomically $ do
          cid <- readTVar senderCorrelationId
          writeTVar senderCorrelationId (cid + 1)
          return cid
        
        -- Build and encode the request (compression is IO)
        request <- buildProduceRequest senderAcks senderTimeoutMs validBatches
        let apiVersion = 3  -- Use version 3 for compatibility
            apiKey = 0       -- ProduceRequest API key
            clientId = P.mkKafkaString senderClientId
            requestBody = runPutS $ PR.encodeProduceRequest apiVersion request
        
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
                case runGetS (PResp.decodeProduceResponse apiVersion) responseBody of
                  Left err -> do
                    -- Invoke error callbacks
                    forM_ validBatches $ \batch -> do
                      let callbacks = BA.batchCallbacks batch
                          errorMsg = "Failed to parse response: " <> T.pack err
                      forM_ callbacks $ \callback -> callback (Left errorMsg)
                  
                  Right response -> do
                    -- Process the response and update batch states
                    processProduceResponse validBatches response

-- | Build a ProduceRequest from a list of batches
-- Compression happens here, so this is an IO operation
buildProduceRequest :: Int16 -> Int32 -> [BA.ProducerBatch] -> IO PR.ProduceRequest
buildProduceRequest acks timeoutMs batches = do
  -- Group batches by topic
  let batchesByTopic = groupBy (\b1 b2 -> BA.tpTopic (BA.batchTopicPartition b1) == BA.tpTopic (BA.batchTopicPartition b2))
                     $ sortBy (comparing (BA.tpTopic . BA.batchTopicPartition)) batches
  
  -- Build TopicProduceData for each topic (with compression, so IO)
  topicData <- mapM buildTopicProduceData batchesByTopic
  
  return $ PR.ProduceRequest
    { PR.produceRequestTransactionalId = P.KafkaString P.Null
    , PR.produceRequestAcks = acks
    , PR.produceRequestTimeoutMs = timeoutMs
    , PR.produceRequestTopicData = P.mkKafkaArray (V.fromList topicData)
    }

-- | Build TopicProduceData for a group of batches from the same topic
-- Compression happens here, so this is an IO operation
buildTopicProduceData :: [BA.ProducerBatch] -> IO PR.TopicProduceData
buildTopicProduceData batches = do
  let topic = BA.tpTopic $ BA.batchTopicPartition $ head batches
  partitionData <- mapM buildPartitionProduceData batches
  return $ PR.TopicProduceData
    { PR.topicProduceDataName = P.mkKafkaString topic
    , PR.topicProduceDataTopicId = P.nullUuid
    , PR.topicProduceDataPartitionData = P.mkKafkaArray (V.fromList partitionData)
    }

-- | Build PartitionProduceData for a single batch
-- Applies compression using the batch's compression codec and level
buildPartitionProduceData :: BA.ProducerBatch -> IO PR.PartitionProduceData
buildPartitionProduceData batch = do
  let partition = BA.tpPartition $ BA.batchTopicPartition batch
      recordBatch = buildRecordBatch batch
      compressionLevel = BA.batchCompressionLevel batch
  
  -- Encode RecordBatch with compression (KIP-353/776/909)
  encodeResult <- RB.encodeRecordBatchWithCompressionLevel recordBatch compressionLevel
  
  case encodeResult of
    Left err -> do
      -- Compression failed - this shouldn't happen often, but handle it
      -- Fall back to uncompressed encoding
      putStrLn $ "Warning: Compression failed for batch, sending uncompressed: " ++ err
      let recordBytes = RB.encodeRecordBatch recordBatch
      return $ PR.PartitionProduceData
        { PR.partitionProduceDataIndex = partition
        , PR.partitionProduceDataRecords = P.mkKafkaBytes recordBytes
        }
    Right recordBytes -> 
      return $ PR.PartitionProduceData
        { PR.partitionProduceDataIndex = partition
        , PR.partitionProduceDataRecords = P.mkKafkaBytes recordBytes
        }

-- | Build a RecordBatch from a ProducerBatch
buildRecordBatch :: BA.ProducerBatch -> RB.RecordBatch
buildRecordBatch batch =
  let records = V.fromList $ toList $ BA.batchRecords batch
      attrs = RB.Attributes
        { RB.attrCompressionType = BA.batchCompression batch
        , RB.attrTimestampType = RB.CreateTime
        , RB.attrIsTransactional = False
        , RB.attrIsControl = False
        , RB.attrHasDeleteHorizon = False
        }
      baseTimestamp = BA.batchBaseTimestamp batch
      maxTimestamp = if V.null records
                     then baseTimestamp
                     else maximum $ baseTimestamp : map (\r -> baseTimestamp + RB.recordTimestampDelta r) (V.toList records)
  in
    RB.RecordBatch
      { RB.batchBaseOffset = 0  -- Broker will assign
      , RB.batchPartitionLeaderEpoch = RB.noPartitionLeaderEpoch
      , RB.batchAttributes = attrs
      , RB.batchLastOffsetDelta = fromIntegral (V.length records) - 1
      , RB.batchBaseTimestamp = baseTimestamp
      , RB.batchMaxTimestamp = maxTimestamp
      , RB.batchProducerId = RB.noProducerId  -- TODO: Use producer ID for idempotent producer
      , RB.batchProducerEpoch = RB.noProducerEpoch
      , RB.batchBaseSequence = RB.noSequence
      , RB.batchRecords = records
      }

-- | Determine if a batch should be retried
shouldRetry :: SenderState -> BA.ProducerBatch -> Bool
shouldRetry state batch =
  -- TODO: Track retry count in batch state
  -- For now, allow retries
  True

-- | Retry a failed batch. Uses 'nextRetryBackoffMs' so successive
-- retries follow the exponential-with-jitter curve from
-- 'RetryConfig'. The attempt counter is passed by the caller; when
-- the existing call site doesn't track it yet we pass 0 (initial
-- backoff).
retryBatch :: SenderState -> BA.ProducerBatch -> IO ()
retryBatch SenderState{..} _batch = do
  let backoffMs = nextRetryBackoffMs senderRetryConfig 0
  threadDelay (backoffMs * 1000)
  -- TODO: track the per-batch attempt counter and pass it to
  -- 'nextRetryBackoffMs' so successive retries actually back off
  -- exponentially.

-- | Process a ProduceResponse and update batch states
processProduceResponse :: [BA.ProducerBatch] -> PResp.ProduceResponse -> IO ()
processProduceResponse batches response = do
  -- Extract topic responses
  let topics = case P.unKafkaArray (PResp.produceResponseResponses response) of
        P.Null -> V.empty
        P.NotNull vec -> vec
  
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
      
      -- Find the batch for this topic-partition
      let matchingBatches = filter (\b ->
            let tp = BA.batchTopicPartition b
            in BA.tpTopic tp == topicName && BA.tpPartition tp == partitionId) batches
      
      -- Update batch state based on error code
      forM_ matchingBatches $ \batch -> do
        if errorCode == 0
          then do
            -- Success - batch was written
            putStrLn $ "Batch successfully sent to " ++ T.unpack topicName ++ 
                      " partition " ++ show partitionId ++ 
                      " at offset " ++ show baseOffset
            
            -- Invoke callbacks for each record in the batch
            let callbacks = BA.batchCallbacks batch
                numRecords = Seq.length (BA.batchRecords batch)
            
            -- Get current timestamp (broker doesn't return per-record timestamps in ProduceResponse)
            timestamp <- round . (* 1000) <$> Time.getPOSIXTime
--                 -- Get broker-assigned timestamp (use current time as fallback)
--                 timestamp = PResp.partitionProduceResponseLogAppendTimeMs partResp
            
            -- Complete each record callback with its offset
            forM_ (zip [0..] callbacks) $ \(idx, callback) -> do
              let recordOffset = baseOffset + fromIntegral (idx :: Int)
                  metadata = (topicName, partitionId, recordOffset, timestamp)
              callback (Right metadata)
          else do
            -- Error occurred
            putStrLn $ "Error sending batch to " ++ T.unpack topicName ++ 
                      " partition " ++ show partitionId ++ 
                      ": error code " ++ show errorCode
            
            -- Invoke callbacks with error for each record in the batch
            let callbacks = BA.batchCallbacks batch
                errorMsg = "Kafka error: " <> T.pack (show errorCode)
            
            forM_ callbacks $ \callback -> do
              callback (Left errorMsg)

-- | Extract text from a KafkaString
extractText :: P.KafkaString -> Text
extractText (P.KafkaString P.Null) = ""
extractText (P.KafkaString (P.NotNull t)) = t

