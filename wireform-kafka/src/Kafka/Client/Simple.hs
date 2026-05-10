{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Kafka.Client.Simple
Description : Simple synchronous Kafka client for basic operations
Copyright   : (c) 2025
License     : BSD-3-Clause

A simple, synchronous Kafka client implementation for basic operations.
This module provides straightforward produce and consume functionality
without the complexity of the full asynchronous client.

This is suitable for:
- Testing and development
- Simple applications with low throughput requirements
- Learning the Kafka protocol

For production use cases with high throughput, use the full Producer
and Consumer APIs instead.

-}
module Kafka.Client.Simple
  ( -- * Simple Client
    SimpleClient
  , createSimpleClient
  , closeSimpleClient
    -- * Metadata Operations
  , getMetadata
  , BrokerInfo(..)
  , TopicInfo(..)
  , PartitionInfo(..)
    -- * Produce Operations
  , produceSimple
  , ProduceResult(..)
    -- * Fetch Operations
  , fetchSimple
  , FetchResult(..)
  , Record(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Kafka.Time as KafkaTime
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)

import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Generated.MetadataRequest as MR
import qualified Kafka.Protocol.Generated.MetadataResponse as MResp
import qualified Kafka.Protocol.Generated.ProduceRequest as PReq
import qualified Kafka.Protocol.Generated.ProduceResponse as PResp
import qualified Kafka.Protocol.Generated.FetchRequest as FR
import qualified Kafka.Protocol.Generated.FetchResponse as FResp

import Kafka.Client.Internal.Request
import qualified Kafka.Protocol.RecordBatch as RB
import qualified Kafka.Protocol.RecordBatchWire as RBW
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Codec as WC

-- | A simple Kafka client with synchronous operations
data SimpleClient = SimpleClient
  { clientConnection :: Conn.Connection
  , clientCorrelationId :: Int32  -- Simple counter, not thread-safe
  , clientConfig :: ClientConfig
  }

data ClientConfig = ClientConfig
  { clientId :: Text
  , clientTimeout :: Int
  }
  deriving (Eq, Show, Generic)

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
  { clientId = "kafka-native-simple"
  , clientTimeout = 30000  -- 30 seconds
  }

-- | Information about a broker
data BrokerInfo = BrokerInfo
  { brokerNodeId :: Int32
  , brokerHost :: Text
  , brokerPort :: Int32
  } deriving (Eq, Show, Generic)

-- | Information about a partition
data PartitionInfo = PartitionInfo
  { partitionId :: Int32
  , partitionLeader :: Int32
  , partitionReplicas :: [Int32]
  } deriving (Eq, Show, Generic)

-- | Information about a topic
data TopicInfo = TopicInfo
  { topicName :: Text
  , topicPartitions :: [PartitionInfo]
  , topicErrorCode :: Int16
  } deriving (Eq, Show, Generic)

-- | Result of a produce operation
data ProduceResult = ProduceResult
  { producePartition :: Int32
  , produceOffset :: Int64
  , produceErrorCode :: Int16
  } deriving (Eq, Show, Generic)

-- | A simple record
data Record = Record
  { recordOffset :: Int64
  , recordKey :: Maybe ByteString
  , recordValue :: ByteString
  , recordTimestamp :: Int64
  } deriving (Eq, Show, Generic)

-- | Result of a fetch operation
data FetchResult = FetchResult
  { fetchPartition :: Int32
  , fetchRecords :: [Record]
  , fetchErrorCode :: Int16
  } deriving (Eq, Show, Generic)

-- | Create a simple client connected to a single broker
createSimpleClient 
  :: String  -- ^ Broker host
  -> Word16  -- ^ Broker port
  -> IO (Either String SimpleClient)
createSimpleClient host port = do
  let addr = Conn.BrokerAddress host (fromIntegral port)
      config = Conn.defaultConnectionConfig
  
  connResult <- Conn.connect addr config
  case connResult of
    Left err -> return $ Left err
    Right conn -> return $ Right $ SimpleClient
      { clientConnection = conn
      , clientCorrelationId = 1
      , clientConfig = defaultClientConfig
      }

-- | Close the simple client
closeSimpleClient :: SimpleClient -> IO ()
closeSimpleClient client = Conn.disconnect (clientConnection client)

-- | Get a new correlation ID (not thread-safe, increments internal counter)
nextCorrelationId :: SimpleClient -> (Int32, SimpleClient)
nextCorrelationId client@SimpleClient{..} =
  (clientCorrelationId, client { clientCorrelationId = clientCorrelationId + 1 })

-- | Get cluster metadata
getMetadata 
  :: SimpleClient 
  -> Maybe [Text]  -- ^ Specific topics (Nothing = all topics)
  -> IO (Either String ([BrokerInfo], [TopicInfo]))
getMetadata client topicsM = do
  let (corrId, client') = nextCorrelationId client
      topics = case topicsM of
        Nothing -> P.mkKafkaArray V.empty
        Just ts -> P.mkKafkaArray $ V.fromList $ map (\t -> MR.MetadataRequestTopic
          { MR.metadataRequestTopicTopicId = P.nullUuid
          , MR.metadataRequestTopicName = P.mkKafkaString t
          }) ts
      
      req = MR.MetadataRequest
        { MR.metadataRequestTopics = topics
        , MR.metadataRequestAllowAutoTopicCreation = True
        , MR.metadataRequestIncludeClusterAuthorizedOperations = False
        , MR.metadataRequestIncludeTopicAuthorizedOperations = False
        }
      
      apiVersion = 0  -- Use version 0 for maximum compatibility
      reqBody = WC.runEncodeVer @MR.MetadataRequest apiVersion req
      clientIdStr = P.mkKafkaString $ clientId $ clientConfig client
  
  result <- sendRequestReceiveResponse 
    (clientConnection client)
    3  -- Metadata API key
    (fromIntegral apiVersion)
    corrId
    clientIdStr
    reqBody
  
  case result of
    Left err -> return $ Left err
    Right (respCorrId, respBody) ->
      if respCorrId /= corrId
        then return $ Left $ "Correlation ID mismatch: expected " ++ show corrId ++ ", got " ++ show respCorrId
        else case WC.runDecodeVer @MResp.MetadataResponse apiVersion respBody of
          Left err -> return $ Left $ "Failed to decode metadata response: " ++ err
          Right resp -> do
            let brokers = extractBrokers resp
                topics = extractTopics resp
            return $ Right (brokers, topics)

-- | Extract broker information from metadata response
extractBrokers :: MResp.MetadataResponse -> [BrokerInfo]
extractBrokers resp =
  case P.unKafkaArray (MResp.metadataResponseBrokers resp) of
    P.Null -> []
    P.NotNull vec -> V.toList $ V.map (\b -> BrokerInfo
      { brokerNodeId = MResp.metadataResponseBrokerNodeId b
      , brokerHost = extractText $ MResp.metadataResponseBrokerHost b
      , brokerPort = MResp.metadataResponseBrokerPort b
      }) vec

-- | Extract topic information from metadata response
extractTopics :: MResp.MetadataResponse -> [TopicInfo]
extractTopics resp =
  case P.unKafkaArray (MResp.metadataResponseTopics resp) of
    P.Null -> []
    P.NotNull vec -> V.toList $ V.map extractTopicInfo vec

extractTopicInfo :: MResp.MetadataResponseTopic -> TopicInfo
extractTopicInfo topic = TopicInfo
  { topicName = extractText $ MResp.metadataResponseTopicName topic
  , topicPartitions = extractPartitions topic
  , topicErrorCode = MResp.metadataResponseTopicErrorCode topic
  }

extractPartitions :: MResp.MetadataResponseTopic -> [PartitionInfo]
extractPartitions topic =
  case P.unKafkaArray (MResp.metadataResponseTopicPartitions topic) of
    P.Null      -> []
    P.NotNull v -> V.toList (V.map extractPartitionInfo v)

extractPartitionInfo :: MResp.MetadataResponsePartition -> PartitionInfo
extractPartitionInfo p =
  PartitionInfo
    { partitionId       = MResp.metadataResponsePartitionPartitionIndex p
    , partitionLeader   = MResp.metadataResponsePartitionLeaderId p
    , partitionReplicas = case P.unKafkaArray (MResp.metadataResponsePartitionReplicaNodes p) of
        P.Null      -> []
        P.NotNull v -> V.toList v
    }

-- | Helper to extract Text from KafkaString
extractText :: P.KafkaString -> Text
extractText ks = case P.unKafkaString ks of
  P.Null -> T.empty
  P.NotNull t -> t

-- | Produce a simple record to a topic partition
--
-- Note: This sends a single record without batching.
-- For production use, use the full Producer API with batching.
produceSimple
  :: SimpleClient
  -> Text        -- ^ Topic
  -> Int32       -- ^ Partition
  -> Maybe ByteString  -- ^ Key
  -> ByteString  -- ^ Value
  -> IO (Either String ProduceResult)
produceSimple client topic partition keyM value = do
  -- Get current timestamp via the fast vDSO-coarse clock.
  timestamp <- KafkaTime.currentTimeMillis
  
  -- Create a single record
  let record = RB.Record
        { RB.recordTimestampDelta = 0  -- First record in batch, no delta
        , RB.recordOffsetDelta = 0     -- First record in batch
        , RB.recordKey = keyM
        , RB.recordValue = value
        , RB.recordHeaders = []        -- No headers for simple produce
        }
  
  -- Create a RecordBatch with the single record
  let batch = RB.mkSimpleBatch 
        0               -- Base offset (broker will assign actual offset)
        timestamp       -- Base timestamp
        (V.singleton record)
  
  -- Encode the batch via the direct-poke Wire encoder
  -- (~10x faster than the legacy Builder shape).
  let batchBytes = RBW.encodeRecordBatchWire batch
      recordsField = P.mkKafkaBytes batchBytes
  
  -- Create partition data
  let partitionData = PReq.PartitionProduceData
        { PReq.partitionProduceDataIndex = partition
        , PReq.partitionProduceDataRecords = recordsField
        }
  
  -- Create topic data
  let topicData = PReq.TopicProduceData
        { PReq.topicProduceDataName = P.mkKafkaString topic
        , PReq.topicProduceDataTopicId = P.nullUuid
        , PReq.topicProduceDataPartitionData = P.mkKafkaArray (V.singleton partitionData)
        }
  
  -- Create the produce request
  -- Using version 3 (minimum supported version for ProduceRequest)
  let apiVersion = 3
      request = PReq.ProduceRequest
        { PReq.produceRequestTransactionalId = P.KafkaString P.Null
        , PReq.produceRequestAcks = 1  -- Wait for leader acknowledgment
        , PReq.produceRequestTimeoutMs = fromIntegral (clientTimeout $ clientConfig client)
        , PReq.produceRequestTopicData = P.mkKafkaArray (V.singleton topicData)
        }
  
  -- Encode the request body
  let requestBody = WC.runEncodeVer @PReq.ProduceRequest apiVersion request
      correlationId = clientCorrelationId client
      clientIdStr = P.mkKafkaString (clientId $ clientConfig client)
  
  -- Send the request using the proper helper function
  result <- sendRequestReceiveResponse
    (clientConnection client)
    0  -- Produce API key
    (fromIntegral apiVersion)
    correlationId
    clientIdStr
    requestBody
  
  case result of
    Left err -> return $ Left err
    Right (respCorrelationId, responseBody) -> do
      if respCorrelationId /= correlationId
        then return $ Left $ "Correlation ID mismatch: expected " ++ 
                             show correlationId ++ ", got " ++ show respCorrelationId
        else case WC.runDecodeVer @PResp.ProduceResponse apiVersion responseBody of
          Left err -> return $ Left $ "Failed to decode produce response: " ++ err
          Right response -> do
            -- Extract the result from the response
            case extractProduceResult response topic partition of
              Nothing -> return $ Left "No matching topic/partition in response"
              Just result -> return $ Right result

-- | Extract produce result from a ProduceResponse
extractProduceResult :: PResp.ProduceResponse -> Text -> Int32 -> Maybe ProduceResult
extractProduceResult response topic partition =
  let topics = case P.unKafkaArray (PResp.produceResponseResponses response) of
                P.NotNull v -> V.toList v
                P.Null -> []
      matchingTopic = find (\t -> extractText (PResp.topicProduceResponseName t) == topic) topics
  in case matchingTopic of
    Nothing -> Nothing
    Just topicResp ->
      let partitions = case P.unKafkaArray (PResp.topicProduceResponsePartitionResponses topicResp) of
                        P.NotNull v -> V.toList v
                        P.Null -> []
          matchingPartition = find (\p -> PResp.partitionProduceResponseIndex p == partition) partitions
      in case matchingPartition of
        Nothing -> Nothing
        Just partResp -> Just $ ProduceResult
          { producePartition = PResp.partitionProduceResponseIndex partResp
          , produceOffset = PResp.partitionProduceResponseBaseOffset partResp
          , produceErrorCode = PResp.partitionProduceResponseErrorCode partResp
          }

-- | Extract fetch result from a FetchResponse
-- This function performs IO to decompress RecordBatches
extractFetchResult :: FResp.FetchResponse -> Text -> Int32 -> IO (Maybe FetchResult)
extractFetchResult response topic partition = do
  let topics = case P.unKafkaArray (FResp.fetchResponseResponses response) of
                P.NotNull v -> V.toList v
                P.Null -> []
      matchingTopic = find (\t -> extractText (FResp.fetchableTopicResponseTopic t) == topic) topics
  case matchingTopic of
    Nothing -> return Nothing
    Just topicResp -> do
      let partitions = case P.unKafkaArray (FResp.fetchableTopicResponsePartitions topicResp) of
                        P.NotNull v -> V.toList v
                        P.Null -> []
          matchingPartition = find (\p -> FResp.partitionDataPartitionIndex p == partition) partitions
      case matchingPartition of
        Nothing -> return Nothing
        Just partResp -> do
          let errorCode = FResp.partitionDataErrorCode partResp
              recordBytes = FResp.partitionDataRecords partResp
          -- Decode the RecordBatch(es) from the bytes
          records <- decodeRecordBatches recordBytes
          return $ Just $ FetchResult
            { fetchPartition = FResp.partitionDataPartitionIndex partResp
            , fetchRecords = records
            , fetchErrorCode = errorCode
            }

-- | Decode one or more RecordBatches from KafkaBytes
-- Returns a list of Records extracted from all batches
decodeRecordBatches :: P.KafkaBytes -> IO [Record]
decodeRecordBatches kafkaBytes =
  case P.unKafkaBytes kafkaBytes of
    P.Null -> return []
    P.NotNull bytes ->
      if BS.null bytes
        then return []
        else decodeBatches bytes ([] :: [[Record]])
  where
    -- Recursively decode batches. Accumulate /chunks/ of records in
    -- reverse order, then 'concat . reverse' once at the end. The
    -- previous shape did 'batchRecords ++ acc' on every iteration,
    -- which is O(|acc|) per step and O(n^2) total in record count.
    decodeBatches :: ByteString -> [[Record]] -> IO [Record]
    decodeBatches bs chunks
      | BS.null bs = return $! concat (reverse chunks)
      | otherwise = do
          -- 'RBW.decodeRecordBatchWireWithDecompression' replaces
          -- the legacy Serial-shape 'RB.decodeRecordBatchWithDecompression';
          -- byte-identical wire output, no 'Data.Bytes.Get' on the
          -- runtime path.
          result <- RBW.decodeRecordBatchWireWithDecompression bs
          case result of
            Left _err ->
              -- If decode fails, return what we have so far
              -- (In production we might want to log this error.)
              return $! concat (reverse chunks)
            Right batch -> do
              let !batchRecords = convertRecords batch
                  -- Calculate how many bytes this batch consumed
                  -- Base offset (8) + Length field (4) + Length value
                  !batchSize = 8 + 4 + fromIntegral (calculateBatchLength batch)
                  !remaining = BS.drop batchSize bs
              decodeBatches remaining (batchRecords : chunks)
    
    -- Calculate the length field value for a batch (everything after the length field)
    calculateBatchLength :: RB.RecordBatch -> Int32
    calculateBatchLength batch =
      let encoded = RBW.encodeRecordBatchWire batch
          -- Skip base offset (8 bytes) to get to length field
          lengthBytes = BS.take 4 $ BS.drop 8 encoded
      in case W.readInt32BE lengthBytes of
          Left _ -> 0
          Right len -> len

-- | Convert RecordBatch Records to Simple client Records
convertRecords :: RB.RecordBatch -> [Record]
convertRecords batch =
  let baseOffset = RB.batchBaseOffset batch
      baseTimestamp = RB.batchBaseTimestamp batch
      records = RB.batchRecords batch
  in V.toList $ V.map (convertRecord baseOffset baseTimestamp) records

-- | Convert a single RecordBatch Record to a Simple client Record
convertRecord :: Int64 -> Int64 -> RB.Record -> Record
convertRecord baseOffset baseTimestamp rbRecord = Record
  { recordOffset = baseOffset + fromIntegral (RB.recordOffsetDelta rbRecord)
  , recordKey = RB.recordKey rbRecord
  , recordValue = RB.recordValue rbRecord
  , recordTimestamp = baseTimestamp + RB.recordTimestampDelta rbRecord
  }

-- | Fetch records from a topic partition
--
-- Note: This is a simple synchronous fetch.
-- For production use, use the full Consumer API.
fetchSimple
  :: SimpleClient
  -> Text    -- ^ Topic
  -> Int32   -- ^ Partition
  -> Int64   -- ^ Offset to fetch from
  -> Int32   -- ^ Max bytes to fetch
  -> IO (Either String FetchResult)
fetchSimple client topic partition offset maxBytes = do
  let (corrId, client') = nextCorrelationId client
      
      -- Create partition fetch request
      partitionData = FR.FetchPartition
        { FR.fetchPartitionPartition = partition
        , FR.fetchPartitionCurrentLeaderEpoch = -1  -- Not tracking leader epochs
        , FR.fetchPartitionFetchOffset = offset
        , FR.fetchPartitionLastFetchedEpoch = -1
        , FR.fetchPartitionLogStartOffset = -1
        , FR.fetchPartitionPartitionMaxBytes = maxBytes
        , -- New v17+ fields (KIP-853 / KIP-405). We don't track
          -- replica directory IDs or high watermarks in the simple
          -- client; the broker accepts the sentinels.
          FR.fetchPartitionReplicaDirectoryId = P.nullUuid
        , FR.fetchPartitionHighWatermark = -1
        }
      
      -- Create topic fetch request
      topicData = FR.FetchTopic
        { FR.fetchTopicTopic = P.mkKafkaString topic  -- Topic name for versions 0-12
        , FR.fetchTopicTopicId = P.nullUuid
        , FR.fetchTopicPartitions = P.mkKafkaArray (V.singleton partitionData)
        }
      
      -- Create the fetch request (using version 4, minimum supported version)
      apiVersion = 4
      request = FR.FetchRequest
        { FR.fetchRequestClusterId = P.KafkaString P.Null  -- v12+, null for v11
        , FR.fetchRequestReplicaId = -1  -- Consumer (not a replica)
        , FR.fetchRequestReplicaState = FR.ReplicaState
            { FR.replicaStateReplicaId = -1  -- v15+, will be ignored
            , FR.replicaStateReplicaEpoch = -1
            }
        , FR.fetchRequestMaxWaitMs = fromIntegral (clientTimeout $ clientConfig client)
        , FR.fetchRequestMinBytes = 1  -- Return as soon as we have at least 1 byte
        , FR.fetchRequestMaxBytes = maxBytes
        , FR.fetchRequestIsolationLevel = 0  -- Read uncommitted
        , FR.fetchRequestSessionId = 0  -- No session
        , FR.fetchRequestSessionEpoch = -1
        , FR.fetchRequestTopics = P.mkKafkaArray (V.singleton topicData)
        , FR.fetchRequestForgottenTopicsData = P.mkKafkaArray V.empty
        , FR.fetchRequestRackId = P.KafkaString P.Null
        }
      
      requestBody = WC.runEncodeVer @FR.FetchRequest apiVersion request
      clientIdStr = P.mkKafkaString (clientId $ clientConfig client)
  
  result <- sendRequestReceiveResponse
    (clientConnection client)
    1  -- Fetch API key
    (fromIntegral apiVersion)
    corrId
    clientIdStr
    requestBody
  
  case result of
    Left err -> return $ Left err
    Right (respCorrId, respBody) ->
      if respCorrId /= corrId
        then return $ Left $ "Correlation ID mismatch: expected " ++ show corrId ++ ", got " ++ show respCorrId
        else case WC.runDecodeVer @FResp.FetchResponse apiVersion respBody of
          Left err -> return $ Left $ "Failed to decode fetch response: " ++ err
          Right response -> do
            -- Extract the records from the response (this performs IO for decompression)
            fetchResultM <- extractFetchResult response topic partition
            case fetchResultM of
              Nothing -> return $ Left "No matching topic/partition in response"
              Just fetchResult -> return $ Right fetchResult

