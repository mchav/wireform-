{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Metadata
Description : Cluster metadata management and caching
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module handles caching and refreshing of Kafka cluster metadata.

Metadata includes:
- List of brokers in the cluster
- Topics and their partitions
- Partition leaders and replicas
- Which broker is the leader for each partition

Metadata is cached and refreshed:
- On demand when needed
- Periodically in the background
- When metadata errors occur

This is critical for:
- Routing produce requests to the correct partition leader
- Discovering available partitions for consumption
- Handling broker failures and leadership changes
-}
module Kafka.Client.Metadata
  ( -- * Metadata Cache
    MetadataCache
  , createMetadataCache
    -- * Metadata Queries
  , getPartitionLeader
  , getTopicPartitions
  , getPartitionCount
  , getAllBrokers
    -- * Metadata Refresh
  , refreshMetadata
  , refreshTopicMetadata
    -- * Metadata Types
  , ClusterMetadata(..)
  , TopicMetadata(..)
  , PartitionMetadata(..)
  , BrokerMetadata(..)
  ) where

import Control.Concurrent.STM
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.Connection (Connection)

import Kafka.Client.Internal.Request
import Kafka.Network.Connection (BrokerAddress(..))
import qualified Kafka.Protocol.Generated.MetadataRequest as MR
import qualified Kafka.Protocol.Generated.MetadataResponse as MResp
import qualified Kafka.Protocol.Primitives as P

-- | Information about a broker in the cluster
data BrokerMetadata = BrokerMetadata
  { brokerMetaNodeId :: !Int32
  , brokerMetaAddress :: !BrokerAddress
  } deriving (Eq, Show, Ord, Generic)

-- | Information about a partition
data PartitionMetadata = PartitionMetadata
  { partitionMetaId :: !Int32
    -- ^ Partition index
  , partitionMetaLeader :: !Int32
    -- ^ Node ID of the partition leader
  , partitionMetaReplicas :: ![Int32]
    -- ^ Node IDs of all replicas
  , partitionMetaIsrs :: ![Int32]
    -- ^ Node IDs of in-sync replicas
  } deriving (Eq, Show, Generic)

-- | Information about a topic
data TopicMetadata = TopicMetadata
  { topicMetaName :: !Text
  , topicMetaPartitions :: !(Map Int32 PartitionMetadata)
    -- ^ Map from partition ID to partition metadata
  , topicMetaErrorCode :: !Int16
  } deriving (Eq, Show, Generic)

-- | Complete cluster metadata
data ClusterMetadata = ClusterMetadata
  { clusterBrokers :: !(Map Int32 BrokerMetadata)
    -- ^ Map from node ID to broker metadata
  , clusterTopics :: !(Map Text TopicMetadata)
    -- ^ Map from topic name to topic metadata
  , clusterControllerId :: !Int32
    -- ^ Node ID of the cluster controller
  } deriving (Eq, Show, Generic)

-- | Metadata cache with STM for concurrent access
newtype MetadataCache = MetadataCache
  { metadataVar :: TVar (Maybe ClusterMetadata)
  }

-- | Create a new empty metadata cache
createMetadataCache :: IO MetadataCache
createMetadataCache = MetadataCache <$> newTVarIO Nothing

-- | Get the leader broker for a specific topic partition
getPartitionLeader
  :: MetadataCache
  -> Text      -- ^ Topic name
  -> Int32     -- ^ Partition ID
  -> STM (Maybe BrokerMetadata)
getPartitionLeader (MetadataCache metaVar) topic partitionId = do
  metaM <- readTVar metaVar
  case metaM of
    Nothing -> return Nothing
    Just ClusterMetadata{..} -> do
      case Map.lookup topic clusterTopics of
        Nothing -> return Nothing
        Just TopicMetadata{..} ->
          case Map.lookup partitionId topicMetaPartitions of
            Nothing -> return Nothing
            Just PartitionMetadata{..} ->
              return $ Map.lookup partitionMetaLeader clusterBrokers

-- | Get all partitions for a topic
getTopicPartitions
  :: MetadataCache
  -> Text      -- ^ Topic name
  -> STM (Maybe [PartitionMetadata])
getTopicPartitions (MetadataCache metaVar) topic = do
  metaM <- readTVar metaVar
  case metaM of
    Nothing -> return Nothing
    Just ClusterMetadata{..} ->
      case Map.lookup topic clusterTopics of
        Nothing -> return Nothing
        Just TopicMetadata{..} ->
          return $ Just $ Map.elems topicMetaPartitions

-- | Get partition count for a topic (KIP-480: needed for sticky partitioner)
getPartitionCount
  :: MetadataCache
  -> Text      -- ^ Topic name
  -> STM (Maybe Int32)
getPartitionCount (MetadataCache metaVar) topic = do
  metaM <- readTVar metaVar
  case metaM of
    Nothing -> return Nothing
    Just ClusterMetadata{..} ->
      case Map.lookup topic clusterTopics of
        Nothing -> return Nothing
        Just TopicMetadata{..} ->
          return $ Just $ fromIntegral $ Map.size topicMetaPartitions

-- | Get all brokers in the cluster
getAllBrokers :: MetadataCache -> STM (Maybe [BrokerMetadata])
getAllBrokers (MetadataCache metaVar) = do
  metaM <- readTVar metaVar
  case metaM of
    Nothing -> return Nothing
    Just ClusterMetadata{..} ->
      return $ Just $ Map.elems clusterBrokers

-- | Refresh metadata for all topics
refreshMetadata
  :: Connection
  -> MetadataCache
  -> Int32         -- ^ Correlation ID
  -> IO (Either String ())
refreshMetadata conn cache correlationId =
  refreshTopicMetadata conn cache Nothing correlationId

-- | Refresh metadata for specific topics (or all if Nothing)
refreshTopicMetadata
  :: Connection
  -> MetadataCache
  -> Maybe [Text]  -- ^ Specific topics or Nothing for all
  -> Int32         -- ^ Correlation ID
  -> IO (Either String ())
refreshTopicMetadata conn (MetadataCache metaVar) topicsM correlationId = do
  -- Create metadata request
  let topics = case topicsM of
        Nothing -> P.mkKafkaArray V.empty  -- Empty array means all topics
        Just ts -> P.mkKafkaArray $ V.fromList $ map (\t -> MR.MetadataRequestTopic
          { MR.metadataRequestTopicTopicId = P.nullUuid
          , MR.metadataRequestTopicName = P.mkKafkaString t
          }) ts
      
      apiVersion = 0  -- Use version 0 for maximum compatibility
      request = MR.MetadataRequest
        { MR.metadataRequestTopics = topics
        , MR.metadataRequestAllowAutoTopicCreation = False
        , MR.metadataRequestIncludeClusterAuthorizedOperations = False
        , MR.metadataRequestIncludeTopicAuthorizedOperations = False
        }
      
      requestBody = runPutS $ MR.encodeMetadataRequest apiVersion request
      clientId = P.mkKafkaString "kafka-native"
  
  -- Send request
  result <- sendRequestReceiveResponse
    conn
    3  -- Metadata API key
    (fromIntegral apiVersion)
    correlationId
    clientId
    requestBody
  
  case result of
    Left err -> return $ Left err
    Right (respCorrId, respBody) ->
      if respCorrId /= correlationId
        then return $ Left $ "Correlation ID mismatch"
        else case runGetS (MResp.decodeMetadataResponse apiVersion) respBody of
          Left err -> return $ Left $ "Failed to decode metadata response: " ++ err
          Right response -> do
            -- Parse metadata from response
            let metadata = parseMetadataResponse response
            -- Store in cache
            atomically $ writeTVar metaVar (Just metadata)
            return $ Right ()

-- | Parse metadata response into our internal format
parseMetadataResponse :: MResp.MetadataResponse -> ClusterMetadata
parseMetadataResponse response =
  let brokers = parseBrokers response
      topics = parseTopics response
      controllerId = MResp.metadataResponseControllerId response
  in ClusterMetadata
      { clusterBrokers = brokers
      , clusterTopics = topics
      , clusterControllerId = controllerId
      }

-- | Parse broker information from metadata response
parseBrokers :: MResp.MetadataResponse -> Map Int32 BrokerMetadata
parseBrokers response =
  case P.unKafkaArray (MResp.metadataResponseBrokers response) of
    P.Null -> Map.empty
    P.NotNull vec -> Map.fromList $ V.toList $ V.map parseBroker vec
  where
    parseBroker :: MResp.MetadataResponseBroker -> (Int32, BrokerMetadata)
    parseBroker broker =
      let nodeId = MResp.metadataResponseBrokerNodeId broker
          host = T.unpack $ extractText $ MResp.metadataResponseBrokerHost broker
          port = MResp.metadataResponseBrokerPort broker
          address = BrokerAddress host (fromIntegral port)
      in (nodeId, BrokerMetadata nodeId address)

-- | Parse topic information from metadata response
parseTopics :: MResp.MetadataResponse -> Map Text TopicMetadata
parseTopics response =
  case P.unKafkaArray (MResp.metadataResponseTopics response) of
    P.Null -> Map.empty
    P.NotNull vec -> Map.fromList $ V.toList $ V.map parseTopic vec
  where
    parseTopic :: MResp.MetadataResponseTopic -> (Text, TopicMetadata)
    parseTopic topic =
      let name = extractText $ MResp.metadataResponseTopicName topic
          errorCode = MResp.metadataResponseTopicErrorCode topic
          partitions = parsePartitions topic
      in (name, TopicMetadata name partitions errorCode)

-- | Parse partition information from a topic
parsePartitions :: MResp.MetadataResponseTopic -> Map Int32 PartitionMetadata
parsePartitions topic =
  case P.unKafkaArray (MResp.metadataResponseTopicPartitions topic) of
    P.Null -> Map.empty
    P.NotNull vec -> Map.fromList $ V.toList $ V.map parsePartition vec
  where
    parsePartition :: MResp.MetadataResponsePartition -> (Int32, PartitionMetadata)
    parsePartition partition =
      let partId = MResp.metadataResponsePartitionPartitionIndex partition
          leader = MResp.metadataResponsePartitionLeaderId partition
          replicas = case P.unKafkaArray (MResp.metadataResponsePartitionReplicaNodes partition) of
            P.Null -> []
            P.NotNull vec -> V.toList vec
          isr = case P.unKafkaArray (MResp.metadataResponsePartitionIsrNodes partition) of
            P.Null -> []
            P.NotNull vec -> V.toList vec
          errorCode = MResp.metadataResponsePartitionErrorCode partition
      in (partId, PartitionMetadata partId leader replicas isr)

-- | Helper to extract Text from KafkaString
extractText :: P.KafkaString -> Text
extractText ks = case P.unKafkaString ks of
  P.Null -> ""
  P.NotNull t -> t

