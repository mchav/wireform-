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
  , getClusterId
  , getTopicIsInternal
    -- * Metadata Refresh
  , refreshMetadata
  , refreshTopicMetadata
    -- * KIP-466 client-side leader cache update
  , updatePartitionLeader
    -- * Metadata Types
  , ClusterMetadata(..)
  , TopicMetadata(..)
  , PartitionMetadata(..)
  , BrokerMetadata(..)
  ) where

import Control.Concurrent.STM
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Int
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
  , topicMetaPartitions :: !(HashMap Int32 PartitionMetadata)
    -- ^ Map from partition ID to partition metadata. 'HashMap'
    -- (not 'Data.Map.Strict.Map') because the producer/consumer
    -- hot path looks up by 'Int32' partition id; O(1) average
    -- vs. tree-based O(log n).
  , topicMetaErrorCode :: !Int16
  , topicMetaIsInternal :: !Bool
    -- ^ KIP-444: whether this topic is a Kafka-internal topic
    -- (e.g. @__consumer_offsets@, @__transaction_state@). Allows
    -- callers to filter internals out of @listTopics@.
  } deriving (Eq, Show, Generic)

-- | Complete cluster metadata
data ClusterMetadata = ClusterMetadata
  { clusterBrokers :: !(HashMap Int32 BrokerMetadata)
    -- ^ Map from node ID to broker metadata. 'HashMap' for the
    -- same reason as 'topicMetaPartitions': hot 'getPartitionLeader'
    -- path does one broker lookup per produce / fetch.
  , clusterTopics :: !(HashMap Text TopicMetadata)
    -- ^ Map from topic name to topic metadata. 'HashMap Text'
    -- avoids the lexicographic 'Text' compare a 'Map Text'
    -- would do at every level of the tree on each lookup.
  , clusterControllerId :: !Int32
    -- ^ Node ID of the cluster controller
  , clusterClusterId :: !(Maybe Text)
    -- ^ KIP-78: the broker's cluster id (a UUID-shaped string),
    -- as returned by @MetadataResponse@ from version 2 onward.
    -- 'Nothing' on older brokers or when the broker hasn't
    -- populated the field. Useful for clients that talk to
    -- multiple Kafka clusters and want to pin requests / metrics
    -- per-cluster.
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
      case HashMap.lookup topic clusterTopics of
        Nothing -> return Nothing
        Just TopicMetadata{..} ->
          case HashMap.lookup partitionId topicMetaPartitions of
            Nothing -> return Nothing
            Just PartitionMetadata{..} ->
              return $ HashMap.lookup partitionMetaLeader clusterBrokers

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
      case HashMap.lookup topic clusterTopics of
        Nothing -> return Nothing
        Just TopicMetadata{..} ->
          return $ Just $ HashMap.elems topicMetaPartitions

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
      case HashMap.lookup topic clusterTopics of
        Nothing -> return Nothing
        Just TopicMetadata{..} ->
          return $ Just $ fromIntegral $ HashMap.size topicMetaPartitions

-- | KIP-466: update the cached leader for a (topic, partition).
-- Called when a Produce / Fetch response surfaces the
-- @CurrentLeader@ tag (an out-of-band leader change). Avoids
-- having to re-issue a full @MetadataRequest@ just to learn the
-- new leader; we patch it into the cache and let the next request
-- pick it up.
--
-- A no-op if the cache hasn't been populated yet, or if the
-- (topic, partition) isn't known. The broker-id need not be a
-- broker we already know about — the caller is expected to pair
-- this with an updated broker registration if necessary.
updatePartitionLeader
  :: MetadataCache
  -> Text       -- ^ topic name
  -> Int32      -- ^ partition id
  -> Int32      -- ^ new leader broker id
  -> STM ()
updatePartitionLeader (MetadataCache metaVar) topic partitionId newLeaderId = do
  metaM <- readTVar metaVar
  case metaM of
    Nothing -> pure ()
    Just m@ClusterMetadata{..} ->
      case HashMap.lookup topic clusterTopics of
        Nothing -> pure ()
        Just t@TopicMetadata{..} ->
          case HashMap.lookup partitionId topicMetaPartitions of
            Nothing -> pure ()
            Just p ->
              let !p'  = p { partitionMetaLeader = newLeaderId }
                  !t'  = t { topicMetaPartitions =
                              HashMap.insert partitionId p' topicMetaPartitions }
                  !m'  = m { clusterTopics =
                              HashMap.insert topic t' clusterTopics }
               in writeTVar metaVar (Just m')

-- | Get all brokers in the cluster
getAllBrokers :: MetadataCache -> STM (Maybe [BrokerMetadata])
getAllBrokers (MetadataCache metaVar) = do
  metaM <- readTVar metaVar
  case metaM of
    Nothing -> return Nothing
    Just ClusterMetadata{..} ->
      return $ Just $ HashMap.elems clusterBrokers

-- | KIP-78: read the broker-supplied cluster id, if known.
-- Returns 'Nothing' before the first metadata refresh, on
-- pre-MetadataResponse-v2 brokers, and when the broker has not
-- populated the field. Otherwise returns the (UUID-shaped)
-- cluster id string.
getClusterId :: MetadataCache -> STM (Maybe Text)
getClusterId (MetadataCache metaVar) = do
  metaM <- readTVar metaVar
  pure (metaM >>= clusterClusterId)

-- | KIP-444: query the cached @isInternal@ flag for a topic.
-- Returns 'Nothing' if the topic isn't in the cache (or the cache
-- hasn't been populated yet).
getTopicIsInternal :: MetadataCache -> Text -> STM (Maybe Bool)
getTopicIsInternal (MetadataCache metaVar) topic = do
  metaM <- readTVar metaVar
  pure $ do
    ClusterMetadata{..} <- metaM
    tm <- HashMap.lookup topic clusterTopics
    pure (topicMetaIsInternal tm)

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
  -- Create metadata request. For v4+ Metadata the convention is
  -- /null/ topics array means "fetch every topic"; an empty
  -- non-null array means "fetch nothing". v0-v3 also accepts an
  -- empty array as "all topics", but we always go through v8
  -- here so the null sentinel is required.
  let topics = case topicsM of
        Nothing -> P.KafkaArray P.Null
        Just ts -> P.mkKafkaArray $ V.fromList $ map (\t -> MR.MetadataRequestTopic
          { MR.metadataRequestTopicTopicId = P.nullUuid
          , MR.metadataRequestTopicName = P.mkKafkaString t
          }) ts
      
      -- v8 is the highest non-flexible Metadata version. Using
      -- it here (instead of v0) buys us the ClusterId field
      -- (KIP-78, v2+) and the IsInternal flag (KIP-444, v1+)
      -- on the populated 'ClusterMetadata' without having to
      -- thread the v9+ flexible-versions request header through
      -- this code path. Brokers from Kafka 0.11 onward all
      -- support v8.
      apiVersion = 8
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
  let !brokers = parseBrokers response
      !topics = parseTopics response
      !controllerId = MResp.metadataResponseControllerId response
      -- KIP-78: ClusterId is a 'KafkaString' that's null on
      -- pre-v2 metadata responses; keep the 'Nothing' shape so
      -- callers can tell "broker didn't tell us" apart from
      -- "we never asked".
      !cId = case P.unKafkaString (MResp.metadataResponseClusterId response) of
              P.Null      -> Nothing
              P.NotNull t | T.null t  -> Nothing
                          | otherwise -> Just t
  in ClusterMetadata
      { clusterBrokers      = brokers
      , clusterTopics       = topics
      , clusterControllerId = controllerId
      , clusterClusterId    = cId
      }

-- | Parse broker information from metadata response
parseBrokers :: MResp.MetadataResponse -> HashMap Int32 BrokerMetadata
parseBrokers response =
  case P.unKafkaArray (MResp.metadataResponseBrokers response) of
    P.Null -> HashMap.empty
    P.NotNull vec -> HashMap.fromList $ V.toList $ V.map parseBroker vec
  where
    parseBroker :: MResp.MetadataResponseBroker -> (Int32, BrokerMetadata)
    parseBroker broker =
      let nodeId = MResp.metadataResponseBrokerNodeId broker
          host = T.unpack $ extractText $ MResp.metadataResponseBrokerHost broker
          port = MResp.metadataResponseBrokerPort broker
          address = BrokerAddress host (fromIntegral port)
      in (nodeId, BrokerMetadata nodeId address)

-- | Parse topic information from metadata response
parseTopics :: MResp.MetadataResponse -> HashMap Text TopicMetadata
parseTopics response =
  case P.unKafkaArray (MResp.metadataResponseTopics response) of
    P.Null -> HashMap.empty
    P.NotNull vec -> HashMap.fromList $ V.toList $ V.map parseTopic vec
  where
    parseTopic :: MResp.MetadataResponseTopic -> (Text, TopicMetadata)
    parseTopic topic =
      let name = extractText $ MResp.metadataResponseTopicName topic
          errorCode = MResp.metadataResponseTopicErrorCode topic
          partitions = parsePartitions topic
          isInternal = MResp.metadataResponseTopicIsInternal topic
      in (name, TopicMetadata name partitions errorCode isInternal)

-- | Parse partition information from a topic
parsePartitions :: MResp.MetadataResponseTopic -> HashMap Int32 PartitionMetadata
parsePartitions topic =
  case P.unKafkaArray (MResp.metadataResponseTopicPartitions topic) of
    P.Null -> HashMap.empty
    P.NotNull vec -> HashMap.fromList $ V.toList $ V.map parsePartition vec
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

