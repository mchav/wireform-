{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.AdminClient  
Description : High-level Kafka admin client API (KIP-117)
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module provides a high-level admin client API for Kafka administrative operations.

Features:

* Topic management (create, delete, list, describe)
* Consumer group management (list, describe, delete)
* Configuration management (describe, alter)
* Cluster metadata
* Version negotiation with brokers

= Usage Example

@
adminClient <- createAdminClient brokers defaultAdminClientConfig
result <- createTopics adminClient [newTopic]
case result of
  Left err -> putStrLn $ "Failed: " ++ err
  Right results -> print results
closeAdminClient adminClient
@

-}
module Kafka.Client.AdminClient
  ( -- * AdminClient Types
    AdminClient
  , AdminClientConfig(..)
    -- * AdminClient Lifecycle
  , createAdminClient
  , closeAdminClient
    -- * Topic Operations
  , NewTopic(..)
  , TopicDescription(..)
  , PartitionInfo(..)
  , createTopics
  , deleteTopics
  , listTopics
  , describeTopics
    -- * Consumer Group Operations
  , ConsumerGroupListing(..)
  , ConsumerGroupDescription(..)
  , MemberDescription(..)
  , listConsumerGroups
  , describeConsumerGroups
  , deleteConsumerGroups
    -- * Configuration Operations
  , ConfigResource(..)
  , ConfigResourceType(..)
  , ConfigEntry(..)
  , ConfigResourceResult(..)
  , describeConfigs
    -- * Configuration
  , defaultAdminClientConfig
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.Connection (Connection)

import qualified Kafka.Client.Metadata as Meta
import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.Generated.CreateTopicsRequest as CTReq
import qualified Kafka.Protocol.Generated.CreateTopicsResponse as CTResp
import qualified Kafka.Protocol.Generated.DeleteTopicsRequest as DTReq
import qualified Kafka.Protocol.Generated.DeleteTopicsResponse as DTResp
import qualified Kafka.Protocol.Generated.MetadataRequest as MReq
import qualified Kafka.Protocol.Generated.MetadataResponse as MResp
import qualified Kafka.Protocol.Generated.DescribeGroupsRequest as DGReq
import qualified Kafka.Protocol.Generated.DescribeGroupsResponse as DGResp
import qualified Kafka.Protocol.Generated.ListGroupsRequest as LGReq
import qualified Kafka.Protocol.Generated.ListGroupsResponse as LGResp
import qualified Kafka.Protocol.Generated.DeleteGroupsRequest as DelGReq
import qualified Kafka.Protocol.Generated.DeleteGroupsResponse as DelGResp
import qualified Kafka.Protocol.Generated.DescribeConfigsRequest as DCReq
import qualified Kafka.Protocol.Generated.DescribeConfigsResponse as DCResp
import qualified Kafka.Protocol.Primitives as P

-- | AdminClient configuration
data AdminClientConfig = AdminClientConfig
  { adminClientId :: !Text
    -- ^ Client identifier (default: "kafka-native-admin")
  , adminRequestTimeoutMs :: !Int
    -- ^ Request timeout in milliseconds (default: 30000)
  } deriving (Eq, Show, Generic)

-- | Default admin client configuration
defaultAdminClientConfig :: AdminClientConfig
defaultAdminClientConfig = AdminClientConfig
  { adminClientId = "kafka-native-admin"
  , adminRequestTimeoutMs = 30000
  }

-- | AdminClient handle
data AdminClient = AdminClient
  { adminConnManager :: !Conn.ConnectionManager
    -- ^ Connection manager for broker connections
  , adminMetadata :: !Meta.MetadataCache
    -- ^ Metadata cache
  , adminVersionCache :: !AV.ApiVersionCache
    -- ^ API version cache
  , adminCorrelationId :: !(TVar Int32)
    -- ^ Next correlation ID
  , adminConfig :: !AdminClientConfig
    -- ^ Configuration
  }

-- | Create a new admin client
createAdminClient
  :: [Text]                -- ^ Bootstrap broker addresses ("host:port")
  -> AdminClientConfig
  -> IO (Either String AdminClient)
createAdminClient brokerAddrs config = do
  -- Parse broker addresses
  let parsedBrokers = map parseBrokerAddress brokerAddrs
  
  -- Validate all brokers parsed successfully
  case sequence parsedBrokers of
    Left err -> return $ Left $ "Failed to parse broker addresses: " ++ err
    Right brokers -> do
      -- Create connection manager
      connManager <- Conn.createConnectionManager
      
      -- Create metadata cache
      metadataCache <- Meta.createMetadataCache
      
      -- Create version cache
      versionCache <- AV.createVersionCache
      
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
              -- Initialize correlation ID
              corrId <- newTVarIO 1
              
              return $ Right AdminClient
                { adminConnManager = connManager
                , adminMetadata = metadataCache
                , adminVersionCache = versionCache
                , adminCorrelationId = corrId
                , adminConfig = config
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

-- | Close the admin client and clean up resources
closeAdminClient :: AdminClient -> IO ()
closeAdminClient AdminClient{..} = do
  Conn.closeAllConnections adminConnManager

-- | Get next correlation ID
getNextCorrelationId :: AdminClient -> IO Int32
getNextCorrelationId AdminClient{..} = atomically $ do
  cid <- readTVar adminCorrelationId
  writeTVar adminCorrelationId (cid + 1)
  return cid

-- * Topic Operations

-- | Specification for creating a new topic
data NewTopic = NewTopic
  { ntName :: !Text
    -- ^ Topic name
  , ntNumPartitions :: !Int32
    -- ^ Number of partitions (must be > 0, or -1 for broker default)
  , ntReplicationFactor :: !Int16
    -- ^ Replication factor (must be > 0, or -1 for broker default)
  , ntConfigs :: ![(Text, Text)]
    -- ^ Topic configuration overrides
  } deriving (Eq, Show, Generic)

-- | Information about a topic partition
data PartitionInfo = PartitionInfo
  { piPartitionId :: !Int32
    -- ^ Partition ID
  , piLeader :: !Int32
    -- ^ Leader broker ID
  , piReplicas :: ![Int32]
    -- ^ Replica broker IDs
  , piIsr :: ![Int32]
    -- ^ In-sync replica broker IDs
  } deriving (Eq, Show, Generic)

-- | Description of a topic
data TopicDescription = TopicDescription
  { tdName :: !Text
    -- ^ Topic name
  , tdInternal :: !Bool
    -- ^ Whether this is an internal topic
  , tdPartitions :: ![PartitionInfo]
    -- ^ Partition information
  } deriving (Eq, Show, Generic)

-- | Create one or more topics
-- Returns a list of (topic name, result) pairs
createTopics
  :: AdminClient
  -> [NewTopic]
  -> IO (Either String [(Text, Either String ())])
createTopics client@AdminClient{..} topics = do
  -- Get any broker connection (Kafka will redirect to controller if needed)
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
      connResult <- Conn.getOrCreateConnection adminConnManager brokerAddr Conn.defaultConnectionConfig
      
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          -- Get correlation ID
          corrId <- getNextCorrelationId client
          
          -- Build CreateTopicsRequest
          let apiKey = 19  -- CreateTopics API key
              clientMaxVersion = 7  -- Max version we support
          
          -- Query broker's supported version
          brokerVersionM <- atomically $ AV.queryApiVersion adminVersionCache brokerAddr apiKey
          let apiVersion = case brokerVersionM of
                Nothing -> 0  -- Fall back to v0 if unknown
                Just range -> case AV.selectVersion clientMaxVersion range of
                  Nothing -> 0  -- Fall back if incompatible
                  Just v -> v
          
          -- Build topic creations
          let creatableTopics = V.fromList $ map buildCreatableTopic topics
              request = CTReq.CreateTopicsRequest
                { CTReq.createTopicsRequestTopics = P.mkKafkaArray creatableTopics
                , CTReq.createTopicsRequesttimeoutMs = fromIntegral (adminRequestTimeoutMs adminConfig)
                , CTReq.createTopicsRequestvalidateOnly = False
                }
              
              requestBody = runPutS $ CTReq.encodeCreateTopicsRequest apiVersion request
              clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
          
          -- Send request and receive response
          result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
          
          case result of
            Left err -> return $ Left $ "CreateTopics request failed: " ++ err
            Right (_, responseBody) -> do
              -- Parse response
              case runGetS (CTResp.decodeCreateTopicsResponse apiVersion) responseBody of
                Left err -> return $ Left $ "Failed to parse CreateTopicsResponse: " ++ err
                Right response -> do
                  -- Extract results
                  let topicResults = case P.unKafkaArray (CTResp.createTopicsResponseTopics response) of
                        P.Null -> V.empty
                        P.NotNull vec -> vec
                      
                      results = V.toList $ V.map processTopicResult topicResults
                  
                  return $ Right results
  where
    buildCreatableTopic :: NewTopic -> CTReq.CreatableTopic
    buildCreatableTopic NewTopic{..} =
      let configs = V.fromList $ map (\(k, v) -> CTReq.CreatableTopicConfig
            { CTReq.creatableTopicConfigName = P.mkKafkaString k
            , CTReq.creatableTopicConfigValue = P.mkKafkaString v
            }) ntConfigs
      in CTReq.CreatableTopic
        { CTReq.creatableTopicName = P.mkKafkaString ntName
        , CTReq.creatableTopicNumPartitions = ntNumPartitions
        , CTReq.creatableTopicReplicationFactor = ntReplicationFactor
        , CTReq.creatableTopicAssignments = P.mkKafkaArray V.empty
        , CTReq.creatableTopicConfigs = P.mkKafkaArray configs
        }
    
    processTopicResult :: CTResp.CreatableTopicResult -> (Text, Either String ())
    processTopicResult result =
      let topicName = extractText $ CTResp.creatableTopicResultName result
          errorCode = CTResp.creatableTopicResultErrorCode result
          errorMsg = extractText $ CTResp.creatableTopicResultErrorMessage result
      in if errorCode == 0
           then (topicName, Right ())
           else (topicName, Left $ "Error " ++ show errorCode ++ ": " ++ T.unpack errorMsg)

-- | Delete one or more topics
-- Returns a list of (topic name, result) pairs
deleteTopics
  :: AdminClient
  -> [Text]
  -> IO (Either String [(Text, Either String ())])
deleteTopics client@AdminClient{..} topicNames = do
  -- Get any broker connection (Kafka will redirect to controller if needed)
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
      connResult <- Conn.getOrCreateConnection adminConnManager brokerAddr Conn.defaultConnectionConfig
      
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          corrId <- getNextCorrelationId client
          
          let apiKey = 20  -- DeleteTopics API key
              clientMaxVersion = 6
          
          brokerVersionM <- atomically $ AV.queryApiVersion adminVersionCache brokerAddr apiKey
          let apiVersion = case brokerVersionM of
                Nothing -> 0
                Just range -> case AV.selectVersion clientMaxVersion range of
                  Nothing -> 0
                  Just v -> v
          
          -- Build topic names (for versions 0-5) and topic states (for version 6+)
          let topicNamesVec = V.fromList $ map P.mkKafkaString topicNames
              topicStatesVec = V.fromList $ map (\name -> DTReq.DeleteTopicState
                { DTReq.deleteTopicStateName = P.mkKafkaString name
                , DTReq.deleteTopicStateTopicId = P.nullUuid
                }) topicNames
              request = DTReq.DeleteTopicsRequest
                { DTReq.deleteTopicsRequestTopics = P.mkKafkaArray topicStatesVec
                , DTReq.deleteTopicsRequestTopicNames = P.mkKafkaArray topicNamesVec
                , DTReq.deleteTopicsRequestTimeoutMs = fromIntegral (adminRequestTimeoutMs adminConfig)
                }
              
              requestBody = runPutS $ DTReq.encodeDeleteTopicsRequest apiVersion request
              clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
          
          result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
          
          case result of
            Left err -> return $ Left $ "DeleteTopics request failed: " ++ err
            Right (_, responseBody) -> do
              case runGetS (DTResp.decodeDeleteTopicsResponse apiVersion) responseBody of
                Left err -> return $ Left $ "Failed to parse DeleteTopicsResponse: " ++ err
                Right response -> do
                  let topicResults = case P.unKafkaArray (DTResp.deleteTopicsResponseResponses response) of
                        P.Null -> V.empty
                        P.NotNull vec -> vec
                      
                      results = V.toList $ V.map (\r ->
                        let name = extractText $ DTResp.deletableTopicResultName r
                            code = DTResp.deletableTopicResultErrorCode r
                            msg = extractText $ DTResp.deletableTopicResultErrorMessage r
                        in if code == 0
                             then (name, Right ())
                             else (name, Left $ "Error " ++ show code ++ ": " ++ T.unpack msg)
                        ) topicResults
                  
                  return $ Right results

-- | List all topics in the cluster
listTopics
  :: AdminClient
  -> IO (Either String [Text])
listTopics client@AdminClient{..} = do
  -- Use Metadata API to list topics
  -- Get any broker connection
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
      connResult <- Conn.getOrCreateConnection adminConnManager brokerAddr Conn.defaultConnectionConfig
      
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          corrId <- getNextCorrelationId client
          
          let apiKey = 3  -- Metadata API key
              apiVersion = 12  -- Use recent version
              
              -- Request metadata for all topics (empty list = all topics)
              request = MReq.MetadataRequest
                { MReq.metadataRequestTopics = P.mkKafkaArray V.empty
                , MReq.metadataRequestAllowAutoTopicCreation = False
                , MReq.metadataRequestIncludeClusterAuthorizedOperations = False
                , MReq.metadataRequestIncludeTopicAuthorizedOperations = False
                }
              
              requestBody = runPutS $ MReq.encodeMetadataRequest apiVersion request
              clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
          
          result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
          
          case result of
            Left err -> return $ Left $ "Metadata request failed: " ++ err
            Right (_, responseBody) -> do
              case runGetS (MResp.decodeMetadataResponse apiVersion) responseBody of
                Left err -> return $ Left $ "Failed to parse MetadataResponse: " ++ err
                Right response -> do
                  let topics = case P.unKafkaArray (MResp.metadataResponseTopics response) of
                        P.Null -> V.empty
                        P.NotNull vec -> vec
                      
                      topicNames = V.toList $ V.map (extractText . MResp.metadataResponseTopicName) topics
                  
                  return $ Right topicNames

-- | Describe one or more topics
describeTopics
  :: AdminClient
  -> [Text]
  -> IO (Either String [TopicDescription])
describeTopics client@AdminClient{..} topicNames = do
  -- Use Metadata API to describe topics
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
      connResult <- Conn.getOrCreateConnection adminConnManager brokerAddr Conn.defaultConnectionConfig
      
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          corrId <- getNextCorrelationId client
          
          let apiKey = 3  -- Metadata API key
              apiVersion = 12
              
              topicReqs = V.fromList $ map (\name -> MReq.MetadataRequestTopic
                { MReq.metadataRequestTopicTopicId = P.nullUuid
                , MReq.metadataRequestTopicName = P.mkKafkaString name
                }) topicNames
              
              request = MReq.MetadataRequest
                { MReq.metadataRequestTopics = P.mkKafkaArray topicReqs
                , MReq.metadataRequestAllowAutoTopicCreation = False
                , MReq.metadataRequestIncludeClusterAuthorizedOperations = False
                , MReq.metadataRequestIncludeTopicAuthorizedOperations = False
                }
              
              requestBody = runPutS $ MReq.encodeMetadataRequest apiVersion request
              clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
          
          result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
          
          case result of
            Left err -> return $ Left $ "Metadata request failed: " ++ err
            Right (_, responseBody) -> do
              case runGetS (MResp.decodeMetadataResponse apiVersion) responseBody of
                Left err -> return $ Left $ "Failed to parse MetadataResponse: " ++ err
                Right response -> do
                  let topics = case P.unKafkaArray (MResp.metadataResponseTopics response) of
                        P.Null -> V.empty
                        P.NotNull vec -> vec
                      
                      descriptions = V.toList $ V.mapMaybe buildTopicDescription topics
                  
                  return $ Right descriptions
  where
    buildTopicDescription :: MResp.MetadataResponseTopic -> Maybe TopicDescription
    buildTopicDescription topic =
      let name = extractText $ MResp.metadataResponseTopicName topic
          isInternal = MResp.metadataResponseTopicIsInternal topic
          partitions = case P.unKafkaArray (MResp.metadataResponseTopicPartitions topic) of
            P.Null -> V.empty
            P.NotNull vec -> vec
          
          partInfos = V.toList $ V.map buildPartitionInfo partitions
      in Just $ TopicDescription
        { tdName = name
        , tdInternal = isInternal
        , tdPartitions = partInfos
        }
    
    buildPartitionInfo :: MResp.MetadataResponsePartition -> PartitionInfo
    buildPartitionInfo part =
      PartitionInfo
        { piPartitionId = MResp.metadataResponsePartitionPartitionIndex part
        , piLeader = MResp.metadataResponsePartitionLeaderId part
        , piReplicas = case P.unKafkaArray (MResp.metadataResponsePartitionReplicaNodes part) of
            P.Null -> []
            P.NotNull vec -> V.toList vec
        , piIsr = case P.unKafkaArray (MResp.metadataResponsePartitionIsrNodes part) of
            P.Null -> []
            P.NotNull vec -> V.toList vec
        }

-- * Consumer Group Operations

-- | Basic information about a consumer group
data ConsumerGroupListing = ConsumerGroupListing
  { cglGroupId :: !Text
    -- ^ Group ID
  , cglIsSimpleGroup :: !Bool
    -- ^ Whether this is a simple consumer group
  } deriving (Eq, Show, Generic)

-- | Description of a consumer group member
data MemberDescription = MemberDescription
  { mdMemberId :: !Text
    -- ^ Member ID
  , mdClientId :: !Text
    -- ^ Client ID
  , mdHost :: !Text
    -- ^ Member host
  } deriving (Eq, Show, Generic)

-- | Description of a consumer group
data ConsumerGroupDescription = ConsumerGroupDescription
  { cgdGroupId :: !Text
    -- ^ Group ID
  , cgdState :: !Text
    -- ^ Group state (e.g., "Stable", "PreparingRebalance")
  , cgdMembers :: ![MemberDescription]
    -- ^ Group members
  } deriving (Eq, Show, Generic)

-- | List all consumer groups in the cluster
listConsumerGroups
  :: AdminClient
  -> IO (Either String [ConsumerGroupListing])
listConsumerGroups client@AdminClient{..} = do
  -- Get any broker connection
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
      connResult <- Conn.getOrCreateConnection adminConnManager brokerAddr Conn.defaultConnectionConfig
      
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          corrId <- getNextCorrelationId client
          
          let apiKey = 16  -- ListGroups API key
              apiVersion = 4
              
              request = LGReq.ListGroupsRequest
                { LGReq.listGroupsRequestStatesFilter = P.mkKafkaArray V.empty
                , LGReq.listGroupsRequestTypesFilter = P.mkKafkaArray V.empty
                }
              
              requestBody = runPutS $ LGReq.encodeListGroupsRequest apiVersion request
              clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
          
          result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
          
          case result of
            Left err -> return $ Left $ "ListGroups request failed: " ++ err
            Right (_, responseBody) -> do
              case runGetS (LGResp.decodeListGroupsResponse apiVersion) responseBody of
                Left err -> return $ Left $ "Failed to parse ListGroupsResponse: " ++ err
                Right response -> do
                  let groups = case P.unKafkaArray (LGResp.listGroupsResponseGroups response) of
                        P.Null -> V.empty
                        P.NotNull vec -> vec
                      
                      listings = V.toList $ V.map (\g ->
                        ConsumerGroupListing
                          { cglGroupId = extractText $ LGResp.listedGroupGroupId g
                          , cglIsSimpleGroup = LGResp.listedGroupGroupType g == P.mkKafkaString "consumer"
                          }) groups
                  
                  return $ Right listings

-- | Describe one or more consumer groups
describeConsumerGroups
  :: AdminClient
  -> [Text]
  -> IO (Either String [ConsumerGroupDescription])
describeConsumerGroups client@AdminClient{..} groupIds = do
  -- Get any broker connection
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
      connResult <- Conn.getOrCreateConnection adminConnManager brokerAddr Conn.defaultConnectionConfig
      
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          corrId <- getNextCorrelationId client
          
          let apiKey = 15  -- DescribeGroups API key
              apiVersion = 5
              
              groupVec = V.fromList $ map P.mkKafkaString groupIds
              request = DGReq.DescribeGroupsRequest
                { DGReq.describeGroupsRequestGroups = P.mkKafkaArray groupVec
                , DGReq.describeGroupsRequestIncludeAuthorizedOperations = False
                }
              
              requestBody = runPutS $ DGReq.encodeDescribeGroupsRequest apiVersion request
              clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
          
          result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
          
          case result of
            Left err -> return $ Left $ "DescribeGroups request failed: " ++ err
            Right (_, responseBody) -> do
              case runGetS (DGResp.decodeDescribeGroupsResponse apiVersion) responseBody of
                Left err -> return $ Left $ "Failed to parse DescribeGroupsResponse: " ++ err
                Right response -> do
                  let groups = case P.unKafkaArray (DGResp.describeGroupsResponseGroups response) of
                        P.Null -> V.empty
                        P.NotNull vec -> vec
                      
                      descriptions = V.toList $ V.map buildGroupDescription groups
                  
                  return $ Right descriptions
  where
    buildGroupDescription :: DGResp.DescribedGroup -> ConsumerGroupDescription
    buildGroupDescription group =
      let members = case P.unKafkaArray (DGResp.describedGroupMembers group) of
            P.Null -> V.empty
            P.NotNull vec -> vec
          
          memberDescs = V.toList $ V.map (\m ->
            MemberDescription
              { mdMemberId = extractText $ DGResp.describedGroupMemberMemberId m
              , mdClientId = extractText $ DGResp.describedGroupMemberClientId m
              , mdHost = extractText $ DGResp.describedGroupMemberClientHost m
              }) members
      in ConsumerGroupDescription
        { cgdGroupId = extractText $ DGResp.describedGroupGroupId group
        , cgdState = extractText $ DGResp.describedGroupGroupState group
        , cgdMembers = memberDescs
        }

-- | Delete one or more consumer groups
-- Returns a list of (group ID, result) pairs
deleteConsumerGroups
  :: AdminClient
  -> [Text]
  -> IO (Either String [(Text, Either String ())])
deleteConsumerGroups client@AdminClient{..} groupIds = do
  -- Get any broker connection
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
      connResult <- Conn.getOrCreateConnection adminConnManager brokerAddr Conn.defaultConnectionConfig
      
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          corrId <- getNextCorrelationId client
          
          let apiKey = 42  -- DeleteGroups API key
              apiVersion = 2
              
              groupVec = V.fromList $ map P.mkKafkaString groupIds
              request = DelGReq.DeleteGroupsRequest
                { DelGReq.deleteGroupsRequestGroupsNames = P.mkKafkaArray groupVec
                }
              
              requestBody = runPutS $ DelGReq.encodeDeleteGroupsRequest apiVersion request
              clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
          
          result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
          
          case result of
            Left err -> return $ Left $ "DeleteGroups request failed: " ++ err
            Right (_, responseBody) -> do
              case runGetS (DelGResp.decodeDeleteGroupsResponse apiVersion) responseBody of
                Left err -> return $ Left $ "Failed to parse DeleteGroupsResponse: " ++ err
                Right response -> do
                  let groupResults = case P.unKafkaArray (DelGResp.deleteGroupsResponseResults response) of
                        P.Null -> V.empty
                        P.NotNull vec -> vec
                      
                      results = V.toList $ V.map (\r ->
                        let gid = extractText $ DelGResp.deletableGroupResultGroupId r
                            code = DelGResp.deletableGroupResultErrorCode r
                        in if code == 0
                             then (gid, Right ())
                             else (gid, Left $ "Error code: " ++ show code)
                        ) groupResults
                  
                  return $ Right results

-- * Configuration Operations

-- | Type of configuration resource
data ConfigResourceType
  = ConfigResourceTopic       -- ^ Topic configuration
  | ConfigResourceBroker      -- ^ Broker configuration  
  | ConfigResourceBrokerLogger -- ^ Broker logger configuration
  deriving (Eq, Show, Generic)

-- | A configuration resource to describe
data ConfigResource = ConfigResource
  { crType :: !ConfigResourceType
    -- ^ Resource type
  , crName :: !Text
    -- ^ Resource name (topic name, broker ID, etc.)
  } deriving (Eq, Show, Generic)

-- | A configuration entry (key-value pair)
data ConfigEntry = ConfigEntry
  { ceName :: !Text
    -- ^ Configuration key
  , ceValue :: !(Maybe Text)
    -- ^ Configuration value (Nothing if not set)
  , ceReadOnly :: !Bool
    -- ^ Whether this config is read-only
  , ceIsDefault :: !Bool
    -- ^ Whether this is the default value
  , ceSensitive :: !Bool
    -- ^ Whether this config contains sensitive data
  } deriving (Eq, Show, Generic)

-- | Result of describing a configuration resource
data ConfigResourceResult = ConfigResourceResult
  { crrResource :: !ConfigResource
    -- ^ The resource that was described
  , crrEntries :: ![ConfigEntry]
    -- ^ Configuration entries
  , crrError :: !(Maybe Text)
    -- ^ Error message, if any
  } deriving (Eq, Show, Generic)

-- | Describe configurations for one or more resources
describeConfigs
  :: AdminClient
  -> [ConfigResource]
  -> IO (Either String [ConfigResourceResult])
describeConfigs client resources = do
  -- TODO: Implement describeConfigs  
  return $ Left "describeConfigs not yet implemented"

-- * Helper Functions

-- | Extract Text from a KafkaString
extractText :: P.KafkaString -> Text
extractText (P.KafkaString P.Null) = ""
extractText (P.KafkaString (P.NotNull t)) = t

