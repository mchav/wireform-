{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Kafka.Client.AdminClient
Description : Manage the cluster — topics, groups, configs, ACLs.
Copyright   : (c) 2025
License     : BSD-3-Clause

The /control plane/ counterpart to the producer and consumer.
'AdminClient' is what you reach for when you need to:

  * create or delete a topic,
  * change a topic's partition count or replication factor,
  * inspect or update broker / topic configs,
  * list / describe / reset / delete consumer groups,
  * trim a partition by deleting records below an offset
    ('deleteRecords'),
  * trigger preferred or unclean leader election.

Like the producer and consumer, an 'AdminClient' holds a
long-lived connection pool — open it once at startup and reuse
it across calls.

= Quick start

@
import qualified Kafka.Client.AdminClient as Admin

main :: IO ()
main =
  Admin.'withAdminClient' [\"localhost:9092\"] Admin.'defaultAdminClientConfig' $ \\adm -> do
    Right results <- Admin.'createTopics' adm
      [ Admin.NewTopic
          { newTopicName              = \"events\"
          , newTopicNumPartitions     = 3
          , newTopicReplicationFactor = 1
          , newTopicConfigs           = []
          }
      ]
    print results
@

Every admin operation returns 'IO (Either String x)'. The
@x@ payload is typically a per-resource result so you can tell
which topic in a batch was rejected and why.
-}
module Kafka.Client.AdminClient
  ( -- * AdminClient Types
    AdminClient
  , AdminClientConfig(..)
    -- * AdminClient Lifecycle
  , withAdminClient
  , createAdminClient
  , closeAdminClient
    -- * Cluster info
  , adminClusterId
    -- * Topic Operations
  , NewTopic(..)
  , TopicDescription(..)
  , PartitionInfo(..)
  , createTopics
  , deleteTopics
  , listTopics
  , listTopicsExcludeInternal
  , describeTopics
    -- * Consumer Group Operations
  , ConsumerGroupListing(..)
  , ConsumerGroupDescription(..)
  , MemberDescription(..)
  , listConsumerGroups
  , describeConsumerGroups
  , deleteConsumerGroups
  , listConsumerGroupOffsets
  , alterConsumerGroupOffsets
    -- * Configuration Operations
  , ConfigResource(..)
  , ConfigResourceType(..)
  , ConfigEntry(..)
  , ConfigResourceResult(..)
  , describeConfigs
  , alterConfigs
    -- * Incremental config alterations
  , AlterConfigOp(..)
  , AlterableConfigEntry(..)
  , incrementalAlterConfigs
    -- * DeleteRecords
  , deleteRecords
  , DeleteRecordsResultEntry(..)
    -- * Leader election
  , ElectionType(..)
  , electLeaders
    -- * Configuration
  , defaultAdminClientConfig
  , defaultAdminApiTimeoutMs
    -- * Topic-create defaults
  , TopicCreateDefaults (..)
  , defaultTopicCreateDefaults
    -- * Null-key compaction policy
  , NullKeyCompactionPolicy (..)
  , defaultNullKeyCompactionPolicy
    -- * Metric names
    --
    -- | Canonical metric names emitted by 'Kafka.Telemetry.Metrics'
    -- for the admin-client operations. Useful when wiring custom
    -- exporters.
  , adminListTopicsLatencyMs
  , adminCreateTopicsLatencyMs
  , adminDescribeGroupsLatencyMs
  , adminAlterConfigsLatencyMs
  , adminDeleteRecordsLatencyMs
    -- * Internal helpers (exposed for testing)
  , decodeResourceTypeCode
  , unpackResourceResult
  , unpackConfigEntry
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (forM, forM_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.Connection (Connection)

import qualified Kafka.Client.Metadata as Meta
import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.VersionNegotiation as VN
import qualified Kafka.Protocol.Generated.CreateTopicsRequest as CTReq
import qualified Kafka.Protocol.Generated.CreateTopicsResponse as CTResp
import qualified Kafka.Protocol.Generated.DescribeConfigsRequest as DCReq
import qualified Kafka.Protocol.Generated.DescribeConfigsResponse as DCResp
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
import qualified Kafka.Protocol.Generated.AlterConfigsRequest as ACReq
import qualified Kafka.Protocol.Generated.AlterConfigsResponse as ACResp
import qualified Kafka.Protocol.Generated.IncrementalAlterConfigsRequest as IACReq
import qualified Kafka.Protocol.Generated.IncrementalAlterConfigsResponse as IACResp
import qualified Kafka.Protocol.Generated.DeleteRecordsRequest as DRReq
import qualified Kafka.Protocol.Generated.DeleteRecordsResponse as DRResp
import qualified Kafka.Protocol.Generated.ElectLeadersRequest as ELReq
import qualified Kafka.Protocol.Generated.ElectLeadersResponse as ELResp
import qualified Kafka.Protocol.Generated.OffsetFetchRequest as OFReq
import qualified Kafka.Protocol.Generated.OffsetFetchResponse as OFResp
import qualified Kafka.Protocol.Generated.OffsetCommitRequest as OCReq
import qualified Kafka.Protocol.Generated.OffsetCommitResponse as OCResp
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Wire.Codec as WC

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
  , adminCorrelationId :: !(IORef Int32)
    -- ^ Next correlation ID. Pre-Tier-1 lived in STM; the admin
    --   client never composes the increment with anything else
    --   transactionally, so 'IORef' + 'atomicModifyIORef\'' is
    --   sufficient and skips the per-RPC STM commit.
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
          -- Initialize correlation ID first so the ApiVersions
          -- handshake and the metadata refresh share the same
          -- correlation-id source.
          corrId <- newIORef 1
          let nextCid = atomicModifyIORef' corrId $ \cid -> (cid + 1, cid)

          -- Run the ApiVersions handshake against the bootstrap
          -- broker before any other RPC. We swallow failure: an
          -- older broker (< 0.10) doesn't recognise
          -- ApiVersions and will tear down the connection with
          -- a protocol error; in that case downstream calls
          -- fall back to their compiled-in defaults.
          _ <- VN.ensureVersionsNegotiated
                 conn firstBroker versionCache nextCid

          fetchResult <- Meta.refreshMetadata conn metadataCache 0
          case fetchResult of
            Left err -> return $ Left $ "Failed to fetch initial metadata: " ++ err
            Right _ ->
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

-- | Open an admin client, run an action with it, and close it on
-- exit. The recommended bracket — guarantees connections are torn
-- down even when the body throws, and raises an 'IOError' on
-- startup failure so callers can use 'Control.Exception.try' /
-- 'catch' to decide whether to retry.
--
-- @
-- 'withAdminClient' [\"localhost:9092\"] 'defaultAdminClientConfig' $ \\adm -> do
--   Right results <- 'createTopics' adm [..]
--   print results
-- @
withAdminClient
  :: [Text]
  -> AdminClientConfig
  -> (AdminClient -> IO a)
  -> IO a
withAdminClient brokers cfg = bracket open closeAdminClient
  where
    open = do
      r <- createAdminClient brokers cfg
      case r of
        Left err -> throwIO (userError ("wireform-kafka: createAdminClient failed: " <> err))
        Right c  -> pure c

-- | Close the admin client and clean up resources
closeAdminClient :: AdminClient -> IO ()
closeAdminClient AdminClient{..} = do
  Conn.closeAllConnections adminConnManager

-- | Get next correlation ID
getNextCorrelationId :: AdminClient -> IO Int32
getNextCorrelationId AdminClient{..} =
  atomicModifyIORef' adminCorrelationId $ \cid -> (cid + 1, cid)

----------------------------------------------------------------------
-- Connection + version-negotiation glue
----------------------------------------------------------------------

-- | Obtain a connection to @addr@ from the admin client's
-- connection pool /and/ make sure @ApiVersions@ has been
-- negotiated for it. Idempotent: if the cache already has an
-- entry for @addr@, the negotiation step is a no-op.
--
-- All admin RPCs that subsequently consult
-- 'pickAdminApiVersion' for this broker can rely on the cache
-- being populated with the broker's actual supported ranges
-- (rather than the empty fallback path).
getNegotiatedConn :: AdminClient -> Conn.BrokerAddress -> IO (Either String Connection)
getNegotiatedConn client@AdminClient{..} addr = do
  connResult <- Conn.getOrCreateConnection adminConnManager addr Conn.defaultConnectionConfig
  case connResult of
    Left err  -> pure (Left err)
    Right conn -> do
      -- Brokers older than 0.10 don't speak ApiVersions; we
      -- treat that as "no information, use the caller-supplied
      -- fallback version" rather than a hard failure.
      _ <- VN.ensureVersionsNegotiated
             conn addr adminVersionCache (getNextCorrelationId client)
      pure (Right conn)

-- | Pick the right API version for an outbound admin RPC.
--
-- Wraps 'VN.pickApiVersion' so a 'VersionMismatch' becomes a
-- string error the caller can return up the stack. Compared to
-- the boilerplate that used to live at every call site, this
-- has two important behaviour differences:
--
--   1. It runs the negotiation handshake on demand if the
--      cache is still empty (via 'getNegotiatedConn'), so the
--      'fallback' path only fires when the broker doesn't
--      speak @ApiVersions@ at all.
--   2. It returns @Left@ for an actual mismatch instead of
--      silently falling back to v0 — a v0 send to a broker
--      that only knows v9+ closes the connection with
--      @InvalidRequestException@.
pickAdminApiVersion
  :: AdminClient
  -> Conn.BrokerAddress
  -> Int16             -- ^ API key
  -> Int16             -- ^ client min version
  -> Int16             -- ^ client max version
  -> Int16             -- ^ fallback when broker doesn't speak ApiVersions
  -> IO (Either String Int16)
pickAdminApiVersion AdminClient{..} addr apiKey clientMin clientMax fallback = do
  r <- VN.pickApiVersion adminVersionCache addr apiKey clientMin clientMax fallback
  pure $ case r of
    Right v -> Right v
    Left mm -> Left $ formatMismatch mm

formatMismatch :: VN.VersionMismatch -> String
formatMismatch (VN.VersionMismatch k cmin cmax bmin bmax) =
  "Broker does not support a compatible version of API "
    <> show k <> ": client supports [" <> show cmin <> ".."
    <> show cmax <> "], broker supports [" <> show bmin
    <> ".." <> show bmax <> "]"

-- | Top-level wrapper used by every admin RPC call site:
-- get a (negotiated) connection to @addr@, pick a version
-- the broker accepts, allocate a correlation id, then run the
-- caller-supplied action with all three. Collapses the
-- 5-line connection / version / correlation-id boilerplate
-- the call sites used to repeat.
--
-- Both connection and version-selection failures collapse into
-- @Left@ before the caller's action runs; the action itself
-- only sees a successful @(conn, corrId, apiVersion)@ triple
-- and returns the usual @Either String result@.
withNegotiatedVersion
  :: AdminClient
  -> Conn.BrokerAddress
  -> Int16             -- ^ API key
  -> Int16             -- ^ client min version
  -> Int16             -- ^ client max version
  -> Int16             -- ^ fallback when broker doesn't speak ApiVersions
  -> (Connection -> Int32 -> Int16 -> IO (Either String a))
  -> IO (Either String a)
withNegotiatedVersion client addr apiKey clientMin clientMax fallback k = do
  connR <- getNegotiatedConn client addr
  case connR of
    Left e     -> pure (Left ("Failed to connect to broker: " <> e))
    Right conn -> do
      verR <- pickAdminApiVersion client addr apiKey clientMin clientMax fallback
      case verR of
        Left e          -> pure (Left e)
        Right apiVersion -> do
          corrId <- getNextCorrelationId client
          k conn corrId apiVersion

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
          apiKey = 19  -- CreateTopics
      withNegotiatedVersion client brokerAddr apiKey 0 7 0 $ \conn corrId apiVersion -> do
        let creatableTopics = V.fromList $ map buildCreatableTopic topics
            request = CTReq.CreateTopicsRequest
              { CTReq.createTopicsRequestTopics = P.mkKafkaArray creatableTopics
              , CTReq.createTopicsRequesttimeoutMs = fromIntegral (adminRequestTimeoutMs adminConfig)
              , CTReq.createTopicsRequestvalidateOnly = False
              }
            requestBody = WC.runEncodeVer @CTReq.CreateTopicsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "CreateTopics request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @CTResp.CreateTopicsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse CreateTopicsResponse: " ++ err
              Right response -> do
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
          apiKey = 20  -- DeleteTopics
      withNegotiatedVersion client brokerAddr apiKey 0 6 0 $ \conn corrId apiVersion -> do
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
            requestBody = WC.runEncodeVer @DTReq.DeleteTopicsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "DeleteTopics request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DTResp.DeleteTopicsResponse apiVersion responseBody of
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
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 3  -- Metadata
      -- Metadata is flexible from v9. With the response-header
      -- v1 trailer skipped correctly the flexible variants
      -- decode cleanly; we still cap at v12 because v13+ adds
      -- TopicId fields the high-level 'TopicDescription' API
      -- doesn't expose yet.
      -- Metadata: codegen handles up to v13. v9 went flexible
      -- (response-header v1 trailer); v12 added Uuid topic id;
      -- v13 made per-topic name nullable when topic id is set.
      -- Our request always supplies the name (TopicId = nullUuid),
      -- which the broker treats as a name-based lookup at every
      -- version up to and including 13.
      withNegotiatedVersion client brokerAddr apiKey 0 13 8 $ \conn corrId apiVersion -> do
        let request = MReq.MetadataRequest
              { MReq.metadataRequestTopics = P.mkKafkaArray V.empty
              , MReq.metadataRequestAllowAutoTopicCreation = False
              , MReq.metadataRequestIncludeClusterAuthorizedOperations = False
              , MReq.metadataRequestIncludeTopicAuthorizedOperations = False
              }
            requestBody = WC.runEncodeVer @MReq.MetadataRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "Metadata request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @MResp.MetadataResponse apiVersion responseBody of
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
          apiKey = 3  -- Metadata
      -- v9+ is flexible; cap at v12 (see 'listTopics').
      withNegotiatedVersion client brokerAddr apiKey 0 13 8 $ \conn corrId apiVersion -> do
        let topicReqs = V.fromList $ map (\name -> MReq.MetadataRequestTopic
              { MReq.metadataRequestTopicTopicId = P.nullUuid
              , MReq.metadataRequestTopicName = P.mkKafkaString name
              }) topicNames
            request = MReq.MetadataRequest
              { MReq.metadataRequestTopics = P.mkKafkaArray topicReqs
              , MReq.metadataRequestAllowAutoTopicCreation = False
              , MReq.metadataRequestIncludeClusterAuthorizedOperations = False
              , MReq.metadataRequestIncludeTopicAuthorizedOperations = False
              }
            requestBody = WC.runEncodeVer @MReq.MetadataRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "Metadata request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @MResp.MetadataResponse apiVersion responseBody of
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
          apiKey = 16  -- ListGroups
      -- ListGroups: codegen handles up to v5 (KIP-848 added the
      -- typesFilter field, which we already supply as empty).
      withNegotiatedVersion client brokerAddr apiKey 0 5 0 $ \conn corrId apiVersion -> do
        let request = LGReq.ListGroupsRequest
              { LGReq.listGroupsRequestStatesFilter = P.mkKafkaArray V.empty
              , LGReq.listGroupsRequestTypesFilter = P.mkKafkaArray V.empty
              }
            requestBody = WC.runEncodeVer @LGReq.ListGroupsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "ListGroups request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @LGResp.ListGroupsResponse apiVersion responseBody of
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
          apiKey = 15  -- DescribeGroups
      -- DescribeGroups: codegen handles up to v6 (KIP-848 added
      -- additional response fields the high-level
      -- 'ConsumerGroupDescription' surface ignores, which is
      -- fine — extra fields decode and we just don't expose them).
      withNegotiatedVersion client brokerAddr apiKey 0 6 0 $ \conn corrId apiVersion -> do
        let groupVec = V.fromList $ map P.mkKafkaString groupIds
            request = DGReq.DescribeGroupsRequest
              { DGReq.describeGroupsRequestGroups = P.mkKafkaArray groupVec
              , DGReq.describeGroupsRequestIncludeAuthorizedOperations = False
              }
            requestBody = WC.runEncodeVer @DGReq.DescribeGroupsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "DescribeGroups request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DGResp.DescribeGroupsResponse apiVersion responseBody of
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
          apiKey = 42  -- DeleteGroups
      withNegotiatedVersion client brokerAddr apiKey 0 2 0 $ \conn corrId apiVersion -> do
        let groupVec = V.fromList $ map P.mkKafkaString groupIds
            request = DelGReq.DeleteGroupsRequest
              { DelGReq.deleteGroupsRequestGroupsNames = P.mkKafkaArray groupVec
              }
            requestBody = WC.runEncodeVer @DelGReq.DeleteGroupsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "DeleteGroups request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DelGResp.DeleteGroupsResponse apiVersion responseBody of
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

-- | Describe configurations for one or more resources. Mirrors
-- @AdminClient.describeConfigs@: issues a single DescribeConfigs
-- RPC against any broker (the broker forwards to the controller
-- for broker-config requests), and unpacks each per-resource
-- result into a 'ConfigResourceResult'. Errors at the resource
-- level are surfaced via 'crrError'; transport-level failures
-- collapse into the outer 'Left'.
describeConfigs
  :: AdminClient
  -> [ConfigResource]
  -> IO (Either String [ConfigResourceResult])
describeConfigs client@AdminClient{..} resources = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing       -> return $ Left "No brokers available"
    Just []       -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 32  -- DescribeConfigs
      -- DescribeConfigs has been v1+ since Kafka 0.11; the encoder
      -- doesn't handle v0. We accept v1..v4 — v4 is the flexible
      -- variant and is exercised by the live-broker integration
      -- suite now that 'parseResponseFrame' correctly skips the
      -- v1 response-header tagged-fields trailer for flexible
      -- responses.
      withNegotiatedVersion client brokerAddr apiKey 1 4 1 $ \conn corrId apiVersion -> do
        let resourcesV = V.fromList (map buildResource resources)
            request = DCReq.DescribeConfigsRequest
              { DCReq.describeConfigsRequestResources = P.mkKafkaArray resourcesV
              , DCReq.describeConfigsRequestIncludeSynonyms = False
              , DCReq.describeConfigsRequestIncludeDocumentation = False
              }
            requestBody  = WC.runEncodeVer @DCReq.DescribeConfigsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse
                    conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "DescribeConfigs request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DCResp.DescribeConfigsResponse apiVersion responseBody of
              Left err -> return $ Left $
                "Failed to parse DescribeConfigsResponse: " ++ err
              Right response -> do
                let results = case P.unKafkaArray (DCResp.describeConfigsResponseResults response) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                return $ Right $ V.toList $ V.map processResourceResult results
  where
    encodeResourceType :: ConfigResourceType -> Int8
    encodeResourceType = \case
      ConfigResourceTopic        -> 2
      ConfigResourceBroker       -> 4
      ConfigResourceBrokerLogger -> 8

    buildResource :: ConfigResource -> DCReq.DescribeConfigsResource
    buildResource cr = DCReq.DescribeConfigsResource
      { DCReq.describeConfigsResourceResourceType = encodeResourceType (crType cr)
      , DCReq.describeConfigsResourceResourceName = P.mkKafkaString (crName cr)
        -- Empty 'configurationKeys' means "every key"; matches
        -- the JVM client's Map<ConfigResource,Collection<String>>
        -- where an empty inner collection requests all configs.
      , DCReq.describeConfigsResourceConfigurationKeys = P.mkKafkaArray V.empty
      }

    processResourceResult = unpackResourceResult

-- | Decode a wire 'ResourceType' code (per the DescribeConfigs
-- RPC) into the higher-level 'ConfigResourceType' enum. Unknown
-- codes fall through to 'ConfigResourceTopic' as a best-effort
-- default; the JVM client behaves the same way (it reads back
-- 'ConfigResource.Type.UNKNOWN', but we don't model that here).
decodeResourceTypeCode :: Int8 -> ConfigResourceType
decodeResourceTypeCode = \case
  2 -> ConfigResourceTopic
  4 -> ConfigResourceBroker
  8 -> ConfigResourceBrokerLogger
  _ -> ConfigResourceTopic

-- | Translate a single 'DescribeConfigsResult' (one per resource
-- in the response) into a 'ConfigResourceResult'. Surfaces any
-- per-resource error code via 'crrError'; preserves 'crrEntries'
-- in the order the broker returned them.
unpackResourceResult :: DCResp.DescribeConfigsResult -> ConfigResourceResult
unpackResourceResult r =
  let !rt        = decodeResourceTypeCode (DCResp.describeConfigsResultResourceType r)
      !rn        = extractText (DCResp.describeConfigsResultResourceName r)
      !errCode   = DCResp.describeConfigsResultErrorCode r
      !errMsgT   = extractText (DCResp.describeConfigsResultErrorMessage r)
      !configsV  = case P.unKafkaArray (DCResp.describeConfigsResultConfigs r) of
                     P.Null      -> V.empty
                     P.NotNull v -> v
      !entries   = V.toList (V.map unpackConfigEntry configsV)
   in ConfigResourceResult
        { crrResource = ConfigResource { crType = rt, crName = rn }
        , crrEntries  = entries
        , crrError    =
            if errCode == 0
              then Nothing
              else Just (if T.null errMsgT
                          then T.pack ("Error code " <> show errCode)
                          else errMsgT)
        }

-- | Translate a single 'DescribeConfigsResourceResult' (one per
-- config key under a resource) into a 'ConfigEntry'. Notable
-- mappings:
--
--   * a null 'KafkaString' value becomes 'Nothing' (caller can
--     distinguish "unset" from "empty");
--   * KIP-226 'ConfigSource': only @5@ (DEFAULT_CONFIG) sets
--     'ceIsDefault' to True; everything else (topic / dynamic /
--     static / per-broker) is treated as a non-default value.
unpackConfigEntry :: DCResp.DescribeConfigsResourceResult -> ConfigEntry
unpackConfigEntry e =
  let !nm  = extractText (DCResp.describeConfigsResourceResultName  e)
      !rawValT = extractText (DCResp.describeConfigsResourceResultValue e)
      !val = case DCResp.describeConfigsResourceResultValue e of
               P.KafkaString P.Null -> Nothing
               _                    -> Just rawValT
      !ro  = DCResp.describeConfigsResourceResultReadOnly  e
      !sen = DCResp.describeConfigsResourceResultIsSensitive e
      !srcCode = DCResp.describeConfigsResourceResultConfigSource e
      !isDef   = srcCode == 5
   in ConfigEntry
        { ceName      = nm
        , ceValue     = val
        , ceReadOnly  = ro
        , ceIsDefault = isDef
        , ceSensitive = sen
        }

-- * Cluster info

-- | Read the broker-supplied cluster id off the admin
-- client's metadata cache. Returns 'Nothing' until the first
-- successful refresh.
adminClusterId :: AdminClient -> IO (Maybe Text)
adminClusterId AdminClient{..} =
  atomically (Meta.getClusterId adminMetadata)

-- * List-topics filtering

-- | Like 'listTopics' but skips Kafka-internal topics (those with
-- the @isInternal@ flag set: @__consumer_offsets@,
-- @__transaction_state@, …). Mirrors the JVM client's
-- @ListTopicsOptions.listInternal(false)@.
listTopicsExcludeInternal :: AdminClient -> IO (Either String [Text])
listTopicsExcludeInternal client@AdminClient{..} = do
  -- Re-issues the same MetadataRequest 'listTopics' does but
  -- filters with the per-topic 'isInternal' flag.
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 3  -- Metadata
      -- v9+ is flexible; cap at v12 (see 'listTopics').
      withNegotiatedVersion client brokerAddr apiKey 0 13 8 $ \conn corrId apiVersion -> do
        let request = MReq.MetadataRequest
              { MReq.metadataRequestTopics = P.mkKafkaArray V.empty
              , MReq.metadataRequestAllowAutoTopicCreation = False
              , MReq.metadataRequestIncludeClusterAuthorizedOperations = False
              , MReq.metadataRequestIncludeTopicAuthorizedOperations = False
              }
            requestBody  = WC.runEncodeVer @MReq.MetadataRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "Metadata request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @MResp.MetadataResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse MetadataResponse: " ++ err
              Right response -> do
                let topicsVec = case P.unKafkaArray (MResp.metadataResponseTopics response) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                    keep t = not (MResp.metadataResponseTopicIsInternal t)
                pure $ Right $ V.toList $
                  V.map (extractText . MResp.metadataResponseTopicName) $
                  V.filter keep topicsVec

-- * alterConfigs

-- | Replace the configuration for one or more resources.
-- /Note/: this is the legacy "AlterConfigs" call which replaces
-- /every/ key for a resource — keys you don't include are reset
-- to their defaults. Prefer 'incrementalAlterConfigs' for new
-- code.
--
-- Returns one entry per input resource: @Right ()@ on success,
-- or @Left errorMessage@ for resources the broker rejected.
alterConfigs
  :: AdminClient
  -> [(ConfigResource, [(Text, Text)])]
  -> IO (Either String [(ConfigResource, Either String ())])
alterConfigs client@AdminClient{..} resources = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 33  -- AlterConfigs
      withNegotiatedVersion client brokerAddr apiKey 0 2 0 $ \conn corrId apiVersion -> do
        let resourcesV = V.fromList (map buildResource resources)
            request = ACReq.AlterConfigsRequest
              { ACReq.alterConfigsRequestResources = P.mkKafkaArray resourcesV
              , ACReq.alterConfigsRequestValidateOnly = False
              }
            requestBody = WC.runEncodeVer @ACReq.AlterConfigsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "AlterConfigs request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @ACResp.AlterConfigsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse AlterConfigsResponse: " ++ err
              Right response -> do
                let respVec = case P.unKafkaArray (ACResp.alterConfigsResponseResponses response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    out = V.toList $ V.map unpackACResp respVec
                return $ Right out
  where
    buildResource :: (ConfigResource, [(Text, Text)]) -> ACReq.AlterConfigsResource
    buildResource (cr, kvs) =
      let configsV = V.fromList $ map (\(k, v) -> ACReq.AlterableConfig
            { ACReq.alterableConfigName  = P.mkKafkaString k
            , ACReq.alterableConfigValue = P.mkKafkaString v
            }) kvs
      in ACReq.AlterConfigsResource
           { ACReq.alterConfigsResourceResourceType = encodeResourceType (crType cr)
           , ACReq.alterConfigsResourceResourceName = P.mkKafkaString (crName cr)
           , ACReq.alterConfigsResourceConfigs      = P.mkKafkaArray configsV
           }

    unpackACResp :: ACResp.AlterConfigsResourceResponse
                 -> (ConfigResource, Either String ())
    unpackACResp r =
      let !rt   = decodeResourceTypeCode (ACResp.alterConfigsResourceResponseResourceType r)
          !rn   = extractText (ACResp.alterConfigsResourceResponseResourceName r)
          !ec   = ACResp.alterConfigsResourceResponseErrorCode r
          !emsg = extractText (ACResp.alterConfigsResourceResponseErrorMessage r)
      in ( ConfigResource { crType = rt, crName = rn }
         , if ec == 0
             then Right ()
             else Left ("Error " ++ show ec ++ ": " ++ T.unpack emsg)
         )

-- * incrementalAlterConfigs

-- | Operation type for an individual configuration key.
data AlterConfigOp
  = AlterConfigOpSet      -- ^ Set the key to the supplied value.
  | AlterConfigOpDelete   -- ^ Delete the key.
  | AlterConfigOpAppend   -- ^ Append to a list-valued key.
  | AlterConfigOpSubtract -- ^ Subtract from a list-valued key.
  deriving stock (Eq, Show, Generic)

-- | Incremental config alteration entry.
data AlterableConfigEntry = AlterableConfigEntry
  { aceName  :: !Text
  , aceOp    :: !AlterConfigOp
  , aceValue :: !(Maybe Text)
    -- ^ 'Nothing' for 'AlterConfigOpDelete'; otherwise required.
  } deriving (Eq, Show, Generic)

-- | Incremental (non-replacing) config alterations.
-- Set / Delete / Append / Subtract per key.
--
-- Strongly preferred over 'alterConfigs' for new code.
incrementalAlterConfigs
  :: AdminClient
  -> [(ConfigResource, [AlterableConfigEntry])]
  -> IO (Either String [(ConfigResource, Either String ())])
incrementalAlterConfigs client@AdminClient{..} resources = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 44  -- IncrementalAlterConfigs
      withNegotiatedVersion client brokerAddr apiKey 0 1 0 $ \conn corrId apiVersion -> do
        let resourcesV = V.fromList (map buildResource resources)
            request = IACReq.IncrementalAlterConfigsRequest
              { IACReq.incrementalAlterConfigsRequestResources = P.mkKafkaArray resourcesV
              , IACReq.incrementalAlterConfigsRequestValidateOnly = False
              }
            requestBody = WC.runEncodeVer @IACReq.IncrementalAlterConfigsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "IncrementalAlterConfigs request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @IACResp.IncrementalAlterConfigsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse IncrementalAlterConfigsResponse: " ++ err
              Right response -> do
                let respVec = case P.unKafkaArray (IACResp.incrementalAlterConfigsResponseResponses response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    out = V.toList $ V.map unpackResp respVec
                return $ Right out
  where
    encodeOp :: AlterConfigOp -> Int8
    encodeOp = \case
      AlterConfigOpSet      -> 0
      AlterConfigOpDelete   -> 1
      AlterConfigOpAppend   -> 2
      AlterConfigOpSubtract -> 3

    buildResource :: (ConfigResource, [AlterableConfigEntry]) -> IACReq.AlterConfigsResource
    buildResource (cr, ents) =
      let configsV = V.fromList $ map buildEntry ents
      in IACReq.AlterConfigsResource
           { IACReq.alterConfigsResourceResourceType = encodeResourceType (crType cr)
           , IACReq.alterConfigsResourceResourceName = P.mkKafkaString (crName cr)
           , IACReq.alterConfigsResourceConfigs      = P.mkKafkaArray configsV
           }

    buildEntry :: AlterableConfigEntry -> IACReq.AlterableConfig
    buildEntry AlterableConfigEntry{..} =
      IACReq.AlterableConfig
        { IACReq.alterableConfigName            = P.mkKafkaString aceName
        , IACReq.alterableConfigConfigOperation = encodeOp aceOp
        , IACReq.alterableConfigValue           = case aceValue of
            Nothing -> P.KafkaString P.Null
            Just v  -> P.mkKafkaString v
        }

    unpackResp :: IACResp.AlterConfigsResourceResponse
               -> (ConfigResource, Either String ())
    unpackResp r =
      let !rt   = decodeResourceTypeCode (IACResp.alterConfigsResourceResponseResourceType r)
          !rn   = extractText (IACResp.alterConfigsResourceResponseResourceName r)
          !ec   = IACResp.alterConfigsResourceResponseErrorCode r
          !emsg = extractText (IACResp.alterConfigsResourceResponseErrorMessage r)
      in ( ConfigResource { crType = rt, crName = rn }
         , if ec == 0
             then Right ()
             else Left ("Error " ++ show ec ++ ": " ++ T.unpack emsg)
         )

-- | Encode a 'ConfigResourceType' to its wire code (used by both
-- 'alterConfigs' and 'incrementalAlterConfigs').
encodeResourceType :: ConfigResourceType -> Int8
encodeResourceType = \case
  ConfigResourceTopic        -> 2
  ConfigResourceBroker       -> 4
  ConfigResourceBrokerLogger -> 8

-- * deleteRecords (admin entry)

-- | One row of the result returned by 'deleteRecords': the new
-- low-watermark for a partition (records strictly below it are
-- gone).
data DeleteRecordsResultEntry = DeleteRecordsResultEntry
  { dreTopic        :: !Text
  , drePartition    :: !Int32
  , dreLowWatermark :: !Int64
  , dreErrorCode    :: !Int16
  } deriving (Eq, Show, Generic)

-- | Trim the partition log up to (but not including) the
-- supplied offset for each (topic, partition). Mirrors
-- @AdminClient.deleteRecords(Map\<TopicPartition, RecordsToDelete\>)@.
deleteRecords
  :: AdminClient
  -> [(Text, Int32, Int64)]   -- ^ (topic, partition, offset)
  -> IO (Either String [DeleteRecordsResultEntry])
deleteRecords _ [] = pure (Right [])
deleteRecords client@AdminClient{..} entries = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 21  -- DeleteRecords
      -- DeleteRecords: codegen handles up to v2 (v2 went flexible).
      -- Request shape unchanged — same (topic, partition, offset,
      -- timeout) tuples at every version.
      withNegotiatedVersion client brokerAddr apiKey 0 2 1 $ \conn corrId apiVersion -> do
        -- Group partitions under their topic so we send one
        -- DeleteRecordsTopic per topic.
        let byTopic = Map.fromListWith (++)
              [ (topic, [(part, off)]) | (topic, part, off) <- entries ]
            topicsV = V.fromList $
              map (\(topic, parts) ->
                     DRReq.DeleteRecordsTopic
                       { DRReq.deleteRecordsTopicName = P.mkKafkaString topic
                       , DRReq.deleteRecordsTopicPartitions = P.mkKafkaArray $ V.fromList $
                           map (\(p, o) ->
                                  DRReq.DeleteRecordsPartition
                                    { DRReq.deleteRecordsPartitionPartitionIndex = p
                                    , DRReq.deleteRecordsPartitionOffset         = o
                                    }) parts
                       })
                  (Map.toList byTopic)
            request = DRReq.DeleteRecordsRequest
              { DRReq.deleteRecordsRequestTopics    = P.mkKafkaArray topicsV
              , DRReq.deleteRecordsRequestTimeoutMs = fromIntegral (adminRequestTimeoutMs adminConfig)
              }
            requestBody  = WC.runEncodeVer @DRReq.DeleteRecordsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "DeleteRecords request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DRResp.DeleteRecordsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse DeleteRecordsResponse: " ++ err
              Right response -> do
                let topicsVec = case P.unKafkaArray (DRResp.deleteRecordsResponseTopics response) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                    out = concatMap unpackTopic (V.toList topicsVec)
                return $ Right out
  where
    unpackTopic :: DRResp.DeleteRecordsTopicResult -> [DeleteRecordsResultEntry]
    unpackTopic t =
      let topic = extractText (DRResp.deleteRecordsTopicResultName t)
          parts = case P.unKafkaArray (DRResp.deleteRecordsTopicResultPartitions t) of
            P.Null      -> V.empty
            P.NotNull v -> v
      in V.toList $ V.map (\p -> DeleteRecordsResultEntry
            { dreTopic        = topic
            , drePartition    = DRResp.deleteRecordsPartitionResultPartitionIndex p
            , dreLowWatermark = DRResp.deleteRecordsPartitionResultLowWatermark p
            , dreErrorCode    = DRResp.deleteRecordsPartitionResultErrorCode p
            }) parts

-- * ElectLeaders

-- | Election type. 'PreferredElection' falls back to the
-- first replica in the assignment ("preferred"); 'UncleanElection'
-- promotes any in-sync replica even if it would lose data.
data ElectionType
  = PreferredElection   -- ^ Wire code 0
  | UncleanElection     -- ^ Wire code 1
  deriving stock (Eq, Show, Generic)

-- | Ask the controller to (re-)elect leaders for the
-- supplied (topic, partition) list. An empty list elects every
-- partition that currently needs election.
--
-- Returns a list of (topic, partition, error-code) triples — one
-- per partition the controller responded about. @0@ means OK.
electLeaders
  :: AdminClient
  -> ElectionType
  -> [(Text, Int32)]
  -> IO (Either String [(Text, Int32, Int16)])
electLeaders client@AdminClient{..} etype tps = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 43  -- ElectLeaders
      withNegotiatedVersion client brokerAddr apiKey 0 2 2 $ \conn corrId apiVersion -> do
        let byTopic = Map.fromListWith (++)
              [ (topic, [part]) | (topic, part) <- tps ]
            topicsV = V.fromList $
              map (\(topic, parts) ->
                     ELReq.TopicPartitions
                       { ELReq.topicPartitionsTopic      = P.mkKafkaString topic
                       , ELReq.topicPartitionsPartitions = P.mkKafkaArray (V.fromList parts)
                       })
                  (Map.toList byTopic)
            request = ELReq.ElectLeadersRequest
              { ELReq.electLeadersRequestElectionType    = case etype of
                  PreferredElection -> 0
                  UncleanElection   -> 1
              , ELReq.electLeadersRequestTopicPartitions = P.mkKafkaArray topicsV
              , ELReq.electLeadersRequestTimeoutMs       = fromIntegral (adminRequestTimeoutMs adminConfig)
              }
            requestBody  = WC.runEncodeVer @ELReq.ElectLeadersRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "ElectLeaders request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @ELResp.ElectLeadersResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse ElectLeadersResponse: " ++ err
              Right response -> do
                let resV = case P.unKafkaArray (ELResp.electLeadersResponseReplicaElectionResults response) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                    out = concatMap unpackTopic (V.toList resV)
                return $ Right out
  where
    unpackTopic :: ELResp.ReplicaElectionResult -> [(Text, Int32, Int16)]
    unpackTopic t =
      let topic = extractText (ELResp.replicaElectionResultTopic t)
          parts = case P.unKafkaArray (ELResp.replicaElectionResultPartitionResult t) of
            P.Null      -> V.empty
            P.NotNull v -> v
      in V.toList $ V.map (\p ->
           ( topic
           , ELResp.partitionResultPartitionId p
           , ELResp.partitionResultErrorCode p
           )) parts

-- * Consumer-group offset management

-- | List every committed offset for a consumer group.
--
-- Returns a 'HashMap' keyed by @(topic, partition)@ containing
-- the broker's committed offset. Partitions with no committed
-- offset are absent.
listConsumerGroupOffsets
  :: AdminClient
  -> Text                                     -- ^ Group ID
  -> IO (Either String (HashMap (Text, Int32) Int64))
listConsumerGroupOffsets client@AdminClient{..} groupId = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 9  -- OffsetFetch
      -- OffsetFetch: codegen handles up to v10. v6 went
      -- flexible. v7 added 'requireStable' which we always
      -- pass as False (matches the JVM client default). v8
      -- introduced the per-group 'groups[]' batched lookup
      -- shape, but our request continues to use the legacy
      -- single-group shape (groups = [], topics = Null) which
      -- the broker still honours through v10. v9/v10 added
      -- response fields ('topicAuthorizedOperations',
      -- KIP-941 'errorCode' on the per-group result) we
      -- decode but don't expose at the high-level
      -- surface — extra fields just round-trip and are
      -- ignored.
      withNegotiatedVersion client brokerAddr apiKey 0 7 5 $ \conn corrId apiVersion -> do
        -- KIP-211 / KIP-465: a /null/ topics array (not an empty
        -- one) is the broker's "fetch every committed offset for
        -- this group" sentinel. 'mkKafkaArray V.empty' produces an
        -- empty-but-non-null array, which the broker interprets
        -- as "no topics, no offsets". Build the Null variant
        -- explicitly.
        let request = OFReq.OffsetFetchRequest
              { OFReq.offsetFetchRequestGroupId = P.mkKafkaString groupId
              , OFReq.offsetFetchRequestTopics  = P.KafkaArray P.Null
              , OFReq.offsetFetchRequestGroups  = P.mkKafkaArray V.empty
              , OFReq.offsetFetchRequestRequireStable = False
              }
            requestBody  = WC.runEncodeVer @OFReq.OffsetFetchRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "OffsetFetch request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @OFResp.OffsetFetchResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse OffsetFetchResponse: " ++ err
              Right response -> do
                let topicsVec = case P.unKafkaArray (OFResp.offsetFetchResponseTopics response) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                    go !acc tr =
                      let topic = extractText (OFResp.offsetFetchResponseTopicName tr)
                          partsVec = case P.unKafkaArray (OFResp.offsetFetchResponseTopicPartitions tr) of
                            P.Null -> V.empty
                            P.NotNull v -> v
                      in V.foldl'
                           (\m p ->
                              let pid = OFResp.offsetFetchResponsePartitionPartitionIndex p
                                  ec  = OFResp.offsetFetchResponsePartitionErrorCode p
                                  off = OFResp.offsetFetchResponsePartitionCommittedOffset p
                              in if ec == 0 && off >= 0
                                   then HashMap.insert (topic, pid) off m
                                   else m)
                           acc partsVec
                return $ Right $! V.foldl' go HashMap.empty topicsVec

-- | Write committed offsets for a group from /outside/
-- the consumer (e.g. a tool resetting a group). The group must
-- not have an active member when called; the broker rejects the
-- write otherwise.
--
-- Returns one entry per (topic, partition) the broker responded
-- about: @Right ()@ on success or @Left errorCode@ otherwise.
alterConsumerGroupOffsets
  :: AdminClient
  -> Text                                     -- ^ Group ID
  -> [(Text, Int32, Int64)]                   -- ^ (topic, partition, offset)
  -> IO (Either String [((Text, Int32), Either Int16 ())])
alterConsumerGroupOffsets _ _ [] = pure (Right [])
alterConsumerGroupOffsets client@AdminClient{..} groupId entries = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 8  -- OffsetCommit
      -- OffsetCommit: codegen handles up to v10. v6+ went
      -- flexible. v7 added 'groupInstanceId' (we send Null for
      -- the external-commit path — there's no group member to
      -- impersonate). v8 added per-topic 'topicId' (nullUuid =
      -- name-based lookup, broker compatible). v9+ moved to the
      -- KIP-848 member-epoch shape but the broker accepts
      -- generationId=-1 + empty memberId through v10 (the
      -- KIP-503 external-commit sentinel).
      withNegotiatedVersion client brokerAddr apiKey 0 9 5 $ \conn corrId apiVersion -> do
        let byTopic = Map.fromListWith (++)
              [ (topic, [(part, off)]) | (topic, part, off) <- entries ]
            topicsV = V.fromList $
              map (\(topic, parts) ->
                     OCReq.OffsetCommitRequestTopic
                       { OCReq.offsetCommitRequestTopicName       = P.mkKafkaString topic
                       , OCReq.offsetCommitRequestTopicPartitions = P.mkKafkaArray $ V.fromList $
                           map (\(p, o) -> OCReq.OffsetCommitRequestPartition
                                  { OCReq.offsetCommitRequestPartitionPartitionIndex = p
                                  , OCReq.offsetCommitRequestPartitionCommittedOffset = o
                                  , OCReq.offsetCommitRequestPartitionCommittedLeaderEpoch = -1
                                  , OCReq.offsetCommitRequestPartitionCommittedMetadata = P.KafkaString P.Null
                                  }) parts
                       })
                  (Map.toList byTopic)
            -- KIP-503 / external offset commit: a memberId of
            -- the empty string is the broker's "no live group
            -- member" sentinel. The field is marked non-nullable
            -- in the spec, so we MUST send an empty string
            -- rather than a null. groupInstanceId stays null
            -- (we're not impersonating a static member) and the
            -- generation id is -1.
            request = OCReq.OffsetCommitRequest
              { OCReq.offsetCommitRequestGroupId = P.mkKafkaString groupId
              , OCReq.offsetCommitRequestGenerationIdOrMemberEpoch = -1
              , OCReq.offsetCommitRequestMemberId = P.mkKafkaString ""
              , OCReq.offsetCommitRequestGroupInstanceId = P.KafkaString P.Null
              , OCReq.offsetCommitRequestRetentionTimeMs = -1
              , OCReq.offsetCommitRequestTopics = P.mkKafkaArray topicsV
              }
            requestBody  = WC.runEncodeVer @OCReq.OffsetCommitRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "OffsetCommit request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @OCResp.OffsetCommitResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse OffsetCommitResponse: " ++ err
              Right response -> do
                let topicsVec = case P.unKafkaArray (OCResp.offsetCommitResponseTopics response) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                    out = concatMap unpackTopic (V.toList topicsVec)
                return $ Right out
  where
    unpackTopic :: OCResp.OffsetCommitResponseTopic
                -> [((Text, Int32), Either Int16 ())]
    unpackTopic t =
      let topic = extractText (OCResp.offsetCommitResponseTopicName t)
          parts = case P.unKafkaArray (OCResp.offsetCommitResponseTopicPartitions t) of
            P.Null      -> V.empty
            P.NotNull v -> v
      in V.toList $ V.map (\p ->
           let pid = OCResp.offsetCommitResponsePartitionPartitionIndex p
               ec  = OCResp.offsetCommitResponsePartitionErrorCode p
           in ( (topic, pid)
              , if ec == 0 then Right () else Left ec
              )) parts

-- * Helper Functions

-- | Extract Text from a KafkaString
extractText :: P.KafkaString -> Text
extractText (P.KafkaString P.Null) = ""
extractText (P.KafkaString (P.NotNull t)) = t

----------------------------------------------------------------------
-- Additional ergonomics
--
-- Previously lived in @Kafka.Client.AdminExtras@.
----------------------------------------------------------------------

-- | Mirrors @AdminClient.DEFAULT_API_TIMEOUT_MS@ in the JVM client.
defaultAdminApiTimeoutMs :: Int
defaultAdminApiTimeoutMs = 60_000

-- | Defaults applied when creating a topic without explicit knobs.
data TopicCreateDefaults = TopicCreateDefaults
  { tcdReplicationFactor :: !Int
  , tcdNumPartitions     :: !Int32
  , tcdConfigOverrides   :: !(Map Text Text)
    -- ^ Topic-level config overrides applied to every newly
    --   created topic when the caller doesn't override them.
  }
  deriving stock (Eq, Show, Generic)

defaultTopicCreateDefaults :: TopicCreateDefaults
defaultTopicCreateDefaults = TopicCreateDefaults
  { tcdReplicationFactor = 1
  , tcdNumPartitions     = 1
  , tcdConfigOverrides   = Map.empty
  }

-- | What to do when a producer sends a 'Nothing' key to a
-- compacted topic. The default Kafka behaviour is to reject
-- with @INVALID_RECORD@; newer brokers can treat missing keys as
-- tombstones / pass-through.
data NullKeyCompactionPolicy
  = NkcReject
  | NkcTombstone
  | NkcPassThrough
  deriving stock (Eq, Show, Generic)

defaultNullKeyCompactionPolicy :: NullKeyCompactionPolicy
defaultNullKeyCompactionPolicy = NkcReject

-- Canonical telemetry metric names for the admin-client operations.
adminListTopicsLatencyMs
  , adminCreateTopicsLatencyMs
  , adminDescribeGroupsLatencyMs
  , adminAlterConfigsLatencyMs
  , adminDeleteRecordsLatencyMs :: Text
adminListTopicsLatencyMs     = "kafka.admin.list-topics.latency.ms"
adminCreateTopicsLatencyMs   = "kafka.admin.create-topics.latency.ms"
adminDescribeGroupsLatencyMs = "kafka.admin.describe-groups.latency.ms"
adminAlterConfigsLatencyMs   = "kafka.admin.alter-configs.latency.ms"
adminDeleteRecordsLatencyMs  = "kafka.admin.delete-records.latency.ms"

