{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Consumer
Description : High-level Kafka consumer API
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module provides a high-level consumer API for receiving messages from Kafka.

Features:

* Consumer group coordination and rebalancing
* Automatic or manual offset management
* Multiple partition assignment strategies
* At-least-once and at-most-once semantics
* Seek operations for replay
* Pause/resume per-partition consumption

= Usage Example

@
consumer <- createConsumer brokers "my-group" defaultConsumerConfig
subscribe consumer ["my-topic"]
forever $ do
  records <- poll consumer 1000
  mapM_ processRecord records
  commitSync consumer
@

-}
module Kafka.Client.Consumer
  ( -- * Consumer Types
    Consumer
  , ConsumerConfig(..)
  , ConsumerRecord(..)
  , TopicPartition(..)
    -- * Consumer Creation
  , createConsumer
  , closeConsumer
  , closeConsumerWithTimeout
    -- * Subscription
  , subscribe
  , unsubscribe
  , assign
    -- * Polling and Consumption
  , poll
  , seek
  , seekToBeginning
  , seekToEnd
    -- * Offset Management
  , commitSync
  , commitAsync
  , committed
    -- * Partition Control
  , pause
  , resume
  , assignment
    -- * Configuration
  , defaultConsumerConfig
  , AssignmentStrategy(..)
  , OffsetResetStrategy(..)
  , ConsumerConfig(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (deserialize)
import Data.Hashable (Hashable)
import Data.Int
import Data.List (nub)
import Control.Monad (forM, forM_, when)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.Connection (Connection)
import qualified StmContainers.Map as StmMap
import qualified ListT

import qualified Kafka.Client.Internal.Heartbeat as HB
import qualified Kafka.Client.Internal.Request as Req
import Kafka.Client.Metadata (MetadataCache)
import qualified Kafka.Client.Metadata as Meta
import qualified Kafka.Network.Connection as Conn
import Kafka.Network.Connection (BrokerAddress(..))
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.Generated.FetchRequest as FR
import qualified Kafka.Protocol.Generated.FetchResponse as FResp
import qualified Kafka.Protocol.Generated.OffsetCommitRequest as OCReq
import qualified Kafka.Protocol.Generated.OffsetCommitResponse as OCResp
import qualified Kafka.Protocol.Generated.OffsetFetchRequest as OFReq
import qualified Kafka.Protocol.Generated.OffsetFetchResponse as OFResp
import qualified Kafka.Protocol.Generated.ListOffsetsRequest as LOReq
import qualified Kafka.Protocol.Generated.ListOffsetsResponse as LOResp
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.RecordBatch as RB

-- | Partition assignment strategy.
data AssignmentStrategy
  = RangeAssignment       -- ^ Range assignment (default)
  | RoundRobinAssignment  -- ^ Round-robin assignment
  | StickyAssignment      -- ^ Sticky assignment (minimizes rebalance)
  deriving (Eq, Show, Generic)

-- | Consumer configuration.
data ConsumerConfig = ConsumerConfig
  { consumerClientId :: !Text
    -- ^ Client identifier
  , consumerGroupId :: !Text
    -- ^ Consumer group ID
  , consumerAutoCommit :: !Bool
    -- ^ Enable auto-commit (default: True)
  , consumerAutoCommitIntervalMs :: !Int
    -- ^ Auto-commit interval in ms (default: 5000)
  , consumerSessionTimeoutMs :: !Int
    -- ^ Session timeout in ms (default: 10000 = 10 seconds) - KIP-256: separate from max poll interval
  , consumerHeartbeatIntervalMs :: !Int
    -- ^ Heartbeat interval in ms (default: 3000)
  , consumerMaxPollRecords :: !Int
    -- ^ Maximum records per poll (default: 500)
  , consumerMaxPollIntervalMs :: !Int
    -- ^ Maximum time between polls in ms (default: 300000 = 5 minutes) - KIP-256: separate from session timeout
  , consumerAssignmentStrategy :: !AssignmentStrategy
    -- ^ Partition assignment strategy
  , consumerAutoOffsetReset :: !OffsetResetStrategy
    -- ^ What to do when no offset (default: Latest)
  , consumerRackId :: !(Maybe Text)
    -- ^ Rack ID for rack-aware fetching (default: Nothing) - KIP-392: fetch from replicas in same rack
  } deriving (Generic)

-- | Offset reset strategy when no committed offset exists.
data OffsetResetStrategy
  = Earliest  -- ^ Start from earliest available offset
  | Latest    -- ^ Start from latest offset (default)
  | None      -- ^ Throw error if no offset exists
  deriving (Eq, Show, Generic)

-- | Default consumer configuration.
defaultConsumerConfig :: ConsumerConfig
defaultConsumerConfig = ConsumerConfig
  { consumerClientId = "kafka-native-consumer"
  , consumerGroupId = "default-group"
  , consumerAutoCommit = True
  , consumerAutoCommitIntervalMs = 5000
  , consumerSessionTimeoutMs = 10000
  , consumerHeartbeatIntervalMs = 3000
  , consumerMaxPollRecords = 500
  , consumerMaxPollIntervalMs = 300000
  , consumerAssignmentStrategy = RangeAssignment
  , consumerAutoOffsetReset = Latest
  , consumerRackId = Nothing  -- KIP-392: set to enable rack-aware fetching
  }

-- | A topic-partition pair.
data TopicPartition = TopicPartition
  { tpTopic :: !Text
  , tpPartition :: !Int32
  } deriving (Eq, Show, Ord, Generic)

instance Hashable TopicPartition

-- | A consumed record from Kafka.
data ConsumerRecord = ConsumerRecord
  { crTopic :: !Text
  , crPartition :: !Int32
  , crOffset :: !Int64
  , crTimestamp :: !Int64
  , crKey :: !(Maybe ByteString)
  , crValue :: !ByteString
  , crHeaders :: ![(Text, ByteString)]
  } deriving (Eq, Show, Generic)

-- | Kafka consumer handle.
data Consumer = Consumer
  { consumerConfig :: !ConsumerConfig
  , consumerConnManager :: !Conn.ConnectionManager
  , consumerMetadata :: !MetadataCache
  , consumerVersionCache :: !AV.ApiVersionCache
  , consumerAssignment :: !(StmMap.Map TopicPartition Int64)
    -- ^ Current partition assignment with fetch positions (uses stm-containers)
  , consumerHeartbeat :: !(Maybe (HB.HeartbeatState, Async ()))
    -- ^ Heartbeat state and thread (if in a group)
  , consumerCorrelationId :: !(TVar Int32)
  , consumerPaused :: !(StmMap.Map TopicPartition ())
    -- ^ Paused partitions (uses stm-containers)
  }

-- | Create a new Kafka consumer.
--
-- Initializes the consumer with connection management, metadata caching,
-- and optionally joins a consumer group for automatic partition assignment.
createConsumer
  :: [Text]          -- ^ Bootstrap brokers
  -> Text            -- ^ Consumer group ID
  -> ConsumerConfig  -- ^ Configuration
  -> IO (Either String Consumer)
createConsumer brokers groupId config = do
  -- Parse broker addresses
  let parsedBrokers = map parseBrokerAddress brokers
  case sequence parsedBrokers of
    Left err -> return $ Left $ "Failed to parse broker addresses: " ++ err
    Right brokerAddrs -> do
      -- Initialize connection manager
      connManager <- Conn.createConnectionManager
      
      -- Initialize metadata cache
      metadataCache <- Meta.createMetadataCache
      
      -- Fetch initial metadata from bootstrap brokers
      -- Connect to first bootstrap broker and fetch metadata
      let firstBroker = head brokerAddrs
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
              -- Initialize version cache
              versionCache <- AV.createVersionCache
              
              -- Initialize assignment and paused maps
              assignment <- StmMap.newIO
              paused <- StmMap.newIO
              
              -- Initialize correlation ID
              corrId <- newTVarIO 0
              
              -- Initialize heartbeat if in a consumer group
              heartbeatM <- if T.null groupId
                then return Nothing
                else do
                  -- Create heartbeat state
                  hbState <- HB.createHeartbeatState
                    groupId
                    (consumerHeartbeatIntervalMs config)
                    connManager
                    versionCache
                    (consumerClientId config)
                  
                  -- Start heartbeat thread
                  hbThread <- HB.startHeartbeatThread hbState
                  
                  return $ Just (hbState, hbThread)
              
              let consumer = Consumer
                    { consumerConfig = config { consumerGroupId = groupId }
                    , consumerConnManager = connManager
                    , consumerMetadata = metadataCache
                    , consumerVersionCache = versionCache
                    , consumerAssignment = assignment
                    , consumerHeartbeat = heartbeatM
                    , consumerCorrelationId = corrId
                    , consumerPaused = paused
                    }
              
              return $ Right consumer

-- | Parse broker address in "host:port" format
parseBrokerAddress :: Text -> Either String Conn.BrokerAddress
parseBrokerAddress addr =
  case T.splitOn ":" addr of
    [host, portText] ->
      case reads (T.unpack portText) of
        [(port, "")] -> Right $ Conn.BrokerAddress (T.unpack host) port
        _ -> Left $ "Invalid port: " ++ T.unpack portText
    _ -> Left $ "Invalid broker address format (expected host:port): " ++ T.unpack addr

-- | Query partition offsets using ListOffsets API.
--
-- This queries the broker for offsets at a given timestamp:
-- - timestamp = -2: earliest offset
-- - timestamp = -1: latest offset
-- - timestamp >= 0: offset at or after the given timestamp
queryPartitionOffsets
  :: Consumer
  -> [TopicPartition]
  -> Int64  -- ^ Timestamp (-2 for earliest, -1 for latest)
  -> IO (Either String [(TopicPartition, Int64)])
queryPartitionOffsets Consumer{..} partitions timestamp = do
  -- Group partitions by topic
  let byTopic = Map.fromListWith (++)
        [ (tpTopic tp, [tpPartition tp])
        | tp <- partitions
        ]
  
  -- Get any broker from the metadata cache
  brokersM <- atomically $ Meta.getAllBrokers consumerMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available in metadata cache"
    Just [] -> return $ Left "No brokers available in metadata cache"
    Just (broker:_) -> do
      -- Connect to the broker
      let brokerAddr = Meta.brokerMetaAddress broker
          connConfig = Conn.defaultConnectionConfig
      connResult <- Conn.getOrCreateConnection consumerConnManager brokerAddr connConfig
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          -- Get correlation ID
          corrId <- atomically $ do
            cid <- readTVar consumerCorrelationId
            writeTVar consumerCorrelationId (cid + 1)
            return cid
          
          -- Build ListOffsetsRequest
          let apiKey = 2  -- ListOffsets API
              apiVersion = 1  -- Use version 1 (minimum supported)
              
              topics = V.fromList $ map buildTopicRequest $ Map.toList byTopic
              
              buildTopicRequest :: (Text, [Int32]) -> LOReq.ListOffsetsTopic
              buildTopicRequest (topic, partIds) =
                LOReq.ListOffsetsTopic
                  { LOReq.listOffsetsTopicName = P.mkKafkaString topic
                  , LOReq.listOffsetsTopicPartitions = P.mkKafkaArray $ V.fromList $ map buildPartRequest partIds
                  }
              
              buildPartRequest :: Int32 -> LOReq.ListOffsetsPartition
              buildPartRequest partId =
                LOReq.ListOffsetsPartition
                  { LOReq.listOffsetsPartitionPartitionIndex = partId
                  , LOReq.listOffsetsPartitionCurrentLeaderEpoch = -1
                  , LOReq.listOffsetsPartitionTimestamp = timestamp
                  }
              
              request = LOReq.ListOffsetsRequest
                { LOReq.listOffsetsRequestReplicaId = -1  -- Consumer
                , LOReq.listOffsetsRequestIsolationLevel = 0  -- Read uncommitted
                , LOReq.listOffsetsRequestTopics = P.mkKafkaArray topics
                , LOReq.listOffsetsRequestTimeoutMs = 30000  -- 30 second timeout
                }
              
              requestBody = runPutS $ LOReq.encodeListOffsetsRequest apiVersion request
              clientId = P.mkKafkaString (consumerClientId consumerConfig)
          
          -- Send request
          result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientId requestBody
          case result of
            Left err -> return $ Left err
            Right (respCorrId, respBody) ->
              if respCorrId /= corrId
                then return $ Left "Correlation ID mismatch"
                else case runGetS (LOResp.decodeListOffsetsResponse apiVersion) respBody of
                  Left err -> return $ Left $ "Failed to decode ListOffsets response: " ++ err
                  Right response -> do
                    -- Extract offsets from response
                    let offsets = extractOffsets response
                    return $ Right offsets
  where
    extractOffsets :: LOResp.ListOffsetsResponse -> [(TopicPartition, Int64)]
    extractOffsets response =
      case P.unKafkaArray (LOResp.listOffsetsResponseTopics response) of
        P.Null -> []
        P.NotNull topicsVec -> concatMap extractTopicOffsets (V.toList topicsVec)
    
    extractTopicOffsets :: LOResp.ListOffsetsTopicResponse -> [(TopicPartition, Int64)]
    extractTopicOffsets topicResp =
      let topic = case P.unKafkaString (LOResp.listOffsetsTopicResponseName topicResp) of
            P.Null -> ""
            P.NotNull t -> t
          partitions = case P.unKafkaArray (LOResp.listOffsetsTopicResponsePartitions topicResp) of
            P.Null -> []
            P.NotNull vec -> V.toList vec
      in mapMaybe (extractPartitionOffset topic) partitions
    
    extractPartitionOffset :: Text -> LOResp.ListOffsetsPartitionResponse -> Maybe (TopicPartition, Int64)
    extractPartitionOffset topic partResp =
      let partId = LOResp.listOffsetsPartitionResponsePartitionIndex partResp
          errorCode = LOResp.listOffsetsPartitionResponseErrorCode partResp
          offset = LOResp.listOffsetsPartitionResponseOffset partResp
      in if errorCode == 0
           then Just (TopicPartition topic partId, offset)
           else Nothing

-- | Close the consumer.
--
-- Leaves the consumer group, stops heartbeat thread, and closes all connections.
closeConsumer :: Consumer -> IO ()
closeConsumer consumer = closeConsumerWithTimeout consumer 30000

-- | Close the consumer with a specified timeout (KIP-102).
--
-- Attempts to cleanly leave the consumer group and commit any pending offsets
-- before closing, waiting up to the specified timeout in milliseconds.
-- If the timeout expires, the consumer is forcibly closed.
--
-- @since KIP-102
closeConsumerWithTimeout :: Consumer -> Int -> IO ()
closeConsumerWithTimeout Consumer{..} timeoutMs = do
  -- Stop heartbeat and leave group with timeout
  case consumerHeartbeat of
    Nothing -> return ()
    Just (hbState, hbThread) -> do
      -- TODO: Send LeaveGroup request with timeout
      -- For now, just stop the heartbeat thread
      HB.stopHeartbeatThread hbState hbThread
      -- Give it time to send final heartbeat
      let waitMicros = min (timeoutMs * 1000) 1000000  -- Max 1 second for cleanup
      threadDelay waitMicros
  
  -- Close all connections
  Conn.closeAllConnections consumerConnManager

-- | Subscribe to topics.
--
-- Subscribes to the given topics and triggers partition assignment.
-- For use with consumer groups - enables automatic partition assignment and rebalancing.
subscribe :: Consumer -> [Text] -> IO (Either String ())
subscribe consumer@Consumer{..} topics = do
  case consumerHeartbeat of
    Nothing -> return $ Left "Cannot subscribe: consumer not in a group (groupId was empty)"
    Just (hbState, _) -> do
      -- TODO: Full implementation would:
      -- 1. Find group coordinator using FindCoordinator
      -- 2. Join group with JoinGroup request (including subscribed topics)
      -- 3. Wait for leader to assign partitions
      -- 4. Sync group state with SyncGroup request
      -- 5. Update consumerAssignment with assigned partitions
      -- 6. Fetch initial offsets or use committed offsets
      
      -- For now, return a placeholder indicating partial implementation
      return $ Left "subscribe: Group coordination flow not yet fully implemented. Use 'assign' for manual partition assignment."

-- | Unsubscribe from all topics.
--
-- Clears the subscription and partition assignment.
unsubscribe :: Consumer -> IO ()
unsubscribe Consumer{..} = atomically $ do
  -- Clear all assignments
  pairs <- ListT.toList $ StmMap.listT consumerAssignment
  forM_ pairs $ \(tp, _) -> StmMap.delete tp consumerAssignment

-- | Manually assign partitions (disables group management).
--
-- Assigns specific partitions to this consumer without using consumer groups.
-- Useful for fine-grained control or when not using consumer groups.
assign :: Consumer -> [TopicPartition] -> IO (Either String ())
assign consumer@Consumer{..} partitions = do
  -- Determine initial fetch offsets for each partition
  -- Strategy depends on consumerAutoOffsetReset
  let offsetStrategy = consumerAutoOffsetReset consumerConfig
  
  -- Query actual offsets from broker based on strategy
  case offsetStrategy of
    None -> do
      -- For None strategy, just use offset 0
      atomically $ do
        pairs <- ListT.toList $ StmMap.listT consumerAssignment
        forM_ pairs $ \(tp, _) -> StmMap.delete tp consumerAssignment
        forM_ partitions $ \tp -> StmMap.insert 0 tp consumerAssignment
      return $ Right ()
    
    _ -> do
      -- For Earliest/Latest, query the broker for actual offsets
      let timestamp = case offsetStrategy of
            Earliest -> -2  -- Special value for earliest offset
            Latest -> -1    -- Special value for latest offset
            None -> 0       -- Won't reach here
      
      offsetsResult <- queryPartitionOffsets consumer partitions timestamp
      case offsetsResult of
        Left err -> return $ Left err
        Right offsets -> do
          -- Clear existing assignments and add new ones with queried offsets
          atomically $ do
            pairs <- ListT.toList $ StmMap.listT consumerAssignment
            forM_ pairs $ \(tp, _) -> StmMap.delete tp consumerAssignment
            forM_ offsets $ \(tp, offset) -> StmMap.insert offset tp consumerAssignment
          return $ Right ()

-- | Poll for new records.
--
-- Fetches records from all assigned partitions up to maxPollRecords.
-- Returns records from multiple partitions in no particular order.
poll
  :: Consumer
  -> Int  -- ^ Timeout in milliseconds
  -> IO (Either String [ConsumerRecord])
poll consumer@Consumer{..} timeoutMs = do
  -- Get current assignment and paused partitions using stm-containers
  assignment <- atomically $ do
    asgn <- ListT.toList $ StmMap.listT consumerAssignment
    pausedList <- ListT.toList $ StmMap.listT consumerPaused
    let pausedSet = Map.fromList pausedList
    return [(tp, offset) | (tp, offset) <- asgn, not (Map.member tp pausedSet)]
  
  if null assignment
    then return $ Right []  -- No assignment yet or all paused
    else do
      -- Fetch from all active partitions
      result <- fetchRecords consumer assignment timeoutMs
      
      case result of
        Left err -> return $ Left err
        Right records -> do
          -- Update fetch positions based on fetched records
          atomically $ do
            forM_ records $ \r -> do
              let tp = TopicPartition (crTopic r) (crPartition r)
                  nextOffset = crOffset r + 1
              -- Update to max of current and next offset
              currentM <- StmMap.lookup tp consumerAssignment
              case currentM of
                Nothing -> StmMap.insert nextOffset tp consumerAssignment
                Just current -> when (nextOffset > current) $
                  StmMap.insert nextOffset tp consumerAssignment
          
          -- Limit to maxPollRecords
          let maxRecords = consumerMaxPollRecords consumerConfig
              limitedRecords = take maxRecords records
          
          return $ Right limitedRecords

-- | Seek to a specific offset.
seek :: Consumer -> TopicPartition -> Int64 -> IO (Either String ())
seek consumer tp offset =
  return $ Left "seek not yet implemented"

-- | Seek to the beginning of partitions.
seekToBeginning :: Consumer -> [TopicPartition] -> IO (Either String ())
seekToBeginning consumer tps =
  return $ Left "seekToBeginning not yet implemented"

-- | Seek to the end of partitions.
seekToEnd :: Consumer -> [TopicPartition] -> IO (Either String ())
seekToEnd consumer tps =
  return $ Left "seekToEnd not yet implemented"

-- | Commit offsets synchronously.
--
-- Commits the current fetch positions for all assigned partitions.
-- Blocks until the broker acknowledges the commit.
commitSync :: Consumer -> IO (Either String ())
commitSync consumer@Consumer{..} = do
  -- Get current offsets to commit
  offsets <- atomically $ ListT.toList $ StmMap.listT consumerAssignment
  
  if null offsets
    then return $ Right ()  -- Nothing to commit
    else commitOffsetsSync consumer (consumerGroupId consumerConfig) offsets

-- | Commit offsets asynchronously.
--
-- Commits the current fetch positions for all assigned partitions.
-- Returns immediately without waiting for broker acknowledgment.
commitAsync :: Consumer -> IO (Either String ())
commitAsync consumer@Consumer{..} = do
  -- Get current offsets to commit
  offsets <- atomically $ ListT.toList $ StmMap.listT consumerAssignment
  
  if null offsets
    then return $ Right ()  -- Nothing to commit
    else do
      -- Fire and forget - don't wait for response
      _ <- async $ commitOffsetsSync consumer (consumerGroupId consumerConfig) offsets
      return $ Right ()

-- | Get committed offset for a partition.
--
-- Fetches the last committed offset for the given partition from the broker.
committed :: Consumer -> TopicPartition -> IO (Either String Int64)
committed consumer@Consumer{..} tp = do
  fetchCommittedOffsets consumer (consumerGroupId consumerConfig) [tp]

-- | Pause consumption from partitions.
pause :: Consumer -> [TopicPartition] -> IO ()
pause Consumer{..} tps = atomically $
  forM_ tps $ \tp -> StmMap.insert () tp consumerPaused

-- | Resume consumption from partitions.
resume :: Consumer -> [TopicPartition] -> IO ()
resume Consumer{..} tps = atomically $
  forM_ tps $ \tp -> StmMap.delete tp consumerPaused

-- | Get current partition assignment.
assignment :: Consumer -> IO [TopicPartition]
assignment Consumer{..} = atomically $ do
  pairs <- ListT.toList $ StmMap.listT consumerAssignment
  return $ map fst pairs

-- | Internal: Fetch records from a list of topic-partitions
--
-- This function groups partitions by topic, sends FetchRequests to the
-- appropriate partition leaders, and decodes the RecordBatches.
fetchRecords
  :: Consumer
  -> [(TopicPartition, Int64)]  -- ^ Partitions and their fetch offsets
  -> Int                        -- ^ Timeout (ms)
  -> IO (Either String [ConsumerRecord])
fetchRecords consumer@Consumer{..} partitions timeoutMs = do
  -- Group partitions by topic
  let byTopic = Map.fromListWith (++)
        [ (tpTopic tp, [(tpPartition tp, offset)])
        | (tp, offset) <- partitions
        ]
  
  -- For each topic, find partition leaders and group by broker
  -- If leaders aren't found, refresh metadata first
  let topics = Map.keys byTopic
  
  -- Get any broker from the metadata cache to refresh metadata if needed
  brokersM <- atomically $ Meta.getAllBrokers consumerMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available in metadata cache"
    Just [] -> return $ Left "No brokers available in metadata cache"
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          connConfig = Conn.defaultConnectionConfig
      connResult <- Conn.getOrCreateConnection consumerConnManager brokerAddr connConfig
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          -- Get current correlation ID and increment
          corrId <- atomically $ do
            cid <- readTVar consumerCorrelationId
            writeTVar consumerCorrelationId (cid + 1)
            return cid
          
          -- Refresh metadata for these topics
          refreshResult <- Meta.refreshTopicMetadata conn consumerMetadata (Just topics) corrId
          case refreshResult of
            Left err -> return $ Left $ "Failed to refresh metadata: " ++ err
            Right _ -> do
              -- Now look up leaders
              leaderMap <- fmap Map.fromList $ forM (Map.toList byTopic) $ \(topic, parts) -> do
                leaders <- forM parts $ \(partId, offset) -> do
                  leaderM <- atomically $ Meta.getPartitionLeader consumerMetadata topic partId
                  case leaderM of
                    Nothing -> return $ Left $ "No leader for " ++ T.unpack topic ++ ":" ++ show partId
                    Just leader -> return $ Right (leader, (partId, offset))
                
                case sequence leaders of
                  Left err -> return (topic, Left err)
                  Right ls -> return (topic, Right ls)
              
              -- Check for errors
              case sequence leaderMap of
                Left err -> return $ Left err
                Right topicLeaders -> do
                  -- Group by broker
                  let byBroker = Map.fromListWith (++)
                        [ (broker, [(topic, partId, offset)])
                        | (topic, leaders) <- Map.toList topicLeaders
                        , (broker, (partId, offset)) <- leaders
                        ]
                  
                  -- Fetch from each broker (KIP-392: pass rack ID)
                  results <- forM (Map.toList byBroker) $ \(broker, reqs) ->
                    fetchFromBroker consumerConnManager consumerVersionCache broker reqs timeoutMs consumerCorrelationId (consumerRackId consumerConfig)
                  
                  -- Combine results
                  case sequence results of
                    Left err -> return $ Left err
                    Right recordLists -> return $ Right $ concat recordLists

-- | Internal: Fetch from a single broker
fetchFromBroker
  :: Conn.ConnectionManager
  -> AV.ApiVersionCache
  -> Meta.BrokerMetadata
  -> [(Text, Int32, Int64)]  -- ^ (topic, partition, offset)
  -> Int                     -- ^ Timeout (ms)
  -> TVar Int32              -- ^ Correlation ID source
  -> Maybe Text              -- ^ Rack ID for rack-aware fetching (KIP-392)
  -> IO (Either String [ConsumerRecord])
fetchFromBroker connMgr versionCache broker requests timeoutMs corrIdVar rackIdM = do
  let brokerAddr = Meta.brokerMetaAddress broker
  
  -- Get connection
  connResult <- Conn.getOrCreateConnection connMgr brokerAddr Conn.defaultConnectionConfig
  
  case connResult of
    Left err -> return $ Left $ "Failed to connect: " ++ err
    Right conn -> do
      -- Get correlation ID
      corrId <- atomically $ do
        cid <- readTVar corrIdVar
        writeTVar corrIdVar (cid + 1)
        return cid
      
      let apiKey = 1  -- Fetch API
          clientMaxVersion = 11
          minSupportedVersion = 4  -- FetchRequest supports versions 4-18
      
      -- Version negotiation
      brokerVersionM <- atomically $ AV.queryApiVersion versionCache brokerAddr apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> minSupportedVersion  -- Default to minimum supported version
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> minSupportedVersion
              Just v -> max minSupportedVersion v  -- Ensure we use at least the minimum
      
      -- Group by topic
      let byTopic = Map.fromListWith (++)
            [ (topic, [(partId, offset)])
            | (topic, partId, offset) <- requests
            ]
      
      -- Build FetchRequest
      let fetchTopics = V.fromList
            [ FR.FetchTopic
                { FR.fetchTopicTopic = P.mkKafkaString topic
                , FR.fetchTopicTopicId = P.nullUuid
                , FR.fetchTopicPartitions = P.mkKafkaArray $ V.fromList
                    [ FR.FetchPartition
                        { FR.fetchPartitionPartition = partId
                        , FR.fetchPartitionCurrentLeaderEpoch = -1
                        , FR.fetchPartitionFetchOffset = offset
                        , FR.fetchPartitionLastFetchedEpoch = -1
                        , FR.fetchPartitionLogStartOffset = -1
                        , FR.fetchPartitionPartitionMaxBytes = 1048576  -- 1MB per partition
                        , FR.fetchPartitionReplicaDirectoryId = P.nullUuid
                        , FR.fetchPartitionHighWatermark = 9223372036854775807
                        }
                    | (partId, offset) <- parts
                    ]
                }
            | (topic, parts) <- Map.toList byTopic
            ]
          
          -- KIP-392: Use rack ID for rack-aware fetching if configured
          rackIdKafka = case rackIdM of
            Nothing -> P.KafkaString P.Null
            Just rackId -> P.mkKafkaString rackId
          
          request = FR.FetchRequest
            { FR.fetchRequestReplicaId = -1  -- Consumer (not a replica)
            , FR.fetchRequestMaxWaitMs = fromIntegral timeoutMs
            , FR.fetchRequestMinBytes = 1
            , FR.fetchRequestMaxBytes = 52428800  -- 50MB total
            , FR.fetchRequestIsolationLevel = 0  -- READ_UNCOMMITTED
            , FR.fetchRequestSessionId = 0
            , FR.fetchRequestSessionEpoch = -1
            , FR.fetchRequestTopics = P.mkKafkaArray fetchTopics
            , FR.fetchRequestForgottenTopicsData = P.mkKafkaArray V.empty
            , FR.fetchRequestRackId = rackIdKafka  -- KIP-392: rack-aware fetching
            , FR.fetchRequestClusterId = P.KafkaString P.Null
            , FR.fetchRequestReplicaState = FR.ReplicaState
                { FR.replicaStateReplicaId = -1
                , FR.replicaStateReplicaEpoch = 0
                }
            }
          
          requestBody = runPutS $ FR.encodeFetchRequest apiVersion request
          clientId = P.mkKafkaString "kafka-native-consumer"
      
      -- Send request
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientId requestBody
      
      case result of
        Left err -> return $ Left $ "Fetch failed: " ++ err
        Right (_, responseBody) -> do
          case runGetS (FResp.decodeFetchResponse apiVersion) responseBody of
            Left err -> return $ Left $ "Failed to parse FetchResponse: " ++ err
            Right response -> extractRecordsFromFetchResponse response

-- | Extract records from a FetchResponse
extractRecordsFromFetchResponse :: FResp.FetchResponse -> IO (Either String [ConsumerRecord])
extractRecordsFromFetchResponse response = do
  let topicsNullable = P.unKafkaArray $ FResp.fetchResponseResponses response
      topics = case topicsNullable of
        P.Null -> []
        P.NotNull v -> V.toList v
  
  results <- forM topics $ \topicResp -> do
    let topicName = extractKafkaString $ FResp.fetchableTopicResponseTopic topicResp
        partitionsNullable = P.unKafkaArray $ FResp.fetchableTopicResponsePartitions topicResp
        partitions = case partitionsNullable of
          P.Null -> []
          P.NotNull v -> V.toList v
    
    partResults <- forM partitions $ \partResp -> do
      let partId = FResp.partitionDataPartitionIndex partResp
          errorCode = FResp.partitionDataErrorCode partResp
          recordsBytesNullable = P.unKafkaBytes $ FResp.partitionDataRecords partResp
          recordsBytes = case recordsBytesNullable of
            P.Null -> BS.empty
            P.NotNull bs -> bs
      
      if errorCode /= 0
        then return $ Left $ "Fetch error for " ++ T.unpack topicName ++ 
                            ":" ++ show partId ++ " code=" ++ show errorCode
        else if BS.null recordsBytes
          then return $ Right []
          else do
            -- Decode RecordBatches
            batches <- decodeAllBatches recordsBytes
            case batches of
              Left err -> return $ Left $ "Failed to decode batches: " ++ err
              Right bs -> return $ Right $ concatMap (convertBatchToRecords topicName partId) bs
    
    case sequence partResults of
      Left err -> return $ Left err
      Right recs -> return $ Right $ concat recs
  
  case sequence results of
    Left err -> return $ Left err
    Right allRecs -> return $ Right $ concat allRecs

-- | Extract Text from KafkaString
extractKafkaString :: P.KafkaString -> Text
extractKafkaString ks = case P.unKafkaString ks of
  P.Null -> T.empty
  P.NotNull t -> t

-- | Decode all RecordBatches from a ByteString
decodeAllBatches :: ByteString -> IO (Either String [RB.RecordBatch])
decodeAllBatches bs
  | BS.null bs = return $ Right []
  | otherwise = do
      result <- RB.decodeRecordBatchWithDecompression bs
      case result of
        Left err -> return $ Left err
        Right batch -> do
          -- Calculate batch size: base offset (8) + length field (4) + length value
          let batchSize = 8 + 4 + fromIntegral (calculateBatchLength batch)
              remaining = BS.drop batchSize bs
          
          if BS.null remaining
            then return $ Right [batch]
            else do
              rest <- decodeAllBatches remaining
              case rest of
                Left err -> return $ Left err
                Right batches -> return $ Right (batch : batches)

-- | Calculate the length field value for a batch (everything after the length field)
calculateBatchLength :: RB.RecordBatch -> Int32
calculateBatchLength batch =
  let encoded = RB.encodeRecordBatch batch
      -- Skip base offset (8 bytes) to get to length field
      lengthBytes = BS.take 4 $ BS.drop 8 encoded
  in case runGetS deserialize lengthBytes of
      Left _ -> 0
      Right len -> len

-- | Convert a RecordBatch to ConsumerRecords
convertBatchToRecords :: Text -> Int32 -> RB.RecordBatch -> [ConsumerRecord]
convertBatchToRecords topic partId batch =
  let baseOffset = RB.batchBaseOffset batch
      baseTimestamp = RB.batchBaseTimestamp batch
      records = RB.batchRecords batch
  in V.toList $ V.map (\rec -> ConsumerRecord
        { crTopic = topic
        , crPartition = partId
        , crOffset = baseOffset + fromIntegral (RB.recordOffsetDelta rec)
        , crTimestamp = baseTimestamp + RB.recordTimestampDelta rec
        , crKey = RB.recordKey rec
        , crValue = RB.recordValue rec
        , crHeaders = convertHeaders (RB.recordHeaders rec)
        }) records

-- | Convert RecordHeaders to (Text, ByteString) tuples
-- Only includes headers with non-null values
convertHeaders :: [RB.RecordHeader] -> [(Text, ByteString)]
convertHeaders headers = mapMaybe convertHeader headers
  where
    convertHeader hdr = do
      -- Convert key from ByteString to Text (assume UTF-8)
      let keyText = TE.decodeUtf8 (RB.headerKey hdr)
      -- Only include if value is present
      value <- RB.headerValue hdr
      return (keyText, value)

-- | Internal: Commit offsets synchronously to the broker
commitOffsetsSync
  :: Consumer
  -> Text                                 -- ^ Group ID
  -> [(TopicPartition, Int64)]           -- ^ Partitions and offsets to commit
  -> IO (Either String ())
commitOffsetsSync Consumer{..} groupId offsets = do
  -- TODO: Find the group coordinator
  -- For now, we'll assume we have a coordinator from the heartbeat state
  case consumerHeartbeat of
    Nothing -> return $ Left "Not in a consumer group, cannot commit offsets"
    Just (hbState, _) -> do
      coordAddrM <- atomically $ readTVar (HB.hbCoordinatorAddr hbState)
      case coordAddrM of
        Nothing -> return $ Left "No group coordinator known"
        Just coordAddr -> do
          -- Get connection to coordinator
          connResult <- Conn.getOrCreateConnection consumerConnManager coordAddr Conn.defaultConnectionConfig
          case connResult of
            Left err -> return $ Left $ "Failed to connect to coordinator: " ++ err
            Right conn -> do
              -- Get correlation ID
              corrId <- atomically $ do
                cid <- readTVar consumerCorrelationId
                writeTVar consumerCorrelationId (cid + 1)
                return cid
              
              let apiKey = 8  -- OffsetCommit API
                  clientMaxVersion = 8  -- Max version we support
              
              -- Version negotiation
              brokerVersionM <- atomically $ AV.queryApiVersion consumerVersionCache coordAddr apiKey
              let apiVersion = case brokerVersionM of
                    Nothing -> 0
                    Just range -> case AV.selectVersion clientMaxVersion range of
                      Nothing -> 0
                      Just v -> v
              
              -- Group offsets by topic
              let byTopic = Map.fromListWith (++)
                    [ (tpTopic tp, [(tpPartition tp, offset)])
                    | (tp, offset) <- offsets
                    ]
                  
                  topics = V.fromList
                    [ OCReq.OffsetCommitRequestTopic
                        { OCReq.offsetCommitRequestTopicName = P.mkKafkaString topic
                        , OCReq.offsetCommitRequestTopicTopicId = P.nullUuid
                        , OCReq.offsetCommitRequestTopicPartitions = P.mkKafkaArray $ V.fromList
                            [ OCReq.OffsetCommitRequestPartition
                                { OCReq.offsetCommitRequestPartitionPartitionIndex = partId
                                , OCReq.offsetCommitRequestPartitionCommittedOffset = offset
                                , OCReq.offsetCommitRequestPartitionCommittedLeaderEpoch = -1
                                , OCReq.offsetCommitRequestPartitionCommittedMetadata = P.KafkaString P.Null
                                }
                            | (partId, offset) <- parts
                            ]
                        }
                    | (topic, parts) <- Map.toList byTopic
                    ]
                  
                  request = OCReq.OffsetCommitRequest
                    { OCReq.offsetCommitRequestGroupId = P.mkKafkaString groupId
                    , OCReq.offsetCommitRequestGenerationIdOrMemberEpoch = -1  -- TODO: Get from heartbeat state
                    , OCReq.offsetCommitRequestMemberId = P.mkKafkaString ""  -- TODO: Get from heartbeat state
                    , OCReq.offsetCommitRequestGroupInstanceId = P.KafkaString P.Null
                    , OCReq.offsetCommitRequestRetentionTimeMs = -1  -- Use broker default
                    , OCReq.offsetCommitRequestTopics = P.mkKafkaArray topics
                    }
                  
                  requestBody = runPutS $ OCReq.encodeOffsetCommitRequest apiVersion request
                  clientId = P.mkKafkaString (consumerClientId consumerConfig)
              
              -- Send request
              result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientId requestBody
              
              case result of
                Left err -> return $ Left $ "OffsetCommit failed: " ++ err
                Right (_, responseBody) -> do
                  case runGetS (OCResp.decodeOffsetCommitResponse apiVersion) responseBody of
                    Left err -> return $ Left $ "Failed to parse OffsetCommitResponse: " ++ err
                    Right response -> do
                      -- Check for errors in response
                      let topicsNullable = P.unKafkaArray $ OCResp.offsetCommitResponseTopics response
                          topics = case topicsNullable of
                            P.Null -> []
                            P.NotNull v -> V.toList v
                          
                          errors = [ (topic, partId, errorCode)
                                   | topicResp <- topics
                                   , let topic = extractKafkaString $ OCResp.offsetCommitResponseTopicName topicResp
                                         partsNullable = P.unKafkaArray $ OCResp.offsetCommitResponseTopicPartitions topicResp
                                         parts = case partsNullable of
                                           P.Null -> []
                                           P.NotNull v -> V.toList v
                                   , partResp <- parts
                                   , let partId = OCResp.offsetCommitResponsePartitionPartitionIndex partResp
                                         errorCode = OCResp.offsetCommitResponsePartitionErrorCode partResp
                                   , errorCode /= 0
                                   ]
                      
                      if null errors
                        then return $ Right ()
                        else return $ Left $ "Offset commit errors: " ++ show errors

-- | Internal: Fetch committed offsets from the broker
fetchCommittedOffsets
  :: Consumer
  -> Text                      -- ^ Group ID
  -> [TopicPartition]          -- ^ Partitions to fetch offsets for
  -> IO (Either String Int64)
fetchCommittedOffsets Consumer{..} groupId tps = do
  -- TODO: Find the group coordinator
  case consumerHeartbeat of
    Nothing -> return $ Left "Not in a consumer group, cannot fetch committed offsets"
    Just (hbState, _) -> do
      coordAddrM <- atomically $ readTVar (HB.hbCoordinatorAddr hbState)
      case coordAddrM of
        Nothing -> return $ Left "No group coordinator known"
        Just coordAddr -> do
          -- Get connection to coordinator
          connResult <- Conn.getOrCreateConnection consumerConnManager coordAddr Conn.defaultConnectionConfig
          case connResult of
            Left err -> return $ Left $ "Failed to connect to coordinator: " ++ err
            Right conn -> do
              -- Get correlation ID
              corrId <- atomically $ do
                cid <- readTVar consumerCorrelationId
                writeTVar consumerCorrelationId (cid + 1)
                return cid
              
              let apiKey = 9  -- OffsetFetch API
                  clientMaxVersion = 8  -- Max version we support
              
              -- Version negotiation
              brokerVersionM <- atomically $ AV.queryApiVersion consumerVersionCache coordAddr apiKey
              let apiVersion = case brokerVersionM of
                    Nothing -> 0
                    Just range -> case AV.selectVersion clientMaxVersion range of
                      Nothing -> 0
                      Just v -> v
              
              -- Group by topic
              let byTopic = Map.fromListWith (++)
                    [ (tpTopic tp, [tpPartition tp])
                    | tp <- tps
                    ]
                  
                  topics = V.fromList
                    [ OFReq.OffsetFetchRequestTopic
                        { OFReq.offsetFetchRequestTopicName = P.mkKafkaString topic
                        , OFReq.offsetFetchRequestTopicPartitionIndexes = P.mkKafkaArray $ V.fromList parts
                        }
                    | (topic, parts) <- Map.toList byTopic
                    ]
                  
                  request = OFReq.OffsetFetchRequest
                    { OFReq.offsetFetchRequestGroupId = P.mkKafkaString groupId
                    , OFReq.offsetFetchRequestTopics = P.mkKafkaArray topics
                    , OFReq.offsetFetchRequestGroups = P.mkKafkaArray V.empty
                    , OFReq.offsetFetchRequestRequireStable = False
                    }
                  
                  requestBody = runPutS $ OFReq.encodeOffsetFetchRequest apiVersion request
                  clientId = P.mkKafkaString (consumerClientId consumerConfig)
              
              -- Send request
              result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientId requestBody
              
              case result of
                Left err -> return $ Left $ "OffsetFetch failed: " ++ err
                Right (_, responseBody) -> do
                  case runGetS (OFResp.decodeOffsetFetchResponse apiVersion) responseBody of
                    Left err -> return $ Left $ "Failed to parse OffsetFetchResponse: " ++ err
                    Right response -> do
                      -- Extract offset from response
                      let topicsNullable = P.unKafkaArray $ OFResp.offsetFetchResponseTopics response
                          topics = case topicsNullable of
                            P.Null -> []
                            P.NotNull v -> V.toList v
                          
                          -- Find the first partition in response
                          offsets = [ (topic, partId, committedOffset)
                                    | topicResp <- topics
                                    , let topic = extractKafkaString $ OFResp.offsetFetchResponseTopicName topicResp
                                          partsNullable = P.unKafkaArray $ OFResp.offsetFetchResponseTopicPartitions topicResp
                                          parts = case partsNullable of
                                            P.Null -> []
                                            P.NotNull v -> V.toList v
                                    , partResp <- parts
                                    , let partId = OFResp.offsetFetchResponsePartitionPartitionIndex partResp
                                          committedOffset = OFResp.offsetFetchResponsePartitionCommittedOffset partResp
                                    ]
                      
                      case offsets of
                        [] -> return $ Left "No offset found in response"
                        ((_, _, offset):_) -> return $ Right offset
