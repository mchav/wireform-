{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
  , closeConsumerWithoutLeavingGroup
  , requestRejoin
  , setSubscriptionUserDataHook
    -- * Subscription
  , subscribe
  , unsubscribe
  , assign
    -- * Rebalance listener
  , setRebalanceListener
  , currentAssignment
  , computeAssignmentDelta
    -- * Polling and Consumption
  , poll
  , seek
  , seekToBeginning
  , seekToEnd
    -- * Offset Management
  , commitSync
  , commitAsync
  , committed
  , committedAll
  , position
    -- * Partition / time queries
  , beginningOffsets
  , endOffsets
  , offsetsForTimes
  , offsetsForTimesFull
  , OffsetAndTimestamp(..)
    -- * Partition Control
  , pause
  , resume
  , assignment
  , paused
    -- * Configuration
  , defaultConsumerConfig
  , AssignmentStrategy(..)
  , OffsetResetStrategy(..)
  , IsolationLevel(..)
  , ConsumerConfig(..)
  , StaticMembershipState(..)
  , currentStaticMembershipState
    -- * Configuration validation
  , validateConsumerConfig
    -- * Cluster info
  , consumerClusterId
  ) where

import Control.Concurrent.Async (Async, async)
import qualified Control.Exception
import Control.Exception (SomeException, try)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified System.Timeout
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable)
import Data.Int
import Control.Monad (forM, forM_, unless, when)
import qualified Data.HashMap.Strict as HashMap
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

import qualified Kafka.Client.Internal.ConsumerGroup as CG
import Kafka.Client.ConfigValidation
  ( ConfigError, renderConfigErrors )
import qualified Kafka.Client.ConfigValidation as CV
import qualified Kafka.Client.Internal.Heartbeat as HB
import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Client.Internal.Subscribe as Sub
import Kafka.Client.Metadata (MetadataCache)
import qualified Kafka.Client.Metadata as Meta
import qualified Kafka.Network.Connection as Conn
import Kafka.Network.Connection (BrokerAddress(..))
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.VersionNegotiation as VN
import qualified Kafka.Protocol.Generated.FetchRequest as FR
import qualified Kafka.Protocol.Generated.FetchResponse as FResp
import qualified Kafka.Protocol.Generated.OffsetCommitRequest as OCReq
import qualified Kafka.Protocol.Generated.OffsetCommitResponse as OCResp
import qualified Kafka.Protocol.Generated.OffsetFetchRequest as OFReq
import qualified Kafka.Protocol.Generated.OffsetFetchResponse as OFResp
import qualified Kafka.Protocol.Generated.ListOffsetsRequest as LOReq
import qualified Kafka.Protocol.Generated.ListOffsetsResponse as LOResp
import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.RecordBatch as RB
import qualified Kafka.Protocol.RecordBatchWire as RBW
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Codec as WC

-- | Partition assignment strategy.
data AssignmentStrategy
  = RangeAssignment       -- ^ Range assignment (default)
  | RoundRobinAssignment  -- ^ Round-robin assignment
  | StickyAssignment      -- ^ Sticky assignment (minimizes rebalance)
  deriving (Eq, Show, Generic)

-- | Isolation level for fetched records.
data IsolationLevel
  = ReadUncommitted
    -- ^ Default. The fetcher returns every record, including
    --   those still inside an open transaction.
  | ReadCommitted
    -- ^ Only return records that belong to a committed
    --   transaction (or no transaction at all).
  deriving (Eq, Show, Generic)

-- | Consumer configuration. Field names + defaults map onto
-- librdkafka's @CONFIGURATION.md@ entries; the librdkafka name
-- is given inline next to the Haskell field.
data ConsumerConfig = ConsumerConfig
  { consumerClientId :: !Text
    -- ^ @client.id@ — identifier sent on every request.
  , consumerGroupId :: !Text
    -- ^ @group.id@ — consumer group id.
  , consumerGroupInstanceId :: !(Maybe Text)
    -- ^ @group.instance.id@ — KIP-345 static membership.
    --   Default 'Nothing'.
  , consumerAutoCommit :: !Bool
    -- ^ @enable.auto.commit@. Default 'True'.
  , consumerAutoCommitIntervalMs :: !Int
    -- ^ @auto.commit.interval.ms@. Default 5000.
  , consumerEnableAutoOffsetStore :: !Bool
    -- ^ @enable.auto.offset.store@: when 'True' (the default),
    --   'poll' implicitly stages every fetched offset for the
    --   next auto-commit. When 'False', the application must
    --   call 'storeOffset' before commit.
  , consumerSessionTimeoutMs :: !Int
    -- ^ @session.timeout.ms@. Default 45000 (KIP-735 widened
    --   from 10000 in Kafka 3.0).
  , consumerHeartbeatIntervalMs :: !Int
    -- ^ @heartbeat.interval.ms@. Default 3000.
  , consumerMaxPollRecords :: !Int
    -- ^ @max.poll.records@. Default 500.
  , consumerMaxPollIntervalMs :: !Int
    -- ^ @max.poll.interval.ms@. Default 300000 (5 minutes).
  , consumerAssignmentStrategy :: !AssignmentStrategy
    -- ^ @partition.assignment.strategy@.
  , consumerAutoOffsetReset :: !OffsetResetStrategy
    -- ^ @auto.offset.reset@. Default 'Latest'.
  , consumerIsolationLevel :: !IsolationLevel
    -- ^ @isolation.level@. Default 'ReadUncommitted'.
  , consumerEnablePartitionEof :: !Bool
    -- ^ @enable.partition.eof@: emit a synthetic EOF event when
    --   the fetcher reaches the partition's high-water mark.
    --   Default 'False'.
  , consumerCheckCrcs :: !Bool
    -- ^ @check.crcs@: verify the CRC32C of every fetched record
    --   batch. Default 'True'.
  , consumerFetchMinBytes :: !Int
    -- ^ @fetch.min.bytes@: hold the fetch response until at
    --   least this many bytes are available (or
    --   'consumerFetchMaxWaitMs' elapses). Default 1.
  , consumerFetchMaxBytes :: !Int
    -- ^ @fetch.max.bytes@: maximum total bytes returned by a
    --   single fetch across all partitions. Default 52428800
    --   (50 MiB).
  , consumerFetchMaxWaitMs :: !Int
    -- ^ @fetch.wait.max.ms@: how long the broker waits to
    --   accumulate 'consumerFetchMinBytes'. Default 500.
  , consumerFetchMessageMaxBytes :: !Int
    -- ^ @max.partition.fetch.bytes@ /
    --   @fetch.message.max.bytes@: cap per (topic, partition).
    --   Default 1048576 (1 MiB).
  , consumerFetchErrorBackoffMs :: !Int
    -- ^ @fetch.error.backoff.ms@: backoff after a failed fetch
    --   before retrying. Default 500.
  , consumerQueuedMaxMessagesKbytes :: !Int
    -- ^ @queued.max.messages.kbytes@: per-partition fetch queue
    --   ceiling in KB. Default 65536.
  , consumerRackId :: !(Maybe Text)
    -- ^ @client.rack@ — KIP-392 rack-aware fetching.
  , consumerConnectionConfig :: !Conn.ConnectionConfig
    -- ^ Lower-level connection settings: TLS, SASL, retry/backoff
    --   knobs. Defaults to 'Conn.defaultConnectionConfig' (plain TCP,
    --   no SASL). Set 'Conn.connSasl' here to enable any of the SASL
    --   mechanisms (PLAIN \/ SCRAM \/ OAUTHBEARER \/ AWS_MSK_IAM \/
    --   GSSAPI-stub) — see "Kafka.Network.Auth.SASL".
  , consumerInterceptor :: !([ConsumerRecord] -> IO [ConsumerRecord])
    -- ^ Post-fetch interceptor (analogue of
    --   @org.apache.kafka.clients.consumer.ConsumerInterceptor.onConsume@).
    --   Applied to the batch returned by 'poll' before the
    --   caller sees it. Defaults to 'pure'. Exceptions propagate.
  , consumerOnCommit
      :: !([(TopicPartition, Int64)] -> IO ())
    -- ^ Per-commit callback (mirrors
    --   @ConsumerInterceptor.onCommit@). Best-effort: exceptions
    --   in the callback are caught + dropped so a buggy hook
    --   can't break the commit pipeline.
  , consumerStaticMembershipPersist
      :: !(Maybe (StaticMembershipState -> IO ()))
    -- ^ KIP-345 callback invoked just before the heartbeat thread
    --   stops (i.e. on 'closeConsumer'). Receives the consumer's
    --   final @(memberId, generationId)@ so the application can
    --   persist them somewhere (a file, a KV store, …) and pass
    --   them back via 'consumerStaticMembershipResume' on the
    --   next start. 'Nothing' (default) disables persistence.
  , consumerStaticMembershipResume
      :: !(Maybe StaticMembershipState)
    -- ^ KIP-345 resume hook: when set, 'createConsumer' uses the
    --   given member id / generation id when joining the group,
    --   so a restart with the same @group.instance.id@ avoids
    --   a generation-bump rebalance.
  } deriving (Generic)

-- | Static-membership state to persist across restarts.
-- Mirrors the JVM client's behaviour where a static member sends
-- its previously-assigned 'memberId' on JoinGroup and the broker
-- avoids triggering a rebalance.
data StaticMembershipState = StaticMembershipState
  { staticMemberId     :: !Text
  , staticGenerationId :: !Int32
  }
  deriving (Eq, Show, Generic)

-- | Offset reset strategy when no committed offset exists.
data OffsetResetStrategy
  = Earliest  -- ^ Start from earliest available offset
  | Latest    -- ^ Start from latest offset (default)
  | None      -- ^ Throw error if no offset exists
  deriving (Eq, Show, Generic)

-- | Default consumer configuration. Values track librdkafka's
-- @CONFIGURATION.md@ defaults except where the JVM client diverges
-- (and we follow the JVM-Kafka 3.x defaults so application
-- behaviour matches what users see in @kafka-console-consumer@).
defaultConsumerConfig :: ConsumerConfig
defaultConsumerConfig = ConsumerConfig
  { consumerClientId                = "kafka-native-consumer"
  , consumerGroupId                 = "default-group"
  , consumerGroupInstanceId         = Nothing
  , consumerAutoCommit              = True
  , consumerAutoCommitIntervalMs    = 5000
  , consumerEnableAutoOffsetStore   = True
  , consumerSessionTimeoutMs        = 45_000        -- KIP-735
  , consumerHeartbeatIntervalMs     = 3000
  , consumerMaxPollRecords          = 500
  , consumerMaxPollIntervalMs       = 300_000       -- 5 minutes
  , consumerAssignmentStrategy      = RangeAssignment
  , consumerAutoOffsetReset         = Latest
  , consumerIsolationLevel          = ReadUncommitted
  , consumerEnablePartitionEof      = False
  , consumerCheckCrcs               = True
  , consumerFetchMinBytes           = 1
  , consumerFetchMaxBytes           = 52_428_800    -- 50 MiB
  , consumerFetchMaxWaitMs          = 500
  , consumerFetchMessageMaxBytes    = 1_048_576     -- 1 MiB
  , consumerFetchErrorBackoffMs     = 500
  , consumerQueuedMaxMessagesKbytes = 65_536
  , consumerRackId                  = Nothing       -- KIP-392
  , consumerConnectionConfig        = Conn.defaultConnectionConfig
  , consumerInterceptor             = pure
  , consumerOnCommit                = \_ -> pure ()
  , consumerStaticMembershipPersist = Nothing
  , consumerStaticMembershipResume  = Nothing
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
  , consumerSubscription :: !(TVar (Maybe [Text]))
    -- ^ Last topics subscribed via 'subscribe' (so 'poll' can
    --   transparently re-run the JoinGroup flow when the heartbeat
    --   thread tells us the group is rebalancing). 'Nothing' means
    --   either we are using manual 'assign' instead of group
    --   subscription, or 'subscribe' has not been called yet.
  , consumerOnAssigned :: !(TVar ([TopicPartition] -> IO ()))
  , consumerOnRevoked  :: !(TVar ([TopicPartition] -> IO ()))
  , consumerOnLost     :: !(TVar ([TopicPartition] -> IO ()))
    -- ^ Per-event callbacks fired on assigned / revoked / lost
    --   transitions. Default is the no-op; replace via
    --   'setRebalanceListener'.
  , consumerLastAssignment :: !(TVar [TopicPartition])
    -- ^ The assignment as of the last fired callback. Used to
    --   compute revoked / assigned deltas on the next rebalance.
  , consumerSubscriptionUserData :: !(TVar (IO ByteString))
    -- ^ KIP-535 hook: when 'subscribe' issues a JoinGroup, the
    --   consumer calls this 'IO ByteString' and stamps the
    --   result into the subscription-userdata blob. Default:
    --   @pure BS.empty@ (no userdata). Replace via
    --   'setSubscriptionUserDataHook'. Used by the streams
    --   runtime to advertise the local instance's
    --   host:port + owned stores so peers can route cross-
    --   instance IQ queries here.
  }

-- | Effective 'Conn.ConnectionConfig' for this consumer: takes the
-- one stored in 'consumerConfig' and overlays the consumer's client
-- id (so SASL request headers identify the right client).
consumerConnConfig :: Consumer -> Conn.ConnectionConfig
consumerConnConfig c =
  let base = consumerConnectionConfig (consumerConfig c)
  in base { Conn.connClientId = consumerClientId (consumerConfig c) }

-- | 'Conn.getOrCreateConnection' + ensure ApiVersions has been
-- negotiated for the broker. Idempotent: subsequent calls to
-- the same broker only do the cache hit, no extra round-trip.
--
-- Use this everywhere we obtain a connection to a /new/ broker
-- (i.e. anywhere that wasn't the bootstrap broker, since the
-- bootstrap broker's handshake already ran in 'createConsumer')
-- so subsequent 'pickApiVersion' / 'queryApiVersion' lookups
-- have data to consult instead of always falling back.
consumerConnect
  :: Consumer
  -> Conn.BrokerAddress
  -> IO (Either String Connection)
consumerConnect c@Consumer{..} addr = do
  cr <- Conn.getOrCreateConnection consumerConnManager addr (consumerConnConfig c)
  case cr of
    Left e     -> pure (Left e)
    Right conn -> do
      _ <- VN.ensureVersionsNegotiated
             conn addr consumerVersionCache
             (atomically $ do
                cid <- readTVar consumerCorrelationId
                writeTVar consumerCorrelationId (cid + 1)
                pure cid)
      pure (Right conn)

-- | Create a new Kafka consumer.
--
-- Initializes the consumer with connection management, metadata caching,
-- and optionally joins a consumer group for automatic partition assignment.
-- | Pure config-validation rules. Mirrors the JVM client's
-- @org.apache.kafka.clients.consumer.ConsumerConfig@ checks: every
-- rule we apply here is something the broker (or, worse, a runtime
-- assertion deep in the fetch loop) would otherwise blow up on with
-- a far less actionable error.
--
-- We deliberately don't enforce a non-empty @group.id@ here because
-- the simple consumer (no group) leaves it blank and only assigns
-- partitions manually; 'Kafka.Client.Group.validateConfig' enforces
-- the non-empty requirement when subscribing through the group API.
--
-- Returns the empty list when the config is acceptable.
validateConsumerConfig :: ConsumerConfig -> [ConfigError]
validateConsumerConfig ConsumerConfig{..} = concat
  [ CV.check (T.null consumerClientId)
      "client.id" "must be non-empty"
  , CV.check (consumerSessionTimeoutMs <= 0)
      "session.timeout.ms" "must be > 0"
  , CV.check (consumerHeartbeatIntervalMs <= 0)
      "heartbeat.interval.ms" "must be > 0"
  , CV.check (consumerHeartbeatIntervalMs >= consumerSessionTimeoutMs)
      "heartbeat.interval.ms"
      "must be < session.timeout.ms (broker fences members otherwise)"
  , CV.check (consumerMaxPollIntervalMs < consumerSessionTimeoutMs)
      "max.poll.interval.ms"
      "must be >= session.timeout.ms (KIP-62)"
  , CV.check (consumerMaxPollRecords < 1)
      "max.poll.records" "must be >= 1 (KIP-41)"
  , CV.check (consumerFetchMinBytes < 0)
      "fetch.min.bytes" "must be >= 0"
  , CV.check (consumerFetchMaxBytes <= 0)
      "fetch.max.bytes" "must be > 0"
  , CV.check (consumerFetchMinBytes > consumerFetchMaxBytes)
      "fetch.min.bytes" "must be <= fetch.max.bytes"
  , CV.check (consumerFetchMaxWaitMs < 0)
      "fetch.wait.max.ms" "must be >= 0"
  , CV.check (consumerFetchMessageMaxBytes <= 0)
      "max.partition.fetch.bytes" "must be > 0"
  , CV.check (consumerFetchErrorBackoffMs < 0)
      "fetch.error.backoff.ms" "must be >= 0"
  , CV.check (consumerQueuedMaxMessagesKbytes <= 0)
      "queued.max.messages.kbytes" "must be > 0"
  , CV.check (consumerAutoCommit && consumerAutoCommitIntervalMs <= 0)
      "auto.commit.interval.ms"
      "must be > 0 when enable.auto.commit=true"
  ]

createConsumer
  :: [Text]          -- ^ Bootstrap brokers
  -> Text            -- ^ Consumer group ID
  -> ConsumerConfig  -- ^ Configuration
  -> IO (Either String Consumer)
createConsumer _ _ config
  | configErrs <- validateConsumerConfig config
  , not (null configErrs)
  = return $ Left $ renderConfigErrors configErrs
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
          baseConn   = consumerConnectionConfig config
          -- Use the user's client id for any SASL handshake.
          connConfig = baseConn { Conn.connClientId = consumerClientId config }
      connResult <- Conn.getOrCreateConnection connManager firstBroker connConfig
      case connResult of
        Left err -> return $ Left $ "Failed to connect to bootstrap broker: " ++ err
        Right conn -> do
          -- Initialize version cache
          versionCache <- AV.createVersionCache

          -- Initialize correlation ID; the ApiVersions handshake
          -- and the metadata refresh share the same source so
          -- their correlation ids stay distinct from later
          -- consumer requests.
          corrId <- newTVarIO 0
          let nextCid = atomically $ do
                cid <- readTVar corrId
                writeTVar corrId (cid + 1)
                pure cid

          -- Run the ApiVersions handshake against the bootstrap
          -- broker so subsequent calls' 'queryApiVersion' /
          -- 'pickApiVersion' lookups find data instead of
          -- always falling back. We deliberately swallow
          -- failure: an older broker (< 0.10) doesn't recognise
          -- ApiVersions, in which case the cache stays empty
          -- and downstream calls hit their compiled-in
          -- fallbacks.
          _ <- VN.ensureVersionsNegotiated
                 conn firstBroker versionCache nextCid

          fetchResult <- Meta.refreshMetadata conn metadataCache 0
          case fetchResult of
            Left err -> return $ Left $ "Failed to fetch initial metadata: " ++ err
            Right _ -> do

              -- Initialize assignment and paused maps
              assignment <- StmMap.newIO
              paused <- StmMap.newIO

              -- No active subscription yet.
              subscription <- newTVarIO Nothing
              
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

                  -- KIP-345 static-membership resume: if the
                  -- application supplied a previously persisted
                  -- @(memberId, generationId)@, seed the
                  -- heartbeat state with it so the next JoinGroup
                  -- reuses the existing slot.
                  case consumerStaticMembershipResume config of
                    Nothing -> pure ()
                    Just StaticMembershipState{..} -> atomically $ do
                      writeTVar (HB.hbMemberId    hbState) staticMemberId
                      writeTVar (HB.hbGenerationId hbState) staticGenerationId

                  -- Start heartbeat thread
                  hbThread <- HB.startHeartbeatThread hbState

                  return $ Just (hbState, hbThread)
              
              onA <- newTVarIO (\_ -> pure () :: IO ())
              onR <- newTVarIO (\_ -> pure () :: IO ())
              onL <- newTVarIO (\_ -> pure () :: IO ())
              lastAsgn <- newTVarIO []
              subUD    <- newTVarIO (pure BS.empty :: IO ByteString)

              let consumer = Consumer
                    { consumerConfig = config { consumerGroupId = groupId }
                    , consumerConnManager = connManager
                    , consumerMetadata = metadataCache
                    , consumerVersionCache = versionCache
                    , consumerAssignment = assignment
                    , consumerHeartbeat = heartbeatM
                    , consumerCorrelationId = corrId
                    , consumerPaused = paused
                    , consumerSubscription = subscription
                    , consumerOnAssigned = onA
                    , consumerOnRevoked  = onR
                    , consumerOnLost     = onL
                    , consumerLastAssignment = lastAsgn
                    , consumerSubscriptionUserData = subUD
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
queryPartitionOffsets consumer@Consumer{..} partitions timestamp = do
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
      let brokerAddr = Meta.brokerMetaAddress broker
      -- Use the negotiation-aware connect so the broker's
      -- ApiVersions cache entry is populated before we read
      -- it back via 'pickApiVersion' below.
      connResult <- consumerConnect consumer brokerAddr
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          corrId <- atomically $ do
            cid <- readTVar consumerCorrelationId
            writeTVar consumerCorrelationId (cid + 1)
            return cid
          let apiKey = 2  -- ListOffsets
          -- ListOffsets: codegen handles up to v10. Schema
          -- changes by version:
          --   v2  KIP-98  IsolationLevel (we always send 0 here;
          --               'fetchFromBroker' honours the consumer's
          --               own isolation-level config)
          --   v4  KIP-320 LeaderEpoch in request + response
          --   v6  flexible (compact + tagged fields)
          --   v7  KIP-734 LeaderEpoch becomes mandatory in
          --               response; no request-shape change
          --   v8  KIP-1146 EarliestLocalTimestamp (-4) sentinel
          --               accepted in the timestamp field; no
          --               schema change
          --   v9  KIP-1133 Tiered storage sentinel (-5); no
          --               schema change
          --   v10 KIP-994 TimeoutMs added to the request
          -- We cap at v8 (the highest the broker accepts on
          -- non-tiered-storage clusters; v9+ rely on the broker
          -- having KIP-405 enabled which Kafka 3.7's default
          -- builds do not). The api key + cache lookup come from
          -- the 'KafkaMessage LOReq.ListOffsetsRequest' instance
          -- via 'pickApiVersionForRange'; the (0, 8) override
          -- enforces the cap. 'offsetsForTimesFull' exposes the
          -- timestamp + leader-epoch fields v4+ adds to the
          -- per-partition response.
          verR <- VN.pickApiVersionForRange @LOReq.ListOffsetsRequest
                    0 8 consumerVersionCache brokerAddr 1
          let apiVersion = case verR of
                Right v -> v
                Left  _ -> 1   -- preserve legacy fallback
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
                , LOReq.listOffsetsRequestTimeoutMs = 30000  -- v10+; ignored otherwise
                }
              
              requestBody = WC.runEncodeVer @LOReq.ListOffsetsRequest apiVersion request
              clientId = P.mkKafkaString (consumerClientId consumerConfig)
          
          result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock consumerConnManager brokerAddr) conn apiKey apiVersion corrId clientId requestBody
          case result of
            Left err -> return $ Left err
            Right (respCorrId, respBody) ->
              if respCorrId /= corrId
                then return $ Left "Correlation ID mismatch"
                else case WC.runDecodeVer @LOResp.ListOffsetsResponse apiVersion respBody of
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

-- | Close the consumer with a specified timeout.
--
-- Attempts to cleanly leave the consumer group and commit any pending offsets
-- before closing, waiting up to the specified timeout in milliseconds.
-- If the timeout expires, the consumer is forcibly closed.
--
-- @since KIP-102
closeConsumerWithTimeout :: Consumer -> Int -> IO ()
closeConsumerWithTimeout = closeConsumerImpl True

-- | @CloseOptions.leaveGroup = false@: close the
-- consumer /without/ sending a @LeaveGroup@ request. The
-- broker keeps this member's assignment alive until the
-- @session.timeout.ms@ expires, then rebalances. Use this
-- when you're doing a fast restart of the same member id
-- (e.g. rolling deploy with static membership) and want to
-- avoid the rebalance churn.
closeConsumerWithoutLeavingGroup :: Consumer -> Int -> IO ()
closeConsumerWithoutLeavingGroup = closeConsumerImpl False

-- | Programmatic rejoin trigger. Flips
-- 'HB.hbNeedsRebalance' so the next 'poll' transparently
-- re-runs JoinGroup \/ SyncGroup against the same
-- subscription — equivalent to what happens when the broker
-- replies @REBALANCE_IN_PROGRESS@ to a heartbeat.
--
-- Used by the streams runtime when its probing-rebalance
-- machinery decides a warmup replica is ready to be
-- promoted. Returns @False@ if the consumer has no
-- heartbeat thread (manual-offset / unsubscribed mode); the
-- caller can treat that as a no-op.
requestRejoin :: Consumer -> IO Bool
requestRejoin Consumer{..} = case consumerHeartbeat of
  Nothing -> pure False
  Just (hbState, _) -> do
    atomically (writeTVar (HB.hbNeedsRebalance hbState) True)
    pure True

-- | Install a callback the consumer invokes whenever
-- it issues a JoinGroup; the returned bytes become the
-- subscription-userdata blob. Used by the streams runtime to
-- advertise the local instance's @application.server@ +
-- materialised store names + owned partitions so peers can
-- compute 'KeyQueryMetadata' for cross-instance IQ routing.
--
-- The callback runs on every subscribe / re-join, so the
-- bytes reflect the /current/ state of the instance — not a
-- snapshot taken at consumer-creation time.
setSubscriptionUserDataHook
  :: Consumer -> IO ByteString -> IO ()
setSubscriptionUserDataHook Consumer{..} f =
  atomically (writeTVar consumerSubscriptionUserData f)

-- | Shared implementation. @doLeaveGroup = False@ skips the
-- @LeaveGroup@ RPC but still:
--
--   * fires onRevoked callbacks so applications can flush
--     in-flight work / commit final offsets;
--   * persists static-membership state via the user callback
--     (so a restart can re-claim the same generation);
--   * tears down the heartbeat thread + connections.
closeConsumerImpl :: Bool -> Consumer -> Int -> IO ()
closeConsumerImpl doLeaveGroup c@Consumer{..} timeoutMs = do
  -- Fire onRevoked for any partitions we still own — graceful
  -- close path so listeners can flush in-flight work and
  -- commit final offsets.
  dispatchAssignmentDelta c [] False
  case consumerHeartbeat of
    Nothing -> return ()
    Just (hbState, hbThread) -> do
      -- KIP-345 static-membership persistence: hand the
      -- application the (memberId, generationId) tuple just
      -- before we tear the heartbeat down so a restart with the
      -- same group.instance.id can avoid a generation bump.
      case consumerStaticMembershipPersist consumerConfig of
        Nothing -> pure ()
        Just k -> do
          memberId <- readTVarIO (HB.hbMemberId    hbState)
          genId    <- readTVarIO (HB.hbGenerationId hbState)
          r <- try (k (StaticMembershipState memberId genId))
                 :: IO (Either Control.Exception.SomeException ())
          case r of
            Right () -> pure ()
            Left  _  -> pure ()  -- best effort
      -- Best-effort LeaveGroup so the broker can rebalance the
      -- group immediately rather than waiting for the
      -- session.timeout.ms to expire. Bounded by the caller's
      -- 'timeoutMs' so a misbehaving coordinator can't block
      -- shutdown indefinitely. Failures here are silent — the
      -- session-timeout fallback is still correct, just slower.
      --
      -- Skipped when the caller passed @doLeaveGroup = False@
      -- (KIP-812 CloseOptions.leaveGroup = false), in which
      -- case the session-timeout path handles reassignment.
      when doLeaveGroup $ do
        _ <- System.Timeout.timeout
               (max 0 timeoutMs * 1000)
               (sendLeaveGroup c hbState)
        pure ()
      HB.stopHeartbeatThread hbState hbThread
  -- Close all connections.
  Conn.closeAllConnections consumerConnManager

-- | Read the consumer's current @(memberId, generationId)@ as a
-- 'StaticMembershipState'. Useful for tests and for callers that
-- want to snapshot the value mid-lifetime (in addition to the
-- 'consumerStaticMembershipPersist' callback that fires on
-- close).
currentStaticMembershipState
  :: Consumer -> IO (Maybe StaticMembershipState)
currentStaticMembershipState Consumer{..} = case consumerHeartbeat of
  Nothing -> pure Nothing
  Just (hbState, _) -> do
    mid <- readTVarIO (HB.hbMemberId    hbState)
    gen <- readTVarIO (HB.hbGenerationId hbState)
    pure (Just (StaticMembershipState mid gen))

-- | Read the broker-supplied cluster id off the
-- consumer's metadata cache. Returns 'Nothing' until the first
-- successful metadata refresh; afterwards reflects whatever the
-- broker set in its @MetadataResponse@.
consumerClusterId :: Consumer -> IO (Maybe Text)
consumerClusterId Consumer{..} =
  atomically (Meta.getClusterId consumerMetadata)

-- | Synchronously issue a LeaveGroup against the group coordinator.
-- Used by 'closeConsumerWithTimeout' to clean-shutdown the group
-- membership; failures are logged in the caller but otherwise ignored.
sendLeaveGroup :: Consumer -> HB.HeartbeatState -> IO ()
sendLeaveGroup c@Consumer{..} hbState = do
  coordAddrM <- atomically $ readTVar (HB.hbCoordinatorAddr hbState)
  memberId   <- readTVarIO (HB.hbMemberId hbState)
  -- Skip if we never actually joined the group (no coordinator
  -- discovered, or no memberId issued). Sending LeaveGroup with
  -- an empty memberId triggers an InvalidRequestException on the
  -- broker, which then closes the connection — and the next
  -- request sees an EOF.
  case coordAddrM of
    Nothing -> pure ()    -- never joined; nothing to leave
    _ | T.null memberId -> pure ()
    Just coordAddr -> do
      connResult <- consumerConnect c coordAddr
      case connResult of
        Left _err -> pure ()
        Right conn -> do
          corrId <- atomically $ do
            cid <- readTVar consumerCorrelationId
            writeTVar consumerCorrelationId (cid + 1)
            pure cid
          _ <- CG.leaveGroup
                 consumerVersionCache
                 consumerConnManager
                 coordAddr
                 conn
                 (HB.hbGroupId hbState)
                 memberId
                 (consumerClientId consumerConfig)
                 corrId
          pure ()

-- | Subscribe to topics with broker-side group coordination.
--
-- Walks the full consumer-group lifecycle:
--
-- 1. Discover the group coordinator ('Sub.subscribeFlow' issues a
--    FindCoordinator).
-- 2. JoinGroup with our subscription metadata and the @range@
--    assignor.
-- 3. If the broker elects us as the group leader we run
--    'Sub.rangeAssign' across every member's subscription list and
--    publish the per-member assignments via SyncGroup; otherwise we
--    just receive ours.
-- 4. OffsetFetch to pick the resume offset for each assigned
--    partition; missing offsets fall back to the consumer's
--    @auto.offset.reset@ policy.
-- 5. Populate the in-memory assignment map so 'poll' starts fetching.
--
-- The heartbeat thread (started in 'createConsumer') picks up the
-- coordinator address / member id / generation id automatically — they
-- live in the shared 'HB.HeartbeatState'.
--
-- Calling 'subscribe' a second time with a different topic set
-- re-runs the whole flow (i.e. is the equivalent of an explicit
-- rebalance request).
subscribe :: Consumer -> [Text] -> IO (Either String ())
subscribe Consumer{..} topics = do
  case consumerHeartbeat of
    Nothing -> return $ Left "Cannot subscribe: consumer not in a group (groupId was empty)"
    Just (hbState, _) -> do
      let resetPolicy = case consumerAutoOffsetReset consumerConfig of
            Earliest -> Sub.ResetEarliest
            Latest   -> Sub.ResetLatest
            None     -> Sub.ResetNone
          assignor = case consumerAssignmentStrategy consumerConfig of
            RangeAssignment      -> Sub.AssignorRange
            RoundRobinAssignment -> Sub.AssignorRoundRobin
            StickyAssignment     -> Sub.AssignorSticky
          sessionTimeout   = fromIntegral (consumerSessionTimeoutMs   consumerConfig)
          rebalanceTimeout = fromIntegral (consumerMaxPollIntervalMs  consumerConfig)
      fetchUD <- readTVarIO consumerSubscriptionUserData
      result <- Sub.subscribeFlow
                  consumerConnManager
                  (consumerConnConfig Consumer{..})
                  consumerMetadata
                  consumerVersionCache
                  hbState
                  (consumerClientId consumerConfig)
                  (consumerGroupId  consumerConfig)
                  topics
                  sessionTimeout
                  rebalanceTimeout
                  resetPolicy
                  assignor
                  consumerCorrelationId
                  fetchUD
      case result of
        Left err -> return $ Left ("subscribe: " ++ show err)
        Right tps -> do
          let !newAssignment =
                [ TopicPartition (Sub.tpTopic stp) (Sub.tpPartition stp)
                | (stp, _) <- tps
                ]
              !newAssignmentSorted =
                List.sortOn (\tp -> (tpTopic tp, tpPartition tp))
                            newAssignment
          -- Decide assigned-vs-revoked-vs-lost BEFORE we replace
          -- the assignment: 'hbLost' tells us if the previous
          -- assignment was fenced by the broker, in which case
          -- the removed half routes to 'rlOnLost' rather than
          -- 'rlOnRevoked'.
          asLost <- atomically $ do
            lost <- readTVar (HB.hbLost hbState)
            writeTVar (HB.hbLost hbState) False
            pure lost
          -- Replace the assignment with the new one, seed offsets,
          -- remember the subscription so 'poll' can transparently
          -- re-run the JoinGroup flow on rebalance.
          atomically $ do
            existing <- ListT.toList $ StmMap.listT consumerAssignment
            forM_ existing $ \(tp, _) -> StmMap.delete tp consumerAssignment
            forM_ tps $ \(stp, off) ->
              let tp = TopicPartition (Sub.tpTopic stp) (Sub.tpPartition stp)
              in StmMap.insert off tp consumerAssignment
            writeTVar consumerSubscription (Just topics)
            -- Heartbeat thread set this when a previous reply
            -- contained REBALANCE_IN_PROGRESS; clear it now that we
            -- have re-joined.
            writeTVar (HB.hbNeedsRebalance hbState) False
          -- Fire the user listener with the new assignment.
          dispatchAssignmentDelta Consumer{..} newAssignmentSorted asLost
          return $ Right ()

-- | Install user callbacks fired on assigned / revoked / lost
-- transitions. Mirrors Java's @ConsumerRebalanceListener@. The
-- listener record comes from
-- "Kafka.Client.RebalanceListener" — we accept three
-- callbacks directly to avoid a module-import cycle between
-- "Kafka.Client.Consumer" and "Kafka.Client.RebalanceListener"
-- (the latter already imports 'TopicPartition' from here).
--
-- Callback semantics:
--
--   * @onAssigned@ fires for every partition newly added to
--     this consumer's assignment.
--   * @onRevoked@ fires for partitions removed via a normal
--     (cooperative) rebalance — i.e. the broker accepted the
--     handoff and we should flush in-flight work, commit
--     offsets, etc.
--   * @onLost@ fires when the broker fenced us
--     (@UNKNOWN_MEMBER_ID@ / @FENCED_INSTANCE_ID@): any
--     in-flight state is junk because we may not commit
--     offsets to the broker any more.
setRebalanceListener
  :: Consumer
  -> ([TopicPartition] -> IO ())     -- ^ onAssigned
  -> ([TopicPartition] -> IO ())     -- ^ onRevoked
  -> ([TopicPartition] -> IO ())     -- ^ onLost
  -> IO ()
setRebalanceListener Consumer{..} onA onR onL = atomically $ do
  writeTVar consumerOnAssigned onA
  writeTVar consumerOnRevoked  onR
  writeTVar consumerOnLost     onL

-- | Current partition assignment, in deterministic order
-- (sorted by topic then partition).
currentAssignment :: Consumer -> IO [TopicPartition]
currentAssignment Consumer{..} = do
  pairs <- atomically $ ListT.toList (StmMap.listT consumerAssignment)
  pure (List.sortOn (\tp -> (tpTopic tp, tpPartition tp))
                    (map fst pairs))

-- | Pure delta computation between two partition assignments.
-- Returns @(revoked, added)@: revoked partitions are present
-- in @prev@ but absent from @now@; added partitions are in
-- @now@ but were not in @prev@. Both result lists are in
-- ascending @(topic, partition)@ order so consumers can rely
-- on deterministic ordering for log lines / metric labels.
computeAssignmentDelta
  :: [TopicPartition]        -- ^ previous assignment
  -> [TopicPartition]        -- ^ new assignment
  -> ([TopicPartition], [TopicPartition])
computeAssignmentDelta prev now =
  let !prevSet = Set.fromList prev
      !nowSet  = Set.fromList now
      !revoked = Set.toAscList (prevSet `Set.difference` nowSet)
      !added   = Set.toAscList (nowSet  `Set.difference` prevSet)
   in (revoked, added)

-- | Compute the assigned / revoked deltas between @prev@ and
-- @now@ and fire the appropriate user callbacks. Records the
-- new assignment as 'consumerLastAssignment' for the next
-- diff.
--
-- @asLost@ routes the "removed" side through 'consumerOnLost'
-- instead of 'consumerOnRevoked'. The runtime determines that
-- via the heartbeat's 'hbLost' flag, which is set by
-- 'Kafka.Client.Internal.Heartbeat.applyHeartbeatOutcome' on
-- @UNKNOWN_MEMBER_ID@ / @FENCED_INSTANCE_ID@.
dispatchAssignmentDelta
  :: Consumer
  -> [TopicPartition]                 -- ^ new assignment
  -> Bool                             -- ^ asLost
  -> IO ()
dispatchAssignmentDelta c@Consumer{..} now asLost = do
  prev <- atomically (readTVar consumerLastAssignment)
  let !(revoked, added) = computeAssignmentDelta prev now
  -- Persist before firing callbacks so that a callback that
  -- queries 'currentAssignment' sees the post-transition state.
  atomically $ writeTVar consumerLastAssignment now
  unless (null revoked) $ do
    if asLost
      then do
        onL <- atomically (readTVar consumerOnLost)
        catchIgnore (onL revoked)
      else do
        onR <- atomically (readTVar consumerOnRevoked)
        catchIgnore (onR revoked)
  unless (null added) $ do
    onA <- atomically (readTVar consumerOnAssigned)
    catchIgnore (onA added)
  where
    -- 'c' is only here to defeat the otherwise-unused warning
    -- for the record-wildcard binding above; the actual access
    -- happens through the TVars.
    _ = c
    catchIgnore m = do
      r <- try m :: IO (Either SomeException ())
      case r of
        Right () -> pure ()
        Left _   -> pure ()

-- | Unsubscribe from all topics.
--
-- Clears the subscription and partition assignment. Fires the
-- rebalance listener's @onRevoked@ callback for every
-- currently-assigned partition before clearing the assignment.
unsubscribe :: Consumer -> IO ()
unsubscribe c@Consumer{..} = do
  dispatchAssignmentDelta c [] False
  atomically $ do
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
--
-- = Auto-rebalance
--
-- If the heartbeat thread has flagged a pending rebalance
-- ('HB.hbNeedsRebalance' set by the broker's @REBALANCE_IN_PROGRESS@
-- (error code 27) on a heartbeat reply), 'poll' transparently re-runs
-- the JoinGroup \/ SyncGroup \/ OffsetFetch flow against the same
-- subscription before fetching. Callers do not need to remember to
-- call 'subscribe' again on rebalance — they only do that the first
-- time, to declare what topics they want.
poll
  :: Consumer
  -> Int  -- ^ Timeout in milliseconds
  -> IO (Either String [ConsumerRecord])
poll consumer@Consumer{..} timeoutMs = do
  -- Auto-rebalance: if the heartbeat thread saw REBALANCE_IN_PROGRESS
  -- and we know what topics we're subscribed to, re-join now.
  needsRejoin <- atomically $ do
    case consumerHeartbeat of
      Nothing -> pure False
      Just (hbSt, _) -> do
        flag <- readTVar (HB.hbNeedsRebalance hbSt)
        topicsM <- readTVar consumerSubscription
        pure (flag && case topicsM of Just _ -> True; Nothing -> False)
  rejoinR <- if needsRejoin
    then do
      topicsM <- readTVarIO consumerSubscription
      case topicsM of
        Just ts -> subscribe consumer ts
        Nothing -> pure (Right ())
    else pure (Right ())
  case rejoinR of
    Left err -> return (Left ("poll: rebalance rejoin failed: " <> err))
    Right () -> doPoll
  where
   doPoll = do
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

            -- Limit to maxPollRecords, then run user interceptor
            let maxRecords = consumerMaxPollRecords consumerConfig
                limitedRecords = take maxRecords records
            -- ConsumerInterceptor.onConsume (KIP-388 / JVM): an
            -- exception here propagates; tracing tools that just
            -- need to attach attributes shouldn't throw.
            iceptedRecords <- consumerInterceptor consumerConfig limitedRecords

            return $ Right iceptedRecords

-- | Seek to a specific offset on an /assigned/ partition. The
-- next 'poll' will re-fetch starting at @offset@. Mirrors
-- @KafkaConsumer.seek(tp, offset)@.
--
-- Returns 'Left' if the partition isn't currently assigned to
-- this consumer (the JVM client throws 'IllegalStateException'
-- in that case).
seek :: Consumer -> TopicPartition -> Int64 -> IO (Either String ())
seek Consumer{..} tp offset = atomically $ do
  m <- StmMap.lookup tp consumerAssignment
  case m of
    Nothing -> pure (Left ("seek: partition not assigned: "
                             ++ T.unpack (tpTopic tp)
                             ++ ":" ++ show (tpPartition tp)))
    Just _  -> do
      StmMap.insert offset tp consumerAssignment
      pure (Right ())

-- | Seek to the earliest available offset for each partition.
-- Mirrors @KafkaConsumer.seekToBeginning(partitions)@.
seekToBeginning :: Consumer -> [TopicPartition] -> IO (Either String ())
seekToBeginning consumer tps = seekToTimestamp consumer tps (-2)

-- | Seek to the latest available offset (i.e. the high water
-- mark). Mirrors @KafkaConsumer.seekToEnd(partitions)@.
seekToEnd :: Consumer -> [TopicPartition] -> IO (Either String ())
seekToEnd consumer tps = seekToTimestamp consumer tps (-1)

-- | Helper: query the broker for the offset at the supplied
-- timestamp (-1 = latest, -2 = earliest, otherwise milliseconds
-- since epoch) and seek every partition to it.
seekToTimestamp :: Consumer -> [TopicPartition] -> Int64 -> IO (Either String ())
seekToTimestamp _ [] _ = pure (Right ())
seekToTimestamp consumer@Consumer{..} tps timestamp = do
  r <- queryPartitionOffsets consumer tps timestamp
  case r of
    Left err -> pure (Left err)
    Right offsets -> atomically $ do
      mapM_ (\(tp, off) -> StmMap.insert off tp consumerAssignment) offsets
      pure (Right ())

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
    else do
      r <- commitOffsetsSync consumer (consumerGroupId consumerConfig) offsets
      case r of
        Right () -> dispatchOnCommit consumer offsets >> pure (Right ())
        Left e   -> pure (Left e)

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
      -- Fire-and-forget; the on-commit callback runs in the same
      -- async after the broker reply.
      _ <- async $ do
        r <- commitOffsetsSync consumer (consumerGroupId consumerConfig) offsets
        case r of
          Right () -> dispatchOnCommit consumer offsets
          Left _   -> pure ()
      return $ Right ()

-- | Best-effort dispatch of 'consumerOnCommit'. Wrapped in 'try'
-- so a buggy hook can't interfere with the commit pipeline.
dispatchOnCommit
  :: Consumer
  -> [(TopicPartition, Int64)]
  -> IO ()
dispatchOnCommit Consumer{..} offsets = do
  r <- try (consumerOnCommit consumerConfig offsets)
       :: IO (Either Control.Exception.SomeException ())
  case r of
    Right () -> pure ()
    Left  _  -> pure ()

-- | Get the committed offset for a single partition. Mirrors
-- @KafkaConsumer.committed(tp)@. Convenience wrapper around
-- 'committedAll'.
committed :: Consumer -> TopicPartition -> IO (Either String Int64)
committed consumer tp = do
  r <- committedAll consumer [tp]
  case r of
    Left err -> pure (Left err)
    Right hm -> case HashMap.lookup tp hm of
      Nothing  -> pure (Left ("committed: no offset returned for "
                                ++ T.unpack (tpTopic tp)
                                ++ ":" ++ show (tpPartition tp)))
      Just off -> pure (Right off)

-- | Fetch committed offsets for many partitions in one
-- broker round-trip. The Java client's
-- @KafkaConsumer.committed(Set\<TopicPartition\>)@ analogue.
--
-- Returns 'HashMap' keyed by 'TopicPartition' with the broker's
-- last committed offset; partitions with no committed offset are
-- absent from the result map (rather than aliased to 0 — that's
-- consistent with the JVM client semantics).
committedAll
  :: Consumer
  -> [TopicPartition]
  -> IO (Either String (HashMap.HashMap TopicPartition Int64))
committedAll _ [] = pure (Right HashMap.empty)
committedAll consumer@Consumer{..} tps =
  fetchCommittedOffsetsBatch consumer
    (consumerGroupId consumerConfig)
    tps

-- | Current consumer position (the offset of the next
-- record that will be returned by 'poll'). Read from the local
-- assignment map, /not/ the broker; this is what the JVM client's
-- @position(tp)@ returns.
position :: Consumer -> TopicPartition -> IO (Either String Int64)
position Consumer{..} tp = atomically $ do
  m <- StmMap.lookup tp consumerAssignment
  case m of
    Nothing  -> pure (Left ("position: partition not assigned: "
                              ++ T.unpack (tpTopic tp)
                              ++ ":" ++ show (tpPartition tp)))
    Just off -> pure (Right off)

-- | Query the earliest offset available for each
-- partition. Mirrors @KafkaConsumer.beginningOffsets(partitions)@.
beginningOffsets
  :: Consumer
  -> [TopicPartition]
  -> IO (Either String (HashMap.HashMap TopicPartition Int64))
beginningOffsets consumer tps = do
  r <- queryPartitionOffsets consumer tps (-2)  -- earliest
  pure (fmap HashMap.fromList r)

-- | Query the high-water-mark offset (i.e. one past the
-- last produced record) for each partition. Mirrors
-- @KafkaConsumer.endOffsets(partitions)@.
endOffsets
  :: Consumer
  -> [TopicPartition]
  -> IO (Either String (HashMap.HashMap TopicPartition Int64))
endOffsets consumer tps = do
  r <- queryPartitionOffsets consumer tps (-1)  -- latest
  pure (fmap HashMap.fromList r)

-- | For each partition, return the earliest offset whose
-- timestamp is greater than or equal to the supplied timestamp.
-- Partitions whose timestamp is past the broker's high water mark
-- are returned with an offset of @-1@.
offsetsForTimes
  :: Consumer
  -> [(TopicPartition, Int64)]      -- ^ (partition, target timestamp ms)
  -> IO (Either String (HashMap.HashMap TopicPartition Int64))
offsetsForTimes _ [] = pure (Right HashMap.empty)
offsetsForTimes consumer pts = do
  r <- offsetsForTimesFull consumer pts
  pure (fmap (HashMap.map oatOffset) r)

-- | The richer return type from 'offsetsForTimesFull'. Mirrors
-- the JVM client's @OffsetAndTimestamp@: on top of the offset,
-- the broker also returns the actual timestamp of the first
-- record at-or-after the requested timestamp (which can be
-- /later/ than the requested one if no record landed exactly on
-- it) and — from ListOffsets v4+ — the partition leader epoch
-- at the time, which callers should pass back into 'seek' /
-- 'commitSync' when they want fencing.
data OffsetAndTimestamp = OffsetAndTimestamp
  { oatOffset      :: !Int64
    -- ^ The offset of the first record at-or-after the
    --   requested timestamp. @-1@ means the broker had no record
    --   at-or-after the timestamp.
  , oatTimestamp   :: !Int64
    -- ^ The actual record timestamp (ms since epoch). May be
    --   greater than the requested timestamp. @-1@ on brokers
    --   that don't report it (very old / pre-v1) or when
    --   @oatOffset == -1@.
  , oatLeaderEpoch :: !Int32
    -- ^ The leader epoch at the time of the record. @-1@ on
    --   pre-v4 brokers that don't include the field. Useful for
    --   fencing on subsequent commit/seek.
  }
  deriving (Eq, Show, Generic)

-- | Like 'offsetsForTimes', but also returns the broker-reported
-- timestamp + leader epoch for each partition. Mirrors
-- @KafkaConsumer.offsetsForTimes@ in the JVM client which
-- returns @Map\<TopicPartition, OffsetAndTimestamp\>@.
--
-- Use this when the caller needs the leader-epoch fencing
-- semantics from KIP-320 (passing the returned epoch back into
-- @commitSync@ rejects commits across leadership changes); for
-- the simple offset-only case, 'offsetsForTimes' is the existing
-- (Int64-only) façade.
offsetsForTimesFull
  :: Consumer
  -> [(TopicPartition, Int64)]
  -> IO (Either String (HashMap.HashMap TopicPartition OffsetAndTimestamp))
offsetsForTimesFull _ [] = pure (Right HashMap.empty)
offsetsForTimesFull consumer pts = do
  r <- queryPartitionOffsetsByTimestampFull consumer pts
  pure (fmap HashMap.fromList r)

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

-- | List the partitions currently paused via 'pause'. Mirrors
-- @KafkaConsumer.paused()@.
paused :: Consumer -> IO [TopicPartition]
paused Consumer{..} = atomically $ do
  pairs <- ListT.toList $ StmMap.listT consumerPaused
  pure (map fst pairs)

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
      connResult <- consumerConnect consumer brokerAddr
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
                  
                  -- Fetch from each broker (KIP-392: pass rack ID).
                  -- Threads the configured isolation level through so
                  -- read-committed consumers actually filter aborted
                  -- transactions; the previous code hardcoded
                  -- READ_UNCOMMITTED (0) which made
                  -- 'consumerIsolationLevel' a no-op.
                  results <- forM (Map.toList byBroker) $ \(broker, reqs) ->
                    fetchFromBroker
                      consumerConnManager
                      consumerVersionCache
                      broker
                      reqs
                      timeoutMs
                      consumerCorrelationId
                      (consumerRackId consumerConfig)
                      (consumerConnConfig consumer)
                      (consumerIsolationLevel consumerConfig)
                  
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
  -> Conn.ConnectionConfig   -- ^ Connection / SASL config (re-used cached conns when matching)
  -> IsolationLevel          -- ^ KIP-98 isolation level (ReadUncommitted / ReadCommitted)
  -> IO (Either String [ConsumerRecord])
fetchFromBroker connMgr versionCache broker requests timeoutMs corrIdVar rackIdM connConfig isolationLevel = do
  let brokerAddr = Meta.brokerMetaAddress broker
      nextCid = atomically $ do
        cid <- readTVar corrIdVar
        writeTVar corrIdVar (cid + 1)
        pure cid
  connResult <- Conn.getOrCreateConnection connMgr brokerAddr connConfig
  case connResult of
    Left err -> return $ Left $ "Failed to connect: " ++ err
    Right conn -> do
      -- Negotiate ApiVersions if we haven't already; idempotent.
      _ <- VN.ensureVersionsNegotiated conn brokerAddr versionCache nextCid
      corrId <- nextCid
      let apiKey = 1  -- Fetch
      -- FetchRequest body-level changes by version:
      --   v4  KIP-98  IsolationLevel
      --   v7  KIP-227 SessionId / SessionEpoch
      --   v11 KIP-392 RackId; KIP-573 ClusterId
      --   v12 went flexible (compact strings + tagged fields).
      --       The ClusterId field at v12 is a /tagged/
      --       compact-string with a null default; the codegen
      --       was emitting it with the non-compact serializer
      --       inside the tagged-fields envelope which the
      --       broker rejected with an EOF — fixed in this
      --       commit's codegen tagged-field-encoder change
      --       (Generator.generateTaggedFieldEncoder now uses
      --       toCompactString for tagged-string payloads).
      --   v13 moved to TopicId-based identification — needs
      --       the topic-id metadata cache plumbing that's
      --       still TODO; cap at v12 until then.
      --   v15 ReplicaState moved into a tagged field
      --   v17 ReplicaDirectoryId added (tagged)
      -- Codegen handles up to v17 against the upstream 4.0.0
      -- schemas. The api key + cache lookup come from the
      -- 'KafkaMessage FR.FetchRequest' instance via
      -- 'pickApiVersionForRange'; the (4, 12) override caps
      -- below the codegen max.
      verR <- VN.pickApiVersionForRange @FR.FetchRequest
                4 12 versionCache brokerAddr 4
      let apiVersion = case verR of
            Right v -> v
            Left  _ -> 4   -- min supported (matches legacy fallback)
      
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
                        -- KIP-915 ReplicaDirectoryId (tagged v17+).
                        -- The wire encoding writes this only on
                        -- v17+ and only inside the TaggedFields
                        -- envelope; non-replica consumers always
                        -- set nullUuid which is the broker's
                        -- "no specific directory" sentinel.
                        , FR.fetchPartitionReplicaDirectoryId = P.nullUuid
                        , FR.fetchPartitionHighWatermark       = -1
                        }
                    | (partId, offset) <- parts
                    ]
                }
            | (topic, parts) <- Map.toList byTopic
            ]
          
          -- KIP-392: Use rack ID for rack-aware fetching if
          -- configured. The field is non-nullable in the upstream
          -- spec (default ""), so a 'Nothing' from the consumer
          -- config sends the empty string — sending 'Null'
          -- (length=-1) breaks the broker's parser at v11+.
          rackIdKafka = case rackIdM of
            Nothing     -> P.mkKafkaString ""
            Just rackId -> P.mkKafkaString rackId
          
          request = FR.FetchRequest
            { FR.fetchRequestReplicaId = -1  -- Consumer (not a replica)
            , FR.fetchRequestMaxWaitMs = fromIntegral timeoutMs
            , FR.fetchRequestMinBytes = 1
            , FR.fetchRequestMaxBytes = 52428800  -- 50MB total
            , FR.fetchRequestIsolationLevel = case isolationLevel of
                ReadUncommitted -> 0
                ReadCommitted   -> 1
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
          
          requestBody = WC.runEncodeVer @FR.FetchRequest apiVersion request
          clientId = P.mkKafkaString "kafka-native-consumer"
      
      result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock connMgr brokerAddr) conn apiKey apiVersion corrId clientId requestBody

      case result of
        Left err -> return $ Left $ "Fetch failed: " ++ err
        Right (_, responseBody) -> do
          case WC.runDecodeVer @FResp.FetchResponse apiVersion responseBody of
            Left err -> return $ Left $ "Failed to parse FetchResponse: " ++ err
            Right response ->
              extractRecordsFromFetchResponse isolationLevel response

-- | Extract records from a FetchResponse.
--
-- Filters batches the consumer should never surface to user code:
--
--   * /Control batches/ ('attrIsControl' = True) carry transaction
--     commit / abort markers. They live in the log alongside data
--     records but are bookkeeping for the read-committed reader,
--     not application data.
--
--   * /Aborted transactional batches/ when 'IsolationLevel' is
--     'ReadCommitted'. The broker tells us which transactions on
--     this partition aborted via @PartitionData.abortedTransactions@
--     (a list of @(producerId, firstOffset)@ pairs); we walk the
--     decoded batches and skip any transactional batch whose
--     producer id matches an entry whose 'firstOffset' is at or
--     before the batch's 'baseOffset'. A producer id can have
--     several aborted transactions in flight, so we use the
--     /smallest/ first-offset per producer id and skip everything
--     past that until we hit a control batch (which the broker
--     wrote at commit/abort time and we then drop too). This
--     mirrors what the JVM client does inside
--     @Fetcher.completedFetch.nextFetchedRecord@.
--
--   * /Aborted transactional batches/ on 'ReadUncommitted' are
--     /not/ filtered — the whole point of 'ReadUncommitted' is to
--     see records as soon as they're written, regardless of
--     whether the txn ultimately commits.
--
-- The previous code surfaced commit/abort markers (raw
-- @\\NUL\\NUL\\NUL\\NUL\\NUL\\NUL@-shaped values) as user records,
-- which broke 'ReadCommitted' end-to-end and surprised
-- 'ReadUncommitted' callers with garbage payloads.
extractRecordsFromFetchResponse
  :: IsolationLevel
  -> FResp.FetchResponse
  -> IO (Either String [ConsumerRecord])
extractRecordsFromFetchResponse isolationLevel response = do
  let topicsVec = case P.unKafkaArray (FResp.fetchResponseResponses response) of
        P.Null -> V.empty
        P.NotNull v -> v
  -- Walk topics with V.foldM' so we don't build an intermediate
  -- list of '[[ConsumerRecord]]' chunks; per-iteration work is
  -- in 'IO' because 'decodeAllBatches' may decompress.
  go (V.length topicsVec) topicsVec
  where
    go 0 _ = pure (Right [])
    go _ topicsVec = do
      r <- V.foldM' addTopic (Right []) topicsVec
      case r of
        Left e   -> pure (Left e)
        Right vs -> pure (Right (V.toList (V.concat (reverse vs))))

    addTopic
      :: Either String [V.Vector ConsumerRecord]
      -> FResp.FetchableTopicResponse
      -> IO (Either String [V.Vector ConsumerRecord])
    addTopic acc topicResp = case acc of
      Left e -> pure (Left e)
      Right chunks -> do
        let topicName = extractKafkaString $ FResp.fetchableTopicResponseTopic topicResp
            partitionsVec =
              case P.unKafkaArray (FResp.fetchableTopicResponsePartitions topicResp) of
                P.Null -> V.empty
                P.NotNull v -> v
        partRes <- V.foldM' (addPartition topicName) (Right V.empty) partitionsVec
        case partRes of
          Left e   -> pure (Left e)
          Right pv -> pure (Right (pv : chunks))

    addPartition
      :: Text
      -> Either String (V.Vector ConsumerRecord)
      -> FResp.PartitionData
      -> IO (Either String (V.Vector ConsumerRecord))
    addPartition topicName acc partResp = case acc of
      Left e -> pure (Left e)
      Right partial -> do
        let partId = FResp.partitionDataPartitionIndex partResp
            errorCode = FResp.partitionDataErrorCode partResp
            recordsBytes =
              case P.unKafkaBytes (FResp.partitionDataRecords partResp) of
                P.Null -> BS.empty
                P.NotNull bs -> bs
            -- Pre-build a HashMap (producerId -> minimum firstOffset)
            -- of aborted transactions on this partition. Skipped
            -- entirely on ReadUncommitted (the map is only ever
            -- consulted in the ReadCommitted branch of
            -- 'keepBatch').
            abortedByProducer = case isolationLevel of
              ReadUncommitted -> HashMap.empty
              ReadCommitted   ->
                let abortsVec =
                      case P.unKafkaArray
                             (FResp.partitionDataAbortedTransactions partResp) of
                        P.Null      -> V.empty
                        P.NotNull v -> v
                in V.foldl'
                     (\m at ->
                        let pid = FResp.abortedTransactionProducerId at
                            off = FResp.abortedTransactionFirstOffset at
                        in HashMap.insertWith min pid off m)
                     HashMap.empty abortsVec
        if errorCode /= 0
          then pure (Left ("Fetch error for " ++ T.unpack topicName
                            ++ ":" ++ show partId ++ " code=" ++ show errorCode))
          else if BS.null recordsBytes
                 then pure (Right partial)
                 else do
                   -- Sliced decoder: keys / values / headers stay
                   -- as 'SliceVector' views over the source buffer
                   -- through 'ConsumerRecord' construction. See
                   -- 'Kafka.Protocol.RecordBatchWire' for the
                   -- SlicedRecordBatch design.
                   batches <- decodeAllBatchesSliced recordsBytes
                   case batches of
                     Left err -> pure (Left ("Failed to decode batches: " ++ err))
                     Right bs ->
                       let !kept = filter (keepSlicedBatch abortedByProducer) bs
                           !v = V.concat
                                  (map (convertSlicedBatchToRecordsV topicName partId) kept)
                       in pure (Right (partial V.++ v))

    -- Sliced-batch variant of 'keepBatch'. See the haddock at
    -- the top of the function for the rule set.
    keepSlicedBatch
      :: HashMap.HashMap Int64 Int64 -> RBW.SlicedRecordBatch -> Bool
    keepSlicedBatch abortedByProducer batch =
      let attrs = RBW.sbAttributes batch
      in not (RB.attrIsControl attrs)
         && (case isolationLevel of
               ReadUncommitted -> True
               ReadCommitted   ->
                 not (RB.attrIsTransactional attrs)
                 || case HashMap.lookup
                          (RBW.sbProducerId batch) abortedByProducer of
                      Nothing          -> True
                      Just firstOffset ->
                        RBW.sbBaseOffset batch < firstOffset)

-- | 'Vector'-returning sibling of 'convertBatchToRecords'.
convertBatchToRecordsV :: Text -> Int32 -> RB.RecordBatch -> V.Vector ConsumerRecord
convertBatchToRecordsV topic partId batch =
  let baseOffset    = RB.batchBaseOffset batch
      baseTimestamp = RB.batchBaseTimestamp batch
  in V.map
       (\rec -> ConsumerRecord
          { crTopic     = topic
          , crPartition = partId
          , crOffset    = baseOffset + fromIntegral (RB.recordOffsetDelta rec)
          , crTimestamp = baseTimestamp + RB.recordTimestampDelta rec
          , crKey       = RB.recordKey rec
          , crValue     = RB.recordValue rec
          , crHeaders   = convertHeaders (RB.recordHeaders rec)
          })
       (RB.batchRecords batch)

-- | Materialise a 'V.Vector ConsumerRecord' directly from the
-- sliced-batch shape. Skips the per-record 'RB.Record' /
-- 'RB.RecordHeader' allocations the 'V.Vector Record' path
-- would have produced; the per-record key + value
-- 'BS.ByteString's are still zero-copy slices of the source
-- buffer (the 'SliceVector' just collapses N independent
-- 'ForeignPtr' GC roots into one).
convertSlicedBatchToRecordsV
  :: Text -> Int32 -> RBW.SlicedRecordBatch -> V.Vector ConsumerRecord
convertSlicedBatchToRecordsV topic partId sb =
  let !n = RBW.slicedRecordCount sb
  in V.generate n $ \i -> ConsumerRecord
       { crTopic     = topic
       , crPartition = partId
       , crOffset    = RBW.slicedRecordOffset    sb i
       , crTimestamp = RBW.slicedRecordTimestamp sb i
       , crKey       = RBW.slicedRecordKey       sb i
       , crValue     = RBW.slicedRecordValue     sb i
       , crHeaders   = convertSlicedHeaders sb i
       }

-- | Pull the i-th record's headers out of a 'SlicedRecordBatch'
-- in the public @[(Text, ByteString)]@ shape. Mirrors
-- 'convertHeaders' (UTF-8 decode the key, drop null values).
convertSlicedHeaders
  :: RBW.SlicedRecordBatch -> Int -> [(Text, ByteString)]
convertSlicedHeaders sb i =
  let !cnt = RBW.slicedRecordHeaderCount sb i
      go !j acc
        | j < 0     = acc
        | otherwise =
            let (kBs, mvBs) = RBW.slicedRecordHeader sb i j
            in case mvBs of
                 Nothing -> go (j - 1) acc
                 Just vBs -> go (j - 1) ((TE.decodeUtf8 kBs, vBs) : acc)
  in go (cnt - 1) []

-- | Extract Text from KafkaString
extractKafkaString :: P.KafkaString -> Text
extractKafkaString ks = case P.unKafkaString ks of
  P.Null -> T.empty
  P.NotNull t -> t

-- | Decode all RecordBatches from a ByteString.
decodeAllBatches :: ByteString -> IO (Either String [RB.RecordBatch])
decodeAllBatches input = go input []
  where
    -- Tail-recursive accumulator so we don't pay 'cons : decode'
    -- per batch. Result list is reversed at the end.
    go bs acc
      | BS.null bs = pure (Right (reverse acc))
      | otherwise =
          -- Check the compression bit on the batch attributes
          -- without decoding the full batch first; we know the
          -- bit's offset (21 + 0 -> the first byte of attrs).
          -- The cheap path: try the Wire decoder; if it returns
          -- a NoCompression batch, use it; otherwise fall back to
          -- the IO-decompressing path.
          case RBW.decodeRecordBatchWire bs of
            Right batch
              | RB.attrCompressionType (RB.batchAttributes batch)
                  == Compression.NoCompression -> do
                  let batchSize = 8 + 4 + fromIntegral (calculateBatchLength batch)
                      remaining = BS.drop batchSize bs
                  go remaining (batch : acc)
            _ -> do
              -- Either the Wire decoder errored (truncated input,
              -- bad CRC, …) or the batch is compressed; fall back
              -- to the IO decoder which handles both cases. The
              -- IO version is the Wire-shaped decompressing
              -- decoder ('decodeRecordBatchWireWithDecompression') —
              -- byte-identical with the pure 'decodeRecordBatchWire'
              -- when the batch is uncompressed; for compressed
              -- batches it slices the records section, decompresses,
              -- and re-decodes via the same Wire pokes.
              result <- RBW.decodeRecordBatchWireWithDecompression bs
              case result of
                Left err -> pure (Left err)
                Right batch -> do
                  let batchSize = 8 + 4 + fromIntegral (calculateBatchLength batch)
                      remaining = BS.drop batchSize bs
                  go remaining (batch : acc)

-- | Calculate the length field value for a batch (everything after the length field)
calculateBatchLength :: RB.RecordBatch -> Int32
calculateBatchLength batch =
  let encoded = RBW.encodeRecordBatchWire batch
      -- Skip base offset (8 bytes) to get to length field
      lengthBytes = BS.take 4 $ BS.drop 8 encoded
  in case W.readInt32BE lengthBytes of
      Left _ -> 0
      Right len -> len

-- | Sliced-shape sibling of 'decodeAllBatches'. Walks the
-- concatenated record-batches buffer once, materialising each
-- batch as a 'SlicedRecordBatch' (memory-efficient flat slice
-- vectors over the source 'ForeignPtr', or over a fresh
-- decompressed buffer for compressed batches).
--
-- The advance-cursor step uses the on-the-wire @length@ field
-- (peeked directly via 'W.readInt32BE') so we don't have to
-- re-encode the batch to learn how many bytes it occupies.
decodeAllBatchesSliced
  :: ByteString -> IO (Either String [RBW.SlicedRecordBatch])
decodeAllBatchesSliced input = go input []
  where
    go bs acc
      | BS.null bs = pure (Right (reverse acc))
      | otherwise = do
          -- Peek the on-the-wire length field at offset 8
          -- (right after baseOffset). Handles both
          -- compressed and uncompressed shapes since the
          -- envelope is identical.
          let !lengthBytes = BS.take 4 (BS.drop 8 bs)
          case W.readInt32BE lengthBytes of
            Left err -> pure (Left ("decodeAllBatchesSliced: bad length: " ++ err))
            Right batchLen -> do
              let !batchSize = 8 + 4 + fromIntegral batchLen :: Int
                  !batchBs   = BS.take batchSize bs
              r <- RBW.decodeRecordBatchWireSlicedWithDecompression batchBs
              case r of
                Left e -> pure (Left e)
                Right sb -> do
                  let !remaining = BS.drop batchSize bs
                  go remaining (sb : acc)

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
commitOffsetsSync consumer@Consumer{..} groupId offsets = do
  -- The heartbeat thread tracks the group coordinator, generation
  -- id, and member id; we read all three off it. If the consumer
  -- isn't in a group (no heartbeat thread), commits aren't
  -- supported — the JVM client behaves the same.
  case consumerHeartbeat of
    Nothing -> return $ Left "Not in a consumer group, cannot commit offsets"
    Just (hbState, _) -> do
      coordAddrM <- atomically $ readTVar (HB.hbCoordinatorAddr hbState)
      genId      <- readTVarIO (HB.hbGenerationId hbState)
      memberId   <- readTVarIO (HB.hbMemberId    hbState)
      case coordAddrM of
        Nothing -> return $ Left "No group coordinator known"
        Just coordAddr -> do
          connResult <- consumerConnect consumer coordAddr
          case connResult of
            Left err -> return $ Left $ "Failed to connect to coordinator: " ++ err
            Right conn -> do
              corrId <- atomically $ do
                cid <- readTVar consumerCorrelationId
                writeTVar consumerCorrelationId (cid + 1)
                return cid
              let apiKey = 8  -- OffsetCommit
              -- OffsetCommit: codegen handles up to v10. The
              -- consumer's commitSync path is identical in
              -- request shape from v0 through v8; v9+ moved to
              -- the KIP-848 member-epoch shape but the broker
              -- still accepts the legacy generation/member-id
              -- pair we send through v9. v10's KIP-1043 added
              -- per-topic 'topicId' (we send nullUuid =
              -- name-based lookup). v8 added 'topicId' which we
              -- already supply as nullUuid.
              verR <- VN.pickApiVersionForRange @OCReq.OffsetCommitRequest
                        0 9 consumerVersionCache coordAddr 5
              let apiVersion = case verR of
                    Right v -> v
                    Left  _ -> 5
              
              -- Group offsets by topic
              let byTopic = Map.fromListWith (++)
                    [ (tpTopic tp, [(tpPartition tp, offset)])
                    | (tp, offset) <- offsets
                    ]
                  
                  topics = V.fromList
                    [ OCReq.OffsetCommitRequestTopic
                        { OCReq.offsetCommitRequestTopicName    = P.mkKafkaString topic
                        , -- KIP-848 (v10+) topic id is required to
                          -- be present on the wire but is ignored
                          -- by older brokers; nullUuid is the
                          -- "I don't know the topic id" sentinel.
                          OCReq.offsetCommitRequestTopicTopicId = P.nullUuid
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
                  
                  -- KIP-345: surface a static @group.instance.id@
                  -- if the user configured one; otherwise leave
                  -- it null (the broker treats that as a dynamic
                  -- member).
                  groupInstanceField =
                    case consumerGroupInstanceId consumerConfig of
                      Nothing  -> P.KafkaString P.Null
                      Just gii -> P.mkKafkaString gii
                  request = OCReq.OffsetCommitRequest
                    { OCReq.offsetCommitRequestGroupId = P.mkKafkaString groupId
                    , OCReq.offsetCommitRequestGenerationIdOrMemberEpoch = genId
                    , OCReq.offsetCommitRequestMemberId = P.mkKafkaString memberId
                    , OCReq.offsetCommitRequestGroupInstanceId = groupInstanceField
                    , OCReq.offsetCommitRequestRetentionTimeMs = -1  -- Use broker default
                    , OCReq.offsetCommitRequestTopics = P.mkKafkaArray topics
                    }
                  
                  requestBody = WC.runEncodeVer @OCReq.OffsetCommitRequest apiVersion request
                  clientId = P.mkKafkaString (consumerClientId consumerConfig)

              result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock consumerConnManager coordAddr) conn apiKey apiVersion corrId clientId requestBody

              case result of
                Left err -> return $ Left $ "OffsetCommit failed: " ++ err
                Right (_, responseBody) -> do
                  case WC.runDecodeVer @OCResp.OffsetCommitResponse apiVersion responseBody of
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

----------------------------------------------------------------------
-- Multi-partition committed offsets (KIP-211)
----------------------------------------------------------------------

-- | Batch sibling of 'fetchCommittedOffsets' that returns every
-- partition's committed offset (rather than only the first one).
-- Used by the public 'committedAll' helper.
fetchCommittedOffsetsBatch
  :: Consumer
  -> Text                                -- ^ Group ID
  -> [TopicPartition]
  -> IO (Either String (HashMap.HashMap TopicPartition Int64))
fetchCommittedOffsetsBatch consumer@Consumer{..} groupId tps = do
  case consumerHeartbeat of
    Nothing -> pure (Left "Not in a consumer group, cannot fetch committed offsets")
    Just (hbState, _) -> do
      coordAddrM <- atomically $ readTVar (HB.hbCoordinatorAddr hbState)
      case coordAddrM of
        Nothing -> pure (Left "No group coordinator known")
        Just coordAddr -> do
          connResult <- consumerConnect consumer coordAddr
          case connResult of
            Left err -> pure (Left ("Failed to connect to coordinator: " ++ err))
            Right conn -> do
              corrId <- atomically $ do
                cid <- readTVar consumerCorrelationId
                writeTVar consumerCorrelationId (cid + 1)
                pure cid
              let apiKey = 9  -- OffsetFetch
              -- OffsetFetch: v8 introduced the per-group batched
              -- shape (KIP-709). The wire is incompatible: v0-v7
              -- carry a single (group_id, topics[]) pair at the
              -- top level; v8+ carries an array of
              -- (group_id, topics[]) and the legacy field is
              -- removed. We dispatch on the negotiated version
              -- below. v9 (KIP-848 member-epoch) is also accepted
              -- here — sending memberId="" + memberEpoch=-1
              -- selects the legacy classic-protocol shape on the
              -- broker side, which mirrors what the JVM client
              -- does when it isn't using the KIP-848 consumer
              -- protocol.
              verR <- VN.pickApiVersionForRange @OFReq.OffsetFetchRequest
                        0 8 consumerVersionCache coordAddr 5
              let apiVersion = case verR of
                    Right v -> v
                    Left  _ -> 5
              let byTopic = Map.fromListWith (++)
                    [ (tpTopic tp, [tpPartition tp]) | tp <- tps ]
                  request
                    | apiVersion >= 8 = buildOffsetFetchRequestV8 groupId byTopic
                    | otherwise       = buildOffsetFetchRequestLegacy groupId byTopic
                  requestBody = WC.runEncodeVer @OFReq.OffsetFetchRequest apiVersion request
                  clientId = P.mkKafkaString (consumerClientId consumerConfig)
              result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock consumerConnManager coordAddr) conn apiKey apiVersion corrId clientId requestBody
              case result of
                Left err -> pure (Left ("OffsetFetch failed: " ++ err))
                Right (_, responseBody) ->
                  case WC.runDecodeVer @OFResp.OffsetFetchResponse apiVersion responseBody of
                    Left err -> pure (Left ("Failed to parse OffsetFetchResponse: " ++ err))
                    Right response
                      | apiVersion >= 8 -> pure $ Right $! parseOffsetFetchResponseV8 response
                      | otherwise       -> pure $ Right $! parseOffsetFetchResponseLegacy response

-- | Build an OffsetFetchRequest in the legacy single-group shape
-- (v0-v7). The (group_id, topics[]) pair lives at the top level
-- and the v8+ groups[] array is left empty.
buildOffsetFetchRequestLegacy
  :: Text
  -> Map.Map Text [Int32]   -- ^ partitions grouped by topic
  -> OFReq.OffsetFetchRequest
buildOffsetFetchRequestLegacy groupId byTopic =
  let topicsVec = V.fromList
        [ OFReq.OffsetFetchRequestTopic
            { OFReq.offsetFetchRequestTopicName = P.mkKafkaString topic
            , OFReq.offsetFetchRequestTopicPartitionIndexes =
                P.mkKafkaArray (V.fromList parts)
            }
        | (topic, parts) <- Map.toList byTopic
        ]
   in OFReq.OffsetFetchRequest
        { OFReq.offsetFetchRequestGroupId = P.mkKafkaString groupId
        , OFReq.offsetFetchRequestTopics  = P.mkKafkaArray topicsVec
        , OFReq.offsetFetchRequestGroups  = P.mkKafkaArray V.empty
        , OFReq.offsetFetchRequestRequireStable = False
        }

-- | Build an OffsetFetchRequest in the v8+ per-group batched
-- shape (KIP-709). The legacy top-level (group_id, topics[])
-- pair is left empty (the codegen drops it on v8+ anyway) and
-- everything goes into the groups[] array.
--
-- We always send a single group (the consumer's own group) so
-- the array has one element. The batched shape lets one client
-- fetch offsets for many groups in a single round-trip; we don't
-- expose that publicly yet, but the wire format is what the
-- broker expects on v8+ regardless.
--
-- For v9 (KIP-848) we leave memberId="" and memberEpoch=-1 which
-- the broker treats as the legacy classic-protocol shape — i.e.
-- mirrors the JVM client when it isn't using the new
-- consumer-group protocol.
buildOffsetFetchRequestV8
  :: Text
  -> Map.Map Text [Int32]
  -> OFReq.OffsetFetchRequest
buildOffsetFetchRequestV8 groupId byTopic =
  let topicsVec = V.fromList
        [ OFReq.OffsetFetchRequestTopics
            { OFReq.offsetFetchRequestTopicsName    = P.mkKafkaString topic
            , -- KIP-848 (v10+) topic id; nullUuid until the broker
              -- starts populating topic ids on the metadata side.
              OFReq.offsetFetchRequestTopicsTopicId = P.nullUuid
            , OFReq.offsetFetchRequestTopicsPartitionIndexes =
                P.mkKafkaArray (V.fromList parts)
            }
        | (topic, parts) <- Map.toList byTopic
        ]
      groupEntry = OFReq.OffsetFetchRequestGroup
        { OFReq.offsetFetchRequestGroupGroupId     = P.mkKafkaString groupId
        , OFReq.offsetFetchRequestGroupMemberId    = P.mkKafkaString ""
        , OFReq.offsetFetchRequestGroupMemberEpoch = -1
        , OFReq.offsetFetchRequestGroupTopics      = P.mkKafkaArray topicsVec
        }
   in OFReq.OffsetFetchRequest
        { OFReq.offsetFetchRequestGroupId =
            -- Legacy field: drop on v8+ (the encoder ignores it
            -- anyway). 'KafkaString Null' avoids accidentally
            -- shipping any bytes if the encoder ever changes.
            P.KafkaString P.Null
        , OFReq.offsetFetchRequestTopics  = P.KafkaArray P.Null
        , OFReq.offsetFetchRequestGroups  = P.mkKafkaArray (V.singleton groupEntry)
        , OFReq.offsetFetchRequestRequireStable = False
        }

-- | Parse a legacy (v0-v7) OffsetFetchResponse into a
-- @TopicPartition -> committed offset@ map. Drops partitions
-- that errored or had no committed offset (offset == -1), which
-- mirrors the JVM client's @committed()@ semantics.
parseOffsetFetchResponseLegacy
  :: OFResp.OffsetFetchResponse
  -> HashMap.HashMap TopicPartition Int64
parseOffsetFetchResponseLegacy resp =
  let topicsVec =
        case P.unKafkaArray (OFResp.offsetFetchResponseTopics resp) of
          P.Null      -> V.empty
          P.NotNull v -> v
      addTopic !acc tr =
        let topic = extractKafkaString (OFResp.offsetFetchResponseTopicName tr)
            partsVec =
              case P.unKafkaArray (OFResp.offsetFetchResponseTopicPartitions tr) of
                P.Null      -> V.empty
                P.NotNull v -> v
         in V.foldl'
              (\m p ->
                 let pid = OFResp.offsetFetchResponsePartitionPartitionIndex p
                     ec  = OFResp.offsetFetchResponsePartitionErrorCode p
                     off = OFResp.offsetFetchResponsePartitionCommittedOffset p
                  in if ec == 0 && off >= 0
                       then HashMap.insert (TopicPartition topic pid) off m
                       else m)
              acc partsVec
   in V.foldl' addTopic HashMap.empty topicsVec

-- | Parse a v8+ OffsetFetchResponse (the per-group batched
-- shape). We only ever request a single group so we walk the
-- groups[] array and merge any successful entries; in practice
-- the array is always length 1.
parseOffsetFetchResponseV8
  :: OFResp.OffsetFetchResponse
  -> HashMap.HashMap TopicPartition Int64
parseOffsetFetchResponseV8 resp =
  let groupsVec =
        case P.unKafkaArray (OFResp.offsetFetchResponseGroups resp) of
          P.Null      -> V.empty
          P.NotNull v -> v
      addGroup !acc gr =
        -- A non-zero group-level error means /none/ of the
        -- group's offsets are valid; drop them.
        if OFResp.offsetFetchResponseGroupErrorCode gr /= 0
          then acc
          else
            let topicsVec =
                  case P.unKafkaArray (OFResp.offsetFetchResponseGroupTopics gr) of
                    P.Null      -> V.empty
                    P.NotNull v -> v
             in V.foldl' addTopic acc topicsVec
      addTopic !acc tr =
        let topic = extractKafkaString (OFResp.offsetFetchResponseTopicsName tr)
            partsVec =
              case P.unKafkaArray (OFResp.offsetFetchResponseTopicsPartitions tr) of
                P.Null      -> V.empty
                P.NotNull v -> v
         in V.foldl'
              (\m p ->
                 let pid = OFResp.offsetFetchResponsePartitionsPartitionIndex p
                     ec  = OFResp.offsetFetchResponsePartitionsErrorCode p
                     off = OFResp.offsetFetchResponsePartitionsCommittedOffset p
                  in if ec == 0 && off >= 0
                       then HashMap.insert (TopicPartition topic pid) off m
                       else m)
              acc partsVec
   in V.foldl' addGroup HashMap.empty groupsVec

----------------------------------------------------------------------
-- Per-partition timestamp -> offset (KIP-79)
----------------------------------------------------------------------

-- | Variant of 'queryPartitionOffsets' that returns
-- the full @OffsetAndTimestamp@ tuple (offset + timestamp +
-- leader epoch). The offset-only helper and 'offsetsForTimesFull'
-- both delegate here so the wire round-trip only happens once
-- per call.
queryPartitionOffsetsByTimestampFull
  :: Consumer
  -> [(TopicPartition, Int64)]
  -> IO (Either String [(TopicPartition, OffsetAndTimestamp)])
queryPartitionOffsetsByTimestampFull consumer@Consumer{..} pts = do
  -- Group partitions by topic, carrying per-partition timestamp.
  let byTopic = Map.fromListWith (++)
        [ (tpTopic tp, [(tpPartition tp, ts)])
        | (tp, ts) <- pts
        ]
  brokersM <- atomically $ Meta.getAllBrokers consumerMetadata
  case brokersM of
    Nothing      -> pure (Left "No brokers available in metadata cache")
    Just []      -> pure (Left "No brokers available in metadata cache")
    Just (broker:_) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
      connResult <- consumerConnect consumer brokerAddr
      case connResult of
        Left err -> pure (Left ("Failed to connect to broker: " ++ err))
        Right conn -> do
          corrId <- atomically $ do
            cid <- readTVar consumerCorrelationId
            writeTVar consumerCorrelationId (cid + 1)
            pure cid
          let apiKey = 2  -- ListOffsets
          -- See 'queryPartitionOffsets' for the v8 cap rationale.
          verR <- VN.pickApiVersionForRange @LOReq.ListOffsetsRequest
                    0 8 consumerVersionCache brokerAddr 1
          let apiVersion = case verR of
                Right v -> v
                Left  _ -> 1
              topics = V.fromList $ map buildTopicReq (Map.toList byTopic)
              buildTopicReq (topic, parts) =
                LOReq.ListOffsetsTopic
                  { LOReq.listOffsetsTopicName = P.mkKafkaString topic
                  , LOReq.listOffsetsTopicPartitions = P.mkKafkaArray $ V.fromList $
                      map (\(pid, ts) ->
                            LOReq.ListOffsetsPartition
                              { LOReq.listOffsetsPartitionPartitionIndex = pid
                              , LOReq.listOffsetsPartitionCurrentLeaderEpoch = -1
                              , LOReq.listOffsetsPartitionTimestamp = ts
                              }) parts
                  }
              request = LOReq.ListOffsetsRequest
                { LOReq.listOffsetsRequestReplicaId = -1
                , LOReq.listOffsetsRequestIsolationLevel = 0
                , LOReq.listOffsetsRequestTopics = P.mkKafkaArray topics
                , LOReq.listOffsetsRequestTimeoutMs = 30000  -- v10+; ignored otherwise
                }
              requestBody = WC.runEncodeVer @LOReq.ListOffsetsRequest apiVersion request
              clientId    = P.mkKafkaString (consumerClientId consumerConfig)
          result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock consumerConnManager brokerAddr) conn apiKey apiVersion corrId clientId requestBody
          case result of
            Left err -> pure (Left err)
            Right (rcid, body)
              | rcid /= corrId -> pure (Left "Correlation ID mismatch")
              | otherwise -> case WC.runDecodeVer @LOResp.ListOffsetsResponse apiVersion body of
                  Left err  -> pure (Left ("Failed to decode ListOffsets response: " ++ err))
                  Right resp -> pure (Right (extract resp))
  where
    extract resp = case P.unKafkaArray (LOResp.listOffsetsResponseTopics resp) of
      P.Null         -> []
      P.NotNull tvec ->
        concatMap
          (\tr ->
             let topic = case P.unKafkaString (LOResp.listOffsetsTopicResponseName tr) of
                   P.Null      -> ""
                   P.NotNull t -> t
                 partsVec = case P.unKafkaArray (LOResp.listOffsetsTopicResponsePartitions tr) of
                   P.Null      -> V.empty
                   P.NotNull v -> v
             in mapMaybe
                  (\pr ->
                     let pid = LOResp.listOffsetsPartitionResponsePartitionIndex pr
                         ec  = LOResp.listOffsetsPartitionResponseErrorCode pr
                         off = LOResp.listOffsetsPartitionResponseOffset pr
                         ts  = LOResp.listOffsetsPartitionResponseTimestamp pr
                         lep = LOResp.listOffsetsPartitionResponseLeaderEpoch pr
                     in if ec == 0
                          then Just ( TopicPartition topic pid
                                    , OffsetAndTimestamp
                                        { oatOffset      = off
                                        , oatTimestamp   = ts
                                        , oatLeaderEpoch = lep
                                        })
                          else Nothing)
                  (V.toList partsVec))
          (V.toList tvec)
