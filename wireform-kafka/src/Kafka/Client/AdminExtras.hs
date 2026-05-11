{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.AdminExtras
Description : AdminClient ergonomics: defaults, pluggable SSL /
              DNS hooks, per-partition fetch knobs, topic-create
              defaults, null-key compaction policy, metric names

Typed configuration and pure helpers that complement the core
'Kafka.Client.AdminClient'. Most entries are pure types / config
records; the SSL and DNS factories are records-of-IO so callers
can plug in their own implementations.

  * 'defaultAdminApiTimeoutMs' — request-timeout default
    matching the JVM @AdminClient@ (60s).
  * 'SslEngineFactory' / 'HostResolver' — pluggable hooks for
    TLS engine construction and DNS resolution.
  * 'PerPartitionFetchKnob' — per-partition fetch-min-bytes +
    min-timestamp.
  * 'TopicCreateDefaults' — defaults applied when creating a
    topic without explicit knobs.
  * 'NullKeyCompactionPolicy' — how the broker should treat
    null-key records during log compaction.
  * @admin*LatencyMs@ — telemetry metric names emitted from
    @Kafka.Telemetry.Metrics@.
-}
module Kafka.Client.AdminExtras
  ( -- * Defaults
    defaultAdminApiTimeoutMs
    -- * Pluggable SSL engine
  , SslEngineFactory (..)
    -- * Pluggable host resolver
  , HostResolver (..)
    -- * Per-partition fetch knobs
  , PerPartitionFetchKnob (..)
    -- * Topic-create defaults
  , TopicCreateDefaults (..)
  , defaultTopicCreateDefaults
    -- * Null-key compaction policy
  , NullKeyCompactionPolicy (..)
  , defaultNullKeyCompactionPolicy
    -- * AdminClient metric names
  , adminListTopicsLatencyMs
  , adminCreateTopicsLatencyMs
  , adminDescribeGroupsLatencyMs
  , adminAlterConfigsLatencyMs
  , adminDeleteRecordsLatencyMs
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

----------------------------------------------------------------------
-- Default api timeout
----------------------------------------------------------------------

-- | Mirrors @AdminClient.DEFAULT_API_TIMEOUT_MS@ in the JVM
-- client.
defaultAdminApiTimeoutMs :: Int
defaultAdminApiTimeoutMs = 60_000

----------------------------------------------------------------------
-- Pluggable SSL engine
----------------------------------------------------------------------

-- | A pluggable SSL-engine factory. The wireform-kafka library
-- doesn't pin a TLS library; callers wire whatever they need.
-- Mirrors Java's @SslEngineFactory@ interface (the actual type
-- it returns is mostly opaque to the producer / consumer
-- layer; we wrap an 'IO ()' the caller uses to reconfigure
-- their TLS context).
newtype SslEngineFactory = SslEngineFactory
  { sslEngineFactory :: IO ()
  }

----------------------------------------------------------------------
-- Pluggable host resolver
----------------------------------------------------------------------

-- | A pluggable hostname resolver — useful for service-mesh /
-- multi-cluster setups where DNS isn't authoritative. Returns
-- the resolved IP string(s); callers iterate with the same
-- shape librdkafka uses.
newtype HostResolver = HostResolver
  { resolveHost :: Text -> IO [Text]
  }

----------------------------------------------------------------------
-- Per-partition fetch min-bytes
----------------------------------------------------------------------

data PerPartitionFetchKnob = PerPartitionFetchKnob
  { ppfkPartition       :: !Int32
  , ppfkMinBytes        :: !Int
    -- ^ Per-partition @fetch.min.bytes@; the broker will not
    --   return less than this for this partition.
  , ppfkMinTimestampMs  :: !(Maybe Int64)
    -- ^ Only return records whose timestamp is at or above
    --   this value.
  }
  deriving stock (Eq, Show, Generic)

----------------------------------------------------------------------
-- Topic-create defaults
----------------------------------------------------------------------

data TopicCreateDefaults = TopicCreateDefaults
  { tcdReplicationFactor :: !Int16'
  , tcdNumPartitions     :: !Int32
  , tcdConfigOverrides   :: !(Map Text Text)
    -- ^ Topic-level config overrides applied to every newly
    --   created topic when the caller doesn't override them.
  }
  deriving stock (Eq, Show, Generic)

-- | Synonym for an Int16-shaped replication factor; the wire
-- field is @i16@ but we use 'Int' here so callers don't need
-- to qualify.
type Int16' = Int

defaultTopicCreateDefaults :: TopicCreateDefaults
defaultTopicCreateDefaults = TopicCreateDefaults
  { tcdReplicationFactor = 1
  , tcdNumPartitions     = 1
  , tcdConfigOverrides   = Map.empty
  }

----------------------------------------------------------------------
-- Null-key compaction policy
----------------------------------------------------------------------

-- | What to do when a producer sends a 'Nothing' key to a
-- compacted topic. The default Kafka behaviour is to reject
-- with 'INVALID_RECORD'; newer brokers can treat missing keys
-- as tombstones / pass-through.
data NullKeyCompactionPolicy
  = NkcReject
  | NkcTombstone
  | NkcPassThrough
  deriving stock (Eq, Show, Generic)

defaultNullKeyCompactionPolicy :: NullKeyCompactionPolicy
defaultNullKeyCompactionPolicy = NkcReject

----------------------------------------------------------------------
-- Admin metric names
----------------------------------------------------------------------

adminListTopicsLatencyMs,
  adminCreateTopicsLatencyMs,
  adminDescribeGroupsLatencyMs,
  adminAlterConfigsLatencyMs,
  adminDeleteRecordsLatencyMs :: Text
adminListTopicsLatencyMs     = "kafka.admin.list-topics.latency.ms"
adminCreateTopicsLatencyMs   = "kafka.admin.create-topics.latency.ms"
adminDescribeGroupsLatencyMs = "kafka.admin.describe-groups.latency.ms"
adminAlterConfigsLatencyMs   = "kafka.admin.alter-configs.latency.ms"
adminDeleteRecordsLatencyMs  = "kafka.admin.delete-records.latency.ms"

-- Re-export for backwards-compatibility with callers that read
-- the byte-shaped form. (Some callers prefer bytes for HTTP
-- content negotiation or generic-key construction; the cost is
-- one extra pure-data line.)
_dummy :: ByteString
_dummy = mempty
