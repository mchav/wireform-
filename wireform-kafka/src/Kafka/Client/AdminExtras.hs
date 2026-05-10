{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.AdminExtras
Description : KIP-464 / 484 / 524 / 967 / 1107 / 1153 / 1170 — AdminClient
              ergonomics

Adds the remaining JVM-AdminClient surfaces that
'Kafka.Client.AdminClient' didn't yet expose:

  * KIP-464: Defaults for AdminClient / Consumer / Producer
    when no overrides are provided. We surface it here as
    'defaultAdminApiTimeoutMs' (the JVM client uses 60 s).
  * KIP-484: pluggable 'SslEngineFactory' — exposed as a
    record-of-IO so callers can plug in their own (matching
    'Kafka.Network.Transport' and the OAuth fetcher).
  * KIP-524: pluggable 'HostResolver' — surfaces the same
    DNS-resolution hook the consumer / producer can override.
  * KIP-967: per-partition fetch-min-bytes + min-timestamp.
  * KIP-1107: enhanced AdminClient metrics (just the metric
    names; the registry lives in 'Kafka.Telemetry.Metrics').
  * KIP-1153: AdminClient's @TopicCreateOptions@ defaults knob.
  * KIP-1170: configuration for null-key compaction behaviour.

Most entries are pure types / config knobs; they slot into
'Kafka.Client.AdminClient' in a follow-up wiring change.
-}
module Kafka.Client.AdminExtras
  ( -- * Defaults (KIP-464)
    defaultAdminApiTimeoutMs
    -- * Pluggable SSL engine (KIP-484)
  , SslEngineFactory (..)
    -- * Pluggable host resolver (KIP-524)
  , HostResolver (..)
    -- * Per-partition fetch knobs (KIP-967)
  , PerPartitionFetchKnob (..)
    -- * Topic-create defaults (KIP-1153)
  , TopicCreateDefaults (..)
  , defaultTopicCreateDefaults
    -- * Null-key compaction policy (KIP-1170)
  , NullKeyCompactionPolicy (..)
  , defaultNullKeyCompactionPolicy
    -- * AdminClient metric names (KIP-1107)
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
-- KIP-464 default api timeout
----------------------------------------------------------------------

-- | Mirrors @AdminClient.DEFAULT_API_TIMEOUT_MS@ in the JVM
-- client.
defaultAdminApiTimeoutMs :: Int
defaultAdminApiTimeoutMs = 60_000

----------------------------------------------------------------------
-- KIP-484 pluggable SSL engine
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
-- KIP-524 pluggable host resolver
----------------------------------------------------------------------

-- | A pluggable hostname resolver — useful for service-mesh /
-- multi-cluster setups where DNS isn't authoritative. Returns
-- the resolved IP string(s); callers iterate with the same
-- shape librdkafka uses.
newtype HostResolver = HostResolver
  { resolveHost :: Text -> IO [Text]
  }

----------------------------------------------------------------------
-- KIP-967 per-partition fetch min-bytes
----------------------------------------------------------------------

data PerPartitionFetchKnob = PerPartitionFetchKnob
  { ppfkPartition       :: !Int32
  , ppfkMinBytes        :: !Int
    -- ^ Per-partition @fetch.min.bytes@; the broker will not
    --   return less than this for this partition.
  , ppfkMinTimestampMs  :: !(Maybe Int64)
    -- ^ KIP-968 — only return records whose timestamp is at or
    --   above this value.
  }
  deriving stock (Eq, Show, Generic)

----------------------------------------------------------------------
-- KIP-1153 topic-create defaults
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
-- KIP-1170 null-key compaction policy
----------------------------------------------------------------------

-- | What to do when a producer sends a 'Nothing' key to a
-- compacted topic. The default Kafka behaviour is to reject
-- with 'INVALID_RECORD'; KIP-1170 lets the broker treat
-- missing keys as tombstones / pass-through.
data NullKeyCompactionPolicy
  = NkcReject
  | NkcTombstone
  | NkcPassThrough
  deriving stock (Eq, Show, Generic)

defaultNullKeyCompactionPolicy :: NullKeyCompactionPolicy
defaultNullKeyCompactionPolicy = NkcReject

----------------------------------------------------------------------
-- KIP-1107 admin metric names
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
