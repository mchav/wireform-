{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Kafka.Streams.Config
-- Description : 'StreamsConfig' — top-level runtime configuration
--
-- Mirrors the @StreamsConfig@ keys in
-- @org.apache.kafka.streams.StreamsConfig@. Every field is documented
-- against the original key name so users porting code can grep across.
module Kafka.Streams.Config
  ( -- * Config
    StreamsConfig (..)
  , DslStore (..)
  , RackAwareStrategy (..)
  , defaultStreamsConfig
    -- * Processing semantics
  , ProcessingGuarantee (..)
    -- * Common defaults
  , defaultCommitIntervalMs
  , defaultPollMs
  , defaultStateDir
    -- * Properties-style overrides
  , streamsConfigFromMap
  , StreamsConfigKey
  ) where

import Data.Int (Int64)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Kafka.Streams.Errors
  ( DeserializationHandler
  , ProductionHandler
  , logAndContinue
  )

-- | What level of processing-guarantee should the runtime offer?
data ProcessingGuarantee
  = AtLeastOnceP   -- ^ default; possible duplicates on failover
  | ExactlyOnceV2  -- ^ KIP-447 transactional commits
  deriving stock (Eq, Show, Generic)

-- | Top-level runtime configuration.  Field names mirror the Java
-- camelCased property keys (with the @StreamsConfig.@ prefix
-- dropped):
--
--   * 'applicationId'             — @application.id@
--   * 'bootstrapServers'          — @bootstrap.servers@
--   * 'numStreamThreads'          — @num.stream.threads@
--   * 'numStandbyReplicas'        — @num.standby.replicas@
--   * 'commitIntervalMs'          — @commit.interval.ms@
--   * 'pollMs'                    — @poll.ms@
--   * 'cacheMaxBytesBuffering'    — @cache.max.bytes.buffering@
--   * 'maxTaskIdleMs'             — @max.task.idle.ms@
--   * 'processingGuarantee'       — @processing.guarantee@
--   * 'replicationFactor'         — @replication.factor@
--   * 'stateDir'                  — @state.dir@
--   * 'defaultDeserHandler'       — @default.deserialization.exception.handler@
--   * 'defaultProductionHandler'  — @default.production.exception.handler@
data StreamsConfig = StreamsConfig
  { applicationId            :: !Text
  , bootstrapServers         :: ![Text]
  , clientId                 :: !Text
  , numStreamThreads         :: !Int
  , numStandbyReplicas       :: !Int
  , commitIntervalMs         :: !Int
  , pollMs                   :: !Int
  , cacheMaxBytesBuffering   :: !Int64
  , maxTaskIdleMs            :: !Int
  , processingGuarantee      :: !ProcessingGuarantee
  , replicationFactor        :: !Int
  , stateDir                 :: !FilePath
  , defaultDeserHandler      :: !DeserializationHandler
  , defaultProductionHandler :: !(Maybe ProductionHandler)
    -- KIP-892 / Streams full-config-surface additions ------------
  , taskTimeoutMs            :: !Int
    -- ^ @task.timeout.ms@ — how long a task may stall on a
    --   recoverable error before the runtime kills it. Default
    --   300_000 (5 minutes), matching Java.
  , acceptableRecoveryLag    :: !Int64
    -- ^ @acceptable.recovery.lag@ — maximum changelog lag (in
    --   records) at which a warmup replica is considered "caught
    --   up" and may be promoted to active. Default 10_000.
  , maxWarmupReplicas        :: !Int
    -- ^ @max.warmup.replicas@ — total warmup replicas allowed
    --   across the application instance. Default 2.
  , probingRebalanceIntervalMs :: !Int
    -- ^ @probing.rebalance.interval.ms@ — KIP-441 cadence for
    --   re-issuing rebalances to check whether warmups are
    --   ready. Default 600_000 (10 minutes).
  , taskAssignorClass        :: !(Maybe Text)
    -- ^ @task.assignor.class@ — fully-qualified class name (or
    --   our @Text@ tag for an in-process assignor) the runtime
    --   should use when computing assignments. 'Nothing' (the
    --   default) selects the built-in cooperative-sticky
    --   assignor.
  , applicationServer        :: !(Maybe Text)
    -- ^ KIP-67 @application.server@ — host:port advertised in
    --   JoinGroup subscription metadata so peers can discover
    --   this instance for cross-instance interactive queries.
    --   'Nothing' disables advertisement.
  , defaultDslStore          :: !DslStore
    -- ^ KIP-591 @default.dsl.store@ — which backend the DSL
    --   uses when 'Materialized' doesn't pin one (in-memory or
    --   RocksDB). Today only 'DslInMemoryStore' is honoured.
  , rackAwareAssignmentStrategy :: !RackAwareStrategy
    -- ^ KIP-925 @rack.aware.assignment.strategy@.
  , rackAwareTrafficCost     :: !Int
    -- ^ KIP-925 @rack.aware.assignment.traffic.cost@.
  , rackAwareNonOverlapCost  :: !Int
    -- ^ KIP-925 @rack.aware.assignment.non.overlap.cost@.
  , windowSizeMs             :: !(Maybe Int64)
    -- ^ KIP-585 @window.size.ms@ — default window size used by
    --   the windowed-serde auto-resolver when no explicit
    --   serde is supplied.
  , upgradeFrom              :: !(Maybe Text)
    -- ^ @upgrade.from@ — multi-version upgrade compatibility
    --   knob (read by the assignor on a rolling upgrade).
  }

-- | KIP-591 @default.dsl.store@ enum.
data DslStore
  = DslInMemoryStore
  | DslRocksDbStore
  deriving stock (Eq, Show)

-- | KIP-925 rack-aware assignment-strategy enum.
data RackAwareStrategy
  = RackAwareNone
  | RackAwareMinTrafficCost
  | RackAwareBalanceSubtopology
  deriving stock (Eq, Show)

defaultCommitIntervalMs :: Int
defaultCommitIntervalMs = 30_000

defaultPollMs :: Int
defaultPollMs = 100

defaultStateDir :: FilePath
defaultStateDir = "/tmp/kafka-streams"

-- | Default config matching Kafka Streams 3.x defaults. The caller
-- must override 'applicationId' and 'bootstrapServers'.
defaultStreamsConfig :: StreamsConfig
defaultStreamsConfig = StreamsConfig
  { applicationId            = "kafka-streams-app"
  , bootstrapServers         = ["localhost:9092"]
  , clientId                 = "kafka-streams-client"
  , numStreamThreads         = 1
  , numStandbyReplicas       = 0
  , commitIntervalMs         = defaultCommitIntervalMs
  , pollMs                   = defaultPollMs
  , cacheMaxBytesBuffering   = 10 * 1024 * 1024
  , maxTaskIdleMs            = 0
  , processingGuarantee      = AtLeastOnceP
  , replicationFactor        = 1
  , stateDir                 = defaultStateDir
  , defaultDeserHandler      = logAndContinue
  , defaultProductionHandler = Nothing
  , taskTimeoutMs              = 300_000
  , acceptableRecoveryLag      = 10_000
  , maxWarmupReplicas          = 2
  , probingRebalanceIntervalMs = 600_000
  , taskAssignorClass          = Nothing
  , applicationServer          = Nothing
  , defaultDslStore            = DslInMemoryStore
  , rackAwareAssignmentStrategy = RackAwareNone
  , rackAwareTrafficCost       = 1
  , rackAwareNonOverlapCost    = 10
  , windowSizeMs               = Nothing
  , upgradeFrom                = Nothing
  }

----------------------------------------------------------------------
-- Properties-style overrides
----------------------------------------------------------------------

-- | The set of supported config keys, named to match the Java
-- @StreamsConfig@ constants.
type StreamsConfigKey = T.Text

-- | Build a 'StreamsConfig' from the standard 'defaultStreamsConfig'
-- by applying @Properties@-style overrides. Mirrors how Java users
-- configure the runtime via @new StreamsConfig(properties)@.
--
-- Recognised keys (matching Java):
--
--   * @application.id@
--   * @bootstrap.servers@           — comma-separated host:port list
--   * @client.id@
--   * @num.stream.threads@
--   * @num.standby.replicas@
--   * @commit.interval.ms@
--   * @poll.ms@
--   * @cache.max.bytes.buffering@
--   * @max.task.idle.ms@
--   * @processing.guarantee@         — @"at_least_once"@ or @"exactly_once_v2"@
--   * @replication.factor@
--   * @state.dir@
--
-- Unknown keys are silently ignored (same as Java's default).
streamsConfigFromMap
  :: Map.Map StreamsConfigKey T.Text -> StreamsConfig
streamsConfigFromMap m = foldl' step defaultStreamsConfig (Map.toAscList m)
  where
    step !cfg (k, v) = case k of
      "application.id"             -> cfg { applicationId = v }
      "bootstrap.servers"          ->
        cfg { bootstrapServers = T.splitOn "," v }
      "client.id"                  -> cfg { clientId = v }
      "num.stream.threads"         ->
        maybe cfg (\n -> cfg { numStreamThreads = n }) (readT v)
      "num.standby.replicas"       ->
        maybe cfg (\n -> cfg { numStandbyReplicas = n }) (readT v)
      "commit.interval.ms"         ->
        maybe cfg (\n -> cfg { commitIntervalMs = n }) (readT v)
      "poll.ms"                    ->
        maybe cfg (\n -> cfg { pollMs = n }) (readT v)
      "cache.max.bytes.buffering"  ->
        maybe cfg (\n -> cfg { cacheMaxBytesBuffering = n }) (readT v)
      "max.task.idle.ms"           ->
        maybe cfg (\n -> cfg { maxTaskIdleMs = n }) (readT v)
      "processing.guarantee"       -> case v of
        "at_least_once"   -> cfg { processingGuarantee = AtLeastOnceP }
        "exactly_once_v2" -> cfg { processingGuarantee = ExactlyOnceV2 }
        _                 -> cfg
      "replication.factor"         ->
        maybe cfg (\n -> cfg { replicationFactor = n }) (readT v)
      "state.dir"                  -> cfg { stateDir = T.unpack v }
      "task.timeout.ms"            ->
        maybe cfg (\n -> cfg { taskTimeoutMs = n }) (readT v)
      "acceptable.recovery.lag"    ->
        maybe cfg (\n -> cfg { acceptableRecoveryLag = n }) (readT v)
      "max.warmup.replicas"        ->
        maybe cfg (\n -> cfg { maxWarmupReplicas = n }) (readT v)
      "probing.rebalance.interval.ms" ->
        maybe cfg (\n -> cfg { probingRebalanceIntervalMs = n }) (readT v)
      "task.assignor.class"        -> cfg { taskAssignorClass = Just v }
      "application.server"         -> cfg { applicationServer = Just v }
      "default.dsl.store"          -> case v of
        "in_memory" -> cfg { defaultDslStore = DslInMemoryStore }
        "ROCKS_DB"  -> cfg { defaultDslStore = DslRocksDbStore }
        "rocksdb"   -> cfg { defaultDslStore = DslRocksDbStore }
        _           -> cfg
      "rack.aware.assignment.strategy" -> case v of
        "MIN_TRAFFIC"          -> cfg { rackAwareAssignmentStrategy = RackAwareMinTrafficCost }
        "BALANCE_SUBTOPOLOGY"  -> cfg { rackAwareAssignmentStrategy = RackAwareBalanceSubtopology }
        "NONE"                 -> cfg { rackAwareAssignmentStrategy = RackAwareNone }
        _                      -> cfg
      "rack.aware.assignment.traffic.cost" ->
        maybe cfg (\n -> cfg { rackAwareTrafficCost = n }) (readT v)
      "rack.aware.assignment.non.overlap.cost" ->
        maybe cfg (\n -> cfg { rackAwareNonOverlapCost = n }) (readT v)
      "statestore.cache.max.bytes" ->
        -- Renamed-from cache.max.bytes.buffering in 3.4. We
        -- accept both names; the field is the same.
        maybe cfg (\n -> cfg { cacheMaxBytesBuffering = n }) (readT v)
      "window.size.ms"             ->
        maybe cfg (\n -> cfg { windowSizeMs = Just n }) (readT v)
      "upgrade.from"               -> cfg { upgradeFrom = Just v }
      _                            -> cfg

readT :: Read a => T.Text -> Maybe a
readT t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _         -> Nothing
