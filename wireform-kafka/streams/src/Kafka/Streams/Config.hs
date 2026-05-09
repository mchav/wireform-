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
  }

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
      _                            -> cfg

readT :: Read a => T.Text -> Maybe a
readT t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _         -> Nothing
