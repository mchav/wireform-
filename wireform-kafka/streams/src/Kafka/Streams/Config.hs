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
  ) where

import Data.Int (Int64)
import Data.Text (Text)
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
