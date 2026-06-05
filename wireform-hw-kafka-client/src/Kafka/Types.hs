{-|
Module      : Kafka.Types
Description : Shared @hw-kafka-client@ compatibility types.

Types shared by the compatibility producer and consumer modules.

This module intentionally keeps the public names, constructors, and
record selectors from @hw-kafka-client@ so existing source can compile
while migrating to @wireform-kafka@. It is a transitional API: new code
should use the typed native modules under "Kafka.Client.*" and
"Kafka.Network.*" directly.
-}
module Kafka.Types
  ( BrokerId (..)
  , PartitionId (..)
  , Millis (..)
  , ClientId (..)
  , BatchSize (..)
  , TopicName (..)
  , BrokerAddress (..)
  , Timeout (..)
  , KafkaLogLevel (..)
  , KafkaError (..)
  , KafkaDebug (..)
  , KafkaCompressionCodec (..)
  , TopicType (..)
  , Headers
  , headersFromList
  , headersToList
  , topicType
  , kafkaDebugToText
  , kafkaCompressionCodecToText
  ) where

import Control.Exception (Exception (..))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Kafka.Internal.Compat (RdKafkaRespErrT)
import qualified Data.Text as T

-- | Kafka broker ID.
newtype BrokerId = BrokerId { unBrokerId :: Int }
  deriving (Show, Eq, Ord, Read, Generic)

-- | Topic partition ID.
newtype PartitionId = PartitionId { unPartitionId :: Int }
  deriving (Show, Eq, Read, Ord, Enum, Generic)

-- | A number of milliseconds, used to represent durations and timestamps.
newtype Millis = Millis { unMillis :: Int64 }
  deriving (Show, Read, Eq, Ord, Num, Generic)

-- | Client ID used by Kafka to better track requests.
--
-- See <https://kafka.apache.org/documentation/#client.id Kafka documentation on client ID>.
newtype ClientId = ClientId
  { unClientId :: Text
  } deriving (Show, Eq, IsString, Ord, Generic)

-- | Batch size used for polling.
newtype BatchSize = BatchSize { unBatchSize :: Int }
  deriving (Show, Read, Eq, Ord, Num, Generic)

data TopicType
  = User
    -- ^ Normal topics that are created by a user.
  | System
    -- ^ Topics starting with a double underscore, such as
    -- @__consumer_offsets@, are considered system topics.
  deriving (Show, Read, Eq, Ord, Generic)

-- | Topic name to consume or produce messages.
--
-- @hw-kafka-client@ documented regex subscriptions through names
-- beginning with @^@. The native wireform consumer has its own
-- subscription API; this compatibility type keeps the old source
-- shape.
newtype TopicName = TopicName
  { unTopicName :: Text
    -- ^ A simple topic name, or a regex-like topic name in legacy code.
  } deriving (Show, Eq, Ord, IsString, Read, Generic)

-- | Deduce the topic type from its name by checking for a leading @__@.
topicType :: TopicName -> TopicType
topicType (TopicName tn)
  | "__" `T.isPrefixOf` tn = System
  | otherwise = User

-- | Kafka broker address string, for example @broker1:9092@.
newtype BrokerAddress = BrokerAddress
  { unBrokerAddress :: Text
  } deriving (Show, Eq, IsString, Generic)

-- | Timeout in milliseconds.
newtype Timeout = Timeout { unTimeout :: Int }
  deriving (Show, Eq, Read, Generic)

-- | Log levels from the @hw-kafka-client@ API.
--
-- The native wireform client does not use librdkafka logging, but the
-- constructors are kept so existing configuration code continues to
-- typecheck.
data KafkaLogLevel
  = KafkaLogEmerg
  | KafkaLogAlert
  | KafkaLogCrit
  | KafkaLogErr
  | KafkaLogWarning
  | KafkaLogNotice
  | KafkaLogInfo
  | KafkaLogDebug
  deriving (Show, Enum, Eq)

-- | Compatibility error type matching @hw-kafka-client@.
data KafkaError
  = KafkaError Text
    -- ^ Free-form error text.
  | KafkaInvalidReturnValue
    -- ^ A legacy invalid return value marker.
  | KafkaBadSpecification Text
    -- ^ Invalid call or unsupported compatibility operation.
  | KafkaResponseError RdKafkaRespErrT
    -- ^ Compatibility wrapper for the old librdkafka response enum.
  | KafkaInvalidConfigurationValue Text
    -- ^ Invalid configuration value.
  | KafkaUnknownConfigurationKey Text
    -- ^ Unknown configuration key.
  | KafkaBadConfiguration
    -- ^ Configuration could not be applied.
  deriving (Eq, Show, Typeable, Generic)

instance Exception KafkaError where
  displayException = show

-- | Available @hw-kafka-client@ debug contexts.
--
-- These render to the same text as upstream; they are accepted by the
-- compatibility property builders as migration-only configuration.
data KafkaDebug
  = DebugGeneric
  | DebugBroker
  | DebugTopic
  | DebugMetadata
  | DebugQueue
  | DebugMsg
  | DebugProtocol
  | DebugCgrp
  | DebugSecurity
  | DebugFetch
  | DebugFeature
  | DebugAll
  deriving (Eq, Show, Typeable, Generic)

-- | Convert a 'KafkaDebug' into its legacy librdkafka string equivalent.
--
-- This is useful when checking migrated configuration values.
kafkaDebugToText :: KafkaDebug -> Text
kafkaDebugToText = \case
  DebugGeneric -> "generic"
  DebugBroker -> "broker"
  DebugTopic -> "topic"
  DebugMetadata -> "metadata"
  DebugQueue -> "queue"
  DebugMsg -> "msg"
  DebugProtocol -> "protocol"
  DebugCgrp -> "cgrp"
  DebugSecurity -> "security"
  DebugFetch -> "fetch"
  DebugFeature -> "feature"
  DebugAll -> "all"

-- | Compression codec used by a topic.
--
-- See <https://kafka.apache.org/documentation/#compression.type Kafka documentation on compression codecs>.
data KafkaCompressionCodec
  = NoCompression
  | Gzip
  | Snappy
  | Lz4
  | Zstd
  deriving (Eq, Show, Typeable, Generic)

-- | Convert a 'KafkaCompressionCodec' into its legacy librdkafka string equivalent.
kafkaCompressionCodecToText :: KafkaCompressionCodec -> Text
kafkaCompressionCodecToText = \case
  NoCompression -> "none"
  Gzip -> "gzip"
  Snappy -> "snappy"
  Lz4 -> "lz4"
  Zstd -> "zstd"

-- | Headers that might be passed along with a record.
newtype Headers = Headers
  { unHeaders :: [(ByteString, ByteString)]
  } deriving (Eq, Show, Semigroup, Monoid, Read, Typeable, Generic)

-- | Build compatibility headers from a list.
headersFromList :: [(ByteString, ByteString)] -> Headers
headersFromList = Headers

-- | Convert compatibility headers to a list.
headersToList :: Headers -> [(ByteString, ByteString)]
headersToList = unHeaders
