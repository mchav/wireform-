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

newtype BrokerId = BrokerId { unBrokerId :: Int }
  deriving (Show, Eq, Ord, Read, Generic)

newtype PartitionId = PartitionId { unPartitionId :: Int }
  deriving (Show, Eq, Read, Ord, Enum, Generic)

newtype Millis = Millis { unMillis :: Int64 }
  deriving (Show, Read, Eq, Ord, Num, Generic)

newtype ClientId = ClientId
  { unClientId :: Text
  } deriving (Show, Eq, IsString, Ord, Generic)

newtype BatchSize = BatchSize { unBatchSize :: Int }
  deriving (Show, Read, Eq, Ord, Num, Generic)

data TopicType
  = User
  | System
  deriving (Show, Read, Eq, Ord, Generic)

newtype TopicName = TopicName
  { unTopicName :: Text
  } deriving (Show, Eq, Ord, IsString, Read, Generic)

topicType :: TopicName -> TopicType
topicType (TopicName tn)
  | "__" `T.isPrefixOf` tn = System
  | otherwise = User

newtype BrokerAddress = BrokerAddress
  { unBrokerAddress :: Text
  } deriving (Show, Eq, IsString, Generic)

newtype Timeout = Timeout { unTimeout :: Int }
  deriving (Show, Eq, Read, Generic)

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

data KafkaError
  = KafkaError Text
  | KafkaInvalidReturnValue
  | KafkaBadSpecification Text
  | KafkaResponseError RdKafkaRespErrT
  | KafkaInvalidConfigurationValue Text
  | KafkaUnknownConfigurationKey Text
  | KafkaBadConfiguration
  deriving (Eq, Show, Typeable, Generic)

instance Exception KafkaError where
  displayException = show

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

data KafkaCompressionCodec
  = NoCompression
  | Gzip
  | Snappy
  | Lz4
  | Zstd
  deriving (Eq, Show, Typeable, Generic)

kafkaCompressionCodecToText :: KafkaCompressionCodec -> Text
kafkaCompressionCodecToText = \case
  NoCompression -> "none"
  Gzip -> "gzip"
  Snappy -> "snappy"
  Lz4 -> "lz4"
  Zstd -> "zstd"

newtype Headers = Headers
  { unHeaders :: [(ByteString, ByteString)]
  } deriving (Eq, Show, Semigroup, Monoid, Read, Typeable, Generic)

headersFromList :: [(ByteString, ByteString)] -> Headers
headersFromList = Headers

headersToList :: Headers -> [(ByteString, ByteString)]
headersToList = unHeaders
