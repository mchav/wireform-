module Kafka.Producer.Types
  ( KafkaProducer (..)
  , ProducerRecord (..)
  , ProducePartition (..)
  , DeliveryReport (..)
  , ImmediateError (..)
  ) where

import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Kafka.Consumer.Types (Offset (..))
import Kafka.Internal.Compat
  ( HasKafka (..)
  , HasKafkaConf (..)
  , HasTopicConf (..)
  , Kafka (..)
  , KafkaConf (..)
  , TopicConf (..)
  )
import Kafka.Types (Headers, KafkaError (..), TopicName (..))

data KafkaProducer = KafkaProducer
  { kpKafkaPtr :: !Kafka
  , kpKafkaConf :: !KafkaConf
  , kpTopicConf :: !TopicConf
  }

instance HasKafka KafkaProducer where
  getKafka = kpKafkaPtr

instance HasKafkaConf KafkaProducer where
  getKafkaConf = kpKafkaConf

instance HasTopicConf KafkaProducer where
  getTopicConf = kpTopicConf

data ProducerRecord = ProducerRecord
  { prTopic :: !TopicName
  , prPartition :: !ProducePartition
  , prKey :: Maybe ByteString
  , prValue :: Maybe ByteString
  , prHeaders :: !Headers
  } deriving (Eq, Show, Typeable, Generic)

data ProducePartition
  = SpecifiedPartition {-# UNPACK #-} !Int
  | UnassignedPartition
  deriving (Show, Eq, Ord, Typeable, Generic)

newtype ImmediateError = ImmediateError KafkaError
  deriving newtype (Eq, Show)

data DeliveryReport
  = DeliverySuccess ProducerRecord Offset
  | DeliveryFailure ProducerRecord KafkaError
  | NoMessageError KafkaError
  deriving (Show, Eq, Generic)
