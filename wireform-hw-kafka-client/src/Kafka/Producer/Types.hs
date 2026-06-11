{- |
Module      : Kafka.Producer.Types
Description : @hw-kafka-client@ producer types backed by wireform handles.

This module mirrors the public producer data types from
@hw-kafka-client@. It is part of the transitional facade for code that
is still importing @Kafka.Producer.Types@ while moving toward the
native "Kafka.Client.Producer" API.
-}
module Kafka.Producer.Types (
  KafkaProducer (..),
  ProducerRecord (..),
  ProducePartition (..),
  DeliveryReport (..),
  ImmediateError (..),
) where

import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Kafka.Consumer.Types (Offset (..))
import Kafka.Internal.Compat (
  HasKafka (..),
  HasKafkaConf (..),
  HasTopicConf (..),
  Kafka (..),
  KafkaConf (..),
  TopicConf (..),
 )
import Kafka.Types (Headers, KafkaError (..), TopicName (..))


{- | Main type for Kafka message production.

Its constructor is exported for source compatibility with
@hw-kafka-client@, but normal code should acquire a value through
'Kafka.Producer.newProducer'. The fields wrap native wireform
producer state rather than librdkafka pointers.
-}
data KafkaProducer = KafkaProducer
  { kpKafkaPtr :: !Kafka
  -- ^ Opaque compatibility handle for the underlying producer.
  , kpKafkaConf :: !KafkaConf
  -- ^ Compatibility copy of Kafka-level properties.
  , kpTopicConf :: !TopicConf
  -- ^ Compatibility copy of topic-level properties.
  }


instance HasKafka KafkaProducer where
  getKafka = kpKafkaPtr


instance HasKafkaConf KafkaProducer where
  getKafkaConf = kpKafkaConf


instance HasTopicConf KafkaProducer where
  getTopicConf = kpTopicConf


-- | Represents messages /to be enqueued/ onto a Kafka broker.
data ProducerRecord = ProducerRecord
  { prTopic :: !TopicName
  -- ^ Target topic.
  , prPartition :: !ProducePartition
  -- ^ Explicit or broker-selected partition.
  , prKey :: Maybe ByteString
  -- ^ Optional message key.
  , prValue :: Maybe ByteString
  {- ^ Optional message value. The native wireform producer uses a
  strict payload; this facade maps 'Nothing' to an empty payload.
  -}
  , prHeaders :: !Headers
  -- ^ Record headers.
  }
  deriving (Eq, Show, Typeable, Generic)


-- | Producer partition choice.
data ProducePartition
  = -- | The partition number of the topic.
    SpecifiedPartition {-# UNPACK #-} !Int
  | -- | Let Kafka decide the partition.
    UnassignedPartition
  deriving (Show, Eq, Ord, Typeable, Generic)


-- | Error caused by pre-flight conditions not being met.
newtype ImmediateError = ImmediateError KafkaError
  deriving newtype (Eq, Show)


-- | Result of sending a message to the broker, useful for callbacks.
data DeliveryReport
  = -- | The message was successfully sent at this offset.
    DeliverySuccess ProducerRecord Offset
  | -- | The message could not be sent.
    DeliveryFailure ProducerRecord KafkaError
  | -- | An error occurred without an attached sent message.
    NoMessageError KafkaError
  deriving (Show, Eq, Generic)
