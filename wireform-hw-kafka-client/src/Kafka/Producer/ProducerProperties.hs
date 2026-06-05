{-|
Module      : Kafka.Producer.ProducerProperties
Description : Producer configuration builders from @hw-kafka-client@.

This module ports the @hw-kafka-client@ property-builder API. The
properties are translated into "Kafka.Client.Producer" configuration
where there is a native equivalent; librdkafka-specific callbacks and
topic properties are retained as transitional source-compatible data.
-}
module Kafka.Producer.ProducerProperties
  ( ProducerProperties (..)
  , brokersList
  , setCallback
  , logLevel
  , compression
  , topicCompression
  , sendTimeout
  , statisticsInterval
  , extraProps
  , extraProp
  , suppressDisconnectLogs
  , extraTopicProps
  , debugOptions
  , module Kafka.Producer.Callbacks
  ) where

import Data.Map (Map)
import Data.Text (Text)
import Kafka.Internal.Callbacks (Callback)
import Kafka.Internal.Compat (decimalText)
import Kafka.Producer.Callbacks
import Kafka.Types
  ( BrokerAddress (..)
  , KafkaCompressionCodec (..)
  , KafkaDebug (..)
  , KafkaLogLevel (..)
  , Millis (..)
  , Timeout (..)
  , kafkaCompressionCodecToText
  , kafkaDebugToText
  )
import qualified Data.Map as M
import qualified Data.Text as T

-- | Properties used to create a 'Kafka.Producer.Types.KafkaProducer'.
data ProducerProperties = ProducerProperties
  { ppKafkaProps :: Map Text Text
    -- ^ Kafka-level configuration properties.
  , ppTopicProps :: Map Text Text
    -- ^ Topic-level configuration properties retained for compatibility.
  , ppLogLevel :: Maybe KafkaLogLevel
    -- ^ Legacy log level.
  , ppCallbacks :: [Callback]
    -- ^ Legacy callback tokens.
  }

instance Semigroup ProducerProperties where
  ProducerProperties k1 t1 ll1 cb1 <> ProducerProperties k2 t2 ll2 cb2 =
    ProducerProperties (M.union k2 k1) (M.union t2 t1) (ll2 <|> ll1) (cb1 <> cb2)

instance Monoid ProducerProperties where
  mempty = ProducerProperties
    { ppKafkaProps = M.empty
    , ppTopicProps = M.empty
    , ppLogLevel = Nothing
    , ppCallbacks = []
    }

-- | Set the list of brokers to contact to connect to the Kafka cluster.
brokersList :: [BrokerAddress] -> ProducerProperties
brokersList bs =
  extraProps (M.singleton "bootstrap.servers" brokers)
  where
    brokers = T.intercalate "," (map unBrokerAddress bs)

-- | Set a producer callback token.
setCallback :: Callback -> ProducerProperties
setCallback cb = mempty { ppCallbacks = [cb] }

-- | Set the legacy logging level.
logLevel :: KafkaLogLevel -> ProducerProperties
logLevel ll = mempty { ppLogLevel = Just ll }

-- | Set the producer @compression.codec@ property.
compression :: KafkaCompressionCodec -> ProducerProperties
compression c = extraProps (M.singleton "compression.codec" (kafkaCompressionCodecToText c))

-- | Set the topic-level @compression.codec@ property.
topicCompression :: KafkaCompressionCodec -> ProducerProperties
topicCompression c =
  extraTopicProps (M.singleton "compression.codec" (kafkaCompressionCodecToText c))

-- | Set the topic-level @message.timeout.ms@ property.
sendTimeout :: Timeout -> ProducerProperties
sendTimeout (Timeout t) =
  extraTopicProps (M.singleton "message.timeout.ms" (decimalText t))

-- | Set the @statistics.interval.ms@ property.
statisticsInterval :: Millis -> ProducerProperties
statisticsInterval (Millis t) =
  extraProps (M.singleton "statistics.interval.ms" (decimalText t))

-- | Set arbitrary Kafka-level properties.
extraProps :: Map Text Text -> ProducerProperties
extraProps m = mempty { ppKafkaProps = m }

-- | Set one arbitrary Kafka-level property.
extraProp :: Text -> Text -> ProducerProperties
extraProp k v = mempty { ppKafkaProps = M.singleton k v }

-- | Suppress disconnect log noise in legacy configuration.
suppressDisconnectLogs :: ProducerProperties
suppressDisconnectLogs =
  extraProps (M.singleton "log.connection.close" "false")

-- | Set arbitrary topic-level properties.
extraTopicProps :: Map Text Text -> ProducerProperties
extraTopicProps m = mempty { ppTopicProps = m }

-- | Set legacy debug contexts.
debugOptions :: [KafkaDebug] -> ProducerProperties
debugOptions [] = extraProps M.empty
debugOptions d =
  extraProps (M.singleton "debug" (T.intercalate "," (map kafkaDebugToText d)))

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _ = Just x
Nothing <|> y = y
