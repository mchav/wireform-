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
import Kafka.Internal.Compat (Callback, decimalText)
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

data ProducerProperties = ProducerProperties
  { ppKafkaProps :: Map Text Text
  , ppTopicProps :: Map Text Text
  , ppLogLevel :: Maybe KafkaLogLevel
  , ppCallbacks :: [Callback]
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

brokersList :: [BrokerAddress] -> ProducerProperties
brokersList bs =
  extraProps (M.singleton "bootstrap.servers" brokers)
  where
    brokers = T.intercalate "," (map unBrokerAddress bs)

setCallback :: Callback -> ProducerProperties
setCallback cb = mempty { ppCallbacks = [cb] }

logLevel :: KafkaLogLevel -> ProducerProperties
logLevel ll = mempty { ppLogLevel = Just ll }

compression :: KafkaCompressionCodec -> ProducerProperties
compression c = extraProps (M.singleton "compression.codec" (kafkaCompressionCodecToText c))

topicCompression :: KafkaCompressionCodec -> ProducerProperties
topicCompression c =
  extraTopicProps (M.singleton "compression.codec" (kafkaCompressionCodecToText c))

sendTimeout :: Timeout -> ProducerProperties
sendTimeout (Timeout t) =
  extraTopicProps (M.singleton "message.timeout.ms" (decimalText t))

statisticsInterval :: Millis -> ProducerProperties
statisticsInterval (Millis t) =
  extraProps (M.singleton "statistics.interval.ms" (decimalText t))

extraProps :: Map Text Text -> ProducerProperties
extraProps m = mempty { ppKafkaProps = m }

extraProp :: Text -> Text -> ProducerProperties
extraProp k v = mempty { ppKafkaProps = M.singleton k v }

suppressDisconnectLogs :: ProducerProperties
suppressDisconnectLogs =
  extraProps (M.singleton "log.connection.close" "false")

extraTopicProps :: Map Text Text -> ProducerProperties
extraTopicProps m = mempty { ppTopicProps = m }

debugOptions :: [KafkaDebug] -> ProducerProperties
debugOptions [] = extraProps M.empty
debugOptions d =
  extraProps (M.singleton "debug" (T.intercalate "," (map kafkaDebugToText d)))

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _ = Just x
Nothing <|> y = y
