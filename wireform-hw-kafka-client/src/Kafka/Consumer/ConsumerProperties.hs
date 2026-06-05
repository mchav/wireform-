module Kafka.Consumer.ConsumerProperties
  ( ConsumerProperties (..)
  , CallbackPollMode (..)
  , brokersList
  , autoCommit
  , noAutoCommit
  , noAutoOffsetStore
  , groupId
  , clientId
  , setCallback
  , logLevel
  , compression
  , suppressDisconnectLogs
  , statisticsInterval
  , extraProps
  , extraProp
  , debugOptions
  , queuedMaxMessagesKBytes
  , callbackPollMode
  , module X
  ) where

import Data.Map (Map)
import Data.Text (Text)
import Kafka.Consumer.Callbacks as X
import Kafka.Consumer.Types (ConsumerGroupId (..))
import Kafka.Internal.Compat (Callback, decimalText)
import Kafka.Types
  ( BrokerAddress (..)
  , ClientId (..)
  , KafkaCompressionCodec (..)
  , KafkaDebug (..)
  , KafkaLogLevel (..)
  , Millis (..)
  , kafkaCompressionCodecToText
  , kafkaDebugToText
  )
import qualified Data.Map as M
import qualified Data.Text as T

data CallbackPollMode
  = CallbackPollModeSync
  | CallbackPollModeAsync
  deriving (Show, Eq)

data ConsumerProperties = ConsumerProperties
  { cpProps :: Map Text Text
  , cpLogLevel :: Maybe KafkaLogLevel
  , cpCallbacks :: [Callback]
  , cpCallbackPollMode :: CallbackPollMode
  }

instance Semigroup ConsumerProperties where
  ConsumerProperties m1 ll1 cb1 _ <> ConsumerProperties m2 ll2 cb2 cup2 =
    ConsumerProperties (M.union m2 m1) (ll2 <|> ll1) (cb1 <> cb2) cup2

instance Monoid ConsumerProperties where
  mempty = ConsumerProperties
    { cpProps = M.empty
    , cpLogLevel = Nothing
    , cpCallbacks = []
    , cpCallbackPollMode = CallbackPollModeAsync
    }

brokersList :: [BrokerAddress] -> ConsumerProperties
brokersList bs =
  extraProps (M.singleton "bootstrap.servers" brokers)
  where
    brokers = T.intercalate "," (map unBrokerAddress bs)

autoCommit :: Millis -> ConsumerProperties
autoCommit (Millis ms) =
  extraProps (M.fromList
    [ ("enable.auto.commit", "true")
    , ("auto.commit.interval.ms", decimalText ms)
    ])

noAutoCommit :: ConsumerProperties
noAutoCommit =
  extraProps (M.singleton "enable.auto.commit" "false")

noAutoOffsetStore :: ConsumerProperties
noAutoOffsetStore =
  extraProps (M.singleton "enable.auto.offset.store" "false")

groupId :: ConsumerGroupId -> ConsumerProperties
groupId (ConsumerGroupId cid) =
  extraProps (M.singleton "group.id" cid)

clientId :: ClientId -> ConsumerProperties
clientId (ClientId cid) =
  extraProps (M.singleton "client.id" cid)

setCallback :: Callback -> ConsumerProperties
setCallback cb = mempty { cpCallbacks = [cb] }

logLevel :: KafkaLogLevel -> ConsumerProperties
logLevel ll = mempty { cpLogLevel = Just ll }

compression :: KafkaCompressionCodec -> ConsumerProperties
compression c =
  extraProps (M.singleton "compression.codec" (kafkaCompressionCodecToText c))

suppressDisconnectLogs :: ConsumerProperties
suppressDisconnectLogs =
  extraProps (M.singleton "log.connection.close" "false")

statisticsInterval :: Millis -> ConsumerProperties
statisticsInterval (Millis t) =
  extraProps (M.singleton "statistics.interval.ms" (decimalText t))

extraProps :: Map Text Text -> ConsumerProperties
extraProps m = mempty { cpProps = m }

extraProp :: Text -> Text -> ConsumerProperties
extraProp k v = mempty { cpProps = M.singleton k v }

debugOptions :: [KafkaDebug] -> ConsumerProperties
debugOptions [] = extraProps M.empty
debugOptions d =
  extraProps (M.singleton "debug" (T.intercalate "," (map kafkaDebugToText d)))

queuedMaxMessagesKBytes :: Int -> ConsumerProperties
queuedMaxMessagesKBytes kBytes =
  extraProp "queued.max.messages.kbytes" (decimalText kBytes)

callbackPollMode :: CallbackPollMode -> ConsumerProperties
callbackPollMode mode = mempty { cpCallbackPollMode = mode }

(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _ = Just x
Nothing <|> y = y
