{- |
Module      : Kafka.Consumer.ConsumerProperties
Description : Consumer configuration builders from @hw-kafka-client@.

This module preserves the legacy @hw-kafka-client@ consumer property
surface for migration. Properties are translated into
"Kafka.Client.Consumer" where possible; librdkafka callback polling and
callback values are kept only as compatibility tokens.
-}
module Kafka.Consumer.ConsumerProperties (
  ConsumerProperties (..),
  CallbackPollMode (..),
  brokersList,
  autoCommit,
  noAutoCommit,
  noAutoOffsetStore,
  groupId,
  clientId,
  setCallback,
  logLevel,
  compression,
  suppressDisconnectLogs,
  statisticsInterval,
  extraProps,
  extraProp,
  debugOptions,
  queuedMaxMessagesKBytes,
  callbackPollMode,
  module X,
) where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Consumer.Callbacks as X
import Kafka.Consumer.Types (ConsumerGroupId (..))
import Kafka.Internal.Callbacks (Callback)
import Kafka.Internal.Compat (decimalText)
import Kafka.Types (
  BrokerAddress (..),
  ClientId (..),
  KafkaCompressionCodec (..),
  KafkaDebug (..),
  KafkaLogLevel (..),
  Millis (..),
  kafkaCompressionCodecToText,
  kafkaDebugToText,
 )


{- | Whether legacy callback polling should be synchronous or asynchronous.

The native wireform facade does not run librdkafka callbacks, but the
setting is preserved so existing configuration code compiles.
-}
data CallbackPollMode
  = CallbackPollModeSync
  | CallbackPollModeAsync
  deriving (Show, Eq)


-- | Properties used to create a 'Kafka.Consumer.Types.KafkaConsumer'.
data ConsumerProperties = ConsumerProperties
  { cpProps :: Map Text Text
  -- ^ Consumer configuration properties.
  , cpLogLevel :: Maybe KafkaLogLevel
  -- ^ Legacy log level.
  , cpCallbacks :: [Callback]
  -- ^ Legacy callback tokens.
  , cpCallbackPollMode :: CallbackPollMode
  -- ^ Legacy callback polling mode.
  }


instance Semigroup ConsumerProperties where
  ConsumerProperties m1 ll1 cb1 _ <> ConsumerProperties m2 ll2 cb2 cup2 =
    ConsumerProperties (M.union m2 m1) (ll2 <|> ll1) (cb1 <> cb2) cup2


instance Monoid ConsumerProperties where
  mempty =
    ConsumerProperties
      { cpProps = M.empty
      , cpLogLevel = Nothing
      , cpCallbacks = []
      , cpCallbackPollMode = CallbackPollModeAsync
      }


-- | Set the list of brokers to contact to connect to the Kafka cluster.
brokersList :: [BrokerAddress] -> ConsumerProperties
brokersList bs =
  extraProps (M.singleton "bootstrap.servers" brokers)
  where
    brokers = T.intercalate "," (map unBrokerAddress bs)


-- | Enable auto commit and set @auto.commit.interval.ms@.
autoCommit :: Millis -> ConsumerProperties
autoCommit (Millis ms) =
  extraProps
    ( M.fromList
        [ ("enable.auto.commit", "true")
        , ("auto.commit.interval.ms", decimalText ms)
        ]
    )


-- | Disable @enable.auto.commit@.
noAutoCommit :: ConsumerProperties
noAutoCommit =
  extraProps (M.singleton "enable.auto.commit" "false")


-- | Disable @enable.auto.offset.store@.
noAutoOffsetStore :: ConsumerProperties
noAutoOffsetStore =
  extraProps (M.singleton "enable.auto.offset.store" "false")


-- | Set the consumer @group.id@.
groupId :: ConsumerGroupId -> ConsumerProperties
groupId (ConsumerGroupId cid) =
  extraProps (M.singleton "group.id" cid)


-- | Set the consumer @client.id@.
clientId :: ClientId -> ConsumerProperties
clientId (ClientId cid) =
  extraProps (M.singleton "client.id" cid)


-- | Set a consumer callback token.
setCallback :: Callback -> ConsumerProperties
setCallback cb = mempty {cpCallbacks = [cb]}


-- | Set the legacy logging level.
logLevel :: KafkaLogLevel -> ConsumerProperties
logLevel ll = mempty {cpLogLevel = Just ll}


-- | Set the @compression.codec@ property.
compression :: KafkaCompressionCodec -> ConsumerProperties
compression c =
  extraProps (M.singleton "compression.codec" (kafkaCompressionCodecToText c))


-- | Suppress disconnect log noise in legacy configuration.
suppressDisconnectLogs :: ConsumerProperties
suppressDisconnectLogs =
  extraProps (M.singleton "log.connection.close" "false")


-- | Set @statistics.interval.ms@.
statisticsInterval :: Millis -> ConsumerProperties
statisticsInterval (Millis t) =
  extraProps (M.singleton "statistics.interval.ms" (decimalText t))


-- | Set arbitrary consumer properties.
extraProps :: Map Text Text -> ConsumerProperties
extraProps m = mempty {cpProps = m}


-- | Set one arbitrary consumer property.
extraProp :: Text -> Text -> ConsumerProperties
extraProp k v = mempty {cpProps = M.singleton k v}


-- | Set legacy debug contexts.
debugOptions :: [KafkaDebug] -> ConsumerProperties
debugOptions [] = extraProps M.empty
debugOptions d =
  extraProps (M.singleton "debug" (T.intercalate "," (map kafkaDebugToText d)))


-- | Set @queued.max.messages.kbytes@.
queuedMaxMessagesKBytes :: Int -> ConsumerProperties
queuedMaxMessagesKBytes kBytes =
  extraProp "queued.max.messages.kbytes" (decimalText kBytes)


-- | Set the legacy callback poll mode.
callbackPollMode :: CallbackPollMode -> ConsumerProperties
callbackPollMode mode = mempty {cpCallbackPollMode = mode}


(<|>) :: Maybe a -> Maybe a -> Maybe a
Just x <|> _ = Just x
Nothing <|> y = y
