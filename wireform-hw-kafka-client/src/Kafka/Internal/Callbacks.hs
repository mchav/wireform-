{- |
Module      : Kafka.Internal.Callbacks
Description : Internal callback representation for the hw-kafka facade.
-}
module Kafka.Internal.Callbacks (
  Callback (..),
  errorCallbacks,
  deliveryCallbacks,
  rebalanceCallbacks,
  offsetCommitCallbacks,
) where

import Data.ByteString (ByteString)
import Kafka.Consumer.Types (
  KafkaConsumer,
  RebalanceEvent,
  TopicPartition,
 )
import Kafka.Producer.Types (DeliveryReport)
import Kafka.Types (KafkaError, KafkaLogLevel)


{- | Internal representation of legacy callbacks.

The public "Kafka.Callbacks" module exports the type abstractly, as
@hw-kafka-client@ did, while producer and consumer callback builder
modules construct the supported variants.
-}
data Callback
  = ErrorCallback (KafkaError -> String -> IO ())
  | LogCallback (KafkaLogLevel -> String -> String -> IO ())
  | StatsCallback (ByteString -> IO ())
  | DeliveryCallback (DeliveryReport -> IO ())
  | RebalanceCallback (KafkaConsumer -> RebalanceEvent -> IO ())
  | OffsetCommitCallback (KafkaConsumer -> KafkaError -> [TopicPartition] -> IO ())


errorCallbacks :: [Callback] -> [KafkaError -> String -> IO ()]
errorCallbacks =
  foldr collect []
  where
    collect (ErrorCallback f) fs = f : fs
    collect _ fs = fs


deliveryCallbacks :: [Callback] -> [DeliveryReport -> IO ()]
deliveryCallbacks =
  foldr collect []
  where
    collect (DeliveryCallback f) fs = f : fs
    collect _ fs = fs


rebalanceCallbacks :: [Callback] -> [KafkaConsumer -> RebalanceEvent -> IO ()]
rebalanceCallbacks =
  foldr collect []
  where
    collect (RebalanceCallback f) fs = f : fs
    collect _ fs = fs


offsetCommitCallbacks :: [Callback] -> [KafkaConsumer -> KafkaError -> [TopicPartition] -> IO ()]
offsetCommitCallbacks =
  foldr collect []
  where
    collect (OffsetCommitCallback f) fs = f : fs
    collect _ fs = fs
