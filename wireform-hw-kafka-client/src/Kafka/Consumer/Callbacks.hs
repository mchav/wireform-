{- |
Module      : Kafka.Consumer.Callbacks
Description : Consumer callback compatibility helpers.

This module preserves the @hw-kafka-client@ callback constructors while
users migrate to wireform-native rebalance and commit hooks. Rebalance
and commit callbacks are attached to the native consumer where the
facade has equivalent lifecycle events.
-}
module Kafka.Consumer.Callbacks (
  rebalanceCallback,
  offsetCommitCallback,
  module X,
) where

import Kafka.Callbacks as X
import Kafka.Consumer.Types (
  KafkaConsumer,
  RebalanceEvent,
  TopicPartition,
 )
import Kafka.Internal.Callbacks (Callback (..))
import Kafka.Types (KafkaError)


-- | Set a legacy rebalance callback token.
rebalanceCallback :: (KafkaConsumer -> RebalanceEvent -> IO ()) -> Callback
rebalanceCallback = RebalanceCallback


-- | Set a legacy offset-commit callback token.
offsetCommitCallback :: (KafkaConsumer -> KafkaError -> [TopicPartition] -> IO ()) -> Callback
offsetCommitCallback = OffsetCommitCallback
