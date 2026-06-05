{-|
Module      : Kafka.Consumer.Callbacks
Description : Consumer callback compatibility helpers.

This module preserves the @hw-kafka-client@ callback constructors while
users migrate to wireform-native rebalance and commit hooks.
-}
module Kafka.Consumer.Callbacks
  ( rebalanceCallback
  , offsetCommitCallback
  , module X
  ) where

import Kafka.Callbacks as X
import Kafka.Consumer.Types
  ( KafkaConsumer
  , RebalanceEvent
  , TopicPartition
  )
import Kafka.Internal.Compat (Callback (..))
import Kafka.Types (KafkaError)

-- | Set a legacy rebalance callback token.
rebalanceCallback :: (KafkaConsumer -> RebalanceEvent -> IO ()) -> Callback
rebalanceCallback _ = Callback

-- | Set a legacy offset-commit callback token.
offsetCommitCallback :: (KafkaConsumer -> KafkaError -> [TopicPartition] -> IO ()) -> Callback
offsetCommitCallback _ = Callback
