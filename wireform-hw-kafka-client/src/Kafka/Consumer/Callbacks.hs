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

rebalanceCallback :: (KafkaConsumer -> RebalanceEvent -> IO ()) -> Callback
rebalanceCallback _ = Callback

offsetCommitCallback :: (KafkaConsumer -> KafkaError -> [TopicPartition] -> IO ()) -> Callback
offsetCommitCallback _ = Callback
