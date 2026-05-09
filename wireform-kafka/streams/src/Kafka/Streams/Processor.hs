module Kafka.Streams.Processor
  ( Processor(..)
  ) where

import Kafka.Streams.Types (Record)

class Monad m => Processor m k v where
  process :: Record k v -> m ()


