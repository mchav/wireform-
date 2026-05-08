module Kafka.Streams.KTable
  ( KTable(..)
  ) where

newtype KTable m k v = KTable { runKTable :: m () }


