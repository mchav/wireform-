module Kafka.Streams.KStream
  ( KStream(..)
  , mapValues
  , filterValues
  ) where

newtype KStream m k v = KStream { runKStream :: m () }

mapValues :: Monad m => (v -> v') -> KStream m k v -> KStream m k v'
mapValues _ (KStream m) = KStream m

filterValues :: Monad m => (v -> Bool) -> KStream m k v -> KStream m k v
filterValues _ (KStream m) = KStream m


