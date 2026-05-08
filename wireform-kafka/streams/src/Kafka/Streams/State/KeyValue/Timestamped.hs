{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.State.KeyValue.Timestamped
-- Description : Timestamped 'KeyValueStore' (KIP-258)
--
-- A 'TimestampedKeyValueStore k v' is a 'KeyValueStore k (ValueAndTimestamp v)'
-- with helpers to put / get a typed value alongside its timestamp.
-- Mirrors the Java
-- @org.apache.kafka.streams.state.TimestampedKeyValueStore<K,V>@.
--
-- The wrapper around an arbitrary 'KeyValueStore k v' allows an
-- in-memory or persistent or RocksDB-backed store to carry
-- timestamps without the underlying store knowing about it.
module Kafka.Streams.State.KeyValue.Timestamped
  ( ValueAndTimestamp (..)
  , wrapVT
  , unwrapVT
  , TimestampedKeyValueStore
  , timestampedFromKV
  , putT
  , getT
  , latestTimestamp
  ) where

import Data.IORef
import GHC.Generics (Generic)

import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  )
import Kafka.Streams.Time (Timestamp (..))

-- | A typed value alongside the timestamp it was last written at.
data ValueAndTimestamp v = ValueAndTimestamp
  { vatValue     :: !v
  , vatTimestamp :: !Timestamp
  }
  deriving stock (Eq, Show, Generic)

wrapVT :: v -> Timestamp -> ValueAndTimestamp v
wrapVT v t = ValueAndTimestamp v t

unwrapVT :: ValueAndTimestamp v -> (v, Timestamp)
unwrapVT (ValueAndTimestamp v t) = (v, t)

-- | A 'KeyValueStore' specialised to 'ValueAndTimestamp' values
-- gets the convenience accessors below.
type TimestampedKeyValueStore k v = KeyValueStore k (ValueAndTimestamp v)

-- | Lift any 'KeyValueStore k (ValueAndTimestamp v)' into a
-- 'TimestampedKeyValueStore'. The function is identity at the type
-- level; it exists for documentation.
timestampedFromKV
  :: KeyValueStore k (ValueAndTimestamp v)
  -> TimestampedKeyValueStore k v
timestampedFromKV = id

-- | Put a value and its timestamp into a timestamped store.
putT :: TimestampedKeyValueStore k v -> k -> v -> Timestamp -> IO ()
putT s k v t = kvsPut s k (ValueAndTimestamp v t)

-- | Get the value and timestamp pair for a key, if any.
getT :: TimestampedKeyValueStore k v -> k -> IO (Maybe (v, Timestamp))
getT s k = (fmap unwrapVT) <$> kvsGet s k

-- | Get just the timestamp at which the key was last updated.
latestTimestamp
  :: TimestampedKeyValueStore k v -> k -> IO (Maybe Timestamp)
latestTimestamp s k = (fmap snd) <$> getT s k

-- 'IORef' kept imported in case future helpers want to lift this
-- into a stateful per-store 'maxTimestamp' counter.
_keepIORef :: IORef Int -> IO ()
_keepIORef r = readIORef r >>= writeIORef r