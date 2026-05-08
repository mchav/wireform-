{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Stores
-- Description : Java-style 'Stores.*' factory helpers
--
-- Mirrors @org.apache.kafka.streams.state.Stores@. Re-exports each
-- backend's factory under the 'Stores.' namespace so user code can
-- write
--
-- @
-- import qualified Kafka.Streams.Stores as Stores
-- ...
-- store <- Stores.inMemoryKeyValueStore "my-counts"
-- builder <- Stores.inMemoryKeyValueStoreBuilder "my-counts"
-- @
--
-- without having to import each per-backend module individually.
module Kafka.Streams.Stores
  ( -- * Key-value
    inMemoryKeyValueStore
  , inMemoryKeyValueStoreBuilder
  , persistentKeyValueStore
  , persistentKeyValueStoreBuilder
  , cachingKeyValueStore
    -- * Window
  , inMemoryWindowStore
  , inMemoryWindowStoreBuilder
    -- * Session
  , inMemorySessionStore
  , inMemorySessionStoreBuilder
    -- * Versioned
  , versionedKeyValueStore
    -- * Common types re-exported
  , module Kafka.Streams.State.Store
  , KVPers.PersistentConfig (..)
  , KVPers.defaultPersistentConfig
  , KVCache.CachingConfig (..)
  , KVCache.defaultCachingConfig
  , KVCache.CacheEmitter
  , KVVer.VersionedConfig (..)
  , KVVer.defaultVersionedConfig
  , KVVer.VersionedKeyValueStore
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)

import qualified Kafka.Streams.State.KeyValue.Caching as KVCache
import qualified Kafka.Streams.State.KeyValue.InMemory as KVInMem
import qualified Kafka.Streams.State.KeyValue.Persistent as KVPers
import qualified Kafka.Streams.State.KeyValue.Versioned as KVVer
import qualified Kafka.Streams.State.Session.InMemory as SSInMem
import qualified Kafka.Streams.State.Window.InMemory as WSInMem
import Kafka.Streams.State.Store

----------------------------------------------------------------------

inMemoryKeyValueStore
  :: Ord k => StoreName -> IO (KeyValueStore k v)
inMemoryKeyValueStore = KVInMem.inMemoryKeyValueStore

inMemoryKeyValueStoreBuilder
  :: Ord k => StoreName -> StoreBuilderKV k v
inMemoryKeyValueStoreBuilder = KVInMem.inMemoryKeyValueStoreBuilder

persistentKeyValueStore
  :: StoreName
  -> KVPers.PersistentConfig
  -> IO (KeyValueStore ByteString ByteString)
persistentKeyValueStore = KVPers.persistentKeyValueStore

persistentKeyValueStoreBuilder
  :: StoreName
  -> KVPers.PersistentConfig
  -> StoreBuilderKV ByteString ByteString
persistentKeyValueStoreBuilder = KVPers.persistentKeyValueStoreBuilder

cachingKeyValueStore
  :: Ord k
  => KeyValueStore k v
  -> KVCache.CachingConfig
  -> KVCache.CacheEmitter k v
  -> IO (KeyValueStore k v)
cachingKeyValueStore = KVCache.cachingKeyValueStore

inMemoryWindowStore
  :: Ord k
  => StoreName -> Int64 -> Int64 -> IO (WindowStore k v)
inMemoryWindowStore = WSInMem.inMemoryWindowStore

inMemoryWindowStoreBuilder
  :: Ord k
  => StoreName -> Int64 -> Int64 -> StoreBuilderW k v
inMemoryWindowStoreBuilder = WSInMem.inMemoryWindowStoreBuilder

inMemorySessionStore
  :: Ord k => StoreName -> Int64 -> IO (SessionStore k v)
inMemorySessionStore = SSInMem.inMemorySessionStore

inMemorySessionStoreBuilder
  :: Ord k => StoreName -> Int64 -> StoreBuilderS k v
inMemorySessionStoreBuilder = SSInMem.inMemorySessionStoreBuilder

versionedKeyValueStore
  :: Ord k
  => StoreName
  -> KVVer.VersionedConfig
  -> IO (KVVer.VersionedKeyValueStore k v)
versionedKeyValueStore = KVVer.inMemoryVersionedKeyValueStore
