{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Kafka.Streams.State.KeyValue.InMemory
-- Description : In-memory @KeyValueStore@
--
-- Single-task store backed by 'Data.Map.Strict'. Tasks are inherently
-- single-threaded inside Streams, so we use 'IORef' rather than STM /
-- MVar; this matches the Java in-memory store's lock-free design.
--
-- Use 'inMemoryKeyValueStoreBuilder' to declare a store inside a
-- topology — the runtime calls 'sbKvBuild' once per assigned task.
module Kafka.Streams.State.KeyValue.InMemory
  ( inMemoryKeyValueStore
  , inMemoryKeyValueStoreBuilder
  ) where

import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Int (Int64)

import Kafka.Streams.State.Store
  ( KeyValueIterator
  , KeyValueStore (..)
  , LoggingConfig
  , StateStore (..)
  , StoreBuilderKV (..)
  , StoreName
  , defaultLoggingConfig
  , kvIteratorFromList
  )

-- | Build a fresh in-memory key-value store with the given name.
inMemoryKeyValueStore
  :: forall k v
   . Ord k
  => StoreName
  -> IO (KeyValueStore k v)
inMemoryKeyValueStore nm = do
  ref <- newIORef (Map.empty :: Map k v)
  pure (mkStore nm ref)

mkStore
  :: forall k v
   . Ord k
  => StoreName
  -> IORef (Map k v)
  -> KeyValueStore k v
mkStore nm ref = KeyValueStore
  { kvsBase = StateStore
      { storeStoreName  = nm
      , storePersistent = False
      , storeFlush      = pure ()
      , storeClose      = writeIORef ref Map.empty
      }
  , kvsGet = \k -> Map.lookup k <$> readIORef ref
  , kvsPut = \k v -> atomicModifyIORef' ref $ \m ->
      let !m' = Map.insert k v m in (m', ())
  , kvsPutIfAbsent = \k v ->
      atomicModifyIORef' ref $ \m ->
        case Map.lookup k m of
          Just existing -> (m, Just existing)
          Nothing       ->
            let !m' = Map.insert k v m in (m', Nothing)
  , kvsDelete = \k ->
      atomicModifyIORef' ref $ \m ->
        case Map.lookup k m of
          Nothing  -> (m, Nothing)
          Just v   ->
            let !m' = Map.delete k m in (m', Just v)
  , kvsRange = \lo hi -> do
      m <- readIORef ref
      let inRange = Map.takeWhileAntitone (<= hi)
                  $ Map.dropWhileAntitone (< lo) m
      kvIteratorFromList (Map.toAscList inRange)
  , kvsAll = do
      m <- readIORef ref
      kvIteratorFromList (Map.toAscList m)
  , kvsApproxEntries = countMap ref
  }

countMap :: IORef (Map k v) -> IO Int64
countMap ref = (fromIntegral . Map.size) <$> readIORef ref

-- | 'StoreBuilderKV' for an in-memory store.  Logging defaults to
-- 'defaultLoggingConfig' (changelog enabled, compacted topic) so
-- in-memory stores still survive failover.
inMemoryKeyValueStoreBuilder
  :: Ord k
  => StoreName
  -> StoreBuilderKV k v
inMemoryKeyValueStoreBuilder nm = StoreBuilderKV
  { sbKvName    = nm
  , sbKvLogging = defaultLoggingConfig
  , sbKvBuild   = inMemoryKeyValueStore nm
  }

-- Silence unused-warning for KeyValueIterator/LoggingConfig imports
-- (used only via type signatures the haddock references).
_unused :: KeyValueIterator () () -> LoggingConfig -> ()
_unused _ _ = ()
