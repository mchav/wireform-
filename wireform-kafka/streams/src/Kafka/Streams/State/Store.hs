{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Kafka.Streams.State.Store
-- Description : Abstract state store + builder
--
-- Mirrors the Java hierarchy
--
-- @
-- StateStore                            -- lifecycle (init / flush / close)
--   |- KeyValueStore<K,V>               -- get / put / delete / range / all
--   |- WindowStore<K,V>                 -- put@T / fetch@[t1..t2]
--   `- SessionStore<K,V>                -- put / findSessions / fetchSession
-- @
--
-- Concrete implementations live under "Kafka.Streams.State.KeyValue.*",
-- "Kafka.Streams.State.Window.*", "Kafka.Streams.State.Session.*". A
-- 'StoreBuilder' is the recipe the topology builder hands to each task
-- so that every task gets its own physical store instance.
module Kafka.Streams.State.Store
  ( -- * Generic store
    StoreName
  , storeName
  , unStoreName
  , StateStore (..)
    -- * Key-value store
  , KeyValueStore (..)
  , KeyValueIterator (..)
  , kvIteratorFromList
  , kvIteratorToList
  , kvIteratorClose
    -- * Window store
  , WindowStore (..)
  , WindowedKey (..)
  , WindowStoreIterator
    -- * Session store
  , SessionStore (..)
  , SessionKey (..)
    -- * Builders
  , StoreBuilder (..)
  , StoreBuilderKV (..)
  , StoreBuilderW (..)
  , StoreBuilderS (..)
  , LoggingConfig (..)
  , defaultLoggingConfig
    -- * Anonymous wrapper
  , AnyStateStore (..)
  , anyStoreName
  ) where

import Control.Exception (Exception)
import Data.Hashable (Hashable)
import Data.IORef (newIORef, atomicModifyIORef', writeIORef)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Streams.Time (Timestamp)

-- | Globally-unique store name. Must match the Java rule
-- (alphanumeric plus @-_@, no leading dot, length 1..249) but we
-- enforce non-emptiness only and let the topology validator catch
-- cross-store conflicts.
newtype StoreName = StoreName { unStoreName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

storeName :: Text -> StoreName
storeName = StoreName

-- | Lifecycle methods every store must implement.
--
-- 'storePersistent' lets the runtime decide whether to send the
-- changelog topic or not (in-memory stores still produce a changelog
-- by default for fault-tolerance; only opt-out via
-- 'LoggingConfig'). 'storeFlush' is invoked at every task commit;
-- 'storeClose' once at task shutdown.
data StateStore = StateStore
  { storeStoreName  :: !StoreName
  , storePersistent :: !Bool
  , storeFlush      :: !(IO ())
  , storeClose      :: !(IO ())
  }

-- | Forward iterator over a snapshot of store contents. Iterators
-- carry a finalizer because the persistent backends hold OS handles
-- (file descriptors, RocksDB iterators) that must be released.
data KeyValueIterator k v = KeyValueIterator
  { kvIterNext  :: !(IO (Maybe (k, v)))
  , kvIterClose :: !(IO ())
  }

kvIteratorClose :: KeyValueIterator k v -> IO ()
kvIteratorClose = kvIterClose

-- | Materialise a list as an iterator. Used by the in-memory stores
-- and tests.
kvIteratorFromList :: [(k, v)] -> IO (KeyValueIterator k v)
kvIteratorFromList xs0 = do
  ref <- newIORef xs0
  pure KeyValueIterator
    { kvIterNext = atomicModifyIORef' ref $ \xs ->
        case xs of
          []       -> ([], Nothing)
          (h : tl) -> (tl, Just h)
    , kvIterClose = writeIORef ref []
    }

-- | Drain an iterator into a list, closing it afterward.
kvIteratorToList :: KeyValueIterator k v -> IO [(k, v)]
kvIteratorToList it = go []
  where
    go acc = do
      mx <- kvIterNext it
      case mx of
        Nothing -> do
          kvIteratorClose it
          pure (reverse acc)
        Just kv -> go (kv : acc)

-- | Key-value store. The Java contract is that 'kvsPut' with a
-- 'Nothing' value deletes the key (tombstone semantics). We keep that
-- by routing through 'kvsDelete' when the value is 'Nothing'.
data KeyValueStore k v = KeyValueStore
  { kvsBase            :: !StateStore
  , kvsGet             :: !(k -> IO (Maybe v))
  , kvsPut             :: !(k -> v -> IO ())
  , kvsPutIfAbsent     :: !(k -> v -> IO (Maybe v))
  , kvsDelete          :: !(k -> IO (Maybe v))
  , kvsRange           :: !(k -> k -> IO (KeyValueIterator k v))
  , kvsAll             :: !(IO (KeyValueIterator k v))
  , kvsApproxEntries   :: !(IO Int64)
  }

-- | Window-keyed value: @(key, windowStart)@. Window length is
-- implicit and held by the store.
data WindowedKey k = WindowedKey
  { wkKey         :: !k
  , wkWindowStart :: !Timestamp
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | Iterator over windows of a single key (matches @WindowStoreIterator@).
type WindowStoreIterator v = KeyValueIterator Timestamp v

-- | Window store. Records are addressable by @(key, timestamp)@ and
-- expire after a retention period.
data WindowStore k v = WindowStore
  { wsBase           :: !StateStore
  , wsWindowSize     :: !Int64    -- ^ window length in millis
  , wsRetention      :: !Int64    -- ^ retention period in millis
  , wsPut            :: !(k -> v -> Timestamp -> IO ())
  , wsFetch          :: !(k -> Timestamp -> IO (Maybe v))
  , wsFetchRange     :: !(k -> Timestamp -> Timestamp -> IO (WindowStoreIterator v))
  , wsFetchAllRange  :: !(Timestamp -> Timestamp -> IO (KeyValueIterator (WindowedKey k) v))
  , wsAll            :: !(IO (KeyValueIterator (WindowedKey k) v))
  }

-- | Session-keyed value: @(key, [start, end])@.
data SessionKey k = SessionKey
  { skKey   :: !k
  , skStart :: !Timestamp
  , skEnd   :: !Timestamp
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | Session store. Sessions can be merged when a new record extends a
-- previous one; the store offers 'ssFindSessions' to discover all
-- sessions within a time band so the aggregator can do that merge.
data SessionStore k v = SessionStore
  { ssBase             :: !StateStore
  , ssRetention        :: !Int64
  , ssPut              :: !(SessionKey k -> v -> IO ())
  , ssRemove           :: !(SessionKey k -> IO ())
  , ssFetchSession     :: !(SessionKey k -> IO (Maybe v))
  , ssFindSessions     :: !(k -> Timestamp -> Timestamp -> IO (KeyValueIterator (SessionKey k) v))
  , ssFindAllSessions  :: !(Timestamp -> Timestamp -> IO (KeyValueIterator (SessionKey k) v))
  }

-- | Per-store logging settings (changelog topic).
--
-- @loggingEnabled = False@ skips the changelog entirely and the store
-- becomes /unrecoverable/ — equivalent to Java's
-- @withLoggingDisabled()@ on a 'Stores.persistentKeyValueStore'.
data LoggingConfig = LoggingConfig
  { loggingEnabled  :: !Bool
  , loggingTopicCfg :: ![(Text, Text)]
    -- ^ Extra topic configuration overrides for the changelog topic
    -- (e.g. @[(\"cleanup.policy\", \"compact\")]@).
  }
  deriving stock (Eq, Show, Generic)

defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig
  { loggingEnabled = True
  , loggingTopicCfg = [("cleanup.policy", "compact")]
  }

-- | Builder for a generic state store. The builder is what the
-- 'StreamsBuilder' / 'Topology' carries; the runtime calls
-- 'sbBuild' once per partition assignment to obtain a private store
-- instance.
data StoreBuilder = StoreBuilder
  { sbName    :: !StoreName
  , sbLogging :: !LoggingConfig
  , sbBuild   :: !(IO StateStore)
  }

-- | Typed key-value store builder.
data StoreBuilderKV k v = StoreBuilderKV
  { sbKvName    :: !StoreName
  , sbKvLogging :: !LoggingConfig
  , sbKvBuild   :: !(IO (KeyValueStore k v))
  }

-- | Typed window-store builder.
data StoreBuilderW k v = StoreBuilderW
  { sbWName    :: !StoreName
  , sbWLogging :: !LoggingConfig
  , sbWBuild   :: !(IO (WindowStore k v))
  }

-- | Typed session-store builder.
data StoreBuilderS k v = StoreBuilderS
  { sbSName    :: !StoreName
  , sbSLogging :: !LoggingConfig
  , sbSBuild   :: !(IO (SessionStore k v))
  }

-- | Existential wrapper used by the topology to keep
-- heterogeneously-typed stores in a single map.
data AnyStateStore where
  AnyKeyValueStore :: !(KeyValueStore k v) -> AnyStateStore
  AnyWindowStore   :: !(WindowStore   k v) -> AnyStateStore
  AnySessionStore  :: !(SessionStore  k v) -> AnyStateStore

anyStoreName :: AnyStateStore -> StoreName
anyStoreName = \case
  AnyKeyValueStore s -> storeStoreName (kvsBase s)
  AnyWindowStore   s -> storeStoreName (wsBase s)
  AnySessionStore  s -> storeStoreName (ssBase s)

-- | When user code asks for a store by the wrong type.
data StoreTypeMismatch = StoreTypeMismatch !StoreName !Text
  deriving stock (Show, Generic)
  deriving anyclass (Exception)
