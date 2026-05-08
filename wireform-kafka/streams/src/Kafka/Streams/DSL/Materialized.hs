-- |
-- Module      : Kafka.Streams.DSL.Materialized
-- Description : @Materialized<K,V,S>@ DSL config — how to materialize a
--               KStream/KTable into a state store
module Kafka.Streams.DSL.Materialized
  ( Materialized (..)
  , materialized
  , materializedAs
  , materializedWithSupplier
  , withCachingDisabled
  , withLoggingDisabled
  , withRetention
  , withStoreSupplier
  , withKeySerde
  , withValueSerde
  ) where

import Data.Int (Int64)

import Kafka.Streams.Serde (Serde)
import Kafka.Streams.State.Store
  ( KeyValueStore
  , StoreName
  , storeName
  )

-- | Materialised view config. The @s@ type parameter (Java's
-- @Materialized<K,V,S>@ third param) is encoded in our store-builder
-- choice at use sites (KV / Window / Session) so we don't carry it on
-- this record.
data Materialized k v = Materialized
  { matName             :: !(Maybe StoreName)
  , matKeySerde         :: !(Maybe (Serde k))
  , matValueSerde       :: !(Maybe (Serde v))
  , matCachingEnabled   :: !Bool
  , matLoggingEnabled   :: !Bool
  , matRetentionMs      :: !(Maybe Int64)
  , matStoreSupplier    :: !(Maybe (StoreName -> IO (KeyValueStore k v)))
    -- ^ User-supplied store builder. When set, DSL operators that
    -- materialise this 'Materialized' will use the supplied
    -- function instead of the default in-memory builder. This is
    -- the hook point for plugging in a 'PersistentKeyValueStore',
    -- 'CachingKeyValueStore', 'VersionedKeyValueStore', or
    -- 'TimestampedKeyValueStore'.
  }

-- | Anonymous 'Materialized' — runtime synthesises a unique store name.
materialized :: Materialized k v
materialized = Materialized
  { matName           = Nothing
  , matKeySerde       = Nothing
  , matValueSerde     = Nothing
  , matCachingEnabled = True
  , matLoggingEnabled = True
  , matRetentionMs    = Nothing
  , matStoreSupplier  = Nothing
  }

-- | Named 'Materialized' — equivalent to @Materialized.as("name")@.
materializedAs :: StoreName -> Materialized k v
materializedAs n = materialized { matName = Just n }

-- | 'Materialized' built with a user-supplied store factory. The
-- factory is given the store's chosen name (whether explicit via
-- 'materializedAs' or auto-synthesised by the DSL) and returns the
-- backing 'KeyValueStore'.
materializedWithSupplier
  :: (StoreName -> IO (KeyValueStore k v))
  -> Materialized k v
materializedWithSupplier f = materialized { matStoreSupplier = Just f }

-- | 'Materialized.withKeySerde' (KIP-182).
withKeySerde :: Serde k -> Materialized k v -> Materialized k v
withKeySerde s m = m { matKeySerde = Just s }

-- | 'Materialized.withValueSerde'.
withValueSerde :: Serde v -> Materialized k v -> Materialized k v
withValueSerde s m = m { matValueSerde = Just s }

-- | Same as 'materializedWithSupplier' but works on an existing
-- 'Materialized' value.
withStoreSupplier
  :: (StoreName -> IO (KeyValueStore k v))
  -> Materialized k v
  -> Materialized k v
withStoreSupplier f m = m { matStoreSupplier = Just f }

withCachingDisabled :: Materialized k v -> Materialized k v
withCachingDisabled m = m { matCachingEnabled = False }

withLoggingDisabled :: Materialized k v -> Materialized k v
withLoggingDisabled m = m { matLoggingEnabled = False }

withRetention :: Int64 -> Materialized k v -> Materialized k v
withRetention r m = m { matRetentionMs = Just r }

-- 'storeName' is re-exported so call sites can build store names
-- without importing 'Kafka.Streams.State.Store'.
_keep :: StoreName
_keep = storeName "internal"
