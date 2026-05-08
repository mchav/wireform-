-- |
-- Module      : Kafka.Streams.DSL.Materialized
-- Description : @Materialized<K,V,S>@ DSL config — how to materialize a
--               KStream/KTable into a state store
module Kafka.Streams.DSL.Materialized
  ( Materialized (..)
  , materialized
  , materializedAs
  , withCachingDisabled
  , withLoggingDisabled
  , withRetention
  ) where

import Data.Int (Int64)

import Kafka.Streams.Serde (Serde)
import Kafka.Streams.State.Store (StoreName, storeName)

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
  }

-- | Named 'Materialized' — equivalent to @Materialized.as("name")@.
materializedAs :: StoreName -> Materialized k v
materializedAs n = materialized { matName = Just n }

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
