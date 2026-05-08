{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.DSL.DslStoreSuppliers
-- Description : Default store backend selection (KIP-1247)
--
-- A 'DslStoreSuppliers' value tells the DSL which store backend
-- to use when an operator needs to materialise its result but the
-- user didn't supply an explicit 'Materialized.withStoreSupplier'.
--
-- @
-- inMemoryDslStoreSuppliers   :: DslStoreSuppliers
-- persistentDslStoreSuppliers :: PersistentConfig -> DslStoreSuppliers
-- @
--
-- The 'StreamsBuilder' carries one 'DslStoreSuppliers' that
-- defaults to 'inMemoryDslStoreSuppliers'. Override it via
-- 'setDefaultDslStoreSuppliers' before adding operators.
module Kafka.Streams.DSL.DslStoreSuppliers
  ( DslStoreSuppliers (..)
  , inMemoryDslStoreSuppliers
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)

import qualified Kafka.Streams.State.KeyValue.InMemory as KVInMem
import qualified Kafka.Streams.State.Session.InMemory as SSInMem
import qualified Kafka.Streams.State.Window.InMemory as WSInMem
import Kafka.Streams.State.Store
  ( KeyValueStore
  , SessionStore
  , StoreName
  , WindowStore
  )

-- | Bundle of factories for the three store kinds. Each factory
-- takes the chosen store name (and any window-/session-specific
-- parameters) and returns a freshly-built backend.
data DslStoreSuppliers = DslStoreSuppliers
  { dssKeyValue :: !(forall k v. Ord k =>
       StoreName -> IO (KeyValueStore k v))
  , dssWindow   :: !(forall k v. Ord k =>
       StoreName -> Int64 -> Int64 -> IO (WindowStore k v))
  , dssSession  :: !(forall k v. Ord k =>
       StoreName -> Int64 -> IO (SessionStore k v))
  }

-- | The default supplier: every backend is the in-memory one. Same
-- behaviour the DSL had before 'DslStoreSuppliers' existed; the
-- factory exists so users can swap to 'persistentDslStoreSuppliers'
-- (or any other backend) for production.
inMemoryDslStoreSuppliers :: DslStoreSuppliers
inMemoryDslStoreSuppliers = DslStoreSuppliers
  { dssKeyValue = KVInMem.inMemoryKeyValueStore
  , dssWindow   = WSInMem.inMemoryWindowStore
  , dssSession  = SSInMem.inMemorySessionStore
  }
