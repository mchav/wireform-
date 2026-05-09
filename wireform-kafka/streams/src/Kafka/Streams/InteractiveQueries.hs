{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.InteractiveQueries
-- Description : Read-only access to live state stores from outside
--               the stream thread
--
-- Mirrors @org.apache.kafka.streams.KafkaStreams.store(...)@. From a
-- running 'KafkaStreams' instance you can obtain a typed read-only
-- handle to any state store the topology declared. This is what the
-- Java docs call "interactive queries".
--
-- == Concurrency model
--
-- The in-memory stores are backed by 'Data.IORef.IORef' over
-- 'Data.Map.Strict'.  All writes go through 'atomicModifyIORef'',
-- and reads through 'readIORef' — which means a query thread can
-- always observe a consistent snapshot of the map at the moment it
-- reads, with the same /linearisability/ guarantees as the rest of
-- the engine.
--
-- The persistent file-backed store is similarly thread-safe for
-- reads (the in-memory shadow map is updated atomically; a query
-- thread reads from the same shadow).
--
-- /Iterators/ returned by 'queryRange' / 'queryAll' are eager
-- snapshots (the underlying iterator is materialised at iterator-
-- creation time), so they don't observe writes that happen after
-- iterator creation but before iteration completes.
--
-- == Multi-task caveat
--
-- The current single-task runtime exposes /one/ instance of every
-- store. A future multi-task runtime will need to surface a
-- distributed-store discovery layer: this module's API is
-- intentionally compatible with that future expansion (the user
-- never sees the underlying 'KeyValueStore' concretely; they query
-- through helpers that can later be re-routed to remote tasks).
module Kafka.Streams.InteractiveQueries
  ( -- * Querying a 'KafkaStreams' instance
    queryKVStore
  , queryWindowStore
  , querySessionStore
    -- * Read-only handles
  , ReadOnlyKeyValueStore (..)
  , ReadOnlyWindowStore (..)
  , ReadOnlySessionStore (..)
  , readOnlyKV
  , readOnlyWindow
  , readOnlySession
    -- * Errors
  , StoreNotFound (..)
  , StoreTypeMismatch (..)
    -- * Lower-level (pre-running runtime)
  , queryEngineStore
  ) where

import Control.Exception (Exception)
import Data.IORef (readIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.Internal.Engine
  ( Engine
  , StoreEntry (..)
  , storeByName
  )
import Kafka.Streams.Runtime
  ( KafkaStreams
  , ksEngine
  )
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueIterator
  , KeyValueStore (..)
  , SessionKey
  , SessionStore (..)
  , StoreName
  , WindowStore (..)
  , WindowedKey
  )
import Kafka.Streams.Time (Timestamp)

----------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------

newtype StoreNotFound = StoreNotFound StoreName
  deriving stock Show
  deriving anyclass Exception

data StoreTypeMismatch = StoreTypeMismatch !StoreName !String
  deriving stock Show
  deriving anyclass Exception

----------------------------------------------------------------------
-- Read-only handles
----------------------------------------------------------------------

-- | The read-only side of a 'KeyValueStore'. Same shape as Java's
-- @ReadOnlyKeyValueStore@.
data ReadOnlyKeyValueStore k v = ReadOnlyKeyValueStore
  { roKvGet   :: !(k -> IO (Maybe v))
  , roKvRange :: !(k -> k -> IO (KeyValueIterator k v))
  , roKvAll   :: !(IO (KeyValueIterator k v))
  , roKvCount :: !(IO Int64)
  }

readOnlyKV :: KeyValueStore k v -> ReadOnlyKeyValueStore k v
readOnlyKV kvs = ReadOnlyKeyValueStore
  { roKvGet   = kvsGet kvs
  , roKvRange = kvsRange kvs
  , roKvAll   = kvsAll kvs
  , roKvCount = kvsApproxEntries kvs
  }

data ReadOnlyWindowStore k v = ReadOnlyWindowStore
  { roWsFetch         :: !(k -> Timestamp -> IO (Maybe v))
  , roWsFetchRange    :: !(k -> Timestamp -> Timestamp -> IO (KeyValueIterator Timestamp v))
  , roWsFetchAllRange :: !(Timestamp -> Timestamp -> IO (KeyValueIterator (WindowedKey k) v))
  , roWsAll           :: !(IO (KeyValueIterator (WindowedKey k) v))
  }

readOnlyWindow :: WindowStore k v -> ReadOnlyWindowStore k v
readOnlyWindow ws = ReadOnlyWindowStore
  { roWsFetch         = wsFetch ws
  , roWsFetchRange    = wsFetchRange ws
  , roWsFetchAllRange = wsFetchAllRange ws
  , roWsAll           = wsAll ws
  }

data ReadOnlySessionStore k v = ReadOnlySessionStore
  { roSsFetchSession    :: !(SessionKey k -> IO (Maybe v))
  , roSsFindSessions    :: !(k -> Timestamp -> Timestamp -> IO (KeyValueIterator (SessionKey k) v))
  , roSsFindAllSessions :: !(Timestamp -> Timestamp -> IO (KeyValueIterator (SessionKey k) v))
  }

readOnlySession :: SessionStore k v -> ReadOnlySessionStore k v
readOnlySession ss = ReadOnlySessionStore
  { roSsFetchSession    = ssFetchSession ss
  , roSsFindSessions    = ssFindSessions ss
  , roSsFindAllSessions = ssFindAllSessions ss
  }

----------------------------------------------------------------------
-- Querying a running KafkaStreams
----------------------------------------------------------------------

-- | Resolve a store from a 'KafkaStreams' by name. Returns 'Nothing'
-- if the runtime is not yet running or the store doesn't exist.
queryKVStore
  :: forall k v
   . KafkaStreams
  -> StoreName
  -> IO (Maybe (ReadOnlyKeyValueStore k v))
queryKVStore ks sn = do
  mEng <- readIORef (ksEngine ks)
  case mEng of
    Nothing  -> pure Nothing
    Just eng -> queryEngineStore @k @v eng sn

queryWindowStore
  :: forall k v
   . KafkaStreams
  -> StoreName
  -> IO (Maybe (ReadOnlyWindowStore k v))
queryWindowStore ks sn = do
  mEng <- readIORef (ksEngine ks)
  case mEng of
    Nothing -> pure Nothing
    Just eng -> do
      m <- storeByName eng sn
      case storeEntryAny <$> m of
        Just (AnyWindowStore ws) ->
          pure (Just (readOnlyWindow (Unsafe.unsafeCoerce ws)))
        _ -> pure Nothing

querySessionStore
  :: forall k v
   . KafkaStreams
  -> StoreName
  -> IO (Maybe (ReadOnlySessionStore k v))
querySessionStore ks sn = do
  mEng <- readIORef (ksEngine ks)
  case mEng of
    Nothing -> pure Nothing
    Just eng -> do
      m <- storeByName eng sn
      case storeEntryAny <$> m of
        Just (AnySessionStore ss) ->
          pure (Just (readOnlySession (Unsafe.unsafeCoerce ss)))
        _ -> pure Nothing

-- | Lower-level: query an 'Engine' directly. Useful from inside
-- 'TopologyTestDriver' tests where there isn't a 'KafkaStreams' but
-- there is an engine.
queryEngineStore
  :: forall k v
   . Engine
  -> StoreName
  -> IO (Maybe (ReadOnlyKeyValueStore k v))
queryEngineStore eng sn = do
  m <- storeByName eng sn
  case storeEntryAny <$> m of
    Just (AnyKeyValueStore kvs) ->
      pure (Just (readOnlyKV (Unsafe.unsafeCoerce kvs)))
    _ -> pure Nothing

-- 'Map.empty' touched so unused-imports stays quiet should we drop
-- one of the helpers above; trivial constant.
_keepMap :: Map.Map () ()
_keepMap = Map.empty