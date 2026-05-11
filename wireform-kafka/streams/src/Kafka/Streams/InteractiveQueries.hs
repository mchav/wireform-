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
    -- * StoreQueryParameters (KIP-535)
  , StoreQueryParameters (..)
  , storeQueryParameters
  , queryKVStoreWithParameters
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
import qualified Data.Foldable as Foldable
import Data.IORef (newIORef, readIORef, writeIORef)
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
  , ksPool
  )
import qualified Kafka.Streams.Runtime.WorkerPool
import Kafka.Streams.Runtime.WorkerPool
  ( poolWorkers
  , workerEngine
  )
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueIterator (..)
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
-- | KIP-535 @StoreQueryParameters@ — the parameter bag the JVM
-- @KafkaStreams.store(...)@ entry-point takes. Mirrors the
-- builder shape so call sites read like the Java original.
data StoreQueryParameters = StoreQueryParameters
  { sqpStoreName        :: !StoreName
  , sqpStaleStoresEnabled :: !Bool
    -- ^ When 'True', return state-store handles even when the
    --   instance is still recovering / rebalancing. Mirrors
    --   @withStaleStoresEnabled()@.
  , sqpPartition        :: !(Maybe Int)
    -- ^ When 'Just', restrict the query to a single
    --   partition. Mirrors @withPartition(int)@.
  }
  deriving stock (Eq, Show)

-- | Build a 'StoreQueryParameters' from a store name; defaults
-- match Java's 'StoreQueryParameters.fromNameAndType' shape.
storeQueryParameters :: StoreName -> StoreQueryParameters
storeQueryParameters n = StoreQueryParameters
  { sqpStoreName          = n
  , sqpStaleStoresEnabled = False
  , sqpPartition          = Nothing
  }

-- | Resolve a key-value store using the full
-- 'StoreQueryParameters' bag. Currently we ignore the
-- partition selector (the local-instance store is partition-
-- agnostic in our model) and 'sqpStaleStoresEnabled' (we don't
-- gate IQ on the instance's state), but the call site is
-- forward-compatible with both knobs once the underlying
-- runtime grows partition-aware IQ.
queryKVStoreWithParameters
  :: forall k v
   . KafkaStreams
  -> StoreQueryParameters
  -> IO (Maybe (ReadOnlyKeyValueStore k v))
queryKVStoreWithParameters ks p = queryKVStore @k @v ks (sqpStoreName p)

queryKVStore
  :: forall k v
   . KafkaStreams
  -> StoreName
  -> IO (Maybe (ReadOnlyKeyValueStore k v))
queryKVStore ks sn = do
  mEng <- readIORef (ksEngine ks)
  case mEng of
    Just eng -> queryEngineStore @k @v eng sn
    Nothing  -> do
      -- No single-thread engine: must be a multi-thread runtime
      -- with a worker pool. Federate across worker engines (same
      -- shape as Java's @CompositeReadOnlyKeyValueStore@: a
      -- 'roKvGet' tries each task in turn, a 'roKvAll' chains
      -- iterators across tasks).
      mPool <- readIORef (ksPool ks)
      case mPool of
        Nothing   -> pure Nothing
        Just pool -> federatedKV @k @v pool sn

-- | Build a federated 'ReadOnlyKeyValueStore' that delegates to
-- every worker's engine. Keys live on exactly one worker (the
-- one the corresponding partition hashes to), so 'roKvGet'
-- finds at most one match. 'roKvAll' chains all workers'
-- iterators in deterministic worker-id order, matching the JVM
-- composite-store ordering across local tasks.
federatedKV
  :: forall k v
   . Kafka.Streams.Runtime.WorkerPool.WorkerPool
  -> StoreName
  -> IO (Maybe (ReadOnlyKeyValueStore k v))
federatedKV pool sn = do
  let !ws = poolWorkers pool
  perWorker <- traverse
    (\w -> queryEngineStore @k @v (workerEngine w) sn)
    (Foldable.toList ws)
  case [ s | Just s <- perWorker ] of
    []     -> pure Nothing
    stores -> pure $ Just $ ReadOnlyKeyValueStore
      { roKvGet   = \k -> firstHit (map (\s -> roKvGet s k) stores)
      , roKvRange = \lo hi -> do
          its <- traverse (\s -> roKvRange s lo hi) stores
          chainIterators its
      , roKvAll = do
          its <- traverse roKvAll stores
          chainIterators its
      , roKvCount = sumCounts stores
      }
  where
    sumCounts ss = do
      cs <- traverse roKvCount ss
      pure $! sum cs

-- | First IO action that yields 'Just'.
firstHit :: [IO (Maybe a)] -> IO (Maybe a)
firstHit []       = pure Nothing
firstHit (a : as) = do
  r <- a
  case r of
    Just _  -> pure r
    Nothing -> firstHit as

-- | Sequence a list of iterators into one. Each iterator is
-- consumed in turn; we close it when it's drained.
chainIterators :: [KeyValueIterator k v] -> IO (KeyValueIterator k v)
chainIterators its0 = do
  ref <- newIORef its0
  pure KeyValueIterator
    { kvIterNext = pump ref
    , kvIterClose = do
        rs <- readIORef ref
        mapM_ kvIterClose rs
        writeIORef ref []
    }
  where
    pump ref = do
      rs <- readIORef ref
      case rs of
        []       -> pure Nothing
        (it : rest) -> do
          mNext <- kvIterNext it
          case mNext of
            Just _  -> pure mNext
            Nothing -> do
              kvIterClose it
              writeIORef ref rest
              pump ref

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