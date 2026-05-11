{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.State.KeyValue.Versioned
-- Description : Versioned 'KeyValueStore' (KIP-889 / KIP-960)
--
-- A versioned store keeps a /history/ of values per key, indexed by
-- the as-of timestamp at which each value was written. Reads can
-- choose:
--
--   * 'getLatest' — the most recent value (regardless of timestamp).
--   * 'getAsOf'   — the value that was current at a given timestamp.
--   * 'getHistory' — the full history within a time band.
--
-- Old versions are dropped past the configured 'historyRetention'.
--
-- Mirrors @org.apache.kafka.streams.state.VersionedKeyValueStore@.
module Kafka.Streams.State.KeyValue.Versioned
  ( VersionedKeyValueStore (..)
  , VersionedConfig (..)
  , defaultVersionedConfig
  , inMemoryVersionedKeyValueStore
  , putV
  , getLatest
  , getAsOf
  , getHistory
  , VersionedRecord (..)
  ) where

import Data.IORef
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

import Kafka.Streams.State.Store
  ( StateStore (..)
  , StoreName
  )
import Kafka.Streams.Time (Timestamp (..))

----------------------------------------------------------------------
-- Config
----------------------------------------------------------------------

data VersionedConfig = VersionedConfig
  { historyRetention :: !Int64
    -- ^ How long (in milliseconds, relative to the latest seen
    -- timestamp) versions are retained. Older versions are dropped.
  }
  deriving stock Show

defaultVersionedConfig :: VersionedConfig
defaultVersionedConfig = VersionedConfig
  { historyRetention = 24 * 3600 * 1000 -- 1 day
  }

----------------------------------------------------------------------
-- Records
----------------------------------------------------------------------

-- | One historical value of a key.
data VersionedRecord v = VersionedRecord
  { vrValue       :: !v
  , vrValidFromTs :: !Timestamp
  }
  deriving stock (Eq, Show, Generic)

----------------------------------------------------------------------
-- Store
----------------------------------------------------------------------

-- | A versioned store. Internally holds @Map k (Map Timestamp v)@.
--
-- This is the in-memory variant; persistent backends slot in by
-- implementing the same interface.
data VersionedKeyValueStore k v = VersionedKeyValueStore
  { vkvBase    :: !StateStore
  , vkvPut     :: !(k -> v -> Timestamp -> IO ())
  , vkvDelete  :: !(k -> Timestamp -> IO ())
    -- ^ Tombstone the key at the supplied 'Timestamp' (KIP-889
    -- @VersionedKeyValueStore.delete(key, validTo)@). Reads
    -- with @validFrom <= ts < validTo@ continue to see the
    -- pre-delete versions; reads at @ts >= validTo@ see no
    -- value.
  , vkvGetLatest :: !(k -> IO (Maybe (VersionedRecord v)))
  , vkvGetAsOf  :: !(k -> Timestamp -> IO (Maybe (VersionedRecord v)))
  , vkvGetHistory
      :: !(k -> Timestamp -> Timestamp -> IO [VersionedRecord v])
  }

-- | Build a fresh in-memory versioned store.
inMemoryVersionedKeyValueStore
  :: forall k v
   . Ord k
  => StoreName
  -> VersionedConfig
  -> IO (VersionedKeyValueStore k v)
inMemoryVersionedKeyValueStore nm cfg = do
  -- Inner map keyed by timestamp so we can range-scan and prune.
  ref  <- newIORef (Map.empty :: Map k (Map Timestamp v))
  -- Track the latest seen timestamp across all keys for retention.
  obsRef <- newIORef (Timestamp minBound)
  let
    putImpl k v ts = do
      atomicModifyIORef' obsRef
        (\cur -> (max cur ts, ()))
      atomicModifyIORef' ref $ \m ->
        let inner0 = Map.findWithDefault Map.empty k m
            !inner1 = Map.insert ts v inner0
            !m1     = Map.insert k inner1 m
        in (m1, ())
      pruneOld
    pruneOld = do
      Timestamp now <- readIORef obsRef
      let !cutoff = Timestamp (now - historyRetention cfg)
      atomicModifyIORef' ref $ \m ->
        let !m' = Map.map
                    (Map.dropWhileAntitone (< cutoff))
                    m
            -- Also drop empty inner maps to keep the outer map tidy.
            !m'' = Map.filter (not . Map.null) m'
        in (m'', ())
    latestImpl k = do
      m <- readIORef ref
      case Map.lookup k m of
        Nothing -> pure Nothing
        Just inner -> case Map.maxViewWithKey inner of
          Just ((ts, v), _) -> pure (Just (VersionedRecord v ts))
          Nothing -> pure Nothing
    asOfImpl k asof = do
      m <- readIORef ref
      case Map.lookup k m of
        Nothing -> pure Nothing
        Just inner ->
          -- Largest entry whose ts <= asof.
          case Map.lookupLE asof inner of
            Just (ts, v) -> pure (Just (VersionedRecord v ts))
            Nothing      -> pure Nothing
    historyImpl k from to = do
      m <- readIORef ref
      case Map.lookup k m of
        Nothing -> pure []
        Just inner ->
          let slice = Map.takeWhileAntitone (<= to)
                    $ Map.dropWhileAntitone (<  from) inner
           in pure
                $ map (\(ts, v) -> VersionedRecord v ts)
                      (Map.toAscList slice)
    deleteImpl k ts = do
      -- KIP-889 delete(key, validTo): drop every version whose
      -- timestamp >= validTo. Versions before validTo stay
      -- queryable via getAsOf.
      atomicModifyIORef' obsRef
        (\cur -> (max cur ts, ()))
      atomicModifyIORef' ref $ \m ->
        case Map.lookup k m of
          Nothing    -> (m, ())
          Just inner ->
            let !kept = Map.takeWhileAntitone (< ts) inner
                !m'   = if Map.null kept
                          then Map.delete k m
                          else Map.insert k kept m
             in (m', ())
  pure VersionedKeyValueStore
    { vkvBase = StateStore
        { storeStoreName  = nm
        , storePersistent = False
        , storeFlush      = pure ()
        , storeClose      = writeIORef ref Map.empty
        }
    , vkvPut         = putImpl
    , vkvDelete      = deleteImpl
    , vkvGetLatest   = latestImpl
    , vkvGetAsOf     = asOfImpl
    , vkvGetHistory  = historyImpl
    }

-- | Convenience aliases that just dispatch to the record fields.

putV :: VersionedKeyValueStore k v -> k -> v -> Timestamp -> IO ()
putV = vkvPut

getLatest :: VersionedKeyValueStore k v -> k -> IO (Maybe (VersionedRecord v))
getLatest = vkvGetLatest

getAsOf
  :: VersionedKeyValueStore k v
  -> k -> Timestamp -> IO (Maybe (VersionedRecord v))
getAsOf = vkvGetAsOf

getHistory
  :: VersionedKeyValueStore k v
  -> k -> Timestamp -> Timestamp -> IO [VersionedRecord v]
getHistory = vkvGetHistory