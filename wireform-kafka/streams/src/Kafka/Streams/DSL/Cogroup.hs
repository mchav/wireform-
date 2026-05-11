{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.DSL.Cogroup
-- Description : Co-grouped aggregations across multiple streams (KIP-150)
--
-- A 'CogroupedStream k a' captures a set of pre-grouped streams,
-- each with their own value type, all feeding the same aggregator
-- state of type @a@. Calling 'aggregateCogrouped' builds a single
-- output 'KTable' whose value is updated by /any/ of the source
-- streams via the source-specific aggregator.
--
-- Mirrors @KGroupedStream.cogroup(...)@ +
-- @CogroupedKStream.aggregate(...)@.
module Kafka.Streams.DSL.Cogroup
  ( CogroupedStream
  , cogroup
  , addCogrouped
  , aggregateCogrouped
    -- * Windowed cogroup (KIP-150)
  , TimeWindowedCogroupedStream (..)
  , windowedByCogroup
  ) where

import Data.IORef
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.DSL.KGroupedStream
  ( CountedTableLocal (..)
  , KGroupedStream
  , kgsBuilder
  , kgsParent
  )
import Kafka.Streams.DSL.Materialized
  ( Materialized (..)
  )
import Kafka.Streams.DSL.StreamsBuilder
  ( StreamsBuilder
  , freshNodeName
  , freshStoreName
  , withTopology_
  )
import qualified Kafka.Streams.Window
import Kafka.Streams.Processor
  ( Processor (..)
  , forwardRecord
  , getStateStore
  , processorName
  )
import Kafka.Streams.State.KeyValue.InMemory
  ( inMemoryKeyValueStoreBuilder
  )
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueStore (..)
  , StoreBuilderKV
  , StoreName
  )
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (Record (..))

-- | One element of a cogroup: a typed grouped stream paired with
-- its source-specific aggregator.
data CogroupSource k a where
  CogroupSource
    :: KGroupedStream k v
    -> (k -> v -> a -> a)
    -> CogroupSource k a

-- | A cogroup-in-progress. The 'a' type is the shared aggregator
-- state; each entry in 'cgsSources' contributes a different source
-- value type via the existential carrier.
data CogroupedStream k a = CogroupedStream
  { cgsBuilder :: !StreamsBuilder
  , cgsSources :: ![CogroupSource k a]
  }

-- | Start a cogroup with one source.
cogroup
  :: KGroupedStream k v
  -> (k -> v -> a -> a)
  -> CogroupedStream k a
cogroup kgs agg = CogroupedStream
  { cgsBuilder = kgsBuilder kgs
  , cgsSources = [CogroupSource kgs agg]
  }

-- | Add another source to an in-progress cogroup. The new source
-- can have a different value type but must share the aggregator
-- state type @a@.
addCogrouped
  :: CogroupedStream k a
  -> KGroupedStream k v
  -> (k -> v -> a -> a)
  -> CogroupedStream k a
addCogrouped cgs kgs agg = cgs
  { cgsSources = cgsSources cgs ++ [CogroupSource kgs agg]
  }

-- | Build the cogroup's output table. Mirrors
-- @CogroupedKStream.aggregate(initializer, Materialized)@.
aggregateCogrouped
  :: forall k a
   . Ord k
  => IO a                               -- initialiser
  -> Materialized k a
  -> CogroupedStream k a
  -> IO (CountedTableLocal k a)
aggregateCogrouped initial m cgs = do
  let b = cgsBuilder cgs
  storeNm <- maybe (freshStoreName b "KSTREAM-COGROUP-STORE")
                   pure
                   (matName m)
  let supplier = inMemoryKeyValueStoreBuilder storeNm
                   :: StoreBuilderKV k a
  -- For each source we register the per-side processor immediately,
  -- and only export the side's NODE NAME outside the existential.
  -- That way the source's @v@ never escapes.
  ownerNms <- traverse
    (\src -> do
        nm <- freshNodeName b "KSTREAM-COGROUP-SIDE"
        addCogrupSideToTopology b nm storeNm initial src
        pure nm)
    (cgsSources cgs)
  withTopology_ b $ \t ->
    Topo.addStateStoreKV supplier ownerNms t
  pure CountedTableLocal
    { ctlNode    = case ownerNms of
                     []      -> error "aggregateCogrouped: no sources"
                     (nm : _) -> nm
    , ctlStore   = storeNm
    , ctlBuilder = b
    }

addCogrupSideToTopology
  :: forall k a
   . Ord k
  => StreamsBuilder
  -> Topo.NodeName              -- side processor name
  -> StoreName
  -> IO a
  -> CogroupSource k a
  -> IO ()
addCogrupSideToTopology b nm storeNm initial (CogroupSource kgs agg) =
  withTopology_ b $
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [kgsParent kgs]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor (cogroupSideProc @k storeNm initial agg)
        , Topo.processorSpecStores   = [storeNm]
        }

-- | The processor body for a single side of the cogroup. Reads the
-- shared store, calls this side's aggregator, and writes back.
cogroupSideProc
  :: forall k v a
   . Ord k
  => StoreName
  -> IO a
  -> (k -> v -> a -> a)
  -> IO (Processor k v)
cogroupSideProc sn initial agg = do
  ctxRef   <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k a))
  pure Processor
    { procName = processorName "KSTREAM-COGROUP-SIDE"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx sn >>= \case
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "cogroup: store missing: " <> show sn
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just k  -> do
          mctx <- readIORef ctxRef
          mst  <- readIORef storeRef
          case (mctx, mst) of
            (Just ctx, Just kvs) -> do
              mPrev <- kvsGet kvs k
              !cur  <- maybe initial pure mPrev
              let !next = agg k (recordValue r) cur
              kvsPut kvs k next
              forwardRecord ctx r { recordValue = next }
            _ -> pure ()
    }

----------------------------------------------------------------------
-- Windowed cogroup (KIP-150)
----------------------------------------------------------------------

-- | A 'CogroupedStream' with an attached 'Windows' definition.
-- Mirrors Java's 'TimeWindowedCogroupedKStream'. The aggregate
-- output is a windowed KTable produced by
-- 'aggregateWindowedCogrouped' (a follow-up combinator).
data TimeWindowedCogroupedStream k a = TimeWindowedCogroupedStream
  { twcsInner   :: !(CogroupedStream k a)
  , twcsWindows :: !Kafka.Streams.Window.Windows
  }

-- | @CogroupedStream.windowedBy(windows)@: attach a 'Windows'
-- to a cogrouped stream so the subsequent aggregation produces
-- a windowed KTable. The actual windowed aggregation is
-- implemented by combining the per-side processors with the
-- existing 'aggregateWindowed' implementation; this carrier
-- type is the type-level entry point that mirrors the JVM
-- contract.
windowedByCogroup
  :: Kafka.Streams.Window.Windows
  -> CogroupedStream k a
  -> TimeWindowedCogroupedStream k a
windowedByCogroup ws cg = TimeWindowedCogroupedStream
  { twcsInner   = cg
  , twcsWindows = ws
  }
