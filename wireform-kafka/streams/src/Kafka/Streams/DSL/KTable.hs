{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.DSL.KTable
-- Description : KTable DSL surface
--
-- A 'KTable' is the changelog view of a 'KStream': for each key, only
-- the latest value matters. KTables are always backed by a state
-- store, and the DSL operations preserve that invariant.
--
-- Mirrors @org.apache.kafka.streams.kstream.KTable<K,V>@.
module Kafka.Streams.DSL.KTable
  ( KTable
  , ktableNode
  , ktableStore
  , ktableBuilder
    -- * Source
  , tableFromTopic
    -- * Stateless
  , filterTable
  , mapValuesTable
    -- * Conversion
  , toStreamTable
  , queryKeyValueStore
  ) where

import Data.IORef
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.DSL.Consumed (Consumed (..))
import Kafka.Streams.DSL.Materialized (Materialized (..))
import Kafka.Streams.DSL.StreamsBuilder
  ( StreamsBuilder
  , freshNodeName
  , freshStoreName
  , withTopology_
  )
import Kafka.Streams.Processor
  ( Processor (..)
  , forwardRecord
  , getStateStore
  , processorName
  )
import Kafka.Streams.Serde (Serde)
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
import Kafka.Streams.Types
  ( Record (..)
  , TopicName
  , mapValue
  )

-- | KTable handle.
data KTable k v = KTable
  { ktableNode    :: !Topo.NodeName
  , ktableStore   :: !StoreName
  , ktableBuilder :: !StreamsBuilder
  , ktableKeySerde   :: !(Serde k)
  , ktableValueSerde :: !(Serde v)
  }

-- | Materialise a topic as a 'KTable'. Each (key,value) updates the
-- store; tombstones (null value) delete the key.
tableFromTopic
  :: forall k v
   . Ord k
  => StreamsBuilder
  -> TopicName
  -> Consumed k v
  -> Materialized k v
  -> IO (KTable k v)
tableFromTopic b topic c m = do
  storeNm <- maybe (freshStoreName b "KTABLE-SOURCE-STORE")
                   pure
                   (matName m)
  let supplier = inMemoryKeyValueStoreBuilder storeNm :: StoreBuilderKV k v
  sourceNm <- freshNodeName b "KTABLE-SOURCE"
  procNm   <- freshNodeName b "KTABLE-SOURCE-PROCESSOR"
  withTopology_ b $ \t ->
    let !t1 = Topo.addSource sourceNm [topic]
                (consumedKeySerde c)
                (consumedValueSerde c)
                (consumedExtractor c) t
        !t2 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [sourceNm]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (sourceTableProcessor @k @v storeNm)
                  , Topo.processorSpecStores   = []
                  }
                t1
        !t3 = Topo.addStateStoreKV supplier [procNm] t2
     in t3
  pure KTable
    { ktableNode       = procNm
    , ktableStore      = storeNm
    , ktableBuilder    = b
    , ktableKeySerde   = consumedKeySerde c
    , ktableValueSerde = consumedValueSerde c
    }

sourceTableProcessor
  :: forall k v
   . Ord k
  => StoreName -> IO (Processor k v)
sourceTableProcessor sn = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef Nothing
  pure Processor
    { procName  = processorName "KTABLE-SOURCE-PROCESSOR"
    , procInit  = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx sn
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (unsafeCastKV kvs :: KeyValueStore k v))
          _ -> error $ "KTable.source: store not found: " <> show sn
    , procClose = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just kvs, Just k) -> do
            kvsPut kvs k (recordValue r)
            forwardRecord ctx r
          _ -> pure ()
    }

----------------------------------------------------------------------
-- Stateless
----------------------------------------------------------------------

filterTable
  :: forall k v
   . Ord k
  => (Record k v -> Bool)
  -> Materialized k v
  -> KTable k v
  -> IO (KTable k v)
filterTable pred_ m parent = do
  storeNm <- maybe (freshStoreName (ktableBuilder parent) "KTABLE-FILTER-STORE")
                   pure
                   (matName m)
  let b = ktableBuilder parent
      supplier = inMemoryKeyValueStoreBuilder storeNm :: StoreBuilderKV k v
  procNm <- freshNodeName b "KTABLE-FILTER"
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [ktableNode parent]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (filterTableProcessor @k @v storeNm pred_)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreKV supplier [procNm] t1
     in t2
  pure KTable
    { ktableNode       = procNm
    , ktableStore      = storeNm
    , ktableBuilder    = b
    , ktableKeySerde   = ktableKeySerde parent
    , ktableValueSerde = ktableValueSerde parent
    }

filterTableProcessor
  :: forall k v
   . Ord k
  => StoreName
  -> (Record k v -> Bool)
  -> IO (Processor k v)
filterTableProcessor sn pred_ = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef Nothing
  pure Processor
    { procName = processorName "KTABLE-FILTER"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx sn
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (unsafeCastKV kvs :: KeyValueStore k v))
          _ -> error $ "KTable.filter: store not found: " <> show sn
    , procClose = pure ()
    , procProcess = \r ->
        case recordKey r of
          Nothing -> pure ()
          Just k  -> do
            mst <- readIORef storeRef
            mctx <- readIORef ctxRef
            case (mst, mctx) of
              (Just kvs, Just ctx) ->
                if pred_ r
                  then do
                    kvsPut kvs k (recordValue r)
                    forwardRecord ctx r
                  else do
                    -- drop and tombstone (Java semantics)
                    _ <- kvsDelete kvs k
                    pure ()
              _ -> pure ()
    }

mapValuesTable
  :: forall k v v'
   . Ord k
  => (v -> v')
  -> Materialized k v'
  -> KTable k v
  -> IO (KTable k v')
mapValuesTable f m parent = do
  storeNm <- maybe (freshStoreName (ktableBuilder parent) "KTABLE-MAPVAL-STORE")
                   pure
                   (matName m)
  let b = ktableBuilder parent
      supplier = inMemoryKeyValueStoreBuilder storeNm :: StoreBuilderKV k v'
  procNm <- freshNodeName b "KTABLE-MAPVAL"
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [ktableNode parent]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (mapValueTableProcessor @k @v @v' storeNm f)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreKV supplier [procNm] t1
     in t2
  pure KTable
    { ktableNode       = procNm
    , ktableStore      = storeNm
    , ktableBuilder    = b
    , ktableKeySerde   = ktableKeySerde parent
    , ktableValueSerde = error "KTable.mapValues: pass Materialized with serde to set output"
    }

mapValueTableProcessor
  :: forall k v v'
   . Ord k
  => StoreName
  -> (v -> v')
  -> IO (Processor k v)
mapValueTableProcessor sn f = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef Nothing
  pure Processor
    { procName = processorName "KTABLE-MAPVAL"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx sn
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (unsafeCastKV kvs :: KeyValueStore k v'))
          _ -> error $ "KTable.mapValues: store not found: " <> show sn
    , procClose = pure ()
    , procProcess = \r -> do
        let !v' = f (recordValue r)
        mst <- readIORef storeRef
        mctx <- readIORef ctxRef
        case (recordKey r, mst, mctx) of
          (Just k, Just kvs, Just ctx) -> do
            kvsPut kvs k v'
            forwardRecord ctx (mapValue (const v') r)
          _ -> pure ()
    }

----------------------------------------------------------------------
-- Conversion
----------------------------------------------------------------------

-- | Convert a 'KTable' to a 'KStream' carrying every change.
--
-- The resulting stream is a 'Kafka.Streams.DSL.KStream.KStream' but
-- we don't import that module here to avoid a cycle; the runtime
-- 'Kafka.Streams' umbrella re-exports a sibling helper.
toStreamTable :: KTable k v -> (Topo.NodeName, StreamsBuilder, Serde k, Serde v)
toStreamTable kt =
  ( ktableNode kt
  , ktableBuilder kt
  , ktableKeySerde kt
  , ktableValueSerde kt
  )

-- | Snapshot the table store and look up a key. Useful inside
-- 'TopologyTestDriver' to assert intermediate state.
queryKeyValueStore
  :: Ord k
  => KTable k v
  -> AnyStateStore
  -> k
  -> IO (Maybe v)
queryKeyValueStore _ (AnyKeyValueStore kvs) k =
  kvsGet (unsafeCastKV kvs) k
queryKeyValueStore _ _ _ =
  pure Nothing

unsafeCastKV :: KeyValueStore k v -> KeyValueStore k' v'
unsafeCastKV = Unsafe.unsafeCoerce
{-# INLINE unsafeCastKV #-}
