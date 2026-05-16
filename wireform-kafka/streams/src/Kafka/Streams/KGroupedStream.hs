{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.KGroupedStream
-- Description : KGroupedStream + aggregation surface
--
-- @
-- KGroupedStream k v
-- @
--
-- represents a stream that has been re-keyed (or kept on its key) and
-- is ready for aggregation. The aggregation primitives (@count@,
-- @reduce@, @aggregate@) materialise their output into a 'KTable'
-- backed by a 'KeyValueStore'.
--
-- Mirrors @org.apache.kafka.streams.kstream.KGroupedStream<K,V>@.
module Kafka.Streams.KGroupedStream
  ( KGroupedStream
  , kgsParent
  , kgsKey
  , kgsValue
  , kgsBuilder
    -- * From 'KStream'
  , groupByKey
  , groupByStream
    -- * Aggregations
  , countStream
  , reduceStream
  , aggregateStream
    -- * Windowed
  , windowedByTime
  , windowedBySession
  , TimeWindowedKStream (..)
  , SessionWindowedKStream (..)
  , CountedTableLocal (..)
  ) where

import Data.IORef
import Data.Int (Int64)
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.Grouped (Grouped (..))
import Kafka.Streams.KStream
  ( KStream
  , kstreamBuilder
  , kstreamKeySerde
  , kstreamParent
  , kstreamValueSerde
  , selectKey
  )
import Kafka.Streams.Materialized
  ( Materialized (..)
  )
import Kafka.Streams.StreamsBuilder
  ( StreamsBuilder
  , freshNodeName
  , freshStoreName
  , withTopology_
  )
import Kafka.Streams.Processor
  ( Processor (..)
  , forwardRecord
  , processorName
  , getStateStore
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
import Kafka.Streams.Types (Record (..))
import Kafka.Streams.Window (Windows (..), SessionWindows (..))

-- | A stream that has been grouped on a key.
data KGroupedStream k v = KGroupedStream
  { kgsParent  :: !Topo.NodeName
  , kgsKey     :: !(Serde k)
  , kgsValue   :: !(Serde v)
  , kgsBuilder :: !StreamsBuilder
  }

-- | Group records by their existing key. No repartition is
-- needed in the single-task driver model; the broker-backed
-- runtime emits a repartition node automatically when the key
-- has been altered upstream.
--
-- /JVM equivalent:/ @KStream.groupByKey(Grouped)@.
groupByKey :: Grouped k v -> KStream k v -> KGroupedStream k v
groupByKey g s = KGroupedStream
  { kgsParent  = kstreamParent s
  , kgsKey     = groupedKeySerde g
  , kgsValue   = groupedValueSerde g
  , kgsBuilder = kstreamBuilder s
  }

-- | Group by a key derived from each record. Inserts a
-- 'selectKey' processor first.
--
-- /JVM equivalent:/ @KStream.groupBy(KeyValueMapper, Grouped)@.
groupByStream
  :: (Record k v -> k')
  -> Grouped k' v
  -> KStream k v
  -> IO (KGroupedStream k' v)
groupByStream f g s = do
  s' <- selectKey f s
  pure KGroupedStream
    { kgsParent  = kstreamParent s'
    , kgsKey     = groupedKeySerde g
    , kgsValue   = groupedValueSerde g
    , kgsBuilder = kstreamBuilder s'
    }

----------------------------------------------------------------------
-- Aggregations (non-windowed → KTable)
----------------------------------------------------------------------

-- | Count the records per key.
--
-- /JVM equivalent:/ @KGroupedStream.count(Materialized)@.
--
-- The result lives in a 'KeyValueStore' the runtime addresses by
-- 'matName' (synthesised if absent).
countStream
  :: forall k v
   . Ord k
  => Materialized k Int64
  -> KGroupedStream k v
  -> IO (CountedTableLocal k Int64)
countStream m kgs = do
  storeNm <- maybe (freshStoreName (kgsBuilder kgs) "KSTREAM-AGGREGATE-STORE")
                   pure
                   (matName m)
  let supplier = inMemoryKeyValueStoreBuilder storeNm
                   :: StoreBuilderKV k Int64
  nodeNm <- freshNodeName (kgsBuilder kgs) "KSTREAM-AGGREGATE"
  withTopology_ (kgsBuilder kgs) $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = nodeNm
                  , Topo.processorSpecParents  = [kgsParent kgs]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (countProcessor @k @v @Int64 storeNm)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreKV supplier [nodeNm] t1
     in t2
  pure CountedTableLocal
    { ctlNode = nodeNm
    , ctlStore = storeNm
    , ctlBuilder = kgsBuilder kgs
    }

-- | Combine values for the same key with a binary reducer.
--
-- /JVM equivalent:/ @KGroupedStream.reduce(Reducer, Materialized)@.
reduceStream
  :: forall k v
   . Ord k
  => (v -> v -> v)
  -> Materialized k v
  -> KGroupedStream k v
  -> IO (CountedTableLocal k v)
reduceStream combine m kgs = do
  storeNm <- maybe (freshStoreName (kgsBuilder kgs) "KSTREAM-REDUCE-STORE")
                   pure
                   (matName m)
  let supplier = inMemoryKeyValueStoreBuilder storeNm :: StoreBuilderKV k v
  nodeNm <- freshNodeName (kgsBuilder kgs) "KSTREAM-REDUCE"
  withTopology_ (kgsBuilder kgs) $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = nodeNm
                  , Topo.processorSpecParents  = [kgsParent kgs]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (reduceProcessor @k @v storeNm combine)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreKV supplier [nodeNm] t1
     in t2
  pure CountedTableLocal
    { ctlNode = nodeNm
    , ctlStore = storeNm
    , ctlBuilder = kgsBuilder kgs
    }

-- | Stateful fold: seed an accumulator and update it per
-- record using the supplied aggregator.
--
-- /JVM equivalent:/ @KGroupedStream.aggregate(Initializer, Aggregator, Materialized)@.
aggregateStream
  :: forall k v a
   . Ord k
  => IO a                         -- ^ initialiser (Java @Initializer<A>@)
  -> (k -> v -> a -> a)           -- ^ aggregator (Java @Aggregator<K,V,A>@)
  -> Materialized k a
  -> KGroupedStream k v
  -> IO (CountedTableLocal k a)
aggregateStream initial agg m kgs = do
  storeNm <- maybe (freshStoreName (kgsBuilder kgs) "KSTREAM-AGGREGATE-STORE")
                   pure
                   (matName m)
  let supplier = inMemoryKeyValueStoreBuilder storeNm :: StoreBuilderKV k a
  nodeNm <- freshNodeName (kgsBuilder kgs) "KSTREAM-AGGREGATE"
  withTopology_ (kgsBuilder kgs) $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = nodeNm
                  , Topo.processorSpecParents  = [kgsParent kgs]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (aggregateProcessor @k @v @a storeNm initial agg)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreKV supplier [nodeNm] t1
     in t2
  pure CountedTableLocal
    { ctlNode = nodeNm
    , ctlStore = storeNm
    , ctlBuilder = kgsBuilder kgs
    }

----------------------------------------------------------------------
-- Aggregation processors
----------------------------------------------------------------------

-- | The 'countProcessor' takes 3 type vars but the input @v@ is
-- ignored — the processor only counts.
countProcessor
  :: forall k v out
   . (Ord k, out ~ Int64)
  => StoreName
  -> IO (Processor k v)
countProcessor sn = aggregateProcessor @k @v @out sn (pure 0) (\_ _ acc -> acc + 1)

reduceProcessor
  :: forall k v
   . Ord k
  => StoreName
  -> (v -> v -> v)
  -> IO (Processor k v)
reduceProcessor sn combine = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  pure Processor
    { procName  = processorName "KSTREAM-REDUCE"
    , procInit  = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx sn
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef
              (Just (unsafeCastKV kvs :: KeyValueStore k v))
          _ -> error $ "KGroupedStream.reduce: store not found: " <> show sn
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just kvs, Just k) -> do
            !mPrev <- kvsGet kvs k
            let !next = case mPrev of
                  Nothing -> recordValue r
                  Just p  -> combine p (recordValue r)
            kvsPut kvs k next
            forwardRecord ctx r { recordValue = next }
          _ -> pure ()
    }

aggregateProcessor
  :: forall k v a
   . Ord k
  => StoreName
  -> IO a
  -> (k -> v -> a -> a)
  -> IO (Processor k v)
aggregateProcessor sn initial agg = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k a))
  pure Processor
    { procName  = processorName "KSTREAM-AGGREGATE"
    , procInit  = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx sn
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (unsafeCastKV kvs))
          _ -> error $ "aggregate: store not found: " <> show sn
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just kvs, Just k) -> do
            mPrev <- kvsGet kvs k
            !cur <- case mPrev of
              Nothing -> initial
              Just p  -> pure p
            let !next = agg k (recordValue r) cur
            kvsPut kvs k next
            forwardRecord ctx r { recordValue = next }
          _ -> pure ()
    }

-- | Type-erased cast across the engine boundary. The DSL guarantees
-- that the store's actual key/value types match the processor's at
-- the call site (both are inserted by the same operation).
unsafeCastKV :: KeyValueStore k v -> KeyValueStore k' v'
unsafeCastKV = Unsafe.unsafeCoerce
{-# INLINE unsafeCastKV #-}

----------------------------------------------------------------------
-- Windowed grouping
----------------------------------------------------------------------

-- | Window the grouped stream by time (tumbling / hopping /
-- sliding — selected via the 'Windows' value).
--
-- /JVM equivalent:/ @KGroupedStream.windowedBy(Windows)@.
windowedByTime
  :: Windows
  -> KGroupedStream k v
  -> TimeWindowedKStream k v
windowedByTime ws kgs = TimeWindowedKStream
  { twksParent  = kgsParent kgs
  , twksKey     = kgsKey kgs
  , twksValue   = kgsValue kgs
  , twksBuilder = kgsBuilder kgs
  , twksWindows = ws
  }

-- | Window the grouped stream by session (gap-based) windows.
--
-- /JVM equivalent:/ @KGroupedStream.windowedBy(SessionWindows)@.
windowedBySession
  :: SessionWindows
  -> KGroupedStream k v
  -> SessionWindowedKStream k v
windowedBySession sw kgs = SessionWindowedKStream
  { swksParent  = kgsParent kgs
  , swksKey     = kgsKey kgs
  , swksValue   = kgsValue kgs
  , swksBuilder = kgsBuilder kgs
  , swksWindows = sw
  }

----------------------------------------------------------------------
-- Window / session structure (defined here to avoid dep cycles)
----------------------------------------------------------------------

data TimeWindowedKStream k v = TimeWindowedKStream
  { twksParent   :: !Topo.NodeName
  , twksKey      :: !(Serde k)
  , twksValue    :: !(Serde v)
  , twksBuilder  :: !StreamsBuilder
  , twksWindows  :: !Windows
  }

data SessionWindowedKStream k v = SessionWindowedKStream
  { swksParent   :: !Topo.NodeName
  , swksKey      :: !(Serde k)
  , swksValue    :: !(Serde v)
  , swksBuilder  :: !StreamsBuilder
  , swksWindows  :: !SessionWindows
  }

----------------------------------------------------------------------
-- Returned aggregate handle
----------------------------------------------------------------------

-- | Result of an aggregation. Carries the materialised store name so
-- callers can later 'queryKeyValueStore' or convert to a 'KStream'.
data CountedTableLocal k v = CountedTableLocal
  { ctlNode    :: !Topo.NodeName
  , ctlStore   :: !StoreName
  , ctlBuilder :: !StreamsBuilder
  }

-- We re-export the aggregation result as 'KTable' from the
-- 'Kafka.Streams.KTable' module; here we keep the local name so
-- the test driver can introspect.
