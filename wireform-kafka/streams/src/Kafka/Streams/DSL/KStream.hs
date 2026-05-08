{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.DSL.KStream
-- Description : The KStream DSL surface
--
-- @
-- KStream k v
-- @
--
-- is a never-ending sequence of @Record k v@ values flowing along a
-- single edge of the topology graph. Operations on a 'KStream'
-- register a new processor as a child of the upstream node and
-- return a 'KStream' pinned to that new node.
--
-- The DSL mirrors the Java methods on
-- @org.apache.kafka.streams.kstream.KStream<K,V>@:
--
--   * 'streamFromTopic'         — Java @StreamsBuilder.stream@
--   * 'filterStream'            — @KStream.filter@
--   * 'filterNotStream'         — @KStream.filterNot@
--   * 'mapValues'               — @KStream.mapValues@
--   * 'mapKeyValue'             — @KStream.map@
--   * 'flatMapValues'           — @KStream.flatMapValues@
--   * 'flatMapKeyValue'         — @KStream.flatMap@
--   * 'foreachStream'           — @KStream.foreach@
--   * 'peekStream'              — @KStream.peek@
--   * 'selectKey'               — @KStream.selectKey@
--   * 'mergeStreams'            — @KStream.merge@
--   * 'branchStream'            — @KStream.branch@
--   * 'toTopic'                 — @KStream.to@
--   * 'throughTopic'            — @KStream.through@
--   * 'groupByKey'              — @KStream.groupByKey@
--   * 'groupByStream'           — @KStream.groupBy@
--   * 'transformValuesStream'   — @KStream.transformValues@
module Kafka.Streams.DSL.KStream
  ( KStream (..)
  , kstreamParent
  , kstreamBuilder
  , kstreamKeySerde
  , kstreamValueSerde
    -- * Sources
  , streamFromTopic
    -- * Stateless transforms
  , filterStream
  , filterStreamNamed
  , filterNotStream
  , mapValues
  , mapValuesNamed
  , mapValuesM
  , mapKeyValue
  , mapKeyValueNamed
  , mapKeyValueM
  , flatMapValues
  , flatMapKeyValue
  , peekStream
  , peekStreamNamed
  , foreachStream
  , selectKey
  , selectKeyNamed
    -- * Composition
  , mergeStreams
  , branchStream
    -- * Sinks
  , toTopic
  , toTopicNamed
  , throughTopic
    -- * Conversions
  , toTable
  , repartition
    -- * Joins
  , joinKStreamKTable
  , leftJoinKStreamKTable
  , joinKStreamKStream
  , leftJoinKStreamKStream
  , outerJoinKStreamKStream
    -- * Branching
  , splitStream
  , Branched (..)
  , branchedFrom
    -- * Low-level access
  , transformValuesStream
  ) where

import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Kafka.Streams.DSL.Consumed
  ( Consumed (..)
  , consumed
  )
import Kafka.Streams.DSL.Joined
  ( JoinWindows (..)
  , Joined (..)
  )
import qualified Kafka.Streams.DSL.Named
import qualified Kafka.Streams.DSL.Named
import Kafka.Streams.DSL.Produced
  ( Produced (..)
  , produced
  )
import Kafka.Streams.DSL.StreamsBuilder
  ( StreamsBuilder
  , freshNodeName
  , freshStoreName
  , withTopology_
  )

import qualified Unsafe.Coerce as Unsafe

import qualified Kafka.Streams.DSL.KTable
import Kafka.Streams.DSL.KTable
  ( KTable (..)
  , ktableNode
  , ktableStore
  )
import Kafka.Streams.DSL.Materialized
  ( Materialized (..)
  )
import qualified Kafka.Streams.State.Store
import Kafka.Streams.Processor
  ( Processor (..)
  , ProcessorContext (..)
  , forwardRecord
  , forwardTo
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
  , StoreBuilderW
  , StoreName
  , WindowStore (..)
  , kvIteratorToList
  )
import Kafka.Streams.State.Window.InMemory
  ( inMemoryWindowStoreBuilder
  )
import Kafka.Streams.Time (Timestamp (..))
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types
  ( Record (..)
  , TopicName
  , mapValue
  )

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

-- | A handle to a stream node in the topology being built.
--
-- Note: the serde fields are intentionally lazy. After a 'mapValues'
-- the downstream value serde is unknown until the user attaches a
-- 'Produced' / 'Materialized' downstream — we encode that as a
-- thunk that errors out only if forced.
data KStream k v = KStream
  { kstreamBuilder    :: !StreamsBuilder
  , kstreamParent     :: !Topo.NodeName
  , kstreamKeySerde   :: ~(Serde k)
  , kstreamValueSerde :: ~(Serde v)
  }

----------------------------------------------------------------------
-- Sources
----------------------------------------------------------------------

streamFromTopic
  :: StreamsBuilder
  -> TopicName
  -> Consumed k v
  -> IO (KStream k v)
streamFromTopic b topic c = do
  nm <- maybe (freshNodeName b "KSTREAM-SOURCE")
              (pure . Topo.NodeName)
              (consumedNodeName c)
  withTopology_ b $
    Topo.addSource nm
                   [topic]
                   (consumedKeySerde c)
                   (consumedValueSerde c)
                   (consumedExtractor c)
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = consumedKeySerde c
    , kstreamValueSerde = consumedValueSerde c
    }

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Internal: register a new processor whose supplier creates a
-- 'Processor' whose /input/ matches @parent@. The new 'KStream'
-- carries the /output/ types declared by the supplied serdes.
--
-- The processor is responsible for forwarding records of the right
-- shape via 'forwardRecord' / 'ctxForward' — the engine itself is
-- type-erased.
attachProcessor
  :: KStream k v
  -> T.Text
  -> IO (Processor k v)
  -> Serde k'
  -> Serde v'
  -> IO (KStream k' v')
attachProcessor parent prefix supplier ks' vs' = do
  let b = kstreamBuilder parent
  nm <- freshNodeName b prefix
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent parent] supplier
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = ks'
    , kstreamValueSerde = vs'
    }

-- | Build a stateless 'Processor' that runs @f@ on each record and
-- forwards the result via 'ProcessorContext.forward'.
statelessProcessorM
  :: T.Text
  -> (Record k v -> Record k' v')
  -> IO (Processor k v)
statelessProcessorM nm f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName nm
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> forwardRecord ctx (f r)
    }

----------------------------------------------------------------------
-- Stateless transforms
----------------------------------------------------------------------

filterStream :: (Record k v -> Bool) -> KStream k v -> IO (KStream k v)
filterStream pred_ s =
  attachProcessor s "KSTREAM-FILTER"
    (filterProcessor "KSTREAM-FILTER" pred_)
    (kstreamKeySerde s) (kstreamValueSerde s)

filterNotStream :: (Record k v -> Bool) -> KStream k v -> IO (KStream k v)
filterNotStream pred_ = filterStream (not . pred_)

filterProcessor
  :: T.Text -> (Record k v -> Bool) -> IO (Processor k v)
filterProcessor nm p = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName nm
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> if p r then forwardRecord ctx r else pure ()
    }

mapValues :: forall k v v'. (v -> v') -> KStream k v -> IO (KStream k v')
mapValues f s = mapValuesM (pure . f) s

mapValuesM :: forall k v v'. (v -> IO v') -> KStream k v -> IO (KStream k v')
mapValuesM f s =
  attachProcessor s "KSTREAM-MAPVALUES"
    (mapValuesProc @k @v @v' f)
    (kstreamKeySerde s)
    (error "KStream.mapValues: downstream value Serde unset; supply via to/through")

-- Processor whose input is @Record k v@ and that forwards a
-- @Record k v'@ via 'ctxForward'. The forward type is universally
-- quantified inside 'ProcessorContext' so it always type-checks.
mapValuesProc
  :: forall k v v'
   . (v -> IO v') -> IO (Processor k v)
mapValuesProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-MAPVALUES"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> do
            !v' <- f (recordValue r)
            let !out = Record
                  { recordKey       = recordKey r
                  , recordValue     = v'
                  , recordTimestamp = recordTimestamp r
                  , recordHeaders   = recordHeaders r
                  } :: Record k v'
            forwardRecord ctx out
    }

mapKeyValue
  :: forall k v k' v'
   . (k -> v -> (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
mapKeyValue f = mapKeyValueM (\k v -> pure (f k v))

mapKeyValueM
  :: forall k v k' v'
   . (k -> v -> IO (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
mapKeyValueM f s =
  attachProcessor s "KSTREAM-MAP"
    (mapKVProc @k @v @k' @v' f)
    (error "KStream.mapKeyValue: downstream key Serde unset")
    (error "KStream.mapKeyValue: downstream value Serde unset")

mapKVProc
  :: forall k v k' v'
   . (k -> v -> IO (k', v')) -> IO (Processor k v)
mapKVProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-MAP"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case (mctx, recordKey r) of
          (Just ctx, Just k) -> do
            (!k', !v') <- f k (recordValue r)
            let !out = Record
                  { recordKey       = Just k'
                  , recordValue     = v'
                  , recordTimestamp = recordTimestamp r
                  , recordHeaders   = recordHeaders r
                  } :: Record k' v'
            forwardRecord ctx out
          _ -> pure ()
    }

flatMapValues
  :: forall k v v'
   . (v -> [v'])
  -> KStream k v
  -> IO (KStream k v')
flatMapValues f s =
  attachProcessor s "KSTREAM-FLATMAPVALUES"
    (flatMapValuesProc @k @v @v' f)
    (kstreamKeySerde s)
    (error "KStream.flatMapValues: downstream value Serde unset")

flatMapValuesProc
  :: forall k v v'
   . (v -> [v']) -> IO (Processor k v)
flatMapValuesProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-FLATMAPVALUES"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx ->
            mapM_
              (\v' ->
                 let !out = Record
                       { recordKey       = recordKey r
                       , recordValue     = v'
                       , recordTimestamp = recordTimestamp r
                       , recordHeaders   = recordHeaders r
                       } :: Record k v'
                  in forwardRecord ctx out)
              (f (recordValue r))
    }

flatMapKeyValue
  :: forall k v k' v'
   . (k -> v -> [(k', v')])
  -> KStream k v
  -> IO (KStream k' v')
flatMapKeyValue f s =
  attachProcessor s "KSTREAM-FLATMAP"
    (flatMapKVProc @k @v @k' @v' f)
    (error "KStream.flatMapKeyValue: downstream key Serde unset")
    (error "KStream.flatMapKeyValue: downstream value Serde unset")

flatMapKVProc
  :: forall k v k' v'
   . (k -> v -> [(k', v')])
  -> IO (Processor k v)
flatMapKVProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-FLATMAP"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case (mctx, recordKey r) of
          (Just ctx, Just k) ->
            mapM_
              (\(k', v') ->
                 let !out = Record
                       { recordKey       = Just k'
                       , recordValue     = v'
                       , recordTimestamp = recordTimestamp r
                       , recordHeaders   = recordHeaders r
                       } :: Record k' v'
                  in forwardRecord ctx out)
              (f k (recordValue r))
          _ -> pure ()
    }

peekStream
  :: (Record k v -> IO ())
  -> KStream k v
  -> IO (KStream k v)
peekStream act s =
  attachProcessor s "KSTREAM-PEEK"
    (do
      ctxRef <- newIORef Nothing
      pure Processor
        { procName    = processorName "KSTREAM-PEEK"
        , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
        , procClose   = pure ()
        , procProcess = \r -> do
            act r
            mctx <- readIORef ctxRef
            case mctx of
              Nothing  -> pure ()
              Just ctx -> forwardRecord ctx r
        })
    (kstreamKeySerde s) (kstreamValueSerde s)

foreachStream :: (Record k v -> IO ()) -> KStream k v -> IO ()
foreachStream act s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-FOREACH"
  withTopology_ b $
    Topo.addProcessor nm [kstreamParent s] $ do
      pure Processor
        { procName    = processorName "KSTREAM-FOREACH"
        , procInit    = \_ -> pure ()
        , procClose   = pure ()
        , procProcess = act
        }

selectKey
  :: forall k v k'
   . (Record k v -> k')
  -> KStream k v
  -> IO (KStream k' v)
selectKey f s =
  attachProcessor s "KSTREAM-SELECTKEY"
    (selectKeyProc @k @v @k' f)
    (error "KStream.selectKey: downstream key Serde unset")
    (kstreamValueSerde s)

selectKeyProc
  :: forall k v k'
   . (Record k v -> k') -> IO (Processor k v)
selectKeyProc f = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName "KSTREAM-SELECTKEY"
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx ->
            let !out = Record
                  { recordKey       = Just (f r)
                  , recordValue     = recordValue r
                  , recordTimestamp = recordTimestamp r
                  , recordHeaders   = recordHeaders r
                  } :: Record k' v
             in forwardRecord ctx out
    }

----------------------------------------------------------------------
-- Composition
----------------------------------------------------------------------

mergeStreams :: KStream k v -> KStream k v -> IO (KStream k v)
mergeStreams a b = do
  let bld = kstreamBuilder a
  nm <- freshNodeName bld "KSTREAM-MERGE"
  withTopology_ bld $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [kstreamParent a, kstreamParent b]
        , Topo.processorSpecSupplier = Topo.AnyProcessor (mkPassThrough "KSTREAM-MERGE")
        , Topo.processorSpecStores   = []
        }
      t
  pure KStream
    { kstreamBuilder    = bld
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde a
    , kstreamValueSerde = kstreamValueSerde a
    }

mkPassThrough :: T.Text -> IO (Processor k v)
mkPassThrough nm = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName    = processorName nm
    , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose   = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> forwardRecord ctx r
    }

-- | Branch a stream into N substreams using the supplied predicates.
-- Records are routed to the first matching predicate; non-matching
-- records are dropped.
branchStream
  :: [(Record k v -> Bool)]
  -> KStream k v
  -> IO [KStream k v]
branchStream preds s = do
  let b = kstreamBuilder s
  -- Create a router processor and one downstream pass-through per
  -- branch. Children nodes attach to the router; the router uses
  -- 'forwardTo' to dispatch.
  router <- freshNodeName b "KSTREAM-BRANCH"
  branches <- mapM (\_ ->
                      freshNodeName b "KSTREAM-BRANCH-CHILD") preds
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = router
                  , Topo.processorSpecParents  = [kstreamParent s]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (mkRouter preds branches)
                  , Topo.processorSpecStores   = []
                  } t
        !t2 = foldl
                (\acc bn ->
                  Topo.addProcessorWith
                    Topo.ProcessorSpec
                      { Topo.processorSpecName     = bn
                      , Topo.processorSpecParents  = [router]
                      , Topo.processorSpecSupplier =
                          Topo.AnyProcessor
                            (mkPassThrough "KSTREAM-BRANCH-CHILD")
                      , Topo.processorSpecStores   = []
                      } acc)
                t1 branches
     in t2
  pure
    [ KStream
        { kstreamBuilder    = b
        , kstreamParent     = bn
        , kstreamKeySerde   = kstreamKeySerde s
        , kstreamValueSerde = kstreamValueSerde s
        }
    | bn <- branches
    ]

mkRouter
  :: [Record k v -> Bool]
  -> [Topo.NodeName]
  -> IO (Processor k v)
mkRouter preds branches = do
  ctxRef <- newIORef Nothing
  pure Processor
    { procName = processorName "KSTREAM-BRANCH"
    , procInit = \ctx -> writeIORef ctxRef (Just ctx)
    , procClose = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        case mctx of
          Nothing  -> pure ()
          Just ctx -> dispatch ctx (zip preds branches) r
    }
  where
    dispatch _   []                  _ = pure ()
    dispatch ctx ((p, target):rest) r =
      if p r
        then forwardTo ctx target r
        else dispatch ctx rest r

----------------------------------------------------------------------
-- Sinks
----------------------------------------------------------------------

toTopic
  :: TopicName
  -> Produced k v
  -> KStream k v
  -> IO ()
toTopic topic p s = do
  let b = kstreamBuilder s
  nm <- maybe (freshNodeName b "KSTREAM-SINK")
              (pure . Topo.NodeName)
              (producedName p)
  withTopology_ b $
    Topo.addSink nm topic
                 (producedKeySerde p) (producedValueSerde p)
                 [kstreamParent s]

-- | @through@ is a sink + a fresh source on the same topic. The
-- runtime handles the loopback through the broker.
throughTopic
  :: TopicName
  -> Produced k v
  -> KStream k v
  -> IO (KStream k v)
throughTopic topic p s = do
  toTopic topic p s
  let b = kstreamBuilder s
  streamFromTopic b topic
    (consumed (producedKeySerde p) (producedValueSerde p))

----------------------------------------------------------------------
-- Low-level
----------------------------------------------------------------------

----------------------------------------------------------------------
-- KStream-KTable join
----------------------------------------------------------------------

-- | Inner join: for each stream record whose key matches an entry in
-- the table, emit @joiner v_stream v_table@. Stream records that
-- find no match are dropped. Mirrors @KStream.join(KTable, ValueJoiner)@.
joinKStreamKTable
  :: forall k v vt v'
   . Ord k
  => (v -> vt -> v')
  -> Joined k v vt
  -> KStream k v
  -> KTable k vt
  -> IO (KStream k v')
joinKStreamKTable joiner _j s tab = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-KTABLE-JOIN"
  -- Note on wiring: the join processor's only parent is the
  -- /stream/ side. The table side updates its store via the topology
  -- it was built with, /before/ any joiner evaluation, because the
  -- engine processes records FIFO across the whole task. The table
  -- store is read by the join processor via 'getStateStore'.
  withTopology_ b $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [kstreamParent s]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor
              (joinKStreamKTableProc @k @v @vt @v' (ktableStore tab) joiner False)
        , Topo.processorSpecStores   = [ktableStore tab]
        }
      t
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = error "KStream.join: downstream value Serde unset"
    }

-- | Left join: stream records always emit, with @Nothing@ on the
-- right side when the table has no entry. Mirrors @KStream.leftJoin@.
leftJoinKStreamKTable
  :: forall k v vt v'
   . Ord k
  => (v -> Maybe vt -> v')
  -> Joined k v vt
  -> KStream k v
  -> KTable k vt
  -> IO (KStream k v')
leftJoinKStreamKTable joiner _j s tab = do
  let b = kstreamBuilder s
  nm <- freshNodeName b "KSTREAM-KTABLE-LEFTJOIN"
  withTopology_ b $ \t ->
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [kstreamParent s]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor
              (joinKStreamKTableProcL @k @v @vt @v' (ktableStore tab) joiner)
        , Topo.processorSpecStores   = [ktableStore tab]
        }
      t
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = error "KStream.leftJoin: downstream value Serde unset"
    }

-- | The join processor. The KTable side has already updated its store
-- before this processor sees a stream record (because the Topology
-- builder lists the table's update node as a parent — and KTable
-- updates flow before the join processor is invoked for the stream
-- side, courtesy of the engine's per-record FIFO).
--
-- Note: in the JVM impl, both sides are "co-partitioned" before
-- joining; here we operate within a single task so partitioning is
-- trivially equal. A real multi-task runtime would need to ensure
-- the upstream KTable store is reachable from the same task as the
-- KStream — which is exactly why the join is materialised against
-- the table's store name.
joinKStreamKTableProc
  :: forall k v vt v'
   . Ord k
  => StoreName
  -> (v -> vt -> v')
  -> Bool                                -- left-join?
  -> IO (Processor k v)
joinKStreamKTableProc storeNm joiner _isLeft = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k vt))
  pure Processor
    { procName = processorName "KSTREAM-KTABLE-JOIN"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx storeNm
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "join: store not found: " <> show storeNm
    , procClose = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just kvs, Just k) -> do
            mt <- kvsGet kvs k
            case mt of
              Just tv ->
                let !v' = joiner (recordValue r) tv
                    !out = Record
                      { recordKey       = Just k
                      , recordValue     = v'
                      , recordTimestamp = recordTimestamp r
                      , recordHeaders   = recordHeaders r
                      } :: Record k v'
                 in forwardRecord ctx out
              Nothing -> pure ()  -- inner-join drops
          _ -> pure ()
    }

joinKStreamKTableProcL
  :: forall k v vt v'
   . Ord k
  => StoreName
  -> (v -> Maybe vt -> v')
  -> IO (Processor k v)
joinKStreamKTableProcL storeNm joiner = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k vt))
  pure Processor
    { procName = processorName "KSTREAM-KTABLE-LEFTJOIN"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx storeNm
        case st of
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "leftJoin: store not found: " <> show storeNm
    , procClose = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just kvs, Just k) -> do
            mt <- kvsGet kvs k
            let !v' = joiner (recordValue r) mt
                !out = Record
                  { recordKey       = Just k
                  , recordValue     = v'
                  , recordTimestamp = recordTimestamp r
                  , recordHeaders   = recordHeaders r
                  } :: Record k v'
             in forwardRecord ctx out
          _ -> pure ()
    }

----------------------------------------------------------------------
-- KStream-KStream window join
--
-- Architecture:
--
--   Stream-A --> JoinSideProc-A --\
--                                  +--> MergePass --> downstream
--   Stream-B --> JoinSideProc-B --/
--
-- Each side processor:
--   1. Stores its incoming record in its /own/ window store, keyed
--      by record key, indexed by record timestamp.
--   2. Range-scans the /other/ store over
--      @[ts - jwBeforeMs, ts + jwAfterMs]@ for matches.
--   3. For each match, calls the joiner with both values in the
--      canonical (left, right) order and forwards the result to the
--      MergePass node.
--   4. For LEFT and OUTER joins, when /this/ side finds no matches
--      and the side's emission rule for "no match" is non-empty, it
--      emits a single record with @Nothing@ on the other side.
----------------------------------------------------------------------

-- | Inner KStream-KStream window join. Mirrors
-- @KStream.join(other, ValueJoiner, JoinWindows)@.
joinKStreamKStream
  :: forall k v1 v2 v'
   . Ord k
  => (v1 -> v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> IO (KStream k v')
joinKStreamKStream joiner jw _j sl sr =
  buildWindowJoin sl sr jw "KSTREAM-WINDOWJOIN"
    (mkSideProc @k @v1 @v2 @v' jw (\v1 v2 -> joiner v1 v2) Inner)
    (mkSideProc @k @v2 @v1 @v' jw (\v2 v1 -> joiner v1 v2) Inner)

-- | Left KStream-KStream window join. Every left record emits at
-- least once: with @Just v2@ for each match, or with @Nothing@ if no
-- match. Right records only contribute matches.
leftJoinKStreamKStream
  :: forall k v1 v2 v'
   . Ord k
  => (v1 -> Maybe v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> IO (KStream k v')
leftJoinKStreamKStream joiner jw _j sl sr =
  buildWindowJoin sl sr jw "KSTREAM-WINDOWLEFTJOIN"
    (mkSideProc @k @v1 @v2 @v' jw
       (\v1 v2 -> joiner v1 (Just v2))
       (LeftEmitNothing (\v1 -> joiner v1 Nothing)))
    (mkSideProc @k @v2 @v1 @v' jw
       (\v2 v1 -> joiner v1 (Just v2))
       Inner)

-- | Outer KStream-KStream window join. Both sides emit at least
-- once. The joiner takes Maybes on both sides.
outerJoinKStreamKStream
  :: forall k v1 v2 v'
   . Ord k
  => (Maybe v1 -> Maybe v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> IO (KStream k v')
outerJoinKStreamKStream joiner jw _j sl sr =
  buildWindowJoin sl sr jw "KSTREAM-WINDOWOUTERJOIN"
    (mkSideProc @k @v1 @v2 @v' jw
       (\v1 v2 -> joiner (Just v1) (Just v2))
       (LeftEmitNothing (\v1 -> joiner (Just v1) Nothing)))
    (mkSideProc @k @v2 @v1 @v' jw
       (\v2 v1 -> joiner (Just v1) (Just v2))
       (LeftEmitNothing (\v2 -> joiner Nothing (Just v2))))

-- | The "what to do when this side finds no matches" mode.
data JoinMode vSelf vOut
  = Inner
  | LeftEmitNothing (vSelf -> vOut)

-- | Internal: build the topology for a window join given the two
-- pre-typed side processors.
buildWindowJoin
  :: forall k v1 v2 v'
   . Ord k
  => KStream k v1
  -> KStream k v2
  -> JoinWindows
  -> T.Text                              -- ^ prefix
  -> (StoreName -> StoreName -> Topo.NodeName -> IO (Processor k v1))
  -> (StoreName -> StoreName -> Topo.NodeName -> IO (Processor k v2))
  -> IO (KStream k v')
buildWindowJoin sl sr jw prefix mkLeftProc mkRightProc = do
  let b = kstreamBuilder sl
  leftStoreNm  <- freshStoreName b (prefix <> "-LEFT-STORE")
  rightStoreNm <- freshStoreName b (prefix <> "-RIGHT-STORE")
  leftNm       <- freshNodeName  b (prefix <> "-LEFT")
  rightNm      <- freshNodeName  b (prefix <> "-RIGHT")
  mergeNm      <- freshNodeName  b (prefix <> "-MERGE")

  let !sz  = jwBeforeMs jw + jwAfterMs jw + 1
      !ret = sz * 2
      lsb = inMemoryWindowStoreBuilder leftStoreNm  sz ret
              :: StoreBuilderW k v1
      rsb = inMemoryWindowStoreBuilder rightStoreNm sz ret
              :: StoreBuilderW k v2

  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = leftNm
                  , Topo.processorSpecParents  = [kstreamParent sl]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkLeftProc leftStoreNm rightStoreNm mergeNm)
                  , Topo.processorSpecStores   = [leftStoreNm, rightStoreNm]
                  } t
        !t2 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = rightNm
                  , Topo.processorSpecParents  = [kstreamParent sr]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkRightProc rightStoreNm leftStoreNm mergeNm)
                  , Topo.processorSpecStores   = [leftStoreNm, rightStoreNm]
                  } t1
        !t3 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = mergeNm
                  , Topo.processorSpecParents  = [leftNm, rightNm]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (mkPassThrough (prefix <> "-MERGE"))
                  , Topo.processorSpecStores   = []
                  } t2
        !t4 = Topo.addStateStoreW lsb [leftNm, rightNm] t3
        !t5 = Topo.addStateStoreW rsb [leftNm, rightNm] t4
     in t5

  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = mergeNm
    , kstreamKeySerde   = kstreamKeySerde sl
    , kstreamValueSerde = error
        "KStream-KStream join: downstream value Serde unset; supply via to/through"
    }

-- | Build a single side's join processor.
--
-- @selfStore@: where this side puts its incoming records.
-- @otherStore@: where this side scans for matches.
-- @merge@: downstream node that aggregates both sides' emissions.
-- @joiner@: takes (this-side-value, other-side-value) and returns the
--   joined output value.
-- @mode@: what to do when this record finds /no/ match on the other
--   side. 'Inner' drops; 'LeftEmitNothing f' emits @f thisValue@.
-- @side@: only used to suppress unused warnings; the joiner is
--   already in canonical (left,right) order.
mkSideProc
  :: forall k vSelf vOther vOut
   . Ord k
  => JoinWindows
  -> (vSelf -> vOther -> vOut)
  -> JoinMode vSelf vOut
  -> StoreName -> StoreName -> Topo.NodeName
  -> IO (Processor k vSelf)
mkSideProc jw joiner mode selfStoreNm otherStoreNm mergeNm = do
  ctxRef   <- newIORef Nothing
  selfRef  <- newIORef (Nothing :: Maybe (WindowStore k vSelf))
  otherRef <- newIORef (Nothing :: Maybe (WindowStore k vOther))
  pure Processor
    { procName = processorName "WINDOW-JOIN-SIDE"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx selfStoreNm >>= \case
          Just (AnyWindowStore ws) ->
            writeIORef selfRef (Just (Unsafe.unsafeCoerce ws))
          _ -> error $ "join: self store not found: " <> show selfStoreNm
        getStateStore ctx otherStoreNm >>= \case
          Just (AnyWindowStore ws) ->
            writeIORef otherRef (Just (Unsafe.unsafeCoerce ws))
          _ -> error $ "join: other store not found: " <> show otherStoreNm
    , procClose = pure ()
    , procProcess = \r ->
        case recordKey r of
          Nothing -> pure ()
          Just k  -> do
            mctx <- readIORef ctxRef
            mself <- readIORef selfRef
            mother <- readIORef otherRef
            case (mctx, mself, mother) of
              (Just ctx, Just self_, Just other_) -> do
                let !ts@(Timestamp tsMs) = recordTimestamp r
                wsPut self_ k (recordValue r) ts
                let !lo = Timestamp (tsMs - jwBeforeMs jw)
                    !hi = Timestamp (tsMs + jwAfterMs  jw)
                it <- wsFetchRange other_ k lo hi
                matches <- kvIteratorToList it
                if null matches
                  then case mode of
                    Inner -> pure ()
                    LeftEmitNothing f ->
                      forwardTo ctx mergeNm
                        (Record
                          { recordKey       = Just k
                          , recordValue     = f (recordValue r)
                          , recordTimestamp = ts
                          , recordHeaders   = recordHeaders r
                          } :: Record k vOut)
                  else mapM_
                         (\(_otherTs, otherV) ->
                            forwardTo ctx mergeNm
                              (Record
                                { recordKey       = Just k
                                , recordValue     = joiner (recordValue r) otherV
                                , recordTimestamp = ts
                                , recordHeaders   = recordHeaders r
                                } :: Record k vOut))
                         matches
              _ -> pure ()
    }

-- | Drop into the low-level Processor API for value-only state, with
-- access to the typed 'ProcessorContext'.
transformValuesStream
  :: T.Text                              -- ^ Processor name prefix
  -> [Topo.NodeName]                     -- ^ Stores attached to this processor
  -> IO (Processor k v)                  -- ^ Processor supplier (already projects to v')
  -> Serde v'                            -- ^ Output value serde
  -> KStream k v
  -> IO (KStream k v')
transformValuesStream prefix _stores supplier vs s =
  attachProcessor s prefix supplier (kstreamKeySerde s) vs

----------------------------------------------------------------------
-- KStream -> KTable conversion
----------------------------------------------------------------------

-- | Convert a stream into a 'KTable' by materialising the latest
-- value per key into a state store. Mirrors @KStream.toTable@.
--
-- Tombstones (records with @recordValue = ...@ but explicitly null
-- in the wire form — represented in our model as a regular 'Record'
-- whose user code chose to encode nullness explicitly) are NOT
-- automatically interpreted as deletes here. If you need delete
-- semantics, route through 'mapValues' producing 'Maybe' and write
-- a tombstone-aware writer; the Java implementation is similarly
-- caller-aware.
toTable
  :: forall k v
   . Ord k
  => Materialized k v
  -> KStream k v
  -> IO (KTable k v)
toTable m s = do
  let b = kstreamBuilder s
  storeNm <- maybe
               (freshStoreName b "KSTREAM-TOTABLE-STORE")
               pure
               (matName m)
  procNm <- freshNodeName b "KSTREAM-TOTABLE"
  let supplier = inMemoryKeyValueStoreBuilder storeNm
                   :: Kafka.Streams.State.Store.StoreBuilderKV k v
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [kstreamParent s]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor (toTableProc @k @v storeNm)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreKV supplier [procNm] t1
     in t2
  pure (mkKTableFromStream procNm storeNm b (kstreamKeySerde s) (kstreamValueSerde s))

mkKTableFromStream
  :: Topo.NodeName
  -> Kafka.Streams.State.Store.StoreName
  -> StreamsBuilder
  -> Serde k -> Serde v
  -> KTable k v
mkKTableFromStream nm sn b ks vs =
  -- KTable is a record with non-strict serde fields (we made them
  -- lazy so the builder can stitch deferred error placeholders).
  Kafka.Streams.DSL.KTable.KTable nm sn b ks vs

toTableProc
  :: forall k v
   . Ord k
  => Kafka.Streams.State.Store.StoreName
  -> IO (Processor k v)
toTableProc sn = do
  ctxRef <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  pure Processor
    { procName = processorName "KSTREAM-TOTABLE"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx sn >>= \case
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "toTable: store missing: " <> show sn
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just k  -> do
          mctx <- readIORef ctxRef
          mst  <- readIORef storeRef
          case (mctx, mst) of
            (Just ctx, Just kvs) -> do
              kvsPut kvs k (recordValue r)
              forwardRecord ctx r
            _ -> pure ()
    }

----------------------------------------------------------------------
-- Repartition
----------------------------------------------------------------------

-- | Force a repartition. In a single-task driver this is a no-op
-- pass-through; in a multi-task / broker-backed runtime the
-- generated topology routes the records through an internal
-- repartition topic, which is what triggers the actual partitioning.
--
-- Mirrors @KStream.repartition()@ (KIP-221).
repartition
  :: forall k v
   . T.Text                              -- ^ topic name prefix
  -> KStream k v
  -> IO (KStream k v)
repartition topicPrefix s = do
  let b = kstreamBuilder s
  nm <- freshNodeName b ("KSTREAM-REPARTITION-" <> topicPrefix)
  withTopology_ b $ \t ->
    Topo.addProcessor nm [kstreamParent s] (mkPassThrough "KSTREAM-REPARTITION") t
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = kstreamValueSerde s
    }

----------------------------------------------------------------------
-- Split (KIP-418)
----------------------------------------------------------------------

-- | A named branch in a 'splitStream'. The runtime evaluates each
-- predicate in order; the first match routes the record to that
-- branch's named output. The 'Branched' carries an optional
-- continuation invoked /at build time/ on the resulting 'KStream'
-- so users can chain into per-branch sink/transform pipelines
-- without having to thread the result list manually.
data Branched k v = Branched
  { branchedName :: !T.Text
  , branchedPred :: !(Record k v -> Bool)
  , branchedAct  :: !(KStream k v -> IO ())
  }

-- | 'Branched' constructor with a pure predicate and no per-branch
-- continuation. Equivalent to Java's @Branched.as("name").withPredicate(p)@.
branchedFrom :: T.Text -> (Record k v -> Bool) -> Branched k v
branchedFrom n p = Branched
  { branchedName = n
  , branchedPred = p
  , branchedAct  = \_ -> pure ()
  }

-- | KIP-418-style split: each input record is routed to the first
-- branch whose predicate matches. Records that match none are sent
-- to the optional default branch ('Nothing' drops them).
--
-- Returns a 'Map' from branch name to the resulting 'KStream', plus
-- (if requested) the default-branch stream.
splitStream
  :: forall k v
   . [Branched k v]                      -- ^ branches in evaluation order
  -> Maybe T.Text                        -- ^ optional default-branch name
  -> KStream k v
  -> IO (Map.Map T.Text (KStream k v))
splitStream branches mDefault s = do
  let b = kstreamBuilder s
  router <- freshNodeName b "KSTREAM-SPLIT"
  let allBranches = branches
        ++ case mDefault of
             Just n  -> [defaultBranch n]
             Nothing -> []
      defaultBranch n = Branched
        { branchedName = n
        , branchedPred = \_ -> True
        , branchedAct  = \_ -> pure ()
        }
  childNodes <- mapM
    (\br -> do
        n <- freshNodeName b ("KSTREAM-SPLIT-" <> branchedName br)
        pure (br, n))
    allBranches
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = router
                  , Topo.processorSpecParents  = [kstreamParent s]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkRouter (map (branchedPred . fst) childNodes)
                                  (map snd childNodes))
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = foldl
                (\acc (_br, n) ->
                   Topo.addProcessorWith
                     Topo.ProcessorSpec
                       { Topo.processorSpecName     = n
                       , Topo.processorSpecParents  = [router]
                       , Topo.processorSpecSupplier =
                           Topo.AnyProcessor (mkPassThrough "KSTREAM-SPLIT-CHILD")
                       , Topo.processorSpecStores   = []
                       } acc)
                t1 childNodes
     in t2
  results <- mapM
    (\(br, n) -> do
        let !sub = KStream
              { kstreamBuilder    = b
              , kstreamParent     = n
              , kstreamKeySerde   = kstreamKeySerde s
              , kstreamValueSerde = kstreamValueSerde s
              }
        branchedAct br sub
        pure (branchedName br, sub))
    childNodes
  pure (Map.fromList results)

-- 'Map' / 'KTable' kept imported here so helper signatures resolve
-- without polluting the public surface.
_unused_split :: Map.Map T.Text Int
_unused_split = Map.empty

----------------------------------------------------------------------
-- Named variants (KIP-307)
----------------------------------------------------------------------

-- | 'filterStream' with an explicit topology node name.
filterStreamNamed
  :: forall k v
   . Kafka.Streams.DSL.Named.Named
  -> (Record k v -> Bool)
  -> KStream k v
  -> IO (KStream k v)
filterStreamNamed nm pred_ s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.DSL.Named.namedOr b nm "KSTREAM-FILTER"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s]
      (filterProcessor (Topo.unNodeName nodeNm) pred_)
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = kstreamValueSerde s
    }

-- | 'mapValues' with an explicit topology node name.
mapValuesNamed
  :: forall k v v'
   . Kafka.Streams.DSL.Named.Named
  -> (v -> v')
  -> KStream k v
  -> IO (KStream k v')
mapValuesNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.DSL.Named.namedOr b nm "KSTREAM-MAPVALUES"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (mapValuesProc (pure . f))
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = error
        "KStream.mapValuesNamed: downstream value Serde unset"
    }

-- | 'mapKeyValue' with an explicit topology node name.
mapKeyValueNamed
  :: forall k v k' v'
   . Kafka.Streams.DSL.Named.Named
  -> (k -> v -> (k', v'))
  -> KStream k v
  -> IO (KStream k' v')
mapKeyValueNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.DSL.Named.namedOr b nm "KSTREAM-MAP"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s]
      (mapKVProc (\k v -> pure (f k v)))
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = error "mapKeyValueNamed: downstream key Serde unset"
    , kstreamValueSerde = error "mapKeyValueNamed: downstream value Serde unset"
    }

-- | 'peekStream' with an explicit topology node name.
peekStreamNamed
  :: forall k v
   . Kafka.Streams.DSL.Named.Named
  -> (Record k v -> IO ())
  -> KStream k v
  -> IO (KStream k v)
peekStreamNamed nm act s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.DSL.Named.namedOr b nm "KSTREAM-PEEK"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] $ do
      ctxRef <- newIORef Nothing
      pure Processor
        { procName    = processorName "KSTREAM-PEEK"
        , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
        , procClose   = pure ()
        , procProcess = \r -> do
            act r
            mctx <- readIORef ctxRef
            case mctx of
              Nothing  -> pure ()
              Just ctx -> forwardRecord ctx r
        }
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = kstreamValueSerde s
    }

-- | 'selectKey' with an explicit topology node name.
selectKeyNamed
  :: forall k v k'
   . Kafka.Streams.DSL.Named.Named
  -> (Record k v -> k')
  -> KStream k v
  -> IO (KStream k' v)
selectKeyNamed nm f s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.DSL.Named.namedOr b nm "KSTREAM-SELECTKEY"
  withTopology_ b $
    Topo.addProcessor nodeNm [kstreamParent s] (selectKeyProc f)
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nodeNm
    , kstreamKeySerde   = error "selectKeyNamed: downstream key Serde unset"
    , kstreamValueSerde = kstreamValueSerde s
    }

-- | 'toTopic' with an explicit topology node name.
toTopicNamed
  :: forall k v
   . Kafka.Streams.DSL.Named.Named
  -> TopicName
  -> Produced k v
  -> KStream k v
  -> IO ()
toTopicNamed nm topic p s = do
  let b = kstreamBuilder s
  nodeNm <- Kafka.Streams.DSL.Named.namedOr b nm "KSTREAM-SINK"
  withTopology_ b $
    Topo.addSink nodeNm topic
                 (producedKeySerde p) (producedValueSerde p)
                 [kstreamParent s]
