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
  ( KStream
  , kstreamParent
  , kstreamBuilder
  , kstreamKeySerde
  , kstreamValueSerde
    -- * Sources
  , streamFromTopic
    -- * Stateless transforms
  , filterStream
  , filterNotStream
  , mapValues
  , mapValuesM
  , mapKeyValue
  , mapKeyValueM
  , flatMapValues
  , flatMapKeyValue
  , peekStream
  , foreachStream
  , selectKey
    -- * Composition
  , mergeStreams
  , branchStream
    -- * Sinks
  , toTopic
  , throughTopic
    -- * Joins
  , joinKStreamKTable
  , leftJoinKStreamKTable
    -- * Low-level access
  , transformValuesStream
  ) where

import Data.IORef
import qualified Data.Text as T

import Kafka.Streams.DSL.Consumed
  ( Consumed (..)
  , consumed
  )
import Kafka.Streams.DSL.Joined (Joined (..))
import Kafka.Streams.DSL.Produced
  ( Produced (..)
  , produced
  )
import Kafka.Streams.DSL.StreamsBuilder
  ( StreamsBuilder
  , freshNodeName
  , withTopology_
  )

import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.DSL.KTable
  ( KTable
  , ktableNode
  , ktableStore
  )
import Kafka.Streams.Processor
  ( Processor (..)
  , ProcessorContext (..)
  , forwardRecord
  , forwardTo
  , getStateStore
  , processorName
  )
import Kafka.Streams.Serde (Serde)
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueStore (..)
  , StoreName
  )
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
