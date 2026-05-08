{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.DSL.TimeWindowedKStream
-- Description : TimeWindowedKStream + windowed aggregations
--
-- Mirrors @org.apache.kafka.streams.kstream.TimeWindowedKStream@.
module Kafka.Streams.DSL.TimeWindowedKStream
  ( TimeWindowedKStream
  , twksParent
  , twksKey
  , twksValue
  , twksBuilder
  , twksWindows
  , countWindowed
  , aggregateWindowed
  , reduceWindowed
  , WindowedTableHandle (..)
  ) where

import Data.IORef
import Data.Int (Int64)
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.DSL.KGroupedStream
  ( TimeWindowedKStream (..)
  )
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
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , StoreBuilderW
  , StoreName
  , WindowStore (..)
  )
import Kafka.Streams.State.Window.InMemory
  ( inMemoryWindowStoreBuilder
  )
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (Record (..))
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Window
  ( Window (..)
  , Windows (..)
  )

import Kafka.Streams.Processor (ProcessorContext (..))

-- | Result of a windowed aggregation: a (synthetic) handle to a
-- window-store materialisation.  Convert into a stream of windowed
-- records using 'toStreamWindowed'.
data WindowedTableHandle k v = WindowedTableHandle
  { wthNode    :: !Topo.NodeName
  , wthStore   :: !StoreName
  , wthBuilder :: !StreamsBuilder
  , wthWindows :: !Windows
  }

countWindowed
  :: forall k v
   . Ord k
  => Materialized k Int64
  -> TimeWindowedKStream k v
  -> IO (WindowedTableHandle k Int64)
countWindowed =
  aggregateWindowed @k @v @Int64 (pure 0) (\_ _ acc -> acc + 1)

reduceWindowed
  :: forall k v
   . Ord k
  => (v -> v -> v)
  -> Materialized k v
  -> TimeWindowedKStream k v
  -> IO (WindowedTableHandle k v)
reduceWindowed combine =
  aggregateWindowed @k @v @v
    (pure (error "reduceWindowed: cannot reduce empty"))
    (\_ v acc -> combine acc v)

aggregateWindowed
  :: forall k v a
   . Ord k
  => IO a
  -> (k -> v -> a -> a)
  -> Materialized k a
  -> TimeWindowedKStream k v
  -> IO (WindowedTableHandle k a)
aggregateWindowed initial agg m twks = do
  let b = twksBuilder twks
      ws = twksWindows twks
  storeNm <- maybe (freshStoreName b "WINDOWED-AGG-STORE")
                   pure
                   (matName m)
  let supplier = inMemoryWindowStoreBuilder
                   storeNm
                   (windowsSize ws)
                   (windowsRetention ws) :: StoreBuilderW k a
  nodeNm <- freshNodeName b "WINDOWED-AGG"
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = nodeNm
                  , Topo.processorSpecParents  = [twksParent twks]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (windowedAggProc @k @v @a storeNm ws initial agg)
                  , Topo.processorSpecStores   = []
                  }
                t
        !t2 = Topo.addStateStoreW supplier [nodeNm] t1
     in t2
  pure WindowedTableHandle
    { wthNode    = nodeNm
    , wthStore   = storeNm
    , wthBuilder = b
    , wthWindows = ws
    }

windowedAggProc
  :: forall k v a
   . Ord k
  => StoreName
  -> Windows
  -> IO a
  -> (k -> v -> a -> a)
  -> IO (Processor k v)
windowedAggProc sn ws initial agg = do
  ctxRef   <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (WindowStore k a))
  pure Processor
    { procName = processorName "WINDOWED-AGG"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        st <- getStateStore ctx sn
        case st of
          Just (AnyWindowStore wstore) ->
            writeIORef storeRef (Just (unsafeCastWS wstore))
          _ -> error $ "windowedAggProc: store not found: " <> show sn
    , procClose = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just store_, Just k) -> do
            -- Grace-period enforcement: skip records whose latest
            -- assignable window has fully closed below stream-time -
            -- grace. Mirrors the "late record drop" behaviour of
            -- KIP-633.
            Timestamp now <- ctxStreamTime ctx
            let !grace      = windowsGracePeriod ws
                !winSize    = windowsSize ws
                Timestamp rt = recordTimestamp r
                !rightEdge  = rt + winSize
                isExpired   = rightEdge + grace < now
            if isExpired
              then pure ()  -- silently drop the late record
              else do
                let !windows = windowsAssign ws (recordTimestamp r)
                mapM_
                  (\(Window startT _) -> do
                     mPrev <- wsFetch store_ k startT
                     cur <- maybe initial pure mPrev
                     let !next = agg k (recordValue r) cur
                     wsPut store_ k next startT
                     forwardRecord ctx r { recordValue = next })
                  windows
          _ -> pure ()
    }

unsafeCastWS :: WindowStore k v -> WindowStore k' v'
unsafeCastWS = Unsafe.unsafeCoerce
{-# INLINE unsafeCastWS #-}
