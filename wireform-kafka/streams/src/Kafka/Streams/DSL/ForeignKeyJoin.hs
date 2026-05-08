{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.DSL.ForeignKeyJoin
-- Description : KTable-KTable foreign-key join (KIP-213)
--
-- Joins a left 'KTable' on a /derived/ key — extracted from the
-- left value — into a right 'KTable' that is partitioned on that
-- derived key. The join result is itself a 'KTable', materialised
-- into a fresh store.
--
-- The Java semantics drive a four-topic protocol (subscription,
-- subscription-response, foreign-key state, output). In a
-- single-task runtime the partitioning collapses, but the
-- /tracking/ does not: when the left side updates @(k, v)@ to a new
-- foreign key @fk@, we must remember that @k@ is now subscribed to
-- @fk@ (and unsubscribe from the previous @fk@), so that subsequent
-- right-side updates on @fk@ re-emit the join for @k@.
--
-- == State stores used
--
--   * @subscriptions@ — @fk -> 'Set' k@: which left keys have
--     subscribed to a given foreign key.
--   * @left-state@ — @k -> v_left@: the latest left-side value per
--     left key (used by right-side updates to recompute joins).
--   * @right-state@ — @fk -> v_right@: the latest right-side value
--     per foreign key (used by left-side updates to compute joins
--     immediately).
--   * @last-fk@ — @k -> fk@: the foreign key the left key was last
--     subscribed to. Used for unsubscribe-on-change.
--   * Materialised output store: the join result.
module Kafka.Streams.DSL.ForeignKeyJoin
  ( foreignKeyJoinKTable
  , leftForeignKeyJoinKTable
  ) where

import Data.IORef
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.DSL.KTable
  ( KTable (..)
  , ktableBuilder
  , ktableKeySerde
  , ktableNode
  , ktableStore
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
import Kafka.Streams.Processor
  ( Processor (..)
  , ProcessorContext
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

-- | Inner foreign-key KTable-KTable join.
foreignKeyJoinKTable
  :: forall k v fk vr v'
   . (Ord k, Ord fk)
  => (v -> fk)                 -- foreign-key extractor
  -> (v -> vr -> v')           -- joiner
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
foreignKeyJoinKTable extractor joiner =
  buildFKJoin "FK-JOIN" extractor (FKModeInner joiner)

-- | Left foreign-key KTable-KTable join. The joiner gets a 'Maybe'
-- on the right side; left records always emit at least once.
leftForeignKeyJoinKTable
  :: forall k v fk vr v'
   . (Ord k, Ord fk)
  => (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
leftForeignKeyJoinKTable extractor joiner =
  buildFKJoin "FK-LEFTJOIN" extractor (FKModeLeft joiner)

-- | Mode + joiner closure carried into the side processors.
data FKMode v vr v' where
  FKModeInner :: (v -> vr -> v')        -> FKMode v vr v'
  FKModeLeft  :: (v -> Maybe vr -> v')  -> FKMode v vr v'

buildFKJoin
  :: forall k v fk vr v'
   . (Ord k, Ord fk)
  => String                            -- prefix (debug only)
  -> (v -> fk)
  -> FKMode v vr v'
  -> Materialized k v'
  -> KTable k v
  -> KTable fk vr
  -> IO (KTable k v')
buildFKJoin _prefix extractor mode m kl kr = do
  let b = ktableBuilder kl
  outNm <- maybe (freshStoreName b "KTABLE-FK-OUT")
                 pure
                 (matName m)
  subsNm  <- freshStoreName b "KTABLE-FK-SUBS"
  leftNm  <- freshStoreName b "KTABLE-FK-LEFT"
  lastFKNm <- freshStoreName b "KTABLE-FK-LASTFK"

  let outBuilder   = inMemoryKeyValueStoreBuilder outNm
                       :: StoreBuilderKV k v'
      subsBuilder  = inMemoryKeyValueStoreBuilder subsNm
                       :: StoreBuilderKV fk (Set k)
      leftBuilder  = inMemoryKeyValueStoreBuilder leftNm
                       :: StoreBuilderKV k v
      lastFKBuilder = inMemoryKeyValueStoreBuilder lastFKNm
                       :: StoreBuilderKV k fk
  leftProcNm  <- freshNodeName b "KTABLE-FK-LEFT-PROC"
  rightProcNm <- freshNodeName b "KTABLE-FK-RIGHT-PROC"

  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = leftProcNm
                  , Topo.processorSpecParents  = [ktableNode kl]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkLeftSideProc @k @v @fk @vr @v'
                           extractor mode
                           (ktableStore kr) outNm subsNm leftNm lastFKNm)
                  , Topo.processorSpecStores   =
                      [outNm, subsNm, leftNm, lastFKNm, ktableStore kr]
                  } t
        !t2 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = rightProcNm
                  , Topo.processorSpecParents  = [ktableNode kr]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (mkRightSideProc @k @v @fk @vr @v'
                           mode
                           outNm subsNm leftNm)
                  , Topo.processorSpecStores   =
                      [outNm, subsNm, leftNm, ktableStore kr]
                  } t1
        !t3 = Topo.addStateStoreKV outBuilder    [leftProcNm, rightProcNm] t2
        !t4 = Topo.addStateStoreKV subsBuilder   [leftProcNm, rightProcNm] t3
        !t5 = Topo.addStateStoreKV leftBuilder   [leftProcNm, rightProcNm] t4
        !t6 = Topo.addStateStoreKV lastFKBuilder [leftProcNm]              t5
     in t6
  pure KTable
    { ktableNode       = leftProcNm
    , ktableStore      = outNm
    , ktableBuilder    = b
    , ktableKeySerde   = ktableKeySerde kl
    , ktableValueSerde = error "FK join: pass Materialized with serde to set output"
    }

-- The left-side joiner-with-Maybe lives inline in 'mkLeftSideProc' /
-- 'mkRightSideProc' under the 'LeftFK' branch. The builder's joiner
-- argument carries the inner shape; we override it inside the side
-- processors when the mode is left.
--
-- For the left-join surface API, callers pass their @v -> Maybe vr ->
-- v'@ joiner via 'leftForeignKeyJoinKTable'. The inner builder
-- discards the placeholder inner joiner (the @\\v _ -> v@) and uses
-- the 'LeftFK' branch which pattern-matches on the optional right
-- value.

----------------------------------------------------------------------
-- Side processors
----------------------------------------------------------------------

-- | Left-side processor: handles updates to the left KTable.
mkLeftSideProc
  :: forall k v fk vr v'
   . (Ord k, Ord fk)
  => (v -> fk)
  -> FKMode v vr v'
  -> StoreName              -- right table store (for lookup)
  -> StoreName              -- output store
  -> StoreName              -- subscriptions store
  -> StoreName              -- left-state store
  -> StoreName              -- last-fk store
  -> IO (Processor k v)
mkLeftSideProc extractor mode rightNm outNm subsNm leftNm lastFKNm = do
  ctxRef    <- newIORef Nothing
  rightRef  <- newIORef (Nothing :: Maybe (KeyValueStore fk vr))
  outRef    <- newIORef (Nothing :: Maybe (KeyValueStore k v'))
  subsRef   <- newIORef (Nothing :: Maybe (KeyValueStore fk (Set k)))
  leftRef   <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  lastFKRef <- newIORef (Nothing :: Maybe (KeyValueStore k fk))
  pure Processor
    { procName = processorName "FK-JOIN-LEFT"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        bindStore ctx rightNm  rightRef
        bindStore ctx outNm    outRef
        bindStore ctx subsNm   subsRef
        bindStore ctx leftNm   leftRef
        bindStore ctx lastFKNm lastFKRef
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just k -> do
          mctx    <- readIORef ctxRef
          mright  <- readIORef rightRef
          mout    <- readIORef outRef
          msubs   <- readIORef subsRef
          mleft   <- readIORef leftRef
          mLastFK <- readIORef lastFKRef
          case (mctx, mright, mout, msubs, mleft, mLastFK) of
            (Just ctx, Just right_, Just out_, Just subs_, Just left_, Just lastFK_) -> do
              let v       = recordValue r
                  newFK   = extractor v
              -- Unsubscribe k from any old fk.
              mOldFK <- kvsGet lastFK_ k
              case mOldFK of
                Just oldFK | oldFK /= newFK -> do
                  mOldSet <- kvsGet subs_ oldFK
                  case mOldSet of
                    Just s  -> do
                      let !s' = Set.delete k s
                      if Set.null s'
                        then () <$ kvsDelete subs_ oldFK
                        else kvsPut subs_ oldFK s'
                    Nothing -> pure ()
                _ -> pure ()
              -- Subscribe k to newFK.
              mNewSet <- kvsGet subs_ newFK
              kvsPut subs_ newFK (Set.insert k (maybe Set.empty id mNewSet))
              kvsPut lastFK_ k newFK
              -- Update left-state.
              kvsPut left_ k v
              -- Compute and emit join result.
              mvr <- kvsGet right_ newFK
              emitJoin mode ctx out_ k r v mvr
            _ -> pure ()
    }

-- | Right-side processor: handles updates to the right KTable.
mkRightSideProc
  :: forall k v fk vr v'
   . (Ord k, Ord fk)
  => FKMode v vr v'
  -> StoreName              -- output store
  -> StoreName              -- subscriptions store
  -> StoreName              -- left-state store
  -> IO (Processor fk vr)
mkRightSideProc mode outNm subsNm leftNm = do
  ctxRef   <- newIORef Nothing
  outRef   <- newIORef (Nothing :: Maybe (KeyValueStore k v'))
  subsRef  <- newIORef (Nothing :: Maybe (KeyValueStore fk (Set k)))
  leftRef  <- newIORef (Nothing :: Maybe (KeyValueStore k v))
  pure Processor
    { procName = processorName "FK-JOIN-RIGHT"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        bindStore ctx outNm   outRef
        bindStore ctx subsNm  subsRef
        bindStore ctx leftNm  leftRef
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just fk -> do
          mctx <- readIORef ctxRef
          mout <- readIORef outRef
          msubs <- readIORef subsRef
          mleft <- readIORef leftRef
          case (mctx, mout, msubs, mleft) of
            (Just ctx, Just out_, Just subs_, Just left_) -> do
              let vr = recordValue r
              -- Look up subscribed left keys.
              mSet <- kvsGet subs_ fk
              let subscribers = maybe [] Set.toList mSet
              -- Re-emit join for each subscriber using the new vr.
              -- We rebuild a 'Record k vr' carrier per subscriber so
              -- the timestamp/headers propagate, but the original
              -- record's key was 'fk' which is wrong for emitJoin.
              let !ts = recordTimestamp r
                  !hs = recordHeaders r
              mapM_
                (\k -> do
                   mLeftV <- kvsGet left_ k
                   case mLeftV of
                     Nothing -> pure ()
                     Just lv ->
                       let !carrier = Record (Just k) vr ts hs :: Record k vr
                        in emitJoin mode ctx out_ k carrier lv (Just vr))
                subscribers
            _ -> pure ()
    }

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

bindStore
  :: forall k v
   . ProcessorContext
  -> StoreName
  -> IORef (Maybe (KeyValueStore k v))
  -> IO ()
bindStore ctx sn ref =
  getStateStore ctx sn >>= \case
    Just (AnyKeyValueStore kvs) ->
      writeIORef ref (Just (Unsafe.unsafeCoerce kvs))
    _ -> error $ "FK join: store missing: " <> show sn

-- | Emit the join output for a single key, dispatching on the mode.
emitJoin
  :: forall k v vr v' rIn
   . FKMode v vr v'
  -> ProcessorContext
  -> KeyValueStore k v'
  -> k
  -> Record k rIn               -- carrier of timestamp/headers
  -> v
  -> Maybe vr
  -> IO ()
emitJoin mode ctx out_ k carrier v mvr =
  case mode of
    FKModeInner joiner ->
      case mvr of
        Just vr -> do
          let !v' = joiner v vr
          kvsPut out_ k v'
          forwardRecord ctx
            (Record (Just k) v'
              (recordTimestamp carrier)
              (recordHeaders carrier) :: Record k v')
        Nothing -> () <$ kvsDelete out_ k
    FKModeLeft joiner -> do
      let !v' = joiner v mvr
      kvsPut out_ k v'
      forwardRecord ctx
        (Record (Just k) v'
          (recordTimestamp carrier)
          (recordHeaders carrier) :: Record k v')