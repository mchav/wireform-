{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Watermark
-- Description : Cross-source watermark coordinator (Riffle §2)
--
-- The Phase 1 'Kafka.Streams.Time.StreamTime' is /per task/: each
-- engine independently tracks the maximum timestamp it has seen
-- on its assigned partitions. That works for single-source
-- topologies but is insufficient when:
--
--   1. The topology joins two streams whose source partitions are
--      consumed by different engines / workers. Downstream
--      windowing must wait for the laggard.
--   2. A partition goes idle (no new records). Its per-task
--      'StreamTime' stops advancing, so a join with another
--      partition that's still active stalls forever.
--   3. Several sources should be /aligned/ — their watermarks
--      must not diverge by more than a configurable bound, or
--      the faster source backpressures.
--
-- This module defines:
--
--   * 'WatermarkStrategy' — how a source extracts a watermark
--     from its records (bounded out-of-orderness, monotonic
--     ascending, or fully custom).
--   * 'WatermarkCoordinator' — a thread-safe registry. Sources
--     report their watermark, idleness, and the coordinator
--     publishes the /effective/ watermark = min of all live
--     sources (or skipping idle ones once 'idleTimeout' has
--     elapsed).
--   * 'AlignmentGroup' — sources sharing the same alignment
--     bound; the coordinator exposes whether a source should
--     pause emitting because it has out-paced a slow peer.
--
-- The module is /pure/ in the sense that it depends only on
-- 'STM' + 'IORef' + the existing 'Timestamp' type, so the runtime
-- can host one coordinator per topology subtopology without
-- pulling in additional dependencies.
--
-- The integration into 'Engine' / 'WorkerPool' happens at the
-- source-processor boundary: every record's timestamp is reported
-- to the coordinator after extraction, and downstream operators
-- that care about cross-source progress (joins, suppress) read
-- 'currentEffectiveWatermark' instead of 'engineStreamTime'.
-- That wiring is deferred to a follow-up PR; the contract is in
-- place here.
module Kafka.Streams.Watermark
  ( -- * Strategies
    WatermarkStrategy (..)
  , monotonicAscending
  , boundedOutOfOrderness
  , noWatermark
  , runStrategy
    -- * Coordinator
  , WatermarkCoordinator
  , SourceId (..)
  , newWatermarkCoordinator
  , registerSource
  , unregisterSource
  , reportRecord
  , markIdle
  , markActive
  , currentEffectiveWatermark
  , perSourceWatermarks
    -- * Alignment groups
  , AlignmentGroup (..)
  , declareAlignmentGroup
  , alignmentBacklog
  , shouldPauseSource
    -- * Idleness
  , IdleTimeout (..)
  , advanceWallClock
  ) where

import Control.Concurrent.STM
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Streams.Time
  ( Duration
  , Timestamp (..)
  , addDuration
  , durationMillis
  , minTimestamp
  , noTimestamp
  )

----------------------------------------------------------------------
-- Strategies
----------------------------------------------------------------------

-- | How a source derives the watermark a record contributes.
data WatermarkStrategy = WatermarkStrategy
  { wsName    :: !Text
  , wsExtract :: !(Timestamp -> Timestamp -> Timestamp)
    -- ^ @prev recordTs -> watermark@. Receives the previous
    -- watermark (so monotonic strategies can ratchet) and the
    -- current record timestamp.
  } deriving stock (Generic)

-- | Watermark equals the record timestamp; never lags. Use when
-- you trust the source to deliver in order (e.g. a CDC source
-- with a single writer).
monotonicAscending :: WatermarkStrategy
monotonicAscending = WatermarkStrategy
  { wsName    = "monotonic-ascending"
  , wsExtract = \prev t ->
      if t > prev then t else prev
  }

-- | Watermark equals @recordTimestamp - lag@, ratcheted to be
-- monotonic non-decreasing. Mirrors Flink's
-- @BoundedOutOfOrdernessWatermarks@.
boundedOutOfOrderness :: Duration -> WatermarkStrategy
boundedOutOfOrderness lag = WatermarkStrategy
  { wsName    = "bounded-out-of-orderness"
  , wsExtract = \prev (Timestamp t) ->
      let !lagMs = durationMillis lag
          !cand  = Timestamp (t - lagMs)
      in if cand > prev then cand else prev
  }

-- | Always emit 'noTimestamp'. Useful for sources whose records
-- carry no timestamp and that don't drive downstream windowing.
noWatermark :: WatermarkStrategy
noWatermark = WatermarkStrategy
  { wsName    = "none"
  , wsExtract = \_ _ -> noTimestamp
  }

-- | Apply a strategy. Pure function over @(previousWatermark,
-- recordTimestamp)@.
runStrategy :: WatermarkStrategy -> Timestamp -> Timestamp -> Timestamp
runStrategy = wsExtract

----------------------------------------------------------------------
-- Per-source state
----------------------------------------------------------------------

-- | Globally-unique identifier for a watermark source. Typically
-- the source-node name + partition number.
newtype SourceId = SourceId { unSourceId :: Text }
  deriving stock (Eq, Ord, Show, Generic)

data SourceState = SourceState
  { sstWatermark :: !Timestamp
  , sstStrategy  :: !WatermarkStrategy
  , sstIdleSince :: !(Maybe Timestamp)
    -- ^ 'Just t' once the source has been marked idle since
    -- wall-clock @t@.
  , sstGroup     :: !(Maybe Text)
    -- ^ Alignment group membership.
  }

----------------------------------------------------------------------
-- Coordinator
----------------------------------------------------------------------

-- | Configurable idle-timeout. Once a source has been silent for
-- this long (in wall-clock), the coordinator removes it from the
-- effective-watermark min.
newtype IdleTimeout = IdleTimeout Duration
  deriving stock (Eq, Show)

-- | Thread-safe registry of every active watermark source for one
-- subtopology.
data WatermarkCoordinator = WatermarkCoordinator
  { wcSources    :: !(TVar (Map SourceId SourceState))
  , wcIdleAfter  :: !IdleTimeout
  , wcWallClock  :: !(TVar Timestamp)
    -- ^ Wall-clock time as observed by the runtime. The runtime
    -- updates this via 'advanceWallClock'; the coordinator uses
    -- it to drive idle-timeout decisions.
  , wcGroups     :: !(TVar (Map Text AlignmentBound))
    -- ^ Alignment-group configuration.
  }

newtype AlignmentBound = AlignmentBound { unAlignmentBound :: Duration }
  deriving stock (Eq, Show)

-- | Construct an empty coordinator. The 'IdleTimeout' is global
-- to this coordinator.
newWatermarkCoordinator :: IdleTimeout -> IO WatermarkCoordinator
newWatermarkCoordinator idle = do
  src <- newTVarIO Map.empty
  wc  <- newTVarIO minTimestamp
  gs  <- newTVarIO Map.empty
  pure WatermarkCoordinator
    { wcSources    = src
    , wcIdleAfter  = idle
    , wcWallClock  = wc
    , wcGroups     = gs
    }

-- | Register a source. Idempotent: re-registering a source name
-- resets its state (useful on rebalance).
registerSource
  :: WatermarkCoordinator
  -> SourceId
  -> WatermarkStrategy
  -> Maybe Text                   -- ^ Optional alignment group
  -> IO ()
registerSource coord sid strat grp =
  atomically $ modifyTVar' (wcSources coord) $
    Map.insert sid SourceState
      { sstWatermark = minTimestamp
      , sstStrategy  = strat
      , sstIdleSince = Nothing
      , sstGroup     = grp
      }

unregisterSource :: WatermarkCoordinator -> SourceId -> IO ()
unregisterSource coord sid =
  atomically $ modifyTVar' (wcSources coord) (Map.delete sid)

-- | Report a record's timestamp for a source. The source's
-- watermark is updated via its strategy. The return value is the
-- /new/ per-source watermark.
reportRecord
  :: WatermarkCoordinator
  -> SourceId
  -> Timestamp
  -> IO Timestamp
reportRecord coord sid recordTs = atomically $ do
  m <- readTVar (wcSources coord)
  case Map.lookup sid m of
    Nothing -> pure minTimestamp
    Just s  -> do
      let !new = runStrategy (sstStrategy s) (sstWatermark s) recordTs
          !s'  = s { sstWatermark = new
                   , sstIdleSince = Nothing
                   }
      writeTVar (wcSources coord) (Map.insert sid s' m)
      pure new

-- | Explicitly mark a source idle as of the current wall-clock
-- timestamp. Idempotent.
markIdle :: WatermarkCoordinator -> SourceId -> IO ()
markIdle coord sid = atomically $ do
  wc <- readTVar (wcWallClock coord)
  modifyTVar' (wcSources coord) $ Map.adjust
    (\s -> case sstIdleSince s of
       Just _  -> s
       Nothing -> s { sstIdleSince = Just wc })
    sid

-- | Mark a source active again (clears the idle flag without
-- waiting for the next record).
markActive :: WatermarkCoordinator -> SourceId -> IO ()
markActive coord sid = atomically $ modifyTVar' (wcSources coord) $
  Map.adjust (\s -> s { sstIdleSince = Nothing }) sid

-- | Advance the coordinator's wall-clock. The runtime should
-- call this on each commit-cycle / poll iteration so idle
-- detection has up-to-date timing.
advanceWallClock :: WatermarkCoordinator -> Timestamp -> IO ()
advanceWallClock coord t = atomically $ writeTVar (wcWallClock coord) t

-- | The /effective/ watermark = min of live, non-idle sources.
-- A source counts as live iff it has been registered AND either
-- it is not marked idle or its idle period is shorter than
-- 'wcIdleAfter'.
--
-- If all sources are idle (or none are registered), returns
-- 'minTimestamp'.
currentEffectiveWatermark :: WatermarkCoordinator -> IO Timestamp
currentEffectiveWatermark coord = atomically $ do
  m  <- readTVar (wcSources coord)
  wc <- readTVar (wcWallClock coord)
  let IdleTimeout idle = wcIdleAfter coord
      isLive s = case sstIdleSince s of
        Nothing  -> True
        Just t0  -> wc < addDuration t0 idle
      lives = [ sstWatermark s | s <- Map.elems m, isLive s ]
  pure $ case lives of
    [] -> minTimestamp
    xs -> minimum xs

-- | Snapshot of every source's current watermark, idle flag, and
-- alignment group. Useful for diagnostics and for the alignment
-- query path.
perSourceWatermarks
  :: WatermarkCoordinator
  -> IO [(SourceId, Timestamp, Bool, Maybe Text)]
perSourceWatermarks coord = atomically $ do
  m  <- readTVar (wcSources coord)
  wc <- readTVar (wcWallClock coord)
  let IdleTimeout idle = wcIdleAfter coord
      isIdle s = case sstIdleSince s of
        Nothing  -> False
        Just t0  -> wc >= addDuration t0 idle
  pure [ (sid, sstWatermark s, isIdle s, sstGroup s)
       | (sid, s) <- Map.toAscList m ]

----------------------------------------------------------------------
-- Alignment groups
----------------------------------------------------------------------

-- | An alignment group bounds the spread between the fastest and
-- the slowest source within it. Used by Flink's
-- @WatermarkAlignmentSupplier@: if you join a slow stream with a
-- fast one, you don't want the fast one to race ahead and buffer
-- state for thousands of milliseconds waiting for the slow one
-- to catch up.
data AlignmentGroup = AlignmentGroup
  { agName  :: !Text
  , agBound :: !Duration
  } deriving stock (Eq, Show, Generic)

-- | Register / update an alignment group's bound.
declareAlignmentGroup
  :: WatermarkCoordinator
  -> AlignmentGroup
  -> IO ()
declareAlignmentGroup coord (AlignmentGroup name bound) =
  atomically $ modifyTVar' (wcGroups coord)
    (Map.insert name (AlignmentBound bound))

-- | Current backlog of one source relative to its alignment
-- group's slowest member, in milliseconds. Returns 0 when the
-- source has no group, when the group has only one member, or
-- when the source is the slowest in its group.
alignmentBacklog
  :: WatermarkCoordinator
  -> SourceId
  -> IO Int64
alignmentBacklog coord sid = atomically $ do
  m <- readTVar (wcSources coord)
  case Map.lookup sid m of
    Nothing -> pure 0
    Just s  -> case sstGroup s of
      Nothing  -> pure 0
      Just grp ->
        let peers = [ p
                    | p <- Map.elems m
                    , sstGroup p == Just grp
                    ]
            slowest = case peers of
              [] -> sstWatermark s
              _  -> minimum (sstWatermark <$> peers)
            Timestamp me     = sstWatermark s
            Timestamp slow   = slowest
        in pure (me - slow)

-- | The coordinator's recommendation: should this source pause
-- emitting new records because it has out-paced its alignment
-- group beyond the configured bound? The caller is expected to
-- honour this advice by deferring 'feedSource' / 'pollMC' on the
-- corresponding partition. If the source has no group, returns
-- 'False'.
shouldPauseSource :: WatermarkCoordinator -> SourceId -> IO Bool
shouldPauseSource coord sid = atomically $ do
  m  <- readTVar (wcSources coord)
  gs <- readTVar (wcGroups coord)
  case Map.lookup sid m of
    Nothing -> pure False
    Just s  -> case sstGroup s of
      Nothing  -> pure False
      Just grp -> case Map.lookup grp gs of
        Nothing -> pure False
        Just (AlignmentBound bound) -> do
          let peers = [ sstWatermark p
                      | p <- Map.elems m
                      , sstGroup p == Just grp
                      ]
              slowest = case peers of
                [] -> sstWatermark s
                _  -> minimum peers
              Timestamp me   = sstWatermark s
              Timestamp slow = slowest
              spread = me - slow
          pure (spread > durationMillis bound)
