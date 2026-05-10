{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Kafka.Streams.Runtime.ProbingRebalance
Description : KIP-441 probing rebalance scheduler

The Java client triggers a fresh @JoinGroup@ on a periodic cadence
(@probing.rebalance.interval.ms@) so the leader can check whether
warmup replicas have caught up enough to be promoted to active.
This module is the pure decision layer:

  * 'WarmupProgress'   — per-task lag against the active leader.
  * 'classifyWarmups'  — splits warmups into "ready" / "still
    catching up" using the config's @acceptable.recovery.lag@.
  * 'shouldProbe'      — returns 'True' when @now - lastProbe >=
    probing.rebalance.interval.ms@ /and/ at least one warmup is
    ready (i.e. there's something to rebalance over).

The runtime side (issuing a JoinGroup when 'shouldProbe' returns
'True') lives in the engine driver; tests against this module
exercise the math.
-}
module Kafka.Streams.Runtime.ProbingRebalance
  ( -- * Inputs
    WarmupProgress (..)
  , WarmupReadiness (..)
    -- * Decisions
  , classifyWarmups
  , readyWarmups
  , shouldProbe
  ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Kafka.Streams.Processor (TaskId)

-- | One warmup replica's progress: how far behind the active
-- task's changelog the warmup currently is. A 'wpLag' of 0 means
-- the replica is at the active's high-water mark and can take over
-- immediately.
data WarmupProgress = WarmupProgress
  { wpTask :: !TaskId
  , wpLag  :: !Int64
    -- ^ Records remaining to apply before the warmup matches the
    --   active leader's offset.
  }
  deriving stock (Eq, Show, Generic)

data WarmupReadiness
  = WarmupReady
  | WarmupCatchingUp
  deriving stock (Eq, Show, Generic)

-- | Classify a warmup as ready / catching-up against the
-- @acceptable.recovery.lag@ threshold. A negative threshold is
-- treated as zero.
classifyWarmups
  :: Int64           -- ^ acceptable.recovery.lag
  -> [WarmupProgress]
  -> [(WarmupProgress, WarmupReadiness)]
classifyWarmups lagThreshold = map step
  where
    !threshold = max 0 lagThreshold
    step w
      | wpLag w <= threshold = (w, WarmupReady)
      | otherwise            = (w, WarmupCatchingUp)

readyWarmups :: Int64 -> [WarmupProgress] -> [WarmupProgress]
readyWarmups lagThreshold ws =
  [ w | (w, r) <- classifyWarmups lagThreshold ws, r == WarmupReady ]

-- | Should we issue a probing rebalance now?
--
-- Returns 'True' when the cadence elapsed /and/ there's at least
-- one ready warmup (otherwise probing would just churn the group
-- with no realistic chance of a promotion).
shouldProbe
  :: Int64    -- ^ now (ms)
  -> Int64    -- ^ lastProbeAt (ms; 0 = never probed yet)
  -> Int      -- ^ probing.rebalance.interval.ms
  -> [WarmupProgress]
  -> Int64    -- ^ acceptable.recovery.lag
  -> Bool
shouldProbe now lastProbeAt intervalMs warmups lagThreshold
  | null (readyWarmups lagThreshold warmups) = False
  | intervalMs <= 0                          = False
  | otherwise                                =
      (now - lastProbeAt) >= fromIntegral intervalMs
