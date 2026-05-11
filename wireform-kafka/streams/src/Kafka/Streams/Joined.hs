{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- |
-- Module      : Kafka.Streams.Joined
-- Description : @Joined<K,V1,V2>@ DSL config + window join configuration
module Kafka.Streams.Joined
  ( Joined (..)
  , joined
  , JoinWindows (..)
  , joinWindowsBefore
  , joinWindowsAfter
  , symmetricJoinWindows
  , withJoinWindowsGrace
    -- * Modern builders
  , ofTimeDifferenceWithNoGrace
  , ofTimeDifferenceAndGrace
    -- * Sliding windows
  , slidingWindowsOf
  , slidingWindowsWithGrace
    -- * StreamJoined
  , StreamJoined (..)
  , streamJoined
  , withStreamJoinedName
    -- * TableJoined
  , TableJoined (..)
  , tableJoined
  , withTableJoinedName
  , withTableJoinedPartitioner
  ) where

import Data.Text (Text)

import Kafka.Streams.Serde (Serde)
import Kafka.Streams.Time (Duration, durationMillis)
import Data.Int (Int64)

data Joined k v1 v2 = Joined
  { keySerde   :: !(Serde k)
  , v1Serde    :: !(Serde v1)
  , v2Serde    :: !(Serde v2)
  , name       :: !(Maybe Text)
  }

joined :: Serde k -> Serde v1 -> Serde v2 -> Joined k v1 v2
joined ks v1s v2s = Joined
  { keySerde = ks
  , v1Serde  = v1s
  , v2Serde  = v2s
  , name     = Nothing
  }

-- | Window over which two records are considered to "match" in a
-- KStream-KStream window join. The window is asymmetric: a record on
-- the left can match a record on the right that arrived up to
-- 'beforeMs' before, and up to 'afterMs' after.
data JoinWindows = JoinWindows
  { beforeMs      :: !Int64
  , afterMs       :: !Int64
  , gracePeriodMs :: !Int64
  }

joinWindowsBefore :: Duration -> JoinWindows
joinWindowsBefore d = JoinWindows
  { beforeMs      = durationMillis d
  , afterMs       = 0
  , gracePeriodMs = 0
  }

joinWindowsAfter :: Duration -> JoinWindows
joinWindowsAfter d = JoinWindows
  { beforeMs      = 0
  , afterMs       = durationMillis d
  , gracePeriodMs = 0
  }

symmetricJoinWindows :: Duration -> JoinWindows
symmetricJoinWindows d =
  let !ms = durationMillis d
   in JoinWindows
        { beforeMs      = ms
        , afterMs       = ms
        , gracePeriodMs = 0
        }

-- | Override the grace period on a 'JoinWindows'. Mirrors Java's
-- @JoinWindows.ofTimeDifference(...).grace(...)@.
withJoinWindowsGrace :: Duration -> JoinWindows -> JoinWindows
withJoinWindowsGrace g jw = jw { gracePeriodMs = durationMillis g }

----------------------------------------------------------------------
-- Sliding windows (KIP-450)
----------------------------------------------------------------------

-- | Sliding window of total time-difference @d@: a record on either
-- side matches another within @[-d, +d]@. Mirrors Java's
-- @SlidingWindows.ofTimeDifferenceWithNoGrace(Duration)@.
--
-- Internally a sliding window is a symmetric 'JoinWindows' — the
-- distinct type in Java enables a different join /algorithm/
-- (single buffer per key with full overlap), but the user-facing
-- semantics here (left × right matches when ts difference <= d)
-- are identical and the 'symmetricJoinWindows' implementation
-- already handles them faithfully.
slidingWindowsOf :: Duration -> JoinWindows
slidingWindowsOf = symmetricJoinWindows

-- | Sliding windows with an explicit grace period.
slidingWindowsWithGrace :: Duration -> Duration -> JoinWindows
slidingWindowsWithGrace size grace =
  withJoinWindowsGrace grace (slidingWindowsOf size)

----------------------------------------------------------------------
-- KIP-633 modern JoinWindows builders
----------------------------------------------------------------------

-- | Symmetric @[-d, +d]@ join window with no grace period.
-- Mirrors Java's @JoinWindows.ofTimeDifferenceWithNoGrace(d)@.
ofTimeDifferenceWithNoGrace :: Duration -> JoinWindows
ofTimeDifferenceWithNoGrace = symmetricJoinWindows

-- | Symmetric @[-d, +d]@ join window with an explicit grace.
-- Mirrors Java's @JoinWindows.ofTimeDifferenceAndGrace(d, g)@.
ofTimeDifferenceAndGrace :: Duration -> Duration -> JoinWindows
ofTimeDifferenceAndGrace size grace =
  withJoinWindowsGrace grace (symmetricJoinWindows size)

----------------------------------------------------------------------
-- StreamJoined (KIP-479)
----------------------------------------------------------------------

-- | Configuration record for KStream-KStream window joins.
-- Mirrors Java's @StreamJoined<K, V1, V2>@: carries the serdes
-- for both sides + the left and right state-store names. The
-- DSL combinator (e.g. @joinKStreamKStream@) materialises a
-- per-side buffer store when the names are present.
--
-- Most users default to 'streamJoined' (auto-named buffers);
-- production callers override store names to share buffers
-- across stages or apply custom topic configuration.
data StreamJoined k v1 v2 = StreamJoined
  { keySerde   :: !(Serde k)
  , v1Serde    :: !(Serde v1)
  , v2Serde    :: !(Serde v2)
  , name       :: !(Maybe Text)
  , leftStore  :: !(Maybe Text)
  , rightStore :: !(Maybe Text)
  }

-- | Build a default 'StreamJoined' from three serdes; store
-- names + processor name are auto-synthesised.
streamJoined :: Serde k -> Serde v1 -> Serde v2 -> StreamJoined k v1 v2
streamJoined ks v1s v2s = StreamJoined
  { keySerde   = ks
  , v1Serde    = v1s
  , v2Serde    = v2s
  , name       = Nothing
  , leftStore  = Nothing
  , rightStore = Nothing
  }

withStreamJoinedName
  :: Text -> StreamJoined k v1 v2 -> StreamJoined k v1 v2
withStreamJoinedName n s = s { name = Just n }

----------------------------------------------------------------------
-- TableJoined (KIP-545)
----------------------------------------------------------------------

-- | @TableJoined<K, KO>@ — partitioner override for
-- KTable-KTable foreign-key joins. The partitioner functions
-- decide which partition a join-side record routes to on the
-- internal subscription / response topics.
data TableJoined k ko = TableJoined
  { name             :: !(Maybe Text)
  , leftPartitioner  :: !(Maybe (Text -> Maybe k  -> Int -> Int))
  , otherPartitioner :: !(Maybe (Text -> Maybe ko -> Int -> Int))
  }

tableJoined :: TableJoined k ko
tableJoined = TableJoined
  { name             = Nothing
  , leftPartitioner  = Nothing
  , otherPartitioner = Nothing
  }

withTableJoinedName :: Text -> TableJoined k ko -> TableJoined k ko
withTableJoinedName n t = t { name = Just n }

withTableJoinedPartitioner
  :: (Text -> Maybe k -> Int -> Int)        -- left
  -> (Text -> Maybe ko -> Int -> Int)       -- other
  -> TableJoined k ko
  -> TableJoined k ko
withTableJoinedPartitioner lp op t = t
  { leftPartitioner  = Just lp
  , otherPartitioner = Just op
  }
