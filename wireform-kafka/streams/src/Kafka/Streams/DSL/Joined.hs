-- |
-- Module      : Kafka.Streams.DSL.Joined
-- Description : @Joined<K,V1,V2>@ DSL config + window join configuration
module Kafka.Streams.DSL.Joined
  ( Joined (..)
  , joined
  , JoinWindows (..)
  , joinWindowsBefore
  , joinWindowsAfter
  , symmetricJoinWindows
  , withJoinWindowsGrace
    -- * Sliding windows (KIP-450)
  , slidingWindowsOf
  , slidingWindowsWithGrace
  ) where

import Data.Text (Text)

import Kafka.Streams.Serde (Serde)
import Kafka.Streams.Time (Duration, durationMillis)
import Data.Int (Int64)

data Joined k v1 v2 = Joined
  { joinedKeySerde    :: !(Serde k)
  , joinedV1Serde     :: !(Serde v1)
  , joinedV2Serde     :: !(Serde v2)
  , joinedName        :: !(Maybe Text)
  }

joined :: Serde k -> Serde v1 -> Serde v2 -> Joined k v1 v2
joined ks v1s v2s = Joined
  { joinedKeySerde = ks
  , joinedV1Serde  = v1s
  , joinedV2Serde  = v2s
  , joinedName     = Nothing
  }

-- | Window over which two records are considered to "match" in a
-- KStream-KStream window join. The window is asymmetric: a record on
-- the left can match a record on the right that arrived up to
-- 'jwBeforeMs' before, and up to 'jwAfterMs' after.
data JoinWindows = JoinWindows
  { jwBeforeMs        :: !Int64
  , jwAfterMs         :: !Int64
  , jwGracePeriodMs   :: !Int64
  }

joinWindowsBefore :: Duration -> JoinWindows
joinWindowsBefore d = JoinWindows
  { jwBeforeMs      = durationMillis d
  , jwAfterMs       = 0
  , jwGracePeriodMs = 0
  }

joinWindowsAfter :: Duration -> JoinWindows
joinWindowsAfter d = JoinWindows
  { jwBeforeMs      = 0
  , jwAfterMs       = durationMillis d
  , jwGracePeriodMs = 0
  }

symmetricJoinWindows :: Duration -> JoinWindows
symmetricJoinWindows d =
  let !ms = durationMillis d
   in JoinWindows
        { jwBeforeMs      = ms
        , jwAfterMs       = ms
        , jwGracePeriodMs = 0
        }

-- | Override the grace period on a 'JoinWindows'. Mirrors Java's
-- @JoinWindows.ofTimeDifference(...).grace(...)@.
withJoinWindowsGrace :: Duration -> JoinWindows -> JoinWindows
withJoinWindowsGrace g jw = jw { jwGracePeriodMs = durationMillis g }

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
