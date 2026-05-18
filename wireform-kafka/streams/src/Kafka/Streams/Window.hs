{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Kafka.Streams.Window
-- Description : Window types and assignment policies
--
-- Mirrors @org.apache.kafka.streams.kstream.{Tumbling,Hopping,Sliding,Session}Windows@.
-- A 'Windows' value computes, for each input timestamp, the set of
-- 'Window's that cover it.
module Kafka.Streams.Window
  ( -- * Window
    Window (..)
  , windowSize
  , windowContains
  , windowOverlaps
    -- * Window assignment
  , Windows (..)
  , tumblingWindows
  , hoppingWindows
  , slidingWindows
  , unlimitedWindows
  , withGracePeriod
  , withWindowsRetention
    -- * Session
  , SessionWindows (..)
  , sessionWindows
  , withSessionGracePeriod
  , mergeSession
  ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Kafka.Streams.Time
  ( Duration
  , Timestamp (..)
  , durationMillis
  )

-- | Half-open window @[start, end)@.
data Window = Window
  { windowStart :: !Timestamp
  , windowEnd   :: !Timestamp
  }
  deriving stock (Eq, Ord, Show, Generic)

windowSize :: Window -> Int64
windowSize (Window (Timestamp s) (Timestamp e)) = e - s

windowContains :: Window -> Timestamp -> Bool
windowContains (Window s e) t = t >= s && t < e

windowOverlaps :: Window -> Window -> Bool
windowOverlaps (Window s1 e1) (Window s2 e2) = s1 < e2 && s2 < e1

-- | A windowing policy: given a record timestamp, returns the list
-- of windows that include it. Tumbling produces exactly one window;
-- hopping / sliding can produce many.
--
-- 'windowsRetention' is how long old window entries are kept around
-- so late-arriving records can still update them; the window store
-- enforces it.
data Windows = Windows
  { windowsAssign     :: !(Timestamp -> [Window])
  , windowsSize       :: !Int64
  , windowsAdvance    :: !Int64
  , windowsRetention  :: !Int64
  , windowsGracePeriod :: !Int64
  }

-- | Tumbling (non-overlapping) windows of fixed size. @advance == size@.
tumblingWindows :: Duration -> Windows
tumblingWindows size =
  let !sz = durationMillis size
   in Windows
        { windowsAssign = \(Timestamp t) ->
            let !start = t - (t `mod` sz)
             in [Window (Timestamp start) (Timestamp (start + sz))]
        , windowsSize   = sz
        , windowsAdvance = sz
        , windowsRetention = sz * 2
        , windowsGracePeriod = 0
        }

-- | Hopping windows: every @advance@ ms a new window opens at an
-- advance-aligned offset; each window has the configured size.
--
-- * If @advance < size@ the windows overlap and a record falls
--   into @ceil(size \/ advance)@ of them.
-- * If @advance == size@ the policy degenerates to tumbling; each
--   record falls into exactly one window.
-- * If @advance > size@ there are /gaps/ between windows; a
--   record whose timestamp falls in a gap belongs to no window
--   and 'windowsAssign' returns @[]@.
--
-- Every returned window has a start that is a multiple of
-- @advance@ — the property is exercised by
-- 'Streams.Antithesis.WindowMathSpec'.
hoppingWindows :: Duration -> Duration -> Windows
hoppingWindows size advance =
  let !sz = durationMillis size
      !ad = max 1 (durationMillis advance)
   in Windows
        { windowsAssign = \(Timestamp t) ->
            -- A window with start @k * ad@ covers @t@ iff
            -- @k * ad <= t < k * ad + sz@. Rearranging:
            --
            --   @ceil((t - sz + 1) \/ ad) <= k <= floor(t \/ ad)@.
            --
            -- When the lower bound exceeds the upper bound, the
            -- record falls in a gap (only possible when
            -- @ad > sz@) and the policy assigns no windows.
            let !kHi    = t `div` ad
                !kLoNum = t - sz + 1
                !kLo    =
                  if kLoNum <= 0
                    then 0
                    else (kLoNum + ad - 1) `div` ad
                mk k =
                  let !s = k * ad
                  in Window (Timestamp s) (Timestamp (s + sz))
            in if kLo > kHi
                 then []
                 else map mk [kLo .. kHi]
        , windowsSize     = sz
        , windowsAdvance  = ad
        , windowsRetention = sz + ad
        , windowsGracePeriod = 0
        }

-- | Set the grace period on any 'Windows' policy. Records whose
-- right edge has been closed for longer than 'grace' are dropped
-- by 'Kafka.Streams.TimeWindowedKStream' (KIP-633).
withGracePeriod :: Duration -> Windows -> Windows
withGracePeriod g w = w { windowsGracePeriod = durationMillis g }

-- | Override the retention period on a 'Windows' policy. Useful
-- when the default @size * 2@ is not long enough to keep late
-- records around (e.g. with a long grace period).
withWindowsRetention :: Duration -> Windows -> Windows
withWindowsRetention r w = w { windowsRetention = durationMillis r }

-- | Set the grace period on a 'SessionWindows' policy.
withSessionGracePeriod :: Duration -> SessionWindows -> SessionWindows
withSessionGracePeriod g sw = sw { swGracePeriod = durationMillis g }

-- | Sliding windows (KIP-450): a fixed-size window slides over time
-- and only one window per record (the window whose right edge is the
-- record timestamp).
slidingWindows :: Duration -> Windows
slidingWindows size =
  let !sz = durationMillis size
   in Windows
        { windowsAssign = \(Timestamp t) ->
            [Window (Timestamp (t - sz + 1)) (Timestamp (t + 1))]
        , windowsSize     = sz
        , windowsAdvance  = 1
        , windowsRetention = sz * 2
        , windowsGracePeriod = 0
        }

-- | Unlimited windows. Mirrors the (deprecated-since-4.0)
-- @org.apache.kafka.streams.kstream.UnlimitedWindows@: every
-- record falls into exactly one window that starts at the
-- record's timestamp and extends forever.
--
-- Java's JVM @UnlimitedWindows.of()@ takes no parameters; we
-- expose a single nullary smart constructor that uses
-- 'maxBound' as the right edge so 'windowContains' is total.
-- Use with caution — retention is effectively infinite, so a
-- topology built on 'unlimitedWindows' must explicitly
-- 'withWindowsRetention' to a finite value or the state store
-- grows without bound.
unlimitedWindows :: Windows
unlimitedWindows = Windows
  { windowsAssign = \(Timestamp t) ->
      [Window (Timestamp t) (Timestamp maxBound)]
  , windowsSize        = maxBound
  , windowsAdvance     = maxBound
  , windowsRetention   = maxBound
  , windowsGracePeriod = 0
  }

-- | Session windows: dynamic; sessions extend by @inactivityGap@.
data SessionWindows = SessionWindows
  { swInactivityGap   :: !Int64
  , swGracePeriod     :: !Int64
  , swRetention       :: !Int64
  }

sessionWindows :: Duration -> SessionWindows
sessionWindows gap =
  let !g = durationMillis gap
   in SessionWindows
        { swInactivityGap = g
        , swGracePeriod   = 0
        , swRetention     = g * 2
        }

-- | Merge two sessions iff they touch. Returns 'Nothing' otherwise.
mergeSession :: SessionWindows -> Window -> Window -> Maybe Window
mergeSession sw (Window s1 e1) (Window s2 e2) =
  let g = swInactivityGap sw
      gap = abs (timestampDeltaMs s2 e1)
      gap' = abs (timestampDeltaMs s1 e2)
   in if gap <= g || gap' <= g
        then Just (Window (min s1 s2) (max e1 e2))
        else Nothing
  where
    timestampDeltaMs (Timestamp a) (Timestamp b) = a - b
