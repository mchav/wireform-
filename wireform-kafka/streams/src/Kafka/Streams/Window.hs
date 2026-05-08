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
    -- * Session
  , SessionWindows (..)
  , sessionWindows
  , mergeSession
  ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Kafka.Streams.Time
  ( Duration
  , Timestamp (..)
  , addDuration
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

-- | Hopping windows: every @advance@ ms a new window opens; they
-- overlap if @advance < size@. Each record falls into
-- @ceil(size/advance)@ windows.
hoppingWindows :: Duration -> Duration -> Windows
hoppingWindows size advance =
  let !sz = durationMillis size
      !ad = max 1 (durationMillis advance)
   in Windows
        { windowsAssign = \(Timestamp t) ->
            -- The smallest start s such that s + sz > t and (s mod ad) == 0
            -- is t - ((t mod ad) + sz - ad). We then iterate until s > t.
            let firstStart = t - (((t `mod` ad) + sz - ad) `mod` sz)
                go s
                  | s > t     = []
                  | s + sz <= t = go (s + ad)
                  | otherwise =
                      Window (Timestamp s) (Timestamp (s + sz)) : go (s + ad)
                walk = go firstStart
             in walk
        , windowsSize     = sz
        , windowsAdvance  = ad
        , windowsRetention = sz + ad
        , windowsGracePeriod = 0
        }

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

-- Force imports we need so the module compiles without dead-code
-- warnings on @addDuration@.
_silence :: Timestamp -> Duration -> Timestamp
_silence = addDuration
