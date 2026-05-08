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
