{- | Simple per-connection rate limiters for HTTP\/2 control-plane
floods.

A misbehaving peer can hand us unlimited PING / SETTINGS /
RST_STREAM / empty-frame traffic that's individually cheap to
respond to but, in aggregate, will pin a connection-handling
thread.  This module provides the small counter+window
machinery; the recv loops decide which frame types to gate and
how to react when the limit trips (typically a GOAWAY with
@ENHANCE_YOUR_CALM@).

Rates are simple "events per second" caps using a sliding
window of 1 second.  Each call to 'tickRate' returns the new
count for the current window; the caller compares against the
configured cap.  We avoid 'Data.Time' on the hot path and use
'GHC.Clock.getMonotonicTime' (cheap, no thread-safety overhead).
-}
module Network.HTTP2.RateLimit
  ( RateCounter
  , newRateCounter
  , tickRate
  ) where

import Data.IORef
import GHC.Clock (getMonotonicTime)

-- | A single (count, window-start) pair.  Single-threaded use:
-- the recv loop is the only caller, so we don't need atomic
-- updates.
data RateCounter = RateCounter
  { rcCount :: !(IORef Int)
  , rcWindowStart :: !(IORef Double)
  }

newRateCounter :: IO RateCounter
newRateCounter = do
  c <- newIORef 0
  now <- getMonotonicTime
  ws <- newIORef now
  pure (RateCounter c ws)

-- | Record one event and return the new count for the current
-- 1-second window.  Resets the counter when the window has
-- rolled over.
tickRate :: RateCounter -> IO Int
tickRate rc = do
  now <- getMonotonicTime
  ws <- readIORef (rcWindowStart rc)
  if now - ws > 1.0
    then do
      writeIORef (rcWindowStart rc) now
      writeIORef (rcCount rc) 1
      pure 1
    else do
      n <- readIORef (rcCount rc)
      let n' = n + 1
      writeIORef (rcCount rc) n'
      pure n'
