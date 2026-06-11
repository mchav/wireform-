{- | A pool of pre-allocated 'MagicRing' buffers for reuse.

Allocating a magic ring costs @memfd_create@ + @ftruncate@ + 3×
@mmap@ + @memset@ per ring. This pool keeps a bounded free list of
rings so connection churn doesn't repeat that syscall work.

Rings are stored as raw @(Ptr Word8, Int)@ pairs — no IORefs, no
per-ring mutable state. The transport-layer cursor state (head,
tail, open\/closed) is always fresh per connection; only the
underlying mmap'd memory is recycled.
-}
module Wireform.Ring.Pool (
  RingPool,
  RingPoolConfig (..),
  defaultRingPoolConfig,
  newRingPool,
  closeRingPool,
  acquireRing,
  releaseRing,
) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, mask_, try)
import Control.Monad (forM_)
import GHC.Exts (RealWorld)
import Wireform.Ring.Internal (MagicRing (..), destroyMagicRing, newMagicRing)


data RingPoolConfig = RingPoolConfig
  { ringPoolMaxIdle :: !Int
  }


defaultRingPoolConfig :: RingPoolConfig
defaultRingPoolConfig =
  RingPoolConfig
    { ringPoolMaxIdle = 64
    }


{- | The pool is just a bounded free list protected by an MVar.
All rings in the list have the same allocated size (the first
allocation's rounded-up size sets the class).
-}
data RingPool = RingPool
  { rpMaxIdle :: !Int
  , rpFree :: !(MVar [MagicRing RealWorld])
  }


newRingPool :: RingPoolConfig -> IO RingPool
newRingPool cfg = do
  free <- newMVar []
  pure
    RingPool
      { rpMaxIdle = ringPoolMaxIdle cfg
      , rpFree = free
      }


{- | Destroy all idle rings. The pool remains usable (acquire
allocates fresh, release destroys immediately).
-}
closeRingPool :: RingPool -> IO ()
closeRingPool pool = mask_ $ do
  rings <- swapMVar (rpFree pool) []
  forM_ rings $ \r ->
    () <$ try @SomeException (destroyMagicRing r)


-- | Take a ring from the pool or allocate a fresh one.
acquireRing :: RingPool -> Int -> IO (MagicRing s)
acquireRing pool requested = mask_ $ do
  rings <- takeMVar (rpFree pool)
  case rings of
    (r : rs) | mrSize r >= requested -> do
      putMVar (rpFree pool) rs
      pure (coerceRing r)
    _ -> do
      putMVar (rpFree pool) rings
      newMagicRing requested


{- | Return a ring to the pool. Destroyed immediately if the pool
is full.
-}
releaseRing :: RingPool -> MagicRing s -> IO ()
releaseRing pool ring = mask_ $ do
  rings <- takeMVar (rpFree pool)
  if length rings >= rpMaxIdle pool
    then do
      putMVar (rpFree pool) rings
      destroyMagicRing ring
    else
      putMVar (rpFree pool) (coerceRing ring : rings)


-- MagicRing's phantom s is nominal, but the pool stores them as
-- RealWorld and hands them out polymorphically. The underlying
-- data is just (Ptr Word8, Int) — the coercion is safe.
coerceRing :: MagicRing s -> MagicRing s'
coerceRing (MagicRing p n) = MagicRing p n
