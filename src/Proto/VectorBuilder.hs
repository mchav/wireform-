{-# LANGUAGE BangPatterns #-}
-- | Mutable growing vector builder for O(1) amortized snoc.
--
-- During proto decoding, repeated fields arrive one element at a time.
-- Using @V.snoc@ is O(n) per element (copies the whole vector).
-- Using a list with reverse is O(n) but allocates n cons cells plus
-- the final vector copy.
--
-- This module provides a mutable growing buffer that:
--
-- * Starts at a small capacity (8 elements)
-- * Doubles on overflow (amortized O(1) append)
-- * Freezes to an immutable vector with a single copy at the end
-- * Works in the ST or IO monad
--
-- For the CPS decoder, we thread the builder through the pure decode
-- loop by using an unsafe trick: the builder is created before the
-- decode loop and mutated during it, then frozen after.
module Proto.VectorBuilder
  ( -- * Boxed vector builder
    VecBuilder
  , newVecBuilder
  , pushVecBuilder
  , freezeVecBuilder
  , vecBuilderLength

    -- * Unboxed vector builder
  , UVecBuilder
  , newUVecBuilder
  , pushUVecBuilder
  , freezeUVecBuilder
  , uvecBuilderLength

    -- * Pure snoc-list that converts efficiently to Vector
  , GrowList (..)
  , emptyGrowList
  , snocGrowList
  , growListToVector
  , growListToVectorU
  , growListLength
  ) where

import Control.Monad.ST (ST, runST)
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU

-- | A growing boxed vector builder in IO.
data VecBuilder a = VecBuilder
  { vbBuf :: !(IORef (MV.IOVector a))
  , vbLen :: !(IORef Int)
  }

newVecBuilder :: IO (VecBuilder a)
newVecBuilder = do
  buf <- MV.new 8
  VecBuilder <$> newIORef buf <*> newIORef 0

pushVecBuilder :: VecBuilder a -> a -> IO ()
pushVecBuilder (VecBuilder bufRef lenRef) x = do
  len <- readIORef lenRef
  buf <- readIORef bufRef
  buf' <- if len >= MV.length buf
    then MV.grow buf (MV.length buf)
    else pure buf
  MV.write buf' len x
  writeIORef bufRef buf'
  writeIORef lenRef (len + 1)
{-# INLINE pushVecBuilder #-}

freezeVecBuilder :: VecBuilder a -> IO (V.Vector a)
freezeVecBuilder (VecBuilder bufRef lenRef) = do
  len <- readIORef lenRef
  buf <- readIORef bufRef
  V.freeze (MV.take len buf)

vecBuilderLength :: VecBuilder a -> IO Int
vecBuilderLength (VecBuilder _ lenRef) = readIORef lenRef

-- | A growing unboxed vector builder in IO.
data UVecBuilder a = UVecBuilder
  { uvbBuf :: !(IORef (MVU.IOVector a))
  , uvbLen :: !(IORef Int)
  }

newUVecBuilder :: VU.Unbox a => IO (UVecBuilder a)
newUVecBuilder = do
  buf <- MVU.new 8
  UVecBuilder <$> newIORef buf <*> newIORef 0

pushUVecBuilder :: VU.Unbox a => UVecBuilder a -> a -> IO ()
pushUVecBuilder (UVecBuilder bufRef lenRef) x = do
  len <- readIORef lenRef
  buf <- readIORef bufRef
  buf' <- if len >= MVU.length buf
    then MVU.grow buf (MVU.length buf)
    else pure buf
  MVU.write buf' len x
  writeIORef bufRef buf'
  writeIORef lenRef (len + 1)
{-# INLINE pushUVecBuilder #-}

freezeUVecBuilder :: VU.Unbox a => UVecBuilder a -> IO (VU.Vector a)
freezeUVecBuilder (UVecBuilder bufRef lenRef) = do
  len <- readIORef lenRef
  buf <- readIORef bufRef
  VU.freeze (MVU.take len buf)

uvecBuilderLength :: UVecBuilder a -> IO Int
uvecBuilderLength (UVecBuilder _ lenRef) = readIORef lenRef

-- Pure growing list — doesn't require IO/ST, but materializes to Vector
-- with a single allocation at the end. Uses a chunked representation
-- to avoid the O(n) reverse.

-- | A pure snoc-efficient list that converts to Vector in O(n).
-- Stores elements in reverse order in chunks, with a running count.
data GrowList a = GrowList
  { glElems :: ![a]     -- elements in reverse order
  , glCount :: {-# UNPACK #-} !Int
  }

emptyGrowList :: GrowList a
emptyGrowList = GrowList [] 0
{-# INLINE emptyGrowList #-}

snocGrowList :: GrowList a -> a -> GrowList a
snocGrowList (GrowList xs n) x = GrowList (x : xs) (n + 1)
{-# INLINE snocGrowList #-}

growListLength :: GrowList a -> Int
growListLength = glCount
{-# INLINE growListLength #-}

-- | Convert to a boxed Vector. Allocates the vector once and fills
-- it back-to-front from the reversed list — no intermediate list copy.
growListToVector :: GrowList a -> V.Vector a
growListToVector (GrowList xs n) = V.create $ do
  mv <- MV.new n
  let go !_ [] = pure ()
      go !i (e:es) = MV.write mv i e >> go (i - 1) es
  go (n - 1) xs
  pure mv
{-# INLINE growListToVector #-}

-- | Convert to an unboxed Vector.
growListToVectorU :: VU.Unbox a => GrowList a -> VU.Vector a
growListToVectorU (GrowList xs n) = VU.create $ do
  mv <- MVU.new n
  let go !_ [] = pure ()
      go !i (e:es) = MVU.write mv i e >> go (i - 1) es
  go (n - 1) xs
  pure mv
{-# INLINE growListToVectorU #-}
