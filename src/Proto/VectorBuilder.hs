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

-- | Pure growing accumulator for repeated fields.
--
-- Uses reversed chunks of small Vectors to amortise allocation.
-- Each chunk holds up to 32 elements. Snoc fills the current chunk;
-- when full, the chunk is frozen and a new one starts. This reduces
-- per-element allocation from ~48 bytes (cons cell + GrowList node)
-- to ~8 bytes amortised (one pointer per element in the Vector).
--
-- Final materialisation concatenates the chunks back-to-front.
data GrowList a = GrowList
  { glChunks  :: ![V.Vector a]
  , glCurrent :: ![a]
  , glCurLen  :: {-# UNPACK #-} !Int
  , glTotal   :: {-# UNPACK #-} !Int
  }

chunkSize :: Int
chunkSize = 32

emptyGrowList :: GrowList a
emptyGrowList = GrowList [] [] 0 0
{-# INLINE emptyGrowList #-}

snocGrowList :: GrowList a -> a -> GrowList a
snocGrowList (GrowList chunks cur curLen total) x =
  let !curLen' = curLen + 1
      !total'  = total + 1
  in if curLen' >= chunkSize
     then let !chunk = V.fromListN curLen' (reverse (x : cur))
          in GrowList (chunk : chunks) [] 0 total'
     else GrowList chunks (x : cur) curLen' total'
{-# INLINE snocGrowList #-}

growListLength :: GrowList a -> Int
growListLength = glTotal
{-# INLINE growListLength #-}

growListToVector :: GrowList a -> V.Vector a
growListToVector (GrowList chunks cur curLen total)
  | total == 0 = V.empty
  | otherwise  = V.create $ do
      mv <- MV.new total
      let !lastChunk = if curLen > 0
            then V.fromListN curLen (reverse cur) : chunks
            else chunks
      let go !_ [] = pure ()
          go !off (c:cs) = do
            let !cLen = V.length c
                !off' = off - cLen
            V.unsafeCopy (MV.slice off' cLen mv) c
            go off' cs
      go total lastChunk
      pure mv
{-# INLINE growListToVector #-}

growListToVectorU :: VU.Unbox a => GrowList a -> VU.Vector a
growListToVectorU gl =
  let !bv = growListToVector gl
  in VU.convert bv
{-# INLINE growListToVectorU #-}
