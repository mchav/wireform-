{-# LANGUAGE BangPatterns #-}

{- | Pure growing accumulator for repeated fields.

Uses a difference-list (Endo-style function composition with cons)
for O(1) amortised snoc, then materialises to Vector via fromListN.
-}
module Proto.Internal.GrowList (
  GrowList (..),
  emptyGrowList,
  snocGrowList,
  growListToVector,
  growListToVectorU,
  growListLength,
) where

import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as VU


data GrowList a = GrowList
  { glBuild :: !([a] -> [a])
  , glCount :: {-# UNPACK #-} !Int
  }


emptyGrowList :: GrowList a
emptyGrowList = GrowList id 0
{-# INLINE emptyGrowList #-}


snocGrowList :: GrowList a -> a -> GrowList a
snocGrowList (GrowList f n) x = GrowList (f . (x :)) (n + 1)
{-# INLINE snocGrowList #-}


growListLength :: GrowList a -> Int
growListLength = glCount
{-# INLINE growListLength #-}


-- | Materialise to a boxed Vector.
growListToVector :: GrowList a -> V.Vector a
growListToVector (GrowList f n)
  | n == 0 = V.empty
  | otherwise = V.create $ do
      let !xs = f []
      mv <- MV.new n
      let fill !_ [] = pure ()
          fill !i (e : es) = MV.unsafeWrite mv i e >> fill (i + 1) es
      fill 0 xs
      pure mv
{-# INLINE growListToVector #-}


-- | Materialise to an unboxed Vector.
growListToVectorU :: VU.Unbox a => GrowList a -> VU.Vector a
growListToVectorU gl = VU.convert (growListToVector gl)
{-# INLINE growListToVectorU #-}
