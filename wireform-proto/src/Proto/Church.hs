{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- | Church-encoded data structures for high-performance protobuf operations.
--
-- Church encoding represents data types as their fold/elimination function.
-- This enables fusion — intermediate data structures are never allocated.
--
-- For protobuf decoding, the key win is in repeated field accumulation:
-- instead of building a list with (:) and reversing at the end (O(n) extra
-- allocation), we use Church-encoded sequences that support O(1) snoc
-- and O(1) conversion to the final representation.
module Proto.Church
  ( -- * Church-encoded list (CPS accumulator)
    ChurchList
  , emptyChurchList
  , snocChurchList
  , churchListToList
  , churchListToVector
  , churchListToVectorU
  , churchListLength

    -- * Difference list (fast repeated field accumulation)
  , DList
  , emptyDList
  , snocDList
  , dlistToList
  , dlistToVector
  , dlistToVectorU
  , singletonDList

    -- * CPS-encoded Maybe (avoids Maybe allocation in hot paths)
  , withMaybe
  , withJust
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- | A difference list: O(1) snoc, O(n) conversion to list.
-- This is the standard technique for avoiding quadratic append
-- in left-to-right accumulation.
newtype DList a = DList ([a] -> [a])

emptyDList :: DList a
emptyDList = DList id
{-# INLINE emptyDList #-}

singletonDList :: a -> DList a
singletonDList x = DList (x :)
{-# INLINE singletonDList #-}

snocDList :: DList a -> a -> DList a
snocDList (DList f) x = DList (f . (x :))
{-# INLINE snocDList #-}

dlistToList :: DList a -> [a]
dlistToList (DList f) = f []
{-# INLINE dlistToList #-}

dlistToVector :: DList a -> V.Vector a
dlistToVector = V.fromList . dlistToList
{-# INLINE dlistToVector #-}

dlistToVectorU :: VU.Unbox a => DList a -> VU.Vector a
dlistToVectorU = VU.fromList . dlistToList
{-# INLINE dlistToVectorU #-}

-- | Church-encoded list: the list IS its fold function.
--
-- @ChurchList a@ ≅ @forall r. (a -> r -> r) -> r -> r@
--
-- This is the Church encoding of lists. The list doesn't exist as a
-- data structure in memory — it's a function that, when given cons and nil,
-- produces the result directly. This enables GHC to fuse the fold with
-- the consumer (e.g., vector construction).
newtype ChurchList a = ChurchList { runChurchList :: forall r. (a -> r -> r) -> r -> r }

emptyChurchList :: ChurchList a
emptyChurchList = ChurchList (\_ nil -> nil)
{-# INLINE emptyChurchList #-}

-- | Snoc an element onto a Church list (O(1) amortized through CPS).
snocChurchList :: ChurchList a -> a -> ChurchList a
snocChurchList (ChurchList f) x = ChurchList (\cons nil -> f cons (cons x nil))
{-# INLINE snocChurchList #-}

churchListToList :: ChurchList a -> [a]
churchListToList (ChurchList f) = f (:) []
{-# INLINE churchListToList #-}

churchListToVector :: ChurchList a -> V.Vector a
churchListToVector = V.fromList . churchListToList
{-# INLINE churchListToVector #-}

churchListToVectorU :: VU.Unbox a => ChurchList a -> VU.Vector a
churchListToVectorU = VU.fromList . churchListToList
{-# INLINE churchListToVectorU #-}

churchListLength :: ChurchList a -> Int
churchListLength (ChurchList f) = f (\_ !n -> n + 1) 0
{-# INLINE churchListLength #-}

-- | CPS-encoded Maybe elimination — avoids allocating a Maybe
-- when the consumer immediately pattern-matches it.
-- This is the Church encoding of Maybe.
withMaybe :: Maybe a -> r -> (a -> r) -> r
withMaybe Nothing  def _ = def
withMaybe (Just a) _  f  = f a
{-# INLINE withMaybe #-}

-- | CPS version of fromJust with a default result.
withJust :: r -> Maybe a -> (a -> r) -> r
withJust def Nothing  _  = def
withJust _   (Just a) f  = f a
{-# INLINE withJust #-}
