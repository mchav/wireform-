-- Copyright © 2018 chessai
-- BSD-3-Clause license (see NOTICE for full text)
--
-- Unpacked Either using unboxed sums for zero-allocation branching.
-- Internal module — public API boundaries convert to standard Either.

{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums     #-}

module Proto.Internal.Either
  ( Either(Either, Left, Right)
  , either
  , fromBaseEither
  , toBaseEither
  ) where

import Prelude ()
import Data.Function ((.))
import qualified Data.Either as BaseEither

data Either a b = Either (# a | b #)

pattern Left :: a -> Either a b
pattern Left a = Either (# a | #)

pattern Right :: b -> Either a b
pattern Right b = Either (# | b #)

{-# COMPLETE Left, Right #-}

either :: (a -> c) -> (b -> c) -> Either a b -> c
either fa fb (Either x) = case x of
  (# a | #) -> fa a
  (# | b #) -> fb b
{-# INLINE either #-}

fromBaseEither :: BaseEither.Either a b -> Either a b
fromBaseEither (BaseEither.Left a) = Left a
fromBaseEither (BaseEither.Right b) = Right b
{-# INLINE fromBaseEither #-}

toBaseEither :: Either a b -> BaseEither.Either a b
toBaseEither = either BaseEither.Left BaseEither.Right
{-# INLINE toBaseEither #-}
