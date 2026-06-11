-- Copyright © 2016 Kyle McKean
-- Copyright © 2018 Daniel Cartwright
-- BSD-3-Clause license (see LICENSE for full text)
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Unpacked Maybe using unboxed sums for zero-allocation optionals.

Internal module — public API boundaries convert to standard Maybe.
Uses GHC's @UnboxedTuples@ extension so the Just\/Nothing distinction
lives on the stack, not the heap.
-}
module Proto.Internal.Maybe (
  Maybe (Maybe, Just, Nothing),
  maybe,
  isJust,
  isNothing,
  fromBaseMaybe,
  toBaseMaybe,
) where

import Data.Function (const)
import Data.Maybe qualified as BaseMaybe
import GHC.Base (Bool (False, True))
import Prelude ()


data Maybe a = Maybe (# (# #) | a #)


pattern Just :: a -> Maybe a
pattern Just a = Maybe (# | a #)


pattern Nothing :: Maybe a
pattern Nothing = Maybe (# (# #) | #)


{-# COMPLETE Just, Nothing #-}


maybe :: b -> (a -> b) -> Maybe a -> b
maybe def f (Maybe x) = case x of
  (# (# #) | #) -> def
  (# | a #) -> f a
{-# INLINE maybe #-}


isJust :: Maybe a -> Bool
isJust = maybe False (const True)
{-# INLINE isJust #-}


isNothing :: Maybe a -> Bool
isNothing = maybe True (const False)
{-# INLINE isNothing #-}


fromBaseMaybe :: BaseMaybe.Maybe a -> Maybe a
fromBaseMaybe (BaseMaybe.Just x) = Just x
fromBaseMaybe BaseMaybe.Nothing = Nothing
{-# INLINE fromBaseMaybe #-}


toBaseMaybe :: Maybe a -> BaseMaybe.Maybe a
toBaseMaybe = maybe BaseMaybe.Nothing BaseMaybe.Just
{-# INLINE toBaseMaybe #-}
