{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
-- | Pull-based streaming primitives shared by every columnar
-- format ("Arrow.Stream", "Parquet.HighLevel", "ORC").
--
-- The point of this module is to give callers /one/ shape for
-- iterating a record batch / row group / stripe sequence, in
-- place of the lazy @[Either String a]@ / @[V.Vector ColumnArray]@
-- lists the per-format readers used to return.
--
-- == Why not lazy lists?
--
-- Lazy lists conflate three orthogonal concerns:
--
-- 1. /Pulling/ — the consumer wants to drive iteration explicitly
--    (e.g. read until a predicate, take @n@ batches, fold an
--    accumulator).
-- 2. /Errors/ — a per-batch decode failure used to surface as a
--    @Left e@ slot in the middle of a list, with no way to stop
--    iteration short of pattern-matching every element.
-- 3. /Resource lifetime/ — even though the underlying byte buffer
--    is in memory, the lazy list holds onto enough offsets to
--    keep every later batch reachable, so consumers that only
--    care about the first few batches still pay for the whole
--    list spine.
--
-- 'Iter' separates them. An 'Iter' value is a single suspended
-- step:
--
-- @
-- 'iterStep' :: 'Iter' a -> Either String ('IterStep' a)
-- @
--
-- which yields either @'IterDone'@ (the iterator is exhausted) or
-- @'IterYield' a next@ (here is the next element + the
-- continuation iterator). Errors live at the step level rather
-- than as a Either-element-of-list, so the consumer's loop is
-- the one place that decides whether to abort or keep going.
--
-- == Building blocks
--
-- * 'iterFromList' — wrap a strict list (handy bridging API).
-- * 'iterFromIndexed' — turn a @(Int, Int -> Either String a)@ pair
--   into an iterator. Used by every format's row-group / stripe /
--   batch reader: the underlying file already knows /how many/
--   batches there are and can decode each by index without
--   touching the others.
-- * 'iterMap', 'iterMapM', 'iterFilter', 'iterTake', 'iterDrop' —
--   the usual streaming combinators.
-- * 'toList', 'toListE', 'foldM' — drain helpers.
--
-- == Resource model
--
-- 'Iter' itself is pure: each step returns @Either String
-- IterStep@. There is no IO threaded through the iterator; if a
-- format needs IO (e.g. reading a fresh disk page) the file is
-- expected to be loaded into memory first (which the existing
-- "Parquet.Read" / "ORC.Read" code does anyway). When we add
-- streaming-from-disk it'll be a separate 'IterIO' shape;
-- keeping the pure shape for now matches the rest of the
-- columnar code's "ByteString in, columns out" boundary.
module Columnar.Stream
  ( -- * Core type
    Iter
  , IterStep (..)
  , iterStep
    -- * Constructors
  , iterEmpty
  , iterSingleton
  , iterFromList
  , iterFromVector
  , iterFromIndexed
  , iterUnfold
    -- * Transformations
  , iterMap
  , iterMapM
  , iterMapMaybe
  , iterFilter
  , iterTake
  , iterDrop
  , iterAppend
  , iterConcat
    -- * Drains
  , iterToList
  , iterToVector
  , iterFold
  , iterFoldM
  , iterForM_
  , iterLength
  ) where

import qualified Data.Vector as V

-- | A pull-based, error-aware, finite iterator.
--
-- Each 'Iter' is a single suspended step: ask it for an
-- 'IterStep' and you either get 'IterDone' (the stream has
-- ended) or 'IterYield' (here's the next element + the next
-- iterator).
--
-- The ergonomic difference from a list is that 'Iter' yields
-- /one element at a time/ and each element decode is allowed
-- to fail, with the caller choosing how to handle the failure.
-- A lazy @[Either String a]@ forces you to either thread the
-- error through every later element or risk exceptions; an
-- 'Iter' lets the caller @case@ on the step and break the loop.
newtype Iter a = Iter { iterStep :: Either String (IterStep a) }

instance Functor Iter where
  fmap f (Iter step) = Iter $ case step of
    Left e -> Left e
    Right IterDone -> Right IterDone
    Right (IterYield a next) -> Right (IterYield (f a) (fmap f next))

-- | One step of an 'Iter'. Either the iterator has ended or it
-- carries one element + the continuation.
data IterStep a
  = IterDone
  | IterYield !a !(Iter a)
  deriving (Functor)

-- ============================================================
-- Constructors
-- ============================================================

-- | The empty iterator.
iterEmpty :: Iter a
iterEmpty = Iter (Right IterDone)

-- | An iterator that yields one element and then ends.
iterSingleton :: a -> Iter a
iterSingleton a = Iter (Right (IterYield a iterEmpty))

-- | Wrap a list as an iterator. Each step pulls one element off
-- the head of the list. The list elements are forced to WHNF
-- when yielded.
iterFromList :: [a] -> Iter a
iterFromList = go
  where
    go []     = Iter (Right IterDone)
    go (x:xs) = Iter (Right (IterYield x (go xs)))

-- | Wrap a vector as an iterator. Indexes the vector lazily on
-- each step rather than copying.
iterFromVector :: V.Vector a -> Iter a
iterFromVector v0 = step 0
  where
    !n = V.length v0
    step !i
      | i >= n    = Iter (Right IterDone)
      | otherwise = Iter (Right (IterYield (V.unsafeIndex v0 i) (step (i + 1))))

-- | Build an iterator over @[0..n-1]@ where each index is
-- decoded on demand by the supplied function. This is the
-- canonical shape for "we know how many batches the file
-- carries; produce them one at a time" — every per-format
-- streaming reader bridges through this.
--
-- A decode failure stops the iterator at that step (the next
-- 'iterStep' on the failing iterator returns @Left e@; later
-- indices are not visited).
iterFromIndexed :: Int -> (Int -> Either String a) -> Iter a
iterFromIndexed !n decode = go 0
  where
    go !i
      | i >= n    = Iter (Right IterDone)
      | otherwise = Iter $ case decode i of
          Left  e  -> Left e
          Right !a -> Right (IterYield a (go (i + 1)))

-- | Generic anamorphism. @iterUnfold seed step@ keeps applying
-- @step@ to the threading seed, yielding the produced element
-- and a fresh seed each step until @step@ returns 'Nothing'.
-- Decode failures from @step@ surface as 'Left' on the failing
-- iterator step.
iterUnfold :: s -> (s -> Either String (Maybe (a, s))) -> Iter a
iterUnfold s0 step = go s0
  where
    go s = Iter $ case step s of
      Left e -> Left e
      Right Nothing -> Right IterDone
      Right (Just (a, s')) -> Right (IterYield a (go s'))

-- ============================================================
-- Transformations
-- ============================================================

-- | Apply a pure function to every yielded element.
iterMap :: (a -> b) -> Iter a -> Iter b
iterMap = fmap

-- | Apply a function that may itself fail. A 'Left' from @f@
-- terminates the iterator at that point.
iterMapM :: (a -> Either String b) -> Iter a -> Iter b
iterMapM f = go
  where
    go (Iter step) = Iter $ case step of
      Left e -> Left e
      Right IterDone -> Right IterDone
      Right (IterYield a next) -> case f a of
        Left e -> Left e
        Right !b -> Right (IterYield b (go next))

-- | Drop elements where @f@ returns 'Nothing'; yield @b@ where
-- it returns @Just b@.
iterMapMaybe :: (a -> Maybe b) -> Iter a -> Iter b
iterMapMaybe f = go
  where
    go (Iter step) = Iter $ case step of
      Left e -> Left e
      Right IterDone -> Right IterDone
      Right (IterYield a next) -> case f a of
        Nothing -> iterStep (go next)
        Just !b -> Right (IterYield b (go next))

-- | Keep only elements that satisfy the predicate.
iterFilter :: (a -> Bool) -> Iter a -> Iter a
iterFilter p = iterMapMaybe (\a -> if p a then Just a else Nothing)

-- | Stop after @n@ elements. Any decode error inside the first
-- @n@ steps still surfaces; errors past @n@ do not.
iterTake :: Int -> Iter a -> Iter a
iterTake = go
  where
    go !k _ | k <= 0 = iterEmpty
    go !k (Iter step) = Iter $ case step of
      Left e -> Left e
      Right IterDone -> Right IterDone
      Right (IterYield a next) -> Right (IterYield a (go (k - 1) next))

-- | Skip the first @n@ elements. Decode errors in the skipped
-- prefix /are/ surfaced (we cannot tell which index produced
-- the error without decoding it).
iterDrop :: Int -> Iter a -> Iter a
iterDrop = go
  where
    go !k it | k <= 0 = it
    go !k (Iter step) = Iter $ case step of
      Left e -> Left e
      Right IterDone -> Right IterDone
      Right (IterYield _ next) -> iterStep (go (k - 1) next)

-- | Concatenate two iterators end-to-end.
iterAppend :: Iter a -> Iter a -> Iter a
iterAppend (Iter step) right = Iter $ case step of
  Left e -> Left e
  Right IterDone -> iterStep right
  Right (IterYield a next) -> Right (IterYield a (iterAppend next right))

-- | Flatten an iterator-of-iterators in left-to-right order.
iterConcat :: Iter (Iter a) -> Iter a
iterConcat outer0 = go outer0 iterEmpty
  where
    go outer inner = Iter $ case iterStep inner of
      Left e -> Left e
      Right (IterYield a next) -> Right (IterYield a (go outer next))
      Right IterDone -> case iterStep outer of
        Left e -> Left e
        Right IterDone -> Right IterDone
        Right (IterYield it' outer') -> iterStep (go outer' it')

-- ============================================================
-- Drains
-- ============================================================

-- | Drain an iterator into a list. The result is strict in
-- spine (errors stop the drain immediately).
iterToList :: Iter a -> Either String [a]
iterToList = go []
  where
    go acc (Iter step) = case step of
      Left e -> Left e
      Right IterDone -> Right (reverse acc)
      Right (IterYield a next) -> go (a : acc) next

-- | Drain into a 'V.Vector'.
iterToVector :: Iter a -> Either String (V.Vector a)
iterToVector = fmap V.fromList . iterToList

-- | Strict left fold.
iterFold :: (b -> a -> b) -> b -> Iter a -> Either String b
iterFold f = go
  where
    go !acc (Iter step) = case step of
      Left e -> Left e
      Right IterDone -> Right acc
      Right (IterYield a next) -> go (f acc a) next

-- | Strict left fold whose step may itself fail.
iterFoldM :: (b -> a -> Either String b) -> b -> Iter a -> Either String b
iterFoldM f = go
  where
    go !acc (Iter step) = case step of
      Left e -> Left e
      Right IterDone -> Right acc
      Right (IterYield a next) -> case f acc a of
        Left e -> Left e
        Right !acc' -> go acc' next

-- | Run an effectful action for every yielded element.
iterForM_ :: Monad m => Iter a -> (a -> m ()) -> m (Either String ())
iterForM_ it0 act = go it0
  where
    go (Iter step) = case step of
      Left e -> pure (Left e)
      Right IterDone -> pure (Right ())
      Right (IterYield a next) -> act a >> go next

-- | How many elements does the iterator carry? Drains the
-- iterator entirely.
iterLength :: Iter a -> Either String Int
iterLength = iterFold (\n _ -> n + 1) 0
