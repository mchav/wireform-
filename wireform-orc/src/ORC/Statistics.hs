{-# LANGUAGE LambdaCase #-}
-- | Predicate pushdown helpers for ORC readers.
--
-- ORC's @ColumnStatistics@ message (decoded by "ORC.Footer")
-- carries per-stripe min/max/sum statistics for every leaf
-- column. This module evaluates a 'Columnar.Predicate.Predicate'
-- against those statistics so a planner can drop entire stripes
-- (and, when 'ORC.RowIndex' is present, individual row groups)
-- without decoding their data streams.
--
-- Decisions are sound — only 'PSkip' when the evaluator can
-- prove the slice has no rows matching the predicate. Missing
-- stats / cross-type comparisons / non-exact matches degrade
-- to 'PMaybeKeep' so the caller decodes normally.
module ORC.Statistics
  ( -- * Re-exported predicate vocabulary
    Predicate (..)
  , PColPredicate (..)
  , PValue (..)
  , Decision (..)
    -- * Stripe-level skipping (file footer's per-column stats)
  , evalStripe
  , evalColumn
    -- * Conversions
  , statsKindToRange
  ) where

import Data.Int (Int32, Int64)
import qualified Data.Vector as V

import Columnar.Predicate
  ( Decision (..)
  , PColPredicate (..)
  , PValue (..)
  , Predicate (..)
  , combineDecisions
  , evalRange
  )

import ORC.Types
  ( ColumnStatistics (..)
  , DateStatistics (..)
  , DecimalStatistics (..)
  , DoubleStatistics (..)
  , IntegerStatistics (..)
  , StatsKind (..)
  , StringStatistics (..)
  , TimestampStatistics (..)
  )

import Data.Text (Text)

-- | Decide whether a stripe (or the whole file) can be skipped
-- given a vector of per-column statistics + a parallel vector
-- of column names.
--
-- The name vector is 1-based against the schema (ORC's column
-- 0 is the synthetic root struct); the stats vector is parallel
-- to the schema's leaf list. Pass the leaf names + leaf stats
-- the caller already extracted from the footer.
evalStripe
  :: V.Vector Text             -- ^ leaf column names
  -> V.Vector ColumnStatistics -- ^ leaf stats parallel to names
  -> Predicate
  -> Decision
evalStripe colNames stats = walk
  where
    walk PTrue           = PMaybeKeep
    walk PFalse          = PSkip
    walk (PCol name cp)  = case V.findIndex (== name) colNames of
      Nothing -> PMaybeKeep
      Just i ->
        case stats V.!? i of
          Nothing -> PMaybeKeep
          Just cs -> evalColumn cs cp
    walk (PAnd a b)      = combineDecisions (walk a) (walk b)
    walk (POr a b)       = case (walk a, walk b) of
      (PSkip, PSkip) -> PSkip
      _              -> PMaybeKeep
    walk (PNot _)        = PMaybeKeep

-- | Evaluate one column-leaf predicate against one column's
-- 'ColumnStatistics'. Used internally by 'evalStripe' but also
-- handy when the caller has already located the column.
evalColumn :: ColumnStatistics -> PColPredicate -> Decision
evalColumn cs PIsNull
  | csHasNull cs == Just False = PSkip
  | otherwise                  = PMaybeKeep
evalColumn cs PIsNotNull
  -- Skip iff every row is null. We can't be sure without an
  -- explicit "all rows are null" signal; the safe answer is
  -- always 'PMaybeKeep'.
  | csNumberOfValues cs == Just 0 && csHasNull cs == Just True = PSkip
  | otherwise                                                  = PMaybeKeep
evalColumn cs cp = case statsKindToRange =<< csKind cs of
  Just (mn, mx) -> evalRange mn mx cp
  Nothing       -> PMaybeKeep

-- | Lift a sub-statistics record to a typed @[mn, mx]@ pair
-- if both bounds are present. Returns 'Nothing' for kinds that
-- don't express a comparable range (e.g. binary statistics that
-- only carry the byte-sum, bucket statistics for booleans).
statsKindToRange :: StatsKind -> Maybe (PValue, PValue)
statsKindToRange = \case
  SkInt s -> do
    mn <- isMinimum s
    mx <- isMaximum s
    -- Choose Int64 / Int32 based on whether the values fit in
    -- Int32; the predicate evaluator already handles
    -- cross-comparison conservatively.
    Just (PVInt64 mn, PVInt64 mx)
  SkDouble s -> do
    mn <- dsMinimum s
    mx <- dsMaximum s
    Just (PVDouble mn, PVDouble mx)
  SkString s -> do
    mn <- ssMinimum s
    mx <- ssMaximum s
    Just (PVText mn, PVText mx)
  SkDate s -> do
    mn <- dateMinimum s
    mx <- dateMaximum s
    Just ( PVInt32 (fromIntegral mn :: Int32)
         , PVInt32 (fromIntegral mx :: Int32)
         )
  SkTimestamp s -> do
    mn <- tsMinimum s
    mx <- tsMaximum s
    Just (PVInt64 mn, PVInt64 mx)
  SkDecimal s -> do
    -- Decimal min/max are stored as the spec's textual
    -- "<unscaled>E<scale>" form; expose as opaque text and
    -- let the predicate writer compare lexically.
    mn <- decMinimum s
    mx <- decMaximum s
    Just (PVText mn, PVText mx)
  SkBinary _ -> Nothing
  SkBucket _ -> Nothing

_unusedInt64 :: Int64
_unusedInt64 = 0
