{-# LANGUAGE LambdaCase #-}

{- | Predicate pushdown helpers for ORC readers.

ORC's @ColumnStatistics@ message (decoded by "ORC.Footer")
carries per-stripe min/max/sum statistics for every leaf
column. This module evaluates a 'Columnar.Predicate.Predicate'
against those statistics so a planner can drop entire stripes
(and, when 'ORC.RowIndex' is present, individual row groups)
without decoding their data streams.

Decisions are sound — only 'PSkip' when the evaluator can
prove the slice has no rows matching the predicate. Missing
stats / cross-type comparisons / non-exact matches degrade
to 'PMaybeKeep' so the caller decodes normally.
-}
module ORC.Statistics (
  -- * Re-exported predicate vocabulary
  Predicate (..),
  PColPredicate (..),
  PValue (..),
  Decision (..),

  -- * Stripe-level skipping (file footer's per-column stats)
  evalStripe,
  evalColumn,

  -- * Conversions
  statsKindToRange,

  -- * ORC textual decimal helpers
  parseDecimalText,

  -- * Row-group (within-stripe) skipping
  evalRowGroupEntry,
  decodeRowGroupStats,
) where

import Columnar.Predicate (
  Decision (..),
  PColPredicate (..),
  PValue (..),
  Predicate (..),
  combineDecisions,
  evalRange,
 )
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import ORC.Types (
  ColumnStatistics (..),
  DateStatistics (..),
  DecimalStatistics (..),
  DoubleStatistics (..),
  IntegerStatistics (..),
  StatsKind (..),
  StringStatistics (..),
  TimestampStatistics (..),
 )


{- | Decide whether a stripe (or the whole file) can be skipped
given a vector of per-column statistics + a parallel vector
of column names.

The name vector is 1-based against the schema (ORC's column
0 is the synthetic root struct); the stats vector is parallel
to the schema's leaf list. Pass the leaf names + leaf stats
the caller already extracted from the footer.
-}
evalStripe
  :: V.Vector Text
  -- ^ leaf column names
  -> V.Vector ColumnStatistics
  -- ^ leaf stats parallel to names
  -> Predicate
  -> Decision
evalStripe colNames stats = walk
  where
    walk PTrue = PMaybeKeep
    walk PFalse = PSkip
    walk (PCol name cp) = case V.findIndex (== name) colNames of
      Nothing -> PMaybeKeep
      Just i ->
        case stats V.!? i of
          Nothing -> PMaybeKeep
          Just cs -> evalColumn cs cp
    walk (PAnd a b) = combineDecisions (walk a) (walk b)
    walk (POr a b) = case (walk a, walk b) of
      (PSkip, PSkip) -> PSkip
      _ -> PMaybeKeep
    walk (PNot _) = PMaybeKeep


{- | Evaluate one column-leaf predicate against one column's
'ColumnStatistics'. Used internally by 'evalStripe' but also
handy when the caller has already located the column.
-}
evalColumn :: ColumnStatistics -> PColPredicate -> Decision
evalColumn cs PIsNull
  | csHasNull cs == Just False = PSkip
  | otherwise = PMaybeKeep
evalColumn cs PIsNotNull
  -- Skip iff every row is null. We can't be sure without an
  -- explicit "all rows are null" signal; the safe answer is
  -- always 'PMaybeKeep'.
  | csNumberOfValues cs == Just 0 && csHasNull cs == Just True = PSkip
  | otherwise = PMaybeKeep
evalColumn cs cp = case statsKindToRange =<< csKind cs of
  Just (mn, mx) -> evalRange mn mx cp
  Nothing -> PMaybeKeep


{- | Lift a sub-statistics record to a typed @[mn, mx]@ pair
if both bounds are present. Returns 'Nothing' for kinds that
don't express a comparable range (e.g. binary statistics that
only carry the byte-sum, bucket statistics for booleans).
-}
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
    Just
      ( PVInt32 (fromIntegral mn :: Int32)
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


{- | Parse ORC's textual decimal representation
@\"<unscaled>E<scale>\"@ into the @(unscaled, scale)@ pair the
spec defines. Returns 'Nothing' if the string isn't in that
shape (no @E@ separator, non-integer parts, etc.).

ORC writes decimal min/max in this form because the
protobuf-encoded statistics share a single field for every
decimal precision; the @E@ separator lets a reader recover
both magnitude and scale without consulting the schema.
-}
parseDecimalText :: Text -> Maybe (Integer, Int)
parseDecimalText t =
  case T.splitOn (T.pack "E") t of
    [unscaled, scale] -> do
      u <- readMaybeInteger unscaled
      s <- readMaybeInt scale
      Just (u, s)
    _ -> Nothing
  where
    readMaybeInteger txt = case reads (T.unpack txt) of
      [(v, "")] -> Just (v :: Integer)
      _ -> Nothing
    readMaybeInt txt = case reads (T.unpack txt) of
      [(v, "")] -> Just (v :: Int)
      _ -> Nothing


-- ============================================================
-- Row-group (within-stripe) skipping
-- ============================================================

{- | Evaluate a per-leaf-column predicate against a row-group's
decoded statistics. The vector is parallel to the column-name
vector; missing or unknown stats degrade to 'PMaybeKeep'.

One row group ≈ 10 000 rows by default in ORC. A reader that
pulls per-stripe @ROW_INDEX@ streams gets one of these vectors
per row group; calling 'evalRowGroupEntry' against each lets
the reader seek directly to surviving row groups via
'ORC.RowIndex.riePositions'.
-}
evalRowGroupEntry
  :: V.Vector Text
  -- ^ leaf column names
  -> V.Vector ColumnStatistics
  -- ^ leaf stats parallel to names
  -> Predicate
  -> Decision
evalRowGroupEntry = evalStripe


-- The math is identical to the stripe-level evaluator; the
-- distinction is operational (row-group entries are denser
-- and the writer typically populates more of them).

{- | Decode the per-row-group 'ColumnStatistics' carried inside
a single 'ORC.RowIndex.RowIndexEntry' payload. The payload
bytes are the spec's @ColumnStatistics@ protobuf (the same
shape the file footer carries); 'Right Nothing' means the
entry didn't include statistics at all.
-}
decodeRowGroupStats
  :: Maybe ColumnStatistics
  -- ^ pre-parsed by the caller (nothing-on-empty)
  -> Maybe ColumnStatistics
decodeRowGroupStats = id

-- Pass-through helper kept for symmetry with future
-- 'decodeColStats'-from-bytes wrapper; today the
-- 'ORC.RowIndex.rieStatistics' field is a length-prefixed
-- ColumnStatistics struct that callers parse via
-- 'ORC.Footer.decodeColStats' before passing in.
