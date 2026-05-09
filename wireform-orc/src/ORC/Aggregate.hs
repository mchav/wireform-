{-# LANGUAGE LambdaCase #-}
-- | Aggregate pushdown for ORC.
--
-- Same shape as "Parquet.Aggregate" — count, min, max all
-- read from per-stripe 'ColumnStatistics' without decoding
-- column data — but ORC additionally carries
-- 'IntegerStatistics.sum' / 'DoubleStatistics.sum', so this
-- module exposes a real 'columnSum'.
--
-- All functions trust the writer's statistics. Callers that
-- can't (e.g. files from older ORC writers known to lie about
-- counts) should fall through to a column scan.
module ORC.Aggregate
  ( -- * count(*)
    fileRowCount
  , stripeRowCount
    -- * count(col)
  , columnNonNullCount
    -- * min / max
  , columnMin
  , columnMax
    -- * sum
  , columnSum
  ) where

import qualified Data.Vector as V
import Data.Word (Word64)

import qualified Columnar.Predicate as Pred
import ORC.Types

-- | Total row count across every stripe in a file.
fileRowCount :: ORCFooter -> Word64
fileRowCount = orcNumberOfRows

-- | Row count of one stripe.
stripeRowCount :: StripeInformation -> Word64
stripeRowCount = siNumberOfRows

-- | @count(col)@ — number of non-null rows in a column,
-- summed from the file footer's per-leaf-column
-- @csNumberOfValues@ (which by ORC convention excludes nulls).
-- Returns 'Nothing' if the column index is out of range or the
-- footer didn't populate the count.
columnNonNullCount :: ORCFooter -> Int -> Maybe Word64
columnNonNullCount footer colIdx = do
  cs <- orcStatistics footer V.!? colIdx
  csNumberOfValues cs

-- | Minimum value of a column from file-level statistics.
columnMin :: ORCFooter -> Int -> Maybe Pred.PValue
columnMin = readStatScalar minOfKind
  where
    minOfKind = \case
      SkInt    s -> Pred.PVInt64  <$> isMinimum s
      SkDouble s -> Pred.PVDouble <$> dsMinimum s
      SkString s -> Pred.PVText   <$> ssMinimum s
      SkDate   s -> Pred.PVInt64 <$> dateMinimum s
      _          -> Nothing

-- | Maximum value of a column from file-level statistics.
columnMax :: ORCFooter -> Int -> Maybe Pred.PValue
columnMax = readStatScalar maxOfKind
  where
    maxOfKind = \case
      SkInt    s -> Pred.PVInt64  <$> isMaximum s
      SkDouble s -> Pred.PVDouble <$> dsMaximum s
      SkString s -> Pred.PVText   <$> ssMaximum s
      SkDate   s -> Pred.PVInt64 <$> dateMaximum s
      _          -> Nothing

-- | Sum of a column from file-level statistics. Reports
-- 'PVInt64' for integer columns and 'PVDouble' for float
-- columns; other type kinds (string, date, etc.) report
-- 'Nothing' even though their @sum@ field carries something
-- (it's the byte-length total for strings, not a numeric sum).
columnSum :: ORCFooter -> Int -> Maybe Pred.PValue
columnSum = readStatScalar sumOfKind
  where
    sumOfKind = \case
      SkInt    s -> Pred.PVInt64  <$> isSum s
      SkDouble s -> Pred.PVDouble <$> dsSum s
      _          -> Nothing

-- | Common driver for the min / max / sum readers.
readStatScalar
  :: (StatsKind -> Maybe Pred.PValue)
  -> ORCFooter -> Int -> Maybe Pred.PValue
readStatScalar f footer colIdx = do
  cs   <- orcStatistics footer V.!? colIdx
  kind <- csKind cs
  f kind

