{-# LANGUAGE LambdaCase #-}
-- | Aggregate pushdown for Parquet.
--
-- Many analytical queries reduce to one of:
--
--   * @count(*)@ — number of rows in the whole file.
--   * @count(col)@ — number of non-null rows in a column.
--   * @sum/min/max(col)@ — folded value over a column.
--
-- All four can be answered from the file footer's row-group
-- metadata + per-column 'Statistics' /without/ decoding any
-- column data. This module exposes the helpers.
--
-- The functions here trust the writer's statistics (writers
-- can lie, but every modern producer populates these
-- correctly). Callers that don't trust the writer should fall
-- through to a full scan.
module Parquet.Aggregate
  ( -- * count(*)
    fileRowCount
  , rowGroupRowCount
    -- * count(col)
  , columnNonNullCount
    -- * min / max
  , columnMin
  , columnMax
    -- * sum (best-effort: only reported when stats include the sum)
  , columnSum
  ) where

import Data.Int (Int64)
import qualified Data.Vector as V

import qualified Columnar.Predicate as Pred

import qualified Parquet.Predicate as PPred
import qualified Parquet.Types as P

-- | Total row count across every row group in a file. @O(rg)@
-- — pure stats arithmetic, no column data decoded.
fileRowCount :: P.FileMetadata -> Int64
fileRowCount fm = V.sum (V.map P.rgNumRows (P.fmRowGroups fm))

-- | Row count of one row group.
rowGroupRowCount :: P.RowGroup -> Int64
rowGroupRowCount = P.rgNumRows

-- | @count(col)@ — the number of /non-null/ rows in the named
-- column across the whole file. Returns 'Nothing' if any
-- contributing row group lacks a populated 'Statistics.statNullCount'
-- (in which case the value can't be safely computed without
-- decoding the column).
columnNonNullCount :: V.Vector Pred.PValue -> P.FileMetadata -> Int -> Maybe Int64
columnNonNullCount _ fm colIdx =
  V.foldl' step (Just 0) (P.fmRowGroups fm)
  where
    step Nothing _ = Nothing
    step (Just acc) rg = case P.rgColumns rg V.!? colIdx of
      Nothing -> Nothing
      Just cc -> case P.ccMetadata cc >>= P.cmStatistics of
        Just s | Just nulls <- P.statNullCount s ->
          Just (acc + (P.rgNumRows rg - nulls))
        _ -> Nothing

-- | Minimum value of a column across the whole file. Returns
-- 'Nothing' if any row group lacks min statistics for the
-- column.
columnMin :: P.FileMetadata -> Int -> Maybe Pred.PValue
columnMin = aggCmp pickMin
  where
    pickMin _ acc v = case Pred.pvLess v acc of
      True  -> Just v
      False -> Just acc

-- | Maximum value of a column across the whole file.
columnMax :: P.FileMetadata -> Int -> Maybe Pred.PValue
columnMax = aggCmp pickMax
  where
    pickMax _ acc v = case Pred.pvLess acc v of
      True  -> Just v
      False -> Just acc

aggCmp
  :: (P.ParquetType -> Pred.PValue -> Pred.PValue -> Maybe Pred.PValue)
  -> P.FileMetadata
  -> Int
  -> Maybe Pred.PValue
aggCmp pick fm colIdx = V.foldl' step Nothing (P.fmRowGroups fm)
  where
    step acc rg = case P.rgColumns rg V.!? colIdx of
      Nothing -> acc
      Just cc -> case P.ccMetadata cc of
        Nothing -> acc
        Just md ->
          let stats = P.cmStatistics md
              ty    = P.cmType md
              minV  = stats >>= takeMin ty
          in case (acc, minV) of
               (Nothing, Just v) -> Just v
               (Just a,  Just v) -> pick ty a v
               _                  -> acc

    takeMin ty s =
      let !raw = case P.statMinValue s of
                   Just b  -> Just b
                   Nothing -> P.statMin s
      in raw >>= PPred.decodePValueLE ty

-- | @sum(col)@ — Parquet's standard 'Statistics' message
-- doesn't carry sums (only ORC does), so this is currently
-- always 'Nothing'. Provided for API symmetry; callers that
-- need real sums should fall through to a column scan.
columnSum :: P.FileMetadata -> Int -> Maybe Pred.PValue
columnSum _ _ = Nothing
