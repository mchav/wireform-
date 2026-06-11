{-# LANGUAGE LambdaCase #-}

{- | Aggregate pushdown for Parquet.

Many analytical queries reduce to one of:

  * @count(*)@ — number of rows in the whole file.
  * @count(col)@ — number of non-null rows in a column.
  * @sum/min/max(col)@ — folded value over a column.

All four can be answered from the file footer's row-group
metadata + per-column 'Statistics' /without/ decoding any
column data. This module exposes the helpers.

The functions here trust the writer's statistics (writers
can lie, but every modern producer populates these
correctly). Callers that don't trust the writer should fall
through to a full scan.
-}
module Parquet.Aggregate (
  -- * count(*)
  fileRowCount,
  rowGroupRowCount,

  -- * count(col)
  columnNonNullCount,

  -- * min / max
  columnMin,
  columnMax,

  -- * Note on @sum(col)@
  -- $sum
) where

-- \$sum
--
-- Parquet's @Statistics@ message intentionally doesn't carry
-- per-row-group sums (the spec authors decided sums are too
-- easy to overflow + lose precision to be safely cached). To
-- compute @sum(col)@ from a Parquet file you have to scan the
-- column values; there is no pushdown helper.
--
-- ORC /does/ carry @IntegerStatistics.sum@ — see
-- "ORC.Aggregate.columnSum" for the parallel helper that
-- works with ORC files.

import Columnar.Predicate qualified as Pred
import Data.Int (Int64)
import Data.Vector qualified as V
import Parquet.Predicate qualified as PPred
import Parquet.Types qualified as P


{- | Total row count across every row group in a file. @O(rg)@
— pure stats arithmetic, no column data decoded.
-}
fileRowCount :: P.FileMetadata -> Int64
fileRowCount fm = V.sum (V.map P.rgNumRows (P.fmRowGroups fm))


-- | Row count of one row group.
rowGroupRowCount :: P.RowGroup -> Int64
rowGroupRowCount = P.rgNumRows


{- | @count(col)@ — the number of /non-null/ rows in the named
column across the whole file. Returns 'Nothing' if any
contributing row group lacks a populated 'Statistics.statNullCount'
(in which case the value can't be safely computed without
decoding the column).
-}
columnNonNullCount :: P.FileMetadata -> Int -> Maybe Int64
columnNonNullCount fm colIdx =
  V.foldl' step (Just 0) (P.fmRowGroups fm)
  where
    step Nothing _ = Nothing
    step (Just acc) rg = case P.rgColumns rg V.!? colIdx of
      Nothing -> Nothing
      Just cc -> case P.ccMetadata cc >>= P.cmStatistics of
        Just s
          | Just nulls <- P.statNullCount s ->
              Just (acc + (P.rgNumRows rg - nulls))
        _ -> Nothing


{- | Minimum value of a column across the whole file. Reads
each row group's @min_value@ (or legacy @min@) statistic
and folds them under the 'PValue' ordering. Returns
'Nothing' if /any/ contributing row group lacks min stats —
otherwise the result might be the min over a strict subset
of the row groups.
-}
columnMin :: P.FileMetadata -> Int -> Maybe Pred.PValue
columnMin = aggCmp takeMinStat (\acc v -> if Pred.pvLess v acc then v else acc)


{- | Maximum value of a column across the whole file. Same
soundness rule as 'columnMin': any row group without max
stats poisons the result to 'Nothing'.
-}
columnMax :: P.FileMetadata -> Int -> Maybe Pred.PValue
columnMax = aggCmp takeMaxStat (\acc v -> if Pred.pvLess acc v then v else acc)


{- | Common driver for column-wise stat aggregation. Polls the
supplied stat extractor on every row group; if any returns
'Nothing' the whole result is 'Nothing' (we can't be sound
about a min / max otherwise).
-}
aggCmp
  :: (P.ParquetType -> P.Statistics -> Maybe Pred.PValue)
  -> (Pred.PValue -> Pred.PValue -> Pred.PValue)
  -> P.FileMetadata
  -> Int
  -> Maybe Pred.PValue
aggCmp takeStat combine fm colIdx =
  V.foldl' step (Just Nothing) (P.fmRowGroups fm) >>= id
  where
    -- Outer Maybe: did any row group fail to produce a stat?
    -- Inner Maybe: have we accumulated a value yet?
    step Nothing _ = Nothing
    step (Just inner) rg = case P.rgColumns rg V.!? colIdx of
      Nothing -> Nothing
      Just cc -> case P.ccMetadata cc of
        Nothing -> Nothing
        Just md -> case P.cmStatistics md >>= takeStat (P.cmType md) of
          Nothing -> Nothing -- this row group has no stat -> poison
          Just v -> case inner of
            Nothing -> Just (Just v)
            Just acc -> Just (Just (combine acc v))


takeMinStat :: P.ParquetType -> P.Statistics -> Maybe Pred.PValue
takeMinStat ty s =
  let !raw = case P.statMinValue s of
        Just b -> Just b
        Nothing -> P.statMin s
  in raw >>= PPred.decodePValueLE ty


takeMaxStat :: P.ParquetType -> P.Statistics -> Maybe Pred.PValue
takeMaxStat ty s =
  let !raw = case P.statMaxValue s of
        Just b -> Just b
        Nothing -> P.statMax s
  in raw >>= PPred.decodePValueLE ty
