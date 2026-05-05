{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Tests for 'Parquet.Aggregate' — pure stats arithmetic over
-- synthetic 'FileMetadata' (no actual parquet files needed).
--
-- The interesting cases are:
--
--   * @count(*)@ across multiple row groups.
--   * @count(col)@ when every row group reports null counts.
--   * @count(col)@ poisoned to 'Nothing' when one row group
--     omits the null count.
--   * @columnMin@ / @columnMax@ folding over per-row-group
--     min / max statistics, including the legacy 'statMin' /
--     'statMax' slots and the modern 'statMinValue' /
--     'statMaxValue' slots.
--   * @columnMin@ / @columnMax@ poisoned to 'Nothing' when one
--     row group omits the stat.
module Aggregate (run) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString, int32LE, int64LE)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import Data.Word ()

import qualified Columnar.Predicate as Pred
import qualified Parquet.Aggregate as Agg
import Parquet.Types

-- | Public entry: prints OK / FAIL lines like the rest of the
-- wireform-parquet test suite.
run :: IO ()
run = do
  test "fileRowCount over 3 row groups"
    (Agg.fileRowCount (mkFm rowGroups123) == 600)
  test "rowGroupRowCount returns the field"
    (Agg.rowGroupRowCount (head rowGroups123) == 100)

  test "columnNonNullCount sums (rg.numRows - null_count)"
    (Agg.columnNonNullCount (mkFm rowGroups123) 0 == Just (600 - (1 + 2 + 3)))
  test "columnNonNullCount poisons to Nothing when one rg lacks null_count"
    (Agg.columnNonNullCount (mkFm rowGroupsMissingNull) 0 == Nothing)

  test "columnMin folds modern statMinValue across rgs"
    (Agg.columnMin (mkFm rowGroupsMinMaxModern) 0
       == Just (Pred.PVInt32 1))
  test "columnMax folds modern statMaxValue across rgs"
    (Agg.columnMax (mkFm rowGroupsMinMaxModern) 0
       == Just (Pred.PVInt32 99))

  test "columnMin falls back to legacy statMin when statMinValue absent"
    (Agg.columnMin (mkFm rowGroupsMinMaxLegacy) 0
       == Just (Pred.PVInt64 (-7)))
  test "columnMax falls back to legacy statMax when statMaxValue absent"
    (Agg.columnMax (mkFm rowGroupsMinMaxLegacy) 0
       == Just (Pred.PVInt64 12345))

  test "columnMin poisons to Nothing when one rg has no min"
    (Agg.columnMin (mkFm rowGroupsMissingMin) 0 == Nothing)
  test "columnMax poisons to Nothing when one rg has no max"
    (Agg.columnMax (mkFm rowGroupsMissingMax) 0 == Nothing)

  -- columnSum was removed from Parquet.Aggregate (Parquet's
  -- Statistics message doesn't carry per-row-group sums; ORC
  -- does, see ORC.Aggregate.columnSum).
  where
    test :: String -> Bool -> IO ()
    test name True  = putStrLn ("OK: " ++ name)
    test name False = error  ("FAIL: " ++ name)

-- ============================================================
-- Synthetic FileMetadata builders
-- ============================================================

mkFm :: [RowGroup] -> FileMetadata
mkFm rgs = FileMetadata
  { fmVersion = 2
  , fmSchema = V.empty
  , fmNumRows = sum (map rgNumRows rgs)
  , fmRowGroups = V.fromList rgs
  , fmCreatedBy = Nothing
  , fmColumnOrders = Nothing
  }

mkRg :: Int64 -> [(Maybe Int64, Maybe Statistics)] -> RowGroup
mkRg n cols = RowGroup
  { rgColumns = V.fromList (map mkCc cols)
  , rgTotalByteSize = 0
  , rgNumRows = n
  , rgSortingColumns = Nothing
  }

mkCc :: (Maybe Int64, Maybe Statistics) -> ColumnChunk
mkCc (mNumValues, mStats) = ColumnChunk
  { ccFilePath = Nothing
  , ccFileOffset = 0
  , ccMetadata = Just ColumnMetadata
      { cmType = PTInt32
      , cmEncodings = V.empty
      , cmPathInSchema = V.singleton "x"
      , cmCodec = Uncompressed
      , cmNumValues = maybe 0 id mNumValues
      , cmTotalUncompressedSize = 0
      , cmTotalCompressedSize = 0
      , cmDataPageOffset = 0
      , cmStatistics = mStats
      , cmBloomFilterOffset = Nothing
      , cmBloomFilterLength = Nothing
      }
  , ccOffsetIndexOffset = Nothing
  , ccOffsetIndexLength = Nothing
  , ccColumnIndexOffset = Nothing
  , ccColumnIndexLength = Nothing
  }

emptyStats :: Statistics
emptyStats = Statistics
  { statMin = Nothing
  , statMax = Nothing
  , statNullCount = Nothing
  , statDistinctCount = Nothing
  , statMinValue = Nothing
  , statMaxValue = Nothing
  }

statsWithNullCount :: Int64 -> Statistics
statsWithNullCount n = emptyStats { statNullCount = Just n }

statsInt32MinMax :: Int32 -> Int32 -> Statistics
statsInt32MinMax lo hi = emptyStats
  { statMinValue = Just (le32 lo)
  , statMaxValue = Just (le32 hi)
  }

statsInt64LegacyMinMax :: Int64 -> Int64 -> Statistics
statsInt64LegacyMinMax lo hi = emptyStats
  { statMin = Just (le64 lo)
  , statMax = Just (le64 hi)
  }

le32 :: Int32 -> BS.ByteString
le32 = BL.toStrict . toLazyByteString . int32LE

le64 :: Int64 -> BS.ByteString
le64 = BL.toStrict . toLazyByteString . int64LE

-- 3 row groups of 100/200/300 rows, 1/2/3 nulls in column 0.
rowGroups123 :: [RowGroup]
rowGroups123 =
  [ mkRg 100 [(Just 99, Just (statsWithNullCount 1))]
  , mkRg 200 [(Just 198, Just (statsWithNullCount 2))]
  , mkRg 300 [(Just 297, Just (statsWithNullCount 3))]
  ]

rowGroupsMissingNull :: [RowGroup]
rowGroupsMissingNull =
  [ mkRg 100 [(Just 100, Just emptyStats)]
  , mkRg 200 [(Just 200, Just (statsWithNullCount 0))]
  ]

rowGroupsMinMaxModern :: [RowGroup]
rowGroupsMinMaxModern =
  -- min across [3,1,42] = 1; max across [50,99,80] = 99.
  -- Column 0 must report ParquetType PTInt32 in every chunk
  -- (mkCc's default).
  [ mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 3 50)))]
  , mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 1 80)))]
  , mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 42 99)))]
  ]

rowGroupsMinMaxLegacy :: [RowGroup]
rowGroupsMinMaxLegacy =
  [ mkRgWithType 100 [(Just 100, Just (statsInt64LegacyMinMax (-7) 12345))]
  , mkRgWithType 100 [(Just 100, Just (statsInt64LegacyMinMax 0 99))]
  ]

rowGroupsMissingMin :: [RowGroup]
rowGroupsMissingMin =
  [ mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 3 50)))]
  , mkRg 100 [(Just 100, Just emptyStats)]
  ]

rowGroupsMissingMax :: [RowGroup]
rowGroupsMissingMax =
  [ mkRg 100 [(Just 100, Just (overrideType PTInt32 (statsInt32MinMax 3 50)))]
  , mkRg 100 [(Just 100, Just emptyStats { statMinValue = Just (le32 0) })]
  ]

-- Force the column type on a synthetic ColumnChunk —
-- cmType gates which 'PValue' constructor decoders pick.
overrideType :: ParquetType -> Statistics -> Statistics
overrideType _ = id  -- placeholder; type is set on the ColumnMetadata

-- Build a row group whose only column reports PTInt64 (so the
-- legacy stats decode correctly).
mkRgWithType :: Int64 -> [(Maybe Int64, Maybe Statistics)] -> RowGroup
mkRgWithType n cols = RowGroup
  { rgColumns = V.fromList (map (mkCcWithType PTInt64) cols)
  , rgTotalByteSize = 0
  , rgNumRows = n
  , rgSortingColumns = Nothing
  }

mkCcWithType :: ParquetType -> (Maybe Int64, Maybe Statistics) -> ColumnChunk
mkCcWithType ty (mNumValues, mStats) = (mkCc (mNumValues, mStats))
  { ccMetadata = Just ((maybe undefined id (ccMetadata (mkCc (mNumValues, mStats))))
      { cmType = ty })
  }
