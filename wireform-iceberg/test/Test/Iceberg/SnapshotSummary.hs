{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.SnapshotSummary (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.Types
import Iceberg.Update

mkEntry :: ManifestContent -> ManifestEntry
mkEntry content = ManifestEntry
  { meStatus = Added, meSnapshotId = Just 1
  , meSequenceNumber = Just 1, meFileSequenceNumber = Just 1
  , meFilePath = "f.parquet", meFileFormat = ParquetFormat
  , mePartition = V.empty, meRecordCount = 100, meFileSizeBytes = 1024
  , meDataFile = Just DataFile
      { dataFileContent = content, dataFileFilePath = "f.parquet"
      , dataFileFileFormat = ParquetFormat, dataFilePartition = V.empty
      , dataFileRecordCount = 100, dataFileFileSize = 1024
      , dataFileColumnSizes = Map.empty, dataFileValueCounts = Map.empty
      , dataFileNullValueCounts = Map.empty, dataFileNanValueCounts = Map.empty
      , dataFileLowerBounds = Map.empty, dataFileUpperBounds = Map.empty
      , dataFileKeyMetadata = Nothing, dataFileSplitOffsets = V.empty
      , dataFileEqualityIds = if content == DeletesContent then V.singleton 1 else V.empty
      , dataFileSortOrderId = Nothing, dataFileFirstRowId = Nothing
      , dataFileReferencedDataFile = Nothing, dataFileContentOffset = Nothing
      , dataFileContentSize = Nothing
      }
  }

tests :: TestTree
tests = testGroup "Iceberg.Update snapshot summary"
  [ testCase "statsFromManifestEntry on a data file" $ do
      let s = statsFromManifestEntry (mkEntry DataContent)
      ssAddedDataFiles s   @?= 1
      ssAddedRecords s     @?= 100
      ssAddedFilesSize s   @?= 1024
      ssAddedDeleteFiles s @?= 0

  , testCase "statsFromManifestEntry on an equality delete file" $ do
      let s = statsFromManifestEntry (mkEntry DeletesContent)
      ssAddedDataFiles s         @?= 0
      ssAddedDeleteFiles s       @?= 1
      ssAddedEqualityDeletes s   @?= 100
      ssAddedPositionDeletes s   @?= 0

  , testCase "autoSummary emits canonical keys" $ do
      let s = (statsFromManifestEntry (mkEntry DataContent))
                { ssTotalDataFiles = 5, ssTotalRecords = 500 }
          summary = autoSummary s
      Map.lookup "added-data-files" summary @?= Just "1"
      Map.lookup "added-records" summary    @?= Just "100"
      Map.lookup "added-files-size" summary @?= Just "1024"
      Map.lookup "total-data-files" summary @?= Just "5"
      Map.lookup "total-records" summary    @?= Just "500"

  , testCase "autoSummary omits zero-valued keys" $ do
      let summary = autoSummary emptySnapshotStats
      Map.size summary @?= 0
  ]
