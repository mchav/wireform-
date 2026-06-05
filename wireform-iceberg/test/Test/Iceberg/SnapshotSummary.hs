{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.SnapshotSummary (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Syd

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

tests :: Spec
tests = describe "Iceberg.Update snapshot summary" $ sequence_
  [ it "statsFromManifestEntry on a data file" $ do
      let s = statsFromManifestEntry (mkEntry DataContent)
      ssAddedDataFiles s   `shouldBe` 1
      ssAddedRecords s     `shouldBe` 100
      ssAddedFilesSize s   `shouldBe` 1024
      ssAddedDeleteFiles s `shouldBe` 0

  , it "statsFromManifestEntry on an equality delete file" $ do
      let s = statsFromManifestEntry (mkEntry DeletesContent)
      ssAddedDataFiles s         `shouldBe` 0
      ssAddedDeleteFiles s       `shouldBe` 1
      ssAddedEqualityDeletes s   `shouldBe` 100
      ssAddedPositionDeletes s   `shouldBe` 0

  , it "autoSummary emits canonical keys" $ do
      let s = (statsFromManifestEntry (mkEntry DataContent))
                { ssTotalDataFiles = 5, ssTotalRecords = 500 }
          summary = autoSummary s
      Map.lookup "added-data-files" summary `shouldBe` Just "1"
      Map.lookup "added-records" summary    `shouldBe` Just "100"
      Map.lookup "added-files-size" summary `shouldBe` Just "1024"
      Map.lookup "total-data-files" summary `shouldBe` Just "5"
      Map.lookup "total-records" summary    `shouldBe` Just "500"

  , it "autoSummary omits zero-valued keys" $ do
      let summary = autoSummary emptySnapshotStats
      Map.size summary `shouldBe` 0
  ]
