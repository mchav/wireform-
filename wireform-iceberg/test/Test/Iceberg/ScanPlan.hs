{-# LANGUAGE OverloadedStrings #-}

{- | End-to-end smoke test for the manifest-pruning scan planner: write a
manifest list + manifest file with two data files, plan a scan with a
filter that prunes one of them, and verify the surviving FileScanTask.
-}
module Test.Iceberg.ScanPlan (tests) where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Iceberg.Expression
import Iceberg.Read
import Iceberg.SingleValue qualified as SV
import Iceberg.Types
import Iceberg.Update (AppendFiles (..), appendFiles)
import Iceberg.Write (writeManifestEntries, writeManifestList)
import Test.Syd


mkSchema :: Schema
mkSchema =
  Schema
    { schemaId = 0
    , schemaFields =
        V.singleton
          (StructField 1 "id" True TLong Nothing Nothing Nothing)
    , schemaIdentifierFieldIds = V.empty
    }


mkDF :: Int -> (Int, Int) -> DataFile
mkDF n (lo, hi) =
  DataFile
    { dataFileContent = DataContent
    , dataFileFilePath = T.pack ("s3://b/" ++ show n ++ ".parquet")
    , dataFileFileFormat = ParquetFormat
    , dataFilePartition = V.empty
    , dataFileRecordCount = 100
    , dataFileFileSize = 1024
    , dataFileColumnSizes = Map.empty
    , dataFileValueCounts = Map.singleton 1 100
    , dataFileNullValueCounts = Map.singleton 1 0
    , dataFileNanValueCounts = Map.empty
    , dataFileLowerBounds = Map.singleton 1 (SV.encodeInt64 (fromIntegral lo))
    , dataFileUpperBounds = Map.singleton 1 (SV.encodeInt64 (fromIntegral hi))
    , dataFileKeyMetadata = Nothing
    , dataFileSplitOffsets = V.empty
    , dataFileEqualityIds = V.empty
    , dataFileSortOrderId = Nothing
    , dataFileFirstRowId = Nothing
    , dataFileReferencedDataFile = Nothing
    , dataFileContentOffset = Nothing
    , dataFileContentSize = Nothing
    }


mkEntry :: Int -> (Int, Int) -> ManifestEntry
mkEntry i bounds =
  ManifestEntry
    { meStatus = Added
    , meSnapshotId = Just 1
    , meSequenceNumber = Just 1
    , meFileSequenceNumber = Just 1
    , meFilePath = dataFileFilePath df
    , meFileFormat = dataFileFileFormat df
    , mePartition = dataFilePartition df
    , meRecordCount = dataFileRecordCount df
    , meFileSizeBytes = dataFileFileSize df
    , meDataFile = Just df
    }
  where
    df = mkDF i bounds


minimal :: TableMetadata
minimal =
  TableMetadata
    { tmFormatVersion = 2
    , tmTableUuid = "uuid"
    , tmLocation = "s3://b"
    , tmLastSequenceNumber = 0
    , tmLastUpdatedMs = 0
    , tmLastColumnId = 1
    , tmCurrentSchemaId = 0
    , tmSchemas = V.singleton mkSchema
    , tmCurrentSnapshotId = Nothing
    , tmSnapshots = V.empty
    , tmPartitionSpecs = V.singleton (PartitionSpec 0 V.empty)
    , tmDefaultSpecId = 0
    , tmLastPartitionId = 0
    , tmSortOrders = V.singleton (SortOrder 0 V.empty)
    , tmDefaultSortOrderId = 0
    , tmProperties = Map.empty
    , tmSnapshotLog = V.empty
    , tmMetadataLog = V.empty
    , tmSnapshotRefs = Map.empty
    , tmStatistics = V.empty
    , tmPartitionStatistics = V.empty
    , tmNextRowId = Nothing
    , tmEncryptionKeys = Map.empty
    }


tests :: Spec
tests =
  describe "Iceberg.ScanPlan filter" $
    sequence_
      [ it "id == 50 keeps only the file whose [10,99] range covers 50" $ do
          let manifestPath = "s3://b/manifest.avro" :: Text
              mlPath = "s3://b/ml.avro" :: Text
              entries =
                V.fromList
                  [ mkEntry 1 (10, 99)
                  , mkEntry 2 (200, 300)
                  ]
              manifestBytes = writeManifestEntries entries
              mfRec =
                ManifestFile
                  { mfPath = manifestPath
                  , mfLength = fromIntegral (BS.length manifestBytes)
                  , mfPartitionSpecId = 0
                  , mfContent = DataContent
                  , mfSequenceNumber = 1
                  , mfMinSequenceNumber = 1
                  , mfAddedSnapshotId = 1
                  , mfAddedDataFilesCount = Just 2
                  , mfExistingDataFilesCount = Just 0
                  , mfDeletedDataFilesCount = Just 0
                  , mfAddedRowsCount = Just 200
                  , mfExistingRowsCount = Nothing
                  , mfDeletedRowsCount = Nothing
                  , mfPartitions = V.empty
                  , mfKeyMetadata = Nothing
                  , mfFirstRowId = Nothing
                  }
              mlBytes = writeManifestList (V.singleton mfRec)
              tm0 =
                appendFiles
                  minimal
                  AppendFiles
                    { apfNewManifestList = mlPath
                    , apfTimestampMs = 1
                    , apfSummary = Map.empty
                    , apfStats = Nothing
                    , apfSchemaId = Just 0
                    }
              fetchManifest path
                | path == manifestPath = Right manifestBytes
                | otherwise = Left ("not found: " ++ T.unpack path)
          case planScanWithFilter tm0 mlBytes fetchManifest (equal "id" (LLong 50)) of
            Left e -> expectationFailure e
            Right (tasks, _, _) -> do
              V.length tasks `shouldBe` 1
              meFilePath (fstDataFile (V.unsafeIndex tasks 0)) `shouldBe` "s3://b/1.parquet"
      ]
