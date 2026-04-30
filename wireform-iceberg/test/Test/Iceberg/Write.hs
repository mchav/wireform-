-- | Manifest write -> read round-trip.
module Test.Iceberg.Write (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.Read (readManifestEntries, readManifestList)
import Iceberg.Types
import Iceberg.Write

mkDataFile :: DataFile
mkDataFile = DataFile
  { dataFileContent = DataContent
  , dataFileFilePath = "s3://b/data.parquet"
  , dataFileFileFormat = ParquetFormat
  , dataFilePartition = V.empty
  , dataFileRecordCount = 100
  , dataFileFileSize = 4096
  , dataFileColumnSizes = Map.fromList [(1, 200), (2, 300)]
  , dataFileValueCounts = Map.fromList [(1, 100), (2, 100)]
  , dataFileNullValueCounts = Map.fromList [(1, 0), (2, 5)]
  , dataFileNanValueCounts = Map.empty
  , dataFileLowerBounds = Map.fromList [(1, "\1\0\0\0\0\0\0\0")]
  , dataFileUpperBounds = Map.fromList [(1, "\200\0\0\0\0\0\0\0")]
  , dataFileKeyMetadata = Nothing
  , dataFileSplitOffsets = V.fromList [0, 2048]
  , dataFileEqualityIds = V.empty
  , dataFileSortOrderId = Just 1
  , dataFileFirstRowId = Just 0
  , dataFileReferencedDataFile = Nothing
  , dataFileContentOffset = Nothing
  , dataFileContentSize = Nothing
  }

mkEntry :: ManifestEntry
mkEntry = ManifestEntry
  { meStatus = Added
  , meSnapshotId = Just 99
  , meSequenceNumber = Just 1
  , meFileSequenceNumber = Just 1
  , meFilePath = dataFileFilePath mkDataFile
  , meFileFormat = dataFileFileFormat mkDataFile
  , mePartition = dataFilePartition mkDataFile
  , meRecordCount = dataFileRecordCount mkDataFile
  , meFileSizeBytes = dataFileFileSize mkDataFile
  , meDataFile = Just mkDataFile
  }

tests :: TestTree
tests = testGroup "Iceberg.Write"
  [ testCase "writeManifestEntries -> readManifestEntries round-trip" $ do
      let bs = writeManifestEntries (V.singleton mkEntry)
      case readManifestEntries bs of
        Left e -> assertFailure e
        Right (_, vec) -> do
          V.length vec @?= 1
          let me = V.unsafeIndex vec 0
          meStatus me @?= Added
          meSnapshotId me @?= Just 99
          meFilePath me @?= "s3://b/data.parquet"
          meFileFormat me @?= ParquetFormat
          meRecordCount me @?= 100
          case meDataFile me of
            Just df -> do
              dataFileColumnSizes df @?= dataFileColumnSizes mkDataFile
              dataFileValueCounts df @?= dataFileValueCounts mkDataFile
              dataFileSplitOffsets df @?= dataFileSplitOffsets mkDataFile
              dataFileSortOrderId df @?= dataFileSortOrderId mkDataFile
            Nothing -> assertFailure "expected DataFile to be populated"

  , testCase "writeManifestList -> readManifestList round-trip with partitions" $ do
      let mf = ManifestFile
            { mfPath = "s3://b/manifest-1.avro"
            , mfLength = 10000
            , mfPartitionSpecId = 0
            , mfContent = DataContent
            , mfSequenceNumber = 5
            , mfMinSequenceNumber = 4
            , mfAddedSnapshotId = 99
            , mfAddedDataFilesCount = Just 1
            , mfExistingDataFilesCount = Just 0
            , mfDeletedDataFilesCount = Just 0
            , mfAddedRowsCount = Just 100
            , mfExistingRowsCount = Nothing
            , mfDeletedRowsCount = Nothing
            , mfPartitions = V.fromList
                [ FieldSummary { fsContainsNull = False, fsContainsNan = Just False
                               , fsLowerBound = Just "\1\0\0\0\0\0\0\0"
                               , fsUpperBound = Just "\200\0\0\0\0\0\0\0"
                               }
                ]
            , mfKeyMetadata = Nothing
            , mfFirstRowId = Nothing
            }
          bs = writeManifestList (V.singleton mf)
      case readManifestList bs of
        Left e -> assertFailure e
        Right (_, vec) -> do
          V.length vec @?= 1
          let m = V.unsafeIndex vec 0
          mfPath m @?= "s3://b/manifest-1.avro"
          mfLength m @?= 10000
          mfAddedDataFilesCount m @?= Just 1
          mfPartitions m @?= mfPartitions mf
  ]
