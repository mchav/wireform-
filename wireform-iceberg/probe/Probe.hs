{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | wireform-iceberg -> pyiceberg interop probe.
--
-- Writes a small catalogue of Iceberg metadata files into
-- @argv[1]@:
--
--   * one manifest file (Avro container holding manifest entries
--     for two data files, with full v2 statistics)
--   * one manifest list (Avro container of manifest_file
--     records pointing at the manifest above)
--   * one table-metadata JSON file
--
-- The companion 'scripts/iceberg_interop.py' driver reads each
-- file with pyiceberg's avro container reader (manifest +
-- manifest list) and pyiceberg.table.metadata.TableMetadata
-- (the JSON), and asserts the structural fields match what we
-- wrote.
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import qualified Iceberg.JSON as IJ
import qualified Iceberg.Types as IT
import qualified Iceberg.Write as IW

main :: IO ()
main = do
  args <- getArgs
  case args of
    [outDir] -> do
      writeManifestProbe outDir
      writeManifestListProbe outDir
      writeTableMetaProbe outDir
      putStrLn $ "wrote iceberg probe outputs to " ++ outDir
    _ -> do
      putStrLn "usage: wireform-iceberg-interop-probe <output-dir>"
      exitFailure

-- ============================================================
-- Manifest file
-- ============================================================

writeManifestProbe :: FilePath -> IO ()
writeManifestProbe outDir = do
  let !entries = V.fromList
        [ mkEntry IT.Added "data/file_a.parquet" 100 4096
        , mkEntry IT.Added "data/file_b.parquet" 200 8192
        ]
      !bs = IW.writeManifestEntries entries
  BS.writeFile (outDir </> "manifest_v2.avro") bs

mkEntry :: IT.ManifestStatus -> Text -> Int64 -> Int64 -> IT.ManifestEntry
mkEntry st path nrows nbytes = IT.ManifestEntry
  { IT.meStatus             = st
  , IT.meSnapshotId         = Just 1234567890
  , IT.meSequenceNumber     = Just 1
  , IT.meFileSequenceNumber = Just 1
  , IT.meFilePath           = path
  , IT.meFileFormat         = IT.ParquetFormat
  , IT.mePartition          = V.empty
  , IT.meRecordCount        = nrows
  , IT.meFileSizeBytes      = nbytes
  , IT.meDataFile           = Just $ mkDataFile path nrows nbytes
  }

mkDataFile :: Text -> Int64 -> Int64 -> IT.DataFile
mkDataFile path nrows nbytes = IT.DataFile
  { IT.dataFileContent          = IT.DataContent
  , IT.dataFileFilePath         = path
  , IT.dataFileFileFormat       = IT.ParquetFormat
  , IT.dataFilePartition        = V.empty
  , IT.dataFileRecordCount      = nrows
  , IT.dataFileFileSize         = nbytes
  , IT.dataFileColumnSizes      = Map.fromList [(1, 1024), (2, 2048)]
  , IT.dataFileValueCounts      = Map.fromList [(1, nrows), (2, nrows)]
  , IT.dataFileNullValueCounts  = Map.fromList [(1, 0), (2, 0)]
  , IT.dataFileNanValueCounts   = Map.empty
  , IT.dataFileLowerBounds      =
      Map.fromList [(1, le32 (1 :: Int32)), (2, BS.pack [0x61])]
  , IT.dataFileUpperBounds      =
      Map.fromList [(1, le32 (fromIntegral nrows :: Int32)), (2, BS.pack [0x7A])]
  , IT.dataFileKeyMetadata      = Nothing
  , IT.dataFileSplitOffsets     = V.empty
  , IT.dataFileEqualityIds      = V.empty
  , IT.dataFileSortOrderId      = Nothing
  , IT.dataFileFirstRowId       = Nothing
  , IT.dataFileReferencedDataFile = Nothing
  , IT.dataFileContentOffset    = Nothing
  , IT.dataFileContentSize      = Nothing
  }

-- | Encode an Int32 as little-endian 4 bytes (matches Iceberg
-- single-value serialisation for ints).
le32 :: Int32 -> BS.ByteString
le32 n = BL.toStrict (BB.toLazyByteString (BB.int32LE n))

-- ============================================================
-- Manifest list
-- ============================================================

writeManifestListProbe :: FilePath -> IO ()
writeManifestListProbe outDir = do
  let !files = V.singleton $ IT.ManifestFile
        { IT.mfPath                   = "metadata/manifest_v2.avro"
        , IT.mfLength                 = 12345
        , IT.mfPartitionSpecId        = 0
        , IT.mfContent                = IT.DataContent
        , IT.mfSequenceNumber         = 1
        , IT.mfMinSequenceNumber      = 1
        , IT.mfAddedSnapshotId        = 1234567890
        , IT.mfAddedDataFilesCount    = Just 2
        , IT.mfExistingDataFilesCount = Just 0
        , IT.mfDeletedDataFilesCount  = Just 0
        , IT.mfAddedRowsCount         = Just 300
        , IT.mfExistingRowsCount      = Just 0
        , IT.mfDeletedRowsCount       = Just 0
        , IT.mfPartitions             = V.empty
        , IT.mfKeyMetadata            = Nothing
        , IT.mfFirstRowId             = Nothing
        }
      !bs = IW.writeManifestList files
  BS.writeFile (outDir </> "manifest_list_v2.avro") bs

-- ============================================================
-- Table metadata JSON
-- ============================================================

writeTableMetaProbe :: FilePath -> IO ()
writeTableMetaProbe outDir = do
  let !meta = IT.TableMetadata
        { IT.tmFormatVersion        = 2
        , IT.tmTableUuid            = "550e8400-e29b-41d4-a716-446655440000"
        , IT.tmLocation             = "s3://example/tbl"
        , IT.tmLastSequenceNumber   = 1
        , IT.tmLastUpdatedMs        = 1700000000000
        , IT.tmLastColumnId         = 2
        , IT.tmSchemas              = V.singleton tableSchema
        , IT.tmCurrentSchemaId      = 0
        , IT.tmPartitionSpecs       = V.singleton emptyPartitionSpec
        , IT.tmDefaultSpecId        = 0
        , IT.tmLastPartitionId      = 999
        , IT.tmProperties           = Map.empty
        , IT.tmCurrentSnapshotId    = Just 1234567890
        , IT.tmSnapshotRefs         = Map.empty
        , IT.tmSnapshots            = V.singleton mkSnapshot
        , IT.tmSnapshotLog          = V.singleton (IT.SnapshotLogEntry 1700000000000 1234567890)
        , IT.tmMetadataLog          = V.empty
        , IT.tmSortOrders           = V.singleton emptySortOrder
        , IT.tmDefaultSortOrderId   = 0
        , IT.tmStatistics           = V.empty
        , IT.tmPartitionStatistics  = V.empty
        , IT.tmNextRowId            = Nothing
        , IT.tmEncryptionKeys       = Map.empty
        }
      !bs = IW.encodeTableMetadata meta
  BS.writeFile (outDir </> "table_metadata_v2.json") bs

tableSchema :: IT.Schema
tableSchema = IT.Schema
  { IT.schemaId               = 0
  , IT.schemaIdentifierFieldIds = V.empty
  , IT.schemaFields           = V.fromList
      [ IT.StructField 1 "id"   True  IT.TInt    Nothing Nothing Nothing
      , IT.StructField 2 "name" False IT.TString Nothing Nothing Nothing
      ]
  }

emptyPartitionSpec :: IT.PartitionSpec
emptyPartitionSpec = IT.PartitionSpec
  { IT.psSpecId = 0
  , IT.psFields = V.empty
  }

emptySortOrder :: IT.SortOrder
emptySortOrder = IT.SortOrder
  { IT.soOrderId = 0
  , IT.soFields  = V.empty
  }

mkSnapshot :: IT.Snapshot
mkSnapshot = IT.Snapshot
  { IT.snapId             = 1234567890
  , IT.snapParentId       = Nothing
  , IT.snapSequenceNumber = 1
  , IT.snapTimestampMs    = 1700000000000
  , IT.snapManifestList   = "metadata/manifest_list_v2.avro"
  , IT.snapSummary        = Map.fromList
      [ ("operation", "append")
      , ("added-data-files", "2")
      , ("added-records", "300")
      ]
  , IT.snapSchemaId       = Just 0
  , IT.snapFirstRowId     = Nothing
  , IT.snapKeyId          = Nothing
  }

-- Suppress warning on Iceberg.JSON import (only re-exported
-- via Iceberg.Write.encodeTableMetadata under the hood).
_unused :: Maybe ()
_unused = const Nothing IJ.metadataToJSON
