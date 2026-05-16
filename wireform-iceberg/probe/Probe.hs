{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | wireform-iceberg -> pyiceberg interop probe.

Writes a small catalogue of Iceberg metadata files into
@argv[1]@:

  * one manifest file (Avro container holding manifest entries
    for two data files, with full v2 statistics)
  * one manifest list (Avro container of manifest_file
    records pointing at the manifest above)
  * one table-metadata JSON file

The companion 'scripts/iceberg_interop.py' driver reads each
file with pyiceberg's avro container reader (manifest +
manifest list) and pyiceberg.table.metadata.TableMetadata
(the JSON), and asserts the structural fields match what we
wrote.
-}
module Main (main) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Iceberg.JSON qualified as IJ
import Iceberg.Types qualified as IT
import Iceberg.Write qualified as IW
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import Wireform.Builder qualified as BB


main :: IO ()
main = do
  args <- getArgs
  case args of
    [outDir] -> do
      writeManifestProbe outDir
      writeManifestListProbe outDir
      writeTableMetaProbe outDir
      writeDeleteManifestProbe outDir
      writeManifestListWithSummariesProbe outDir
      writeRichTableMetaProbe outDir
      putStrLn $ "wrote iceberg probe outputs to " ++ outDir
    _ -> do
      putStrLn "usage: wireform-iceberg-interop-probe <output-dir>"
      exitFailure


-- ============================================================
-- Manifest file
-- ============================================================

writeManifestProbe :: FilePath -> IO ()
writeManifestProbe outDir = do
  let !entries =
        V.fromList
          [ mkEntry IT.Added "data/file_a.parquet" 100 4096
          , mkEntry IT.Added "data/file_b.parquet" 200 8192
          ]
      !bs = IW.writeManifestEntries entries
  BS.writeFile (outDir </> "manifest_v2.avro") bs


mkEntry :: IT.ManifestStatus -> Text -> Int64 -> Int64 -> IT.ManifestEntry
mkEntry st path nrows nbytes =
  IT.ManifestEntry
    { IT.meStatus = st
    , IT.meSnapshotId = Just 1234567890
    , IT.meSequenceNumber = Just 1
    , IT.meFileSequenceNumber = Just 1
    , IT.meFilePath = path
    , IT.meFileFormat = IT.ParquetFormat
    , IT.mePartition = V.empty
    , IT.meRecordCount = nrows
    , IT.meFileSizeBytes = nbytes
    , IT.meDataFile = Just $ mkDataFile path nrows nbytes
    }


mkDataFile :: Text -> Int64 -> Int64 -> IT.DataFile
mkDataFile path nrows nbytes =
  IT.DataFile
    { IT.dataFileContent = IT.DataContent
    , IT.dataFileFilePath = path
    , IT.dataFileFileFormat = IT.ParquetFormat
    , IT.dataFilePartition = V.empty
    , IT.dataFileRecordCount = nrows
    , IT.dataFileFileSize = nbytes
    , IT.dataFileColumnSizes = Map.fromList [(1, 1024), (2, 2048)]
    , IT.dataFileValueCounts = Map.fromList [(1, nrows), (2, nrows)]
    , IT.dataFileNullValueCounts = Map.fromList [(1, 0), (2, 0)]
    , IT.dataFileNanValueCounts = Map.empty
    , IT.dataFileLowerBounds =
        Map.fromList [(1, le32 (1 :: Int32)), (2, BS.pack [0x61])]
    , IT.dataFileUpperBounds =
        Map.fromList [(1, le32 (fromIntegral nrows :: Int32)), (2, BS.pack [0x7A])]
    , IT.dataFileKeyMetadata = Nothing
    , IT.dataFileSplitOffsets = V.empty
    , IT.dataFileEqualityIds = V.empty
    , IT.dataFileSortOrderId = Nothing
    , IT.dataFileFirstRowId = Nothing
    , IT.dataFileReferencedDataFile = Nothing
    , IT.dataFileContentOffset = Nothing
    , IT.dataFileContentSize = Nothing
    }


{- | Encode an Int32 as little-endian 4 bytes (matches Iceberg
single-value serialisation for ints).
-}
le32 :: Int32 -> BS.ByteString
le32 n = BL.toStrict (BB.toLazyByteString (BB.int32LE n))


-- ============================================================
-- Manifest list
-- ============================================================

writeManifestListProbe :: FilePath -> IO ()
writeManifestListProbe outDir = do
  let !files =
        V.singleton $
          IT.ManifestFile
            { IT.mfPath = "metadata/manifest_v2.avro"
            , IT.mfLength = 12345
            , IT.mfPartitionSpecId = 0
            , IT.mfContent = IT.DataContent
            , IT.mfSequenceNumber = 1
            , IT.mfMinSequenceNumber = 1
            , IT.mfAddedSnapshotId = 1234567890
            , IT.mfAddedDataFilesCount = Just 2
            , IT.mfExistingDataFilesCount = Just 0
            , IT.mfDeletedDataFilesCount = Just 0
            , IT.mfAddedRowsCount = Just 300
            , IT.mfExistingRowsCount = Just 0
            , IT.mfDeletedRowsCount = Just 0
            , IT.mfPartitions = V.empty
            , IT.mfKeyMetadata = Nothing
            , IT.mfFirstRowId = Nothing
            }
      !bs = IW.writeManifestList files
  BS.writeFile (outDir </> "manifest_list_v2.avro") bs


-- ============================================================
-- Table metadata JSON
-- ============================================================

writeTableMetaProbe :: FilePath -> IO ()
writeTableMetaProbe outDir = do
  let !meta =
        IT.TableMetadata
          { IT.tmFormatVersion = 2
          , IT.tmTableUuid = "550e8400-e29b-41d4-a716-446655440000"
          , IT.tmLocation = "s3://example/tbl"
          , IT.tmLastSequenceNumber = 1
          , IT.tmLastUpdatedMs = 1700000000000
          , IT.tmLastColumnId = 2
          , IT.tmSchemas = V.singleton tableSchema
          , IT.tmCurrentSchemaId = 0
          , IT.tmPartitionSpecs = V.singleton emptyPartitionSpec
          , IT.tmDefaultSpecId = 0
          , IT.tmLastPartitionId = 999
          , IT.tmProperties = Map.empty
          , IT.tmCurrentSnapshotId = Just 1234567890
          , IT.tmSnapshotRefs = Map.empty
          , IT.tmSnapshots = V.singleton mkSnapshot
          , IT.tmSnapshotLog = V.singleton (IT.SnapshotLogEntry 1700000000000 1234567890)
          , IT.tmMetadataLog = V.empty
          , IT.tmSortOrders = V.singleton emptySortOrder
          , IT.tmDefaultSortOrderId = 0
          , IT.tmStatistics = V.empty
          , IT.tmPartitionStatistics = V.empty
          , IT.tmNextRowId = Nothing
          , IT.tmEncryptionKeys = Map.empty
          }
      !bs = IW.encodeTableMetadata meta
  BS.writeFile (outDir </> "table_metadata_v2.json") bs


tableSchema :: IT.Schema
tableSchema =
  IT.Schema
    { IT.schemaId = 0
    , IT.schemaIdentifierFieldIds = V.empty
    , IT.schemaFields =
        V.fromList
          [ IT.StructField 1 "id" True IT.TInt Nothing Nothing Nothing
          , IT.StructField 2 "name" False IT.TString Nothing Nothing Nothing
          ]
    }


emptyPartitionSpec :: IT.PartitionSpec
emptyPartitionSpec =
  IT.PartitionSpec
    { IT.psSpecId = 0
    , IT.psFields = V.empty
    }


emptySortOrder :: IT.SortOrder
emptySortOrder =
  IT.SortOrder
    { IT.soOrderId = 0
    , IT.soFields = V.empty
    }


mkSnapshot :: IT.Snapshot
mkSnapshot =
  IT.Snapshot
    { IT.snapId = 1234567890
    , IT.snapParentId = Nothing
    , IT.snapSequenceNumber = 1
    , IT.snapTimestampMs = 1700000000000
    , IT.snapManifestList = "metadata/manifest_list_v2.avro"
    , IT.snapSummary =
        Map.fromList
          [ ("operation", "append")
          , ("added-data-files", "2")
          , ("added-records", "300")
          ]
    , IT.snapSchemaId = Just 0
    , IT.snapFirstRowId = Nothing
    , IT.snapKeyId = Nothing
    }


-- ============================================================
-- Delete manifest (manifest_v2_deletes.avro)
-- ============================================================
--
-- A v2 delete-manifest: 'mfContent = DeletesContent' would be set
-- on the manifest_file pointer in the manifest list, but the
-- manifest_entry rows themselves carry @data_file.content = 1@
-- (position deletes) or @= 2@ (equality deletes). Iceberg readers
-- key off the @content@ tag.

writeDeleteManifestProbe :: FilePath -> IO ()
writeDeleteManifestProbe outDir = do
  let !entries =
        V.fromList
          [ mkPositionDeleteEntry "data/deletes/pos_001.parquet" 5 1024
          , mkEqualityDeleteEntry "data/deletes/eq_001.parquet" 3 768
          ]
      !bs = IW.writeManifestEntries entries
  BS.writeFile (outDir </> "manifest_v2_deletes.avro") bs


mkPositionDeleteEntry :: Text -> Int64 -> Int64 -> IT.ManifestEntry
mkPositionDeleteEntry path nrows nbytes =
  let !df =
        (mkDataFile path nrows nbytes)
          { IT.dataFileContent = IT.DeletesContent
          , IT.dataFileColumnSizes = Map.empty
          , IT.dataFileValueCounts = Map.empty
          , IT.dataFileNullValueCounts = Map.empty
          , IT.dataFileLowerBounds = Map.empty
          , IT.dataFileUpperBounds = Map.empty
          , IT.dataFileEqualityIds = V.empty
          }
  in IT.ManifestEntry
      { IT.meStatus = IT.Added
      , IT.meSnapshotId = Just 1234567892
      , IT.meSequenceNumber = Just 3
      , IT.meFileSequenceNumber = Just 3
      , IT.meFilePath = path
      , IT.meFileFormat = IT.ParquetFormat
      , IT.mePartition = V.empty
      , IT.meRecordCount = nrows
      , IT.meFileSizeBytes = nbytes
      , IT.meDataFile = Just df
      }


mkEqualityDeleteEntry :: Text -> Int64 -> Int64 -> IT.ManifestEntry
mkEqualityDeleteEntry path nrows nbytes =
  let !df =
        (mkDataFile path nrows nbytes)
          { IT.dataFileContent = IT.DeletesContent
          , IT.dataFileColumnSizes = Map.empty
          , IT.dataFileValueCounts = Map.empty
          , IT.dataFileNullValueCounts = Map.empty
          , IT.dataFileLowerBounds = Map.empty
          , IT.dataFileUpperBounds = Map.empty
          , -- Equality-delete files declare which field ids are
            -- compared per the Iceberg spec.
            IT.dataFileEqualityIds = V.fromList [1]
          }
  in IT.ManifestEntry
      { IT.meStatus = IT.Added
      , IT.meSnapshotId = Just 1234567892
      , IT.meSequenceNumber = Just 3
      , IT.meFileSequenceNumber = Just 3
      , IT.meFilePath = path
      , IT.meFileFormat = IT.ParquetFormat
      , IT.mePartition = V.empty
      , IT.meRecordCount = nrows
      , IT.meFileSizeBytes = nbytes
      , IT.meDataFile = Just df
      }


-- ============================================================
-- Manifest list with field summaries
-- ============================================================
--
-- Emits a manifest list pointing at the data manifest, the
-- partitioned manifest, and the delete manifest above with
-- realistic counts and per-partition field summaries derived from
-- the partition tuples.

writeManifestListWithSummariesProbe :: FilePath -> IO ()
writeManifestListWithSummariesProbe outDir = do
  let !files =
        V.fromList
          [ IT.ManifestFile
              { IT.mfPath = "metadata/manifest_v2.avro"
              , IT.mfLength = 12345
              , IT.mfPartitionSpecId = 0
              , IT.mfContent = IT.DataContent
              , IT.mfSequenceNumber = 1
              , IT.mfMinSequenceNumber = 1
              , IT.mfAddedSnapshotId = 1234567890
              , IT.mfAddedDataFilesCount = Just 2
              , IT.mfExistingDataFilesCount = Just 0
              , IT.mfDeletedDataFilesCount = Just 0
              , IT.mfAddedRowsCount = Just 300
              , IT.mfExistingRowsCount = Just 0
              , IT.mfDeletedRowsCount = Just 0
              , IT.mfPartitions = V.empty
              , IT.mfKeyMetadata = Nothing
              , IT.mfFirstRowId = Nothing
              }
          , IT.ManifestFile
              { IT.mfPath = "metadata/manifest_v2_partitioned.avro"
              , IT.mfLength = 23456
              , IT.mfPartitionSpecId = 1
              , IT.mfContent = IT.DataContent
              , IT.mfSequenceNumber = 2
              , IT.mfMinSequenceNumber = 2
              , IT.mfAddedSnapshotId = 1234567891
              , IT.mfAddedDataFilesCount = Just 3
              , IT.mfExistingDataFilesCount = Just 0
              , IT.mfDeletedDataFilesCount = Just 0
              , IT.mfAddedRowsCount = Just 310
              , IT.mfExistingRowsCount = Just 0
              , IT.mfDeletedRowsCount = Just 0
              , IT.mfPartitions =
                  V.singleton
                    IT.FieldSummary
                      { IT.fsContainsNull = False
                      , IT.fsContainsNan = Nothing
                      , IT.fsLowerBound = Just (TE.encodeUtf8 "A")
                      , IT.fsUpperBound = Just (TE.encodeUtf8 "B")
                      }
              , IT.mfKeyMetadata = Nothing
              , IT.mfFirstRowId = Nothing
              }
          , IT.ManifestFile
              { IT.mfPath = "metadata/manifest_v2_deletes.avro"
              , IT.mfLength = 4096
              , IT.mfPartitionSpecId = 0
              , IT.mfContent = IT.DeletesContent
              , IT.mfSequenceNumber = 3
              , IT.mfMinSequenceNumber = 3
              , IT.mfAddedSnapshotId = 1234567892
              , IT.mfAddedDataFilesCount = Just 2
              , IT.mfExistingDataFilesCount = Just 0
              , IT.mfDeletedDataFilesCount = Just 0
              , IT.mfAddedRowsCount = Just 8
              , IT.mfExistingRowsCount = Just 0
              , IT.mfDeletedRowsCount = Just 0
              , IT.mfPartitions = V.empty
              , IT.mfKeyMetadata = Nothing
              , IT.mfFirstRowId = Nothing
              }
          ]
      !bs = IW.writeManifestList files
  BS.writeFile (outDir </> "manifest_list_v2_full.avro") bs


-- ============================================================
-- Rich table metadata
-- ============================================================
--
-- Exercises the JSON encoder on more of the surface that
-- pyiceberg.table.metadata.TableMetadataUtil cares about:

-- * Two snapshots with parent linkage


-- * A non-trivial partition spec (truncate[1] on @name@)


-- * A non-trivial sort order (asc, nulls-first on @id@)


-- * Snapshot refs (a 'main' branch + a 'snap-v1' tag)


writeRichTableMetaProbe :: FilePath -> IO ()
writeRichTableMetaProbe outDir = do
  let !meta =
        IT.TableMetadata
          { IT.tmFormatVersion = 2
          , IT.tmTableUuid = "550e8400-e29b-41d4-a716-446655440001"
          , IT.tmLocation = "s3://example/rich"
          , IT.tmLastSequenceNumber = 2
          , IT.tmLastUpdatedMs = 1700000010000
          , IT.tmLastColumnId = 2
          , IT.tmSchemas = V.singleton tableSchema
          , IT.tmCurrentSchemaId = 0
          , IT.tmPartitionSpecs =
              V.fromList
                [ emptyPartitionSpec
                , richPartitionSpec
                ]
          , IT.tmDefaultSpecId = 1
          , IT.tmLastPartitionId = 1000
          , IT.tmProperties =
              Map.fromList
                [ ("write.format.default", "parquet")
                , ("history.expire.max-snapshot-age-ms", "604800000")
                ]
          , IT.tmCurrentSnapshotId = Just 1234567891
          , IT.tmSnapshotRefs =
              Map.fromList
                [
                  ( "main"
                  , IT.SnapshotRef
                      { IT.srSnapshotId = 1234567891
                      , IT.srType = "branch"
                      , IT.srMaxRefAgeMs = Nothing
                      , IT.srMaxSnapshotAgeMs = Just 86400000
                      , IT.srMinSnapshotsToKeep = Just 5
                      }
                  )
                ,
                  ( "snap-v1"
                  , IT.SnapshotRef
                      { IT.srSnapshotId = 1234567890
                      , IT.srType = "tag"
                      , IT.srMaxRefAgeMs = Nothing
                      , IT.srMaxSnapshotAgeMs = Nothing
                      , IT.srMinSnapshotsToKeep = Nothing
                      }
                  )
                ]
          , IT.tmSnapshots = V.fromList [mkSnapshot, mkSnapshot2]
          , IT.tmSnapshotLog =
              V.fromList
                [ IT.SnapshotLogEntry 1700000000000 1234567890
                , IT.SnapshotLogEntry 1700000010000 1234567891
                ]
          , IT.tmMetadataLog = V.empty
          , IT.tmSortOrders =
              V.fromList
                [ emptySortOrder
                , richSortOrder
                ]
          , IT.tmDefaultSortOrderId = 1
          , IT.tmStatistics = V.empty
          , IT.tmPartitionStatistics = V.empty
          , IT.tmNextRowId = Nothing
          , IT.tmEncryptionKeys = Map.empty
          }
      !bs = IW.encodeTableMetadata meta
  BS.writeFile (outDir </> "table_metadata_v2_full.json") bs


richPartitionSpec :: IT.PartitionSpec
richPartitionSpec =
  IT.PartitionSpec
    { IT.psSpecId = 1
    , IT.psFields =
        V.singleton
          IT.PartitionField
            { IT.pfSourceIds = V.singleton 2
            , IT.pfFieldId = 1000
            , IT.pfName = "name_trunc"
            , IT.pfTransform = IT.Truncate 1
            }
    }


richSortOrder :: IT.SortOrder
richSortOrder =
  IT.SortOrder
    { IT.soOrderId = 1
    , IT.soFields =
        V.singleton
          IT.SortField
            { IT.sortSourceId = 1
            , IT.sortTransform = IT.Identity
            , IT.sortDirection = IT.Asc
            , IT.sortNullOrder = IT.NullsFirst
            }
    }


mkSnapshot2 :: IT.Snapshot
mkSnapshot2 =
  IT.Snapshot
    { IT.snapId = 1234567891
    , IT.snapParentId = Just 1234567890
    , IT.snapSequenceNumber = 2
    , IT.snapTimestampMs = 1700000010000
    , IT.snapManifestList = "metadata/manifest_list_v2_full.avro"
    , IT.snapSummary =
        Map.fromList
          [ ("operation", "append")
          , ("added-data-files", "3")
          , ("added-records", "310")
          ]
    , IT.snapSchemaId = Just 0
    , IT.snapFirstRowId = Nothing
    , IT.snapKeyId = Nothing
    }


-- Suppress warning on Iceberg.JSON import (only re-exported
-- via Iceberg.Write.encodeTableMetadata under the hood).
_unused :: Maybe ()
_unused = const Nothing IJ.metadataToJSON
