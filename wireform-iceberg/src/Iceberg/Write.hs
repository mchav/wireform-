{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Writers for Iceberg metadata files: manifest files, manifest lists,
-- and table-metadata JSON.
--
-- The Avro container schema produced here is a superset of the v1 spec
-- and matches what Spark / Java write for v2: every "Iceberg-as-of-v2"
-- field is included as a nullable union with default @null@. Reader-side
-- schema resolution (handled by @Avro.Container@) handles older files
-- written without the v2 / v3 fields.
module Iceberg.Write
  ( -- * Avro schemas (writer-side, with full v2/v3 fields)
    writerManifestEntrySchema
  , writerManifestFileSchema
  , writerDataFileSchema
    -- * Writers
  , writeManifestEntries
  , writeManifestList
    -- * Table metadata JSON
  , encodeTableMetadata
  , encodeViewMetadata
  ) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import Avro.Container (writeContainer)
import Avro.Schema
  ( AvroField (..)
  , AvroSchema (..)
  , AvroType (..)
  )
import qualified Avro.Value as AV

import Iceberg.JSON (metadataToJSON, viewMetadataToJSON)
import Iceberg.Types

-- ============================================================
-- Writer-side schemas (richer than Iceberg.Manifest's minimal pair)
-- ============================================================

-- | Manifest entry schema with full v2/v3 metadata. Fields beyond the
-- minimal v1 set are written as nullable unions @[null, T]@ with default
-- @null@ to preserve forward/backward compatibility.
writerManifestEntrySchema :: AvroType
writerManifestEntrySchema = AvroRecord
  { avroRecordName      = "manifest_entry"
  , avroRecordNamespace = Just "org.apache.iceberg"
  , avroRecordDoc       = Just "Entry in an Iceberg manifest file"
  , avroRecordAliases   = V.empty
  , avroRecordProps     = Map.empty
  , avroRecordFields    = V.fromList
      [ field "status" (AvroPrimitive AvroInt)
      , optField "snapshot_id" (AvroPrimitive AvroLong)
      , optField "sequence_number" (AvroPrimitive AvroLong)
      , optField "file_sequence_number" (AvroPrimitive AvroLong)
      , field "data_file" writerDataFileSchema
      ]
  }

-- | Data file record carrying the full v2/v3 statistics fields.
writerDataFileSchema :: AvroType
writerDataFileSchema = AvroRecord
  { avroRecordName      = "data_file"
  , avroRecordNamespace = Just "org.apache.iceberg"
  , avroRecordDoc       = Just "Description of a data or delete file"
  , avroRecordAliases   = V.empty
  , avroRecordProps     = Map.empty
  , avroRecordFields    = V.fromList
      [ optField "content" (AvroPrimitive AvroInt)
      , field "file_path"  (AvroPrimitive AvroString)
      , field "file_format" (AvroPrimitive AvroString)
      , field "partition" emptyPartitionSchema
      , field "record_count" (AvroPrimitive AvroLong)
      , field "file_size_in_bytes" (AvroPrimitive AvroLong)
      , optField "column_sizes"      (intInt64KvArray "k101_v102")
      , optField "value_counts"      (intInt64KvArray "k103_v104")
      , optField "null_value_counts" (intInt64KvArray "k105_v106")
      , optField "nan_value_counts"  (intInt64KvArray "k121_v122")
      , optField "lower_bounds"      (intBytesKvArray "k107_v108")
      , optField "upper_bounds"      (intBytesKvArray "k109_v110")
      , optField "key_metadata"      (AvroPrimitive AvroBytes)
      , optField "split_offsets"     (AvroArray { avroArrayItems = AvroPrimitive AvroLong })
      , optField "equality_ids"      (AvroArray { avroArrayItems = AvroPrimitive AvroInt })
      , optField "sort_order_id"     (AvroPrimitive AvroInt)
      , optField "first_row_id"      (AvroPrimitive AvroLong)
      , optField "referenced_data_file" (AvroPrimitive AvroString)
      , optField "content_offset"    (AvroPrimitive AvroLong)
      , optField "content_size_in_bytes" (AvroPrimitive AvroLong)
      ]
  }

emptyPartitionSchema :: AvroType
emptyPartitionSchema = AvroRecord
  { avroRecordName      = "partition_data"
  , avroRecordNamespace = Just "org.apache.iceberg"
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordProps     = Map.empty
  , avroRecordFields    = V.empty
  }

-- | Manifest list entry schema with v2/v3 fields.
writerManifestFileSchema :: AvroType
writerManifestFileSchema = AvroRecord
  { avroRecordName      = "manifest_file"
  , avroRecordNamespace = Just "org.apache.iceberg"
  , avroRecordDoc       = Just "Entry in an Iceberg manifest list"
  , avroRecordAliases   = V.empty
  , avroRecordProps     = Map.empty
  , avroRecordFields    = V.fromList
      [ field "manifest_path" (AvroPrimitive AvroString)
      , field "manifest_length" (AvroPrimitive AvroLong)
      , field "partition_spec_id" (AvroPrimitive AvroInt)
      , field "content" (AvroPrimitive AvroInt)
      , field "sequence_number" (AvroPrimitive AvroLong)
      , field "min_sequence_number" (AvroPrimitive AvroLong)
      , field "added_snapshot_id" (AvroPrimitive AvroLong)
      , optField "added_data_files_count" (AvroPrimitive AvroInt)
      , optField "existing_data_files_count" (AvroPrimitive AvroInt)
      , optField "deleted_data_files_count" (AvroPrimitive AvroInt)
      , optField "added_rows_count" (AvroPrimitive AvroLong)
      , optField "existing_rows_count" (AvroPrimitive AvroLong)
      , optField "deleted_rows_count" (AvroPrimitive AvroLong)
      , optField "partitions" (AvroArray { avroArrayItems = fieldSummarySchema })
      , optField "key_metadata" (AvroPrimitive AvroBytes)
      , optField "first_row_id" (AvroPrimitive AvroLong)
      ]
  }

fieldSummarySchema :: AvroType
fieldSummarySchema = AvroRecord
  { avroRecordName      = "field_summary"
  , avroRecordNamespace = Just "org.apache.iceberg"
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordProps     = Map.empty
  , avroRecordFields    = V.fromList
      [ field "contains_null" (AvroPrimitive AvroBool)
      , optField "contains_nan" (AvroPrimitive AvroBool)
      , optField "lower_bound" (AvroPrimitive AvroBytes)
      , optField "upper_bound" (AvroPrimitive AvroBytes)
      ]
  }

-- ============================================================
-- Encoders (typed value -> AV.Value)
-- ============================================================

manifestEntryToAvro :: ManifestEntry -> AV.Value
manifestEntryToAvro me = AV.Record $ V.fromList
  [ AV.Int (statusToInt (meStatus me))
  , optLong (meSnapshotId me)
  , optLong (meSequenceNumber me)
  , optLong (meFileSequenceNumber me)
  , dataFileToAvro
      (case meDataFile me of
         Just df -> df
         Nothing -> manifestEntryToImpliedDataFile me)
  ]

manifestEntryToImpliedDataFile :: ManifestEntry -> DataFile
manifestEntryToImpliedDataFile me = DataFile
  { dataFileContent = DataContent
  , dataFileFilePath = meFilePath me
  , dataFileFileFormat = meFileFormat me
  , dataFilePartition = mePartition me
  , dataFileRecordCount = meRecordCount me
  , dataFileFileSize = meFileSizeBytes me
  , dataFileColumnSizes = Map.empty
  , dataFileValueCounts = Map.empty
  , dataFileNullValueCounts = Map.empty
  , dataFileNanValueCounts = Map.empty
  , dataFileLowerBounds = Map.empty
  , dataFileUpperBounds = Map.empty
  , dataFileKeyMetadata = Nothing
  , dataFileSplitOffsets = V.empty
  , dataFileEqualityIds = V.empty
  , dataFileSortOrderId = Nothing
  , dataFileFirstRowId = Nothing
  , dataFileReferencedDataFile = Nothing
  , dataFileContentOffset = Nothing
  , dataFileContentSize = Nothing
  }

statusToInt :: ManifestStatus -> Int32
statusToInt Existing = 0
statusToInt Added = 1
statusToInt Deleted = 2

dataFileToAvro :: DataFile -> AV.Value
dataFileToAvro df = AV.Record $ V.fromList
  [ optInt (Just (contentToInt (dataFileContent df)))
  , AV.String (dataFileFilePath df)
  , AV.String (fileFormatToText (dataFileFileFormat df))
  , partitionToAvro (dataFilePartition df)
  , AV.Long (dataFileRecordCount df)
  , AV.Long (dataFileFileSize df)
  , optMapIntInt64 (dataFileColumnSizes df)
  , optMapIntInt64 (dataFileValueCounts df)
  , optMapIntInt64 (dataFileNullValueCounts df)
  , optMapIntInt64 (dataFileNanValueCounts df)
  , optMapIntBytes (dataFileLowerBounds df)
  , optMapIntBytes (dataFileUpperBounds df)
  , optBytes (dataFileKeyMetadata df)
  , optLongArray (dataFileSplitOffsets df)
  , optIntArray (dataFileEqualityIds df)
  , optInt (fmap fromIntegral (dataFileSortOrderId df))
  , optLong (dataFileFirstRowId df)
  , optString (dataFileReferencedDataFile df)
  , optLong (dataFileContentOffset df)
  , optLong (dataFileContentSize df)
  ]

contentToInt :: ManifestContent -> Int32
contentToInt DataContent = 0
contentToInt DeletesContent = 1

fileFormatToText :: FileFormat -> T.Text
fileFormatToText AvroFormat = "AVRO"
fileFormatToText ParquetFormat = "PARQUET"
fileFormatToText OrcFormat = "ORC"

partitionToAvro :: V.Vector (Maybe AV.Value) -> AV.Value
partitionToAvro vs = AV.Record (V.map (maybe AV.Null id) vs)

manifestFileToAvro :: ManifestFile -> AV.Value
manifestFileToAvro mf = AV.Record $ V.fromList
  [ AV.String (mfPath mf)
  , AV.Long (mfLength mf)
  , AV.Int (fromIntegral (mfPartitionSpecId mf))
  , AV.Int (contentToInt (mfContent mf))
  , AV.Long (mfSequenceNumber mf)
  , AV.Long (mfMinSequenceNumber mf)
  , AV.Long (mfAddedSnapshotId mf)
  , optInt (fmap fromIntegral (mfAddedDataFilesCount mf))
  , optInt (fmap fromIntegral (mfExistingDataFilesCount mf))
  , optInt (fmap fromIntegral (mfDeletedDataFilesCount mf))
  , optLong (mfAddedRowsCount mf)
  , optLong (mfExistingRowsCount mf)
  , optLong (mfDeletedRowsCount mf)
  , optFieldSummaryArray (mfPartitions mf)
  , optBytes (mfKeyMetadata mf)
  , optLong (mfFirstRowId mf)
  ]

fieldSummaryToAvro :: FieldSummary -> AV.Value
fieldSummaryToAvro fs = AV.Record $ V.fromList
  [ AV.Bool (fsContainsNull fs)
  , optBool (fsContainsNan fs)
  , optBytes (fsLowerBound fs)
  , optBytes (fsUpperBound fs)
  ]

-- ============================================================
-- Optional encoders ("Avro union with null" wrappers)
-- ============================================================

optLong :: Maybe Int64 -> AV.Value
optLong = maybe (AV.Union 0 AV.Null) (\x -> AV.Union 1 (AV.Long x))

optInt :: Maybe Int32 -> AV.Value
optInt = maybe (AV.Union 0 AV.Null) (\x -> AV.Union 1 (AV.Int x))

optString :: Maybe T.Text -> AV.Value
optString = maybe (AV.Union 0 AV.Null) (\x -> AV.Union 1 (AV.String x))

optBytes :: Maybe ByteString -> AV.Value
optBytes = maybe (AV.Union 0 AV.Null) (\x -> AV.Union 1 (AV.Bytes x))

optBool :: Maybe Bool -> AV.Value
optBool = maybe (AV.Union 0 AV.Null) (\x -> AV.Union 1 (AV.Bool x))

optMapIntInt64 :: Map.Map Int Int64 -> AV.Value
optMapIntInt64 m
  | Map.null m = AV.Union 0 AV.Null
  | otherwise =
      AV.Union 1 (AV.Array (V.fromList
        [ AV.Record (V.fromList [AV.Int (fromIntegral k), AV.Long v])
        | (k, v) <- Map.toAscList m
        ]))

optMapIntBytes :: Map.Map Int ByteString -> AV.Value
optMapIntBytes m
  | Map.null m = AV.Union 0 AV.Null
  | otherwise =
      AV.Union 1 (AV.Array (V.fromList
        [ AV.Record (V.fromList [AV.Int (fromIntegral k), AV.Bytes v])
        | (k, v) <- Map.toAscList m
        ]))

optLongArray :: V.Vector Int64 -> AV.Value
optLongArray xs
  | V.null xs = AV.Union 0 AV.Null
  | otherwise = AV.Union 1 (AV.Array (V.map AV.Long xs))

optIntArray :: V.Vector Int -> AV.Value
optIntArray xs
  | V.null xs = AV.Union 0 AV.Null
  | otherwise = AV.Union 1 (AV.Array (V.map (AV.Int . fromIntegral) xs))

optFieldSummaryArray :: V.Vector FieldSummary -> AV.Value
optFieldSummaryArray xs
  | V.null xs = AV.Union 0 AV.Null
  | otherwise = AV.Union 1 (AV.Array (V.map fieldSummaryToAvro xs))

-- ============================================================
-- Schema field shorthands
-- ============================================================

field :: T.Text -> AvroType -> AvroField
field name ty = AvroField
  { avroFieldName    = name
  , avroFieldType    = ty
  , avroFieldDefault = Nothing
  , avroFieldOrder   = Nothing
  , avroFieldAliases = V.empty
  , avroFieldDoc     = Nothing
  , avroFieldProps   = Map.empty
  }

optField :: T.Text -> AvroType -> AvroField
optField name ty = AvroField
  { avroFieldName    = name
  , avroFieldType    = AvroUnion { avroUnionBranches = V.fromList [AvroPrimitive AvroNull, ty] }
  , avroFieldDefault = Just AvroNull
  , avroFieldOrder   = Nothing
  , avroFieldAliases = V.empty
  , avroFieldDoc     = Nothing
  , avroFieldProps   = Map.empty
  }

-- | Iceberg encodes maps in manifest files as arrays of @{key, value}@ records
-- so that integer keys are supported (Avro standard maps are keyed by string).
intInt64KvArray :: T.Text -> AvroType
intInt64KvArray recName = AvroArray
  { avroArrayItems = AvroRecord
      { avroRecordName      = recName
      , avroRecordNamespace = Just "org.apache.iceberg"
      , avroRecordDoc       = Nothing
      , avroRecordAliases   = V.empty
      , avroRecordProps     = Map.empty
      , avroRecordFields    = V.fromList
          [ field "key"   (AvroPrimitive AvroInt)
          , field "value" (AvroPrimitive AvroLong)
          ]
      }
  }

intBytesKvArray :: T.Text -> AvroType
intBytesKvArray recName = AvroArray
  { avroArrayItems = AvroRecord
      { avroRecordName      = recName
      , avroRecordNamespace = Just "org.apache.iceberg"
      , avroRecordDoc       = Nothing
      , avroRecordAliases   = V.empty
      , avroRecordProps     = Map.empty
      , avroRecordFields    = V.fromList
          [ field "key"   (AvroPrimitive AvroInt)
          , field "value" (AvroPrimitive AvroBytes)
          ]
      }
  }

-- ============================================================
-- Top-level writers
-- ============================================================

-- | Write a manifest file: an Avro container of @manifest_entry@ records.
writeManifestEntries :: V.Vector ManifestEntry -> ByteString
writeManifestEntries entries =
  writeContainer writerManifestEntrySchema (V.map manifestEntryToAvro entries)

-- | Write a manifest list: an Avro container of @manifest_file@ records.
writeManifestList :: V.Vector ManifestFile -> ByteString
writeManifestList files =
  writeContainer writerManifestFileSchema (V.map manifestFileToAvro files)

-- ============================================================
-- Table metadata JSON
-- ============================================================

-- | Encode 'TableMetadata' as canonical Iceberg JSON, suitable for writing
-- to @metadata/v\<n\>.metadata.json@.
encodeTableMetadata :: TableMetadata -> ByteString
encodeTableMetadata = BL.toStrict . Aeson.encode . metadataToJSON

-- | Encode 'ViewMetadata' as canonical Iceberg view JSON.
encodeViewMetadata :: ViewMetadata -> ByteString
encodeViewMetadata = BL.toStrict . Aeson.encode . viewMetadataToJSON
