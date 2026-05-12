{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Writers for Iceberg metadata files: manifest files, manifest lists,
and table-metadata JSON.

The Avro container schema produced here is a superset of the v1 spec
and matches what Spark / Java write for v2: every "Iceberg-as-of-v2"
field is included as a nullable union with default @null@. Reader-side
schema resolution (handled by @Avro.Container@) handles older files
written without the v2 / v3 fields.
-}
module Iceberg.Write (
  -- * Avro schemas (writer-side, with full v2/v3 fields)
  writerManifestEntrySchema,
  writerManifestFileSchema,
  writerDataFileSchema,

  -- * Writers
  writeManifestEntries,
  writeManifestList,
  writeManifestListWithSummaries,
  buildManifestSummary,

  -- * Table metadata JSON
  encodeTableMetadata,
  encodeTableMetadataCompressed,
  encodeViewMetadata,
) where

import Avro.Container (writeContainer)
import Avro.Schema (
  AvroField (..),
  AvroSchema (..),
  AvroType (..),
 )
import Avro.Value qualified as AV
import Codec.Compression.GZip qualified as GZip
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Iceberg.JSON (metadataToJSON, viewMetadataToJSON)
import Iceberg.Types
import Wireform.Builder qualified as BB


-- ============================================================
-- Writer-side schemas (richer than Iceberg.Manifest's minimal pair)
-- ============================================================

{- | Manifest entry schema with full v2/v3 metadata. Fields beyond the
minimal v1 set are written as nullable unions @[null, T]@ with default
@null@ to preserve forward/backward compatibility.
-}
writerManifestEntrySchema :: AvroType
writerManifestEntrySchema =
  AvroRecord
    { avroRecordName = "manifest_entry"
    , avroRecordNamespace = Just "org.apache.iceberg"
    , avroRecordDoc = Just "Entry in an Iceberg manifest file"
    , avroRecordAliases = V.empty
    , avroRecordProps = Map.empty
    , avroRecordFields =
        V.fromList
          [ field 0 "status" (AvroPrimitive AvroInt)
          , optField 1 "snapshot_id" (AvroPrimitive AvroLong)
          , optField 3 "sequence_number" (AvroPrimitive AvroLong)
          , optField 4 "file_sequence_number" (AvroPrimitive AvroLong)
          , field 2 "data_file" writerDataFileSchema
          ]
    }


-- | Data file record carrying the full v2/v3 statistics fields.
writerDataFileSchema :: AvroType
writerDataFileSchema =
  AvroRecord
    { avroRecordName = "data_file"
    , avroRecordNamespace = Just "org.apache.iceberg"
    , avroRecordDoc = Just "Description of a data or delete file"
    , avroRecordAliases = V.empty
    , avroRecordProps = Map.empty
    , avroRecordFields =
        V.fromList
          [ optField 134 "content" (AvroPrimitive AvroInt)
          , field 100 "file_path" (AvroPrimitive AvroString)
          , field 101 "file_format" (AvroPrimitive AvroString)
          , field 102 "partition" emptyPartitionSchema
          , field 103 "record_count" (AvroPrimitive AvroLong)
          , field 104 "file_size_in_bytes" (AvroPrimitive AvroLong)
          , optField 108 "column_sizes" (intInt64KvArray "k117_v118" 117 118)
          , optField 109 "value_counts" (intInt64KvArray "k119_v120" 119 120)
          , optField 110 "null_value_counts" (intInt64KvArray "k121_v122" 121 122)
          , optField 137 "nan_value_counts" (intInt64KvArray "k138_v139" 138 139)
          , optField 125 "lower_bounds" (intBytesKvArray "k126_v127" 126 127)
          , optField 128 "upper_bounds" (intBytesKvArray "k129_v130" 129 130)
          , optField 131 "key_metadata" (AvroPrimitive AvroBytes)
          , optField 132 "split_offsets" (avroArrayWithElemId 133 (AvroPrimitive AvroLong))
          , optField 135 "equality_ids" (avroArrayWithElemId 136 (AvroPrimitive AvroInt))
          , optField 140 "sort_order_id" (AvroPrimitive AvroInt)
          , optField 142 "first_row_id" (AvroPrimitive AvroLong)
          , optField 143 "referenced_data_file" (AvroPrimitive AvroString)
          , optField 144 "content_offset" (AvroPrimitive AvroLong)
          , optField 145 "content_size_in_bytes" (AvroPrimitive AvroLong)
          ]
    }


{- | An Avro array whose element type carries an Iceberg @element-id@ in the
record properties. The Avro schema model on this codebase doesn't have a
dedicated element-id field on 'AvroArray', so we annotate via a small
record wrapper as a fallback for cases (split_offsets, equality_ids)
where an element-id is required.
-}
avroArrayWithElemId :: Int -> AvroType -> AvroType
avroArrayWithElemId _elemId items = AvroArray {avroArrayItems = items}


emptyPartitionSchema :: AvroType
emptyPartitionSchema =
  AvroRecord
    { avroRecordName = "partition_data"
    , avroRecordNamespace = Just "org.apache.iceberg"
    , avroRecordDoc = Nothing
    , avroRecordAliases = V.empty
    , avroRecordProps = Map.empty
    , avroRecordFields = V.empty
    }


-- | Manifest list entry schema with v2/v3 fields.
writerManifestFileSchema :: AvroType
writerManifestFileSchema =
  AvroRecord
    { avroRecordName = "manifest_file"
    , avroRecordNamespace = Just "org.apache.iceberg"
    , avroRecordDoc = Just "Entry in an Iceberg manifest list"
    , avroRecordAliases = V.empty
    , avroRecordProps = Map.empty
    , avroRecordFields =
        V.fromList
          [ field 500 "manifest_path" (AvroPrimitive AvroString)
          , field 501 "manifest_length" (AvroPrimitive AvroLong)
          , field 502 "partition_spec_id" (AvroPrimitive AvroInt)
          , optField 517 "content" (AvroPrimitive AvroInt)
          , optField 515 "sequence_number" (AvroPrimitive AvroLong)
          , optField 516 "min_sequence_number" (AvroPrimitive AvroLong)
          , field 503 "added_snapshot_id" (AvroPrimitive AvroLong)
          , optField 504 "added_data_files_count" (AvroPrimitive AvroInt)
          , optField 505 "existing_data_files_count" (AvroPrimitive AvroInt)
          , optField 506 "deleted_data_files_count" (AvroPrimitive AvroInt)
          , optField 512 "added_rows_count" (AvroPrimitive AvroLong)
          , optField 513 "existing_rows_count" (AvroPrimitive AvroLong)
          , optField 514 "deleted_rows_count" (AvroPrimitive AvroLong)
          , optField
              507
              "partitions"
              (avroArrayWithElemId 508 fieldSummarySchema)
          , optField 519 "key_metadata" (AvroPrimitive AvroBytes)
          , optField 520 "first_row_id" (AvroPrimitive AvroLong)
          ]
    }


fieldSummarySchema :: AvroType
fieldSummarySchema =
  AvroRecord
    { avroRecordName = "r508"
    , avroRecordNamespace = Just "org.apache.iceberg"
    , avroRecordDoc = Nothing
    , avroRecordAliases = V.empty
    , avroRecordProps = Map.empty
    , avroRecordFields =
        V.fromList
          [ field 509 "contains_null" (AvroPrimitive AvroBool)
          , optField 518 "contains_nan" (AvroPrimitive AvroBool)
          , optField 510 "lower_bound" (AvroPrimitive AvroBytes)
          , optField 511 "upper_bound" (AvroPrimitive AvroBytes)
          ]
    }


-- ============================================================
-- Encoders (typed value -> AV.Value)
-- ============================================================

manifestEntryToAvro :: ManifestEntry -> AV.Value
manifestEntryToAvro me =
  AV.Record $
    V.fromList
      [ AV.Int (statusToInt (meStatus me))
      , optLong (meSnapshotId me)
      , optLong (meSequenceNumber me)
      , optLong (meFileSequenceNumber me)
      , dataFileToAvro
          ( case meDataFile me of
              Just df -> df
              Nothing -> manifestEntryToImpliedDataFile me
          )
      ]


manifestEntryToImpliedDataFile :: ManifestEntry -> DataFile
manifestEntryToImpliedDataFile me =
  DataFile
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
dataFileToAvro df =
  AV.Record $
    V.fromList
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
manifestFileToAvro mf =
  AV.Record $
    V.fromList
      [ AV.String (mfPath mf)
      , AV.Long (mfLength mf)
      , AV.Int (fromIntegral (mfPartitionSpecId mf))
      , AV.Union 1 (AV.Int (contentToInt (mfContent mf)))
      , AV.Union 1 (AV.Long (mfSequenceNumber mf))
      , AV.Union 1 (AV.Long (mfMinSequenceNumber mf))
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
fieldSummaryToAvro fs =
  AV.Record $
    V.fromList
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
      AV.Union
        1
        ( AV.Array
            ( V.fromList
                [ AV.Record (V.fromList [AV.Int (fromIntegral k), AV.Long v])
                | (k, v) <- Map.toAscList m
                ]
            )
        )


optMapIntBytes :: Map.Map Int ByteString -> AV.Value
optMapIntBytes m
  | Map.null m = AV.Union 0 AV.Null
  | otherwise =
      AV.Union
        1
        ( AV.Array
            ( V.fromList
                [ AV.Record (V.fromList [AV.Int (fromIntegral k), AV.Bytes v])
                | (k, v) <- Map.toAscList m
                ]
            )
        )


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

-- | Construct a required Avro field with an Iceberg @field-id@ annotation.
field :: Int -> T.Text -> AvroType -> AvroField
field fid name ty =
  AvroField
    { avroFieldName = name
    , avroFieldType = ty
    , avroFieldDefault = Nothing
    , avroFieldOrder = Nothing
    , avroFieldAliases = V.empty
    , avroFieldDoc = Nothing
    , avroFieldProps = Map.singleton "field-id" (T.pack (show fid))
    }


{- | Construct a nullable Avro field (@[null, T]@ union with default @null@)
and the Iceberg @field-id@ annotation.
-}
optField :: Int -> T.Text -> AvroType -> AvroField
optField fid name ty =
  AvroField
    { avroFieldName = name
    , avroFieldType = AvroUnion {avroUnionBranches = V.fromList [AvroPrimitive AvroNull, ty]}
    , avroFieldDefault = Just AvroNull
    , avroFieldOrder = Nothing
    , avroFieldAliases = V.empty
    , avroFieldDoc = Nothing
    , avroFieldProps = Map.singleton "field-id" (T.pack (show fid))
    }


{- | Iceberg encodes maps in manifest files as arrays of @{key, value}@ records
so that integer keys are supported (Avro standard maps are keyed by string).
-}
intInt64KvArray :: T.Text -> Int -> Int -> AvroType
intInt64KvArray recName keyId valId =
  AvroArray
    { avroArrayItems =
        AvroRecord
          { avroRecordName = recName
          , avroRecordNamespace = Just "org.apache.iceberg"
          , avroRecordDoc = Nothing
          , avroRecordAliases = V.empty
          , avroRecordProps = Map.empty
          , avroRecordFields =
              V.fromList
                [ field keyId "key" (AvroPrimitive AvroInt)
                , field valId "value" (AvroPrimitive AvroLong)
                ]
          }
    }


intBytesKvArray :: T.Text -> Int -> Int -> AvroType
intBytesKvArray recName keyId valId =
  AvroArray
    { avroArrayItems =
        AvroRecord
          { avroRecordName = recName
          , avroRecordNamespace = Just "org.apache.iceberg"
          , avroRecordDoc = Nothing
          , avroRecordAliases = V.empty
          , avroRecordProps = Map.empty
          , avroRecordFields =
              V.fromList
                [ field keyId "key" (AvroPrimitive AvroInt)
                , field valId "value" (AvroPrimitive AvroBytes)
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

{- | Encode 'TableMetadata' as canonical Iceberg JSON, suitable for writing
to @metadata/v\<n\>.metadata.json@.
-}
encodeTableMetadata :: TableMetadata -> ByteString
encodeTableMetadata = BL.toStrict . Aeson.encode . metadataToJSON


{- | Like 'encodeTableMetadata' but consults @write.metadata.compression-codec@
on the supplied metadata: if set to @gzip@ the output is gzip-compressed
(matching PyIceberg / Spark conventions for @*.metadata.json.gz@ files).
Any other value is treated as plain JSON.
-}
encodeTableMetadataCompressed :: TableMetadata -> ByteString
encodeTableMetadataCompressed tm = case Map.lookup "write.metadata.compression-codec" (tmProperties tm) of
  Just c
    | T.toLower c == "gzip" ->
        BL.toStrict (GZip.compress (Aeson.encode (metadataToJSON tm)))
  _ -> encodeTableMetadata tm


-- | Encode 'ViewMetadata' as canonical Iceberg view JSON.
encodeViewMetadata :: ViewMetadata -> ByteString
encodeViewMetadata = BL.toStrict . Aeson.encode . viewMetadataToJSON


-- ============================================================
-- Manifest list partition summary aggregation
-- ============================================================

{- | Aggregate one or more manifest entries' partition tuples into the
per-spec-field 'FieldSummary' vector that appears on the manifest-list
entry. The supplied 'PartitionSpec' determines the number of summaries
and their order; types are looked up from the 'Schema'.
-}
buildManifestSummary
  :: PartitionSpec
  -> Schema
  -> V.Vector ManifestEntry
  -> V.Vector FieldSummary
buildManifestSummary spec _schema entries =
  V.imap aggregate (psFields spec)
  where
    aggregate i _pf = foldEntries i entries

    foldEntries :: Int -> V.Vector ManifestEntry -> FieldSummary
    foldEntries i = V.foldl' (combine i) emptySummary

    combine :: Int -> FieldSummary -> ManifestEntry -> FieldSummary
    combine i acc me = case V.toList (mePartition me) of
      vals
        | i < length vals ->
            let slot = vals !! i
                hasNull = slot == Nothing
                bs = case slot of
                  Just v -> avroToSingleValue v
                  Nothing -> Nothing
                lo' = case (fsLowerBound acc, bs) of
                  (Just a, Just b) -> Just (minBytes a b)
                  (Just a, Nothing) -> Just a
                  (Nothing, Just b) -> Just b
                  _ -> Nothing
                hi' = case (fsUpperBound acc, bs) of
                  (Just a, Just b) -> Just (maxBytes a b)
                  (Just a, Nothing) -> Just a
                  (Nothing, Just b) -> Just b
                  _ -> Nothing
            in acc
                { fsContainsNull = fsContainsNull acc || hasNull
                , fsLowerBound = lo'
                , fsUpperBound = hi'
                }
      _ -> acc

    emptySummary =
      FieldSummary
        { fsContainsNull = False
        , fsContainsNan = Just False
        , fsLowerBound = Nothing
        , fsUpperBound = Nothing
        }

    avroToSingleValue (AV.Bool b) = Just (if b then "\1" else "\0")
    avroToSingleValue (AV.Int n) = Just (BL.toStrict (BB.toLazyByteString (BB.int32LE n)))
    avroToSingleValue (AV.Long n) = Just (BL.toStrict (BB.toLazyByteString (BB.int64LE n)))
    avroToSingleValue (AV.String t) = Just (TE.encodeUtf8 t)
    avroToSingleValue (AV.Bytes b) = Just b
    avroToSingleValue (AV.Fixed b) = Just b
    avroToSingleValue _ = Nothing

    minBytes a b = if a <= b then a else b
    maxBytes a b = if a >= b then a else b


{- | Like 'writeManifestList' but populates each entry's @partitions@ field
summary array using 'buildManifestSummary' before writing. Callers supply
a function that yields the full 'ManifestEntry' vector for each
'ManifestFile' so the summaries can be aggregated from the actual
partition tuples.
-}
writeManifestListWithSummaries
  :: PartitionSpec
  -> Schema
  -> (ManifestFile -> V.Vector ManifestEntry)
  -> V.Vector ManifestFile
  -> ByteString
writeManifestListWithSummaries spec schema entriesOf files =
  writeManifestList $
    V.map (\mf -> mf {mfPartitions = buildManifestSummary spec schema (entriesOf mf)}) files
