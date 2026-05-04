-- | Avro schemas for Apache Iceberg manifest files and manifest lists.
--
-- The Iceberg specification defines manifest entries and manifest file
-- (manifest list) entries as Avro records. This module constructs the
-- standard 'AvroType' schemas for these structures.
--
-- Also provides SIMD-accelerated helpers for manifest file pruning
-- (comparing serialized partition bounds against filter predicates).
module Iceberg.Manifest
  ( manifestEntrySchema
  , manifestFileSchema
  , filterBoundsMask
    -- * High-level pruning
  , ManifestPruneResult (..)
  , pruneManifestFiles
  ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..))
import Wireform.FFI (compareBoundsBS)

-- | Avro schema for @manifest_entry@ records, as defined by the Iceberg
-- specification. Each manifest entry describes a single data or delete file.
manifestEntrySchema :: AvroType
manifestEntrySchema = AvroRecord
  { avroRecordName      = "manifest_entry"
  , avroRecordNamespace  = Just "org.apache.iceberg"
  , avroRecordDoc        = Just "Entry in an Iceberg manifest file"
  , avroRecordAliases    = V.empty
  , avroRecordProps      = Map.empty
  , avroRecordFields     = V.fromList
      [ mkField "status"          (AvroPrimitive AvroInt) (Just "File status: 0=existing, 1=added, 2=deleted")
      , mkFieldOpt "snapshot_id"  (AvroPrimitive AvroLong) (Just "Snapshot that added the file")
      , mkFieldOpt "sequence_number" (AvroPrimitive AvroLong) (Just "Data sequence number")
      , mkFieldOpt "file_sequence_number" (AvroPrimitive AvroLong) (Just "File sequence number")
      , mkField "data_file"       dataFileSchema Nothing
      ]
  }

dataFileSchema :: AvroType
dataFileSchema = AvroRecord
  { avroRecordName      = "data_file"
  , avroRecordNamespace  = Just "org.apache.iceberg"
  , avroRecordDoc        = Just "Description of a data file"
  , avroRecordAliases    = V.empty
  , avroRecordProps      = Map.empty
  , avroRecordFields     = V.fromList
      [ mkField "file_path"      (AvroPrimitive AvroString) (Just "Full URI of data file")
      , mkField "file_format"    (AvroPrimitive AvroString) (Just "File format: avro, parquet, or orc")
      , mkField "partition"      (AvroRecord
          { avroRecordName      = "partition_data"
          , avroRecordNamespace  = Just "org.apache.iceberg"
          , avroRecordDoc        = Nothing
          , avroRecordAliases    = V.empty
          , avroRecordProps      = Map.empty
          , avroRecordFields     = V.empty
          }) Nothing
      , mkField "record_count"   (AvroPrimitive AvroLong) (Just "Number of records in file")
      , mkField "file_size_in_bytes" (AvroPrimitive AvroLong) (Just "Total file size in bytes")
      , mkField "block_size_in_bytes" (AvroPrimitive AvroLong) Nothing
      , mkFieldOpt "column_sizes" (AvroMap { avroMapValues = AvroPrimitive AvroLong }) (Just "Map from column id to size")
      , mkFieldOpt "value_counts" (AvroMap { avroMapValues = AvroPrimitive AvroLong }) (Just "Map from column id to value count")
      , mkFieldOpt "null_value_counts" (AvroMap { avroMapValues = AvroPrimitive AvroLong }) (Just "Map from column id to null count")
      , mkFieldOpt "lower_bounds" (AvroMap { avroMapValues = AvroPrimitive AvroBytes }) (Just "Map from column id to lower bound")
      , mkFieldOpt "upper_bounds" (AvroMap { avroMapValues = AvroPrimitive AvroBytes }) (Just "Map from column id to upper bound")
      ]
  }

-- | Avro schema for manifest list entries (@manifest_file@ records),
-- as defined by the Iceberg specification. Each entry describes a
-- single manifest file within a snapshot's manifest list.
manifestFileSchema :: AvroType
manifestFileSchema = AvroRecord
  { avroRecordName      = "manifest_file"
  , avroRecordNamespace  = Just "org.apache.iceberg"
  , avroRecordDoc        = Just "Entry in an Iceberg manifest list"
  , avroRecordAliases    = V.empty
  , avroRecordProps      = Map.empty
  , avroRecordFields     = V.fromList
      [ mkField "manifest_path"    (AvroPrimitive AvroString) (Just "Location of the manifest file")
      , mkField "manifest_length"  (AvroPrimitive AvroLong) (Just "Length of the manifest file")
      , mkField "partition_spec_id" (AvroPrimitive AvroInt) (Just "ID of the partition spec")
      , mkField "content"          (AvroPrimitive AvroInt) (Just "0=data, 1=deletes")
      , mkField "sequence_number"  (AvroPrimitive AvroLong) (Just "Sequence number when manifest was written")
      , mkField "min_sequence_number" (AvroPrimitive AvroLong) (Just "Minimum data sequence number in manifest")
      , mkField "added_snapshot_id" (AvroPrimitive AvroLong) (Just "Snapshot ID that added the manifest")
      , mkFieldOpt "added_data_files_count" (AvroPrimitive AvroInt) Nothing
      , mkFieldOpt "existing_data_files_count" (AvroPrimitive AvroInt) Nothing
      , mkFieldOpt "deleted_data_files_count" (AvroPrimitive AvroInt) Nothing
      , mkFieldOpt "added_rows_count" (AvroPrimitive AvroLong) Nothing
      , mkFieldOpt "existing_rows_count" (AvroPrimitive AvroLong) Nothing
      , mkFieldOpt "deleted_rows_count" (AvroPrimitive AvroLong) Nothing
      ]
  }

-- ============================================================
-- Helpers
-- ============================================================

mkField :: String -> AvroType -> Maybe String -> AvroField
mkField name ty doc = AvroField
  { avroFieldName    = T.pack name
  , avroFieldType    = ty
  , avroFieldDefault = Nothing
  , avroFieldOrder   = Nothing
  , avroFieldAliases = V.empty
  , avroFieldDoc     = fmap T.pack doc
  , avroFieldProps   = Map.empty
  }

mkFieldOpt :: String -> AvroType -> Maybe String -> AvroField
mkFieldOpt name ty doc = AvroField
  { avroFieldName    = T.pack name
  , avroFieldType    = AvroUnion { avroUnionBranches = V.fromList [AvroPrimitive AvroNull, ty] }
  , avroFieldDefault = Just AvroNull
  , avroFieldOrder   = Nothing
  , avroFieldAliases = V.empty
  , avroFieldDoc     = fmap T.pack doc
  , avroFieldProps   = Map.empty
  }

-- | Bulk-compare a search value against @N@ serialized partition bounds
-- (all the same @width@ bytes each, packed contiguously in @bounds@).
--
-- Returns a bitmask where bit @i@ is set if @bounds[i] <= search@.
-- For 4-byte LE int32 bounds (common for Iceberg int32 columns), this
-- uses SSE2 to compare 4 bounds at once.
--
-- @bounds@ must contain exactly @count * width@ bytes.
-- @search@ must contain exactly @width@ bytes.
filterBoundsMask
  :: ByteString  -- ^ Packed serialized bounds, @count * width@ bytes
  -> Int         -- ^ Number of bounds
  -> Int         -- ^ Width of each bound in bytes
  -> ByteString  -- ^ Search value, @width@ bytes
  -> Int         -- ^ Bitmask result
filterBoundsMask = compareBoundsBS
{-# INLINE filterBoundsMask #-}

-- ============================================================
-- High-level manifest pruning
-- ============================================================

-- | Result of pruning a manifest list against a predicate.
-- Carries both the surviving manifest paths and a tally of
-- the prune decision so callers can log skip rates.
data ManifestPruneResult a = ManifestPruneResult
  { mprKept    :: ![a]
    -- ^ Manifest files (or any user-tagged value) that the
    -- predicate could not prove unmatched.
  , mprDropped :: !Int
  , mprTotal   :: !Int
  } deriving (Show, Eq)

-- | Run a partition-bounds predicate over a list of manifest
-- file entries before any data file is opened.
--
-- The predicate sees the manifest's @lower_bounds@ /
-- @upper_bounds@ (typically pre-extracted from the Avro
-- @manifest_file@ record) as a @[(columnId, lowerBs, upperBs)]@
-- triple. Returning @False@ drops the manifest entirely;
-- @True@ keeps it (possibly to be pruned again at the data-file
-- level).
--
-- This is the entry point Iceberg readers use to skip whole
-- manifests at scan-planning time. A single manifest typically
-- describes thousands of data files; dropping one without
-- decoding any data file is the largest win in a partitioned-
-- table read.
pruneManifestFiles
  :: ([(Int, ByteString, ByteString)] -> Bool)
    -- ^ Keep predicate over (column id, lower bound, upper bound).
  -> [(a, [(Int, ByteString, ByteString)])]
    -- ^ Manifest tag + per-column partition bounds.
  -> ManifestPruneResult a
pruneManifestFiles keep manifests =
  let !total  = length manifests
      !kept   = [ a | (a, bounds) <- manifests, keep bounds ]
      !nKept  = length kept
  in ManifestPruneResult
       { mprKept    = kept
       , mprDropped = total - nKept
       , mprTotal   = total
       }
