{- | Apache Iceberg table metadata types.

Iceberg is a table format whose metadata is stored as JSON (table metadata)
and Avro (manifest files/manifest lists). This module defines the core
schema types and metadata structures per the Iceberg specification.
-}
module Iceberg.Types (
  -- * Table metadata
  TableMetadata (..),
  Schema (..),
  StructField (..),
  IcebergType (..),
  Snapshot (..),
  PartitionSpec (..),
  PartitionField (..),
  pfPrimarySourceId,
  Transform (..),
  SortOrder (..),
  SortField (..),
  SortDirection (..),
  NullOrder (..),
  SnapshotLogEntry (..),
  MetadataLogEntry (..),
  StatisticsFile (..),
  BlobMetadata (..),
  PartitionStatisticsFile (..),

  -- * Default values
  DefaultValue (..),

  -- * Manifest types
  ManifestEntry (..),
  ManifestStatus (..),
  FileFormat (..),
  ManifestFile (..),
  ManifestContent (..),
  FieldSummary (..),
  DataFile (..),

  -- * Delete file types
  DeleteFileContent (..),
  DeleteFile (..),
  PositionDelete (..),
  EqualityDeleteSpec (..),

  -- * Snapshot references
  SnapshotRef (..),

  -- * Name mapping
  NameMapping (..),
  MappedField (..),

  -- * View spec
  ViewMetadata (..),
  ViewVersion (..),
  ViewRepresentation (..),
  ViewHistoryEntry (..),

  -- * Partition values (re-export)
  Value,
) where

import Avro.Value qualified as Avro
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)


-- | Re-export Avro.Value for use as Iceberg partition values.
type Value = Avro.Value


data TableMetadata = TableMetadata
  { tmFormatVersion :: {-# UNPACK #-} !Int
  , tmTableUuid :: !Text
  , tmLocation :: !Text
  , tmLastSequenceNumber :: {-# UNPACK #-} !Int64
  , tmLastUpdatedMs :: {-# UNPACK #-} !Int64
  , tmLastColumnId :: {-# UNPACK #-} !Int
  , tmCurrentSchemaId :: {-# UNPACK #-} !Int
  , tmSchemas :: !(Vector Schema)
  , tmCurrentSnapshotId :: !(Maybe Int64)
  , tmSnapshots :: !(Vector Snapshot)
  , tmPartitionSpecs :: !(Vector PartitionSpec)
  , tmDefaultSpecId :: {-# UNPACK #-} !Int
  , tmLastPartitionId :: {-# UNPACK #-} !Int
  -- ^ Highest partition field id ever assigned across all specs (v2+).
  , tmSortOrders :: !(Vector SortOrder)
  , tmDefaultSortOrderId :: {-# UNPACK #-} !Int
  , tmProperties :: !(Map Text Text)
  , tmSnapshotLog :: !(Vector SnapshotLogEntry)
  , tmMetadataLog :: !(Vector MetadataLogEntry)
  -- ^ Log of past table metadata file locations.
  , tmSnapshotRefs :: !(Map Text SnapshotRef)
  , tmStatistics :: !(Vector StatisticsFile)
  -- ^ Optional Puffin statistics files referenced by snapshot.
  , tmPartitionStatistics :: !(Vector PartitionStatisticsFile)
  -- ^ Optional partition statistics files referenced by snapshot.
  , tmNextRowId :: !(Maybe Int64)
  -- ^ V3 row lineage: next available row id for new rows.
  , tmEncryptionKeys :: !(Map Text Text)
  -- ^ V3 table encryption: kms-key-id keyed by reference name.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data Schema = Schema
  { schemaId :: {-# UNPACK #-} !Int
  , schemaFields :: !(Vector StructField)
  , schemaIdentifierFieldIds :: !(Vector Int)
  -- ^ Optional set of primitive field ids that identify rows.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | A column default value (initial-default, write-default).

Stored as a JSON 'Avro.Value' analogue. We reuse 'Avro.Value' as the carrier
because Iceberg defaults follow the same single-value JSON encoding as
Avro's logical types (booleans, numbers, strings, byte sequences,
struct\/list\/map literals).
-}
data DefaultValue
  = DefaultNull
  | DefaultJSON !Avro.Value
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data StructField = StructField
  { sfId :: {-# UNPACK #-} !Int
  , sfName :: !Text
  , sfRequired :: !Bool
  , sfType :: !IcebergType
  , sfDoc :: !(Maybe Text)
  , sfInitialDefault :: !(Maybe DefaultValue)
  -- ^ V3 default for rows written before the field existed.
  , sfWriteDefault :: !(Maybe DefaultValue)
  -- ^ V3 default for new rows when the writer does not supply a value.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data IcebergType
  = TBoolean
  | TInt
  | TLong
  | TFloat
  | TDouble
  | TDate
  | TTime
  | TTimestamp
  | TTimestampTz
  | -- | V3: nanosecond-precision timestamp without timezone.
    TTimestampNs
  | -- | V3: nanosecond-precision timestamp with timezone.
    TTimestampTzNs
  | TString
  | TUuid
  | TFixed {-# UNPACK #-} !Int
  | TBinary
  | TDecimal {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | TStruct !(Vector StructField)
  | TList {-# UNPACK #-} !Int !IcebergType
  | TMap {-# UNPACK #-} !Int !IcebergType {-# UNPACK #-} !Int !IcebergType
  | -- | V3: typeless null column not stored in data files.
    TUnknown
  | -- | V3: semi-structured (Parquet variant) column.
    TVariant
  | -- | V3: geometry with optional CRS string.
    TGeometry !Text
  | -- | V3: geography with CRS and edge-interpolation algorithm.
    TGeography !Text !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data Snapshot = Snapshot
  { snapId :: {-# UNPACK #-} !Int64
  , snapParentId :: !(Maybe Int64)
  , snapSequenceNumber :: {-# UNPACK #-} !Int64
  , snapTimestampMs :: {-# UNPACK #-} !Int64
  , snapManifestList :: !Text
  , snapSummary :: !(Map Text Text)
  , snapSchemaId :: !(Maybe Int)
  -- ^ Schema id used for this snapshot (recorded from V2 onward).
  , snapFirstRowId :: !(Maybe Int64)
  -- ^ V3 row lineage: starting row id assigned during this snapshot.
  , snapKeyId :: !(Maybe Text)
  -- ^ V3 encryption: KMS reference for this snapshot's key.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data PartitionSpec = PartitionSpec
  { psSpecId :: {-# UNPACK #-} !Int
  , psFields :: !(Vector PartitionField)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data PartitionField = PartitionField
  { pfSourceIds :: !(Vector Int)
  {- ^ The source columns the transform consumes. In the V1 / V2
  spec exactly one source column is allowed and it's encoded in
  the metadata as @source-id@; in V3 the multi-arg variants of
  @bucket[N]@ and @truncate[W]@ accept several source columns and
  it's encoded as @source-ids@. This single field models both.
  -}
  , pfFieldId :: {-# UNPACK #-} !Int
  , pfName :: !Text
  , pfTransform :: !Transform
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | The first source id of a partition field. This is the unique
source column for V1/V2 fields and the first source column for V3
multi-arg fields (which is the column most projection / pruning
machinery still keys off of).
-}
pfPrimarySourceId :: PartitionField -> Int
pfPrimarySourceId pf = case V.length (pfSourceIds pf) of
  0 -> error "Iceberg.Types.pfPrimarySourceId: empty pfSourceIds"
  _ -> V.unsafeIndex (pfSourceIds pf) 0
{-# INLINE pfPrimarySourceId #-}


data Transform
  = Identity
  | Bucket {-# UNPACK #-} !Int
  | Truncate {-# UNPACK #-} !Int
  | Year
  | Month
  | Day
  | Hour
  | Void
  | {- | Forward-compat: transform whose name is recognised at parse time
    but is not implemented for evaluation.
    -}
    UnknownTransform !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data SortOrder = SortOrder
  { soOrderId :: {-# UNPACK #-} !Int
  , soFields :: !(Vector SortField)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data SortField = SortField
  { sortSourceId :: {-# UNPACK #-} !Int
  , sortTransform :: !Transform
  , sortDirection :: !SortDirection
  , sortNullOrder :: !NullOrder
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data SortDirection = Asc | Desc
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data NullOrder = NullsFirst | NullsLast
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data SnapshotLogEntry = SnapshotLogEntry
  { sleTimestampMs :: {-# UNPACK #-} !Int64
  , sleSnapshotId :: {-# UNPACK #-} !Int64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data MetadataLogEntry = MetadataLogEntry
  { mleTimestampMs :: {-# UNPACK #-} !Int64
  , mleMetadataFile :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Reference to a Puffin statistics file.
data StatisticsFile = StatisticsFile
  { sfsSnapshotId :: {-# UNPACK #-} !Int64
  , sfsStatPath :: !Text
  , sfsFileSize :: {-# UNPACK #-} !Int64
  , sfsFooterSize :: {-# UNPACK #-} !Int64
  , sfsKeyMetadata :: !(Maybe Text)
  , sfsBlobMetadata :: !(Vector BlobMetadata)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Per-blob metadata in a Puffin statistics file (e.g. NDV sketches).
data BlobMetadata = BlobMetadata
  { bmType :: !Text
  , bmSnapshotId :: {-# UNPACK #-} !Int64
  , bmSequenceNumber :: {-# UNPACK #-} !Int64
  , bmFields :: !(Vector Int)
  , bmProperties :: !(Map Text Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Reference to a partition statistics file.
data PartitionStatisticsFile = PartitionStatisticsFile
  { psfSnapshotId :: {-# UNPACK #-} !Int64
  , psfPath :: !Text
  , psfFileSize :: {-# UNPACK #-} !Int64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | Full @data_file@ record from the manifest spec. This is the value read out
of an Iceberg manifest file in addition to the entry envelope.
-}
data DataFile = DataFile
  { dataFileContent :: !ManifestContent
  -- ^ V2+: 0 = data, 1 = position deletes, 2 = equality deletes.
  , dataFileFilePath :: !Text
  , dataFileFileFormat :: !FileFormat
  , dataFilePartition :: !(Vector (Maybe Value))
  , dataFileRecordCount :: {-# UNPACK #-} !Int64
  , dataFileFileSize :: {-# UNPACK #-} !Int64
  , dataFileColumnSizes :: !(Map Int Int64)
  , dataFileValueCounts :: !(Map Int Int64)
  , dataFileNullValueCounts :: !(Map Int Int64)
  , dataFileNanValueCounts :: !(Map Int Int64)
  -- ^ Optional in the spec; Java/PyIceberg both surface it.
  , dataFileLowerBounds :: !(Map Int ByteString)
  , dataFileUpperBounds :: !(Map Int ByteString)
  , dataFileKeyMetadata :: !(Maybe ByteString)
  , dataFileSplitOffsets :: !(Vector Int64)
  , dataFileEqualityIds :: !(Vector Int)
  -- ^ Field ids the file uses as equality predicates (delete files only).
  , dataFileSortOrderId :: !(Maybe Int)
  , dataFileFirstRowId :: !(Maybe Int64)
  -- ^ V3: lineage; first row id for new rows in this file.
  , dataFileReferencedDataFile :: !(Maybe Text)
  -- ^ V3: deletion-vector blob references this data file path.
  , dataFileContentOffset :: !(Maybe Int64)
  -- ^ V3 deletion-vector: byte offset within the puffin file.
  , dataFileContentSize :: !(Maybe Int64)
  -- ^ V3 deletion-vector: byte length of the blob within the puffin file.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ManifestEntry = ManifestEntry
  { meStatus :: !ManifestStatus
  , meSnapshotId :: !(Maybe Int64)
  , meSequenceNumber :: !(Maybe Int64)
  , meFileSequenceNumber :: !(Maybe Int64)
  , meFilePath :: !Text
  , meFileFormat :: !FileFormat
  , mePartition :: !(Vector (Maybe Value))
  , meRecordCount :: {-# UNPACK #-} !Int64
  , meFileSizeBytes :: {-# UNPACK #-} !Int64
  , meDataFile :: !(Maybe DataFile)
  {- ^ Full data-file record when the manifest contains the optional
  statistics fields, deletion-vector pointers, etc.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ManifestStatus = Existing | Added | Deleted
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data FileFormat = AvroFormat | ParquetFormat | OrcFormat
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


{- | Per-partition-field summary inside a manifest list entry. Mirrors the
@field_summary@ Avro record described by the Iceberg spec: a "may contain
null" bit, a "may contain NaN" bit (V2+), and serialised lower\/upper
bound bytes that match the table's binary single-value serialisation.
-}
data FieldSummary = FieldSummary
  { fsContainsNull :: !Bool
  , fsContainsNan :: !(Maybe Bool)
  , fsLowerBound :: !(Maybe ByteString)
  , fsUpperBound :: !(Maybe ByteString)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ManifestFile = ManifestFile
  { mfPath :: !Text
  , mfLength :: {-# UNPACK #-} !Int64
  , mfPartitionSpecId :: {-# UNPACK #-} !Int
  , mfContent :: !ManifestContent
  , mfSequenceNumber :: {-# UNPACK #-} !Int64
  , mfMinSequenceNumber :: {-# UNPACK #-} !Int64
  , mfAddedSnapshotId :: {-# UNPACK #-} !Int64
  , mfAddedDataFilesCount :: !(Maybe Int)
  , mfExistingDataFilesCount :: !(Maybe Int)
  , mfDeletedDataFilesCount :: !(Maybe Int)
  , mfAddedRowsCount :: !(Maybe Int64)
  , mfExistingRowsCount :: !(Maybe Int64)
  , mfDeletedRowsCount :: !(Maybe Int64)
  , mfPartitions :: !(Vector FieldSummary)
  -- ^ Optional per-spec partition field summaries used for manifest pruning.
  , mfKeyMetadata :: !(Maybe ByteString)
  -- ^ V3: encryption key metadata for the manifest.
  , mfFirstRowId :: !(Maybe Int64)
  -- ^ V3: row-lineage starting id for new rows in this manifest.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ManifestContent
  = -- | data files (content = 0)
    DataContent
  | -- | delete files of either type (content = 1)
    DeletesContent
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data DeleteFileContent = PositionDeletes | EqualityDeletes
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data DeleteFile = DeleteFile
  { dfFilePath :: !Text
  , dfFileFormat :: !FileFormat
  , dfContent :: !DeleteFileContent
  , dfRecordCount :: {-# UNPACK #-} !Int64
  , dfFileSizeInBytes :: {-# UNPACK #-} !Int64
  , dfEqualityFieldIds :: !(Vector Int32)
  , dfPartition :: !(Map Text Value)
  , dfSequenceNumber :: !(Maybe Int64)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data PositionDelete = PositionDelete
  { pdFilePath :: !Text
  , pdPosition :: {-# UNPACK #-} !Int64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data EqualityDeleteSpec = EqualityDeleteSpec
  { edsFieldIds :: !(Vector Int32)
  , edsSchema :: !Schema
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data SnapshotRef = SnapshotRef
  { srSnapshotId :: {-# UNPACK #-} !Int64
  , srType :: !Text
  , srMaxRefAgeMs :: !(Maybe Int64)
  , srMaxSnapshotAgeMs :: !(Maybe Int64)
  , srMinSnapshotsToKeep :: !(Maybe Int32)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Iceberg view metadata as defined by the View Spec.
data ViewMetadata = ViewMetadata
  { vmViewUuid :: !Text
  , vmFormatVersion :: {-# UNPACK #-} !Int
  , vmLocation :: !Text
  , vmSchemas :: !(Vector Schema)
  , vmCurrentVersionId :: {-# UNPACK #-} !Int
  , vmVersions :: !(Vector ViewVersion)
  , vmVersionLog :: !(Vector ViewHistoryEntry)
  , vmProperties :: !(Map Text Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ViewVersion = ViewVersion
  { vvVersionId :: {-# UNPACK #-} !Int
  , vvTimestampMs :: {-# UNPACK #-} !Int64
  , vvSchemaId :: {-# UNPACK #-} !Int
  , vvSummary :: !(Map Text Text)
  , vvRepresentations :: !(Vector ViewRepresentation)
  , vvDefaultCatalog :: !(Maybe Text)
  , vvDefaultNamespace :: !(Vector Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | A single representation of a view (currently only "sql" is widely used).
data ViewRepresentation
  = SqlViewRepresentation !Text !Text
  | UnknownViewRepresentation !Text !(Map Text Text)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ViewHistoryEntry = ViewHistoryEntry
  { vheTimestampMs :: {-# UNPACK #-} !Int64
  , vheVersionId :: {-# UNPACK #-} !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | Iceberg @schema.name-mapping.default@ entry.

Each 'MappedField' may have multiple @names@, an optional Iceberg field id,
and nested children (used for structs, lists, and maps).
-}
data MappedField = MappedField
  { mfName :: !(Vector Text)
  -- ^ One or more names that map to this field id.
  , mfFieldId :: !(Maybe Int)
  , mfFields :: !NameMapping
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | A name mapping is just an ordered list of mapped fields. Empty maps to
"no entries" rather than \"null\" so that it round-trips cleanly to JSON.
-}
newtype NameMapping = NameMapping {unNameMapping :: Vector MappedField}
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid, NFData)
