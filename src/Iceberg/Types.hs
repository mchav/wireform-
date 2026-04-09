-- | Apache Iceberg table metadata types.
--
-- Iceberg is a table format whose metadata is stored as JSON (table metadata)
-- and Avro (manifest files/manifest lists). This module defines the core
-- schema types and metadata structures per the Iceberg specification.
module Iceberg.Types
  ( -- * Table metadata
    TableMetadata(..)
  , Schema(..)
  , StructField(..)
  , IcebergType(..)
  , Snapshot(..)
  , PartitionSpec(..)
  , PartitionField(..)
  , Transform(..)
  , SortOrder(..)
  , SortField(..)
  , SortDirection(..)
  , NullOrder(..)
  , SnapshotLogEntry(..)
    -- * Manifest types
  , ManifestEntry(..)
  , ManifestStatus(..)
  , FileFormat(..)
  , ManifestFile(..)
  , ManifestContent(..)
    -- * Delete file types
  , DeleteFileContent(..)
  , DeleteFile(..)
  , PositionDelete(..)
  , EqualityDeleteSpec(..)
    -- * Snapshot references
  , SnapshotRef(..)
    -- * Partition values (re-export)
  , Value
  ) where

import Control.DeepSeq (NFData)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import qualified Avro.Value as Avro

-- | Re-export Avro.Value for use as Iceberg partition values.
type Value = Avro.Value

data TableMetadata = TableMetadata
  { tmFormatVersion      :: {-# UNPACK #-} !Int
  , tmTableUuid          :: !Text
  , tmLocation           :: !Text
  , tmLastSequenceNumber :: {-# UNPACK #-} !Int64
  , tmLastUpdatedMs      :: {-# UNPACK #-} !Int64
  , tmLastColumnId       :: {-# UNPACK #-} !Int
  , tmCurrentSchemaId    :: {-# UNPACK #-} !Int
  , tmSchemas            :: !(Vector Schema)
  , tmCurrentSnapshotId  :: !(Maybe Int64)
  , tmSnapshots          :: !(Vector Snapshot)
  , tmPartitionSpecs     :: !(Vector PartitionSpec)
  , tmDefaultSpecId      :: {-# UNPACK #-} !Int
  , tmSortOrders         :: !(Vector SortOrder)
  , tmDefaultSortOrderId :: {-# UNPACK #-} !Int
  , tmProperties         :: !(Map Text Text)
  , tmSnapshotLog        :: !(Vector SnapshotLogEntry)
  , tmSnapshotRefs       :: !(Map Text SnapshotRef)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data Schema = Schema
  { schemaId     :: {-# UNPACK #-} !Int
  , schemaFields :: !(Vector StructField)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data StructField = StructField
  { sfId       :: {-# UNPACK #-} !Int
  , sfName     :: !Text
  , sfRequired :: !Bool
  , sfType     :: !IcebergType
  , sfDoc      :: !(Maybe Text)
  } deriving stock (Show, Eq, Generic)
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
  | TString
  | TUuid
  | TFixed {-# UNPACK #-} !Int
  | TBinary
  | TDecimal {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | TStruct !(Vector StructField)
  | TList {-# UNPACK #-} !Int !IcebergType
  | TMap {-# UNPACK #-} !Int !IcebergType {-# UNPACK #-} !Int !IcebergType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data Snapshot = Snapshot
  { snapId             :: {-# UNPACK #-} !Int64
  , snapParentId       :: !(Maybe Int64)
  , snapSequenceNumber :: {-# UNPACK #-} !Int64
  , snapTimestampMs    :: {-# UNPACK #-} !Int64
  , snapManifestList   :: !Text
  , snapSummary        :: !(Map Text Text)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data PartitionSpec = PartitionSpec
  { psSpecId :: {-# UNPACK #-} !Int
  , psFields :: !(Vector PartitionField)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data PartitionField = PartitionField
  { pfSourceId  :: {-# UNPACK #-} !Int
  , pfFieldId   :: {-# UNPACK #-} !Int
  , pfName      :: !Text
  , pfTransform :: !Transform
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data Transform
  = Identity
  | Bucket {-# UNPACK #-} !Int
  | Truncate {-# UNPACK #-} !Int
  | Year
  | Month
  | Day
  | Hour
  | Void
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data SortOrder = SortOrder
  { soOrderId :: {-# UNPACK #-} !Int
  , soFields  :: !(Vector SortField)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data SortField = SortField
  { sortSourceId  :: {-# UNPACK #-} !Int
  , sortTransform :: !Transform
  , sortDirection :: !SortDirection
  , sortNullOrder :: !NullOrder
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data SortDirection = Asc | Desc
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data NullOrder = NullsFirst | NullsLast
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data SnapshotLogEntry = SnapshotLogEntry
  { sleTimestampMs :: {-# UNPACK #-} !Int64
  , sleSnapshotId  :: {-# UNPACK #-} !Int64
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ManifestEntry = ManifestEntry
  { meStatus             :: !ManifestStatus
  , meSnapshotId         :: !(Maybe Int64)
  , meSequenceNumber     :: !(Maybe Int64)
  , meFileSequenceNumber :: !(Maybe Int64)
  , meFilePath           :: !Text
  , meFileFormat         :: !FileFormat
  , mePartition          :: !(Vector (Maybe Value))
  , meRecordCount        :: {-# UNPACK #-} !Int64
  , meFileSizeBytes      :: {-# UNPACK #-} !Int64
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ManifestStatus = Existing | Added | Deleted
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data FileFormat = AvroFormat | ParquetFormat | OrcFormat
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data ManifestFile = ManifestFile
  { mfPath                   :: !Text
  , mfLength                 :: {-# UNPACK #-} !Int64
  , mfPartitionSpecId        :: {-# UNPACK #-} !Int
  , mfContent                :: !ManifestContent
  , mfSequenceNumber         :: {-# UNPACK #-} !Int64
  , mfMinSequenceNumber      :: {-# UNPACK #-} !Int64
  , mfAddedSnapshotId        :: {-# UNPACK #-} !Int64
  , mfAddedDataFilesCount    :: !(Maybe Int)
  , mfExistingDataFilesCount :: !(Maybe Int)
  , mfDeletedDataFilesCount  :: !(Maybe Int)
  , mfAddedRowsCount         :: !(Maybe Int64)
  , mfExistingRowsCount      :: !(Maybe Int64)
  , mfDeletedRowsCount       :: !(Maybe Int64)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ManifestContent = DataContent | DeletesContent
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data DeleteFileContent = PositionDeletes | EqualityDeletes
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data DeleteFile = DeleteFile
  { dfFilePath        :: !Text
  , dfFileFormat      :: !FileFormat
  , dfContent         :: !DeleteFileContent
  , dfRecordCount     :: {-# UNPACK #-} !Int64
  , dfFileSizeInBytes :: {-# UNPACK #-} !Int64
  , dfEqualityFieldIds :: !(Vector Int32)
  , dfPartition       :: !(Map Text Value)
  , dfSequenceNumber  :: !(Maybe Int64)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data PositionDelete = PositionDelete
  { pdFilePath :: !Text
  , pdPosition :: {-# UNPACK #-} !Int64
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data EqualityDeleteSpec = EqualityDeleteSpec
  { edsFieldIds :: !(Vector Int32)
  , edsSchema   :: !Schema
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data SnapshotRef = SnapshotRef
  { srSnapshotId         :: {-# UNPACK #-} !Int64
  , srType               :: !Text
  , srMaxRefAgeMs        :: !(Maybe Int64)
  , srMaxSnapshotAgeMs   :: !(Maybe Int64)
  , srMinSnapshotsToKeep :: !(Maybe Int32)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)
