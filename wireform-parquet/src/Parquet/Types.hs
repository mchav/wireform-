-- | Apache Parquet file metadata types.
--
-- Parquet is a columnar storage format. The metadata (file footer, schema,
-- row groups, column chunks) is Thrift Compact Protocol encoded. These types
-- mirror the Parquet spec's Thrift definitions.
module Parquet.Types
  ( FileMetadata(..)
  , SchemaElement(..)
  , RowGroup(..)
  , ColumnChunk(..)
  , ColumnMetadata(..)
  , ParquetType(..)
  , Repetition(..)
  , Encoding(..)
  , Compression(..)
  , ConvertedType(..)
  , LogicalType(..)
  , Statistics(..)
  , PageLocation(..)
  , OffsetIndex(..)
  , ColumnIndex(..)
  , BoundaryOrder(..)
  , parquetTypeToInt
  , intToParquetType
  , boundaryOrderToInt
  , intToBoundaryOrder
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data ParquetType
  = PTBoolean
  | PTInt32
  | PTInt64
  | PTInt96
  | PTFloat
  | PTDouble
  | PTByteArray
  | PTFixedLenByteArray
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

parquetTypeToInt :: ParquetType -> Int32
parquetTypeToInt = \case
  PTBoolean          -> 0
  PTInt32            -> 1
  PTInt64            -> 2
  PTInt96            -> 3
  PTFloat            -> 4
  PTDouble           -> 5
  PTByteArray        -> 6
  PTFixedLenByteArray -> 7

intToParquetType :: Int32 -> Maybe ParquetType
intToParquetType = \case
  0 -> Just PTBoolean
  1 -> Just PTInt32
  2 -> Just PTInt64
  3 -> Just PTInt96
  4 -> Just PTFloat
  5 -> Just PTDouble
  6 -> Just PTByteArray
  7 -> Just PTFixedLenByteArray
  _ -> Nothing

data Repetition = Required | Optional | Repeated
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data Encoding
  = Plain
  | PlainDictionary
  | RLE
  | BitPacked
  | DeltaBinaryPacked
  | DeltaLengthByteArray
  | DeltaByteArray
  | RLEDictionary
  | ByteStreamSplit
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data Compression
  = Uncompressed
  | Snappy
  | GZip
  | LZO
  | Brotli
  | LZ4
  | ZSTD
  | LZ4Raw
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data ConvertedType
  = CTUtf8
  | CTMap
  | CTMapKeyValue
  | CTList
  | CTEnum
  | CTDecimal
  | CTDate
  | CTTimeMillis
  | CTTimeMicros
  | CTTimestampMillis
  | CTTimestampMicros
  | CTUInt8
  | CTUInt16
  | CTUInt32
  | CTUInt64
  | CTInt8
  | CTInt16
  | CTInt32
  | CTInt64
  | CTJson
  | CTBson
  | CTInterval
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data LogicalType
  = LTString
  | LTMap
  | LTList
  | LTEnum
  | LTDecimal !Int32 !Int32
  | LTDate
  | LTTime !Bool !Bool
  | LTTimestamp !Bool !Bool
  | LTInteger !Int32 !Bool
  | LTNull
  | LTJson
  | LTBson
  | LTUUID
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data SchemaElement = SchemaElement
  { seName        :: !Text
  , seRepetition  :: !(Maybe Repetition)
  , seType        :: !(Maybe ParquetType)
  , seNumChildren :: !(Maybe Int32)
  , seConvertedType :: !(Maybe ConvertedType)
  , seLogicalType :: !(Maybe LogicalType)
  , seFieldId     :: !(Maybe Int32)
    -- ^ Iceberg's identifier for this leaf column. Required for any
    -- Parquet file that participates in an Iceberg table — readers
    -- match @field_id@, not @name@. Encoded into Thrift field 9 of
    -- @parquet.thrift::SchemaElement@ (field 8 is @precision@, field 7
    -- is @scale@).
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ColumnMetadata = ColumnMetadata
  { cmType                  :: !ParquetType
  , cmEncodings             :: !(Vector Encoding)
  , cmPathInSchema          :: !(Vector Text)
  , cmCodec                 :: !Compression
  , cmNumValues             :: !Int64
  , cmTotalUncompressedSize :: !Int64
  , cmTotalCompressedSize   :: !Int64
  , cmDataPageOffset        :: !Int64
  , cmStatistics            :: !(Maybe Statistics)
  -- | Byte offset from beginning of file to the bloom filter for this
  -- column chunk, if a bloom filter is written. Field 14.
  , cmBloomFilterOffset     :: !(Maybe Int64)
  -- | Length of the bloom filter (header + bitset) in bytes. Field 15
  -- (added in parquet-format 2.10).
  , cmBloomFilterLength     :: !(Maybe Int32)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ColumnChunk = ColumnChunk
  { ccFilePath          :: !(Maybe Text)
  , ccFileOffset        :: !Int64
  , ccMetadata          :: !(Maybe ColumnMetadata)
  -- | Byte offset of this column chunk's 'OffsetIndex' (Thrift Compact),
  -- relative to the start of the file. Page-index spec, field 7.
  , ccOffsetIndexOffset :: !(Maybe Int64)
  -- | Length in bytes of the serialized 'OffsetIndex'. Field 8.
  , ccOffsetIndexLength :: !(Maybe Int32)
  -- | Byte offset of this column chunk's 'ColumnIndex' (Thrift Compact),
  -- relative to the start of the file. Field 9.
  , ccColumnIndexOffset :: !(Maybe Int64)
  -- | Length in bytes of the serialized 'ColumnIndex'. Field 10.
  , ccColumnIndexLength :: !(Maybe Int32)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data RowGroup = RowGroup
  { rgColumns       :: !(Vector ColumnChunk)
  , rgTotalByteSize :: !Int64
  , rgNumRows       :: !Int64
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data FileMetadata = FileMetadata
  { fmVersion   :: !Int32
  , fmSchema    :: !(Vector SchemaElement)
  , fmNumRows   :: !Int64
  , fmRowGroups :: !(Vector RowGroup)
  , fmCreatedBy :: !(Maybe Text)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data Statistics = Statistics
  { statMin           :: !(Maybe ByteString)
  , statMax           :: !(Maybe ByteString)
  , statNullCount     :: !(Maybe Int64)
  , statDistinctCount :: !(Maybe Int64)
  , statMinValue      :: !(Maybe ByteString)
  , statMaxValue      :: !(Maybe ByteString)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data PageLocation = PageLocation
  { plOffset             :: !Int64
  , plCompressedPageSize :: !Int32
  , plFirstRowIndex      :: !Int64
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data OffsetIndex = OffsetIndex
  { oiPageLocations              :: !(Vector PageLocation)
  -- | Per-page unencoded byte counts for BYTE_ARRAY columns (parquet-format
  -- 2.11+). Same length as 'oiPageLocations' when present; @Nothing@ for
  -- non-BYTE_ARRAY columns or older writers.
  , oiUnencodedByteArrayDataBytes :: !(Maybe (Vector Int64))
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Page-level boundary ordering for a column index's @min_values@ and
-- @max_values@. See @parquet.thrift@ enum @BoundaryOrder@.
data BoundaryOrder
  = OrderUnordered
  | OrderAscending
  | OrderDescending
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

boundaryOrderToInt :: BoundaryOrder -> Int32
boundaryOrderToInt = \case
  OrderUnordered -> 0
  OrderAscending -> 1
  OrderDescending -> 2

intToBoundaryOrder :: Int32 -> Maybe BoundaryOrder
intToBoundaryOrder = \case
  0 -> Just OrderUnordered
  1 -> Just OrderAscending
  2 -> Just OrderDescending
  _ -> Nothing

data ColumnIndex = ColumnIndex
  { ciNullPages       :: !(Vector Bool)
  , ciMinValues       :: !(Vector ByteString)
  , ciMaxValues       :: !(Vector ByteString)
  , ciBoundaryOrder   :: !BoundaryOrder
  , ciNullCounts      :: !(Maybe (Vector Int64))
  -- | Per-page repetition level histograms (flattened: @max_rep + 1@ values
  -- per page). Optional, parquet-format 2.11+.
  , ciRepetitionLevelHistograms :: !(Maybe (Vector Int64))
  -- | Per-page definition level histograms (flattened: @max_def + 1@ values
  -- per page). Optional, parquet-format 2.11+.
  , ciDefinitionLevelHistograms :: !(Maybe (Vector Int64))
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)
