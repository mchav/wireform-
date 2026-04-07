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
  , parquetTypeToInt
  , intToParquetType
  ) where

import Control.DeepSeq (NFData)
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
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ColumnChunk = ColumnChunk
  { ccFilePath   :: !(Maybe Text)
  , ccFileOffset :: !Int64
  , ccMetadata   :: !(Maybe ColumnMetadata)
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
