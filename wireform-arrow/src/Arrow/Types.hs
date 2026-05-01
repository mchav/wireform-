-- | Apache Arrow IPC metadata types.
--
-- Arrow IPC uses FlatBuffers for metadata. These types mirror the Arrow
-- Schema.fbs definitions for schema, field, type, and record batch metadata.
module Arrow.Types
  ( Schema(..)
  , Field(..)
  , DictionaryEncoding(..)
  , ArrowType(..)
  , Endianness(..)
  , Precision(..)
  , DateUnit(..)
  , TimeUnit(..)
  , IntervalUnit(..)
  , UnionMode(..)
  , Message(..)
  , RecordBatchDef(..)
  , BodyCompressionCodec(..)
  , FieldNode(..)
  , Buffer(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Endianness = Little | Big
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data Precision = Half | Single | DoublePrecision
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data DateUnit = DateDay | DateMillisecond
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data TimeUnit = Second | Millisecond | Microsecond | Nanosecond
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data IntervalUnit = YearMonth | DayTime | MonthDayNano
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data UnionMode = Sparse | Dense
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data ArrowType
  = ANull
  | AInt !Int !Bool
  | AFloatingPoint !Precision
  | ABinary
  | AUtf8
  | ABool
  | ADecimal !Int !Int
    -- ^ 128-bit 'Decimal' (precision, scale). Kept as @ADecimal@
    -- for source compatibility with the original schema; the
    -- 256-bit variant is 'ADecimal256'.
  | ADecimal256 !Int !Int
    -- ^ 256-bit 'Decimal' (precision, scale). Arrow's on-wire
    -- @Decimal@ type carries a @bitWidth@ field; we keep the two
    -- widths as distinct 'ArrowType' constructors so the reader
    -- can dispatch to the right 'ColumnArray' variant.
  | ADate !DateUnit
  | ATime !TimeUnit !Int
  | ATimestamp !TimeUnit !(Maybe Text)
  | AInterval !IntervalUnit
  | AList
  | AStruct
  | AUnion !UnionMode !(Vector Int32)
  | AFixedSizeBinary !Int
  | AFixedSizeList !Int
  | AMap !Bool
  | ADuration !TimeUnit
  | ALargeBinary
  | ALargeUtf8
  | ALargeList
  -- The remaining constructors are post-V5 schema additions
  -- (Arrow format version >= 1.4). The metadata writer in
  -- "Arrow.FlatBufferIPC" understands them; a corresponding column
  -- materializer / writer in "Arrow.Column" / "Arrow.Write" is not
  -- yet wired up — the schema slot is enough for round-tripping
  -- type information from external readers.
  | ARunEndEncoded
  | ABinaryView
  | AUtf8View
  | AListView
  | ALargeListView
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data Field = Field
  { fieldName     :: !Text
  , fieldNullable :: !Bool
  , fieldType     :: !ArrowType
  , fieldChildren :: !(Vector Field)
  , fieldDictionary :: !(Maybe DictionaryEncoding)
    -- ^ When non-'Nothing', this field's @fieldType@ refers to the
    -- /index/ type and the actual values live in a separate
    -- 'DictionaryBatch' message keyed by 'deId'.
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Per @format/Schema.fbs@:
--
-- @
-- table DictionaryEncoding {
--   id: long;
--   indexType: Int;
--   isOrdered: bool;
--   dictionaryKind: DictionaryKind;
-- }
-- @
data DictionaryEncoding = DictionaryEncoding
  { deId        :: !Int64
  , deIndexType :: !ArrowType   -- always an AInt; default Int32 signed
  , deIsOrdered :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data Schema = Schema
  { arrowFields     :: !(Vector Field)
  , arrowEndianness :: !Endianness
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data FieldNode = FieldNode
  { fnLength    :: !Int64
  , fnNullCount :: !Int64
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data Buffer = Buffer
  { bufOffset :: !Int64
  , bufLength :: !Int64
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data RecordBatchDef = RecordBatchDef
  { rbLength  :: !Int64
  , rbNodes   :: !(Vector FieldNode)
  , rbBuffers :: !(Vector Buffer)
  , rbVariadicBufferCounts :: !(Vector Int64)
    -- ^ Per Arrow @format/Message.fbs@: when the schema contains
    -- @Utf8View@ or @BinaryView@ fields each such field has a
    -- variable number of additional data buffers for out-of-line
    -- string payloads. The vector lists, in pre-order schema
    -- traversal order, the number of variadic data buffers per
    -- view column. Empty for schemas without view types.
  , rbBodyCompression :: !(Maybe BodyCompressionCodec)
    -- ^ Per Arrow @format/Message.fbs@'s 'BodyCompression' table.
    -- When 'Just', each buffer in the body is wrapped in an
    -- @<i64 uncompressedLength><compressed bytes>@ envelope using
    -- the named codec; readers see the original layout once
    -- decompressed. 'Nothing' = uncompressed buffers (the wire
    -- default).
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Body-compression codecs supported by Arrow IPC's
-- 'BodyCompression' table (per @format/Message.fbs@).
data BodyCompressionCodec
  = LZ4Frame
    -- ^ LZ4 frame format (@CompressionType = LZ4_FRAME = 0@).
    -- Most widely supported; the default for pyarrow when
    -- compression is enabled.
  | BodyZstd
    -- ^ Zstandard (@CompressionType = ZSTD = 1@).
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data Message
  = SchemaMessage !Schema
  | DictionaryBatch
  | RecordBatch !RecordBatchDef
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
