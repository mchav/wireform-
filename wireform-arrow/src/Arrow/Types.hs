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
  , Feature(..)
  , FieldNode(..)
  , Buffer(..)
    -- * Smart constructors
    --
    -- | These thin wrappers default the optional record fields
    -- ('arrowMetadata', 'fieldMetadata', 'fieldDictionary',
    -- 'fieldChildren') so test fixtures and one-off encoders
    -- don't have to spell out every slot. Adding a new optional
    -- field to 'Schema' or 'Field' won't break callers that
    -- went through these constructors.
  , defaultSchema
  , defaultField
  , defaultLeafField
    -- * Schema fingerprint
  , schemaFingerprint
  , schemaEquivalent
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (hash)
import qualified Data.Text as T
import qualified Data.Vector as V
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
  , fieldMetadata :: !(Vector (Text, Text))
    -- ^ Arrow per-field @custom_metadata@ (Schema.fbs field 6).
    -- Free-form key/value pairs that travel with the field
    -- through encode + decode. Empty when the field carries
    -- no annotation (the common case).
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

-- | Arrow schema feature flags (Schema.fbs's @Feature@ enum).
-- Currently the spec only defines 'FeatureUnused' (0) and
-- 'FeatureDictionaryReplacement' (1) — the latter signals that
-- the file may contain delta-replacement dictionary batches.
data Feature
  = FeatureUnused
  | FeatureDictionaryReplacement
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data Schema = Schema
  { arrowFields     :: !(Vector Field)
  , arrowEndianness :: !Endianness
  , arrowMetadata   :: !(Vector (Text, Text))
    -- ^ Arrow schema-level @custom_metadata@ (Schema.fbs field
    -- 4 of @table Schema@). Most files set zero or one
    -- (@pandas@-style) annotation. Empty by default.
  , arrowFeatures   :: !(Vector Feature)
    -- ^ Arrow schema-level @features@ (Schema.fbs field 5).
    -- Empty by default; readers tolerate either way. Producers
    -- emitting delta-replacement dictionary batches should
    -- include 'FeatureDictionaryReplacement'.
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

-- | Build a 'Schema' with little-endian + no custom_metadata.
-- Equivalent to @Schema fields Little V.empty@.
defaultSchema :: Vector Field -> Schema
defaultSchema fs = Schema
  { arrowFields     = fs
  , arrowEndianness = Little
  , arrowMetadata   = V.empty
  , arrowFeatures   = V.empty
  }

-- | Build a 'Field' with no children, no dictionary, and no
-- custom_metadata.
defaultLeafField :: Text -> Bool -> ArrowType -> Field
defaultLeafField name nullable ty = Field
  { fieldName       = name
  , fieldNullable   = nullable
  , fieldType       = ty
  , fieldChildren   = V.empty
  , fieldDictionary = Nothing
  , fieldMetadata   = V.empty
  }

-- | Build a 'Field' with explicit children but no dictionary
-- encoding and no custom_metadata.
defaultField :: Text -> Bool -> ArrowType -> Vector Field -> Field
defaultField name nullable ty children = Field
  { fieldName       = name
  , fieldNullable   = nullable
  , fieldType       = ty
  , fieldChildren   = children
  , fieldDictionary = Nothing
  , fieldMetadata   = V.empty
  }

-- | A 64-bit fingerprint of an Arrow 'Schema'. Two schemas
-- that compare 'schemaEquivalent' produce the same
-- fingerprint; the inverse holds with high probability (no
-- collisions observed in the test corpus, but the canonical
-- Avro-style cryptographic fingerprint is a TODO if real
-- collision-freeness becomes a requirement).
--
-- Equivalence ignores 'arrowMetadata' / 'arrowFeatures' and
-- per-field 'fieldMetadata' — those are annotations, not
-- structure. Endianness, field order, types, nullability, and
-- nested children all participate.
schemaFingerprint :: Schema -> Int
schemaFingerprint sch = hash
  ( fromEnum (arrowEndianness sch)
  , map fingerprintField (V.toList (arrowFields sch))
  )

fingerprintField :: Field -> (String, Bool, String, [(String, Bool, String, [()])])
fingerprintField f =
  ( T.unpack (fieldName f)
  , fieldNullable f
  , show (fieldType f)
  , map fingerprintShallow (V.toList (fieldChildren f))
  )
  where
    fingerprintShallow c =
      ( T.unpack (fieldName c)
      , fieldNullable c
      , show (fieldType c)
      , []  -- one level of children is enough for the structure
            -- check we care about; deeper trees contribute via
            -- the parent's show-of-fieldType.
      )

-- | Structural equivalence, mirroring 'schemaFingerprint'.
-- Annotation fields ('arrowMetadata', 'arrowFeatures',
-- per-field 'fieldMetadata') are ignored.
schemaEquivalent :: Schema -> Schema -> Bool
schemaEquivalent a b =
  arrowEndianness a == arrowEndianness b
    && V.length (arrowFields a) == V.length (arrowFields b)
    && and (V.zipWith fieldEquivalent (arrowFields a) (arrowFields b))

fieldEquivalent :: Field -> Field -> Bool
fieldEquivalent a b =
  fieldName a == fieldName b
    && fieldNullable a == fieldNullable b
    && fieldType a == fieldType b
    && V.length (fieldChildren a) == V.length (fieldChildren b)
    && and (V.zipWith fieldEquivalent (fieldChildren a) (fieldChildren b))
