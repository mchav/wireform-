{- | Apache ORC file metadata types.

ORC (Optimized Row Columnar) uses Protocol Buffers for its metadata.
These types model the ORC file footer, stripe information, column types,
and column statistics as defined in the ORC specification.
-}
module ORC.Types (
  ORCFooter (..),
  FooterEncryption (..),
  StripeInformation (..),
  ORCType (..),
  TypeKind (..),
  ColumnStatistics (..),
  IntegerStatistics (..),
  DoubleStatistics (..),
  StringStatistics (..),
  BinaryStatistics (..),
  DateStatistics (..),
  TimestampStatistics (..),
  DecimalStatistics (..),
  BucketStatistics (..),
  StatsKind (..),
  CompressionKind (..),
  typeKindToInt,
  intToTypeKind,
  compressionFromInt,
) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)


data TypeKind
  = TKBoolean
  | TKByte
  | TKShort
  | TKInt
  | TKLong
  | TKFloat
  | TKDouble
  | TKString
  | TKBinary
  | TKTimestamp
  | TKList
  | TKMap
  | TKStruct
  | TKUnion
  | TKDecimal
  | TKDate
  | TKVarchar
  | TKChar
  | {- | Timezone-adjusted timestamp (ORC 1.6+, orc_proto.proto
    @Type.Kind = TIMESTAMP_INSTANT = 18@). Distinct from
    'TKTimestamp' which is in the writer's local time zone;
    this one is always UTC. Hive / Spark distinguish the two
    when emitting tz-adjusted columns.
    -}
    TKTimestampInstant
  deriving stock (Show, Eq, Enum, Bounded, Ord, Generic)
  deriving anyclass (NFData)


typeKindToInt :: TypeKind -> Int
typeKindToInt = \case
  TKBoolean -> 0
  TKByte -> 1
  TKShort -> 2
  TKInt -> 3
  TKLong -> 4
  TKFloat -> 5
  TKDouble -> 6
  TKString -> 7
  TKBinary -> 8
  TKTimestamp -> 9
  TKList -> 10
  TKMap -> 11
  TKStruct -> 12
  TKUnion -> 13
  TKDecimal -> 14
  TKDate -> 15
  TKVarchar -> 16
  TKChar -> 17
  TKTimestampInstant -> 18


intToTypeKind :: Int -> Maybe TypeKind
intToTypeKind = \case
  0 -> Just TKBoolean
  1 -> Just TKByte
  2 -> Just TKShort
  3 -> Just TKInt
  4 -> Just TKLong
  5 -> Just TKFloat
  6 -> Just TKDouble
  7 -> Just TKString
  8 -> Just TKBinary
  9 -> Just TKTimestamp
  10 -> Just TKList
  11 -> Just TKMap
  12 -> Just TKStruct
  13 -> Just TKUnion
  14 -> Just TKDecimal
  15 -> Just TKDate
  16 -> Just TKVarchar
  17 -> Just TKChar
  18 -> Just TKTimestampInstant
  _ -> Nothing


data CompressionKind
  = CompressionNone
  | CompressionZlib
  | CompressionSnappy
  | CompressionLZO
  | CompressionLZ4
  | CompressionZstd
  deriving stock (Show, Eq, Enum, Bounded, Ord, Generic)
  deriving anyclass (NFData)


compressionFromInt :: Word64 -> Maybe CompressionKind
compressionFromInt = \case
  0 -> Just CompressionNone
  1 -> Just CompressionZlib
  2 -> Just CompressionSnappy
  3 -> Just CompressionLZO
  4 -> Just CompressionLZ4
  5 -> Just CompressionZstd
  _ -> Nothing


data StripeInformation = StripeInformation
  { siOffset :: !Word64
  , siIndexLength :: !Word64
  , siDataLength :: !Word64
  , siFooterLength :: !Word64
  , siNumberOfRows :: !Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ORCType = ORCType
  { otKind :: !TypeKind
  , otSubtypes :: !(Vector Word32)
  , otFieldNames :: !(Vector Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | Per-column statistics — the @ColumnStatistics@ message in
@orc_proto.proto@.

The sub-statistics field ('csKind') carries the type-specific
min / max / sum triple. ORC writers populate exactly one of
IntegerStatistics / DoubleStatistics / etc. matching the
column's 'TypeKind'; the rest are 'Nothing'.
-}
data ColumnStatistics = ColumnStatistics
  { csNumberOfValues :: !(Maybe Word64)
  , csHasNull :: !(Maybe Bool)
  , csBytesOnDisk :: !(Maybe Word64)
  , csKind :: !(Maybe StatsKind)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | Tagged sub-statistics for one column. Exactly one variant is
populated per column; the constructor name picks which.
-}
data StatsKind
  = SkInt !IntegerStatistics
  | SkDouble !DoubleStatistics
  | SkString !StringStatistics
  | SkBucket !BucketStatistics
  | SkDecimal !DecimalStatistics
  | SkDate !DateStatistics
  | SkBinary !BinaryStatistics
  | SkTimestamp !TimestampStatistics
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data IntegerStatistics = IntegerStatistics
  { isMinimum :: !(Maybe Int64)
  , isMaximum :: !(Maybe Int64)
  , isSum :: !(Maybe Int64)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data DoubleStatistics = DoubleStatistics
  { dsMinimum :: !(Maybe Double)
  , dsMaximum :: !(Maybe Double)
  , dsSum :: !(Maybe Double)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data StringStatistics = StringStatistics
  { ssMinimum :: !(Maybe Text)
  , ssMaximum :: !(Maybe Text)
  , ssSum :: !(Maybe Int64)
  -- ^ Total UTF-8 bytes across all values.
  , ssLowerBound :: !(Maybe Text)
  , ssUpperBound :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data BinaryStatistics = BinaryStatistics
  { bsSum :: !(Maybe Int64)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Date min/max are days since the ORC epoch (1970-01-01).
data DateStatistics = DateStatistics
  { dateMinimum :: !(Maybe Int64)
  , dateMaximum :: !(Maybe Int64)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | Timestamps carry both UTC and writer-local-tz min/max because
ORC supports both flavours of TIMESTAMP column.
-}
data TimestampStatistics = TimestampStatistics
  { tsMinimum :: !(Maybe Int64)
  -- ^ ms since epoch (writer's local tz)
  , tsMaximum :: !(Maybe Int64)
  , tsMinimumUtc :: !(Maybe Int64)
  -- ^ ms since epoch UTC (TIMESTAMP_INSTANT)
  , tsMaximumUtc :: !(Maybe Int64)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | DECIMAL min/max/sum carried as the spec's textual
@\"<unscaled>E<scale>\"@-shape strings.
-}
data DecimalStatistics = DecimalStatistics
  { decMinimum :: !(Maybe Text)
  , decMaximum :: !(Maybe Text)
  , decSum :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | True/false counts for boolean columns.
newtype BucketStatistics = BucketStatistics
  { bucketCounts :: Vector Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ORCFooter = ORCFooter
  { orcHeaderLength :: !Word64
  , orcContentLength :: !Word64
  , orcStripes :: !(Vector StripeInformation)
  , orcTypes :: !(Vector ORCType)
  , orcMetadata :: !(Vector (Text, ByteString))
  , orcNumberOfRows :: !Word64
  , orcStatistics :: !(Vector ColumnStatistics)
  , orcEncryption :: !(Maybe FooterEncryption)
  {- ^ ORC 1.6+ column encryption metadata (protobuf
  @Footer.encryption@, field 10). Carried as a raw byte string so
  the footer codec can round-trip it without depending on
  "ORC.Encryption" — the higher-level 'ORC.Encryption.Encryption'
  record is encoded to / decoded from the same bytes at the
  integration layer.
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | Opaque container for the serialized @Footer.encryption@ bytes.
Keeping this as 'ByteString' rather than the parsed
'ORC.Encryption.Encryption' record avoids a circular dependency
(the encryption record lives in "ORC.Encryption", which in turn
needs the protobuf encoders).
-}
newtype FooterEncryption = FooterEncryption
  { feBytes :: ByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
