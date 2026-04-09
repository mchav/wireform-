-- | Apache Arrow IPC metadata types.
--
-- Arrow IPC uses FlatBuffers for metadata. These types mirror the Arrow
-- Schema.fbs definitions for schema, field, type, and record batch metadata.
module Arrow.Types
  ( Schema(..)
  , Field(..)
  , ArrowType(..)
  , Endianness(..)
  , Precision(..)
  , DateUnit(..)
  , TimeUnit(..)
  , IntervalUnit(..)
  , UnionMode(..)
  , Message(..)
  , RecordBatchDef(..)
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data Field = Field
  { fieldName     :: !Text
  , fieldNullable :: !Bool
  , fieldType     :: !ArrowType
  , fieldChildren :: !(Vector Field)
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
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data Message
  = SchemaMessage !Schema
  | DictionaryBatch
  | RecordBatch !RecordBatchDef
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
