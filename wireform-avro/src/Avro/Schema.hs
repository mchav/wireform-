{-# OPTIONS_GHC -Wno-partial-fields #-}

{- | Avro schema abstract syntax tree.

Represents the full Avro schema specification including primitive types,
complex types (records, enums, arrays, maps, unions, fixed), and logical
types (decimal, date, time, timestamp, duration, uuid).

Data types use strict fields with @UNPACK@ where appropriate for
cache-friendly, allocation-light representations.  All names use 'Text'.
-}
module Avro.Schema (
  -- * Schema types
  AvroSchema (..),
  AvroField (..),
  AvroType (..),
  SortOrder (..),
  LogicalType (..),
) where

import Control.DeepSeq (NFData)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)


-- | Sort order for record fields, used by Avro's sort-order comparison.
data SortOrder
  = Ascending
  | Descending
  | Ignore
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)


-- | Avro logical types layered on top of primitive or fixed types.
data LogicalType
  = -- | @decimal(precision, scale)@ — annotates @bytes@ or @fixed@
    DecimalLogical {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | -- | @date@ — days since Unix epoch, annotates @int@
    DateLogical
  | -- | @time-millis@ — milliseconds since midnight, annotates @int@
    TimeMillisLogical
  | -- | @time-micros@ — microseconds since midnight, annotates @long@
    TimeMicrosLogical
  | -- | @timestamp-millis@ — milliseconds since Unix epoch, annotates @long@
    TimestampMillisLogical
  | -- | @timestamp-micros@ — microseconds since Unix epoch, annotates @long@
    TimestampMicrosLogical
  | -- | @duration@ — annotates a 12-byte @fixed@ (months, days, millis as 3 LE uint32s)
    DurationLogical
  | -- | @uuid@ — annotates @string@
    UuidLogical
  | -- | An unknown\/user-defined logical type, preserved for registry-driven codegen.
    CustomLogical !Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | A single field within an Avro record schema.
data AvroField = AvroField
  { avroFieldName :: !Text
  , avroFieldType :: !AvroType
  , avroFieldDefault :: !(Maybe AvroSchema)
  , avroFieldOrder :: !(Maybe SortOrder)
  , avroFieldAliases :: !(Vector Text)
  , avroFieldDoc :: !(Maybe Text)
  , avroFieldProps :: !(Map Text Text)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


{- | Core Avro schema AST.

A schema is either an 'AvroPrimitive' type reference, a 'AvroLogical' type
wrapping a base type, or one of the complex types (record, enum, array,
map, union, fixed).  The 'AvroSchemaRef' constructor handles named type
references within unions and recursive schemas.
-}
data AvroSchema
  = -- | The null type: no value, zero bytes on the wire.
    AvroNull
  | -- | Boolean: single byte 0x00 or 0x01.
    AvroBool
  | -- | 32-bit signed integer (ZigZag + varint on the wire).
    AvroInt
  | -- | 64-bit signed integer (ZigZag + varint on the wire).
    AvroLong
  | -- | 32-bit IEEE 754 float, little-endian.
    AvroFloat
  | -- | 64-bit IEEE 754 double, little-endian.
    AvroDouble
  | -- | Arbitrary byte sequence, length-prefixed.
    AvroBytes
  | -- | UTF-8 string, length-prefixed.
    AvroString
  | -- | Named reference to another schema (for recursive types).
    AvroSchemaRef !Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Avro type, encompassing primitives, complex types, and logical types.
data AvroType
  = -- | A primitive schema used as a type.
    AvroPrimitive !AvroSchema
  | -- | Record: ordered list of named, typed fields.
    AvroRecord
      { avroRecordName :: !Text
      , avroRecordNamespace :: !(Maybe Text)
      , avroRecordDoc :: !(Maybe Text)
      , avroRecordAliases :: !(Vector Text)
      , avroRecordFields :: !(Vector AvroField)
      , avroRecordProps :: !(Map Text Text)
      }
  | -- | Enum: ordered list of symbolic names, encoded as a varint index.
    AvroEnum
      { avroEnumName :: !Text
      , avroEnumNamespace :: !(Maybe Text)
      , avroEnumDoc :: !(Maybe Text)
      , avroEnumAliases :: !(Vector Text)
      , avroEnumSymbols :: !(Vector Text)
      , avroEnumDefault :: !(Maybe Text)
      }
  | -- | Array: homogeneous sequence, encoded as blocks of (count, items).
    AvroArray
      { avroArrayItems :: !AvroType
      }
  | -- | Map: string-keyed, encoded as blocks of (count, key-value pairs).
    AvroMap
      { avroMapValues :: !AvroType
      }
  | -- | Union: discriminated by a varint branch index on the wire.
    AvroUnion
      { avroUnionBranches :: !(Vector AvroType)
      }
  | -- | Fixed: fixed number of bytes, no length prefix on the wire.
    AvroFixed
      { avroFixedName :: !Text
      , avroFixedNamespace :: !(Maybe Text)
      , avroFixedSize :: {-# UNPACK #-} !Int
      , avroFixedAliases :: !(Vector Text)
      }
  | -- | Logical type layered on a base type.
    AvroLogical
      { avroLogicalBase :: !AvroType
      , avroLogicalType :: !LogicalType
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
