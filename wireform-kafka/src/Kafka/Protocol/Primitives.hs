{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

{-|
Module      : Kafka.Protocol.Primitives
Description : Core primitive types for the Kafka wire protocol
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module provides the fundamental newtype + smart-constructor
surface for the Kafka wire-protocol primitives — fixed-width
integers, variable-length integers, strings, byte blobs, arrays,
UUIDs, and the @TaggedFields@ container.

The on-the-wire codec lives in "Kafka.Protocol.Wire" /
"Kafka.Protocol.Wire.Primitives" and uses these types via the
'Wire' typeclass / per-helper poke / peek pairs. The legacy
'Data.Bytes.Serial' instances that used to live here are gone:
the runtime path is fully Wire and pre-existing 'Serial'-shape
clients are unsupported.

The Kafka protocol uses big-endian (network byte order) for all
integer types.

= Variable-Length Integers

Kafka uses variable-length encoding for integers in "compact"
message formats (introduced in flexible versions, KIP-482):

* 'VarInt'  — variable-length signed 32-bit integer using ZigZag
* 'VarLong' — variable-length signed 64-bit integer using ZigZag

= Strings and Arrays

Kafka strings and arrays have length prefixes:

* Non-compact: 16-bit or 32-bit signed length prefix
* Compact: Variable-length unsigned integer prefix
* Nullable: Length of -1 indicates null

= Tagged Fields

Flexible message versions (9+ for most messages) support tagged
fields, which allow backward-compatible protocol evolution. Tagged
fields are encoded as a 'UVarInt' count followed by a sequence of
@(UVarInt tag, UVarInt size, bytes)@ triples.
-}
module Kafka.Protocol.Primitives
  ( -- * Fixed-Width Integer Types
    Int8
  , Int16
  , Int32
  , Int64
  , Word32
    -- * Variable-Length Integer Types
  , VarInt(..)
  , VarLong(..)
  , UVarInt(..)
    -- * String Types
  , KafkaString(..)
  , mkKafkaString
  , unKafkaString
  , CompactString
  , mkCompactString
  , unCompactString
    -- * UUID Type
  , KafkaUuid
  , mkKafkaUuid
  , unKafkaUuid
  , nullUuid
    -- * Array Types  
  , KafkaArray(..)
  , mkKafkaArray
  , unKafkaArray
  , CompactArray(..)
  , mkCompactArray
  , unCompactArray
    -- * Bytes Types
  , KafkaBytes(..)
  , mkKafkaBytes
  , unKafkaBytes
  , CompactBytes
  , mkCompactBytes
  , unCompactBytes
    -- * Tagged Fields
  , TaggedFields
  , TaggedField(..)
  , emptyTaggedFields
  , lookupTaggedField
  , insertTaggedField
    -- * Nullable Types
  , Nullable(..)
  , toNullable
  , fromNullable
    -- * Type Conversions
  , toCompactString
  , fromCompactString
  , toCompactBytes
  , fromCompactBytes
  , toCompactArray
  , fromCompactArray
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)

-- | Represents a value that may be null in the Kafka protocol.
-- Many Kafka fields can be explicitly null (encoded as -1 length).
data Nullable a = Null | NotNull a
  deriving (Eq, Show, Ord, Generic)

-- | Convert a 'Maybe' to a 'Nullable'.
toNullable :: Maybe a -> Nullable a
toNullable Nothing = Null
toNullable (Just x) = NotNull x

-- | Convert a 'Nullable' to a 'Maybe'.
fromNullable :: Nullable a -> Maybe a
fromNullable Null = Nothing
fromNullable (NotNull x) = Just x
-- -----------------------------------------------------------------------------
-- Variable-Length Integers
-- -----------------------------------------------------------------------------

-- | Variable-length signed 32-bit integer using ZigZag encoding.
-- Used in compact message formats. Values are encoded using a continuation
-- bit scheme where the MSB indicates more bytes follow.
--
-- ZigZag encoding maps signed integers to unsigned integers:
-- 
-- > 0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, 2 -> 4, ...
newtype VarInt = VarInt { unVarInt :: Int32 }
  deriving (Eq, Show, Ord, Num, Generic)

-- | Variable-length signed 64-bit integer using ZigZag encoding.
newtype VarLong = VarLong { unVarLong :: Int64 }
  deriving (Eq, Show, Ord, Num, Generic)

-- | Variable-length unsigned 32-bit integer.
-- Used for compact array and string lengths.
newtype UVarInt = UVarInt { unUVarInt :: Word32 }
  deriving (Eq, Show, Ord, Num, Generic)

-- | Encode a signed 32-bit integer as ZigZag encoded VarInt.
zigZagEncode32 :: Int32 -> Word32
zigZagEncode32 n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` 31)

-- | Decode a ZigZag encoded value to a signed 32-bit integer.
zigZagDecode32 :: Word32 -> Int32
zigZagDecode32 n = fromIntegral $ (n `shiftR` 1) `xor` (-(n .&. 1))

-- | Encode a signed 64-bit integer as ZigZag encoded VarLong.
zigZagEncode64 :: Int64 -> Word64
zigZagEncode64 n = fromIntegral $ (n `shiftL` 1) `xor` (n `shiftR` 63)

-- | Decode a ZigZag encoded value to a signed 64-bit integer.
zigZagDecode64 :: Word64 -> Int64
zigZagDecode64 n = fromIntegral $ (n `shiftR` 1) `xor` (-(n .&. 1))
-- (Serial-shape varint encode / decode helpers removed by the
-- no-Serial migration; the equivalent direct-poke / direct-peek
-- primitives live in "Kafka.Protocol.Wire".)
-- -----------------------------------------------------------------------------
-- String Types
-- -----------------------------------------------------------------------------

-- | Standard Kafka string with 16-bit length prefix (non-compact format).
-- A length of -1 indicates a null string.
newtype KafkaString = KafkaString { unKafkaString :: Nullable Text }
  deriving (Eq, Show, Generic)

-- | Create a non-null Kafka string.
mkKafkaString :: Text -> KafkaString
mkKafkaString = KafkaString . NotNull
-- | Compact Kafka string with variable-length unsigned length prefix.
-- Used in flexible message versions. A length of 0 indicates null,
-- so actual lengths are encoded as length + 1.
newtype CompactString = CompactString { unCompactString :: Nullable Text }
  deriving (Eq, Show, Generic)

-- | Create a non-null compact Kafka string.
mkCompactString :: Text -> CompactString
mkCompactString = CompactString . NotNull
-- -----------------------------------------------------------------------------
-- UUID Type
-- -----------------------------------------------------------------------------

-- | Kafka UUID (128-bit identifier).
-- The null UUID is all zeros.
newtype KafkaUuid = KafkaUuid { unKafkaUuid :: UUID }
  deriving (Eq, Show, Ord, Generic)

-- | Create a Kafka UUID from a UUID.
mkKafkaUuid :: UUID -> KafkaUuid
mkKafkaUuid = KafkaUuid

-- | The null UUID (all zeros).
nullUuid :: KafkaUuid
nullUuid = KafkaUuid UUID.nil
-- -----------------------------------------------------------------------------
-- Array Types
-- -----------------------------------------------------------------------------

-- | Standard Kafka array with 32-bit length prefix (non-compact format).
-- A length of -1 indicates a null array.
newtype KafkaArray a = KafkaArray { unKafkaArray :: Nullable (Vector a) }
  deriving (Eq, Show, Generic)

-- | Create a non-null Kafka array.
mkKafkaArray :: Vector a -> KafkaArray a
mkKafkaArray = KafkaArray . NotNull
-- | Compact Kafka array with variable-length unsigned length prefix.
-- Used in flexible message versions. A length of 0 indicates null,
-- so actual lengths are encoded as length + 1.
newtype CompactArray a = CompactArray { unCompactArray :: Nullable (Vector a) }
  deriving (Eq, Show, Generic)

-- | Create a non-null compact Kafka array.
mkCompactArray :: Vector a -> CompactArray a
mkCompactArray = CompactArray . NotNull
-- -----------------------------------------------------------------------------
-- Bytes Types
-- -----------------------------------------------------------------------------

-- | Standard Kafka bytes with 32-bit length prefix (non-compact format).
-- A length of -1 indicates null bytes.
newtype KafkaBytes = KafkaBytes { unKafkaBytes :: Nullable ByteString }
  deriving (Eq, Show, Generic)

-- | Create non-null Kafka bytes.
mkKafkaBytes :: ByteString -> KafkaBytes
mkKafkaBytes = KafkaBytes . NotNull
-- | Compact Kafka bytes with variable-length unsigned length prefix.
-- Used in flexible message versions. A length of 0 indicates null,
-- so actual lengths are encoded as length + 1.
newtype CompactBytes = CompactBytes { unCompactBytes :: Nullable ByteString }
  deriving (Eq, Show, Generic)

-- | Create non-null compact Kafka bytes.
mkCompactBytes :: ByteString -> CompactBytes
mkCompactBytes = CompactBytes . NotNull
-- -----------------------------------------------------------------------------
-- Tagged Fields
-- -----------------------------------------------------------------------------

-- | A single tagged field, consisting of a tag number and raw bytes.
-- Tagged fields enable backward-compatible protocol evolution.
data TaggedField = TaggedField
  { tagNumber :: !Word32  -- ^ Tag identifier
  , tagData   :: !ByteString  -- ^ Raw field data
  } deriving (Eq, Show, Generic)

-- | Collection of tagged fields, indexed by tag number.
-- Tagged fields appear at the end of flexible message versions.
newtype TaggedFields = TaggedFields { unTaggedFields :: Map Word32 ByteString }
  deriving (Eq, Show, Generic)

-- | Empty tagged fields (no tags present).
emptyTaggedFields :: TaggedFields
emptyTaggedFields = TaggedFields Map.empty

-- | Look up a tagged field by tag number.
lookupTaggedField :: Word32 -> TaggedFields -> Maybe ByteString
lookupTaggedField tag (TaggedFields m) = Map.lookup tag m

-- | Insert or update a tagged field.
insertTaggedField :: Word32 -> ByteString -> TaggedFields -> TaggedFields
insertTaggedField tag bs (TaggedFields m) = TaggedFields (Map.insert tag bs m)

-- ('serializeTaggedFieldEntries' Serial-shape helper removed by the
-- no-Serial migration; the equivalent direct-poke helper lives in
-- "Kafka.Protocol.Wire.Primitives.pokeTaggedFieldEntries".)

-- -----------------------------------------------------------------------------
-- Type Conversions
-- -----------------------------------------------------------------------------

-- | Convert a standard KafkaString to CompactString for flexible encoding.
toCompactString :: KafkaString -> CompactString
toCompactString (KafkaString Null) = CompactString Null
toCompactString (KafkaString (NotNull t)) = CompactString (NotNull t)

-- | Convert a CompactString back to standard KafkaString.
fromCompactString :: CompactString -> KafkaString
fromCompactString (CompactString Null) = KafkaString Null
fromCompactString (CompactString (NotNull t)) = KafkaString (NotNull t)

-- | Convert standard KafkaBytes to CompactBytes for flexible encoding.
toCompactBytes :: KafkaBytes -> CompactBytes
toCompactBytes (KafkaBytes Null) = CompactBytes Null
toCompactBytes (KafkaBytes (NotNull bs)) = CompactBytes (NotNull bs)

-- | Convert CompactBytes back to standard KafkaBytes.
fromCompactBytes :: CompactBytes -> KafkaBytes
fromCompactBytes (CompactBytes Null) = KafkaBytes Null
fromCompactBytes (CompactBytes (NotNull bs)) = KafkaBytes (NotNull bs)

-- | Convert standard KafkaArray to CompactArray for flexible encoding.
toCompactArray :: KafkaArray a -> CompactArray a
toCompactArray (KafkaArray Null) = CompactArray Null
toCompactArray (KafkaArray (NotNull vec)) = CompactArray (NotNull vec)

-- | Convert CompactArray back to standard KafkaArray.
fromCompactArray :: CompactArray a -> KafkaArray a
fromCompactArray (CompactArray Null) = KafkaArray Null
fromCompactArray (CompactArray (NotNull vec)) = KafkaArray (NotNull vec)

