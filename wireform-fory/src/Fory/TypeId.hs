{-# LANGUAGE PatternSynonyms #-}
-- | Apache Fory xlang internal type IDs.
--
-- These follow the
-- <https://fory.apache.org/docs/specification/xlang_serialization_spec#internal-type-id-table
-- xlang specification table>. IDs 0-56 are reserved for internal
-- types; 57-255 are reserved for future internal use; user-registered
-- IDs occupy the @user_type_id@ varuint32 written separately after a
-- 'STRUCT', 'COMPATIBLE_STRUCT', 'ENUM', 'EXT', or 'TYPED_UNION' kind.
module Fory.TypeId
  ( TypeId (..)
  , typeIdToWord8
  , typeIdFromWord8

    -- * Convenient pattern synonyms
  , pattern UNKNOWN
  , pattern BOOL
  , pattern INT8
  , pattern INT16
  , pattern INT32
  , pattern VARINT32
  , pattern INT64
  , pattern VARINT64
  , pattern TAGGED_INT64
  , pattern UINT8
  , pattern UINT16
  , pattern UINT32
  , pattern VAR_UINT32
  , pattern UINT64
  , pattern VAR_UINT64
  , pattern TAGGED_UINT64
  , pattern FLOAT8
  , pattern FLOAT16
  , pattern BFLOAT16
  , pattern FLOAT32
  , pattern FLOAT64
  , pattern STRING
  , pattern LIST
  , pattern SET
  , pattern MAP
  , pattern ENUM
  , pattern NAMED_ENUM
  , pattern STRUCT
  , pattern COMPATIBLE_STRUCT
  , pattern NAMED_STRUCT
  , pattern NAMED_COMPATIBLE_STRUCT
  , pattern EXT
  , pattern NAMED_EXT
  , pattern UNION
  , pattern TYPED_UNION
  , pattern NAMED_UNION
  , pattern NONE
  , pattern DURATION
  , pattern TIMESTAMP
  , pattern DATE
  , pattern DECIMAL
  , pattern BINARY
  , pattern ARRAY
  , pattern BOOL_ARRAY
  , pattern INT8_ARRAY
  , pattern INT16_ARRAY
  , pattern INT32_ARRAY
  , pattern INT64_ARRAY
  , pattern UINT8_ARRAY
  , pattern UINT16_ARRAY
  , pattern UINT32_ARRAY
  , pattern UINT64_ARRAY
  , pattern FLOAT8_ARRAY
  , pattern FLOAT16_ARRAY
  , pattern BFLOAT16_ARRAY
  , pattern FLOAT32_ARRAY
  , pattern FLOAT64_ARRAY
  ) where

import Data.Word (Word8)

-- | An 8-bit Fory internal type id.
newtype TypeId = TypeId { unTypeId :: Word8 }
  deriving stock (Eq, Ord, Show)

typeIdToWord8 :: TypeId -> Word8
typeIdToWord8 = unTypeId
{-# INLINE typeIdToWord8 #-}

typeIdFromWord8 :: Word8 -> TypeId
typeIdFromWord8 = TypeId
{-# INLINE typeIdFromWord8 #-}

pattern UNKNOWN, BOOL, INT8, INT16, INT32, VARINT32, INT64, VARINT64,
        TAGGED_INT64, UINT8, UINT16, UINT32, VAR_UINT32, UINT64,
        VAR_UINT64, TAGGED_UINT64, FLOAT8, FLOAT16, BFLOAT16, FLOAT32,
        FLOAT64, STRING, LIST, SET, MAP, ENUM, NAMED_ENUM, STRUCT,
        COMPATIBLE_STRUCT, NAMED_STRUCT, NAMED_COMPATIBLE_STRUCT, EXT,
        NAMED_EXT, UNION, TYPED_UNION, NAMED_UNION, NONE, DURATION,
        TIMESTAMP, DATE, DECIMAL, BINARY, ARRAY, BOOL_ARRAY, INT8_ARRAY,
        INT16_ARRAY, INT32_ARRAY, INT64_ARRAY, UINT8_ARRAY, UINT16_ARRAY,
        UINT32_ARRAY, UINT64_ARRAY, FLOAT8_ARRAY, FLOAT16_ARRAY,
        BFLOAT16_ARRAY, FLOAT32_ARRAY, FLOAT64_ARRAY :: TypeId

pattern UNKNOWN                = TypeId  0
pattern BOOL                   = TypeId  1
pattern INT8                   = TypeId  2
pattern INT16                  = TypeId  3
pattern INT32                  = TypeId  4
pattern VARINT32               = TypeId  5
pattern INT64                  = TypeId  6
pattern VARINT64               = TypeId  7
pattern TAGGED_INT64           = TypeId  8
pattern UINT8                  = TypeId  9
pattern UINT16                 = TypeId 10
pattern UINT32                 = TypeId 11
pattern VAR_UINT32             = TypeId 12
pattern UINT64                 = TypeId 13
pattern VAR_UINT64             = TypeId 14
pattern TAGGED_UINT64          = TypeId 15
pattern FLOAT8                 = TypeId 16
pattern FLOAT16                = TypeId 17
pattern BFLOAT16               = TypeId 18
pattern FLOAT32                = TypeId 19
pattern FLOAT64                = TypeId 20
pattern STRING                 = TypeId 21
pattern LIST                   = TypeId 22
pattern SET                    = TypeId 23
pattern MAP                    = TypeId 24
pattern ENUM                   = TypeId 25
pattern NAMED_ENUM             = TypeId 26
pattern STRUCT                 = TypeId 27
pattern COMPATIBLE_STRUCT      = TypeId 28
pattern NAMED_STRUCT           = TypeId 29
pattern NAMED_COMPATIBLE_STRUCT = TypeId 30
pattern EXT                    = TypeId 31
pattern NAMED_EXT              = TypeId 32
pattern UNION                  = TypeId 33
pattern TYPED_UNION            = TypeId 34
pattern NAMED_UNION            = TypeId 35
pattern NONE                   = TypeId 36
pattern DURATION               = TypeId 37
pattern TIMESTAMP              = TypeId 38
pattern DATE                   = TypeId 39
pattern DECIMAL                = TypeId 40
pattern BINARY                 = TypeId 41
pattern ARRAY                  = TypeId 42
pattern BOOL_ARRAY             = TypeId 43
pattern INT8_ARRAY             = TypeId 44
pattern INT16_ARRAY            = TypeId 45
pattern INT32_ARRAY            = TypeId 46
pattern INT64_ARRAY            = TypeId 47
pattern UINT8_ARRAY            = TypeId 48
pattern UINT16_ARRAY           = TypeId 49
pattern UINT32_ARRAY           = TypeId 50
pattern UINT64_ARRAY           = TypeId 51
pattern FLOAT8_ARRAY           = TypeId 52
pattern FLOAT16_ARRAY          = TypeId 53
pattern BFLOAT16_ARRAY         = TypeId 54
pattern FLOAT32_ARRAY          = TypeId 55
pattern FLOAT64_ARRAY          = TypeId 56
