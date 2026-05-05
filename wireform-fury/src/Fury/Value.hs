-- | Dynamic Apache Fory value.
--
-- 'Value' carries the union of types we encode at the value layer:
-- primitive scalars, strings/binary, lists/sets/maps, primitive
-- arrays, and named structs. It is the AST that 'Fury.Encode' and
-- 'Fury.Decode' walk.
--
-- A struct is represented as @StructVal namespace typeName fields@
-- with 'fields' a vector of (snake_case field name, value) pairs in
-- the order they were written. This is the @NAMED_STRUCT@ shape
-- from the spec, with deterministic field order and no
-- meta-string deduplication (see 'Fury.MetaString').
module Fury.Value
  ( Value (..)
  , StructFields
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

-- | Field list for a struct: pairs of @(field_name, value)@,
-- already in canonical write order.
type StructFields = Vector (Text, Value)

data Value
  = NoneVal
  | BoolVal       !Bool
  | Int8Val       {-# UNPACK #-} !Int8
  | Int16Val      {-# UNPACK #-} !Int16
  | Int32Val      {-# UNPACK #-} !Int32
  | Int64Val      {-# UNPACK #-} !Int64
  | Uint8Val      {-# UNPACK #-} !Word8
  | Uint16Val     {-# UNPACK #-} !Word16
  | Uint32Val     {-# UNPACK #-} !Word32
  | Uint64Val     {-# UNPACK #-} !Word64
  | Float32Val    {-# UNPACK #-} !Float
  | Float64Val    {-# UNPACK #-} !Double
  | StringVal     !Text
  | BinaryVal     !ByteString
  | ListVal       !(Vector Value)
  | SetVal        !(Vector Value)
  | MapVal        !(Vector (Value, Value))
  | StructVal     !Text !Text !StructFields
    -- ^ @StructVal namespace typeName fields@.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
