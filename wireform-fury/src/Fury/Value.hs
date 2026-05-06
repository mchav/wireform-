-- | Dynamic Apache Fory value.
--
-- 'Value' is the AST that 'Fury.Encode' and 'Fury.Decode' walk. It
-- covers the primitive scalars, strings\/binary, list\/set\/map
-- collections, named structs, and the spec\'s extended kinds:
--
-- * 'RefVal' – a value carrying a sharing key. The encoder turns
--   the first occurrence of each key into a @REF_VALUE_FLAG@ and
--   subsequent occurrences into a @REF_FLAG + varuint32@ back
--   reference, exactly mirroring the spec\'s reference-tracking
--   algorithm.
--
-- * 'CompatibleStructVal' – a struct that uses the spec\'s
--   @NAMED_COMPATIBLE_STRUCT@ tag with a shared 'TypeDef' sidecar,
--   so receivers can perform schema-evolution-style field-by-name
--   matching.
--
-- * @*ArrayVal@ – the canonical wire tags for one-dimensional
--   bool / numeric arrays (BOOL_ARRAY … FLOAT64_ARRAY).
module Fury.Value
  ( Value (..)
  , StructFields
  , typeIdOf
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

import Fury.TypeId (TypeId)
import qualified Fury.TypeId as T

-- | Field list for a struct: pairs of @(field_name, value)@,
-- already in canonical write order.
type StructFields = Vector (Text, Value)

data Value
  = NoneVal
  | BoolVal       !Bool
  | Int8Val       {-# UNPACK #-} !Int8
  | Int16Val      {-# UNPACK #-} !Int16
  | Int32Val      {-# UNPACK #-} !Int32
  | VarInt32Val   {-# UNPACK #-} !Int32
    -- ^ Zigzag-then-varuint-encoded int32 (xlang VARINT32, type id 5).
  | Int64Val      {-# UNPACK #-} !Int64
  | VarInt64Val   {-# UNPACK #-} !Int64
    -- ^ Zigzag-then-varuint-encoded int64 (xlang VARINT64, type id 7).
    -- This is the encoding the Apache Fory python and java
    -- implementations use by default for native integer types.
  | Uint8Val      {-# UNPACK #-} !Word8
  | Uint16Val     {-# UNPACK #-} !Word16
  | Uint32Val     {-# UNPACK #-} !Word32
  | VarUint32Val  {-# UNPACK #-} !Word32
    -- ^ Varuint-encoded uint32 (xlang VAR_UINT32, type id 12).
  | Uint64Val     {-# UNPACK #-} !Word64
  | VarUint64Val  {-# UNPACK #-} !Word64
    -- ^ Varuint-encoded uint64 (xlang VAR_UINT64, type id 14).
  | Float32Val    {-# UNPACK #-} !Float
  | Float64Val    {-# UNPACK #-} !Double
  | StringVal     !Text
  | BinaryVal     !ByteString
  | ListVal       !(Vector Value)
  | SetVal        !(Vector Value)
  | MapVal        !(Vector (Value, Value))
  | StructVal     !Text !Text !StructFields
    -- ^ @StructVal namespace typeName fields@, written as
    -- @NAMED_STRUCT@.
  | CompatibleStructVal !Text !Text !StructFields
    -- ^ @CompatibleStructVal namespace typeName fields@. Same
    -- payload as 'StructVal' but written under the @NAMED_COMPATIBLE_STRUCT@
    -- tag with a shared 'TypeDef' sidecar so the receiver can
    -- match fields by name across schema drift.
  | RefVal              {-# UNPACK #-} !Int !Value
    -- ^ @RefVal sharingKey inner@. The 'sharingKey' is opaque to
    -- the wire (it never appears on the wire — only the encoder\'s
    -- auto-assigned @ref_id@ does). When the encoder sees the same
    -- 'sharingKey' more than once it emits a @REF_FLAG@
    -- back-reference instead of re-encoding 'inner'. On
    -- round-trip, the decoded 'sharingKey' is the wire @ref_id@,
    -- /not/ the one the user originally supplied; structural
    -- sharing is preserved but the integer key may be remapped.
  | BoolArrayVal        !(Vector Bool)
  | Int8ArrayVal        !(Vector Int8)
  | Int16ArrayVal       !(Vector Int16)
  | Int32ArrayVal       !(Vector Int32)
  | Int64ArrayVal       !(Vector Int64)
  | Uint8ArrayVal       !(Vector Word8)
  | Uint16ArrayVal      !(Vector Word16)
  | Uint32ArrayVal      !(Vector Word32)
  | Uint64ArrayVal      !(Vector Word64)
  | Float32ArrayVal     !(Vector Float)
  | Float64ArrayVal     !(Vector Double)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | The internal Fory type id that the encoder uses for this
-- value\'s leading type tag. Used by both encoder and decoder when
-- dispatching on a value\'s shape.
--
-- 'NoneVal' and 'RefVal' are not encoded as type-tagged values but
-- as ref-flag bytes, so 'typeIdOf' returns 'T.NONE' / 'T.UNKNOWN'
-- respectively as a placeholder.
typeIdOf :: Value -> TypeId
typeIdOf v = case v of
  NoneVal              -> T.NONE
  BoolVal{}            -> T.BOOL
  Int8Val{}            -> T.INT8
  Int16Val{}           -> T.INT16
  Int32Val{}           -> T.INT32
  VarInt32Val{}        -> T.VARINT32
  Int64Val{}           -> T.INT64
  VarInt64Val{}        -> T.VARINT64
  Uint8Val{}           -> T.UINT8
  Uint16Val{}          -> T.UINT16
  Uint32Val{}          -> T.UINT32
  VarUint32Val{}       -> T.VAR_UINT32
  Uint64Val{}          -> T.UINT64
  VarUint64Val{}       -> T.VAR_UINT64
  Float32Val{}         -> T.FLOAT32
  Float64Val{}         -> T.FLOAT64
  StringVal{}          -> T.STRING
  BinaryVal{}          -> T.BINARY
  ListVal{}            -> T.LIST
  SetVal{}             -> T.SET
  MapVal{}             -> T.MAP
  StructVal{}          -> T.NAMED_STRUCT
  CompatibleStructVal{} -> T.NAMED_COMPATIBLE_STRUCT
  RefVal{}             -> T.UNKNOWN
  BoolArrayVal{}       -> T.BOOL_ARRAY
  Int8ArrayVal{}       -> T.INT8_ARRAY
  Int16ArrayVal{}      -> T.INT16_ARRAY
  Int32ArrayVal{}      -> T.INT32_ARRAY
  Int64ArrayVal{}      -> T.INT64_ARRAY
  Uint8ArrayVal{}      -> T.UINT8_ARRAY
  Uint16ArrayVal{}     -> T.UINT16_ARRAY
  Uint32ArrayVal{}     -> T.UINT32_ARRAY
  Uint64ArrayVal{}     -> T.UINT64_ARRAY
  Float32ArrayVal{}    -> T.FLOAT32_ARRAY
  Float64ArrayVal{}    -> T.FLOAT64_ARRAY
