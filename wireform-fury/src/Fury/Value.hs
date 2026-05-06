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

    -- * Registered structs (pyfory-compatible NAMED_STRUCT)
  , registeredStructFieldByName
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable, hashWithSalt)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
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
    -- ^ @StructVal namespace typeName fields@. Encodes to a
    -- self-describing per-field meta-string + value layout that
    -- round-trips inside this package but does /not/ match
    -- pyfory's @NAMED_STRUCT@ wire format. Use
    -- 'RegisteredStructVal' instead for pyfory interop.
  | RegisteredStructVal !Text !Text !StructFields
    -- ^ @RegisteredStructVal namespace typeName fields@. Both
    -- producer and consumer must agree on the struct schema
    -- (passed to the encoder via 'Fury.Encode.encodeWithSchema').
    -- Encodes as the spec's @NAMED_STRUCT@: type tag + namespace
    -- meta-string + type-name meta-string + 4-byte schema hash
    -- + field values in pyfory's canonical order.
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

-- | Structural hashing for 'Value', used by the encoder to
-- detect repeated subtrees when reference tracking is enabled.
-- Vector contents are folded constructor-by-constructor so two
-- 'Value's that are 'Eq'-equal also hash equal.
instance Hashable Value where
  hashWithSalt s v = case v of
    NoneVal           -> hashWithSalt s (0  :: Int)
    BoolVal b         -> s `hashWithSalt` (1  :: Int) `hashWithSalt` b
    Int8Val n         -> s `hashWithSalt` (2  :: Int) `hashWithSalt` n
    Int16Val n        -> s `hashWithSalt` (3  :: Int) `hashWithSalt` n
    Int32Val n        -> s `hashWithSalt` (4  :: Int) `hashWithSalt` n
    VarInt32Val n     -> s `hashWithSalt` (5  :: Int) `hashWithSalt` n
    Int64Val n        -> s `hashWithSalt` (6  :: Int) `hashWithSalt` n
    VarInt64Val n     -> s `hashWithSalt` (7  :: Int) `hashWithSalt` n
    Uint8Val n        -> s `hashWithSalt` (8  :: Int) `hashWithSalt` n
    Uint16Val n       -> s `hashWithSalt` (9  :: Int) `hashWithSalt` n
    Uint32Val n       -> s `hashWithSalt` (10 :: Int) `hashWithSalt` n
    VarUint32Val n    -> s `hashWithSalt` (11 :: Int) `hashWithSalt` n
    Uint64Val n       -> s `hashWithSalt` (12 :: Int) `hashWithSalt` n
    VarUint64Val n    -> s `hashWithSalt` (13 :: Int) `hashWithSalt` n
    Float32Val f      -> s `hashWithSalt` (14 :: Int) `hashWithSalt` f
    Float64Val d      -> s `hashWithSalt` (15 :: Int) `hashWithSalt` d
    StringVal t       -> s `hashWithSalt` (16 :: Int) `hashWithSalt` t
    BinaryVal b       -> s `hashWithSalt` (17 :: Int) `hashWithSalt` b
    ListVal xs        -> hashVec (s `hashWithSalt` (18 :: Int)) xs
    SetVal xs         -> hashVec (s `hashWithSalt` (19 :: Int)) xs
    MapVal kvs        -> hashMap (s `hashWithSalt` (20 :: Int)) kvs
    StructVal ns nm fs ->
      hashStruct (s `hashWithSalt` (21 :: Int)
                    `hashWithSalt` ns `hashWithSalt` nm) fs
    RegisteredStructVal ns nm fs ->
      hashStruct (s `hashWithSalt` (35 :: Int)
                    `hashWithSalt` ns `hashWithSalt` nm) fs
    CompatibleStructVal ns nm fs ->
      hashStruct (s `hashWithSalt` (22 :: Int)
                    `hashWithSalt` ns `hashWithSalt` nm) fs
    RefVal i x        -> s `hashWithSalt` (23 :: Int)
                           `hashWithSalt` i `hashWithSalt` x
    BoolArrayVal xs   -> hashPrimVec (s `hashWithSalt` (24 :: Int)) xs
    Int8ArrayVal xs   -> hashPrimVec (s `hashWithSalt` (25 :: Int)) xs
    Int16ArrayVal xs  -> hashPrimVec (s `hashWithSalt` (26 :: Int)) xs
    Int32ArrayVal xs  -> hashPrimVec (s `hashWithSalt` (27 :: Int)) xs
    Int64ArrayVal xs  -> hashPrimVec (s `hashWithSalt` (28 :: Int)) xs
    Uint8ArrayVal xs  -> hashPrimVec (s `hashWithSalt` (29 :: Int)) xs
    Uint16ArrayVal xs -> hashPrimVec (s `hashWithSalt` (30 :: Int)) xs
    Uint32ArrayVal xs -> hashPrimVec (s `hashWithSalt` (31 :: Int)) xs
    Uint64ArrayVal xs -> hashPrimVec (s `hashWithSalt` (32 :: Int)) xs
    Float32ArrayVal xs -> hashPrimVec (s `hashWithSalt` (33 :: Int)) xs
    Float64ArrayVal xs -> hashPrimVec (s `hashWithSalt` (34 :: Int)) xs
    where
      hashVec :: Int -> Vector Value -> Int
      hashVec = V.foldl' hashWithSalt
      hashMap :: Int -> Vector (Value, Value) -> Int
      hashMap =
        V.foldl' (\acc (k, x) -> acc `hashWithSalt` k `hashWithSalt` x)
      hashStruct :: Int -> Vector (Text, Value) -> Int
      hashStruct =
        V.foldl' (\acc (k, x) -> acc `hashWithSalt` k `hashWithSalt` x)
      -- We hash primitive vectors lazily by length only to keep
      -- the cost bounded; structural sharing of large arrays is
      -- rare in practice and 'Eq' still ensures correctness.
      hashPrimVec :: Int -> Vector a -> Int
      hashPrimVec ss xs = ss `hashWithSalt` V.length xs

-- | Linear scan over @Vector (Text, Value)@ struct fields,
-- returning the first value associated with the given name (or
-- 'Nothing'). Used by the 'Fury.Encode.encodeRegisteredStruct'
-- machinery to look up a logical field in user-supplied order
-- when emitting it in pyfory's canonical wire order.
registeredStructFieldByName
  :: Text -> Vector (Text, Value) -> Maybe Value
registeredStructFieldByName k = V.foldr step Nothing
  where
    step (n, v) acc | n == k    = Just v
                    | otherwise = acc

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
  RegisteredStructVal{} -> T.NAMED_STRUCT
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
