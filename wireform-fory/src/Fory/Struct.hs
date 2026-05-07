{-# LANGUAGE BangPatterns #-}
-- | Registered-struct schemas for byte-for-byte
-- @NAMED_STRUCT@ (type id 29) interop with @pyfory@ 0.17.
--
-- Apache Fory's struct serializer is registry-based: both
-- producer and consumer agree on a struct's namespace, type
-- name, ordered field list, and the per-field type ids. The
-- producer computes a fingerprint hash from this schema and
-- writes it as a 4-byte little-endian int32 right after the
-- namespace + type-name meta-strings, so the consumer can
-- detect schema mismatches early.
--
-- This module exposes the primitives needed to participate in
-- that protocol:
--
-- * 'StructSchema' / 'FieldSpec'  — declarative schema.
-- * 'fieldOrder'                  — pyfory's canonical field
--                                   ordering (group_fields +
--                                   sort).
-- * 'computeStructHash'           — pyfory's
--                                   compute_struct_fingerprint
--                                   + MurmurHash3_x64_128
--                                   (seed 47), low 32 bits as
--                                   signed int32.
-- * 'isPrimitiveTypeId'           — mirrors pyfory's
--                                   @is_primitive_type@ check.
module Fory.Struct
  ( -- * Schemas
    FieldSpec (..)
  , StructSchema (..)
  , mkSchema

    -- * Hash
  , computeStructFingerprint
  , computeStructHash

    -- * Field categorisation + ordering
  , isPrimitiveTypeId
  , isBasicTypeId
  , primitiveTypeSize
  , fieldOrder
  , computeFieldOrder
  ) where

import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Function (on)
import Data.Int (Int32)
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Fory.MetaString.Encoder (Encoding (UTF8), encodeMetaString,
                                namespaceSpecialChars, typenameSpecialChars)
import Fory.MetaString.Hash (murmur3X64_128)
import Fory.TypeId (TypeId (..))
import qualified Fory.TypeId as T

-- ---------------------------------------------------------------------------
-- Field + struct schema
-- ---------------------------------------------------------------------------

-- | One field's contribution to a struct's schema.
data FieldSpec = FieldSpec
  { fsName     :: !Text
  , fsTypeId   :: !TypeId
  , fsRef      :: !Bool
    -- ^ Whether the field opts into reference tracking. For our
    -- common cases (a dataclass with primitive / string fields)
    -- this is 'False'.
  , fsNullable :: !Bool
    -- ^ Whether the wire prefixes the field with a
    -- @NULL_FLAG@ \/ @NOT_NULL_VALUE_FLAG@ slot byte. For a
    -- pyfory dataclass declared without @Optional[...]@ this
    -- is 'False'; otherwise 'True'.
  } deriving (Show, Eq)

-- | A complete struct schema. The 'ssHash' and 'ssFieldOrder'
-- fields are derived from the others; we cache them at
-- construction time so that the per-struct encode path is just
-- a hash byte-write + a vector traversal in canonical order.
data StructSchema = StructSchema
  { ssNamespace  :: !Text
  , ssTypename   :: !Text
  , ssFields     :: !(Vector FieldSpec)
    -- | Cached @MurmurHash3-x64-128(seed=47) low 32 bits@ of
    -- the fingerprint string. Pre-computed in 'mkSchema'.
  , ssHash       :: !Int32
    -- | Cached canonical field order pyfory writes for this
    -- schema (see 'fieldOrder' for the algorithm).
  , ssFieldOrder :: !(Vector FieldSpec)
    -- | Field names in canonical order. Cached so the
    -- decode hot path doesn't have to re-derive it via
    -- 'V.map fsName' for every struct it decodes.
  , ssFieldOrderNames :: !(Vector Text)
    -- | Cached pre-encoded namespace meta-string body and
    -- chosen 'Encoding' tag. Skips the per-encode
    -- 'encodeMetaString' bit-packing + char classification
    -- when the encoder hits a fresh-meta-string slot for
    -- this schema.
  , ssNsBody     :: !BS.ByteString
  , ssNsEncoding :: !Encoding
    -- | Cached pre-encoded type-name meta-string body and
    -- encoding tag.
  , ssTnBody     :: !BS.ByteString
  , ssTnEncoding :: !Encoding
  } deriving (Show, Eq)

-- | Convenience constructor. Computes the schema fingerprint
-- hash, the canonical field order, and the bit-packed
-- meta-string bodies for the namespace + type name once so
-- that subsequent encodes pay a constant 'O(1)' lookup
-- instead of re-running the meta-string char-classification
-- + bit-pack pipeline on every emit.
mkSchema :: Text -> Text -> [(Text, TypeId)] -> StructSchema
mkSchema ns nm fs =
  let !fields  = V.fromList [ FieldSpec n t False False | (n, t) <- fs ]
      !empty   = StructSchema ns nm fields 0 V.empty V.empty
                   BS.empty UTF8 BS.empty UTF8
      !h       = computeStructHash empty
      !ord     = computeFieldOrder empty
      !ordNms  = V.map fsName ord
      (!nsEnc, !nsBody) = encodeMetaString namespaceSpecialChars ns
      (!tnEnc, !tnBody) = encodeMetaString typenameSpecialChars  nm
  in StructSchema ns nm fields h ord ordNms nsBody nsEnc tnBody tnEnc

-- ---------------------------------------------------------------------------
-- Schema fingerprint + hash
-- ---------------------------------------------------------------------------

-- | Mirror @pyfory.struct.compute_struct_fingerprint@: build
-- @"<name>,<type_id>,<ref>,<nullable>;"@ for each field, then
-- sort fields by name. Only the name-keyed branch is emitted
-- (we don't yet support tag-id'd fields).
computeStructFingerprint :: StructSchema -> BS.ByteString
computeStructFingerprint sch =
  let fields  = V.toList (ssFields sch)
      sorted  = sortBy (comparing fsName) fields
      pieces  = map fieldPiece sorted
  in BS.concat (map BS8.pack pieces)
  where
    fieldPiece :: FieldSpec -> String
    fieldPiece f =
      T.unpack (fsName f)
        ++ "," ++ show (unTypeId (fsTypeId f))
        ++ "," ++ (if fsRef f      then "1" else "0")
        ++ "," ++ (if fsNullable f then "1" else "0")
        ++ ";"

    unTypeId :: TypeId -> Int
    unTypeId (TypeId w) = fromIntegral w

-- | The 4-byte schema version hash pyfory writes after the
-- namespace + type-name meta-strings of a non-compatible
-- 'NAMED_STRUCT'. Returns the value as a signed 'Int32'
-- (the encoder writes it little-endian).
computeStructHash :: StructSchema -> Int32
computeStructHash sch =
  let fp = computeStructFingerprint sch
  in if BS.null fp
       then 47
       else
         let (h1, _) = murmur3X64_128 fp 47
             low32   = fromIntegral (h1 .&. 0xFFFFFFFF)
         in (low32 :: Int32)

-- ---------------------------------------------------------------------------
-- Field categorisation
-- ---------------------------------------------------------------------------

-- | The type ids pyfory's @is_primitive_type@ recognises:
-- bool, all signed/unsigned/varint flavours of intN, and floatN.
isPrimitiveTypeId :: TypeId -> Bool
isPrimitiveTypeId t = t `elem`
  [ T.BOOL
  , T.INT8, T.INT16, T.INT32, T.VARINT32, T.INT64, T.VARINT64
  , T.TAGGED_INT64
  , T.UINT8, T.UINT16, T.UINT32, T.VAR_UINT32, T.UINT64
  , T.VAR_UINT64, T.TAGGED_UINT64
  , T.FLOAT16, T.BFLOAT16, T.FLOAT32, T.FLOAT64
  ]

-- | The type ids in pyfory's @DataClassSerializer._BASIC_SERIALIZERS@:
-- primitives (excluding the var* and tagged* variants since
-- those aren't in @_BASIC_SERIALIZERS@) plus 'STRING'. Used by
-- the registered-struct field writer to decide whether to wrap
-- a nullable field with @NULL_FLAG@ \/ @NOT_NULL_VALUE_FLAG@.
isBasicTypeId :: TypeId -> Bool
isBasicTypeId t = t `elem`
  [ T.BOOL
  , T.INT8, T.INT16, T.INT32, T.VARINT32, T.INT64, T.VARINT64
  , T.UINT8, T.UINT16, T.UINT32, T.VAR_UINT32, T.UINT64
  , T.VAR_UINT64
  , T.FLOAT32, T.FLOAT64
  , T.STRING
  ]

-- | Width in bytes of a primitive type id, used by pyfory's
-- 'numeric_sorter'. Var-coded types report their compressed
-- representation's maximum width.
primitiveTypeSize :: TypeId -> Int
primitiveTypeSize t
  | t == T.BOOL                              = 1
  | t == T.INT8 || t == T.UINT8              = 1
  | t == T.INT16 || t == T.UINT16
      || t == T.FLOAT16 || t == T.BFLOAT16   = 2
  | t == T.INT32 || t == T.UINT32
      || t == T.VARINT32 || t == T.VAR_UINT32
      || t == T.FLOAT32                      = 4
  | t == T.INT64 || t == T.UINT64
      || t == T.VARINT64 || t == T.VAR_UINT64
      || t == T.TAGGED_INT64 || t == T.TAGGED_UINT64
      || t == T.FLOAT64                      = 8
  | otherwise                                = 0

-- | Whether a primitive type is a compressed (varint /
-- tagged) flavour. Used as the first sort key in pyfory's
-- 'numeric_sorter'.
isCompressedPrimitive :: TypeId -> Bool
isCompressedPrimitive t = t `elem`
  [ T.VARINT32, T.VARINT64, T.TAGGED_INT64
  , T.VAR_UINT32, T.VAR_UINT64, T.TAGGED_UINT64
  ]

-- ---------------------------------------------------------------------------
-- Field ordering (pyfory's group_fields + per-group sort)
-- ---------------------------------------------------------------------------

-- | The canonical wire order pyfory writes a struct's fields
-- in. This is an O(1) read of the cached 'ssFieldOrder' field;
-- 'mkSchema' calls 'computeFieldOrder' once at construction.
fieldOrder :: StructSchema -> Vector FieldSpec
fieldOrder = ssFieldOrder
{-# INLINE fieldOrder #-}

-- | The actual canonical-ordering algorithm. Categorise +
-- sort fields, returning them in the canonical wire order
-- pyfory writes them in:
--
-- * primitives (boxed_types / nullable_boxed_types) — sorted
--   by (compress flag, -size, -type_id, name)
-- * strings + binary (internal_types) — sorted by (type_id, name)
-- * lists / sets / maps — sorted by (type_id, name)
--
-- Anything else falls into the @other_types@ group, sorted by
-- name.
computeFieldOrder :: StructSchema -> Vector FieldSpec
computeFieldOrder sch =
  let fields = V.toList (ssFields sch)
      categorised = map categorise fields
      boxed    = [ f | (B,    f) <- categorised ]
      nboxed   = [ f | (NB,   f) <- categorised ]
      collsL   = [ f | (Coll, f) <- categorised ]
      sets     = [ f | (Set_, f) <- categorised ]
      maps     = [ f | (Map_, f) <- categorised ]
      internal = [ f | (Int_, f) <- categorised ]
      other    = [ f | (Other,f) <- categorised ]
      sortedBoxed   = sortBy primSorter boxed
      sortedNBoxed  = sortBy primSorter nboxed
      sortedColls   = sortBy genericSorter collsL
      sortedSets    = sortBy genericSorter sets
      sortedMaps    = sortBy genericSorter maps
      sortedIntern  = sortBy genericSorter internal
      sortedOther   = sortBy (compare `on` fsName) other
  in V.fromList
       (  sortedBoxed
       ++ sortedNBoxed
       ++ sortedIntern
       ++ sortedColls
       ++ sortedSets
       ++ sortedMaps
       ++ sortedOther
       )
  where
    categorise :: FieldSpec -> (Cat, FieldSpec)
    categorise f
      | isPrimitiveTypeId (fsTypeId f) =
          if fsNullable f then (NB, f) else (B, f)
      | fsTypeId f == T.SET   = (Set_, f)
      | fsTypeId f == T.LIST  = (Coll, f)
      | fsTypeId f == T.MAP   = (Map_, f)
      | otherwise =
          -- Strings, binary, dates, etc.: internal types.
          if isInternalTypeId (fsTypeId f)
            then (Int_, f)
            else (Other, f)

    primSorter a b =
      let key f = ( isCompressedPrimitive (fsTypeId f)
                  , Down (primitiveTypeSize (fsTypeId f))
                  , Down (typeIdInt (fsTypeId f))
                  , fsName f
                  )
      in compare (key a) (key b)

    genericSorter a b =
      compare (typeIdInt (fsTypeId a), fsName a)
              (typeIdInt (fsTypeId b), fsName b)

    typeIdInt :: TypeId -> Int
    typeIdInt (TypeId w) = fromIntegral w

data Cat = B | NB | Coll | Set_ | Map_ | Int_ | Other

-- | Internal type ids that 'group_fields' assigns to the
-- @internal_types@ bucket: anything strictly between 'UNKNOWN'
-- and 'BOUND' that isn't already routed to boxed / collection /
-- set / map / other. Strings + binary fall here.
isInternalTypeId :: TypeId -> Bool
isInternalTypeId t = case t of
  T.STRING -> True
  T.BINARY -> True
  T.DURATION -> True
  T.TIMESTAMP -> True
  T.DATE -> True
  T.DECIMAL -> True
  -- Primitive 1-D arrays count as internal too.
  T.BOOL_ARRAY    -> True
  T.INT8_ARRAY    -> True
  T.INT16_ARRAY   -> True
  T.INT32_ARRAY   -> True
  T.INT64_ARRAY   -> True
  T.UINT8_ARRAY   -> True
  T.UINT16_ARRAY  -> True
  T.UINT32_ARRAY  -> True
  T.UINT64_ARRAY  -> True
  T.FLOAT32_ARRAY -> True
  T.FLOAT64_ARRAY -> True
  _ -> False

