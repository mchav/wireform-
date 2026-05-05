{-# LANGUAGE BangPatterns #-}
-- | Apache Fory xlang value encoder.
--
-- 'encode' lays out a 'Value' tree as
--
-- @
-- | fory header (1 byte) | NOT_NULL_VALUE_FLAG | type id | payload |
-- @
--
-- with the @xlang@ flag bit set in the header and reference
-- tracking disabled (so the only ref flags ever emitted are
-- @NULL@ for a top-level @None@ and @NOT_NULL_VALUE@ for non-null
-- values).
--
-- Scope and divergences from the published xlang spec:
--
-- * Reference tracking is always off; we never emit @REF_VALUE@ or
--   @REF@ flags. This matches the cross-language default for
--   non-cyclic data and keeps the encoder pure.
-- * Meta-string deduplication is disabled (see 'Fury.MetaString'),
--   so a @NAMED_STRUCT@ pays a fresh namespace + type-name on
--   every occurrence.
-- * Structs use only the @NAMED_STRUCT@ shape with a per-field
--   @field_name + value@ pair list. The TypeDef sidecar described
--   in the spec for schema evolution is not produced here.
-- * The dynamic 'Value' layer only carries 'ListVal' / 'SetVal' /
--   'MapVal' for collections; 1D primitive-array fast paths
--   (BOOL_ARRAY, INT8_ARRAY, …) are not yet emitted by this
--   encoder.
--
-- The 'Fury.Decode' decoder accepts everything this module emits.
module Fury.Encode
  ( encode
  , encodeBuilder
  , encodeValue
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word32)

import qualified Fury.Encoding as E
import qualified Fury.MetaString as MS
import qualified Fury.TypeId as T
import qualified Fury.Value as VV

-- | Encode a 'Value' to a fory xlang byte sequence.
encode :: VV.Value -> ByteString
encode = E.runBuilder . encodeBuilder

-- | Like 'encode' but exposes the inner builder for callers that
-- want to compose multiple encodings without intervening
-- @ByteString@ allocations.
encodeBuilder :: VV.Value -> E.Builder
encodeBuilder v =
     E.byte E.foryXlangHeader
  <> case v of
       VV.NoneVal -> E.byte E.refFlagNull
       _          -> E.byte E.refFlagNotNullValue <> encodeValue v

-- | Encode a value and its leading type tag, without the outer
-- fory header or ref flag. Useful when embedding a value inside a
-- larger structure (for example as a struct field).
encodeValue :: VV.Value -> E.Builder
encodeValue val = case val of
  VV.NoneVal       -> tag T.NONE
  VV.BoolVal b     -> tag T.BOOL    <> E.byte (if b then 1 else 0)
  VV.Int8Val n     -> tag T.INT8    <> E.byte (fromIntegral n)
  VV.Int16Val n    -> tag T.INT16   <> E.int16LE n
  VV.Int32Val n    -> tag T.INT32   <> E.int32LE n
  VV.Int64Val n    -> tag T.INT64   <> E.int64LE n
  VV.Uint8Val n    -> tag T.UINT8   <> E.byte n
  VV.Uint16Val n   -> tag T.UINT16  <> E.word16LE n
  VV.Uint32Val n   -> tag T.UINT32  <> E.word32LE n
  VV.Uint64Val n   -> tag T.UINT64  <> E.word64LE n
  VV.Float32Val f  -> tag T.FLOAT32 <> E.float32LE f
  VV.Float64Val d  -> tag T.FLOAT64 <> E.float64LE d
  VV.StringVal s   -> tag T.STRING  <> E.utf8String s
  VV.BinaryVal bs  -> tag T.BINARY  <> binaryPayload bs
  VV.ListVal vs    -> tag T.LIST    <> collectionPayload vs
  VV.SetVal vs     -> tag T.SET     <> collectionPayload vs
  VV.MapVal kvs    -> tag T.MAP     <> mapPayload kvs
  VV.StructVal ns nm fields ->
       tag T.NAMED_STRUCT
    <> MS.metaString ns
    <> MS.metaString nm
    <> structFieldsPayload fields
  where
    tag :: T.TypeId -> E.Builder
    tag (T.TypeId w) = E.byte w

binaryPayload :: ByteString -> E.Builder
binaryPayload !bs =
     E.varuint32 (fromIntegral (BS.length bs) :: Word32)
  <> E.bytes bs

collectionPayload :: Vector VV.Value -> E.Builder
collectionPayload vs =
     E.varuint32 (fromIntegral (V.length vs) :: Word32)
  <> V.foldl' (\acc x -> acc <> encodeValue x) mempty vs

mapPayload :: Vector (VV.Value, VV.Value) -> E.Builder
mapPayload kvs =
     E.varuint32 (fromIntegral (V.length kvs) :: Word32)
  <> V.foldl'
       (\acc (k, v) -> acc <> encodeValue k <> encodeValue v)
       mempty
       kvs

-- | Encode a struct's fields as a varuint32-prefixed sequence of
-- @(meta-string field name, value)@ pairs. Used both at the top
-- level (after a @NAMED_STRUCT@ tag + namespace + type name) and
-- by 'Fury.Class' instances for derived struct types.
structFieldsPayload
  :: VV.StructFields
  -> E.Builder
structFieldsPayload fields =
     E.varuint32 (fromIntegral (V.length fields) :: Word32)
  <> V.foldl'
       (\acc (k, v) -> acc <> MS.metaString k <> encodeValue v)
       mempty
       fields
