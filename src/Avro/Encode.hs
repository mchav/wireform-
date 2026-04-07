{-# LANGUAGE BangPatterns #-}
-- | High-level Avro binary encoding.
--
-- Encodes an 'Avro.Value.Value' according to an 'AvroType' schema using the
-- wire primitives from "Avro.Wire".
module Avro.Encode
  ( encodeAvro
  , encodeAvroBuilder
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Vector as V

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..))
import qualified Avro.Value as AV
import Avro.Wire
  ( avroEncodeNull, avroEncodeBool, avroEncodeInt, avroEncodeLong
  , avroEncodeFloat, avroEncodeDouble, avroEncodeBytes, avroEncodeString
  )

-- | Encode a value according to its Avro schema, returning strict 'ByteString'.
encodeAvro :: AvroType -> AV.Value -> ByteString
encodeAvro !ty !val = BL.toStrict (B.toLazyByteString (encodeAvroBuilder ty val))
{-# INLINE encodeAvro #-}

-- | Encode a value as a 'Builder' for zero-copy composition.
encodeAvroBuilder :: AvroType -> AV.Value -> Builder
encodeAvroBuilder = go
  where
    go :: AvroType -> AV.Value -> Builder
    go (AvroPrimitive s) v = encodePrimitive s v
    go (AvroRecord {avroRecordFields = fields}) (AV.Record vals) =
      encodeRecord (V.toList fields) (V.toList vals)
    go (AvroEnum {}) (AV.Enum idx) =
      avroEncodeInt (fromIntegral idx)
    go (AvroArray {avroArrayItems = itemTy}) (AV.Array items) =
      encodeArray itemTy (V.toList items)
    go (AvroMap {avroMapValues = valTy}) (AV.Map entries) =
      encodeMap valTy (V.toList entries)
    go (AvroUnion {avroUnionBranches = branches}) (AV.Union idx val) =
      avroEncodeLong (fromIntegral idx) <> go (V.unsafeIndex branches idx) val
    go (AvroFixed {}) (AV.Fixed bs) =
      B.byteString bs
    go (AvroLogical {avroLogicalBase = base}) v =
      go base v
    go _ v = error $ "Avro.Encode: schema/value mismatch: " ++ show v

    encodePrimitive :: AvroSchema -> AV.Value -> Builder
    encodePrimitive AvroNull   AV.Null       = avroEncodeNull
    encodePrimitive AvroBool   (AV.Bool b)   = avroEncodeBool b
    encodePrimitive AvroInt    (AV.Int n)    = avroEncodeInt n
    encodePrimitive AvroLong   (AV.Long n)   = avroEncodeLong n
    encodePrimitive AvroFloat  (AV.Float f)  = avroEncodeFloat f
    encodePrimitive AvroDouble (AV.Double d)  = avroEncodeDouble d
    encodePrimitive AvroBytes  (AV.Bytes bs)  = avroEncodeBytes bs
    encodePrimitive AvroString (AV.String t)  = avroEncodeString t
    encodePrimitive s v = error $ "Avro.Encode: primitive mismatch: " ++ show s ++ " vs " ++ show v

    encodeRecord :: [AvroField] -> [AV.Value] -> Builder
    encodeRecord [] [] = mempty
    encodeRecord (f:fs) (v:vs) =
      go (avroFieldType f) v <> encodeRecord fs vs
    encodeRecord _ _ = error "Avro.Encode: record field count mismatch"

    encodeArray :: AvroType -> [AV.Value] -> Builder
    encodeArray _itemTy [] = avroEncodeLong 0
    encodeArray itemTy items =
      let !cnt = length items
      in avroEncodeLong (fromIntegral cnt)
         <> foldMap (go itemTy) items
         <> avroEncodeLong 0

    encodeMap :: AvroType -> [(Text, AV.Value)] -> Builder
    encodeMap _valTy [] = avroEncodeLong 0
    encodeMap valTy entries =
      let !cnt = length entries
      in avroEncodeLong (fromIntegral cnt)
         <> foldMap (\(k, v) -> avroEncodeString k <> go valTy v) entries
         <> avroEncodeLong 0
