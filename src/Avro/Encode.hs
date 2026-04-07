{-# LANGUAGE BangPatterns #-}
-- | High-level Avro binary encoding.
--
-- Encodes an 'AvroValue' according to an 'AvroType' schema using the
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
import Avro.Value (AvroValue(..))
import Avro.Wire
  ( avroEncodeNull, avroEncodeBool, avroEncodeInt, avroEncodeLong
  , avroEncodeFloat, avroEncodeDouble, avroEncodeBytes, avroEncodeString
  )

-- | Encode a value according to its Avro schema, returning strict 'ByteString'.
encodeAvro :: AvroType -> AvroValue -> ByteString
encodeAvro !ty !val = BL.toStrict (B.toLazyByteString (encodeAvroBuilder ty val))
{-# INLINE encodeAvro #-}

-- | Encode a value as a 'Builder' for zero-copy composition.
encodeAvroBuilder :: AvroType -> AvroValue -> Builder
encodeAvroBuilder = go
  where
    go :: AvroType -> AvroValue -> Builder
    go (AvroPrimitive s) v = encodePrimitive s v
    go (AvroRecord {avroRecordFields = fields}) (AvRecord vals) =
      encodeRecord (V.toList fields) vals
    go (AvroEnum {}) (AvEnum idx) =
      avroEncodeInt (fromIntegral idx)
    go (AvroArray {avroArrayItems = itemTy}) (AvArray items) =
      encodeArray itemTy items
    go (AvroMap {avroMapValues = valTy}) (AvMap entries) =
      encodeMap valTy entries
    go (AvroUnion {avroUnionBranches = branches}) (AvUnion idx val) =
      avroEncodeLong (fromIntegral idx) <> go (V.unsafeIndex branches idx) val
    go (AvroFixed {}) (AvFixed bs) =
      B.byteString bs
    go (AvroLogical {avroLogicalBase = base}) v =
      go base v
    go _ v = error $ "Avro.Encode: schema/value mismatch: " ++ show v

    encodePrimitive :: AvroSchema -> AvroValue -> Builder
    encodePrimitive AvroNull   AvNull       = avroEncodeNull
    encodePrimitive AvroBool   (AvBool b)   = avroEncodeBool b
    encodePrimitive AvroInt    (AvInt n)     = avroEncodeInt n
    encodePrimitive AvroLong   (AvLong n)   = avroEncodeLong n
    encodePrimitive AvroFloat  (AvFloat f)  = avroEncodeFloat f
    encodePrimitive AvroDouble (AvDouble d)  = avroEncodeDouble d
    encodePrimitive AvroBytes  (AvBytes bs)  = avroEncodeBytes bs
    encodePrimitive AvroString (AvString t)  = avroEncodeString t
    encodePrimitive s v = error $ "Avro.Encode: primitive mismatch: " ++ show s ++ " vs " ++ show v

    encodeRecord :: [AvroField] -> [AvroValue] -> Builder
    encodeRecord [] [] = mempty
    encodeRecord (f:fs) (v:vs) =
      go (avroFieldType f) v <> encodeRecord fs vs
    encodeRecord _ _ = error "Avro.Encode: record field count mismatch"

    encodeArray :: AvroType -> [AvroValue] -> Builder
    encodeArray _itemTy [] = avroEncodeLong 0
    encodeArray itemTy items =
      let !cnt = length items
      in avroEncodeLong (fromIntegral cnt)
         <> foldMap (go itemTy) items
         <> avroEncodeLong 0

    encodeMap :: AvroType -> [(Text, AvroValue)] -> Builder
    encodeMap _valTy [] = avroEncodeLong 0
    encodeMap valTy entries =
      let !cnt = length entries
      in avroEncodeLong (fromIntegral cnt)
         <> foldMap (\(k, v) -> avroEncodeString k <> go valTy v) entries
         <> avroEncodeLong 0
