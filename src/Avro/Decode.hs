{-# LANGUAGE BangPatterns #-}
-- | High-level Avro binary decoding.
--
-- Schema-driven: the writer's schema is required to interpret the raw bytes.
-- Uses the wire primitives from "Avro.Wire".
module Avro.Decode
  ( decodeAvro
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Vector as V

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..))
import qualified Avro.Value as AV
import Avro.Wire
  ( AvroDecodeResult(..)
  , avroDecodeNull, avroDecodeBool, avroDecodeInt, avroDecodeLong
  , avroDecodeFloat, avroDecodeDouble, avroDecodeBytes, avroDecodeString
  )

-- | Decode a value from a strict 'ByteString' given its schema.
-- Returns 'Left' on any wire-format error.
decodeAvro :: AvroType -> ByteString -> Either String AV.Value
decodeAvro !ty !bs = case decodeValue ty bs 0 of
  AvroDecodeOK val off
    | off == BS.length bs -> Right val
    | otherwise -> Left $ "Avro.Decode: " ++ show (BS.length bs - off) ++ " trailing bytes"
  AvroDecodeFail e -> Left e

decodeValue :: AvroType -> ByteString -> Int -> AvroDecodeResult AV.Value
decodeValue (AvroPrimitive s) bs off = decodePrimitive s bs off
decodeValue (AvroRecord {avroRecordFields = fields}) bs off =
  decodeRecord (V.toList fields) bs off []
decodeValue (AvroEnum {}) bs off =
  case avroDecodeInt bs off of
    AvroDecodeOK n off' -> AvroDecodeOK (AV.Enum (fromIntegral n)) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodeValue (AvroArray {avroArrayItems = itemTy}) bs off =
  decodeArray itemTy bs off
decodeValue (AvroMap {avroMapValues = valTy}) bs off =
  decodeMap valTy bs off
decodeValue (AvroUnion {avroUnionBranches = branches}) bs off =
  case avroDecodeLong bs off of
    AvroDecodeOK idxI64 off' ->
      let !idx = fromIntegral idxI64 :: Int
      in if idx < 0 || idx >= V.length branches
         then AvroDecodeFail $ "Avro.Decode: union index out of range: " ++ show idx
         else case decodeValue (V.unsafeIndex branches idx) bs off' of
           AvroDecodeOK val off'' -> AvroDecodeOK (AV.Union idx val) off''
           AvroDecodeFail e       -> AvroDecodeFail e
    AvroDecodeFail e -> AvroDecodeFail e
decodeValue (AvroFixed {avroFixedSize = sz}) bs off
  | off + sz > BS.length bs = AvroDecodeFail "Avro.Decode: fixed: unexpected end of input"
  | otherwise = AvroDecodeOK (AV.Fixed (BS.take sz (BS.drop off bs))) (off + sz)
decodeValue (AvroLogical {avroLogicalBase = base}) bs off =
  decodeValue base bs off

decodePrimitive :: AvroSchema -> ByteString -> Int -> AvroDecodeResult AV.Value
decodePrimitive AvroNull bs off =
  case avroDecodeNull bs off of
    AvroDecodeOK _ off' -> AvroDecodeOK AV.Null off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroBool bs off =
  case avroDecodeBool bs off of
    AvroDecodeOK b off' -> AvroDecodeOK (AV.Bool b) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroInt bs off =
  case avroDecodeInt bs off of
    AvroDecodeOK n off' -> AvroDecodeOK (AV.Int n) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroLong bs off =
  case avroDecodeLong bs off of
    AvroDecodeOK n off' -> AvroDecodeOK (AV.Long n) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroFloat bs off =
  case avroDecodeFloat bs off of
    AvroDecodeOK f off' -> AvroDecodeOK (AV.Float f) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroDouble bs off =
  case avroDecodeDouble bs off of
    AvroDecodeOK d off' -> AvroDecodeOK (AV.Double d) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroBytes bs off =
  case avroDecodeBytes bs off of
    AvroDecodeOK b off' -> AvroDecodeOK (AV.Bytes b) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive AvroString bs off =
  case avroDecodeString bs off of
    AvroDecodeOK t off' -> AvroDecodeOK (AV.String t) off'
    AvroDecodeFail e    -> AvroDecodeFail e
decodePrimitive s _ _ = AvroDecodeFail $ "Avro.Decode: unsupported primitive: " ++ show s

decodeRecord :: [AvroField] -> ByteString -> Int -> [AV.Value] -> AvroDecodeResult AV.Value
decodeRecord [] _bs off acc = AvroDecodeOK (AV.Record (V.fromList (reverse acc))) off
decodeRecord (f:fs) bs off acc =
  case decodeValue (avroFieldType f) bs off of
    AvroDecodeOK val off' -> decodeRecord fs bs off' (val : acc)
    AvroDecodeFail e      -> AvroDecodeFail e

decodeArray :: AvroType -> ByteString -> Int -> AvroDecodeResult AV.Value
decodeArray itemTy bs off0 = decodeArrayBlocks bs off0 []
  where
    decodeArrayBlocks :: ByteString -> Int -> [AV.Value] -> AvroDecodeResult AV.Value
    decodeArrayBlocks bsB off acc =
      case avroDecodeLong bsB off of
        AvroDecodeOK cnt64 off' ->
          let !cnt = cnt64
          in if cnt == 0
             then AvroDecodeOK (AV.Array (V.fromList (reverse acc))) off'
             else if cnt < 0
             then case avroDecodeLong bsB off' of
               AvroDecodeOK _blockSize off'' ->
                 decodeArrayItems bsB off'' (fromIntegral (negate cnt)) acc
               AvroDecodeFail e -> AvroDecodeFail e
             else decodeArrayItems bsB off' (fromIntegral cnt) acc
        AvroDecodeFail e -> AvroDecodeFail e

    decodeArrayItems :: ByteString -> Int -> Int -> [AV.Value] -> AvroDecodeResult AV.Value
    decodeArrayItems bsI off 0 acc = decodeArrayBlocks bsI off acc
    decodeArrayItems bsI off n acc =
      case decodeValue itemTy bsI off of
        AvroDecodeOK val off' -> decodeArrayItems bsI off' (n - 1) (val : acc)
        AvroDecodeFail e      -> AvroDecodeFail e

decodeMap :: AvroType -> ByteString -> Int -> AvroDecodeResult AV.Value
decodeMap valTy bs off0 = decodeMapBlocks bs off0 []
  where
    decodeMapBlocks :: ByteString -> Int -> [(Text, AV.Value)] -> AvroDecodeResult AV.Value
    decodeMapBlocks bsB off acc =
      case avroDecodeLong bsB off of
        AvroDecodeOK cnt64 off' ->
          let !cnt = cnt64
          in if cnt == 0
             then AvroDecodeOK (AV.Map (V.fromList (reverse acc))) off'
             else if cnt < 0
             then case avroDecodeLong bsB off' of
               AvroDecodeOK _blockSize off'' ->
                 decodeMapEntries bsB off'' (fromIntegral (negate cnt)) acc
               AvroDecodeFail e -> AvroDecodeFail e
             else decodeMapEntries bsB off' (fromIntegral cnt) acc
        AvroDecodeFail e -> AvroDecodeFail e

    decodeMapEntries :: ByteString -> Int -> Int -> [(Text, AV.Value)] -> AvroDecodeResult AV.Value
    decodeMapEntries bsE off 0 acc = decodeMapBlocks bsE off acc
    decodeMapEntries bsE off n acc =
      case avroDecodeString bsE off of
        AvroDecodeOK key off' ->
          case decodeValue valTy bsE off' of
            AvroDecodeOK val off'' -> decodeMapEntries bsE off'' (n - 1) ((key, val) : acc)
            AvroDecodeFail e       -> AvroDecodeFail e
        AvroDecodeFail e -> AvroDecodeFail e
