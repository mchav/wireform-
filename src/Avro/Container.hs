{-# LANGUAGE BangPatterns #-}
module Avro.Container
  ( ContainerHeader(..)
  , readContainer
  , writeContainer
  ) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Avro.Decode (decodeAvroAt)
import Avro.Encode (encodeAvro)
import Avro.JSON (avroSchemaFromJSON, avroSchemaToJSON)
import Avro.Schema (AvroType)
import qualified Avro.Value as AV
import Avro.Wire
  ( AvroDecodeResult(..)
  , avroDecodeLong
  , avroEncodeBytes
  , avroEncodeLong
  , avroDecodeBytes
  )

data ContainerHeader = ContainerHeader
  { containerSchema :: !AvroType
  , containerCodec  :: !T.Text
  , containerSync   :: !ByteString
  } deriving stock (Show, Eq)

magic :: ByteString
magic = BS.pack [0x4F, 0x62, 0x6A, 0x01]

syncMarkerSize :: Int
syncMarkerSize = 16

nullSync :: ByteString
nullSync = BS.replicate syncMarkerSize 0

-- | Write an Avro container file (Object Container File) with null codec.
writeContainer :: AvroType -> V.Vector AV.Value -> ByteString
writeContainer schema vals =
  BL.toStrict (B.toLazyByteString (writeContainerBuilder schema vals))

writeContainerBuilder :: AvroType -> V.Vector AV.Value -> B.Builder
writeContainerBuilder schema vals =
  let schemaJSON = BL.toStrict $ Aeson.encode (avroSchemaToJSON schema)
      meta = [ ("avro.schema", schemaJSON)
             , ("avro.codec", "null")
             ]
      headerBuilder =
        B.byteString magic
        <> encodeAvroMap meta
        <> B.byteString nullSync
      blockData = mconcat [encodeAvro schema v | v <- V.toList vals]
      blockCount = V.length vals
      blockBuilder =
        avroEncodeLong (fromIntegral blockCount)
        <> avroEncodeLong (fromIntegral (BS.length blockData))
        <> B.byteString blockData
        <> B.byteString nullSync
  in if V.null vals
     then headerBuilder
     else headerBuilder <> blockBuilder

encodeAvroMap :: [(ByteString, ByteString)] -> B.Builder
encodeAvroMap [] = avroEncodeLong 0
encodeAvroMap entries =
  avroEncodeLong (fromIntegral (length entries))
  <> foldMap (\(k, v) -> avroEncodeBytes k <> avroEncodeBytes v) entries
  <> avroEncodeLong 0

-- | Read an Avro container file, returning the schema and all values.
readContainer :: ByteString -> Either String (AvroType, V.Vector AV.Value)
readContainer bs = do
  (hdr, off) <- parseHeader bs
  vals <- parseBlocks (containerSchema hdr) (containerSync hdr) bs off []
  Right (containerSchema hdr, V.fromList (reverse vals))

parseHeader :: ByteString -> Either String (ContainerHeader, Int)
parseHeader bs
  | BS.length bs < 4 = Left "Avro.Container: too short for magic"
  | BS.take 4 bs /= magic = Left "Avro.Container: invalid magic bytes"
  | otherwise = do
      (meta, off) <- decodeAvroMap bs 4
      let schemaBS = lookup "avro.schema" meta
          codecBS  = lookup "avro.codec" meta
      schema <- case schemaBS of
        Nothing -> Left "Avro.Container: missing avro.schema in header"
        Just s  -> case Aeson.decodeStrict s of
          Nothing -> Left "Avro.Container: invalid JSON in avro.schema"
          Just j  -> avroSchemaFromJSON j
      let codec = case codecBS of
            Just c  -> case TE.decodeUtf8' c of
              Right t -> t
              Left _  -> "null"
            Nothing -> "null"
      if off + syncMarkerSize > BS.length bs
        then Left "Avro.Container: not enough bytes for sync marker"
        else do
          let sync = BS.take syncMarkerSize (BS.drop off bs)
          Right (ContainerHeader schema codec sync, off + syncMarkerSize)

decodeAvroMap :: ByteString -> Int -> Either String ([(ByteString, ByteString)], Int)
decodeAvroMap bs off = decodeMapBlocks bs off []
  where
    decodeMapBlocks :: ByteString -> Int -> [(ByteString, ByteString)] -> Either String ([(ByteString, ByteString)], Int)
    decodeMapBlocks bs' off' acc =
      case avroDecodeLong bs' off' of
        AvroDecodeFail e -> Left e
        AvroDecodeOK cnt off'' ->
          if cnt == 0
          then Right (reverse acc, off'')
          else if cnt < 0
          then case avroDecodeLong bs' off'' of
            AvroDecodeFail e -> Left e
            AvroDecodeOK _blockSz off''' ->
              decodeMapEntries bs' off''' (fromIntegral (negate cnt)) acc
          else decodeMapEntries bs' off'' (fromIntegral cnt) acc

    decodeMapEntries :: ByteString -> Int -> Int -> [(ByteString, ByteString)] -> Either String ([(ByteString, ByteString)], Int)
    decodeMapEntries _bs' off' 0 acc = decodeMapBlocks _bs' off' acc
    decodeMapEntries bs' off' n acc =
      case avroDecodeBytes bs' off' of
        AvroDecodeFail e -> Left e
        AvroDecodeOK key off'' ->
          case avroDecodeBytes bs' off'' of
            AvroDecodeFail e -> Left e
            AvroDecodeOK val off''' ->
              decodeMapEntries bs' off''' (n - 1) ((key, val) : acc)

parseBlocks :: AvroType -> ByteString -> ByteString -> Int -> [AV.Value] -> Either String [AV.Value]
parseBlocks schema sync bs off acc
  | off >= BS.length bs = Right acc
  | otherwise = do
      (cnt64, off') <- decodeLong' bs off
      let !cnt = fromIntegral cnt64 :: Int
      (_byteSz64, off'') <- decodeLong' bs off'
      (vals, off''') <- decodeNValues schema bs off'' cnt acc
      if off''' + syncMarkerSize > BS.length bs
        then Left "Avro.Container: block sync marker truncated"
        else do
          let blockSync = BS.take syncMarkerSize (BS.drop off''' bs)
          if blockSync /= sync
            then Left "Avro.Container: sync marker mismatch"
            else parseBlocks schema sync bs (off''' + syncMarkerSize) vals

decodeLong' :: ByteString -> Int -> Either String (Int64, Int)
decodeLong' bs off = case avroDecodeLong bs off of
  AvroDecodeFail e    -> Left e
  AvroDecodeOK v off' -> Right (v, off')

decodeNValues :: AvroType -> ByteString -> Int -> Int -> [AV.Value] -> Either String ([AV.Value], Int)
decodeNValues _schema _bs off 0 acc = Right (acc, off)
decodeNValues schema bs off n acc = do
  (val, off') <- decodeAvroAt schema bs off
  decodeNValues schema bs off' (n - 1) (val : acc)
