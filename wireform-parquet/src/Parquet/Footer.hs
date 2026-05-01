{-# LANGUAGE BangPatterns #-}
-- | Read/write Apache Parquet file footer.
--
-- Parquet file layout ends with:
--   [Thrift Compact Protocol encoded FileMetadata] [4-byte LE metadata length] [PAR1 magic]
--
-- We use the existing Thrift Compact Protocol encoder/decoder to serialize
-- the FileMetadata as a Thrift struct.
module Parquet.Footer
  ( readFooter
  , readFooterRaw
  , writeFooter
  , writeRawFooter
  , fileMetadataToThrift
  , thriftToFileMetadata
  , parquetMagic
  , parquetEncryptedMagic
    -- * Trailer parsing
  , FooterTrailer (..)
  , readFooterTrailer
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Data.Word (Word32)
import qualified Data.Vector as V

import Parquet.Types
import qualified Thrift.Value as TV
import qualified Thrift.Wire as TW
import Thrift.Encode (encodeCompact)
import Thrift.Decode (decodeCompact)

parquetMagic :: ByteString
parquetMagic = BS.pack [0x50, 0x41, 0x52, 0x31]

-- | The trailing magic for an encrypted-footer Parquet file: @PARE@.
-- Replaces 'parquetMagic' in the encrypted-footer mode so a reader
-- can distinguish a file whose footer is itself an AES-GCM module
-- from a plaintext-footer file whose columns happen to be encrypted.
parquetEncryptedMagic :: ByteString
parquetEncryptedMagic = BS.pack [0x50, 0x41, 0x52, 0x45]

writeFooter :: FileMetadata -> ByteString
writeFooter fm =
  let !thriftVal = fileMetadataToThrift fm
      !encoded = encodeCompact thriftVal
   in writeRawFooter parquetMagic encoded

-- | Build a footer trailer from already-encoded thrift bytes plus a
-- magic number. Encrypted-footer mode passes 'parquetEncryptedMagic'
-- and the GCM-encrypted thrift bytes; plaintext-footer mode passes
-- 'parquetMagic' and the raw thrift.
writeRawFooter :: ByteString -> ByteString -> ByteString
writeRawFooter magic encoded =
  let !metaLen = BS.length encoded
   in BL.toStrict $ B.toLazyByteString $
        B.byteString encoded
        <> B.word8 (fromIntegral (metaLen .&. 0xFF))
        <> B.word8 (fromIntegral ((metaLen `shiftR` 8) .&. 0xFF))
        <> B.word8 (fromIntegral ((metaLen `shiftR` 16) .&. 0xFF))
        <> B.word8 (fromIntegral ((metaLen `shiftR` 24) .&. 0xFF))
        <> B.byteString magic

-- | The bytes the footer trailer points at, plus which magic ended
-- the file. For a plaintext-footer file (@PAR1@) the bytes are the
-- compact-encoded 'FileMetadata' thrift; for an encrypted-footer
-- file (@PARE@) they are the AES-GCM module the caller must
-- decrypt before parsing the same thrift.
data FooterTrailer = FooterTrailer
  { ftMagic    :: !ByteString
    -- ^ Either 'parquetMagic' or 'parquetEncryptedMagic'.
  , ftBytes    :: !ByteString
    -- ^ Footer bytes (raw thrift for plaintext, GCM module for
    --   encrypted).
  } deriving (Show, Eq)

-- | Parse the trailing 8 bytes (length + magic) and slice out the
-- footer module bytes. Doesn't decode them - that's the caller's
-- decision based on which magic appeared.
readFooterTrailer :: ByteString -> Either String FooterTrailer
readFooterTrailer bs
  | BS.length bs < 8 = Left "Parquet.Footer: input too short"
  | otherwise =
      let !totalLen = BS.length bs
          !magic = BSU.unsafeTake 4 (BSU.unsafeDrop (totalLen - 4) bs)
       in if magic /= parquetMagic && magic /= parquetEncryptedMagic
            then Left "Parquet.Footer: invalid magic (expected PAR1 or PARE)"
            else
              let !metaLenOff = totalLen - 8
                  !metaLen    = fromIntegral (readLE32 bs metaLenOff) :: Int
               in if metaLen < 0 || metaLen > totalLen - 8
                    then Left "Parquet.Footer: invalid metadata length"
                    else
                      let !metaStart = totalLen - 8 - metaLen
                          !metaBytes = BSU.unsafeTake metaLen
                                        (BSU.unsafeDrop metaStart bs)
                       in Right (FooterTrailer magic metaBytes)

-- | Parse the plaintext-thrift bytes of a footer (i.e. what
-- 'readFooterTrailer' returns for a @PAR1@ file or what the caller
-- got after decrypting a @PARE@ footer module).
readFooterRaw :: ByteString -> Either String FileMetadata
readFooterRaw thriftBytes = do
  thriftVal <- decodeCompact thriftBytes
  thriftToFileMetadata thriftVal

readFooter :: ByteString -> Either String FileMetadata
readFooter bs = do
  trailer <- readFooterTrailer bs
  if ftMagic trailer == parquetEncryptedMagic
    then Left "Parquet.Footer: file has encrypted footer (PARE); use loadParquetFileEncrypted"
    else readFooterRaw (ftBytes trailer)

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off =
  let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
      !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
      !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
      !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

-- Thrift field IDs for FileMetadata:
-- 1: version (i32), 2: schema (list<SchemaElement>), 3: num_rows (i64),
-- 4: row_groups (list<RowGroup>), 5: created_by (string)

fileMetadataToThrift :: FileMetadata -> TV.Value
fileMetadataToThrift fm = TV.Struct $ V.fromList $
  [ (1, TV.I32 (fmVersion fm))
  , (2, TV.List TW.TT_STRUCT (V.map schemaElementToThrift (fmSchema fm)))
  , (3, TV.I64 (fmNumRows fm))
  , (4, TV.List TW.TT_STRUCT (V.map rowGroupToThrift (fmRowGroups fm)))
  ] ++ maybe [] (\t -> [(5, TV.String t)]) (fmCreatedBy fm)

-- | Encode a SchemaElement matching parquet.thrift exactly:
--
--   1: optional Type                    type
--   2: optional i32                     type_length
--   3: optional FieldRepetitionType     repetition_type
--   4: required string                  name
--   5: optional i32                     num_children
--   6: optional ConvertedType           converted_type
--   7: optional i32                     scale
--   8: optional i32                     field_id
schemaElementToThrift :: SchemaElement -> TV.Value
schemaElementToThrift se = TV.Struct $ V.fromList $
  maybe [] (\t -> [(1, TV.I32 (parquetTypeToInt t))]) (seType se)
  ++ maybe [] (\r -> [(3, TV.I32 (fromIntegral (fromEnum r)))]) (seRepetition se)
  ++ [(4, TV.String (seName se))]
  ++ maybe [] (\n -> [(5, TV.I32 n)]) (seNumChildren se)
  ++ maybe [] (\c -> [(6, TV.I32 (fromIntegral (fromEnum c)))]) (seConvertedType se)
  ++ maybe [] (\fid -> [(8, TV.I32 fid)]) (seFieldId se)

rowGroupToThrift :: RowGroup -> TV.Value
rowGroupToThrift rg = TV.Struct $ V.fromList
  [ (1, TV.List TW.TT_STRUCT (V.map columnChunkToThrift (rgColumns rg)))
  , (2, TV.I64 (rgTotalByteSize rg))
  , (3, TV.I64 (rgNumRows rg))
  ]

columnChunkToThrift :: ColumnChunk -> TV.Value
columnChunkToThrift cc = TV.Struct $ V.fromList $
  maybe [] (\fp -> [(1, TV.String fp)]) (ccFilePath cc)
  ++ [ (2, TV.I64 (ccFileOffset cc)) ]
  ++ maybe [] (\cm -> [(3, columnMetadataToThrift cm)]) (ccMetadata cc)
  -- field 4 (offset_index_offset) - 6 are reserved by parquet.thrift for
  -- offset_index_length / column_index_offset / column_index_length when
  -- carried inline. We mirror the upstream layout:
  --   4: optional i64 offset_index_offset
  --   5: optional i32 offset_index_length
  --   6: optional i64 column_index_offset
  --   7: optional i32 column_index_length
  -- Older wireform writers omitted these fields entirely, which decoders
  -- treat as @Nothing@.
  ++ maybe [] (\v -> [(4, TV.I64 v)]) (ccOffsetIndexOffset cc)
  ++ maybe [] (\v -> [(5, TV.I32 v)]) (ccOffsetIndexLength cc)
  ++ maybe [] (\v -> [(6, TV.I64 v)]) (ccColumnIndexOffset cc)
  ++ maybe [] (\v -> [(7, TV.I32 v)]) (ccColumnIndexLength cc)

-- | Encode ColumnMetaData per parquet.thrift field numbers:
--
--   1 type, 2 encodings, 3 path_in_schema, 4 codec, 5 num_values,
--   6 total_uncompressed_size, 7 total_compressed_size,
--   8 key_value_metadata (skipped), 9 data_page_offset,
--   10 index_page_offset, 11 dictionary_page_offset,
--   12 statistics, 13 encoding_stats (skipped),
--   14 bloom_filter_offset, 15 bloom_filter_length.
columnMetadataToThrift :: ColumnMetadata -> TV.Value
columnMetadataToThrift cm = TV.Struct $ V.fromList $
  [ (1, TV.I32 (parquetTypeToInt (cmType cm)))
  , (2, TV.List TW.TT_I32 (V.map (TV.I32 . encodingToInt) (cmEncodings cm)))
  , (3, TV.List TW.TT_STRING (V.map TV.String (cmPathInSchema cm)))
  , (4, TV.I32 (compressionToInt (cmCodec cm)))
  , (5, TV.I64 (cmNumValues cm))
  , (6, TV.I64 (cmTotalUncompressedSize cm))
  , (7, TV.I64 (cmTotalCompressedSize cm))
  , (9, TV.I64 (cmDataPageOffset cm))
  ] ++ maybe [] (\s -> [(12, statisticsToThrift s)]) (cmStatistics cm)
  ++ maybe [] (\v -> [(14, TV.I64 v)]) (cmBloomFilterOffset cm)
  ++ maybe [] (\v -> [(15, TV.I32 v)]) (cmBloomFilterLength cm)

statisticsToThrift :: Statistics -> TV.Value
statisticsToThrift st = TV.Struct $ V.fromList $
  maybe [] (\v -> [(1, TV.Binary v)]) (statMax st)
  ++ maybe [] (\v -> [(2, TV.Binary v)]) (statMin st)
  ++ maybe [] (\v -> [(3, TV.I64 v)]) (statNullCount st)
  ++ maybe [] (\v -> [(4, TV.I64 v)]) (statDistinctCount st)
  ++ maybe [] (\v -> [(5, TV.Binary v)]) (statMaxValue st)
  ++ maybe [] (\v -> [(6, TV.Binary v)]) (statMinValue st)

encodingToInt :: Encoding -> Int32
encodingToInt = \case
  Plain               -> 0
  PlainDictionary     -> 2
  RLE                 -> 3
  BitPacked           -> 4
  DeltaBinaryPacked   -> 5
  DeltaLengthByteArray -> 6
  DeltaByteArray      -> 7
  RLEDictionary       -> 8
  ByteStreamSplit     -> 9

intToEncoding :: Int32 -> Maybe Encoding
intToEncoding = \case
  0 -> Just Plain
  2 -> Just PlainDictionary
  3 -> Just RLE
  4 -> Just BitPacked
  5 -> Just DeltaBinaryPacked
  6 -> Just DeltaLengthByteArray
  7 -> Just DeltaByteArray
  8 -> Just RLEDictionary
  9 -> Just ByteStreamSplit
  _ -> Nothing

compressionToInt :: Compression -> Int32
compressionToInt = \case
  Uncompressed -> 0
  Snappy       -> 1
  GZip         -> 2
  LZO          -> 3
  Brotli       -> 4
  LZ4          -> 5
  ZSTD         -> 6
  LZ4Raw       -> 7

intToCompression :: Int32 -> Maybe Compression
intToCompression = \case
  0 -> Just Uncompressed
  1 -> Just Snappy
  2 -> Just GZip
  3 -> Just LZO
  4 -> Just Brotli
  5 -> Just LZ4
  6 -> Just ZSTD
  7 -> Just LZ4Raw
  _ -> Nothing

-- Decoding from Thrift value back to our types

thriftToFileMetadata :: TV.Value -> Either String FileMetadata
thriftToFileMetadata (TV.Struct fields) = do
  let fm = V.toList fields
  version <- getI32 fm 1 "version"
  schema <- getListStruct fm 2 "schema" thriftToSchemaElement
  numRows <- getI64 fm 3 "num_rows"
  rowGroups <- getListStruct fm 4 "row_groups" thriftToRowGroup
  let createdBy = getOptionalString fm 5
  Right FileMetadata
    { fmVersion = version
    , fmSchema = schema
    , fmNumRows = numRows
    , fmRowGroups = rowGroups
    , fmCreatedBy = createdBy
    }
thriftToFileMetadata _ = Left "Parquet.Footer: expected struct"

thriftToSchemaElement :: TV.Value -> Either String SchemaElement
thriftToSchemaElement (TV.Struct fields) = do
  let fm = V.toList fields
  name <- getString fm 4 "schema name"
  let typ = case lookupField fm 1 of
              Just (TV.I32 t) -> intToParquetType t
              _ -> Nothing
      rep = case lookupField fm 3 of
              Just (TV.I32 r) -> Just (toEnum (fromIntegral r))
              _ -> Nothing
      numCh = case lookupField fm 5 of
                Just (TV.I32 n) -> Just n
                _ -> Nothing
      conv = case lookupField fm 6 of
               Just (TV.I32 c) | c >= 0, c <= 21 -> Just (toEnum (fromIntegral c))
               _ -> Nothing
      fid = case lookupField fm 8 of
              Just (TV.I32 v) -> Just v
              _ -> Nothing
  Right SchemaElement
    { seName = name
    , seRepetition = rep
    , seType = typ
    , seNumChildren = numCh
    , seConvertedType = conv
    , seLogicalType = Nothing
    , seFieldId = fid
    }
thriftToSchemaElement _ = Left "Parquet.Footer: expected struct for SchemaElement"

thriftToRowGroup :: TV.Value -> Either String RowGroup
thriftToRowGroup (TV.Struct fields) = do
  let fm = V.toList fields
  cols <- getListStruct fm 1 "columns" thriftToColumnChunk
  totalBytes <- getI64 fm 2 "total_byte_size"
  numRows <- getI64 fm 3 "num_rows"
  Right RowGroup
    { rgColumns = cols
    , rgTotalByteSize = totalBytes
    , rgNumRows = numRows
    }
thriftToRowGroup _ = Left "Parquet.Footer: expected struct for RowGroup"

thriftToColumnChunk :: TV.Value -> Either String ColumnChunk
thriftToColumnChunk (TV.Struct fields) = do
  let fm = V.toList fields
      fp = getOptionalString fm 1
  fileOff <- getI64 fm 2 "file_offset"
  meta <- case lookupField fm 3 of
            Just v  -> Just <$> thriftToColumnMetadata v
            Nothing -> Right Nothing
  let oio = getOptionalI64 fm 4
      oil = getOptionalI32 fm 5
      cio = getOptionalI64 fm 6
      cil = getOptionalI32 fm 7
  Right ColumnChunk
    { ccFilePath = fp
    , ccFileOffset = fileOff
    , ccMetadata = meta
    , ccOffsetIndexOffset = oio
    , ccOffsetIndexLength = oil
    , ccColumnIndexOffset = cio
    , ccColumnIndexLength = cil
    }
thriftToColumnChunk _ = Left "Parquet.Footer: expected struct for ColumnChunk"

thriftToColumnMetadata :: TV.Value -> Either String ColumnMetadata
thriftToColumnMetadata (TV.Struct fields) = do
  let fm = V.toList fields
  typeVal <- getI32 fm 1 "type"
  pt <- maybe (Left "Parquet.Footer: invalid parquet type") Right (intToParquetType typeVal)
  encodings <- case lookupField fm 2 of
    Just (TV.List _ es) -> V.mapM (\case
      TV.I32 e -> maybe (Left "Parquet.Footer: invalid encoding") Right (intToEncoding e)
      _ -> Left "Parquet.Footer: expected i32 in encodings") es
    _ -> Left "Parquet.Footer: missing encodings"
  paths <- case lookupField fm 3 of
    Just (TV.List _ ps) -> V.mapM (\case
      TV.String t -> Right t
      _ -> Left "Parquet.Footer: expected string in path") ps
    _ -> Left "Parquet.Footer: missing path_in_schema"
  codecVal <- getI32 fm 4 "codec"
  codec <- maybe (Left "Parquet.Footer: invalid compression") Right (intToCompression codecVal)
  numVals <- getI64 fm 5 "num_values"
  uncompSz <- getI64 fm 6 "total_uncompressed_size"
  compSz <- getI64 fm 7 "total_compressed_size"
  -- field 8 = key_value_metadata (optional, ignored)
  dataOff <- getI64 fm 9 "data_page_offset"
  -- field 10 = index_page_offset (optional, ignored)
  -- field 11 = dictionary_page_offset (optional, ignored)
  let stats = case lookupField fm 12 of
        Just v -> case thriftToStatistics v of
          Right s -> Just s
          Left _  -> Nothing
        Nothing -> Nothing
      bfo = getOptionalI64 fm 14
      bfl = getOptionalI32 fm 15
  Right ColumnMetadata
    { cmType = pt
    , cmEncodings = encodings
    , cmPathInSchema = paths
    , cmCodec = codec
    , cmNumValues = numVals
    , cmTotalUncompressedSize = uncompSz
    , cmTotalCompressedSize = compSz
    , cmDataPageOffset = dataOff
    , cmStatistics = stats
    , cmBloomFilterOffset = bfo
    , cmBloomFilterLength = bfl
    }
thriftToColumnMetadata _ = Left "Parquet.Footer: expected struct for ColumnMetadata"

thriftToStatistics :: TV.Value -> Either String Statistics
thriftToStatistics (TV.Struct fields) = do
  let fm = V.toList fields
      -- Thrift Compact stores both binary and UTF-8 strings under
      -- TT_STRING; the decoder surfaces TV.String when the bytes
      -- happen to parse as UTF-8.  Stats values are arbitrary bytes
      -- (PLAIN-encoded primitives) so accept either shape.
      getBinary fid = case lookupField fm fid of
        Just (TV.Binary b) -> Just b
        Just (TV.String t) -> Just (Data.Text.Encoding.encodeUtf8 t)
        _ -> Nothing
      getOptI64 fid = case lookupField fm fid of
        Just (TV.I64 v) -> Just v
        _ -> Nothing
  Right Statistics
    { statMax = getBinary 1
    , statMin = getBinary 2
    , statNullCount = getOptI64 3
    , statDistinctCount = getOptI64 4
    , statMaxValue = getBinary 5
    , statMinValue = getBinary 6
    }
thriftToStatistics _ = Left "Parquet.Footer: expected struct for Statistics"

-- Helpers

lookupField :: [(Int16, TV.Value)] -> Int16 -> Maybe TV.Value
lookupField fm fid = lookup fid fm

getI32 :: [(Int16, TV.Value)] -> Int16 -> String -> Either String Int32
getI32 fm fid name = case lookupField fm fid of
  Just (TV.I32 v) -> Right v
  _ -> Left $ "Parquet.Footer: missing or invalid field " ++ name

getI64 :: [(Int16, TV.Value)] -> Int16 -> String -> Either String Int64
getI64 fm fid name = case lookupField fm fid of
  Just (TV.I64 v) -> Right v
  _ -> Left $ "Parquet.Footer: missing or invalid field " ++ name

getString :: [(Int16, TV.Value)] -> Int16 -> String -> Either String T.Text
getString fm fid name = case lookupField fm fid of
  Just (TV.String t) -> Right t
  _ -> Left $ "Parquet.Footer: missing or invalid field " ++ name

getOptionalString :: [(Int16, TV.Value)] -> Int16 -> Maybe T.Text
getOptionalString fm fid = case lookupField fm fid of
  Just (TV.String t) -> Just t
  _ -> Nothing

getOptionalI32 :: [(Int16, TV.Value)] -> Int16 -> Maybe Int32
getOptionalI32 fm fid = case lookupField fm fid of
  Just (TV.I32 v) -> Just v
  _ -> Nothing

getOptionalI64 :: [(Int16, TV.Value)] -> Int16 -> Maybe Int64
getOptionalI64 fm fid = case lookupField fm fid of
  Just (TV.I64 v) -> Just v
  _ -> Nothing

getListStruct :: [(Int16, TV.Value)] -> Int16 -> String
              -> (TV.Value -> Either String a) -> Either String (V.Vector a)
getListStruct fm fid name decode = case lookupField fm fid of
  Just (TV.List _ vs) -> V.mapM decode vs
  _ -> Left $ "Parquet.Footer: missing or invalid field " ++ name
