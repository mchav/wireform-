{-# LANGUAGE BangPatterns #-}
-- | Apache Parquet page index: 'OffsetIndex' + 'ColumnIndex' (Thrift).
--
-- The page index tier (added in parquet-format 2.5) lets readers skip
-- entire pages without scanning column chunks. It is stored separately
-- from the row-group metadata, near the file footer:
--
-- * 'OffsetIndex' — per-page @(offset, compressed_page_size, first_row_index)@
--   plus optional @unencoded_byte_array_data_bytes@ (size_statistics).
-- * 'ColumnIndex' — per-page @null_pages@, @min_values@, @max_values@,
--   plus a 'BoundaryOrder' and optional null/level histograms.
--
-- 'ColumnChunk' carries 'ccOffsetIndexOffset' / @offset_index_length@ and
-- 'ccColumnIndexOffset' / @column_index_length@; this module reads/writes
-- the structures themselves with the Thrift Compact Protocol used by
-- "Parquet.Footer".
module Parquet.PageIndex
  ( -- * OffsetIndex
    encodeOffsetIndex
  , decodeOffsetIndex
  , readOffsetIndex
    -- * ColumnIndex
  , encodeColumnIndex
  , decodeColumnIndex
  , readColumnIndex
    -- * Slicing
  , columnChunkOffsetIndexSlice
  , columnChunkColumnIndexSlice
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
import qualified Data.Text.Encoding
import qualified Data.Vector as V

import Parquet.Read (ParquetFile (..))
import Parquet.Types
  ( ColumnChunk (..)
  , ColumnIndex (..)
  , FileMetadata (..)
  , OffsetIndex (..)
  , PageLocation (..)
  , RowGroup (..)
  , boundaryOrderToInt
  , intToBoundaryOrder
  )
import qualified Thrift.Value as TV
import qualified Thrift.Wire as TW
import Thrift.Encode (encodeCompact)
import Thrift.Decode (decodeCompact)

-- ============================================================
-- OffsetIndex
-- ============================================================

-- | Serialize an 'OffsetIndex' in Thrift Compact Protocol.
encodeOffsetIndex :: OffsetIndex -> ByteString
encodeOffsetIndex = encodeCompact . offsetIndexToThrift

-- | Parse an 'OffsetIndex' from a Thrift Compact byte slice.
decodeOffsetIndex :: ByteString -> Either String OffsetIndex
decodeOffsetIndex bs = decodeCompact bs >>= thriftToOffsetIndex

-- | Look up the 'OffsetIndex' for the column chunk at @(rowGroupIdx, columnIdx)@
-- if 'ccOffsetIndexOffset' / 'ccOffsetIndexLength' are populated and within
-- file bounds.
readOffsetIndex
  :: ParquetFile
  -> Int -> Int
  -> Either String (Maybe OffsetIndex)
readOffsetIndex pf rg col = do
  msl <- columnChunkOffsetIndexSlice pf rg col
  case msl of
    Nothing -> Right Nothing
    Just bs -> Just <$> decodeOffsetIndex bs

-- | Slice a 'ParquetFile' to the bytes covering a column chunk's serialized
-- 'OffsetIndex'. Returns 'Nothing' when the column chunk did not record one.
columnChunkOffsetIndexSlice
  :: ParquetFile
  -> Int -> Int
  -> Either String (Maybe ByteString)
columnChunkOffsetIndexSlice pf rgIdx colIdx = do
  cc <- selectColumnChunk pf rgIdx colIdx
  case (ccOffsetIndexOffset cc, ccOffsetIndexLength cc) of
    (Just off, Just len) -> Just <$> sliceFile pf off len "offset index"
    _                    -> Right Nothing

-- ============================================================
-- ColumnIndex
-- ============================================================

-- | Serialize a 'ColumnIndex' in Thrift Compact Protocol.
encodeColumnIndex :: ColumnIndex -> ByteString
encodeColumnIndex = encodeCompact . columnIndexToThrift

-- | Parse a 'ColumnIndex' from a Thrift Compact byte slice.
decodeColumnIndex :: ByteString -> Either String ColumnIndex
decodeColumnIndex bs = decodeCompact bs >>= thriftToColumnIndex

-- | Look up the 'ColumnIndex' for the column chunk at @(rowGroupIdx,
-- columnIdx)@.
readColumnIndex
  :: ParquetFile
  -> Int -> Int
  -> Either String (Maybe ColumnIndex)
readColumnIndex pf rg col = do
  msl <- columnChunkColumnIndexSlice pf rg col
  case msl of
    Nothing -> Right Nothing
    Just bs -> Just <$> decodeColumnIndex bs

-- | Slice a 'ParquetFile' to the bytes covering a column chunk's serialized
-- 'ColumnIndex'. Returns 'Nothing' when the column chunk did not record one.
columnChunkColumnIndexSlice
  :: ParquetFile
  -> Int -> Int
  -> Either String (Maybe ByteString)
columnChunkColumnIndexSlice pf rgIdx colIdx = do
  cc <- selectColumnChunk pf rgIdx colIdx
  case (ccColumnIndexOffset cc, ccColumnIndexLength cc) of
    (Just off, Just len) -> Just <$> sliceFile pf off len "column index"
    _                    -> Right Nothing

-- ============================================================
-- Internal: Thrift conversions
-- ============================================================

offsetIndexToThrift :: OffsetIndex -> TV.Value
offsetIndexToThrift oi =
  TV.Struct $ V.fromList $
    (1, TV.List TW.TT_STRUCT (V.map pageLocationToThrift (oiPageLocations oi)))
    : maybeUnencoded
  where
    maybeUnencoded = case oiUnencodedByteArrayDataBytes oi of
      Nothing -> []
      Just v  -> [(2, TV.List TW.TT_I64 (V.map TV.I64 v))]

pageLocationToThrift :: PageLocation -> TV.Value
pageLocationToThrift pl = TV.Struct $ V.fromList
  [ (1, TV.I64 (plOffset pl))
  , (2, TV.I32 (plCompressedPageSize pl))
  , (3, TV.I64 (plFirstRowIndex pl))
  ]

thriftToOffsetIndex :: TV.Value -> Either String OffsetIndex
thriftToOffsetIndex (TV.Struct fields) = do
  let fm = V.toList fields
  pages <- case lookupField fm 1 of
    Just (TV.List _ vs) -> V.mapM thriftToPageLocation vs
    _ -> Left "Parquet.PageIndex: missing page_locations"
  let unenc = case lookupField fm 2 of
        Just (TV.List _ vs) -> Just <$> V.mapM expectI64 vs
        _                   -> Right Nothing
  unencV <- unenc
  Right OffsetIndex
    { oiPageLocations = pages
    , oiUnencodedByteArrayDataBytes = unencV
    }
thriftToOffsetIndex _ = Left "Parquet.PageIndex: expected struct for OffsetIndex"

thriftToPageLocation :: TV.Value -> Either String PageLocation
thriftToPageLocation (TV.Struct fields) = do
  let fm = V.toList fields
  off <- expectI64Field fm 1 "page_location.offset"
  csz <- expectI32Field fm 2 "page_location.compressed_page_size"
  fri <- expectI64Field fm 3 "page_location.first_row_index"
  Right PageLocation
    { plOffset = off
    , plCompressedPageSize = csz
    , plFirstRowIndex = fri
    }
thriftToPageLocation _ =
  Left "Parquet.PageIndex: expected struct for PageLocation"

columnIndexToThrift :: ColumnIndex -> TV.Value
columnIndexToThrift ci =
  TV.Struct $ V.fromList $
    [ (1, TV.List TW.TT_BOOL   (V.map TV.Bool   (ciNullPages ci)))
    , (2, TV.List TW.TT_STRING (V.map TV.Binary (ciMinValues ci)))
    , (3, TV.List TW.TT_STRING (V.map TV.Binary (ciMaxValues ci)))
    , (4, TV.I32 (boundaryOrderToInt (ciBoundaryOrder ci)))
    ]
    ++ optList 5 (ciNullCounts ci)
    ++ optList 6 (ciRepetitionLevelHistograms ci)
    ++ optList 7 (ciDefinitionLevelHistograms ci)
  where
    optList :: Int16 -> Maybe (V.Vector Int64) -> [(Int16, TV.Value)]
    optList _ Nothing  = []
    optList fid (Just v) =
      [(fid, TV.List TW.TT_I64 (V.map TV.I64 v))]

thriftToColumnIndex :: TV.Value -> Either String ColumnIndex
thriftToColumnIndex (TV.Struct fields) = do
  let fm = V.toList fields
  nullPages <- case lookupField fm 1 of
    Just (TV.List _ vs) -> V.mapM expectBool vs
    _ -> Left "Parquet.PageIndex: missing null_pages"
  mins <- case lookupField fm 2 of
    Just (TV.List _ vs) -> V.mapM expectBinary vs
    _ -> Left "Parquet.PageIndex: missing min_values"
  maxs <- case lookupField fm 3 of
    Just (TV.List _ vs) -> V.mapM expectBinary vs
    _ -> Left "Parquet.PageIndex: missing max_values"
  bo <- case lookupField fm 4 of
    Just (TV.I32 i) -> case intToBoundaryOrder i of
      Just b  -> Right b
      Nothing -> Left $
        "Parquet.PageIndex: invalid boundary_order " ++ show i
    _ -> Left "Parquet.PageIndex: missing boundary_order"
  let nc = case lookupField fm 5 of
        Just (TV.List _ vs) -> Just <$> V.mapM expectI64 vs
        _                   -> Right Nothing
  ncv <- nc
  let rep = case lookupField fm 6 of
        Just (TV.List _ vs) -> Just <$> V.mapM expectI64 vs
        _                   -> Right Nothing
  repv <- rep
  let def = case lookupField fm 7 of
        Just (TV.List _ vs) -> Just <$> V.mapM expectI64 vs
        _                   -> Right Nothing
  defv <- def
  Right ColumnIndex
    { ciNullPages = nullPages
    , ciMinValues = mins
    , ciMaxValues = maxs
    , ciBoundaryOrder = bo
    , ciNullCounts = ncv
    , ciRepetitionLevelHistograms = repv
    , ciDefinitionLevelHistograms = defv
    }
thriftToColumnIndex _ =
  Left "Parquet.PageIndex: expected struct for ColumnIndex"

-- ============================================================
-- Helpers
-- ============================================================

selectColumnChunk
  :: ParquetFile -> Int -> Int -> Either String ColumnChunk
selectColumnChunk pf rgIdx colIdx = do
  let rgs = fmRowGroups (pfFooter pf)
  if rgIdx < 0 || rgIdx >= V.length rgs
    then Left "Parquet.PageIndex: row group index out of range"
    else do
      let cs = rgColumns (V.unsafeIndex rgs rgIdx)
      if colIdx < 0 || colIdx >= V.length cs
        then Left "Parquet.PageIndex: column index out of range"
        else Right (V.unsafeIndex cs colIdx)

sliceFile
  :: ParquetFile -> Int64 -> Int32 -> String
  -> Either String ByteString
sliceFile pf off len what =
  let !o = fromIntegral off :: Int
      !l = fromIntegral len :: Int
      !bs = pfBytes pf
  in if o < 0 || l < 0 || o + l > BS.length bs
       then Left $ "Parquet.PageIndex: " ++ what ++ " slice out of bounds"
       else Right $! BS.take l (BS.drop o bs)

lookupField :: [(Int16, TV.Value)] -> Int16 -> Maybe TV.Value
lookupField fm fid = lookup fid fm

expectI32 :: TV.Value -> Either String Int32
expectI32 (TV.I32 v) = Right v
expectI32 v = Left $
  "Parquet.PageIndex: expected i32 but got " ++ show v

expectI64 :: TV.Value -> Either String Int64
expectI64 (TV.I64 v) = Right v
expectI64 v = Left $
  "Parquet.PageIndex: expected i64 but got " ++ show v

expectBool :: TV.Value -> Either String Bool
expectBool (TV.Bool b) = Right b
expectBool v = Left $
  "Parquet.PageIndex: expected bool but got " ++ show v

-- Thrift Compact transports binary and UTF-8 strings under the same
-- @TT_STRING@ wire type, so the decoder may surface either shape.
expectBinary :: TV.Value -> Either String ByteString
expectBinary (TV.Binary b) = Right b
expectBinary (TV.String t) =
  Right $! Data.Text.Encoding.encodeUtf8 t
expectBinary v = Left $
  "Parquet.PageIndex: expected binary but got " ++ show v

expectI32Field :: [(Int16, TV.Value)] -> Int16 -> String -> Either String Int32
expectI32Field fm fid name = case lookupField fm fid of
  Just v -> expectI32 v
  Nothing -> Left $ "Parquet.PageIndex: missing field " ++ name

expectI64Field :: [(Int16, TV.Value)] -> Int16 -> String -> Either String Int64
expectI64Field fm fid name = case lookupField fm fid of
  Just v -> expectI64 v
  Nothing -> Left $ "Parquet.PageIndex: missing field " ++ name
