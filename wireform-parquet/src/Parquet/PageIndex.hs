{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Apache Parquet page index: 'OffsetIndex' + 'ColumnIndex' (Thrift).

The page index tier (added in parquet-format 2.5) lets readers skip
entire pages without scanning column chunks. It is stored separately
from the row-group metadata, near the file footer:

* 'OffsetIndex' — per-page @(offset, compressed_page_size, first_row_index)@
  plus optional @unencoded_byte_array_data_bytes@ (size_statistics).
* 'ColumnIndex' — per-page @null_pages@, @min_values@, @max_values@,
  plus a 'BoundaryOrder' and optional null/level histograms.

'ColumnChunk' carries 'ccOffsetIndexOffset' / @offset_index_length@ and
'ccColumnIndexOffset' / @column_index_length@; this module reads/writes
the structures themselves with the Thrift Compact Protocol used by
"Parquet.Footer".
-}
module Parquet.PageIndex (
  -- * OffsetIndex
  encodeOffsetIndex,
  decodeOffsetIndex,
  readOffsetIndex,

  -- * ColumnIndex
  encodeColumnIndex,
  decodeColumnIndex,
  readColumnIndex,

  -- * Slicing
  columnChunkOffsetIndexSlice,
  columnChunkColumnIndexSlice,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64)
import Data.Text.Encoding qualified
import Data.Vector qualified as V
import Parquet.Read (ParquetFile (..))
import Parquet.Thrift.Schema
import Parquet.Types (
  ColumnChunk (..),
  ColumnIndex (..),
  FileMetadata (..),
  OffsetIndex (..),
  PageLocation (..),
  RowGroup (..),
  boundaryOrderToInt,
  intToBoundaryOrder,
 )
import Thrift.Decode (decodeCompact)
import Thrift.Encode (encodeCompact)
import Thrift.Value qualified as TV


-- ============================================================
-- OffsetIndex
-- ============================================================

-- | Serialize an 'OffsetIndex' in Thrift Compact Protocol.
encodeOffsetIndex :: OffsetIndex -> ByteString
encodeOffsetIndex = encodeCompact . offsetIndexToThrift


-- | Parse an 'OffsetIndex' from a Thrift Compact byte slice.
decodeOffsetIndex :: ByteString -> Either String OffsetIndex
decodeOffsetIndex bs = decodeCompact bs >>= thriftToOffsetIndex


{- | Look up the 'OffsetIndex' for the column chunk at @(rowGroupIdx, columnIdx)@
if 'ccOffsetIndexOffset' / 'ccOffsetIndexLength' are populated and within
file bounds.
-}
readOffsetIndex
  :: ParquetFile
  -> Int
  -> Int
  -> Either String (Maybe OffsetIndex)
readOffsetIndex pf rg col = do
  msl <- columnChunkOffsetIndexSlice pf rg col
  case msl of
    Nothing -> Right Nothing
    Just bs -> Just <$> decodeOffsetIndex bs


{- | Slice a 'ParquetFile' to the bytes covering a column chunk's serialized
'OffsetIndex'. Returns 'Nothing' when the column chunk did not record one.
-}
columnChunkOffsetIndexSlice
  :: ParquetFile
  -> Int
  -> Int
  -> Either String (Maybe ByteString)
columnChunkOffsetIndexSlice pf rgIdx colIdx = do
  cc <- selectColumnChunk pf rgIdx colIdx
  case (ccOffsetIndexOffset cc, ccOffsetIndexLength cc) of
    (Just off, Just len) -> Just <$> sliceFile pf off len "offset index"
    _ -> Right Nothing


-- ============================================================
-- ColumnIndex
-- ============================================================

-- | Serialize a 'ColumnIndex' in Thrift Compact Protocol.
encodeColumnIndex :: ColumnIndex -> ByteString
encodeColumnIndex = encodeCompact . columnIndexToThrift


-- | Parse a 'ColumnIndex' from a Thrift Compact byte slice.
decodeColumnIndex :: ByteString -> Either String ColumnIndex
decodeColumnIndex bs = decodeCompact bs >>= thriftToColumnIndex


{- | Look up the 'ColumnIndex' for the column chunk at @(rowGroupIdx,
columnIdx)@.
-}
readColumnIndex
  :: ParquetFile
  -> Int
  -> Int
  -> Either String (Maybe ColumnIndex)
readColumnIndex pf rg col = do
  msl <- columnChunkColumnIndexSlice pf rg col
  case msl of
    Nothing -> Right Nothing
    Just bs -> Just <$> decodeColumnIndex bs


{- | Slice a 'ParquetFile' to the bytes covering a column chunk's serialized
'ColumnIndex'. Returns 'Nothing' when the column chunk did not record one.
-}
columnChunkColumnIndexSlice
  :: ParquetFile
  -> Int
  -> Int
  -> Either String (Maybe ByteString)
columnChunkColumnIndexSlice pf rgIdx colIdx = do
  cc <- selectColumnChunk pf rgIdx colIdx
  case (ccColumnIndexOffset cc, ccColumnIndexLength cc) of
    (Just off, Just len) -> Just <$> sliceFile pf off len "column index"
    _ -> Right Nothing


-- ============================================================
-- Internal: Thrift conversions
-- ============================================================

offsetIndexToThrift :: OffsetIndex -> TV.Value
offsetIndexToThrift oi =
  TV.Struct $
    V.fromList $
      concat
        [ [OffsetIndex_PageLocations (V.map pageLocationToThrift (oiPageLocations oi))]
        , optField
            (oiUnencodedByteArrayDataBytes oi)
            (OffsetIndex_UnencodedByteArrayDataBytes . V.map TV.I64)
        ]


pageLocationToThrift :: PageLocation -> TV.Value
pageLocationToThrift pl =
  TV.Struct $
    V.fromList
      [ PageLocation_Offset (plOffset pl)
      , PageLocation_CompressedPageSize (plCompressedPageSize pl)
      , PageLocation_FirstRowIndex (plFirstRowIndex pl)
      ]


thriftToOffsetIndex :: TV.Value -> Either String OffsetIndex
thriftToOffsetIndex (TV.Struct fields) = do
  let fm = V.toList fields
  pages <- case findField
    fm
    ( \case
        OffsetIndex_PageLocations xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> V.mapM thriftToPageLocation vs
    Nothing -> Left "Parquet.PageIndex: missing page_locations"
  unencV <- case findField
    fm
    ( \case
        OffsetIndex_UnencodedByteArrayDataBytes xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> Just <$> V.mapM expectI64 vs
    Nothing -> Right Nothing
  Right
    OffsetIndex
      { oiPageLocations = pages
      , oiUnencodedByteArrayDataBytes = unencV
      }
thriftToOffsetIndex _ = Left "Parquet.PageIndex: expected struct for OffsetIndex"


thriftToPageLocation :: TV.Value -> Either String PageLocation
thriftToPageLocation (TV.Struct fields) = do
  let fm = V.toList fields
  off <- requireField fm "page_location.offset" $ \case
    PageLocation_Offset v -> Just v
    _ -> Nothing
  csz <- requireField fm "page_location.compressed_page_size" $ \case
    PageLocation_CompressedPageSize v -> Just v
    _ -> Nothing
  fri <- requireField fm "page_location.first_row_index" $ \case
    PageLocation_FirstRowIndex v -> Just v
    _ -> Nothing
  Right
    PageLocation
      { plOffset = off
      , plCompressedPageSize = csz
      , plFirstRowIndex = fri
      }
thriftToPageLocation _ =
  Left "Parquet.PageIndex: expected struct for PageLocation"


columnIndexToThrift :: ColumnIndex -> TV.Value
columnIndexToThrift ci =
  TV.Struct $
    V.fromList $
      concat
        [
          [ ColumnIndex_NullPages (V.map TV.Bool (ciNullPages ci))
          , ColumnIndex_MinValues (V.map TV.Binary (ciMinValues ci))
          , ColumnIndex_MaxValues (V.map TV.Binary (ciMaxValues ci))
          , ColumnIndex_BoundaryOrder (boundaryOrderToInt (ciBoundaryOrder ci))
          ]
        , optField
            (ciNullCounts ci)
            (ColumnIndex_NullCounts . V.map TV.I64)
        , optField
            (ciRepetitionLevelHistograms ci)
            (ColumnIndex_RepetitionLevelHistograms . V.map TV.I64)
        , optField
            (ciDefinitionLevelHistograms ci)
            (ColumnIndex_DefinitionLevelHistograms . V.map TV.I64)
        ]


thriftToColumnIndex :: TV.Value -> Either String ColumnIndex
thriftToColumnIndex (TV.Struct fields) = do
  let fm = V.toList fields
  nullPages <- case findField
    fm
    ( \case
        ColumnIndex_NullPages xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> V.mapM expectBool vs
    Nothing -> Left "Parquet.PageIndex: missing null_pages"
  mins <- case findField
    fm
    ( \case
        ColumnIndex_MinValues xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> V.mapM expectBinary vs
    Nothing -> Left "Parquet.PageIndex: missing min_values"
  maxs <- case findField
    fm
    ( \case
        ColumnIndex_MaxValues xs -> Just xs
        _ -> Nothing
    ) of
    Just vs -> V.mapM expectBinary vs
    Nothing -> Left "Parquet.PageIndex: missing max_values"
  boInt <- requireField fm "boundary_order" $ \case
    ColumnIndex_BoundaryOrder v -> Just v
    _ -> Nothing
  bo <-
    maybe
      (Left $ "Parquet.PageIndex: invalid boundary_order " ++ show boInt)
      Right
      (intToBoundaryOrder boInt)
  ncv <- optListI64 fm $ \case
    ColumnIndex_NullCounts xs -> Just xs
    _ -> Nothing
  repv <- optListI64 fm $ \case
    ColumnIndex_RepetitionLevelHistograms xs -> Just xs
    _ -> Nothing
  defv <- optListI64 fm $ \case
    ColumnIndex_DefinitionLevelHistograms xs -> Just xs
    _ -> Nothing
  Right
    ColumnIndex
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


optListI64
  :: [(Int16, TV.Value)]
  -> ((Int16, TV.Value) -> Maybe (V.Vector TV.Value))
  -> Either String (Maybe (V.Vector Int64))
optListI64 fm probe = case findField fm probe of
  Just vs -> Just <$> V.mapM expectI64 vs
  Nothing -> Right Nothing


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
  :: ParquetFile
  -> Int64
  -> Int32
  -> String
  -> Either String ByteString
sliceFile pf off len what =
  let !o = fromIntegral off :: Int
      !l = fromIntegral len :: Int
      !bs = pfBytes pf
  in if o < 0 || l < 0 || o + l > BS.length bs
       then Left $ "Parquet.PageIndex: " ++ what ++ " slice out of bounds"
       else Right $! BS.take l (BS.drop o bs)


requireField
  :: [(Int16, TV.Value)]
  -> String
  -> ((Int16, TV.Value) -> Maybe a)
  -> Either String a
requireField fm name probe = case findField fm probe of
  Just v -> Right v
  Nothing -> Left $ "Parquet.PageIndex: missing or invalid field " ++ name


expectI64 :: TV.Value -> Either String Int64
expectI64 (TV.I64 v) = Right v
expectI64 v =
  Left $
    "Parquet.PageIndex: expected i64 but got " ++ show v


expectBool :: TV.Value -> Either String Bool
expectBool (TV.Bool b) = Right b
expectBool v =
  Left $
    "Parquet.PageIndex: expected bool but got " ++ show v


-- Thrift Compact transports binary and UTF-8 strings under the same
-- @TT_STRING@ wire type, so the decoder may surface either shape.
expectBinary :: TV.Value -> Either String ByteString
expectBinary (TV.Binary b) = Right b
expectBinary (TV.String t) =
  Right $! Data.Text.Encoding.encodeUtf8 t
expectBinary v =
  Left $
    "Parquet.PageIndex: expected binary but got " ++ show v
