{-# LANGUAGE BangPatterns #-}
-- | Write Parquet files.
--
-- Provides page-level encoding, column chunk assembly, and whole-file builders.
--
-- @
-- import qualified Data.Vector.Primitive as VP
-- import qualified Data.Vector as V
-- import Parquet.Write
-- import Parquet.Types
--
-- let schema = V.fromList
--       [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
--       , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing
--       ]
--     vals = VP.fromList [1, 2, 3 :: Int32]
--     bs = buildParquetFile schema (V.singleton (V.singleton vals))
-- @
module Parquet.Write
  ( writeParquetFile
  , encodePlainInt32Page
  , encodePlainInt64Page
  , encodePlainByteArrayPage
  , encodePageHeader
  , assembleColumnChunk
  , buildParquetFile
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP

import Parquet.Footer (writeFooter, parquetMagic)
import Parquet.Page
  ( DataPageHeader (..)
  , DataPageHeaderV2 (..)
  , DictionaryPageHeader (..)
  , PageHeader (..)
  , pageTypeDataPage
  )
import Parquet.Types
  ( ColumnChunk (..)
  , ColumnMetadata (..)
  , Compression (..)
  , Encoding (..)
  , FileMetadata (..)
  , ParquetType (..)
  , RowGroup (..)
  , SchemaElement (..)
  )
import Thrift.Encode (encodeCompact)
import qualified Thrift.Value as TV

-- | Assemble a complete Parquet file from pre-computed metadata and encoded
-- column chunk data. Each inner vector is one row group's column chunks
-- (already-encoded pages).
writeParquetFile :: FileMetadata -> V.Vector (V.Vector ByteString) -> ByteString
writeParquetFile fm rowGroupData = BL.toStrict $ B.toLazyByteString $
  B.byteString parquetMagic
  <> V.foldl' (\b rg -> V.foldl' (\b2 col -> b2 <> B.byteString col) b rg) mempty rowGroupData
  <> B.byteString (writeFooter fm)

-- | Encode a vector of @INT32@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (header + body).
encodePlainInt32Page :: VP.Vector Int32 -> ByteString
encodePlainInt32Page vals =
  let !n = VP.length vals
      !bodySize = n * 4
      !body = BL.toStrict $ B.toLazyByteString $
        VP.foldl' (\b v -> b <> B.int32LE v) mempty vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

-- | Encode a vector of @INT64@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@.
encodePlainInt64Page :: VP.Vector Int64 -> ByteString
encodePlainInt64Page vals =
  let !n = VP.length vals
      !bodySize = n * 8
      !body = BL.toStrict $ B.toLazyByteString $
        VP.foldl' (\b v -> b <> B.int64LE v) mempty vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

-- | Encode a vector of @BYTE_ARRAY@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (4-byte LE length prefix per value).
encodePlainByteArrayPage :: V.Vector ByteString -> ByteString
encodePlainByteArrayPage vals =
  let !body = BL.toStrict $ B.toLazyByteString $
        V.foldl' (\b v ->
          b <> B.word32LE (fromIntegral (BS.length v)) <> B.byteString v
        ) mempty vals
      !bodySize = BS.length body
      !n = V.length vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

mkPlainDataPageHeader :: Int32 -> Int32 -> PageHeader
mkPlainDataPageHeader numValues bodySize = PageHeader
  { phType = pageTypeDataPage
  , phUncompressedPageSize = Just bodySize
  , phCompressedPageSize = Just bodySize
  , phDataPage = Just DataPageHeader
      { dphNumValues = numValues
      , dphEncoding = 0
      }
  , phDictionaryPage = Nothing
  , phDataPageV2 = Nothing
  }

-- | Thrift compact-encode a 'PageHeader'.
encodePageHeader :: PageHeader -> ByteString
encodePageHeader hdr = encodeCompact (pageHeaderToThrift hdr)

pageHeaderToThrift :: PageHeader -> TV.Value
pageHeaderToThrift hdr = TV.Struct $ V.fromList $
  [(1, TV.I32 (phType hdr))]
  ++ maybe [] (\s -> [(2, TV.I32 s)]) (phUncompressedPageSize hdr)
  ++ maybe [] (\s -> [(3, TV.I32 s)]) (phCompressedPageSize hdr)
  ++ maybe [] (\dph -> [(5, dataPageHeaderToThrift dph)]) (phDataPage hdr)
  ++ maybe [] (\dk -> [(7, dictPageHeaderToThrift dk)]) (phDictionaryPage hdr)
  ++ maybe [] (\v2 -> [(8, dataPageHeaderV2ToThrift v2)]) (phDataPageV2 hdr)

dataPageHeaderToThrift :: DataPageHeader -> TV.Value
dataPageHeaderToThrift dph = TV.Struct $ V.fromList
  [ (1, TV.I32 (dphNumValues dph))
  , (2, TV.I32 (dphEncoding dph))
  ]

dictPageHeaderToThrift :: DictionaryPageHeader -> TV.Value
dictPageHeaderToThrift dk = TV.Struct $ V.fromList
  [ (1, TV.I32 (dictNumValues dk))
  , (2, TV.I32 (dictEncoding dk))
  ]

dataPageHeaderV2ToThrift :: DataPageHeaderV2 -> TV.Value
dataPageHeaderV2ToThrift v2 = TV.Struct $ V.fromList
  [ (1, TV.I32 (dph2NumValues v2))
  , (2, TV.I32 (dph2NumNulls v2))
  , (3, TV.I32 (dph2NumRows v2))
  , (4, TV.I32 (dph2Encoding v2))
  , (5, TV.I32 (dph2DefLevelsLen v2))
  , (6, TV.I32 (dph2RepLevelsLen v2))
  , (7, TV.Bool (dph2IsCompressed v2))
  ]

-- | Concatenate pre-encoded pages into a single column chunk. Currently only
-- @Uncompressed@ is supported for writing; pages must already be encoded with
-- their headers.
assembleColumnChunk :: Compression -> [ByteString] -> ByteString
assembleColumnChunk _codec pages = mconcat pages

-- | Build a complete Parquet file from a schema and row groups of @INT32@
-- column vectors. Produces @PAR1@ magic, uncompressed @PLAIN@ pages, footer,
-- and trailing magic.
buildParquetFile :: V.Vector SchemaElement -> V.Vector (V.Vector (VP.Vector Int32)) -> ByteString
buildParquetFile schema rowGroupVecs =
  let !encodedRGs = V.map (V.map encodePlainInt32Page) rowGroupVecs
      (!rgMetas, !_) = V.ifoldl' buildRG (V.empty, 4) encodedRGs
      !totalRows = V.foldl' (\a rg -> a + rgNumRows rg) 0 rgMetas
      !fm = FileMetadata
        { fmVersion = 1
        , fmSchema = schema
        , fmNumRows = totalRows
        , fmRowGroups = rgMetas
        , fmCreatedBy = Just "wireform"
        }
  in writeParquetFile fm encodedRGs
  where
    leaves :: V.Vector SchemaElement
    !leaves = V.filter (maybe False (const True) . seType) schema

    buildRG :: (V.Vector RowGroup, Int) -> Int -> V.Vector ByteString -> (V.Vector RowGroup, Int)
    buildRG (!rgs, !off) rgIdx encodedCols =
      let !colVecs = V.unsafeIndex rowGroupVecs rgIdx
          (!cols, !off2) = V.ifoldl' (buildCol colVecs) (V.empty, off) encodedCols
          !nRows = if V.null colVecs then 0 else fromIntegral (VP.length (V.unsafeIndex colVecs 0))
          !rg = RowGroup
            { rgColumns = cols
            , rgTotalByteSize = fromIntegral (off2 - off)
            , rgNumRows = nRows
            }
      in (V.snoc rgs rg, off2)

    buildCol :: V.Vector (VP.Vector Int32) -> (V.Vector ColumnChunk, Int) -> Int -> ByteString -> (V.Vector ColumnChunk, Int)
    buildCol colVecs (!cs, !cOff) colIdx pageBs =
      let !colVec = V.unsafeIndex colVecs colIdx
          !leaf = V.unsafeIndex leaves colIdx
          !sz = BS.length pageBs
          !cc = ColumnChunk
            { ccFilePath = Nothing
            , ccFileOffset = fromIntegral cOff
            , ccMetadata = Just ColumnMetadata
                { cmType = fromMaybe PTInt32 (seType leaf)
                , cmEncodings = V.singleton Plain
                , cmPathInSchema = V.singleton (seName leaf)
                , cmCodec = Uncompressed
                , cmNumValues = fromIntegral (VP.length colVec)
                , cmTotalUncompressedSize = fromIntegral sz
                , cmTotalCompressedSize = fromIntegral sz
                , cmDataPageOffset = fromIntegral cOff
                , cmStatistics = Nothing
                , cmBloomFilterOffset = Nothing
                , cmBloomFilterLength = Nothing
                }
            , ccOffsetIndexOffset = Nothing
            , ccOffsetIndexLength = Nothing
            , ccColumnIndexOffset = Nothing
            , ccColumnIndexLength = Nothing
            }
      in (V.snoc cs cc, cOff + sz)
