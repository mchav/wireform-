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
  , buildParquetFileWithIndex
    -- * Statistics
  , statisticsForInt32
  , statisticsForInt64
  , statisticsForByteArray
    -- * Per-column auxiliary metadata for the indexed writer
  , ColumnAux(..)
  , emptyColumnAux
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP

import Parquet.BloomFilter (Sbbf, encodeBloomFilter, sbbfNumBytes)
import qualified Parquet.BloomFilter as BF
import Parquet.Footer (writeFooter, parquetMagic)
import Parquet.PageIndex
  ( encodeColumnIndex
  , encodeOffsetIndex
  )
import Parquet.Types
  ( ColumnIndex (..)
  , OffsetIndex (..)
  )
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
  , Statistics (..)
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
                , cmStatistics = Just (statisticsForInt32 colVec)
                , cmBloomFilterOffset = Nothing
                , cmBloomFilterLength = Nothing
                }
            , ccOffsetIndexOffset = Nothing
            , ccOffsetIndexLength = Nothing
            , ccColumnIndexOffset = Nothing
            , ccColumnIndexLength = Nothing
            }
      in (V.snoc cs cc, cOff + sz)

-- ============================================================
-- Page / column statistics
-- ============================================================

-- | Compute Parquet 'Statistics' for an @INT32@ column.
--
-- Encodes @min_value@ / @max_value@ as little-endian @INT32@ per the
-- spec (PLAIN encoding for variable-length types is the same except
-- for byte arrays).  Both legacy @min@/@max@ and the modern
-- @min_value@/@max_value@ slots are populated.
statisticsForInt32 :: VP.Vector Int32 -> Statistics
statisticsForInt32 vs
  | VP.null vs = emptyStats
  | otherwise =
      let !mn = VP.foldl1' min vs
          !mx = VP.foldl1' max vs
          encMin = i32LE mn
          encMax = i32LE mx
      in Statistics
           { statMin = Just encMin
           , statMax = Just encMax
           , statNullCount = Just 0
           , statDistinctCount = Nothing
           , statMinValue = Just encMin
           , statMaxValue = Just encMax
           }

-- | Compute Parquet 'Statistics' for an @INT64@ column (LE i64 min/max).
statisticsForInt64 :: VP.Vector Int64 -> Statistics
statisticsForInt64 vs
  | VP.null vs = emptyStats
  | otherwise =
      let !mn = VP.foldl1' min vs
          !mx = VP.foldl1' max vs
          encMin = i64LE mn
          encMax = i64LE mx
      in Statistics
           { statMin = Just encMin
           , statMax = Just encMax
           , statNullCount = Just 0
           , statDistinctCount = Nothing
           , statMinValue = Just encMin
           , statMaxValue = Just encMax
           }

-- | Compute Parquet 'Statistics' for a @BYTE_ARRAY@ column. Values are
-- compared lexicographically (unsigned byte-by-byte).  The min/max
-- bytes are stored without their PLAIN length prefix per the spec.
statisticsForByteArray :: V.Vector ByteString -> Statistics
statisticsForByteArray vs
  | V.null vs = emptyStats
  | otherwise =
      let !mn = V.foldl1' minBS vs
          !mx = V.foldl1' maxBS vs
      in Statistics
           { statMin = Just mn
           , statMax = Just mx
           , statNullCount = Just 0
           , statDistinctCount = Nothing
           , statMinValue = Just mn
           , statMaxValue = Just mx
           }
  where
    minBS a b = if a <= b then a else b
    maxBS a b = if a >= b then a else b

emptyStats :: Statistics
emptyStats = Statistics Nothing Nothing (Just 0) Nothing Nothing Nothing

i32LE :: Int32 -> ByteString
i32LE v = BL.toStrict (B.toLazyByteString (B.int32LE v))

i64LE :: Int64 -> ByteString
i64LE v = BL.toStrict (B.toLazyByteString (B.int64LE v))

-- ============================================================
-- Indexed writer: bloom filter + page index footers
-- ============================================================

-- | Auxiliary metadata for one column chunk that the indexed writer
-- emits alongside the data pages. Any 'Nothing' field is omitted from
-- the produced file.
data ColumnAux = ColumnAux
  { caBloomFilter  :: !(Maybe Sbbf)
    -- ^ Pre-built split-block bloom filter for this column chunk. The
    -- writer serialises it into the bloom-filter region and records the
    -- (offset, length) on the column metadata.
  , caOffsetIndex  :: !(Maybe OffsetIndex)
    -- ^ Per-page @(offset, compressed_size, first_row_index)@ entries.
    -- The writer rewrites the @plOffset@ of each 'PageLocation' so that
    -- it points at the actual page-bytes location in the assembled file.
  , caColumnIndex  :: !(Maybe ColumnIndex)
    -- ^ Per-page null/min/max statistics. Written verbatim.
  } deriving (Show, Eq)

emptyColumnAux :: ColumnAux
emptyColumnAux = ColumnAux Nothing Nothing Nothing

-- | Like 'buildParquetFile' but also emits a bloom-filter region, an
-- offset-index region, and a column-index region between the row-group
-- bytes and the file footer. Each column chunk's @ColumnMetadata@ is
-- updated with the resulting offsets and lengths so the resulting file
-- is byte-compatible with what parquet-mr / arrow-rs produce.
--
-- File layout:
--
-- @
-- PAR1                         -- 4 bytes
-- <row group bytes ...>        -- as before
-- <bloom filter bitsets ...>   -- one per column with caBloomFilter = Just
-- <offset index thrifts ...>   -- one per column with caOffsetIndex = Just
-- <column index thrifts ...>   -- one per column with caColumnIndex = Just
-- <footer>                     -- Thrift-encoded FileMetadata
-- <footer length: int32 LE>
-- PAR1
-- @
buildParquetFileWithIndex
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector (VP.Vector Int32))   -- ^ Row groups; each is a vector of int32 columns.
  -> V.Vector (V.Vector ColumnAux)            -- ^ One @ColumnAux@ per (row group, column).
  -> ByteString
buildParquetFileWithIndex schema rowGroupVecs auxes =
  let !encodedRGs = V.map (V.map encodePlainInt32Page) rowGroupVecs
      !rowGroupBytes = concatMap V.toList (V.toList encodedRGs)
      !rgBytesLen = sum (map BS.length rowGroupBytes)
      !startOfData = 4 :: Int  -- "PAR1"
      !startOfBloom = startOfData + rgBytesLen

      -- Phase 1: lay out row group metadata with absolute page offsets.
      (!rgMetasBase, _) = V.ifoldl' buildRG (V.empty, startOfData) encodedRGs

      -- Phase 2: lay out the bloom-filter region. For each (rg, col) we
      -- decide whether a bloom filter exists and reserve its bytes.
      (!rgMetasBloom, !bloomBytes, !endOfBloom) =
        layoutBlooms rgMetasBase auxes startOfBloom

      -- Phase 3: offset-index region.
      (!rgMetasOff, !offBytes, !endOfOff) =
        layoutOffsetIndex rgMetasBloom auxes endOfBloom encodedRGs

      -- Phase 4: column-index region.
      (!rgMetasCol, !colBytes, _endOfCol) =
        layoutColumnIndex rgMetasOff auxes endOfOff

      !totalRows = V.foldl' (\a rg -> a + rgNumRows rg) 0 rgMetasCol
      !fm = FileMetadata
        { fmVersion = 1
        , fmSchema = schema
        , fmNumRows = totalRows
        , fmRowGroups = rgMetasCol
        , fmCreatedBy = Just "wireform"
        }
   in BL.toStrict $ B.toLazyByteString $
        B.byteString parquetMagic
        <> mconcat (map B.byteString rowGroupBytes)
        <> mconcat (map B.byteString bloomBytes)
        <> mconcat (map B.byteString offBytes)
        <> mconcat (map B.byteString colBytes)
        <> B.byteString (writeFooter fm)
  where
    leaves :: V.Vector SchemaElement
    !leaves = V.filter (maybe False (const True) . seType) schema

    -- Identical to buildParquetFile's layout phase.
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
                , cmStatistics = Just (statisticsForInt32 colVec)
                , cmBloomFilterOffset = Nothing
                , cmBloomFilterLength = Nothing
                }
            , ccOffsetIndexOffset = Nothing
            , ccOffsetIndexLength = Nothing
            , ccColumnIndexOffset = Nothing
            , ccColumnIndexLength = Nothing
            }
      in (V.snoc cs cc, cOff + sz)

layoutBlooms
  :: V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int                        -- ^ starting offset
  -> (V.Vector RowGroup, [ByteString], Int)
layoutBlooms rgs auxes start = go 0 start [] V.empty
  where
    go !i !off !payloads !acc
      | i >= V.length rgs = (acc, reverse payloads, off)
      | otherwise =
          let !rg = V.unsafeIndex rgs i
              !cols = rgColumns rg
              !aux  = if i < V.length auxes then V.unsafeIndex auxes i else V.empty
              (!cols', !off', !payloads') =
                V.ifoldl' (rewriteCol aux) (V.empty, off, payloads) cols
              !rg' = rg { rgColumns = cols' }
           in go (i + 1) off' payloads' (V.snoc acc rg')

    rewriteCol
      :: V.Vector ColumnAux
      -> (V.Vector ColumnChunk, Int, [ByteString])
      -> Int -> ColumnChunk
      -> (V.Vector ColumnChunk, Int, [ByteString])
    rewriteCol aux (!ccs, !off, !payloads) cIdx cc =
      let mAux = if cIdx < V.length aux then Just (V.unsafeIndex aux cIdx) else Nothing
       in case mAux >>= caBloomFilter of
            Nothing -> (V.snoc ccs cc, off, payloads)
            Just bf ->
              let !bs = encodeBloomFilter bf
                  !bsLen = BS.length bs
                  !cm0 = fromMaybe defaultMetadata (ccMetadata cc)
                  !cm' = cm0
                    { cmBloomFilterOffset = Just (fromIntegral off)
                    , cmBloomFilterLength = Just (fromIntegral bsLen)
                    }
                  !cc' = cc { ccMetadata = Just cm' }
               in (V.snoc ccs cc', off + bsLen, bs : payloads)

    defaultMetadata = ColumnMetadata
      { cmType = PTInt32, cmEncodings = V.empty, cmPathInSchema = V.empty
      , cmCodec = Uncompressed, cmNumValues = 0
      , cmTotalUncompressedSize = 0, cmTotalCompressedSize = 0
      , cmDataPageOffset = 0, cmStatistics = Nothing
      , cmBloomFilterOffset = Nothing, cmBloomFilterLength = Nothing
      }

layoutOffsetIndex
  :: V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int
  -> V.Vector (V.Vector ByteString)
  -> (V.Vector RowGroup, [ByteString], Int)
layoutOffsetIndex rgs auxes start _ = go 0 start [] V.empty
  where
    go !i !off !payloads !acc
      | i >= V.length rgs = (acc, reverse payloads, off)
      | otherwise =
          let !rg = V.unsafeIndex rgs i
              !cols = rgColumns rg
              !aux  = if i < V.length auxes then V.unsafeIndex auxes i else V.empty
              (!cols', !off', !payloads') =
                V.ifoldl' (rewriteCol aux) (V.empty, off, payloads) cols
              !rg' = rg { rgColumns = cols' }
           in go (i + 1) off' payloads' (V.snoc acc rg')

    rewriteCol
      :: V.Vector ColumnAux
      -> (V.Vector ColumnChunk, Int, [ByteString])
      -> Int -> ColumnChunk
      -> (V.Vector ColumnChunk, Int, [ByteString])
    rewriteCol aux (!ccs, !off, !payloads) cIdx cc =
      let mAux = if cIdx < V.length aux then Just (V.unsafeIndex aux cIdx) else Nothing
       in case mAux >>= caOffsetIndex of
            Nothing -> (V.snoc ccs cc, off, payloads)
            Just oi ->
              let !bs = encodeOffsetIndex oi
                  !bsLen = BS.length bs
                  !cc' = cc
                    { ccOffsetIndexOffset = Just (fromIntegral off)
                    , ccOffsetIndexLength = Just (fromIntegral bsLen)
                    }
               in (V.snoc ccs cc', off + bsLen, bs : payloads)

layoutColumnIndex
  :: V.Vector RowGroup
  -> V.Vector (V.Vector ColumnAux)
  -> Int
  -> (V.Vector RowGroup, [ByteString], Int)
layoutColumnIndex rgs auxes start = go 0 start [] V.empty
  where
    go !i !off !payloads !acc
      | i >= V.length rgs = (acc, reverse payloads, off)
      | otherwise =
          let !rg = V.unsafeIndex rgs i
              !cols = rgColumns rg
              !aux  = if i < V.length auxes then V.unsafeIndex auxes i else V.empty
              (!cols', !off', !payloads') =
                V.ifoldl' (rewriteCol aux) (V.empty, off, payloads) cols
              !rg' = rg { rgColumns = cols' }
           in go (i + 1) off' payloads' (V.snoc acc rg')

    rewriteCol
      :: V.Vector ColumnAux
      -> (V.Vector ColumnChunk, Int, [ByteString])
      -> Int -> ColumnChunk
      -> (V.Vector ColumnChunk, Int, [ByteString])
    rewriteCol aux (!ccs, !off, !payloads) cIdx cc =
      let mAux = if cIdx < V.length aux then Just (V.unsafeIndex aux cIdx) else Nothing
       in case mAux >>= caColumnIndex of
            Nothing -> (V.snoc ccs cc, off, payloads)
            Just ci ->
              let !bs = encodeColumnIndex ci
                  !bsLen = BS.length bs
                  !cc' = cc
                    { ccColumnIndexOffset = Just (fromIntegral off)
                    , ccColumnIndexLength = Just (fromIntegral bsLen)
                    }
               in (V.snoc ccs cc', off + bsLen, bs : payloads)

-- Suppress unused-import warning when bloom-filter sizing helpers aren't
-- referenced directly in the writer (they are part of the public API).
_unusedBF :: Sbbf -> Int
_unusedBF = BF.sbbfNumBytes
