{-# LANGUAGE BangPatterns #-}
-- | Write Parquet files.
--
-- Provides page-level encoding, column chunk assembly, and whole-file builders.
-- The user-facing entry point is 'buildParquetFile' (or
-- 'buildParquetFileWithIndex' when emitting bloom filter / page index
-- regions). Both accept heterogeneous primitive columns via 'ColumnData'.
--
-- @
-- import qualified Data.Vector.Primitive as VP
-- import qualified Data.Vector as V
-- import Parquet.Write
-- import Parquet.Types
--
-- let schema = V.fromList
--       [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing
--       , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing
--       ]
--     vals = VP.fromList [1, 2, 3 :: Int32]
--     bs = buildParquetFile schema (V.singleton (V.singleton (ColInt32 vals)))
-- @
module Parquet.Write
  ( -- * Whole-file builders (preferred entry points)
    buildParquetFile
  , buildParquetFileWithIndex
    -- * Per-column auxiliary metadata for the indexed writer
  , ColumnAux(..)
  , emptyColumnAux
    -- * Column data
  , ColumnData(..)
  , columnDataLength
  , columnDataParquetType
  , columnDataStatistics
  , encodeColumnDataPage
    -- * Nullable columns (definition levels)
  , OptionalColumn(..)
  , optionalColumnLength
  , optionalColumnNullCount
  , optionalColumnPresentValues
  , encodeOptionalColumnPage
    -- * Dictionary encoding
  , Dictionary(..)
  , buildDictionary
  , encodeDictPage
  , encodeDictDataPage
    -- * Data page version (V1 vs V2)
  , PageVersion(..)
  , encodeColumnDataPageV2
  , encodeOptionalColumnPageV2
    -- * Page / footer building blocks (used by composite writers)
  , writeParquetFile
  , encodePageHeader
  , assembleColumnChunk
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import GHC.Float (castDoubleToWord64, castFloatToWord32)

import Parquet.BloomFilter (Sbbf, encodeBloomFilter, sbbfNumBytes)
import qualified Parquet.BloomFilter as BF
import qualified Parquet.Compress as Compress
import qualified Parquet.LevelsEncode as LE
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
  , PageType (..)
  , pageTypeTag
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

-- | Encode a vector of @FLOAT@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (4-byte little-endian IEEE 754 per value).
encodePlainFloatPage :: VP.Vector Float -> ByteString
encodePlainFloatPage vals =
  let !n = VP.length vals
      !bodySize = n * 4
      !body = BL.toStrict $ B.toLazyByteString $
        VP.foldl' (\b v -> b <> writeFloatLE v) mempty vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

-- | Encode a vector of @DOUBLE@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (8-byte little-endian IEEE 754 per value).
encodePlainDoublePage :: VP.Vector Double -> ByteString
encodePlainDoublePage vals =
  let !n = VP.length vals
      !bodySize = n * 8
      !body = BL.toStrict $ B.toLazyByteString $
        VP.foldl' (\b v -> b <> writeDoubleLE v) mempty vals
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body

-- | Encode a vector of @BOOLEAN@ values as a single uncompressed @PLAIN@
-- @DATA_PAGE@ (1 bit per value, LSB-first packed bytes per the spec).
encodePlainBooleanPage :: V.Vector Bool -> ByteString
encodePlainBooleanPage vals =
  let !n = V.length vals
      !numBytes = (n + 7) `quot` 8
      !body = BL.toStrict $ B.toLazyByteString $
        flip foldMap [0 .. numBytes - 1] $ \byteIdx ->
          let buildByte !bit !acc
                | bit >= 8                = acc
                | byteIdx * 8 + bit >= n  = acc
                | otherwise =
                    let !v = V.unsafeIndex vals (byteIdx * 8 + bit)
                        !flag = if v then (1 :: Int) `shiftL'` bit else 0
                     in buildByte (bit + 1) (acc + flag)
          in B.word8 (fromIntegral (buildByte 0 0))
      !bodySize = numBytes
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
  in encodePageHeader hdr <> body
  where
    shiftL' :: Int -> Int -> Int
    shiftL' x i = x * (2 ^ i)

writeFloatLE :: Float -> B.Builder
writeFloatLE = B.word32LE . castFloatToWord32

writeDoubleLE :: Double -> B.Builder
writeDoubleLE = B.word64LE . castDoubleToWord64

mkPlainDataPageHeader :: Int32 -> Int32 -> PageHeader
mkPlainDataPageHeader numValues bodySize = PageHeader
  { phType = PtDataPage DataPageHeader
      { dphNumValues = numValues
      , dphEncoding = 0
      }
  , phUncompressedPageSize = Just bodySize
  , phCompressedPageSize = Just bodySize
  }

-- | Thrift compact-encode a 'PageHeader'.
encodePageHeader :: PageHeader -> ByteString
encodePageHeader hdr = encodeCompact (pageHeaderToThrift hdr)

pageHeaderToThrift :: PageHeader -> TV.Value
pageHeaderToThrift hdr = TV.Struct $ V.fromList $
  [(1, TV.I32 (pageTypeTag (phType hdr)))]
  ++ maybe [] (\s -> [(2, TV.I32 s)]) (phUncompressedPageSize hdr)
  ++ maybe [] (\s -> [(3, TV.I32 s)]) (phCompressedPageSize hdr)
  ++ case phType hdr of
       PtDataPage dph        -> [(5, dataPageHeaderToThrift dph)]
       PtDictionaryPage dk   -> [(7, dictPageHeaderToThrift dk)]
       PtDataPageV2 v2       -> [(8, dataPageHeaderV2ToThrift v2)]
       PtIndexPage           -> []

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

-- ============================================================
-- DATA_PAGE_V2
-- ============================================================

-- | Encode an 'OptionalColumn' as a single @DATA_PAGE_V2@.
--
-- Layout (per the Parquet spec):
--
-- @
-- <repetition_levels>   -- length given by header.repetition_levels_byte_length
--                       -- always uncompressed; we emit zero bytes for
--                       -- non-repeated columns.
-- <definition_levels>   -- length given by header.definition_levels_byte_length
--                       -- always uncompressed; max def-level is 1 for
--                       -- optional columns.
-- <values>              -- compressed iff codec /= Uncompressed; PLAIN encoded.
-- @
--
-- The level segments are RLE-hybrid encoded *without* the 4-byte length
-- prefix that V1 uses. The header records both segment lengths and an
-- @is_compressed@ flag.
encodeOptionalColumnPageV2 :: Compression -> OptionalColumn -> Either String ByteString
encodeOptionalColumnPageV2 codec oc = do
  let !defs = VP.fromList
        [ if isJust v then 1 else 0
        | v <- presenceList oc
        ]
      !defStream = LE.encodeRLEHybrid 1 defs
      !defLen = BS.length defStream
      !repStream = BS.empty
      !repLen = 0 :: Int
      !valuesRaw = encodeColumnDataPagePayload (optionalColumnPresentValues oc)
      !uncompValuesLen = BS.length valuesRaw
  (valuesBs, codecActual) <- case Compress.compressPageBytes codec valuesRaw of
        Right cb -> Right (cb, codec)
        Left  e  -> Left e
  let !nVals = optionalColumnLength oc
      !nNulls = sum (map (\v -> if isJust v then 0 else 1) (presenceList oc)) :: Int
      !nRows = nVals  -- non-repeated => num_rows == num_values
      !uncompPageSize = repLen + defLen + uncompValuesLen
      !compPageSize   = repLen + defLen + BS.length valuesBs
      !hdr = PageHeader
        { phType = PtDataPageV2 DataPageHeaderV2
            { dph2NumValues    = fromIntegral nVals
            , dph2NumNulls     = fromIntegral nNulls
            , dph2NumRows      = fromIntegral nRows
            , dph2Encoding     = 0 -- PLAIN
            , dph2DefLevelsLen = fromIntegral defLen
            , dph2RepLevelsLen = fromIntegral repLen
            , dph2IsCompressed = codecActual /= Uncompressed
            }
        , phUncompressedPageSize = Just (fromIntegral uncompPageSize)
        , phCompressedPageSize   = Just (fromIntegral compPageSize)
        }
  Right (encodePageHeader hdr <> repStream <> defStream <> valuesBs)
  where
    isJust (Just _) = True
    isJust Nothing  = False

    presenceList :: OptionalColumn -> [Maybe ()]
    presenceList = \case
      OptInt32 v     -> map void' (V.toList v)
      OptInt64 v     -> map void' (V.toList v)
      OptFloat v     -> map void' (V.toList v)
      OptDouble v    -> map void' (V.toList v)
      OptBool v      -> map void' (V.toList v)
      OptByteArray v -> map void' (V.toList v)

    void' Nothing  = Nothing
    void' (Just _) = Just ()

-- | Encode a required (non-null) 'ColumnData' as a single
-- @DATA_PAGE_V2@. No def/rep level streams are emitted; only the values
-- segment, compressed if the caller requested it.
encodeColumnDataPageV2 :: Compression -> ColumnData -> Either String ByteString
encodeColumnDataPageV2 codec cd = do
  let !valuesRaw = encodeColumnDataPagePayload cd
      !uncompValuesLen = BS.length valuesRaw
  (valuesBs, codecActual) <- case Compress.compressPageBytes codec valuesRaw of
        Right cb -> Right (cb, codec)
        Left  e  -> Left e
  let !nVals = columnDataLength cd
      !uncompPageSize = uncompValuesLen
      !compPageSize   = BS.length valuesBs
      !hdr = PageHeader
        { phType = PtDataPageV2 DataPageHeaderV2
            { dph2NumValues    = fromIntegral nVals
            , dph2NumNulls     = 0
            , dph2NumRows      = fromIntegral nVals
            , dph2Encoding     = 0
            , dph2DefLevelsLen = 0
            , dph2RepLevelsLen = 0
            , dph2IsCompressed = codecActual /= Uncompressed
            }
        , phUncompressedPageSize = Just (fromIntegral uncompPageSize)
        , phCompressedPageSize   = Just (fromIntegral compPageSize)
        }
  Right (encodePageHeader hdr <> valuesBs)

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

-- | Compute Parquet 'Statistics' for a @FLOAT@ column. Min/max are
-- compared as IEEE 754 floats then encoded as 4-byte little-endian.
statisticsForFloat :: VP.Vector Float -> Statistics
statisticsForFloat vs
  | VP.null vs = emptyStats
  | otherwise =
      let !mn = VP.foldl1' min vs
          !mx = VP.foldl1' max vs
          encMin = fLE mn
          encMax = fLE mx
      in Statistics (Just encMin) (Just encMax) (Just 0) Nothing
                   (Just encMin) (Just encMax)

-- | Compute Parquet 'Statistics' for a @DOUBLE@ column.
statisticsForDouble :: VP.Vector Double -> Statistics
statisticsForDouble vs
  | VP.null vs = emptyStats
  | otherwise =
      let !mn = VP.foldl1' min vs
          !mx = VP.foldl1' max vs
          encMin = dLE mn
          encMax = dLE mx
      in Statistics (Just encMin) (Just encMax) (Just 0) Nothing
                   (Just encMin) (Just encMax)

-- | Compute Parquet 'Statistics' for a @BOOLEAN@ column. Min is the
-- first @False@ that appears (otherwise @True@); max is the first @True@.
-- Encoded as a single byte (0 or 1) per the PLAIN spec.
statisticsForBool :: V.Vector Bool -> Statistics
statisticsForBool vs
  | V.null vs = emptyStats
  | otherwise =
      let hasFalse = V.any not vs
          hasTrue  = V.any id vs
          encMin = if hasFalse then BS.singleton 0 else BS.singleton 1
          encMax = if hasTrue  then BS.singleton 1 else BS.singleton 0
      in Statistics (Just encMin) (Just encMax) (Just 0) Nothing
                   (Just encMin) (Just encMax)

fLE :: Float -> ByteString
fLE v = BL.toStrict (B.toLazyByteString (B.word32LE (castFloatToWord32 v)))

dLE :: Double -> ByteString
dLE v = BL.toStrict (B.toLazyByteString (B.word64LE (castDoubleToWord64 v)))

-- ============================================================
-- Heterogeneous column data
-- ============================================================

-- | A single column's worth of values, tagged with its physical type.
-- Used by 'buildParquetFileTyped' / 'buildParquetFileTypedWithIndex' so
-- writers can emit any primitive type from one entry point.
data ColumnData
  = ColInt32     !(VP.Vector Int32)
  | ColInt64     !(VP.Vector Int64)
  | ColFloat     !(VP.Vector Float)
  | ColDouble    !(VP.Vector Double)
  | ColBool      !(V.Vector  Bool)
  | ColByteArray !(V.Vector ByteString)
  deriving (Show, Eq)

-- | Number of values in the column.
columnDataLength :: ColumnData -> Int
columnDataLength = \case
  ColInt32 v     -> VP.length v
  ColInt64 v     -> VP.length v
  ColFloat v     -> VP.length v
  ColDouble v    -> VP.length v
  ColBool v      -> V.length v
  ColByteArray v -> V.length v

-- | The Parquet physical type the column data should be written as.
columnDataParquetType :: ColumnData -> ParquetType
columnDataParquetType = \case
  ColInt32{}     -> PTInt32
  ColInt64{}     -> PTInt64
  ColFloat{}     -> PTFloat
  ColDouble{}    -> PTDouble
  ColBool{}      -> PTBoolean
  ColByteArray{} -> PTByteArray

-- | Encode the column as a single uncompressed @PLAIN@ @DATA_PAGE@.
encodeColumnDataPage :: ColumnData -> ByteString
encodeColumnDataPage = \case
  ColInt32 v     -> encodePlainInt32Page v
  ColInt64 v     -> encodePlainInt64Page v
  ColFloat v     -> encodePlainFloatPage v
  ColDouble v    -> encodePlainDoublePage v
  ColBool v      -> encodePlainBooleanPage v
  ColByteArray v -> encodePlainByteArrayPage v

-- | Compute Parquet 'Statistics' for the column.
columnDataStatistics :: ColumnData -> Statistics
columnDataStatistics = \case
  ColInt32 v     -> statisticsForInt32 v
  ColInt64 v     -> statisticsForInt64 v
  ColFloat v     -> statisticsForFloat v
  ColDouble v    -> statisticsForDouble v
  ColBool v      -> statisticsForBool v
  ColByteArray v -> statisticsForByteArray v

-- ============================================================
-- Nullable columns: definition levels + PLAIN values
-- ============================================================

-- | A column with optional values. Internally we store the present-flag
-- vector alongside the present-only values; readers reconstruct the
-- @Maybe@ shape via 'Parquet.Levels.materializePlainInt32Optional' and
-- friends.
data OptionalColumn
  = OptInt32     !(V.Vector (Maybe Int32))
  | OptInt64     !(V.Vector (Maybe Int64))
  | OptFloat     !(V.Vector (Maybe Float))
  | OptDouble    !(V.Vector (Maybe Double))
  | OptBool      !(V.Vector (Maybe Bool))
  | OptByteArray !(V.Vector (Maybe ByteString))
  deriving (Show, Eq)

optionalColumnLength :: OptionalColumn -> Int
optionalColumnLength = \case
  OptInt32 v     -> V.length v
  OptInt64 v     -> V.length v
  OptFloat v     -> V.length v
  OptDouble v    -> V.length v
  OptBool v      -> V.length v
  OptByteArray v -> V.length v

optionalColumnNullCount :: OptionalColumn -> Int
optionalColumnNullCount = \case
  OptInt32 v     -> V.length (V.filter (== Nothing) v)
  OptInt64 v     -> V.length (V.filter (== Nothing) v)
  OptFloat v     -> V.length (V.filter (== Nothing) v)
  OptDouble v    -> V.length (V.filter (== Nothing) v)
  OptBool v      -> V.length (V.filter (== Nothing) v)
  OptByteArray v -> V.length (V.filter (== Nothing) v)

-- | Strip nulls and return only the present values as a 'ColumnData'.
optionalColumnPresentValues :: OptionalColumn -> ColumnData
optionalColumnPresentValues = \case
  OptInt32 v     -> ColInt32     (VP.fromList [x | Just x <- V.toList v])
  OptInt64 v     -> ColInt64     (VP.fromList [x | Just x <- V.toList v])
  OptFloat v     -> ColFloat     (VP.fromList [x | Just x <- V.toList v])
  OptDouble v    -> ColDouble    (VP.fromList [x | Just x <- V.toList v])
  OptBool v      -> ColBool      (V.fromList  [x | Just x <- V.toList v])
  OptByteArray v -> ColByteArray (V.fromList  [x | Just x <- V.toList v])

-- | Encode an 'OptionalColumn' as a single uncompressed @PLAIN@
-- @DATA_PAGE@ V1 carrying the definition-level stream + present-only
-- PLAIN values.
encodeOptionalColumnPage :: OptionalColumn -> ByteString
encodeOptionalColumnPage oc =
  let !defs = VP.fromList
        [ if isPresent v then 1 else 0
        | v <- presenceList oc
        ]
      !defStream = LE.encodeLengthPrefixedHybrid 1 defs
      !valuesBs = encodeColumnDataPagePayload (optionalColumnPresentValues oc)
      !body = defStream <> valuesBs
      !bodySize = BS.length body
      !n = optionalColumnLength oc
      !hdr = mkPlainDataPageHeader (fromIntegral n) (fromIntegral bodySize)
   in encodePageHeader hdr <> body
  where
    isPresent (Just _) = True
    isPresent Nothing  = False

    presenceList :: OptionalColumn -> [Maybe ()]
    presenceList = \case
      OptInt32 v     -> map void (V.toList v)
      OptInt64 v     -> map void (V.toList v)
      OptFloat v     -> map void (V.toList v)
      OptDouble v    -> map void (V.toList v)
      OptBool v      -> map void (V.toList v)
      OptByteArray v -> map void (V.toList v)

    void Nothing  = Nothing
    void (Just _) = Just ()

-- | Encode just the PLAIN-values portion (no page header). Used inside
-- 'encodeOptionalColumnPage' so we can prepend the definition-level
-- stream and write a single page header.
encodeColumnDataPagePayload :: ColumnData -> ByteString
encodeColumnDataPagePayload = \case
  ColInt32 v     -> BL.toStrict $ B.toLazyByteString $
                     VP.foldl' (\b x -> b <> B.int32LE x) mempty v
  ColInt64 v     -> BL.toStrict $ B.toLazyByteString $
                     VP.foldl' (\b x -> b <> B.int64LE x) mempty v
  ColFloat v     -> BL.toStrict $ B.toLazyByteString $
                     VP.foldl' (\b x -> b <> writeFloatLE x) mempty v
  ColDouble v    -> BL.toStrict $ B.toLazyByteString $
                     VP.foldl' (\b x -> b <> writeDoubleLE x) mempty v
  ColBool v      ->
      let !n = V.length v
          !numBytes = (n + 7) `quot` 8
       in BL.toStrict $ B.toLazyByteString $
            flip foldMap [0 .. numBytes - 1] $ \byteIdx ->
              let goBit !bit !acc
                    | bit >= 8                = acc
                    | byteIdx * 8 + bit >= n  = acc
                    | otherwise =
                        let !x = V.unsafeIndex v (byteIdx * 8 + bit)
                            !flag = if x then (1 :: Int) * (2 ^ bit) else 0
                         in goBit (bit + 1) (acc + flag)
              in B.word8 (fromIntegral (goBit 0 0))
  ColByteArray v -> BL.toStrict $ B.toLazyByteString $
                     V.foldl' (\b x ->
                       b <> B.word32LE (fromIntegral (BS.length x))
                         <> B.byteString x
                     ) mempty v

-- ============================================================
-- Dictionary encoding (PLAIN_DICTIONARY / RLE_DICTIONARY)
-- ============================================================

-- | A computed dictionary for a single column chunk. 'dictUniques' holds
-- the column values in dictionary order; 'dictIndices' maps each row's
-- value to its dictionary index (0-based).
data Dictionary = Dictionary
  { dictUniques :: !ColumnData
  , dictIndices :: !(VP.Vector Int32)
  } deriving (Show, Eq)

-- | Compute a dictionary for a 'ColumnData' by deduplicating the input.
-- Order of unique values follows their first appearance.
buildDictionary :: ColumnData -> Dictionary
buildDictionary = \case
  ColInt32 v     -> generic (V.fromList (VP.toList v)) (\xs -> ColInt32 (VP.fromList xs))
  ColInt64 v     -> generic (V.fromList (VP.toList v)) (\xs -> ColInt64 (VP.fromList xs))
  ColFloat v     -> generic (V.fromList (VP.toList v)) (\xs -> ColFloat (VP.fromList xs))
  ColDouble v    -> generic (V.fromList (VP.toList v)) (\xs -> ColDouble (VP.fromList xs))
  ColBool v      -> generic (V.fromList (V.toList v))  (\xs -> ColBool  (V.fromList xs))
  ColByteArray v -> generic v                          (\xs -> ColByteArray (V.fromList xs))
  where
    generic
      :: (Eq a)
      => V.Vector a
      -> ([a] -> ColumnData)
      -> Dictionary
    generic xs reify =
      let !uniques = V.toList (V.uniq (V.fromList (V.toList (orderedUniq xs))))
          !lookupIdx = \x ->
            case lookup x (zip uniques [0 ..]) of
              Just i  -> fromIntegral (i :: Int) :: Int32
              Nothing -> 0
          !indices = VP.fromList (map lookupIdx (V.toList xs))
       in Dictionary { dictUniques = reify uniques, dictIndices = indices }

    -- Stable order-of-first-appearance unique extraction.
    orderedUniq :: Eq a => V.Vector a -> V.Vector a
    orderedUniq xs0 = V.fromList (go (V.toList xs0) [])
      where
        go [] acc = reverse acc
        go (x:xs) acc
          | x `elem` acc = go xs acc
          | otherwise    = go xs (x : acc)

-- | Encode a 'Dictionary' as a @DICTIONARY_PAGE@.
encodeDictPage :: Dictionary -> ByteString
encodeDictPage d =
  let !payload = encodeColumnDataPagePayload (dictUniques d)
      !numEntries = columnDataLength (dictUniques d)
      !hdr = PageHeader
        { phType = PtDictionaryPage DictionaryPageHeader
            { dictNumValues = fromIntegral numEntries
            , dictEncoding  = parquetEncodingPlainDictionary
            }
        , phUncompressedPageSize = Just (fromIntegral (BS.length payload))
        , phCompressedPageSize   = Just (fromIntegral (BS.length payload))
        }
   in encodePageHeader hdr <> payload

-- | Encode the 'DATA_PAGE' that consumes a dictionary (RLE-hybrid index
-- stream prefixed by a single byte recording the bit width).
encodeDictDataPage :: Dictionary -> ByteString
encodeDictDataPage d =
  let !indices = dictIndices d
      !numIndices = VP.length indices
      !uniqueCount = columnDataLength (dictUniques d)
      !bw = LE.bitWidthFor (max 0 (uniqueCount - 1))
      !indexStream = LE.encodeRLEHybrid bw indices
      !body = BS.singleton (fromIntegral bw) <> indexStream
      !hdr = PageHeader
        { phType = PtDataPage DataPageHeader
            { dphNumValues = fromIntegral numIndices
            , dphEncoding  = parquetEncodingRleDictionary
            }
        , phUncompressedPageSize = Just (fromIntegral (BS.length body))
        , phCompressedPageSize   = Just (fromIntegral (BS.length body))
        }
   in encodePageHeader hdr <> body

parquetEncodingPlainDictionary :: Int32
parquetEncodingPlainDictionary = 2

parquetEncodingRleDictionary :: Int32
parquetEncodingRleDictionary = 8

-- ============================================================
-- Indexed writer: bloom filter + page index footers
-- ============================================================

-- | Which Parquet data page header to emit for a column chunk.
--
-- - 'PageV1': @DATA_PAGE@ (legacy). Definition + repetition levels are
--   length-prefixed RLE-hybrid streams concatenated with the values
--   inside the (optionally compressed) page body.
-- - 'PageV2': @DATA_PAGE_V2@. Definition and repetition levels are NOT
--   length-prefixed and are NOT compressed. Only the values segment is
--   compressed, and the level segment lengths are recorded in the page
--   header so readers can skip directly to the data.
data PageVersion = PageV1 | PageV2
  deriving (Show, Eq)

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
  , caCodec        :: !Compression
    -- ^ Compression codec for the column-chunk page bytes. Default
    -- 'Uncompressed'; use any codec listed in 'Parquet.Compress.compressPageBytes'.
  , caPageVersion  :: !PageVersion
    -- ^ Page header version to emit. Defaults to 'PageV1'.
  } deriving (Show, Eq)

emptyColumnAux :: ColumnAux
emptyColumnAux = ColumnAux Nothing Nothing Nothing Uncompressed PageV1

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

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- ============================================================
-- Heterogeneous typed builders
-- ============================================================

-- | Build a complete Parquet file from a schema and one or more row
-- groups of typed column data. Each leaf in the schema (i.e. each
-- entry whose 'seType' is @Just@) must have a matching 'ColumnData'
-- column in every row group, in the same order. Produces @PAR1@
-- magic, uncompressed @PLAIN@ pages, footer, and trailing magic.
--
-- Use 'buildParquetFileWithIndex' to additionally emit bloom filter,
-- offset index, and column index regions.
buildParquetFile
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> ByteString
buildParquetFile schema rowGroups =
  buildParquetFileWithIndex schema rowGroups
    (V.map (V.map (const emptyColumnAux)) rowGroups)

-- | Build a Parquet file with optional bloom filter, offset index,
-- column index, and per-column compression. The 'ColumnAux' parallel
-- array specifies what to emit for each column; 'emptyColumnAux'
-- omits all extras.
--
-- File layout:
--
-- @
-- PAR1                         -- 4 bytes
-- <row group bytes ...>        -- compressed PLAIN data pages
-- <bloom filter bitsets ...>   -- one per column with caBloomFilter = Just
-- <offset index thrifts ...>   -- one per column with caOffsetIndex = Just
-- <column index thrifts ...>   -- one per column with caColumnIndex = Just
-- <footer>                     -- Thrift-encoded FileMetadata
-- <footer length: int32 LE>
-- PAR1
-- @
buildParquetFileWithIndex
  :: V.Vector SchemaElement
  -> V.Vector (V.Vector ColumnData)
  -> V.Vector (V.Vector ColumnAux)
  -> ByteString
buildParquetFileWithIndex schema rowGroups auxes =
  let -- Per-column page bytes plus uncompressed size, both required to
      -- populate ColumnMetadata. If a codec is requested but not built,
      -- the writer falls back to Uncompressed so callers get a usable
      -- file rather than a runtime crash.
      !encodedRGs = V.imap encodeRG rowGroups
      pageBytesOnly = V.map fst3
      !rowGroupBytes = concatMap (V.toList . pageBytesOnly) (V.toList encodedRGs)
      !rgBytesLen = sum (map BS.length rowGroupBytes)
      !startOfData = 4 :: Int
      !startOfBloom = startOfData + rgBytesLen

      (!rgMetasBase, _) = V.ifoldl' buildRG (V.empty, startOfData) encodedRGs

      (!rgMetasBloom, !bloomBytes, !endOfBloom) =
        layoutBlooms rgMetasBase auxes startOfBloom

      (!rgMetasOff, !offBytes, !endOfOff) =
        layoutOffsetIndex rgMetasBloom auxes endOfBloom
          (V.map pageBytesOnly encodedRGs)

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
    !leaves = V.filter (maybe False (const True) . seType) schema

    -- Encode each column's page, then compress it with the codec the
    -- caller requested via the matching ColumnAux. Returns the *final*
    -- bytes that go into the file plus the uncompressed size.
    encodeRG :: Int -> V.Vector ColumnData -> V.Vector (ByteString, Int, Compression)
    encodeRG rgIdx colsData =
      let !aux = if rgIdx < V.length auxes then V.unsafeIndex auxes rgIdx else V.empty
       in V.imap (encodeOne aux) colsData

    encodeOne
      :: V.Vector ColumnAux -> Int -> ColumnData -> (ByteString, Int, Compression)
    encodeOne aux cIdx cd =
      let !mAux = if cIdx < V.length aux
                    then Just (V.unsafeIndex aux cIdx)
                    else Nothing
          !codecRequested = maybe Uncompressed caCodec mAux
          !pageVer        = maybe PageV1 caPageVersion mAux
       in case pageVer of
            PageV1 ->
              let !raw = encodeColumnDataPage cd
                  (compressed, codecActual) = case Compress.compressPageBytes codecRequested raw of
                    Right cb -> (cb, codecRequested)
                    Left _   -> (raw, Uncompressed)
               in (compressed, BS.length raw, codecActual)
            PageV2 ->
              -- For V2 the codec is applied only to the values segment,
              -- but we account for the *full* page bytes (header +
              -- segments) when recording sizes on the column metadata so
              -- that ccTotalCompressedSize / ccTotalUncompressedSize
              -- reflect on-disk reality.
              let !raw      = encodeColumnDataPage cd
                  uncompSz  = BS.length raw
                  encoded   = case encodeColumnDataPageV2 codecRequested cd of
                    Right bs -> bs
                    Left  _  -> case encodeColumnDataPageV2 Uncompressed cd of
                                  Right bs -> bs
                                  Left  _  -> raw  -- defensive fallback
                  codecActual = if codecRequested == Uncompressed
                                  then Uncompressed
                                  else codecRequested
               in (encoded, uncompSz, codecActual)

    buildRG
      :: (V.Vector RowGroup, Int) -> Int -> V.Vector (ByteString, Int, Compression)
      -> (V.Vector RowGroup, Int)
    buildRG (!rgs, !off) rgIdx encodedCols =
      let !colsData = V.unsafeIndex rowGroups rgIdx
          (!cols, !off2) = V.ifoldl' (buildCol colsData) (V.empty, off) encodedCols
          !nRows = if V.null colsData
                     then 0
                     else fromIntegral (columnDataLength (V.unsafeIndex colsData 0))
          !rg = RowGroup
            { rgColumns = cols
            , rgTotalByteSize = fromIntegral (off2 - off)
            , rgNumRows = nRows
            }
      in (V.snoc rgs rg, off2)

    buildCol
      :: V.Vector ColumnData
      -> (V.Vector ColumnChunk, Int) -> Int -> (ByteString, Int, Compression)
      -> (V.Vector ColumnChunk, Int)
    buildCol colsData (!cs, !cOff) colIdx (pageBs, uncompSize, codec) =
      let !cd = V.unsafeIndex colsData colIdx
          !leaf = V.unsafeIndex leaves colIdx
          !sz = BS.length pageBs
          !cc = ColumnChunk
            { ccFilePath = Nothing
            , ccFileOffset = fromIntegral cOff
            , ccMetadata = Just ColumnMetadata
                { cmType = fromMaybe (columnDataParquetType cd) (seType leaf)
                , cmEncodings = V.singleton Plain
                , cmPathInSchema = V.singleton (seName leaf)
                , cmCodec = codec
                , cmNumValues = fromIntegral (columnDataLength cd)
                , cmTotalUncompressedSize = fromIntegral uncompSize
                , cmTotalCompressedSize = fromIntegral sz
                , cmDataPageOffset = fromIntegral cOff
                , cmStatistics = Just (columnDataStatistics cd)
                , cmBloomFilterOffset = Nothing
                , cmBloomFilterLength = Nothing
                }
            , ccOffsetIndexOffset = Nothing
            , ccOffsetIndexLength = Nothing
            , ccColumnIndexOffset = Nothing
            , ccColumnIndexLength = Nothing
            }
      in (V.snoc cs cc, cOff + sz)
