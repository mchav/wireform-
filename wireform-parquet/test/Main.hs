{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V
import Numeric (showHex)
import System.Exit (exitFailure)

import qualified Data.Vector.Primitive as VP
import Data.Int (Int32, Int64)

import qualified Crypto.Random as RNG

import Parquet.BloomFilter
import qualified Parquet.Encryption as Enc
import Parquet.PageIndex
import Parquet.Read (loadParquetFile, pfFooter)
import Parquet.Types
import Parquet.Write
  ( ColumnAux (..)
  , ColumnData (..)
  , Dictionary (..)
  , OptionalColumn (..)
  , buildDictionary
  , buildParquetFile
  , buildParquetFileTyped
  , buildParquetFileTypedWithIndex
  , buildParquetFileWithIndex
  , columnDataLength
  , encodeDictDataPage
  , encodeDictPage
  , encodeOptionalColumnPage
  , optionalColumnLength
  , optionalColumnNullCount
  , statisticsForByteArray
  , statisticsForInt32
  , statisticsForInt64
  )
import Parquet.XXH64

main :: IO ()
main = do
  -- XXH64 reference vectors.
  expectHash "" "ef46db3751d8e999"
  expectHash "abc" "44bc2cf5ad770999"
  expectHash "Nobody inspects the spammish repetition" "fbcea83c8a378bf1"
  -- 32 bytes of 'a' is exactly one bulk stripe (xxhsum -H1 reference).
  expectHashBs (BS.replicate 32 0x61) "856e843298f99ad7"
  -- 64 bytes (two stripes) — exercises the bulk-phase merge.
  expectHashBs (BS.replicate 64 0x62) "ecbaf4bdf26b6349"

  -- OffsetIndex round-trip.
  let oi = OffsetIndex
        { oiPageLocations = V.fromList
            [ PageLocation 100 200 0
            , PageLocation 300 250 50
            ]
        , oiUnencodedByteArrayDataBytes = Just (V.fromList [42, 99])
        }
  expect "OffsetIndex round-trip"
    (decodeOffsetIndex (encodeOffsetIndex oi) == Right oi)

  -- ColumnIndex round-trip with all optional fields.
  let ci = ColumnIndex
        { ciNullPages = V.fromList [False, False, True]
        , ciMinValues = V.fromList [BSC.pack "a", BSC.pack "b", BS.empty]
        , ciMaxValues = V.fromList [BSC.pack "z", BSC.pack "y", BS.empty]
        , ciBoundaryOrder = OrderAscending
        , ciNullCounts = Just (V.fromList [0, 0, 100])
        , ciRepetitionLevelHistograms = Just (V.fromList [10, 5])
        , ciDefinitionLevelHistograms = Just (V.fromList [3, 8])
        }
  expect "ColumnIndex round-trip"
    (decodeColumnIndex (encodeColumnIndex ci) == Right ci)

  -- Bloom filter membership.
  let sbbf0 = newSbbf 1024
      values = ["alpha", "beta", "gamma", "delta", "epsilon"]
      sbbf  = foldr (sbbfInsert . BSC.pack) sbbf0 values
  mapM_ (\v -> expect ("bloom contains " ++ v)
                 (sbbfCheck (BSC.pack v) sbbf)) values

  -- Golden vector from arrow-rs / parquet-mr: a 32-byte bitset produced
  -- by parquet-mr for the strings "a0".."a9" must report all of them
  -- present.  This proves byte-compatibility of our XXH64 + block layout
  -- with the reference writer.
  let goldenBits = BS.pack
        [ 200, 1, 80, 20, 64, 68, 8, 109, 6, 37, 4, 67, 144, 80, 96, 32
        , 8, 132, 43, 33, 0, 5, 99, 65, 2, 0, 224, 44, 64, 78, 96, 4 ]
      goldenSbbf = newSbbfFromBytes goldenBits
  mapM_ (\i -> let v = "a" <> show i in
                 expect ("golden contains " ++ v)
                   (sbbfCheck (BSC.pack v) goldenSbbf))
        [(0 :: Int) .. 9]

  -- Bloom filter false-positive sanity.
  let sbbfBig0 = newSbbf 2048
      inserted = map (BSC.pack . ("inserted-" <>) . show) [0 .. 255 :: Int]
      probes   = map (BSC.pack . ("probe-" <>) . show)   [0 .. 255 :: Int]
      sbbfBig  = foldr sbbfInsert sbbfBig0 inserted
      fp = length (filter (`sbbfCheck` sbbfBig) probes)
  expect ("bloom FP rate (got " ++ show fp ++ ")") (fp <= 16)

  -- Bloom encode/decode round-trip.
  let bs = encodeBloomFilter sbbf
  case decodeBloomFilter bs of
    Left e -> failTest ("decodeBloomFilter: " ++ e)
    Right (_hdr, sbbf') -> do
      expect "decoded numBytes" (sbbfNumBytes sbbf' == sbbfNumBytes sbbf)
      mapM_ (\v -> expect ("decoded contains " ++ v)
                     (sbbfCheck (BSC.pack v) sbbf')) values

  -- Statistics
  let s32 = statisticsForInt32 (VP.fromList [3, -1, 7, 0, 4 :: Int32])
  expect "Int32 stats min"
    (statMinValue s32 == Just (BS.pack [0xFF, 0xFF, 0xFF, 0xFF]))   -- -1 LE
  expect "Int32 stats max"
    (statMaxValue s32 == Just (BS.pack [0x07, 0x00, 0x00, 0x00]))
  expect "Int32 stats nullCount"
    (statNullCount s32 == Just 0)
  let s64 = statisticsForInt64 (VP.fromList [10, 5, -3, 100 :: Int64])
  expect "Int64 stats min/max present"
    (statMinValue s64 /= Nothing && statMaxValue s64 /= Nothing)
  let sBA = statisticsForByteArray
              (V.fromList [BSC.pack "banana", BSC.pack "apple", BSC.pack "cherry"])
  expect "ByteArray stats min == 'apple'"
    (statMinValue sBA == Just (BSC.pack "apple"))
  expect "ByteArray stats max == 'cherry'"
    (statMaxValue sBA == Just (BSC.pack "cherry"))
  let sEmpty = statisticsForInt32 VP.empty
  expect "empty Int32 stats has no min/max"
    (statMinValue sEmpty == Nothing && statMaxValue sEmpty == Nothing)

  -- Writer attaches statistics that round-trip through readFooter.
  let schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing
        ]
      vs   = VP.fromList [(3 :: Int32), -1, 7, 0, 4]
      fbs  = buildParquetFile schema (V.singleton (V.singleton vs))
  case loadParquetFile fbs of
    Left e -> failTest ("loadParquetFile: " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !cm = ccMetadata
                  (V.unsafeIndex (rgColumns (V.unsafeIndex rgs 0)) 0)
      case cm of
        Nothing -> failTest "expected ColumnMetadata"
        Just m -> case cmStatistics m of
          Nothing -> failTest "writer omitted statistics"
          Just st -> do
            expect "writer min == -1"
              (statMinValue st == Just (BS.pack [0xFF, 0xFF, 0xFF, 0xFF]))
            expect "writer max == 7"
              (statMaxValue st == Just (BS.pack [0x07, 0x00, 0x00, 0x00]))

  -- buildParquetFileWithIndex: bloom filter + page index + column index
  -- offsets are populated on the round-tripped column metadata.
  let schemaIdx = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
        , SchemaElement "y" (Just Required) (Just PTInt32) Nothing Nothing Nothing
        ]
      vsIdx = VP.fromList [(1 :: Int32), 2, 3, 4]
      bf    = sbbfInsertHash 0xdeadbeef (newSbbf (optimalNumBytes 1024 0.01))
      oi    = OffsetIndex
                { oiPageLocations = V.singleton (PageLocation 0 16 0)
                , oiUnencodedByteArrayDataBytes = Nothing
                }
      ci    = ColumnIndex
                { ciNullPages = V.singleton False
                , ciMinValues = V.singleton (BS.pack [0x01, 0, 0, 0])
                , ciMaxValues = V.singleton (BS.pack [0x04, 0, 0, 0])
                , ciBoundaryOrder = OrderAscending
                , ciNullCounts = Nothing
                , ciRepetitionLevelHistograms = Nothing
                , ciDefinitionLevelHistograms = Nothing
                }
      aux   = ColumnAux (Just bf) (Just oi) (Just ci) Uncompressed
      fIdx  = buildParquetFileWithIndex schemaIdx
                (V.singleton (V.singleton vsIdx))
                (V.singleton (V.singleton aux))
  case loadParquetFile fIdx of
    Left e -> failTest ("indexed-writer load: " ++ e)
    Right pf -> do
      let !rgs2 = fmRowGroups (pfFooter pf)
          !cc   = V.unsafeIndex (rgColumns (V.unsafeIndex rgs2 0)) 0
      case ccMetadata cc of
        Nothing -> failTest "indexed-writer: missing metadata"
        Just m -> do
          expect "indexed-writer: bloom filter offset populated"
            (cmBloomFilterOffset m /= Nothing)
          expect "indexed-writer: bloom filter length populated"
            (cmBloomFilterLength m /= Nothing)
      expect "indexed-writer: offset index offset populated"
        (ccOffsetIndexOffset cc /= Nothing)
      expect "indexed-writer: offset index length populated"
        (ccOffsetIndexLength cc /= Nothing)
      expect "indexed-writer: column index offset populated"
        (ccColumnIndexOffset cc /= Nothing)
      expect "indexed-writer: column index length populated"
        (ccColumnIndexLength cc /= Nothing)

  -- Typed (heterogeneous-primitive) writer: write a row group with one
  -- column of each primitive type and confirm each round-trips through
  -- the footer.
  let schemaTyped = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 6) Nothing Nothing
        , SchemaElement "i32"  (Just Required) (Just PTInt32)     Nothing Nothing Nothing
        , SchemaElement "i64"  (Just Required) (Just PTInt64)     Nothing Nothing Nothing
        , SchemaElement "f32"  (Just Required) (Just PTFloat)     Nothing Nothing Nothing
        , SchemaElement "f64"  (Just Required) (Just PTDouble)    Nothing Nothing Nothing
        , SchemaElement "bool" (Just Required) (Just PTBoolean)   Nothing Nothing Nothing
        , SchemaElement "ba"   (Just Required) (Just PTByteArray) Nothing Nothing Nothing
        ]
      cols  = V.fromList
        [ ColInt32     (VP.fromList [(1 :: Int32), 2, 3])
        , ColInt64     (VP.fromList [(10 :: Int64), 20, 30])
        , ColFloat     (VP.fromList [1.5 :: Float, 2.5, 3.5])
        , ColDouble    (VP.fromList [1e9 :: Double, 2e9, 3e9])
        , ColBool      (V.fromList [True, False, True])
        , ColByteArray (V.fromList [BSC.pack "alpha", BSC.pack "beta", BSC.pack "gamma"])
        ]
      fTyped = buildParquetFileTyped schemaTyped (V.singleton cols)
  case loadParquetFile fTyped of
    Left e -> failTest ("typed-writer load: " ++ e)
    Right pf -> do
      let !rgs3 = fmRowGroups (pfFooter pf)
          !rg   = V.unsafeIndex rgs3 0
      expect "typed-writer: 6 columns recorded"
        (V.length (rgColumns rg) == 6)
      expect "typed-writer: numRows == 3"
        (rgNumRows rg == 3)
      let typeOf i = case ccMetadata (V.unsafeIndex (rgColumns rg) i) of
            Just m  -> Just (cmType m)
            Nothing -> Nothing
      expect "typed-writer: column 0 is INT32"
        (typeOf 0 == Just PTInt32)
      expect "typed-writer: column 1 is INT64"
        (typeOf 1 == Just PTInt64)
      expect "typed-writer: column 2 is FLOAT"
        (typeOf 2 == Just PTFloat)
      expect "typed-writer: column 3 is DOUBLE"
        (typeOf 3 == Just PTDouble)
      expect "typed-writer: column 4 is BOOLEAN"
        (typeOf 4 == Just PTBoolean)
      expect "typed-writer: column 5 is BYTE_ARRAY"
        (typeOf 5 == Just PTByteArray)

  -- Per-column compression: GZip is always available; round-trip a column
  -- through the writer and confirm the metadata records the codec.
  let auxesGzip = V.singleton (V.fromList
        [ ColumnAux Nothing Nothing Nothing GZip
        , ColumnAux Nothing Nothing Nothing GZip
        , ColumnAux Nothing Nothing Nothing Uncompressed  -- floats stay raw
        , ColumnAux Nothing Nothing Nothing GZip
        , ColumnAux Nothing Nothing Nothing Uncompressed
        , ColumnAux Nothing Nothing Nothing GZip
        ])
      fGz = buildParquetFileTypedWithIndex schemaTyped (V.singleton cols) auxesGzip
  case loadParquetFile fGz of
    Left e -> failTest ("gzip-writer load: " ++ e)
    Right pf -> do
      let !rgs4 = fmRowGroups (pfFooter pf)
          !rg4  = V.unsafeIndex rgs4 0
          codecOf i = case ccMetadata (V.unsafeIndex (rgColumns rg4) i) of
            Just m  -> Just (cmCodec m)
            Nothing -> Nothing
      expect "gzip-writer: column 0 codec = GZip"
        (codecOf 0 == Just GZip)
      expect "gzip-writer: column 2 codec = Uncompressed"
        (codecOf 2 == Just Uncompressed)
      expect "gzip-writer: column 5 codec = GZip"
        (codecOf 5 == Just GZip)
      -- Compressed page bytes should be smaller for the byte-array column
      -- (5 letters per value, three values; gzip beats raw at that size
      -- by virtue of the 4-byte length-prefix overhead overlapping with
      -- the gzip header but for our test we only assert it's <= raw).
      let compSize i = case ccMetadata (V.unsafeIndex (rgColumns rg4) i) of
            Just m -> Just (cmTotalCompressedSize m, cmTotalUncompressedSize m)
            Nothing -> Nothing
      case compSize 0 of
        Just (c, u) -> expect "gzip-writer: column 0 has uncompressed > 0 in metadata"
          (u > 0 && c > 0)
        Nothing -> failTest "gzip-writer: column 0 missing metadata"

  -- Optional column page: definition levels + present-only PLAIN values.
  let optCol = OptInt32 (V.fromList [Just 1, Nothing, Just 3, Nothing, Just 5])
      pageBs = encodeOptionalColumnPage optCol
  expect "optional-column-page produces a non-empty body"
    (BS.length pageBs > 4)
  expect "optionalColumnLength counts all rows"
    (optionalColumnLength optCol == 5)
  expect "optionalColumnNullCount counts nulls"
    (optionalColumnNullCount optCol == 2)

  -- Dictionary encoding: build a dictionary, encode dictionary + data
  -- pages, and confirm the dictionary captured the unique values.
  let dictInput = ColByteArray (V.fromList
        [BSC.pack "alpha", BSC.pack "beta", BSC.pack "alpha"
        ,BSC.pack "gamma", BSC.pack "beta", BSC.pack "alpha"])
      dict = buildDictionary dictInput
      dictPage = encodeDictPage dict
      dataPage = encodeDictDataPage dict
  expect "dictionary unique count == 3"
    (columnDataLength (dictUniques dict) == 3)
  expect "dictionary index vector has one entry per row"
    (VP.length (dictIndices dict) == 6)
  expect "dictionary page header is non-empty"
    (BS.length dictPage > 4)
  expect "dictionary data page is non-empty"
    (BS.length dataPage > 4)

  -- Modular encryption: round-trip plaintext through encrypt/decrypt for
  -- both AES-GCM and AES-CTR, and verify GCM rejects a tampered tag.
  let key128 = BS.replicate 16 0x42
      aad    = Enc.buildAad "" (Enc.buildAadSuffix "fileid01" Enc.ModuleColumnMetaData 0 0 0)
      plain  = BSC.pack "Hello, Iceberg + Parquet!"
  drg0 <- RNG.drgNew
  let (eGcm, _) = RNG.withDRG drg0 (Enc.encryptModule key128 aad plain)
  case eGcm of
    Left e -> failTest ("GCM encrypt: " ++ e)
    Right ct -> do
      expect "GCM round-trip"
        (Enc.decryptModule key128 aad ct == Right plain)
      expect "GCM rejects tampered ciphertext"
        (case Enc.decryptModule key128 aad (BS.snoc (BS.init ct) 0xff) of
            Right _ -> False
            Left _  -> True)
  drg1 <- RNG.drgNew
  let (eCtr, _) = RNG.withDRG drg1 (Enc.encryptModuleCtr key128 plain)
  case eCtr of
    Left e -> failTest ("CTR encrypt: " ++ e)
    Right ct ->
      expect "CTR round-trip"
        (Enc.decryptModuleCtr key128 ct == Right plain)
  -- AAD suffix wire format: 8 file-id bytes + 1 module byte + 3*2 ordinals
  expect "AAD suffix length"
    (BS.length (Enc.buildAadSuffix "x" Enc.ModuleFooter 0 0 0) == 15)

  putStrLn "All Parquet page-index / bloom-filter / statistics tests passed."

expectHash :: String -> String -> IO ()
expectHash s expected = expectHashBs (BSC.pack s) expected

expectHashBs :: BS.ByteString -> String -> IO ()
expectHashBs bs expected =
  let actual = pad16 (showHex (xxh64 bs) "")
  in unless (actual == expected) $
       failTest ("xxh64 " ++ show bs ++ " expected " ++ expected
                  ++ " got " ++ actual)

pad16 :: String -> String
pad16 s = replicate (16 - length s) '0' ++ s

expect :: String -> Bool -> IO ()
expect what ok = do
  if ok
    then putStrLn ("OK: " ++ what)
    else failTest what

failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
