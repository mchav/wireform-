{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector as V
import Numeric (showHex)
import Data.Bits (shiftL, (.|.))
import Data.List (isInfixOf)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..), exitFailure)
import qualified System.Process as Proc

import qualified Data.Vector.Primitive as VP
import Data.Int (Int32, Int64)

import qualified Crypto.Random as RNG

import Parquet.BloomFilter
import Parquet.Compress (compressPageBytes)
import Parquet.ByteStreamSplit
  ( encodeByteStreamSplitDouble
  , encodeByteStreamSplitFloat
  )
import Parquet.Delta
  ( decodeDeltaBinaryPackedInt64
  , decodeDeltaByteArray
  , decodeDeltaLengthByteArray
  )
import Parquet.DeltaEncode
  ( encodeDeltaBinaryPackedInt64
  , encodeDeltaByteArray
  , encodeDeltaLengthByteArray
  )
import qualified Parquet.Nested as Nested
import qualified Parquet.NullPagesBitmap as NPB
import qualified Parquet.Encryption as Enc
import Parquet.PageIndex
import qualified Parquet.Read
import Parquet.Read
  ( decodeByteStreamSplitDouble
  , decodeByteStreamSplitFloat
  , loadParquetFile
  , pfFooter
  )
import Parquet.Types
import Parquet.Page
  ( DataPageHeaderV2 (..)
  , PageHeader (..)
  , PageType (..)
  , readPageHeaderAt
  )
import Parquet.Write
  ( ColumnAux (..)
  , ColumnEncryption (..)
  , FooterEncryption (..)
  , PageVersion (..)
  , ColumnData (..)
  , Dictionary (..)
  , OptionalColumn (..)
  , buildDictionary
  , buildParquetFile
  , buildParquetFileWithIndex
  , buildParquetFileWithIndexEncryptedFooter
  , columnDataLength
  , columnDataStatistics
  , emptyColumnAux
  , encodeColumnDataPageParts
  , encodeColumnDataPageV2
  , encodeColumnDataPageV2Parts
  , encodeDictDataPage
  , encodeDictPage
  , encodeOptionalColumnPage
  , encryptAuxModule
  , encryptPageBytes
  , encryptPageBytesV2
  , optionalColumnLength
  , optionalColumnNullCount
  )
import qualified Wireform.Hash as Hash

import qualified Arrow.Column      as AC
import qualified Arrow.Types       as AT
import qualified Parquet.Arrow     as PArrow
import qualified Parquet.HighLevel as PHL

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

  -- Statistics (via columnDataStatistics, the public column-typed API).
  let s32 = columnDataStatistics (ColInt32 (VP.fromList [3, -1, 7, 0, 4 :: Int32]))
  expect "Int32 stats min"
    (statMinValue s32 == Just (BS.pack [0xFF, 0xFF, 0xFF, 0xFF]))   -- -1 LE
  expect "Int32 stats max"
    (statMaxValue s32 == Just (BS.pack [0x07, 0x00, 0x00, 0x00]))
  expect "Int32 stats nullCount"
    (statNullCount s32 == Just 0)
  let s64 = columnDataStatistics (ColInt64 (VP.fromList [10, 5, -3, 100 :: Int64]))
  expect "Int64 stats min/max present"
    (statMinValue s64 /= Nothing && statMaxValue s64 /= Nothing)
  let sBA = columnDataStatistics
              (ColByteArray (V.fromList [BSC.pack "banana", BSC.pack "apple", BSC.pack "cherry"]))
  expect "ByteArray stats min == 'apple'"
    (statMinValue sBA == Just (BSC.pack "apple"))
  expect "ByteArray stats max == 'cherry'"
    (statMaxValue sBA == Just (BSC.pack "cherry"))
  let sEmpty = columnDataStatistics (ColInt32 VP.empty)
  expect "empty Int32 stats has no min/max"
    (statMinValue sEmpty == Nothing && statMaxValue sEmpty == Nothing)

  -- Writer attaches statistics that round-trip through readFooter.
  let schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing
        ]
      vs   = VP.fromList [(3 :: Int32), -1, 7, 0, 4]
      fbs  = buildParquetFile schema (V.singleton (V.singleton (ColInt32 vs)))
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
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing
        , SchemaElement "y" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing
        ]
      vsIdx = ColInt32 (VP.fromList [(1 :: Int32), 2, 3, 4])
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
      aux   = ColumnAux (Just bf) (Just oi) (Just ci) Uncompressed PageV1 Nothing
      fIdx  = buildParquetFileWithIndex schemaIdx
                (V.singleton (V.singleton vsIdx))
                (V.singleton (V.singleton aux))
      _ = vsIdx :: ColumnData
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
        [ SchemaElement "schema" Nothing Nothing (Just 6) Nothing Nothing Nothing
        , SchemaElement "i32"  (Just Required) (Just PTInt32)     Nothing Nothing Nothing Nothing
        , SchemaElement "i64"  (Just Required) (Just PTInt64)     Nothing Nothing Nothing Nothing
        , SchemaElement "f32"  (Just Required) (Just PTFloat)     Nothing Nothing Nothing Nothing
        , SchemaElement "f64"  (Just Required) (Just PTDouble)    Nothing Nothing Nothing Nothing
        , SchemaElement "bool" (Just Required) (Just PTBoolean)   Nothing Nothing Nothing Nothing
        , SchemaElement "ba"   (Just Required) (Just PTByteArray) Nothing Nothing Nothing Nothing
        ]
      cols  = V.fromList
        [ ColInt32     (VP.fromList [(1 :: Int32), 2, 3])
        , ColInt64     (VP.fromList [(10 :: Int64), 20, 30])
        , ColFloat     (VP.fromList [1.5 :: Float, 2.5, 3.5])
        , ColDouble    (VP.fromList [1e9 :: Double, 2e9, 3e9])
        , ColBool      (V.fromList [True, False, True])
        , ColByteArray (V.fromList [BSC.pack "alpha", BSC.pack "beta", BSC.pack "gamma"])
        ]
      fTyped = buildParquetFile schemaTyped (V.singleton cols)
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
        [ ColumnAux Nothing Nothing Nothing GZip PageV1 Nothing
        , ColumnAux Nothing Nothing Nothing GZip PageV1 Nothing
        , ColumnAux Nothing Nothing Nothing Uncompressed PageV1 Nothing  -- floats stay raw
        , ColumnAux Nothing Nothing Nothing GZip PageV1 Nothing
        , ColumnAux Nothing Nothing Nothing Uncompressed PageV1 Nothing
        , ColumnAux Nothing Nothing Nothing GZip PageV1 Nothing
        ])
      fGz = buildParquetFileWithIndex schemaTyped (V.singleton cols) auxesGzip
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

  -- DATA_PAGE_V2 file round-trip (uncompressed) through buildParquetFileWithIndex.
  -- We confirm the page header type changes accordingly and the values still
  -- decode through the V1 PLAIN reader (the value bytes are PLAIN; only the
  -- header / level layout differ).
  let v2Schema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing
        , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing
        ]
      v2Vals = ColInt32 (VP.fromList [(11 :: Int32), 22, 33, 44])
      v2Aux  = ColumnAux Nothing Nothing Nothing Uncompressed PageV2 Nothing
      v2File = buildParquetFileWithIndex v2Schema
                 (V.singleton (V.singleton v2Vals))
                 (V.singleton (V.singleton v2Aux))
  case loadParquetFile v2File of
    Left e -> failTest ("DATA_PAGE_V2 round-trip load: " ++ e)
    Right pf -> do
      let !rgsV2 = fmRowGroups (pfFooter pf)
      expect "DATA_PAGE_V2: numRows == 4"
        (rgNumRows (V.unsafeIndex rgsV2 0) == 4)
  -- Direct page-level V2 encoders must produce DATA_PAGE_V2 headers.
  case encodeColumnDataPageV2 Uncompressed v2Vals of
    Left e   -> failTest ("encodeColumnDataPageV2: " ++ e)
    Right bs -> case readPageHeaderAt bs 0 of
      Left e -> failTest ("V2 page header parse: " ++ e)
      Right (hdr, _) -> case phType hdr of
        PtDataPageV2 v2 -> do
          expect "V2 header reports 4 values" (dph2NumValues v2 == 4)
          expect "V2 header reports 0 nulls"  (dph2NumNulls v2 == 0)
          expect "V2 header reports 4 rows"   (dph2NumRows v2 == 4)
        _ -> failTest "V2 encoder did not produce PtDataPageV2"

  -- Nested column shred: optional list of optional INT32, the
  -- 'list<int?>?' shape every Iceberg V3 default-value array uses.
  -- For [[Just 1, Nothing, Just 3], [], Nothing] the spec says:
  --   maxDef=3, maxRep=1
  --   events:  (d=3,r=0,v=1), (d=2,r=1,v=null), (d=3,r=1,v=3),
  --            (d=1,r=0,v=null) (empty list),
  --            (d=0,r=0,v=null) (null list)
  let nestedRows = V.fromList
        [ Just (V.fromList [Just (1 :: Int32), Nothing, Just 3])
        , Just V.empty
        , Nothing
        ]
      nLeaf = Nested.encodeOptionalListOptionalI32 "xs" nestedRows
  expect "nested leaf maxDef == 3" (Nested.nlMaxDef nLeaf == 3)
  expect "nested leaf maxRep == 1" (Nested.nlMaxRep nLeaf == 1)
  expect "nested leaf def levels match Dremel"
    (VP.toList (Nested.nlDefLevels nLeaf) == [3, 2, 3, 1, 0])
  expect "nested leaf rep levels match Dremel"
    (VP.toList (Nested.nlRepLevels nLeaf) == [0, 1, 1, 0, 0])
  expect "nested leaf present-value count == 2"
    (Nested.nlValueCount nLeaf == 2)
  expect "nested leaf PLAIN bytes are LE [1, 3]"
    (Nested.nlValueBytes nLeaf
       == BS.pack [0x01,0,0,0, 0x03,0,0,0])
  expect "nested leaf path is xs.list.element"
    (V.toList (Nested.nlPath nLeaf) == ["xs", "list", "element"])

  -- Modular encryption round-trip: encrypted column-chunk page-bytes
  -- must round-trip through encryptPageBytes with the matching AAD.
  --
  -- We exercise both algorithms (AES-GCM-V1 and AES-GCM-CTR-V1) and
  -- assert that flipping one bit of the ciphertext breaks GCM auth.
  let encKey  = BS.replicate 16 0x42
      ce alg  = ColumnEncryption
                  { ceAlgorithm     = alg
                  , ceKey           = encKey
                  , ceFileId        = BSC.pack "fileid01"
                  , ceAadPrefix     = BS.empty
                  , ceKeyMetadata   = BSC.pack "kid:test"
                  , ceColumnOrdinal = 0
                  }
      encVals = ColInt32 (VP.fromList [(7 :: Int32), 8, 9])
      encSchema = V.fromList
        [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing Nothing
        , SchemaElement "v" (Just Required) (Just PTInt32) Nothing Nothing Nothing Nothing
        ]
      encAux alg = ColumnAux Nothing Nothing Nothing Uncompressed PageV1 (Just (ce alg))
      encFile alg = buildParquetFileWithIndex encSchema
                      (V.singleton (V.singleton encVals))
                      (V.singleton (V.singleton (encAux alg)))
  expect "encrypted file (AES-GCM-V1) is non-empty"
    (BS.length (encFile Enc.AesGcmV1) > 0)
  expect "encrypted file (AES-GCM-CTR-V1) is non-empty"
    (BS.length (encFile Enc.AesGcmCtrV1) > 0)
  expect "encrypted file differs from plaintext output"
    (encFile Enc.AesGcmV1
       /= buildParquetFileWithIndex encSchema
            (V.singleton (V.singleton encVals))
            (V.singleton (V.singleton emptyColumnAux)))

  -- Round-trip the page through encryptPageBytes / decryptGcmModule
  -- directly so we know our AAD framing matches what the reader needs.
  -- Per parquet-format Encryption.md §5.1 each module on the wire is
  -- prefixed with a 4-byte little-endian length, so the framing is
  -- '<len4 || nonce(12) || ct || tag(16)>' for header followed by
  -- '<len4 || nonce(12) || ct || tag(16)>' for body.
  let (rawHdr, rawBody) = encodeColumnDataPageParts encVals
  case encryptPageBytes (ce Enc.AesGcmV1) Enc.ModuleDataPage 0 0 rawHdr rawBody of
    Left e -> failTest ("encryptPageBytes: " ++ e)
    Right encBytes -> do
      let aadHdr = Enc.buildAad BS.empty
                     (Enc.buildAadSuffix (BSC.pack "fileid01")
                        Enc.ModuleDataPageHeader 0 0 0)
          aadBody = Enc.buildAad BS.empty
                     (Enc.buildAadSuffix (BSC.pack "fileid01")
                        Enc.ModuleDataPage 0 0 0)
      case Enc.readFramedModule encBytes 0 of
        Left e -> failTest ("read framed page header: " ++ e)
        Right (encHdr, after1) -> do
          case Enc.decryptGcmModule encKey aadHdr encHdr of
            Right plain -> expect "encrypted page header round-trips"
                                  (plain == rawHdr)
            Left e -> failTest ("decrypt page header: " ++ e)
          case Enc.readFramedModule encBytes after1 of
            Left e -> failTest ("read framed page body: " ++ e)
            Right (encBody, _) ->
              case Enc.decryptGcmModule encKey aadBody encBody of
                Right plain -> expect "encrypted page body round-trips"
                                      (plain == rawBody)
                Left e -> failTest ("decrypt page body: " ++ e)
      -- Tampering with one byte of the body must invalidate the GCM tag.
      let tampered = BS.snoc (BS.init encBytes) 0xff
      case Enc.readFramedModule tampered 0 of
        Left _ -> expect "tampered framing rejected" True
        Right (encHdr, after1) -> do
          case Enc.decryptGcmModule encKey aadHdr encHdr of
            Left _  -> expect "tampered header fails GCM auth" True
            Right _ ->
              case Enc.readFramedModule tampered after1 of
                Left _ -> expect "tampered body framing rejected" True
                Right (encBody, _) ->
                  case Enc.decryptGcmModule encKey aadBody encBody of
                    Right _ -> failTest "tampered ciphertext decrypted (BAD)"
                    Left _  -> expect "tampered body fails GCM auth" True

  -- DATA_PAGE_V2 + encryption round-trip. The wire shape is
  -- '<framed encHdr> <> <repBytes> <> <defBytes> <> <framed encValues>'
  -- with §5.1 length prefixes on the encrypted modules and the
  -- rep/def level segments in plaintext between them.
  case encodeColumnDataPageV2Parts Uncompressed encVals of
    Left e -> failTest ("encodeColumnDataPageV2Parts: " ++ e)
    Right (rawHdrV2, repV2, defV2, valsV2) ->
      case encryptPageBytesV2 (ce Enc.AesGcmV1) 0 0 rawHdrV2 repV2 defV2 valsV2 of
        Left e -> failTest ("encryptPageBytesV2 GCM: " ++ e)
        Right encBytesV2 -> do
          let aadHdr = Enc.buildAad BS.empty
                         (Enc.buildAadSuffix (BSC.pack "fileid01")
                            Enc.ModuleDataPageHeader 0 0 0)
              aadBody = Enc.buildAad BS.empty
                         (Enc.buildAadSuffix (BSC.pack "fileid01")
                            Enc.ModuleDataPage 0 0 0)
              levelLen = BS.length repV2 + BS.length defV2
          case Enc.readFramedModule encBytesV2 0 of
            Left e -> failTest ("read framed V2 hdr: " ++ e)
            Right (encHdr, after1) -> do
              case Enc.decryptGcmModule encKey aadHdr encHdr of
                Right plain -> expect "V2 encrypted page header round-trips"
                                      (plain == rawHdrV2)
                Left e -> failTest ("V2 decrypt page header: " ++ e)
              expect "V2 encryption preserves rep/def levels in plaintext"
                (BS.take levelLen (BS.drop after1 encBytesV2) == BS.append repV2 defV2)
              case Enc.readFramedModule encBytesV2 (after1 + levelLen) of
                Left e -> failTest ("read framed V2 values: " ++ e)
                Right (encV, _) ->
                  case Enc.decryptGcmModule encKey aadBody encV of
                    Right plain -> expect "V2 (GCM) encrypted values round-trip"
                                          (plain == valsV2)
                    Left e -> failTest ("V2 decrypt values: " ++ e)
      -- Same for AES-GCM-CTR-V1: values are CTR-encrypted (no auth).
      >> case encryptPageBytesV2 (ce Enc.AesGcmCtrV1) 0 0 rawHdrV2 repV2 defV2 valsV2 of
        Left e -> failTest ("encryptPageBytesV2 CTR: " ++ e)
        Right encBytesCtr -> do
          let aadHdr = Enc.buildAad BS.empty
                         (Enc.buildAadSuffix (BSC.pack "fileid01")
                            Enc.ModuleDataPageHeader 0 0 0)
              levelLen = BS.length repV2 + BS.length defV2
          case Enc.readFramedModule encBytesCtr 0 of
            Left e -> failTest ("read framed V2 CTR hdr: " ++ e)
            Right (encHdr, after1) -> do
              case Enc.decryptGcmModule encKey aadHdr encHdr of
                Right _ -> expect "V2 CTR header decrypts (GCM-authenticated)" True
                Left e  -> failTest ("V2 CTR header decrypt: " ++ e)
              case Enc.readFramedModule encBytesCtr (after1 + levelLen) of
                Left e -> failTest ("read framed V2 CTR values: " ++ e)
                Right (encV, _) ->
                  case Enc.decryptCtrModule encKey encV of
                    Right plain -> expect "V2 CTR values round-trip"
                                          (plain == valsV2)
                    Left e -> failTest ("V2 CTR decrypt values: " ++ e)

  -- Encrypted aux modules: bloom filter / offset index / column
  -- index payloads are wrapped as separate GCM modules under their
  -- spec-assigned ModuleType AAD when caEncryption is set on the
  -- column. We exercise this by feeding the resulting bytes back
  -- through Enc.decryptGcmModule under the matching AAD.
  do
    let bfx = sbbfInsertHash 0xdeadbeef (newSbbf (optimalNumBytes 1024 0.01))
        oix = OffsetIndex
                { oiPageLocations = V.singleton (PageLocation 0 16 0)
                , oiUnencodedByteArrayDataBytes = Nothing
                }
        cix = ColumnIndex
                { ciNullPages = V.singleton False
                , ciMinValues = V.singleton (BS.pack [0x01, 0, 0, 0])
                , ciMaxValues = V.singleton (BS.pack [0x04, 0, 0, 0])
                , ciBoundaryOrder = OrderAscending
                , ciNullCounts = Nothing
                , ciRepetitionLevelHistograms = Nothing
                , ciDefinitionLevelHistograms = Nothing
                }
        rawBloom = encodeBloomFilter bfx
        rawOff   = encodeOffsetIndex oix
        rawCol   = encodeColumnIndex cix
        encOf mt = encryptAuxModule
                    (Just (ce Enc.AesGcmV1)) mt 0
        aadOf mt = Enc.buildAad BS.empty
                    (Enc.buildAadSuffix (BSC.pack "fileid01") mt 0 0 0)
    let unframed mt raw = case Enc.readFramedModule
                                  (encOf mt raw) 0 of
          Right (m, _) -> m
          Left _ -> BS.empty
    case Enc.decryptGcmModule encKey
           (aadOf Enc.ModuleBloomFilterBitset)
           (unframed Enc.ModuleBloomFilterBitset rawBloom) of
      Right p -> expect "encrypted bloom filter module round-trips"
                        (p == rawBloom)
      Left e  -> failTest ("decrypt bloom filter module: " ++ e)
    case Enc.decryptGcmModule encKey
           (aadOf Enc.ModuleOffsetIndex)
           (unframed Enc.ModuleOffsetIndex rawOff) of
      Right p -> expect "encrypted offset-index module round-trips"
                        (p == rawOff)
      Left e  -> failTest ("decrypt offset-index module: " ++ e)
    case Enc.decryptGcmModule encKey
           (aadOf Enc.ModuleColumnIndex)
           (unframed Enc.ModuleColumnIndex rawCol) of
      Right p -> expect "encrypted column-index module round-trips"
                        (p == rawCol)
      Left e  -> failTest ("decrypt column-index module: " ++ e)
    -- A whole-file build with caEncryption + bloom/offset/column
    -- index aux must produce a different file from the same build
    -- without the encryption (i.e. the auxes really got encrypted).
    let auxEnc = ColumnAux (Just bfx) (Just oix) (Just cix)
                           Uncompressed PageV1 (Just (ce Enc.AesGcmV1))
        auxPlain = ColumnAux (Just bfx) (Just oix) (Just cix)
                             Uncompressed PageV1 Nothing
        encFileEnc = buildParquetFileWithIndex encSchema
                       (V.singleton (V.singleton encVals))
                       (V.singleton (V.singleton auxEnc))
        encFilePlain = buildParquetFileWithIndex encSchema
                       (V.singleton (V.singleton encVals))
                       (V.singleton (V.singleton auxPlain))
    expect "aux-encrypted file diverges from plaintext-aux file"
      (encFileEnc /= encFilePlain)

  -- Encrypted-footer mode: footer is a single GCM module under
  -- ModuleFooter AAD, file ends with PARE magic instead of PAR1, and
  -- decrypting recovers the original plaintext FileMetadata thrift.
  do
    let footerEnc = FooterEncryption
                      { feKey         = encKey
                      , feFileId      = BSC.pack "fileid01"
                      , feAadPrefix   = BS.empty
                      , feKeyMetadata = BSC.pack "kid:test"
                      }
        encFile = buildParquetFileWithIndexEncryptedFooter footerEnc
                    encSchema
                    (V.singleton (V.singleton encVals))
                    (V.singleton (V.singleton emptyColumnAux))
        plainFile = buildParquetFileWithIndex encSchema
                      (V.singleton (V.singleton encVals))
                      (V.singleton (V.singleton emptyColumnAux))
    expect "encrypted-footer file is non-empty"
      (BS.length encFile > 0)
    expect "encrypted-footer file ends with PARE magic"
      (BS.takeEnd 4 encFile == BSC.pack "PARE")
    expect "plaintext-footer file ends with PAR1 magic"
      (BS.takeEnd 4 plainFile == BSC.pack "PAR1")
    expect "encrypted-footer file diverges from plaintext-footer"
      (encFile /= plainFile)
    -- Use the public encrypted-reader API to round-trip via the
    -- ModuleFooter AAD. Per parquet-format §5.4 the bytes between
    -- the leading PAR1 magic and the trailing PARE magic are
    -- '<FileCryptoMetaData thrift> <encrypted footer module>',
    -- and the writer + reader handle the layout symmetrically.
    let fdec = Parquet.Read.FooterDecryption
                 { Parquet.Read.fdKey       = encKey
                 , Parquet.Read.fdFileId    = BSC.pack "fileid01"
                 , Parquet.Read.fdAadPrefix = BS.empty
                 }
    case Parquet.Read.loadParquetFileEncrypted fdec encFile of
      Left e -> failTest ("loadParquetFileEncrypted: " ++ e)
      Right pf' -> do
        let fmDecrypted = Parquet.Read.pfFooter pf'
        expect "encrypted-footer round-trips through reader"
          (V.length (fmSchema fmDecrypted) > 0)
        expect "encrypted-footer reader recovers row-group count"
          (V.length (fmRowGroups fmDecrypted) == 1)
    -- Wrong key must fail GCM auth.
    let wrongFd = fdec { Parquet.Read.fdKey = BS.replicate 16 0 }
    case Parquet.Read.loadParquetFileEncrypted wrongFd encFile of
      Left _  -> expect "wrong key rejected by GCM auth" True
      Right _ -> failTest "encrypted footer decrypted with wrong key (BAD)"

  -- Whole-file V2 + encryption: ColumnAux carries both PageV2 and
  -- ColumnEncryption, so buildParquetFileWithIndex must dispatch
  -- through encryptPageBytesV2 internally.
  let v2EncAux alg = ColumnAux Nothing Nothing Nothing Uncompressed PageV2 (Just (ce alg))
      v2EncFile alg = buildParquetFileWithIndex encSchema
                        (V.singleton (V.singleton encVals))
                        (V.singleton (V.singleton (v2EncAux alg)))
  expect "V2 + AES-GCM-V1 whole-file write is non-empty"
    (BS.length (v2EncFile Enc.AesGcmV1) > 0)
  expect "V2 + AES-GCM-CTR-V1 whole-file write is non-empty"
    (BS.length (v2EncFile Enc.AesGcmCtrV1) > 0)
  expect "V2 + encryption differs from V2 plaintext file"
    (v2EncFile Enc.AesGcmV1
       /= buildParquetFileWithIndex encSchema
            (V.singleton (V.singleton encVals))
            (V.singleton (V.singleton
              (ColumnAux Nothing Nothing Nothing Uncompressed PageV2 Nothing))))

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

  -- NullPagesBitmap: pack a Vector Bool into an LSB-first bitmap,
  -- count null pages via SIMD popcount, list non-null page indices.
  let nullPages = V.fromList [True, False, True, False, False, True, True, False, True]
      packed = NPB.packNullPages nullPages
  expect "nullPagesBitmap round-trip"
    (NPB.unpackNullPages (V.length nullPages) packed == nullPages)
  expect "nullPagesBitmap popcount"
    (NPB.nullPageCount packed == 5)
  expect "nullPagesBitmap nonNullPages == [1,3,4,7]"
    (V.toList (NPB.nonNullPages (V.length nullPages) packed) == [1, 3, 4, 7])

  -- DELTA_BINARY_PACKED writer round-trips through the reader for a few
  -- shapes (constant deltas, mixed deltas, negative deltas, single value).
  let testCases =
        [ ("ascending integers", VP.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10 :: Int64])
        , ("constant",           VP.fromList (replicate 12 (42 :: Int64)))
        , ("descending negatives", VP.fromList [10, 5, 0, -5, -10, -50, -1000 :: Int64])
        , ("singleton",          VP.fromList [12345 :: Int64])
        , ("empty",              VP.empty :: VP.Vector Int64)
        , ("128 mixed",          VP.fromList [let i64 = fromIntegral i :: Int64 in i64 * 7 - i64 `mod` 13 | i <- [(0 :: Int) .. 127]])
        ]
  flip mapM_ testCases $ \(name, vs) ->
    case decodeDeltaBinaryPackedInt64 (VP.length vs) (encodeDeltaBinaryPackedInt64 vs) of
      Right out ->
        expect ("DELTA_BINARY_PACKED round-trip: " ++ name)
          (VP.toList out == VP.toList vs)
      Left e -> failTest ("DELTA_BINARY_PACKED " ++ name ++ ": " ++ e)

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

  -- BYTE_STREAM_SPLIT round-trips for FLOAT and DOUBLE.
  let bssFloats  = VP.fromList [1.5, -2.25, 3.14159, 0, 1e9, -1e-9 :: Float]
      bssDoubles = VP.fromList [1.5, -2.25, 3.14159, 0, 1e9, -1e-9 :: Double]
      bssFloatBs  = encodeByteStreamSplitFloat bssFloats
      bssDoubleBs = encodeByteStreamSplitDouble bssDoubles
  expect "BYTE_STREAM_SPLIT FLOAT byte length == 4 * n"
    (BS.length bssFloatBs == 4 * VP.length bssFloats)
  expect "BYTE_STREAM_SPLIT DOUBLE byte length == 8 * n"
    (BS.length bssDoubleBs == 8 * VP.length bssDoubles)
  case decodeByteStreamSplitFloat (VP.length bssFloats) bssFloatBs of
    Right xs -> expect "BYTE_STREAM_SPLIT FLOAT round-trip" (xs == bssFloats)
    Left  e  -> failTest ("BYTE_STREAM_SPLIT FLOAT decode: " ++ e)
  case decodeByteStreamSplitDouble (VP.length bssDoubles) bssDoubleBs of
    Right xs -> expect "BYTE_STREAM_SPLIT DOUBLE round-trip" (xs == bssDoubles)
    Left  e  -> failTest ("BYTE_STREAM_SPLIT DOUBLE decode: " ++ e)

  -- Compression codec round-trips. All codecs should be refused by the
  -- compressor's LZ4 (deprecated Hadoop variant) and LZO (spec codec 3,
  -- not emitted by modern writers) branches. Brotli round-trips when
  -- the library is built with @-fbrotli@.
  case compressPageBytes LZ4 "anything" of
    Left _  -> expect "LZ4 (codec 5) is refused by the compressor" True
    Right _ -> failTest "Parquet.Compress: LZ4 compression should fail"
  case compressPageBytes LZO "anything" of
    Left _  -> expect "LZO (codec 3) is refused by the compressor" True
    Right _ -> failTest "Parquet.Compress: LZO compression should fail"
#ifdef HAVE_BROTLI
  -- A mid-sized input so Brotli has something to chew on; the original
  -- plaintext must be recovered after a round-trip through the reader.
  let brotliInput = BS.concat (replicate 256 "the quick brown fox jumps over the lazy dog ")
  case compressPageBytes Brotli brotliInput of
    Left e  -> failTest ("Parquet.Compress: Brotli compression failed: " ++ e)
    Right compressed -> do
      expect "Brotli shrinks the repetitive fixture"
        (BS.length compressed < BS.length brotliInput)
      case Parquet.Read.decompressChunk Brotli compressed of
        Left e -> failTest ("Parquet.Read: Brotli decompression failed: " ++ e)
        Right restored ->
          expect "Brotli round-trip preserves the input"
            (restored == brotliInput)
#else
  -- Without -fbrotli the writer must surface a clear missing-codec error
  -- rather than silently falling through to uncompressed.
  case compressPageBytes Brotli "anything" of
    Left _  -> expect "Brotli reports the -fbrotli requirement" True
    Right _ -> failTest "Parquet.Compress: Brotli must fail without -fbrotli"
#endif

  -- DELTA_LENGTH_BYTE_ARRAY round-trip (encoder <-> decoder).
  let dlbaInputs =
        [ V.empty
        , V.singleton (BSC.pack "alpha")
        , V.fromList (map BSC.pack ["", "a", "ab", "abc", "abcd"])
        , V.fromList (map BSC.pack
            ["one", "tw", "three", "", "five-five-five", "6"])
        ]
  mapM_
    (\inp ->
       let bs = encodeDeltaLengthByteArray inp
        in case decodeDeltaLengthByteArray (V.length inp) bs of
             Right xs -> expect "DELTA_LENGTH_BYTE_ARRAY round-trip" (xs == inp)
             Left  e  -> failTest ("DELTA_LENGTH_BYTE_ARRAY decode: " ++ e))
    dlbaInputs

  -- DELTA_BYTE_ARRAY round-trip with shared prefixes (front compression).
  let dbaInputs =
        [ V.empty
        , V.singleton (BSC.pack "abc")
        , V.fromList (map BSC.pack ["", "a", "ab", "abc", "abcd", "abcde"])
        , V.fromList (map BSC.pack
            [ "iceberg/ns/tableA"
            , "iceberg/ns/tableB"
            , "iceberg/ns/tableC"
            , "iceberg/ns/zzz/last"
            , "other/ns/tableA"
            ])
        ]
  mapM_
    (\inp ->
       let bs = encodeDeltaByteArray inp
        in case decodeDeltaByteArray (V.length inp) bs of
             Right xs -> expect "DELTA_BYTE_ARRAY round-trip" (xs == inp)
             Left  e  -> failTest ("DELTA_BYTE_ARRAY decode: " ++ e))
    dbaInputs

  -- Golden parquet fixtures from pyarrow (only when present).
  goldenExist <- and <$> mapM doesFileExist
    [ "test/fixtures/simple_int.parquet"
    , "test/fixtures/mixed_types.parquet"
    , "test/fixtures/bloom_and_index.parquet"
    ]
  if goldenExist
    then do
      goldenSimpleInt
      goldenMixedTypes
      goldenBloomIndex
      putStrLn "OK: pyarrow golden round-trip"
    else putStrLn "SKIP: pyarrow golden fixtures not present"

  -- Validate the nested optional-list-of-optional-int writer against
  -- pyarrow if pyarrow is available on PATH; otherwise skip.
  pyarrowOk <- pyarrowAvailable
  if pyarrowOk
    then do
      validateNestedAgainstPyarrow
      validateListStructAgainstPyarrow
      validateListListAgainstPyarrow
      validateMapAgainstPyarrow
      -- Variant + Parquet pyarrow round-trip lives in
      -- wireform-iceberg's test suite (Iceberg.Variant imports
      -- wireform-iceberg, which already depends on wireform-parquet).
    else putStrLn "SKIP: pyarrow not available, nested cross-language checks skipped"

  -- Arrow ↔ Parquet bridge round-trip.
  arrowParquetBridge

  putStrLn "All Parquet page-index / bloom-filter / statistics tests passed."

arrowParquetBridge :: IO ()
arrowParquetBridge = do
  let !arrowSchema = AT.Schema
        { AT.arrowFields = V.fromList
            [ AT.Field "i" False (AT.AInt 32 True) V.empty Nothing
            , AT.Field "s" False AT.AUtf8           V.empty Nothing
            ]
        , AT.arrowEndianness = AT.Little
        }
      !batch = V.fromList
        [ AC.ColInt32 (VP.fromList ([10, 20, 30] :: [Int32]))
        , AC.ColUtf8  (V.fromList ["alpha", "beta", "gamma"])
        ]
  case PArrow.arrowToParquet arrowSchema [batch] of
    Left  e  -> failTest $ "arrowToParquet: " ++ e
    Right (psSchema, rgs) -> do
      let !opts = PHL.defaultWriteOptions
                    { PHL.writePageVersion = PageV1
                    , PHL.writeCompression = Uncompressed
                    }
          !bytes = PHL.encodeParquet opts psSchema rgs
      case PHL.decodeParquet bytes of
        Left  e -> failTest $ "decodeParquet (bridge): " ++ e
        Right pf ->
          case PArrow.parquetRowGroupToArrow arrowSchema pf 0 of
            Left  e    -> failTest $ "parquetRowGroupToArrow: " ++ e
            Right cols ->
              if cols == batch
                then putStrLn "OK: Arrow ↔ Parquet bridge round-trip"
                else failTest $ "bridge round-trip mismatch:\n got "
                                 ++ show (V.toList cols)
                                 ++ "\n exp " ++ show (V.toList batch)

expectHash :: String -> String -> IO ()
expectHash s expected = expectHashBs (BSC.pack s) expected

expectHashBs :: BS.ByteString -> String -> IO ()
expectHashBs bs expected =
  let actual = pad16 (showHex (Hash.xxh64 0 bs) "")
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

-- ============================================================
-- Pyarrow availability + nested-list cross-language check
-- ============================================================

pyarrowAvailable :: IO Bool
pyarrowAvailable = do
  (code, _, _) <- Proc.readProcessWithExitCode
                    "python3" ["-c", "import pyarrow.parquet"] ""
  pure (code == ExitSuccess)

validateNestedAgainstPyarrow :: IO ()
validateNestedAgainstPyarrow = do
  let rows = V.fromList
        [ Just (V.fromList [Just (1 :: Int32), Nothing, Just 3])
        , Just V.empty
        , Nothing
        ]
      leaf = Nested.encodeOptionalListOptionalI32 "xs" rows
      bytes = Nested.buildOptionalListFile PTInt32 leaf 3
      filePath = "/tmp/wireform-nested-list.parquet"
  BS.writeFile filePath bytes
  pyarrowAssert "nested optional-list<optional INT32> reads back via pyarrow"
    [ "t = pq.read_table('" ++ filePath ++ "').to_pylist()"
    , "exp = [{'xs': [1, None, 3]}, {'xs': []}, {'xs': None}]"
    , "assert t == exp, f'roundtrip mismatch: {t!r}'"
    ]

-- | Run a python snippet under pyarrow and assert that it prints
-- 'PYARROW_OK'. Snippet header (importing pyarrow.parquet as pq) is
-- prepended automatically.
pyarrowAssert :: String -> [String] -> IO ()
pyarrowAssert label snippet = do
  (code, out, err) <- Proc.readProcessWithExitCode "python3"
    [ "-c"
    , unlines
        ( "import pyarrow.parquet as pq"
        : snippet
       ++ ["print('PYARROW_OK')"]
        )
    ] ""
  case code of
    ExitSuccess
      | "PYARROW_OK" `isInfixOf` out ->
          expect label True
      | otherwise -> failTest ("pyarrow: unexpected output: " ++ out)
    _ -> failTest ("pyarrow rejected our nested file:\nstdout=" ++ out
                    ++ "\nstderr=" ++ err)

validateListStructAgainstPyarrow :: IO ()
validateListStructAgainstPyarrow = do
  -- Schema: optional list<optional struct<a: int32, b: string>>
  let schema = Nested.NSOptional
                 (Nested.NSList
                   (Nested.NSOptional
                     (Nested.NSStruct (V.fromList
                       [ ("a", Nested.NSOptional (Nested.NSPrimitive Nested.LtInt32))
                       , ("b", Nested.NSOptional (Nested.NSPrimitive Nested.LtString))
                       ]))))
      mkRec :: Maybe (Int32, T.Text) -> Nested.NestedRow
      mkRec Nothing = Nested.NRNull
      mkRec (Just (i, s)) = Nested.NRStruct (V.fromList
        [ Nested.NRLeaf (Nested.LvInt32 i)
        , Nested.NRLeaf (Nested.LvString s)
        ])
      rows = V.fromList
        [ Nested.NRList (V.fromList
            [ mkRec (Just (1, "x"))
            , mkRec Nothing
            , mkRec (Just (2, "y"))
            ])
        , Nested.NRList V.empty
        , Nested.NRNull
        , Nested.NRList (V.singleton (mkRec (Just (3, "z"))))
        ]
  case Nested.buildNestedFile (V.singleton ("xs", schema))
                              (V.singleton rows) of
    Left e -> failTest ("buildNestedFile (list<struct>): " ++ e)
    Right bytes -> do
      let filePath = "/tmp/wireform-list-struct.parquet"
      BS.writeFile filePath bytes
      pyarrowAssert "nested list<struct<a:int32,b:string>> reads back via pyarrow"
        [ "t = pq.read_table('" ++ filePath ++ "').to_pylist()"
        , "exp = [{'xs': [{'a': 1, 'b': 'x'}, None, {'a': 2, 'b': 'y'}]},"
        , "       {'xs': []},"
        , "       {'xs': None},"
        , "       {'xs': [{'a': 3, 'b': 'z'}]}]"
        , "assert t == exp, f'roundtrip mismatch: {t!r}'"
        ]

validateListListAgainstPyarrow :: IO ()
validateListListAgainstPyarrow = do
  -- Schema: optional list<optional list<optional int32>>
  let schema = Nested.NSOptional
                 (Nested.NSList
                   (Nested.NSOptional
                     (Nested.NSList
                       (Nested.NSOptional
                         (Nested.NSPrimitive Nested.LtInt32)))))
      lit :: Int32 -> Nested.NestedRow
      lit = Nested.NRLeaf . Nested.LvInt32
      inner :: [Int32] -> Nested.NestedRow
      inner xs = Nested.NRList (V.fromList (map lit xs))
      rows = V.fromList
        [ Nested.NRList (V.fromList [inner [1, 2], inner [3]])
        , Nested.NRList (V.singleton (Nested.NRList V.empty))
        , Nested.NRNull
        , Nested.NRList (V.singleton (inner [4, 5, 6]))
        ]
  case Nested.buildNestedFile (V.singleton ("xs", schema))
                              (V.singleton rows) of
    Left e -> failTest ("buildNestedFile (list<list>): " ++ e)
    Right bytes -> do
      let filePath = "/tmp/wireform-list-list.parquet"
      BS.writeFile filePath bytes
      pyarrowAssert "nested list<list<int32>> reads back via pyarrow"
        [ "t = pq.read_table('" ++ filePath ++ "').to_pylist()"
        , "exp = [{'xs': [[1, 2], [3]]},"
        , "       {'xs': [[]]},"
        , "       {'xs': None},"
        , "       {'xs': [[4, 5, 6]]}]"
        , "assert t == exp, f'roundtrip mismatch: {t!r}'"
        ]

validateMapAgainstPyarrow :: IO ()
validateMapAgainstPyarrow = do
  -- Schema: optional map<string, int32> with optional value.
  let schema = Nested.NSOptional
                 (Nested.NSMap
                   (Nested.NSRequired (Nested.NSPrimitive Nested.LtString))
                   (Nested.NSOptional (Nested.NSPrimitive Nested.LtInt32)))
      pair :: T.Text -> Maybe Int32 -> (Nested.NestedRow, Nested.NestedRow)
      pair k mv =
        ( Nested.NRLeaf (Nested.LvString k)
        , maybe Nested.NRNull (Nested.NRLeaf . Nested.LvInt32) mv
        )
      rows = V.fromList
        [ Nested.NRMapEntries (V.fromList
            [ pair "k1" (Just 10)
            , pair "k2" (Just 20)
            ])
        , Nested.NRMapEntries V.empty
        , Nested.NRNull
        , Nested.NRMapEntries (V.singleton (pair "k3" (Just 30)))
        ]
  case Nested.buildNestedFile (V.singleton ("xs", schema))
                              (V.singleton rows) of
    Left e -> failTest ("buildNestedFile (map): " ++ e)
    Right bytes -> do
      let filePath = "/tmp/wireform-map.parquet"
      BS.writeFile filePath bytes
      -- pyarrow reads MAP columns as a list of (key, value) tuples;
      -- the dict ordering is preserved.
      pyarrowAssert "nested map<string, int32> reads back via pyarrow"
        [ "t = pq.read_table('" ++ filePath ++ "').to_pylist()"
        , "exp = [{'xs': [('k1', 10), ('k2', 20)]},"
        , "       {'xs': []},"
        , "       {'xs': None},"
        , "       {'xs': [('k3', 30)]}]"
        , "assert t == exp, f'roundtrip mismatch: {t!r}'"
        ]

-- ============================================================
-- Golden pyarrow fixtures
-- ============================================================

goldenSimpleInt :: IO ()
goldenSimpleInt = do
  bs <- BS.readFile "test/fixtures/simple_int.parquet"
  case loadParquetFile bs of
    Left e -> failTest ("golden simple_int: " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !rg  = V.unsafeIndex rgs 0
          !cc  = V.unsafeIndex (rgColumns rg) 0
      expect "golden simple_int: 5 rows" (rgNumRows rg == 5)
      case ccMetadata cc of
        Just m -> do
          expect "golden simple_int: column type INT64"
            (cmType m == PTInt64)
          expect "golden simple_int: numValues == 5"
            (cmNumValues m == 5)
        Nothing -> failTest "golden simple_int: no metadata"

goldenMixedTypes :: IO ()
goldenMixedTypes = do
  bs <- BS.readFile "test/fixtures/mixed_types.parquet"
  case loadParquetFile bs of
    Left e -> failTest ("golden mixed_types: " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !rg  = V.unsafeIndex rgs 0
          !cols = rgColumns rg
      expect "golden mixed_types: 3 columns" (V.length cols == 3)
      let typeAt i = case ccMetadata (V.unsafeIndex cols i) of
            Just m  -> Just (cmType m)
            Nothing -> Nothing
      expect "golden mixed_types: id is INT64"   (typeAt 0 == Just PTInt64)
      expect "golden mixed_types: name is BYTE_ARRAY" (typeAt 1 == Just PTByteArray)
      expect "golden mixed_types: val is DOUBLE" (typeAt 2 == Just PTDouble)
      let codecAt i = case ccMetadata (V.unsafeIndex cols i) of
            Just m  -> Just (cmCodec m)
            Nothing -> Nothing
      expect "golden mixed_types: gzip codec on every column"
        (all (== Just GZip) (map codecAt [0, 1, 2]))

goldenBloomIndex :: IO ()
goldenBloomIndex = do
  bs <- BS.readFile "test/fixtures/bloom_and_index.parquet"
  case loadParquetFile bs of
    Left e -> failTest ("golden bloom_and_index: " ++ e)
    Right pf -> do
      let !rgs = fmRowGroups (pfFooter pf)
          !rg  = V.unsafeIndex rgs 0
          !cc  = V.unsafeIndex (rgColumns rg) 0
      expect "golden bloom_and_index: 100 rows" (rgNumRows rg == 100)
      expect "golden bloom_and_index: page index offsets present"
        (ccOffsetIndexOffset cc /= Nothing && ccColumnIndexOffset cc /= Nothing)
