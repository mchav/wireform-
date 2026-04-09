module Test.Parquet (parquetTests) where

import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Parquet.Levels
  ( materializePlainBoolOptional
  , materializePlainByteArrayOptional
  , materializePlainInt32Optional
  , materializePlainInt64Optional
  , maxLevelsForColumnPath
  , parseDataPageV1Levels
  )
import Parquet.Page
import Parquet.Types
import Parquet.Footer
import Parquet.Read
import Parquet.Write
import Thrift.Encode (encodeCompact)
import qualified Thrift.Value as TV

parquetTests :: TestTree
parquetTests = testGroup "Parquet"
  [ footerRoundtrips
  , plainDecoderTests
  , hybridRleDecoderTests
  , levelsAndSchemaTests
  , magicTests
  , edgeCases
  , propertyRoundtrips
  , dictionaryOptionalTests
  , deltaBinaryPackedTests
  , dataPageV2Tests
  , writerRoundtripTests
  ]

plainDecoderTests :: TestTree
plainDecoderTests = testGroup "PLAIN column decoders"
  [ testCase "INT32 little-endian" $ do
      let bs = BS.pack [0x07, 0x00, 0x00, 0x00, 0xFE, 0xFF, 0xFF, 0xFF]
      decodePlainInt32 2 bs @?= Right (VP.fromList [(7 :: Int32), (-2 :: Int32)])
  , testCase "INT64 little-endian" $ do
      let bs =
            BS.pack
              [ 0x2A, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
              , 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
              ]
      decodePlainInt64 2 bs @?= Right (VP.fromList [(42 :: Int64), (-1 :: Int64)])
  , testCase "DOUBLE little-endian" $ do
      let bs = BS.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
      decodePlainDouble 1 bs @?= Right (VP.fromList [0.0])
  , testCase "BYTE_ARRAY length-prefixed" $ do
      let bs = BS.pack [0x03, 0x00, 0x00, 0x00, 0x61, 0x62, 0x63, 0x01, 0x00, 0x00, 0x00, 0x7A]
      decodePlainByteArray 2 bs
        @?= Right (V.fromList [BS.pack [0x61, 0x62, 0x63], BS.pack [0x7A]])
  ]

hybridRleDecoderTests :: TestTree
hybridRleDecoderTests = testGroup "Hybrid RLE"
  [ testGroup "dictionary indices (width byte + body)"
      [ testCase "RLE run of zeros (width 2)" $ do
          let bs = BS.pack [2, 0x06, 0x00]
          decodeDictionaryIndices 3 bs
            @?= Right (VP.fromList [(0 :: Int32), 0, 0])
      , testCase "Bit-packed 0..7 (width 3, Apache example bytes)" $ do
          let bs = BS.pack [3, 0x03, 0x88, 0xC6, 0xFA]
          decodeDictionaryIndices 8 bs
            @?= Right (VP.fromList (map (fromIntegral @Int @Int32) [0 .. 7]))
      ]
  , testGroup "length-prefixed body (data page v1 levels)"
      [ testCase "same RLE run as dictionary case (inner = tail of dict layout)" $ do
          let inner = BS.pack [0x06, 0x00]
              bs = BS.append (BS.pack [0x02, 0x00, 0x00, 0x00]) inner
          decodeHybridRleLengthPrefixed 2 3 bs
            @?= Right (VP.fromList [(0 :: Int32), 0, 0])
      , testCase "same bit-packed run as dictionary case" $ do
          let inner = BS.pack [0x03, 0x88, 0xC6, 0xFA]
              bs = BS.append (BS.pack [0x04, 0x00, 0x00, 0x00]) inner
          decodeHybridRleLengthPrefixed 3 8 bs
            @?= Right (VP.fromList (map (fromIntegral @Int @Int32) [0 .. 7]))
      , testCase "reject declared length past buffer" $ do
          let bs = BS.pack [0xFF, 0x00, 0x00, 0x00, 0x00]
          case decodeHybridRleLengthPrefixed 1 1 bs of
            Left _ -> pure ()
            Right v -> assertFailure ("expected Left, got " ++ show v)
      ]
  ]

levelsAndSchemaTests :: TestTree
levelsAndSchemaTests = testGroup "Levels + schema max levels"
  [ testCase "maxLevels optional and required leaves" $ do
      let schOpt =
            V.fromList
              [ SchemaElement (T.pack "schema") Nothing Nothing (Just 1) Nothing Nothing
              , SchemaElement (T.pack "x") (Just Optional) (Just PTInt32) Nothing Nothing Nothing
              ]
          schReq =
            V.fromList
              [ SchemaElement (T.pack "schema") Nothing Nothing (Just 1) Nothing Nothing
              , SchemaElement (T.pack "y") (Just Required) (Just PTInt32) Nothing Nothing Nothing
              ]
      maxLevelsForColumnPath schOpt (V.singleton (T.pack "x")) @?= Right (0, 1)
      maxLevelsForColumnPath schReq (V.singleton (T.pack "y")) @?= Right (0, 0)
  , testCase "materializePlainInt32Optional with null middle" $ do
      let defs = VP.fromList [(1 :: Int32), 0, 1]
          plain = BS.pack [0x0A, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00]
      materializePlainInt32Optional defs 1 plain
        @?= Right (V.fromList [Just 10, Nothing, Just 3])
  , testCase "parseDataPageV1Levels + materialize (all present)" $ do
      let defSec =
            BS.append
              (BS.pack [0x02, 0x00, 0x00, 0x00])
              (BS.pack [0x06, 0x01])
          plain = BS.pack [0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00]
          raw = BS.append defSec plain
      case parseDataPageV1Levels 0 1 3 raw of
        Left e -> assertFailure e
        Right (rep, def, rest) -> do
          rep @?= VP.replicate 3 (0 :: Int32)
          def @?= VP.replicate 3 (1 :: Int32)
          rest @?= plain
          materializePlainInt32Optional def 1 rest
            @?= Right (V.fromList [Just 1, Just 2, Just 3])
  , testCase "materializePlainInt64Optional with null" $ do
      let defs = VP.fromList [(1 :: Int32), 0, 1]
          plain =
            BS.append
              (BS.pack [0xE8, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
              (BS.pack [0xD0, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
      materializePlainInt64Optional defs 1 plain
        @?= Right (V.fromList [Just 1000, Nothing, Just 2000])
  , testCase "materializePlainBoolOptional two defined" $ do
      let defs = VP.fromList [(1 :: Int32), 1, 0]
          plain = BS.pack [0x03]
      materializePlainBoolOptional defs 1 plain
        @?= Right (V.fromList [Just True, Just True, Nothing])
  , testCase "materializePlainByteArrayOptional null second row" $ do
      let defs = VP.fromList [(1 :: Int32), 0]
          plain = BS.pack [0x02, 0x00, 0x00, 0x00, 0x61, 0x62]
      materializePlainByteArrayOptional defs 1 plain
        @?= Right (V.fromList [Just (BS.pack [0x61, 0x62]), Nothing])
  ]

footerRoundtrips :: TestTree
footerRoundtrips = testGroup "Footer roundtrips"
  [ testCase "Minimal file metadata" $ do
      let fm = FileMetadata
            { fmVersion = 2
            , fmSchema = V.fromList
                [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
                , SchemaElement "value" (Just Required) (Just PTInt32) Nothing Nothing Nothing
                ]
            , fmNumRows = 100
            , fmRowGroups = V.empty
            , fmCreatedBy = Just "wireform"
            }
      readFooter (writeFooter fm) @?= Right fm
  , testCase "File metadata with row group" $ do
      let cm = ColumnMetadata PTInt64 (V.fromList [Plain, RLE])
                 (V.fromList ["value"]) Snappy 1000 8000 4000 4 Nothing
          cc = ColumnChunk Nothing 4 (Just cm)
          rg = RowGroup (V.singleton cc) 4000 1000
          fm = FileMetadata 2
                 (V.fromList
                   [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
                   , SchemaElement "value" (Just Optional) (Just PTInt64) Nothing Nothing Nothing
                   ])
                 1000 (V.singleton rg) Nothing
      readFooter (writeFooter fm) @?= Right fm
  , testCase "Multiple row groups" $ do
      let mkRG n = RowGroup V.empty (n * 1000) n
          fm = FileMetadata 1
                 (V.singleton (SchemaElement "root" Nothing Nothing (Just 0) Nothing Nothing))
                 3000 (V.fromList [mkRG 1000, mkRG 1000, mkRG 1000]) (Just "test-writer v1.0")
      readFooter (writeFooter fm) @?= Right fm
  , testCase "All parquet types" $ do
      let types = [PTBoolean, PTInt32, PTInt64, PTInt96, PTFloat, PTDouble, PTByteArray, PTFixedLenByteArray]
          mkSchema t = SchemaElement (T.pack (show t)) (Just Required) (Just t) Nothing Nothing Nothing
          fm = FileMetadata 2 (V.fromList (map mkSchema types)) 0 V.empty Nothing
      readFooter (writeFooter fm) @?= Right fm
  ]

magicTests :: TestTree
magicTests = testGroup "Magic"
  [ testCase "Footer ends with PAR1" $ do
      let fm = FileMetadata 2 V.empty 0 V.empty Nothing
          bs = writeFooter fm
          magic = BS.drop (BS.length bs - 4) bs
      magic @?= parquetMagic
  , testCase "PAR1 magic bytes" $
      BS.unpack parquetMagic @?= [0x50, 0x41, 0x52, 0x31]
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "Empty input fails" $
      case readFooter BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"
  , testCase "Too short input fails" $
      case readFooter (BS.pack [1,2,3]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on short input"
  , testCase "Wrong magic fails" $
      case readFooter (BS.pack [0,0,0,0, 0,0,0,0]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on wrong magic"
  , testCase "All encodings round-trip" $ do
      let encs = [Plain, PlainDictionary, RLE, BitPacked, DeltaBinaryPacked,
                  DeltaLengthByteArray, DeltaByteArray, RLEDictionary, ByteStreamSplit]
          cm = ColumnMetadata PTInt32 (V.fromList encs) (V.singleton "x") Uncompressed 0 0 0 0 Nothing
          cc = ColumnChunk Nothing 0 (Just cm)
          rg = RowGroup (V.singleton cc) 0 0
          fm = FileMetadata 2 V.empty 0 (V.singleton rg) Nothing
      readFooter (writeFooter fm) @?= Right fm
  ]

propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "FileMetadata with random version and numRows" $ property $ do
      ver <- forAll $ Gen.int32 (Range.linear 1 3)
      nRows <- forAll $ Gen.int64 (Range.linear 0 1000000)
      createdBy <- forAll $ Gen.maybe (Gen.text (Range.linear 1 32) Gen.alphaNum)
      let fm = FileMetadata ver V.empty nRows V.empty createdBy
      readFooter (writeFooter fm) === Right fm
  , testProperty "FileMetadata with random schema elements" $ property $ do
      nFields <- forAll $ Gen.int (Range.linear 0 5)
      fields <- forAll $ traverse (\_ -> do
          name <- Gen.text (Range.linear 1 20) Gen.alphaNum
          rep <- Gen.maybe (Gen.element [Required, Optional, Repeated])
          pt <- Gen.maybe (Gen.element [PTBoolean, PTInt32, PTInt64, PTFloat, PTDouble, PTByteArray])
          pure (SchemaElement name rep pt Nothing Nothing Nothing)
        ) [1..nFields]
      let fm = FileMetadata 2 (V.fromList fields) 0 V.empty Nothing
      readFooter (writeFooter fm) === Right fm
  , testProperty "FileMetadata with random row groups" $ property $ do
      nGroups <- forAll $ Gen.int (Range.linear 0 3)
      rgs <- forAll $ traverse (\_ -> do
          nRows <- Gen.int64 (Range.linear 0 100000)
          totalBytes <- Gen.int64 (Range.linear 0 1000000)
          pure (RowGroup V.empty totalBytes nRows)
        ) [1..nGroups]
      let fm = FileMetadata 2 V.empty (sum (map rgNumRows rgs)) (V.fromList rgs) Nothing
      readFooter (writeFooter fm) === Right fm
  ]

dictionaryOptionalTests :: TestTree
dictionaryOptionalTests = testGroup "Dictionary optional columns"
  [ testCase "dict page + data page with null" $ do
      let dictBody = BS.pack
            [ 0x0A, 0x00, 0x00, 0x00
            , 0x14, 0x00, 0x00, 0x00
            , 0x1E, 0x00, 0x00, 0x00
            ]
          dictHdr = encodePageHeader PageHeader
            { phType = pageTypeDictionaryPage
            , phUncompressedPageSize = Just 12
            , phCompressedPageSize = Just 12
            , phDataPage = Nothing
            , phDictionaryPage = Just (DictionaryPageHeader 3 0)
            , phDataPageV2 = Nothing
            }
          defLevels = BS.pack [0x02, 0x00, 0x00, 0x00, 0x03, 0x05]
          dictIndices = BS.pack [0x02, 0x03, 0x08, 0x00]
          dataBody = defLevels <> dictIndices
          dataHdr = encodePageHeader PageHeader
            { phType = pageTypeDataPage
            , phUncompressedPageSize = Just (fromIntegral (BS.length dataBody))
            , phCompressedPageSize = Just (fromIntegral (BS.length dataBody))
            , phDataPage = Just (DataPageHeader 3 2)
            , phDictionaryPage = Nothing
            , phDataPageV2 = Nothing
            }
          chunk = dictHdr <> dictBody <> dataHdr <> dataBody
          lookupInt32 v idx =
            let i = fromIntegral idx :: Int
            in if i >= 0 && i < VP.length v then Just (VP.unsafeIndex v i) else Nothing
      case readDictionaryOptionalColumnChunk decodePlainInt32 lookupInt32
             Uncompressed 0 1 chunk of
        Left e -> assertFailure e
        Right result -> result @?= V.fromList [Just (10 :: Int32), Nothing, Just 30]
  , testCase "all-null optional dictionary column" $ do
      let dictBody = BS.pack [0x07, 0x00, 0x00, 0x00]
          dictHdr = encodePageHeader PageHeader
            { phType = pageTypeDictionaryPage
            , phUncompressedPageSize = Just 4
            , phCompressedPageSize = Just 4
            , phDataPage = Nothing
            , phDictionaryPage = Just (DictionaryPageHeader 1 0)
            , phDataPageV2 = Nothing
            }
          defLevels = BS.pack [0x02, 0x00, 0x00, 0x00, 0x03, 0x00]
          dictIndices = BS.pack [0x00]
          dataBody = defLevels <> dictIndices
          dataHdr = encodePageHeader PageHeader
            { phType = pageTypeDataPage
            , phUncompressedPageSize = Just (fromIntegral (BS.length dataBody))
            , phCompressedPageSize = Just (fromIntegral (BS.length dataBody))
            , phDataPage = Just (DataPageHeader 2 2)
            , phDictionaryPage = Nothing
            , phDataPageV2 = Nothing
            }
          chunk = dictHdr <> dictBody <> dataHdr <> dataBody
          lookupInt32 v idx =
            let i = fromIntegral idx :: Int
            in if i >= 0 && i < VP.length v then Just (VP.unsafeIndex v i) else Nothing
      case readDictionaryOptionalColumnChunk decodePlainInt32 lookupInt32
             Uncompressed 0 1 chunk of
        Left e -> assertFailure e
        Right result -> result @?= V.fromList [Nothing, Nothing :: Maybe Int32]
  ]

deltaBinaryPackedTests :: TestTree
deltaBinaryPackedTests = testGroup "DELTA_BINARY_PACKED"
  [ testCase "single value (header only)" $ do
      let bs = BS.pack [0x08, 0x01, 0x01, 0x54]
      decodeDeltaBinaryPackedInt32 1 bs @?= Right (VP.fromList [42 :: Int32])
  , testCase "constant delta (bit_width=0)" $ do
      let bs = BS.pack [0x08, 0x01, 0x03, 0xC8, 0x01, 0x14, 0x00]
      decodeDeltaBinaryPackedInt32 3 bs @?= Right (VP.fromList [100, 110, 120 :: Int32])
  , testCase "variable deltas (bit_width=2)" $ do
      let bs = BS.pack [0x08, 0x01, 0x04, 0x00, 0x02, 0x02, 0x24, 0x00]
      decodeDeltaBinaryPackedInt32 4 bs @?= Right (VP.fromList [0, 1, 3, 6 :: Int32])
  , testCase "INT64 variant" $ do
      let bs = BS.pack [0x08, 0x01, 0x01, 0x54]
      decodeDeltaBinaryPackedInt64 1 bs @?= Right (VP.fromList [42 :: Int64])
  , testCase "negative first value" $ do
      let bs = BS.pack [0x08, 0x01, 0x01, 0x09]
      decodeDeltaBinaryPackedInt32 1 bs @?= Right (VP.fromList [-5 :: Int32])
  , testCase "negative deltas" $ do
      let bs = BS.pack [0x08, 0x01, 0x03, 0x14, 0x05, 0x00]
      decodeDeltaBinaryPackedInt32 3 bs @?= Right (VP.fromList [10, 7, 4 :: Int32])
  , testCase "empty (total_values=0)" $ do
      let bs = BS.pack [0x08, 0x01, 0x00, 0x00]
      decodeDeltaBinaryPackedInt32 0 bs @?= Right VP.empty
  ]

dataPageV2Tests :: TestTree
dataPageV2Tests = testGroup "DATA_PAGE_V2"
  [ testCase "header parse from Thrift bytes" $ do
      let v2Struct = TV.Struct $ V.fromList
            [ (1, TV.I32 1000), (2, TV.I32 100), (3, TV.I32 1000)
            , (4, TV.I32 0), (5, TV.I32 50), (6, TV.I32 25)
            , (7, TV.Bool True)
            ]
          pageHdrStruct = TV.Struct $ V.fromList
            [ (1, TV.I32 3), (2, TV.I32 200), (3, TV.I32 150), (8, v2Struct) ]
          bs = encodeCompact pageHdrStruct
      case readPageHeaderAt bs 0 of
        Left e -> assertFailure e
        Right (hdr, _) -> do
          phType hdr @?= pageTypeDataPageV2
          case phDataPageV2 hdr of
            Nothing -> assertFailure "expected DataPageHeaderV2"
            Just v2 -> do
              dph2NumValues v2 @?= 1000
              dph2NumNulls v2 @?= 100
              dph2NumRows v2 @?= 1000
              dph2Encoding v2 @?= 0
              dph2DefLevelsLen v2 @?= 50
              dph2RepLevelsLen v2 @?= 25
              dph2IsCompressed v2 @?= True
  , testCase "is_compressed defaults to True when absent" $ do
      let v2Struct = TV.Struct $ V.fromList
            [ (1, TV.I32 5), (2, TV.I32 0), (3, TV.I32 5)
            , (4, TV.I32 0), (5, TV.I32 10), (6, TV.I32 0) ]
          bs = encodeCompact $ TV.Struct $ V.fromList
            [ (1, TV.I32 3), (2, TV.I32 40), (3, TV.I32 40), (8, v2Struct) ]
      case readPageHeaderAt bs 0 of
        Left e -> assertFailure e
        Right (hdr, _) -> case phDataPageV2 hdr of
          Nothing -> assertFailure "expected DataPageHeaderV2"
          Just v2 -> dph2IsCompressed v2 @?= True
  , testCase "page header round-trip through encode/parse" $ do
      let hdr = PageHeader pageTypeDataPageV2 (Just 500) (Just 400) Nothing Nothing
                  (Just (DataPageHeaderV2 200 50 200 0 30 20 False))
          bs = encodePageHeader hdr
      case readPageHeaderAt bs 0 of
        Left e -> assertFailure e
        Right (hdr', _) -> do
          phType hdr' @?= pageTypeDataPageV2
          case phDataPageV2 hdr' of
            Nothing -> assertFailure "round-trip lost DataPageHeaderV2"
            Just v2 -> do
              dph2NumValues v2 @?= 200
              dph2NumNulls v2 @?= 50
              dph2IsCompressed v2 @?= False
  ]

writerRoundtripTests :: TestTree
writerRoundtripTests = testGroup "Writer round-trips"
  [ testCase "buildParquetFile -> loadParquetFile -> readPlainInt32ColumnChunk" $ do
      let schema = V.fromList
            [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
            , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing
            ]
          vals = VP.fromList [1, 2, 3, 4, 5 :: Int32]
          fileBytes = buildParquetFile schema (V.singleton (V.singleton vals))
      case loadParquetFile fileBytes of
        Left e -> assertFailure e
        Right pf -> case columnChunkSlice pf 0 0 of
          Left e -> assertFailure e
          Right chunkData ->
            readPlainInt32ColumnChunk Uncompressed chunkData @?= Right vals
  , testCase "multiple columns round-trip" $ do
      let schema = V.fromList
            [ SchemaElement "schema" Nothing Nothing (Just 2) Nothing Nothing
            , SchemaElement "a" (Just Required) (Just PTInt32) Nothing Nothing Nothing
            , SchemaElement "b" (Just Required) (Just PTInt32) Nothing Nothing Nothing
            ]
          colA = VP.fromList [10, 20, 30 :: Int32]
          colB = VP.fromList [100, 200, 300 :: Int32]
          fileBytes = buildParquetFile schema (V.singleton (V.fromList [colA, colB]))
      case loadParquetFile fileBytes of
        Left e -> assertFailure e
        Right pf -> do
          rA <- either assertFailure pure (columnChunkSlice pf 0 0)
          rB <- either assertFailure pure (columnChunkSlice pf 0 1)
          readPlainInt32ColumnChunk Uncompressed rA @?= Right colA
          readPlainInt32ColumnChunk Uncompressed rB @?= Right colB
  , testCase "empty column round-trip" $ do
      let schema = V.fromList
            [ SchemaElement "schema" Nothing Nothing (Just 1) Nothing Nothing
            , SchemaElement "x" (Just Required) (Just PTInt32) Nothing Nothing Nothing
            ]
          vals = VP.empty :: VP.Vector Int32
          fileBytes = buildParquetFile schema (V.singleton (V.singleton vals))
      case loadParquetFile fileBytes of
        Left e -> assertFailure e
        Right pf -> case columnChunkSlice pf 0 0 of
          Left e -> assertFailure e
          Right chunkData ->
            readPlainInt32ColumnChunk Uncompressed chunkData @?= Right vals
  , testCase "page header encode/decode round-trip" $ do
      let hdr = PageHeader pageTypeDataPage (Just 100) (Just 100)
                  (Just (DataPageHeader 25 0)) Nothing Nothing
          bs = encodePageHeader hdr
      case readPageHeaderAt bs 0 of
        Left e -> assertFailure e
        Right (hdr', _) -> do
          phType hdr' @?= pageTypeDataPage
          phCompressedPageSize hdr' @?= Just 100
          case phDataPage hdr' of
            Nothing -> assertFailure "expected DataPageHeader"
            Just dph -> do
              dphNumValues dph @?= 25
              dphEncoding dph @?= 0
  ]
