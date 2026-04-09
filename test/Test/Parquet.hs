module Test.Parquet (parquetTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Parquet.Types
import Parquet.Footer

parquetTests :: TestTree
parquetTests = testGroup "Parquet"
  [ footerRoundtrips
  , magicTests
  , edgeCases
  , propertyRoundtrips
  ]

footerRoundtrips :: TestTree
footerRoundtrips = testGroup "Footer roundtrips"
  [ testCase "Minimal file metadata" $ do
      let fm = FileMetadata
            { fmVersion = 2
            , fmSchema = V.fromList
                [ SchemaElement
                    { seName = T.pack "schema"
                    , seRepetition = Nothing
                    , seType = Nothing
                    , seNumChildren = Just 1
                    , seConvertedType = Nothing
                    , seLogicalType = Nothing
                    }
                , SchemaElement
                    { seName = T.pack "value"
                    , seRepetition = Just Required
                    , seType = Just PTInt32
                    , seNumChildren = Nothing
                    , seConvertedType = Nothing
                    , seLogicalType = Nothing
                    }
                ]
            , fmNumRows = 100
            , fmRowGroups = V.empty
            , fmCreatedBy = Just (T.pack "wireform")
            }
      readFooter (writeFooter fm) @?= Right fm

  , testCase "File metadata with row group" $ do
      let cm = ColumnMetadata
            { cmType = PTInt64
            , cmEncodings = V.fromList [Plain, RLE]
            , cmPathInSchema = V.fromList [T.pack "value"]
            , cmCodec = Snappy
            , cmNumValues = 1000
            , cmTotalUncompressedSize = 8000
            , cmTotalCompressedSize = 4000
            , cmDataPageOffset = 4
            }
          cc = ColumnChunk
            { ccFilePath = Nothing
            , ccFileOffset = 4
            , ccMetadata = Just cm
            }
          rg = RowGroup
            { rgColumns = V.singleton cc
            , rgTotalByteSize = 4000
            , rgNumRows = 1000
            }
          fm = FileMetadata
            { fmVersion = 2
            , fmSchema = V.fromList
                [ SchemaElement
                    { seName = T.pack "schema"
                    , seRepetition = Nothing
                    , seType = Nothing
                    , seNumChildren = Just 1
                    , seConvertedType = Nothing
                    , seLogicalType = Nothing
                    }
                , SchemaElement
                    { seName = T.pack "value"
                    , seRepetition = Just Optional
                    , seType = Just PTInt64
                    , seNumChildren = Nothing
                    , seConvertedType = Nothing
                    , seLogicalType = Nothing
                    }
                ]
            , fmNumRows = 1000
            , fmRowGroups = V.singleton rg
            , fmCreatedBy = Nothing
            }
      readFooter (writeFooter fm) @?= Right fm

  , testCase "Multiple row groups" $ do
      let mkRG n = RowGroup
            { rgColumns = V.empty
            , rgTotalByteSize = n * 1000
            , rgNumRows = n
            }
          fm = FileMetadata
            { fmVersion = 1
            , fmSchema = V.singleton SchemaElement
                { seName = T.pack "root"
                , seRepetition = Nothing
                , seType = Nothing
                , seNumChildren = Just 0
                , seConvertedType = Nothing
                , seLogicalType = Nothing
                }
            , fmNumRows = 3000
            , fmRowGroups = V.fromList [mkRG 1000, mkRG 1000, mkRG 1000]
            , fmCreatedBy = Just (T.pack "test-writer v1.0")
            }
      readFooter (writeFooter fm) @?= Right fm

  , testCase "All compression types" $ do
      let mkCol codec = ColumnChunk
            { ccFilePath = Nothing
            , ccFileOffset = 0
            , ccMetadata = Just ColumnMetadata
                { cmType = PTBoolean
                , cmEncodings = V.singleton Plain
                , cmPathInSchema = V.singleton (T.pack "flag")
                , cmCodec = codec
                , cmNumValues = 10
                , cmTotalUncompressedSize = 10
                , cmTotalCompressedSize = 10
                , cmDataPageOffset = 0
                }
            }
          rg = RowGroup
            { rgColumns = V.fromList (map mkCol [Uncompressed, Snappy, GZip, ZSTD])
            , rgTotalByteSize = 40
            , rgNumRows = 10
            }
          fm = FileMetadata
            { fmVersion = 2
            , fmSchema = V.singleton SchemaElement
                { seName = T.pack "schema"
                , seRepetition = Nothing
                , seType = Nothing
                , seNumChildren = Just 0
                , seConvertedType = Nothing
                , seLogicalType = Nothing
                }
            , fmNumRows = 10
            , fmRowGroups = V.singleton rg
            , fmCreatedBy = Nothing
            }
      readFooter (writeFooter fm) @?= Right fm

  , testCase "All parquet types" $ do
      let types = [PTBoolean, PTInt32, PTInt64, PTInt96, PTFloat, PTDouble, PTByteArray, PTFixedLenByteArray]
          mkSchema t = SchemaElement
            { seName = T.pack (show t)
            , seRepetition = Just Required
            , seType = Just t
            , seNumChildren = Nothing
            , seConvertedType = Nothing
            , seLogicalType = Nothing
            }
          fm = FileMetadata
            { fmVersion = 2
            , fmSchema = V.fromList (map mkSchema types)
            , fmNumRows = 0
            , fmRowGroups = V.empty
            , fmCreatedBy = Nothing
            }
      readFooter (writeFooter fm) @?= Right fm
  ]

magicTests :: TestTree
magicTests = testGroup "Magic"
  [ testCase "Footer ends with PAR1" $ do
      let fm = FileMetadata
            { fmVersion = 2
            , fmSchema = V.empty
            , fmNumRows = 0
            , fmRowGroups = V.empty
            , fmCreatedBy = Nothing
            }
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
          cm = ColumnMetadata
            { cmType = PTInt32
            , cmEncodings = V.fromList encs
            , cmPathInSchema = V.singleton (T.pack "x")
            , cmCodec = Uncompressed
            , cmNumValues = 0
            , cmTotalUncompressedSize = 0
            , cmTotalCompressedSize = 0
            , cmDataPageOffset = 0
            }
          cc = ColumnChunk { ccFilePath = Nothing, ccFileOffset = 0, ccMetadata = Just cm }
          rg = RowGroup { rgColumns = V.singleton cc, rgTotalByteSize = 0, rgNumRows = 0 }
          fm = FileMetadata
            { fmVersion = 2
            , fmSchema = V.empty
            , fmNumRows = 0
            , fmRowGroups = V.singleton rg
            , fmCreatedBy = Nothing
            }
      readFooter (writeFooter fm) @?= Right fm
  ]

propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "FileMetadata with random version and numRows" $ property $ do
      ver <- forAll $ Gen.int32 (Range.linear 1 3)
      nRows <- forAll $ Gen.int64 (Range.linear 0 1000000)
      createdBy <- forAll $ Gen.maybe (Gen.text (Range.linear 1 32) Gen.alphaNum)
      let fm = FileMetadata
            { fmVersion = ver
            , fmSchema = V.empty
            , fmNumRows = nRows
            , fmRowGroups = V.empty
            , fmCreatedBy = createdBy
            }
      readFooter (writeFooter fm) === Right fm

  , testProperty "FileMetadata with random schema elements" $ property $ do
      nFields <- forAll $ Gen.int (Range.linear 0 5)
      fields <- forAll $ traverse (\_ -> do
          name <- Gen.text (Range.linear 1 20) Gen.alphaNum
          rep <- Gen.maybe (Gen.element [Required, Optional, Repeated])
          pt <- Gen.maybe (Gen.element [PTBoolean, PTInt32, PTInt64, PTFloat, PTDouble, PTByteArray])
          pure SchemaElement
            { seName = name
            , seRepetition = rep
            , seType = pt
            , seNumChildren = Nothing
            , seConvertedType = Nothing
            , seLogicalType = Nothing
            }
        ) [1..nFields]
      let fm = FileMetadata
            { fmVersion = 2
            , fmSchema = V.fromList fields
            , fmNumRows = 0
            , fmRowGroups = V.empty
            , fmCreatedBy = Nothing
            }
      readFooter (writeFooter fm) === Right fm

  , testProperty "FileMetadata with random row groups" $ property $ do
      nGroups <- forAll $ Gen.int (Range.linear 0 3)
      rgs <- forAll $ traverse (\_ -> do
          nRows <- Gen.int64 (Range.linear 0 100000)
          totalBytes <- Gen.int64 (Range.linear 0 1000000)
          pure RowGroup
            { rgColumns = V.empty
            , rgTotalByteSize = totalBytes
            , rgNumRows = nRows
            }
        ) [1..nGroups]
      let fm = FileMetadata
            { fmVersion = 2
            , fmSchema = V.empty
            , fmNumRows = sum (map rgNumRows rgs)
            , fmRowGroups = V.fromList rgs
            , fmCreatedBy = Nothing
            }
      readFooter (writeFooter fm) === Right fm
  ]
