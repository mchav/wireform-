module Test.Arrow (arrowTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Arrow.Types
import Arrow.IPC

arrowTests :: TestTree
arrowTests = testGroup "Arrow"
  [ schemaRoundtrips
  , recordBatchRoundtrips
  , edgeCases
  , wireFormatTests
  , propertyRoundtrips
  ]

schemaRoundtrips :: TestTree
schemaRoundtrips = testGroup "Schema roundtrips"
  [ testCase "Empty schema" $ do
      let msg = SchemaMessage Schema
            { arrowFields = V.empty
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Simple int field" $ do
      let msg = SchemaMessage Schema
            { arrowFields = V.singleton Field
                { fieldName = T.pack "id"
                , fieldNullable = False
                , fieldType = AInt 32 True
                , fieldChildren = V.empty
                }
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Multiple fields" $ do
      let msg = SchemaMessage Schema
            { arrowFields = V.fromList
                [ Field (T.pack "name") True AUtf8 V.empty
                , Field (T.pack "age") False (AInt 32 True) V.empty
                , Field (T.pack "active") False ABool V.empty
                , Field (T.pack "score") True (AFloatingPoint DoublePrecision) V.empty
                ]
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Nested struct" $ do
      let childField = Field (T.pack "x") False (AInt 64 True) V.empty
          msg = SchemaMessage Schema
            { arrowFields = V.singleton Field
                { fieldName = T.pack "point"
                , fieldNullable = True
                , fieldType = AStruct
                , fieldChildren = V.singleton childField
                }
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Big endian" $ do
      let msg = SchemaMessage Schema
            { arrowFields = V.singleton Field
                { fieldName = T.pack "val"
                , fieldNullable = False
                , fieldType = AInt 16 False
                , fieldChildren = V.empty
                }
            , arrowEndianness = Big
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "All basic types" $ do
      let types = [ ANull, AInt 8 True, AInt 16 False, AInt 32 True, AInt 64 True
                  , AFloatingPoint Half, AFloatingPoint Single, AFloatingPoint DoublePrecision
                  , ABinary, AUtf8, ABool
                  , ADate DateDay, ADate DateMillisecond
                  , ATime Second 32, ATime Nanosecond 64
                  , AInterval YearMonth, AInterval DayTime
                  , AList, AStruct
                  , AFixedSizeBinary 16, AFixedSizeList 10
                  , AMap False, AMap True
                  , ADuration Millisecond
                  , ALargeBinary, ALargeUtf8, ALargeList
                  ]
          mkField (i, at) = Field (T.pack ("f" ++ show i)) False at V.empty
          msg = SchemaMessage Schema
            { arrowFields = V.fromList (map mkField (zip [(0::Int)..] types))
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Timestamp with timezone" $ do
      let msg = SchemaMessage Schema
            { arrowFields = V.singleton Field
                { fieldName = T.pack "ts"
                , fieldNullable = True
                , fieldType = ATimestamp Nanosecond (Just (T.pack "UTC"))
                , fieldChildren = V.empty
                }
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Timestamp without timezone" $ do
      let msg = SchemaMessage Schema
            { arrowFields = V.singleton Field
                { fieldName = T.pack "ts"
                , fieldNullable = False
                , fieldType = ATimestamp Microsecond Nothing
                , fieldChildren = V.empty
                }
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Union type" $ do
      let msg = SchemaMessage Schema
            { arrowFields = V.singleton Field
                { fieldName = T.pack "u"
                , fieldNullable = False
                , fieldType = AUnion Dense (V.fromList [0, 1, 2])
                , fieldChildren = V.empty
                }
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Decimal type" $ do
      let msg = SchemaMessage Schema
            { arrowFields = V.singleton Field
                { fieldName = T.pack "amount"
                , fieldNullable = True
                , fieldType = ADecimal 10 2
                , fieldChildren = V.empty
                }
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg
  ]

recordBatchRoundtrips :: TestTree
recordBatchRoundtrips = testGroup "RecordBatch roundtrips"
  [ testCase "Empty record batch" $ do
      let msg = RecordBatch RecordBatchDef
            { rbLength = 0
            , rbNodes = V.empty
            , rbBuffers = V.empty
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Record batch with nodes and buffers" $ do
      let msg = RecordBatch RecordBatchDef
            { rbLength = 1000
            , rbNodes = V.fromList
                [ FieldNode 1000 50
                , FieldNode 1000 0
                ]
            , rbBuffers = V.fromList
                [ Buffer 0 128
                , Buffer 128 8000
                , Buffer 8128 128
                , Buffer 8256 8000
                ]
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "Decode empty input" $
      case decodeIPCMessage BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"

  , testCase "DictionaryBatch roundtrip" $ do
      let msg = DictionaryBatch
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg

  , testCase "Field with empty name" $ do
      let msg = SchemaMessage Schema
            { arrowFields = V.singleton Field
                { fieldName = T.empty
                , fieldNullable = False
                , fieldType = ANull
                , fieldChildren = V.empty
                }
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) @?= Right msg
  ]

wireFormatTests :: TestTree
wireFormatTests = testGroup "Wire format"
  [ testCase "Starts with continuation 0xFFFFFFFF" $ do
      let bs = encodeIPCMessage DictionaryBatch
      BS.index bs 0 @?= 0xFF
      BS.index bs 1 @?= 0xFF
      BS.index bs 2 @?= 0xFF
      BS.index bs 3 @?= 0xFF

  , testCase "Metadata is 8-byte aligned" $ do
      let bs = encodeIPCMessage DictionaryBatch
          metaLen = fromIntegral (BS.index bs 4)
                    + fromIntegral (BS.index bs 5) * 256
                    + fromIntegral (BS.index bs 6) * 65536
                    + fromIntegral (BS.index bs 7) * 16777216 :: Int
      (metaLen `mod` 8) @?= 0
  ]

propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "Schema with random int fields" $ property $ do
      nFields <- forAll $ Gen.int (Range.linear 0 8)
      fields <- forAll $ traverse (\i -> do
          let name = T.pack ("field_" ++ show i)
          nullable <- Gen.bool
          bitWidth <- Gen.element [8, 16, 32, 64]
          signed <- Gen.bool
          pure Field
            { fieldName = name
            , fieldNullable = nullable
            , fieldType = AInt bitWidth signed
            , fieldChildren = V.empty
            }
        ) [0..nFields-1]
      let msg = SchemaMessage Schema
            { arrowFields = V.fromList fields
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) === Right msg

  , testProperty "Schema with random type selection" $ property $ do
      fieldType <- forAll $ Gen.element
        [ ANull, ABool, AUtf8, ABinary
        , AInt 32 True, AInt 64 True
        , AFloatingPoint Single, AFloatingPoint DoublePrecision
        , AStruct, AList
        ]
      name <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
      nullable <- forAll Gen.bool
      let msg = SchemaMessage Schema
            { arrowFields = V.singleton Field
                { fieldName = name
                , fieldNullable = nullable
                , fieldType = fieldType
                , fieldChildren = V.empty
                }
            , arrowEndianness = Little
            }
      decodeIPCMessage (encodeIPCMessage msg) === Right msg

  , testProperty "Schema with random endianness" $ property $ do
      endian <- forAll $ Gen.element [Little, Big]
      let msg = SchemaMessage Schema
            { arrowFields = V.empty
            , arrowEndianness = endian
            }
      decodeIPCMessage (encodeIPCMessage msg) === Right msg

  , testProperty "RecordBatch with random dimensions" $ property $ do
      len <- forAll $ Gen.int64 (Range.linear 0 100000)
      nNodes <- forAll $ Gen.int (Range.linear 0 5)
      nodes <- forAll $ traverse (\_ -> do
          nodeLen <- Gen.int64 (Range.linear 0 100000)
          nullCount <- Gen.int64 (Range.linear 0 1000)
          pure (FieldNode nodeLen nullCount)
        ) [1..nNodes]
      nBufs <- forAll $ Gen.int (Range.linear 0 8)
      bufs <- forAll $ traverse (\_ -> do
          bOff <- Gen.int64 (Range.linear 0 100000)
          bLen <- Gen.int64 (Range.linear 0 100000)
          pure (Buffer bOff bLen)
        ) [1..nBufs]
      let msg = RecordBatch RecordBatchDef
            { rbLength = len
            , rbNodes = V.fromList nodes
            , rbBuffers = V.fromList bufs
            }
      decodeIPCMessage (encodeIPCMessage msg) === Right msg
  ]
