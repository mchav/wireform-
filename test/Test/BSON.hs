module Test.BSON (bsonTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified BSON.Value as B
import BSON.Encode (encode)
import BSON.Decode (decode)

bsonTests :: TestTree
bsonTests = testGroup "BSON"
  [ unitTests
  , propertyRoundtrips
  , edgeCases
  , wireFormatTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit roundtrips"
  [ testCase "Null" $ do
      let val = B.Document (V.singleton (T.pack "x", B.Null))
      decode (encode val) @?= Right val

  , testCase "Bool true" $ do
      let val = B.Document (V.singleton (T.pack "b", B.Bool True))
      decode (encode val) @?= Right val

  , testCase "Bool false" $ do
      let val = B.Document (V.singleton (T.pack "b", B.Bool False))
      decode (encode val) @?= Right val

  , testCase "Int32" $ do
      let val = B.Document (V.singleton (T.pack "n", B.Int32 42))
      decode (encode val) @?= Right val

  , testCase "Int64" $ do
      let val = B.Document (V.singleton (T.pack "n", B.Int64 (-999999999999)))
      decode (encode val) @?= Right val

  , testCase "Double" $ do
      let val = B.Document (V.singleton (T.pack "d", B.Double 3.14))
      decode (encode val) @?= Right val

  , testCase "String" $ do
      let val = B.Document (V.singleton (T.pack "s", B.String (T.pack "hello world")))
      decode (encode val) @?= Right val

  , testCase "Binary" $ do
      let val = B.Document (V.singleton (T.pack "b", B.Binary (BS.pack [1,2,3,4])))
      decode (encode val) @?= Right val

  , testCase "DateTime" $ do
      let val = B.Document (V.singleton (T.pack "dt", B.DateTime 1609459200000))
      decode (encode val) @?= Right val

  , testCase "ObjectId" $ do
      let oid = BS.pack [0x50,0x7f,0x1f,0x77,0xbc,0xf8,0x6c,0xd7,0x99,0x43,0x90,0x11]
          val = B.Document (V.singleton (T.pack "id", B.ObjectId oid))
      decode (encode val) @?= Right val

  , testCase "Regex" $ do
      let val = B.Document (V.singleton (T.pack "r", B.Regex (T.pack "abc.*") (T.pack "i")))
      decode (encode val) @?= Right val

  , testCase "Nested document" $ do
      let inner = B.Document (V.singleton (T.pack "x", B.Int32 1))
          val = B.Document (V.singleton (T.pack "nested", inner))
      decode (encode val) @?= Right val

  , testCase "Array" $ do
      let val = B.Document (V.singleton (T.pack "arr",
                  B.Array (V.fromList [B.Int32 1, B.Int32 2, B.Int32 3])))
      decode (encode val) @?= Right val
  ]

propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "Int32 roundtrip" $ property $ do
      n <- forAll $ Gen.int32 Range.linearBounded
      let val = B.Document (V.singleton (T.pack "v", B.Int32 n))
      decode (encode val) === Right val

  , testProperty "Int64 roundtrip" $ property $ do
      n <- forAll $ Gen.int64 Range.linearBounded
      let val = B.Document (V.singleton (T.pack "v", B.Int64 n))
      decode (encode val) === Right val

  , testProperty "Double roundtrip" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
      let val = B.Document (V.singleton (T.pack "v", B.Double d))
      decode (encode val) === Right val

  , testProperty "String roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 128) Gen.alphaNum
      let val = B.Document (V.singleton (T.pack "v", B.String t))
      decode (encode val) === Right val

  , testProperty "Binary roundtrip" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 256)
      let val = B.Document (V.singleton (T.pack "v", B.Binary bs))
      decode (encode val) === Right val

  , testProperty "Bool roundtrip" $ property $ do
      b <- forAll Gen.bool
      let val = B.Document (V.singleton (T.pack "v", B.Bool b))
      decode (encode val) === Right val

  , testProperty "DateTime roundtrip" $ property $ do
      ms <- forAll $ Gen.int64 Range.linearBounded
      let val = B.Document (V.singleton (T.pack "v", B.DateTime ms))
      decode (encode val) === Right val

  , testProperty "Nested document roundtrip" $ property $ do
      n <- forAll $ Gen.int32 Range.linearBounded
      t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
      b <- forAll Gen.bool
      let inner = B.Document (V.fromList
            [ (T.pack "n", B.Int32 n)
            , (T.pack "s", B.String t)
            ])
          val = B.Document (V.fromList
            [ (T.pack "inner", inner)
            , (T.pack "flag", B.Bool b)
            , (T.pack "top", B.Null)
            ])
      decode (encode val) === Right val

  , testProperty "Array roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 10) (Gen.int32 Range.linearBounded)
      let arr = B.Array (V.fromList (map B.Int32 ns))
          val = B.Document (V.singleton (T.pack "a", arr))
      decode (encode val) === Right val

  , testProperty "Multiple fields roundtrip" $ property $ do
      n <- forAll $ Gen.int32 Range.linearBounded
      m <- forAll $ Gen.int64 Range.linearBounded
      t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
      b <- forAll Gen.bool
      let val = B.Document (V.fromList
            [ (T.pack "i32", B.Int32 n)
            , (T.pack "i64", B.Int64 m)
            , (T.pack "str", B.String t)
            , (T.pack "bool", B.Bool b)
            , (T.pack "null", B.Null)
            ])
      decode (encode val) === Right val
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "Empty document" $ do
      let val = B.Document V.empty
      decode (encode val) @?= Right val

  , testCase "Empty string" $ do
      let val = B.Document (V.singleton (T.pack "s", B.String T.empty))
      decode (encode val) @?= Right val

  , testCase "Empty binary" $ do
      let val = B.Document (V.singleton (T.pack "b", B.Binary BS.empty))
      decode (encode val) @?= Right val

  , testCase "Empty array" $ do
      let val = B.Document (V.singleton (T.pack "a", B.Array V.empty))
      decode (encode val) @?= Right val

  , testCase "Nested arrays" $ do
      let val = B.Document (V.singleton (T.pack "a",
                  B.Array (V.singleton (B.Array (V.singleton (B.Int32 42))))))
      decode (encode val) @?= Right val

  , testCase "Multiple fields" $ do
      let val = B.Document (V.fromList
                  [ (T.pack "a", B.Int32 1)
                  , (T.pack "b", B.String (T.pack "two"))
                  , (T.pack "c", B.Bool True)
                  , (T.pack "d", B.Null)
                  ])
      decode (encode val) @?= Right val

  , testCase "Decode empty input" $
      case decode BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"
  ]

wireFormatTests :: TestTree
wireFormatTests = testGroup "Wire format"
  [ testCase "Empty document is 5 bytes" $ do
      let bs = encode (B.Document V.empty)
      BS.length bs @?= 5
      BS.unpack bs @?= [5, 0, 0, 0, 0]

  , testCase "Document size is first 4 LE bytes" $ do
      let bs = encode (B.Document V.empty)
          sz = BS.index bs 0
      sz @?= 5

  , testCase "Document ends with 0x00" $ do
      let bs = encode (B.Document V.empty)
      BS.last bs @?= 0x00

  , testCase "Bool true type tag is 0x08" $ do
      let bs = encode (B.Document (V.singleton (T.pack "b", B.Bool True)))
      BS.index bs 4 @?= 0x08
  ]
