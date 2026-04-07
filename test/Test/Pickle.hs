module Test.Pickle (pickleTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified Pickle.Value as P
import Pickle.Encode (encode)
import Pickle.Decode (decode)

pickleTests :: TestTree
pickleTests = testGroup "Pickle"
  [ unitTests
  , propertyRoundtrips
  , edgeCases
  , wireFormatTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit roundtrips"
  [ testCase "None" $ do
      let val = P.None
      decode (encode val) @?= Right val

  , testCase "Bool True" $ do
      let val = P.Bool True
      decode (encode val) @?= Right val

  , testCase "Bool False" $ do
      let val = P.Bool False
      decode (encode val) @?= Right val

  , testCase "Int 0" $ do
      let val = P.Int 0
      decode (encode val) @?= Right val

  , testCase "Int 42" $ do
      let val = P.Int 42
      decode (encode val) @?= Right val

  , testCase "Int -1" $ do
      let val = P.Int (-1)
      decode (encode val) @?= Right val

  , testCase "Int large" $ do
      let val = P.Int 1000000
      decode (encode val) @?= Right val

  , testCase "Float" $ do
      let val = P.Float 3.14
      decode (encode val) @?= Right val

  , testCase "String" $ do
      let val = P.String (T.pack "hello")
      decode (encode val) @?= Right val

  , testCase "Bytes" $ do
      let val = P.Bytes (BS.pack [1, 2, 3])
      decode (encode val) @?= Right val

  , testCase "Empty list" $ do
      let val = P.List V.empty
      decode (encode val) @?= Right val

  , testCase "List of ints" $ do
      let val = P.List (V.fromList [P.Int 1, P.Int 2, P.Int 3])
      decode (encode val) @?= Right val

  , testCase "Empty tuple" $ do
      let val = P.Tuple V.empty
      decode (encode val) @?= Right val

  , testCase "Tuple1" $ do
      let val = P.Tuple (V.singleton (P.Int 42))
      decode (encode val) @?= Right val

  , testCase "Tuple2" $ do
      let val = P.Tuple (V.fromList [P.Int 1, P.Int 2])
      decode (encode val) @?= Right val

  , testCase "Tuple3" $ do
      let val = P.Tuple (V.fromList [P.Int 1, P.Int 2, P.Int 3])
      decode (encode val) @?= Right val

  , testCase "Empty dict" $ do
      let val = P.Dict V.empty
      decode (encode val) @?= Right val

  , testCase "Dict" $ do
      let val = P.Dict (V.fromList
                  [ (P.String (T.pack "a"), P.Int 1)
                  , (P.String (T.pack "b"), P.Int 2)
                  ])
      decode (encode val) @?= Right val

  , testCase "Nested structures" $ do
      let val = P.List (V.fromList
                  [ P.Tuple (V.fromList [P.Int 1, P.String (T.pack "x")])
                  , P.None
                  , P.Bool True
                  ])
      decode (encode val) @?= Right val
  ]

propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "Int roundtrip" $ property $ do
      n <- forAll $ Gen.int64 Range.linearBounded
      let val = P.Int n
      decode (encode val) === Right val

  , testProperty "String roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 128) Gen.alphaNum
      let val = P.String t
      decode (encode val) === Right val

  , testProperty "Bytes roundtrip" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 256)
      let val = P.Bytes bs
      decode (encode val) === Right val

  , testProperty "Bool roundtrip" $ property $ do
      b <- forAll Gen.bool
      let val = P.Bool b
      decode (encode val) === Right val

  , testProperty "Float roundtrip" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
      let val = P.Float d
      decode (encode val) === Right val

  , testProperty "List roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 15) (Gen.int64 Range.linearBounded)
      let val = P.List (V.fromList (map P.Int ns))
      decode (encode val) === Right val

  , testProperty "Tuple roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 8) (Gen.int64 (Range.linear (-1000) 1000))
      let val = P.Tuple (V.fromList (map P.Int ns))
      decode (encode val) === Right val

  , testProperty "Dict roundtrip" $ property $ do
      kvs <- forAll $ Gen.list (Range.linear 0 10) $ do
        k <- Gen.text (Range.linear 1 32) Gen.alphaNum
        v <- Gen.int64 Range.linearBounded
        pure (P.String k, P.Int v)
      let val = P.Dict (V.fromList kvs)
      decode (encode val) === Right val

  , testProperty "None roundtrip" $ property $ do
      let val = P.None
      decode (encode val) === Right val
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "Empty string" $ do
      let val = P.String T.empty
      decode (encode val) @?= Right val

  , testCase "Empty bytes" $ do
      let val = P.Bytes BS.empty
      decode (encode val) @?= Right val

  , testCase "Int min bound" $ do
      let val = P.Int minBound
      decode (encode val) @?= Right val

  , testCase "Int max bound" $ do
      let val = P.Int maxBound
      decode (encode val) @?= Right val

  , testCase "Decode empty input" $
      case decode BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"

  , testCase "Long tuple (>3 elements)" $ do
      let val = P.Tuple (V.fromList [P.Int 1, P.Int 2, P.Int 3, P.Int 4, P.Int 5])
      decode (encode val) @?= Right val

  , testCase "Long string (>255 bytes)" $ do
      let val = P.String (T.replicate 300 (T.pack "a"))
      decode (encode val) @?= Right val

  , testCase "Long bytes (>255 bytes)" $ do
      let val = P.Bytes (BS.replicate 300 0x42)
      decode (encode val) @?= Right val
  ]

wireFormatTests :: TestTree
wireFormatTests = testGroup "Wire format"
  [ testCase "Starts with protocol header 0x80 0x02" $ do
      let bs = encode P.None
      BS.index bs 0 @?= 0x80
      BS.index bs 1 @?= 0x02

  , testCase "Ends with STOP 0x2E" $ do
      let bs = encode P.None
      BS.last bs @?= 0x2E

  , testCase "None is N (0x4E)" $ do
      let bs = encode P.None
      BS.index bs 2 @?= 0x4E

  , testCase "True is NEWTRUE (0x88)" $ do
      let bs = encode (P.Bool True)
      BS.index bs 2 @?= 0x88

  , testCase "False is NEWFALSE (0x89)" $ do
      let bs = encode (P.Bool False)
      BS.index bs 2 @?= 0x89
  ]
