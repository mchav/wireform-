module Test.CBORDiagnostic (cborDiagnosticTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word64)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified CBOR.Value as C
import CBOR.Diagnostic (toDiagnostic, fromDiagnostic)

cborDiagnosticTests :: TestTree
cborDiagnosticTests = testGroup "CBOR Diagnostic"
  [ renderTests
  , parseTests
  , roundtripTests
  ]

renderTests :: TestTree
renderTests = testGroup "Render"
  [ testCase "unsigned int 0" $
      toDiagnostic (C.UInt 0) @?= "0"

  , testCase "unsigned int 23" $
      toDiagnostic (C.UInt 23) @?= "23"

  , testCase "unsigned int 24" $
      toDiagnostic (C.UInt 24) @?= "24"

  , testCase "negative -1" $
      toDiagnostic (C.NInt 0) @?= "-1"

  , testCase "negative -100" $
      toDiagnostic (C.NInt 99) @?= "-100"

  , testCase "byte string" $
      toDiagnostic (C.ByteString (BS.pack [1, 2, 3, 4])) @?= "h'01020304'"

  , testCase "empty byte string" $
      toDiagnostic (C.ByteString BS.empty) @?= "h''"

  , testCase "text string" $
      toDiagnostic (C.TextString "hello") @?= "\"hello\""

  , testCase "text string with escape" $
      toDiagnostic (C.TextString "a\"b") @?= "\"a\\\"b\""

  , testCase "array" $
      toDiagnostic (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3])) @?= "[1, 2, 3]"

  , testCase "empty array" $
      toDiagnostic (C.Array V.empty) @?= "[]"

  , testCase "map with string keys" $
      toDiagnostic (C.Map (V.fromList [(C.TextString "a", C.UInt 1)])) @?= "{\"a\": 1}"

  , testCase "map with int keys" $
      toDiagnostic (C.Map (V.fromList [(C.UInt 1, C.TextString "a")])) @?= "{1: \"a\"}"

  , testCase "tag" $
      toDiagnostic (C.Tag 0 (C.TextString "2013-03-21T20:04:00Z"))
        @?= "0(\"2013-03-21T20:04:00Z\")"

  , testCase "false" $
      toDiagnostic (C.Bool False) @?= "false"

  , testCase "true" $
      toDiagnostic (C.Bool True) @?= "true"

  , testCase "null" $
      toDiagnostic C.Null @?= "null"

  , testCase "undefined" $
      toDiagnostic C.Undefined @?= "undefined"

  , testCase "simple value" $
      toDiagnostic (C.Simple 16) @?= "simple(16)"

  , testCase "float 1.5" $
      toDiagnostic (C.Float64 1.5) @?= "1.5"

  , testCase "NaN" $
      toDiagnostic (C.Float64 (0/0)) @?= "NaN"

  , testCase "Infinity" $
      toDiagnostic (C.Float64 (1/0)) @?= "Infinity"

  , testCase "-Infinity" $
      toDiagnostic (C.Float64 ((-1)/0)) @?= "-Infinity"
  ]

parseTests :: TestTree
parseTests = testGroup "Parse"
  [ testCase "unsigned int" $
      fromDiagnostic "42" @?= Right (C.UInt 42)

  , testCase "negative int" $
      fromDiagnostic "-1" @?= Right (C.NInt 0)

  , testCase "negative -100" $
      fromDiagnostic "-100" @?= Right (C.NInt 99)

  , testCase "byte string" $
      fromDiagnostic "h'01020304'" @?= Right (C.ByteString (BS.pack [1, 2, 3, 4]))

  , testCase "empty byte string" $
      fromDiagnostic "h''" @?= Right (C.ByteString BS.empty)

  , testCase "text string" $
      fromDiagnostic "\"hello\"" @?= Right (C.TextString "hello")

  , testCase "text string with escape" $
      fromDiagnostic "\"a\\\"b\"" @?= Right (C.TextString "a\"b")

  , testCase "array" $
      fromDiagnostic "[1, 2, 3]"
        @?= Right (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3]))

  , testCase "empty array" $
      fromDiagnostic "[]" @?= Right (C.Array V.empty)

  , testCase "map" $
      fromDiagnostic "{\"a\": 1}"
        @?= Right (C.Map (V.fromList [(C.TextString "a", C.UInt 1)]))

  , testCase "empty map" $
      fromDiagnostic "{}" @?= Right (C.Map V.empty)

  , testCase "tag" $
      fromDiagnostic "0(\"2013-03-21T20:04:00Z\")"
        @?= Right (C.Tag 0 (C.TextString "2013-03-21T20:04:00Z"))

  , testCase "false" $
      fromDiagnostic "false" @?= Right (C.Bool False)

  , testCase "true" $
      fromDiagnostic "true" @?= Right (C.Bool True)

  , testCase "null" $
      fromDiagnostic "null" @?= Right C.Null

  , testCase "undefined" $
      fromDiagnostic "undefined" @?= Right C.Undefined

  , testCase "NaN" $
      case fromDiagnostic "NaN" of
        Right (C.Float64 d) | isNaN d -> pure ()
        other -> assertFailure $ "expected NaN, got " ++ show other

  , testCase "Infinity" $
      fromDiagnostic "Infinity" @?= Right (C.Float64 (1/0))

  , testCase "-Infinity" $
      fromDiagnostic "-Infinity" @?= Right (C.Float64 ((-1)/0))

  , testCase "simple value" $
      fromDiagnostic "simple(16)" @?= Right (C.Simple 16)

  , testCase "float" $
      fromDiagnostic "1.5" @?= Right (C.Float64 1.5)

  , testCase "negative float" $
      fromDiagnostic "-1.5" @?= Right (C.Float64 (-1.5))

  , testCase "scientific notation" $
      fromDiagnostic "1.0e300" @?= Right (C.Float64 1.0e300)

  , testCase "nested" $
      fromDiagnostic "[1, [2, 3], {\"a\": true}]"
        @?= Right (C.Array (V.fromList
              [ C.UInt 1
              , C.Array (V.fromList [C.UInt 2, C.UInt 3])
              , C.Map (V.fromList [(C.TextString "a", C.Bool True)])
              ]))

  , testCase "error on empty" $
      case fromDiagnostic "" of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty"

  , testCase "error on trailing" $
      case fromDiagnostic "42 extra" of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on trailing"
  ]

roundtripTests :: TestTree
roundtripTests = testGroup "Roundtrip"
  [ testProperty "UInt roundtrip" $ property $ do
      n <- forAll $ Gen.word64 (Range.linear 0 1000000)
      let val = C.UInt n
      fromDiagnostic (toDiagnostic val) === Right val

  , testProperty "NInt roundtrip" $ property $ do
      n <- forAll $ Gen.word64 (Range.linear 0 1000000)
      let val = C.NInt n
      fromDiagnostic (toDiagnostic val) === Right val

  , testProperty "Bool roundtrip" $ property $ do
      b <- forAll Gen.bool
      let val = C.Bool b
      fromDiagnostic (toDiagnostic val) === Right val

  , testCase "Null roundtrip" $
      fromDiagnostic (toDiagnostic C.Null) @?= Right C.Null

  , testCase "Undefined roundtrip" $
      fromDiagnostic (toDiagnostic C.Undefined) @?= Right C.Undefined

  , testProperty "TextString roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
      let val = C.TextString t
      fromDiagnostic (toDiagnostic val) === Right val

  , testProperty "ByteString roundtrip" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 50)
      let val = C.ByteString bs
      fromDiagnostic (toDiagnostic val) === Right val

  , testProperty "Array of UInts roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 10) (Gen.word64 (Range.linear 0 1000))
      let val = C.Array (V.fromList (map C.UInt ns))
      fromDiagnostic (toDiagnostic val) === Right val

  , testProperty "Tag roundtrip" $ property $ do
      tagNum <- forAll $ Gen.word64 (Range.linear 0 1000)
      n <- forAll $ Gen.word64 (Range.linear 0 1000)
      let val = C.Tag tagNum (C.UInt n)
      fromDiagnostic (toDiagnostic val) === Right val
  ]
