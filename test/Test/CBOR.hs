module Test.CBOR (cborTests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
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
import CBOR.Encode (encode)
import CBOR.Decode (decode)
import CBOR.JSON (toJSON, fromJSON)

cborTests :: TestTree
cborTests = testGroup "CBOR"
  [ rfc8949AppendixA
  , propertyRoundtrips
  , edgeCases
  , jsonTests
  ]

-- | RFC 8949 Appendix A test vectors: exact byte sequences.
rfc8949AppendixA :: TestTree
rfc8949AppendixA = testGroup "RFC 8949 Appendix A test vectors"
  [ testGroup "Unsigned integers"
      [ testCase "0 = [0x00]" $
          encode (C.UInt 0) @?= BS.pack [0x00]
      , testCase "1 = [0x01]" $
          encode (C.UInt 1) @?= BS.pack [0x01]
      , testCase "10 = [0x0a]" $
          encode (C.UInt 10) @?= BS.pack [0x0a]
      , testCase "23 = [0x17]" $
          encode (C.UInt 23) @?= BS.pack [0x17]
      , testCase "24 = [0x18, 0x18]" $
          encode (C.UInt 24) @?= BS.pack [0x18, 0x18]
      , testCase "25 = [0x18, 0x19]" $
          encode (C.UInt 25) @?= BS.pack [0x18, 0x19]
      , testCase "100 = [0x18, 0x64]" $
          encode (C.UInt 100) @?= BS.pack [0x18, 0x64]
      , testCase "1000 = [0x19, 0x03, 0xe8]" $
          encode (C.UInt 1000) @?= BS.pack [0x19, 0x03, 0xe8]
      , testCase "1000000 = [0x1a, 0x00, 0x0f, 0x42, 0x40]" $
          encode (C.UInt 1000000) @?= BS.pack [0x1a, 0x00, 0x0f, 0x42, 0x40]
      , testCase "1000000000000 = [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00]" $
          encode (C.UInt 1000000000000) @?= BS.pack [0x1b, 0x00, 0x00, 0x00, 0xe8, 0xd4, 0xa5, 0x10, 0x00]
      ]

  , testGroup "Negative integers"
      [ testCase "-1 = [0x20]" $
          encode (C.NInt 0) @?= BS.pack [0x20]
      , testCase "-10 = [0x29]" $
          encode (C.NInt 9) @?= BS.pack [0x29]
      , testCase "-100 = [0x38, 0x63]" $
          encode (C.NInt 99) @?= BS.pack [0x38, 0x63]
      , testCase "-1000 = [0x39, 0x03, 0xe7]" $
          encode (C.NInt 999) @?= BS.pack [0x39, 0x03, 0xe7]
      ]

  , testGroup "Simple values / booleans"
      [ testCase "false = [0xf4]" $
          encode (C.Bool False) @?= BS.pack [0xf4]
      , testCase "true = [0xf5]" $
          encode (C.Bool True) @?= BS.pack [0xf5]
      , testCase "null = [0xf6]" $
          encode C.Null @?= BS.pack [0xf6]
      , testCase "undefined = [0xf7]" $
          encode C.Undefined @?= BS.pack [0xf7]
      ]

  , testGroup "Text strings"
      [ testCase "\"\" = [0x60]" $
          encode (C.TextString "") @?= BS.pack [0x60]
      , testCase "\"a\" = [0x61, 0x61]" $
          encode (C.TextString "a") @?= BS.pack [0x61, 0x61]
      , testCase "\"IETF\" = [0x64, 0x49, 0x45, 0x54, 0x46]" $
          encode (C.TextString "IETF") @?= BS.pack [0x64, 0x49, 0x45, 0x54, 0x46]
      , testCase "\"\\\"\\\\\" = [0x62, 0x22, 0x5c]" $
          encode (C.TextString "\"\\") @?= BS.pack [0x62, 0x22, 0x5c]
      , testCase "\"\\u00fc\" = [0x62, 0xc3, 0xbc]" $
          encode (C.TextString "\252") @?= BS.pack [0x62, 0xc3, 0xbc]
      ]

  , testGroup "Byte strings"
      [ testCase "h'' = [0x40]" $
          encode (C.ByteString BS.empty) @?= BS.pack [0x40]
      , testCase "h'01020304' = [0x44, 0x01, 0x02, 0x03, 0x04]" $
          encode (C.ByteString (BS.pack [1,2,3,4])) @?= BS.pack [0x44, 0x01, 0x02, 0x03, 0x04]
      ]

  , testGroup "Arrays"
      [ testCase "[] = [0x80]" $
          encode (C.Array V.empty) @?= BS.pack [0x80]
      , testCase "[1, 2, 3] = [0x83, 0x01, 0x02, 0x03]" $
          encode (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3]))
            @?= BS.pack [0x83, 0x01, 0x02, 0x03]
      , testCase "[1, [2, 3], [4, 5]]" $
          encode (C.Array (V.fromList
            [ C.UInt 1
            , C.Array (V.fromList [C.UInt 2, C.UInt 3])
            , C.Array (V.fromList [C.UInt 4, C.UInt 5])
            ]))
            @?= BS.pack [0x83, 0x01, 0x82, 0x02, 0x03, 0x82, 0x04, 0x05]
      , testCase "25 element array" $
          encode (C.Array (V.fromList [C.UInt (fromIntegral i) | i <- [1..25 :: Int]]))
            @?= BS.pack ([0x98, 25] ++ [1..23] ++ [0x18, 24, 0x18, 25])
      ]

  , testGroup "Maps"
      [ testCase "{} = [0xa0]" $
          encode (C.Map V.empty) @?= BS.pack [0xa0]
      , testCase "{1: 2, 3: 4}" $
          encode (C.Map (V.fromList [(C.UInt 1, C.UInt 2), (C.UInt 3, C.UInt 4)]))
            @?= BS.pack [0xa2, 0x01, 0x02, 0x03, 0x04]
      ]

  , testGroup "Tags"
      [ testCase "tag 1 with integer" $
          encode (C.Tag 1 (C.UInt 1363896240))
            @?= BS.pack [0xc1, 0x1a, 0x51, 0x4b, 0x67, 0xb0]
      ]

  , testGroup "Decode test vectors"
      [ testCase "decode 0" $
          decode (BS.pack [0x00]) @?= Right (C.UInt 0)
      , testCase "decode 1" $
          decode (BS.pack [0x01]) @?= Right (C.UInt 1)
      , testCase "decode 23" $
          decode (BS.pack [0x17]) @?= Right (C.UInt 23)
      , testCase "decode 24" $
          decode (BS.pack [0x18, 0x18]) @?= Right (C.UInt 24)
      , testCase "decode 100" $
          decode (BS.pack [0x18, 0x64]) @?= Right (C.UInt 100)
      , testCase "decode 1000" $
          decode (BS.pack [0x19, 0x03, 0xe8]) @?= Right (C.UInt 1000)
      , testCase "decode 1000000" $
          decode (BS.pack [0x1a, 0x00, 0x0f, 0x42, 0x40]) @?= Right (C.UInt 1000000)
      , testCase "decode -1" $
          decode (BS.pack [0x20]) @?= Right (C.NInt 0)
      , testCase "decode -100" $
          decode (BS.pack [0x38, 0x63]) @?= Right (C.NInt 99)
      , testCase "decode false" $
          decode (BS.pack [0xf4]) @?= Right (C.Bool False)
      , testCase "decode true" $
          decode (BS.pack [0xf5]) @?= Right (C.Bool True)
      , testCase "decode null" $
          decode (BS.pack [0xf6]) @?= Right C.Null
      , testCase "decode \"\"" $
          decode (BS.pack [0x60]) @?= Right (C.TextString "")
      , testCase "decode \"a\"" $
          decode (BS.pack [0x61, 0x61]) @?= Right (C.TextString "a")
      , testCase "decode \"IETF\"" $
          decode (BS.pack [0x64, 0x49, 0x45, 0x54, 0x46]) @?= Right (C.TextString "IETF")
      , testCase "decode []" $
          decode (BS.pack [0x80]) @?= Right (C.Array V.empty)
      , testCase "decode [1,2,3]" $
          decode (BS.pack [0x83, 0x01, 0x02, 0x03])
            @?= Right (C.Array (V.fromList [C.UInt 1, C.UInt 2, C.UInt 3]))
      , testCase "decode {}" $
          decode (BS.pack [0xa0]) @?= Right (C.Map V.empty)
      , testCase "decode {1:2, 3:4}" $
          decode (BS.pack [0xa2, 0x01, 0x02, 0x03, 0x04])
            @?= Right (C.Map (V.fromList [(C.UInt 1, C.UInt 2), (C.UInt 3, C.UInt 4)]))
      ]

  , testGroup "Decode indefinite-length"
      [ testCase "indefinite array [_ 1, 2]" $
          decode (BS.pack [0x9f, 0x01, 0x02, 0xff])
            @?= Right (C.Array (V.fromList [C.UInt 1, C.UInt 2]))
      , testCase "indefinite map {_ 1:2}" $
          decode (BS.pack [0xbf, 0x01, 0x02, 0xff])
            @?= Right (C.Map (V.fromList [(C.UInt 1, C.UInt 2)]))
      ]

  , testGroup "Float encoding/decoding"
      [ testCase "float32 100000.0" $ do
          let val = C.Float32 100000.0
              bs  = encode val
          BS.index bs 0 @?= 0xfa
          decode bs @?= Right val
      , testCase "float64 1.1" $ do
          let val = C.Float64 1.1
              bs  = encode val
          BS.index bs 0 @?= 0xfb
          decode bs @?= Right val
      ]
  ]

-- | Property-based roundtrip tests.
propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "UInt roundtrip" $ property $ do
      n <- forAll $ Gen.word64 Range.linearBounded
      let val = C.UInt n
      decode (encode val) === Right val

  , testProperty "NInt roundtrip" $ property $ do
      n <- forAll $ Gen.word64 Range.linearBounded
      let val = C.NInt n
      decode (encode val) === Right val

  , testProperty "Bool roundtrip" $ property $ do
      b <- forAll Gen.bool
      let val = C.Bool b
      decode (encode val) === Right val

  , testProperty "Null roundtrip" $ property $ do
      decode (encode C.Null) === Right C.Null

  , testProperty "ByteString roundtrip" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 512)
      let val = C.ByteString bs
      decode (encode val) === Right val

  , testProperty "TextString roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 256) Gen.unicode
      let val = C.TextString t
      decode (encode val) === Right val

  , testProperty "Float32 roundtrip" $ property $ do
      f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
      let val = C.Float32 f
      decode (encode val) === Right val

  , testProperty "Float64 roundtrip" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
      let val = C.Float64 d
      decode (encode val) === Right val

  , testProperty "Array of UInts roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 50)
                       (Gen.word64 (Range.linear 0 0xffffffff))
      let val = C.Array (V.fromList (map C.UInt ns))
      decode (encode val) === Right val

  , testProperty "Map roundtrip" $ property $ do
      entries <- forAll $ Gen.list (Range.linear 0 30) $ do
        k <- Gen.text (Range.linear 1 32) Gen.alphaNum
        v <- Gen.word64 (Range.linear 0 0xffff)
        pure (C.TextString k, C.UInt v)
      let val = C.Map (V.fromList entries)
      decode (encode val) === Right val

  , testProperty "Tag roundtrip" $ property $ do
      tagNum <- forAll $ Gen.word64 (Range.linear 0 0xffff)
      n <- forAll $ Gen.word64 (Range.linear 0 0xffffffff)
      let val = C.Tag tagNum (C.UInt n)
      decode (encode val) === Right val

  , testProperty "Nested arrays roundtrip" $ property $ do
      inner <- forAll $ Gen.list (Range.linear 0 10)
                          (Gen.word64 (Range.linear 0 255))
      let val = C.Array (V.fromList
                  [ C.UInt 1
                  , C.Array (V.fromList (map C.UInt inner))
                  , C.TextString "nested"
                  ])
      decode (encode val) === Right val
  ]

-- | Edge cases.
edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "large integer (max Word64)" $ do
      let val = C.UInt maxBound
      decode (encode val) @?= Right val

  , testCase "large negative integer (max Word64)" $ do
      let val = C.NInt maxBound
      decode (encode val) @?= Right val

  , testCase "deeply nested" $ do
      let nest 0 = C.UInt 42
          nest n = C.Array (V.singleton (nest (n - 1)))
          val = nest (20 :: Int)
      decode (encode val) @?= Right val

  , testCase "tagged tagged value" $ do
      let val = C.Tag 0 (C.Tag 1 (C.TextString "epoch"))
      decode (encode val) @?= Right val

  , testCase "map with mixed key types" $ do
      let val = C.Map (V.fromList
                  [ (C.UInt 1, C.TextString "one")
                  , (C.TextString "two", C.UInt 2)
                  , (C.Bool True, C.Null)
                  ])
      decode (encode val) @?= Right val

  , testCase "empty byte string" $ do
      let val = C.ByteString BS.empty
      decode (encode val) @?= Right val

  , testCase "empty text string" $ do
      let val = C.TextString ""
      decode (encode val) @?= Right val

  , testCase "simple value 16" $ do
      let val = C.Simple 16
      decode (encode val) @?= Right val

  , testCase "simple value 255" $ do
      let val = C.Simple 255
      decode (encode val) @?= Right val

  , testCase "decode empty input" $
      case decode BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"
  ]

-- | JSON conversion tests.
jsonTests :: TestTree
jsonTests = testGroup "JSON conversion"
  [ testCase "UInt to JSON" $
      toJSON (C.UInt 42) @?= Aeson.Number 42

  , testCase "NInt to JSON" $
      toJSON (C.NInt 0) @?= Aeson.Number (-1)

  , testCase "Bool to JSON" $ do
      toJSON (C.Bool True) @?= Aeson.Bool True
      toJSON (C.Bool False) @?= Aeson.Bool False

  , testCase "Null to JSON" $
      toJSON C.Null @?= Aeson.Null

  , testCase "TextString to JSON" $
      toJSON (C.TextString "hello") @?= Aeson.String "hello"

  , testCase "Array to JSON" $
      toJSON (C.Array (V.fromList [C.UInt 1, C.UInt 2]))
        @?= Aeson.Array (V.fromList [Aeson.Number 1, Aeson.Number 2])

  , testCase "Tag to JSON" $ do
      let json = toJSON (C.Tag 1 (C.UInt 42))
      case json of
        Aeson.Object _ -> pure ()
        _ -> assertFailure "expected JSON object for tag"

  , testCase "fromJSON null" $
      fromJSON Aeson.Null @?= C.Null

  , testCase "fromJSON bool" $
      fromJSON (Aeson.Bool True) @?= C.Bool True

  , testCase "fromJSON string" $
      fromJSON (Aeson.String "hi") @?= C.TextString "hi"

  , testCase "fromJSON positive int" $
      fromJSON (Aeson.Number 42) @?= C.UInt 42

  , testCase "fromJSON negative int" $
      fromJSON (Aeson.Number (-1)) @?= C.NInt 0

  , testCase "fromJSON array" $
      fromJSON (Aeson.Array (V.fromList [Aeson.Number 1]))
        @?= C.Array (V.fromList [C.UInt 1])
  ]
