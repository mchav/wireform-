module Test.ASN1 (asn1Tests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import ASN1.Value
import ASN1.Encode (encode)
import ASN1.Decode (decode)

asn1Tests :: TestTree
asn1Tests = testGroup "ASN1"
  [ unitTests
  , propertyRoundtrips
  , edgeCases
  , wireFormatTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit roundtrips"
  [ testCase "Boolean True" $ do
      let val = Boolean True
      decode (encode val) @?= Right val

  , testCase "Boolean False" $ do
      let val = Boolean False
      decode (encode val) @?= Right val

  , testCase "Integer 0" $ do
      let val = Integer 0
      decode (encode val) @?= Right val

  , testCase "Integer 127" $ do
      let val = Integer 127
      decode (encode val) @?= Right val

  , testCase "Integer 128" $ do
      let val = Integer 128
      decode (encode val) @?= Right val

  , testCase "Integer -1" $ do
      let val = Integer (-1)
      decode (encode val) @?= Right val

  , testCase "Integer -128" $ do
      let val = Integer (-128)
      decode (encode val) @?= Right val

  , testCase "Integer large positive" $ do
      let val = Integer 123456789
      decode (encode val) @?= Right val

  , testCase "Integer large negative" $ do
      let val = Integer (-123456789)
      decode (encode val) @?= Right val

  , testCase "Null" $ do
      let val = Null
      decode (encode val) @?= Right val

  , testCase "OctetString" $ do
      let val = OctetString (BS.pack [1, 2, 3, 4])
      decode (encode val) @?= Right val

  , testCase "UTF8String" $ do
      let val = UTF8String (T.pack "hello world")
      decode (encode val) @?= Right val

  , testCase "PrintableString" $ do
      let val = PrintableString (T.pack "test123")
      decode (encode val) @?= Right val

  , testCase "IA5String" $ do
      let val = IA5String (T.pack "user@example.com")
      decode (encode val) @?= Right val

  , testCase "UTCTime" $ do
      let val = UTCTime (T.pack "230101120000Z")
      decode (encode val) @?= Right val

  , testCase "GeneralizedTime" $ do
      let val = GeneralizedTime (T.pack "20230101120000Z")
      decode (encode val) @?= Right val

  , testCase "BitString" $ do
      let val = BitString 0 (BS.pack [0xFF, 0xF0])
      decode (encode val) @?= Right val

  , testCase "OID 1.2.840.113549" $ do
      let val = OID (V.fromList [1, 2, 840, 113549])
      decode (encode val) @?= Right val

  , testCase "OID 2.5.4.3" $ do
      let val = OID (V.fromList [2, 5, 4, 3])
      decode (encode val) @?= Right val

  , testCase "Sequence" $ do
      let val = Sequence (V.fromList [Integer 1, Boolean True, Null])
      decode (encode val) @?= Right val

  , testCase "Set" $ do
      let val = Set (V.fromList [Integer 42, UTF8String (T.pack "abc")])
      decode (encode val) @?= Right val

  , testCase "Nested sequence" $ do
      let inner = Sequence (V.fromList [Integer 1, Integer 2])
          val = Sequence (V.fromList [inner, Boolean True])
      decode (encode val) @?= Right val

  , testCase "Tagged context-specific" $ do
      let val = Tagged ContextSpecific 0 (Integer 42)
      decode (encode val) @?= Right val
  ]

propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "Integer roundtrip" $ property $ do
      n <- forAll $ Gen.integral (Range.linear (-1000000) 1000000)
      let val = Integer n
      decode (encode val) === Right val

  , testProperty "OctetString roundtrip" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 128)
      let val = OctetString bs
      decode (encode val) === Right val

  , testProperty "UTF8String roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
      let val = UTF8String t
      decode (encode val) === Right val

  , testProperty "Sequence of integers roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 10) (Gen.integral (Range.linear (-999) 999))
      let val = Sequence (V.fromList (map Integer ns))
      decode (encode val) === Right val
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "Empty sequence" $ do
      let val = Sequence V.empty
      decode (encode val) @?= Right val

  , testCase "Empty set" $ do
      let val = Set V.empty
      decode (encode val) @?= Right val

  , testCase "Empty octet string" $ do
      let val = OctetString BS.empty
      decode (encode val) @?= Right val

  , testCase "Decode empty input" $
      case decode BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"

  , testCase "Very large integer" $ do
      let val = Integer (2^(256 :: Int))
      decode (encode val) @?= Right val
  ]

wireFormatTests :: TestTree
wireFormatTests = testGroup "Wire format"
  [ testCase "Null is 0x05 0x00" $ do
      let bs = encode Null
      BS.unpack bs @?= [0x05, 0x00]

  , testCase "Boolean True is 0x01 0x01 0xFF" $ do
      let bs = encode (Boolean True)
      BS.unpack bs @?= [0x01, 0x01, 0xFF]

  , testCase "Boolean False is 0x01 0x01 0x00" $ do
      let bs = encode (Boolean False)
      BS.unpack bs @?= [0x01, 0x01, 0x00]

  , testCase "Integer 0 is 0x02 0x01 0x00" $ do
      let bs = encode (Integer 0)
      BS.unpack bs @?= [0x02, 0x01, 0x00]

  , testCase "Sequence tag is 0x30" $ do
      let bs = encode (Sequence V.empty)
      BS.index bs 0 @?= 0x30
  ]
