module Test.Wire (wireTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Proto.Wire
import Proto.Wire.Encode
import Proto.Wire.Decode

wireTests :: TestTree
wireTests = testGroup "Wire Format"
  [ testGroup "Varint encoding/decoding"
      [ testCase "encode 0" $ do
          let bs = buildToBS (putVarint 0)
          bs @?= BS.pack [0x00]

      , testCase "encode 1" $ do
          let bs = buildToBS (putVarint 1)
          bs @?= BS.pack [0x01]

      , testCase "encode 127" $ do
          let bs = buildToBS (putVarint 127)
          bs @?= BS.pack [0x7F]

      , testCase "encode 128" $ do
          let bs = buildToBS (putVarint 128)
          bs @?= BS.pack [0x80, 0x01]

      , testCase "encode 300" $ do
          let bs = buildToBS (putVarint 300)
          bs @?= BS.pack [0xAC, 0x02]

      , testCase "encode 16384" $ do
          let bs = buildToBS (putVarint 16384)
          bs @?= BS.pack [0x80, 0x80, 0x01]

      , testCase "decode 0" $ do
          let bs = BS.pack [0x00]
          runDecoder getVarint bs @?= Right 0

      , testCase "decode 1" $ do
          let bs = BS.pack [0x01]
          runDecoder getVarint bs @?= Right 1

      , testCase "decode 300" $ do
          let bs = BS.pack [0xAC, 0x02]
          runDecoder getVarint bs @?= Right 300

      , testProperty "varint roundtrip" $ property $ do
          n <- forAll $ Gen.word64 (Range.linear 0 maxBound)
          let encoded = buildToBS (putVarint n)
          runDecoder getVarint encoded === Right n

      , testProperty "varint size bound" $ property $ do
          n <- forAll $ Gen.word64 (Range.linear 0 maxBound)
          let encoded = buildToBS (putVarint n)
          assert (BS.length encoded <= 10)
      ]

  , testGroup "ZigZag encoding"
      [ testCase "zigzag 0" $ zigZag32 0 @?= 0
      , testCase "zigzag -1" $ zigZag32 (-1) @?= 1
      , testCase "zigzag 1" $ zigZag32 1 @?= 2
      , testCase "zigzag -2" $ zigZag32 (-2) @?= 3

      , testProperty "zigzag32 roundtrip" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          unZigZag32 (zigZag32 n) === n

      , testProperty "zigzag64 roundtrip" $ property $ do
          n <- forAll $ Gen.int64 Range.linearBounded
          unZigZag64 (zigZag64 n) === n

      , testProperty "sint32 roundtrip" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          let encoded = buildToBS (putSVarint32 n)
          runDecoder getSVarint32 encoded === Right n

      , testProperty "sint64 roundtrip" $ property $ do
          n <- forAll $ Gen.int64 Range.linearBounded
          let encoded = buildToBS (putSVarint64 n)
          runDecoder getSVarint64 encoded === Right n
      ]

  , testGroup "Fixed-width encoding"
      [ testProperty "fixed32 roundtrip" $ property $ do
          n <- forAll $ Gen.word32 Range.linearBounded
          let encoded = buildToBS (putFixed32 n)
          assert (BS.length encoded == 4)
          runDecoder getFixed32 encoded === Right n

      , testProperty "fixed64 roundtrip" $ property $ do
          n <- forAll $ Gen.word64 Range.linearBounded
          let encoded = buildToBS (putFixed64 n)
          assert (BS.length encoded == 8)
          runDecoder getFixed64 encoded === Right n

      , testProperty "float roundtrip" $ property $ do
          n <- forAll $ Gen.float (Range.linearFrac (-1e30) 1e30)
          let encoded = buildToBS (putFloat n)
          runDecoder getFloat encoded === Right n

      , testProperty "double roundtrip" $ property $ do
          n <- forAll $ Gen.double (Range.linearFrac (-1e300) 1e300)
          let encoded = buildToBS (putDouble n)
          runDecoder getDouble encoded === Right n
      ]

  , testGroup "Length-delimited"
      [ testProperty "bytestring roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 1000)
          let encoded = buildToBS (putByteString bs)
          runDecoder getByteString encoded === Right bs

      , testProperty "text roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 200) Gen.unicode
          let encoded = buildToBS (putText t)
          runDecoder getText encoded === Right t

      , testCase "zero-copy bytestring" $ do
          let payload = "hello world"
              encoded = buildToBS (putByteString payload)
          case runDecoder getByteString encoded of
            Right decoded -> decoded @?= payload
            Left e -> assertFailure (show e)
      ]

  , testGroup "Tag encoding"
      [ testCase "tag field 1 varint" $ do
          let tag = makeTag 1 WireVarint
          encodeTag tag @?= 0x08

      , testCase "tag field 1 length-delimited" $ do
          let tag = makeTag 1 WireLengthDelimited
          encodeTag tag @?= 0x0A

      , testCase "tag field 2 varint" $ do
          let tag = makeTag 2 WireVarint
          encodeTag tag @?= 0x10

      , testProperty "tag roundtrip" $ property $ do
          fn <- forAll $ Gen.int (Range.linear 1 ((2 :: Int)^(29 :: Int) - 1))
          wt <- forAll $ Gen.element [WireVarint, Wire64Bit, WireLengthDelimited, Wire32Bit]
          let tag = makeTag fn wt
              encoded = encodeTag tag
          decodeTag encoded === Just tag
      ]

  , testGroup "Skip unknown fields"
      [ testCase "skip varint" $ do
          let bs = buildToBS (putVarint 12345)
          case runDecoder (skipField WireVarint) bs of
            Right () -> pure ()
            Left e   -> assertFailure (show e)

      , testCase "skip fixed32" $ do
          let bs = buildToBS (putFixed32 42)
          case runDecoder (skipField Wire32Bit) bs of
            Right () -> pure ()
            Left e   -> assertFailure (show e)

      , testCase "skip fixed64" $ do
          let bs = buildToBS (putFixed64 42)
          case runDecoder (skipField Wire64Bit) bs of
            Right () -> pure ()
            Left e   -> assertFailure (show e)

      , testCase "skip length-delimited" $ do
          let bs = buildToBS (putByteString "hello")
          case runDecoder (skipField WireLengthDelimited) bs of
            Right () -> pure ()
            Left e   -> assertFailure (show e)
      ]

  , testGroup "Error handling"
      [ testCase "unexpected end on varint" $ do
          runDecoder getVarint BS.empty @?= Left UnexpectedEnd

      , testCase "unexpected end on fixed32" $ do
          runDecoder getFixed32 (BS.pack [1, 2]) @?= Left UnexpectedEnd

      , testCase "unexpected end on fixed64" $ do
          runDecoder getFixed64 (BS.pack [1, 2, 3, 4]) @?= Left UnexpectedEnd

      , testCase "extra bytes after decode" $ do
          let bs = BS.pack [0x00, 0xFF]
          case runDecoder getVarint bs of
            Left ExtraBytes -> pure ()
            other           -> assertFailure ("Expected ExtraBytes, got: " <> show other)
      ]
  ]

buildToBS :: B.Builder -> ByteString
buildToBS = BL.toStrict . B.toLazyByteString
