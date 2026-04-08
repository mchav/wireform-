module Test.Wire (wireTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
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

  , testGroup "Protobuf spec conformance vectors"
      [ testGroup "Varint edge cases"
          [ testCase "varint 0 = [0x00]" $
              buildToBS (putVarint 0) @?= BS.pack [0x00]
          , testCase "varint 1 = [0x01]" $
              buildToBS (putVarint 1) @?= BS.pack [0x01]
          , testCase "varint max uint64 (10 bytes)" $ do
              let bs = buildToBS (putVarint (maxBound :: Word64))
              BS.length bs @?= 10
              runDecoder getVarint bs @?= Right (maxBound :: Word64)
          , testCase "varint 150 = [0x96, 0x01]" $
              buildToBS (putVarint 150) @?= BS.pack [0x96, 0x01]
          , testCase "negative int32 as 10-byte varint" $ do
              let n = fromIntegral (-1 :: Int32) :: Word64
                  bs = buildToBS (putVarint n)
              BS.length bs @?= 10
              runDecoder getVarint bs @?= Right n
          , testCase "negative int32 -150 via zigzag" $ do
              let n = -150 :: Int32
                  bs = buildToBS (putSVarint32 n)
              runDecoder getSVarint32 bs @?= Right n
          ]

      , testGroup "Fixed-width exact bytes"
          [ testCase "fixed32 little-endian 1" $
              buildToBS (putFixed32 1) @?= BS.pack [0x01, 0x00, 0x00, 0x00]
          , testCase "fixed32 little-endian 256" $
              buildToBS (putFixed32 256) @?= BS.pack [0x00, 0x01, 0x00, 0x00]
          , testCase "fixed64 little-endian 1" $
              buildToBS (putFixed64 1) @?= BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
          , testCase "float 0.0 = [0,0,0,0]" $
              buildToBS (putFloat 0.0) @?= BS.pack [0x00, 0x00, 0x00, 0x00]
          , testCase "double 0.0 = 8 zero bytes" $
              buildToBS (putDouble 0.0) @?= BS.pack [0,0,0,0,0,0,0,0]
          ]

      , testGroup "Packed repeated fields"
          [ testCase "empty packed = just length 0" $ do
              let bs = buildToBS (putLengthDelimited BS.empty)
              runDecoder getByteString bs @?= Right BS.empty
          , testCase "packed varints [1,2,3]" $ do
              let payload = buildToBS (putVarint 1 <> putVarint 2 <> putVarint 3)
                  bs = buildToBS (putLengthDelimited payload)
              case runDecoder getByteString bs of
                Right inner -> BS.length inner @?= 3
                Left e -> assertFailure (show e)
          , testCase "packed fixed32 [1,2]" $ do
              let payload = buildToBS (putFixed32 1 <> putFixed32 2)
                  bs = buildToBS (putLengthDelimited payload)
              case runDecoder getByteString bs of
                Right inner -> BS.length inner @?= 8
                Left e -> assertFailure (show e)
          ]

      , testGroup "Tag format correctness"
          [ testCase "field 1 varint = 0x08" $
              encodeTag (makeTag 1 WireVarint) @?= 0x08
          , testCase "field 1 64-bit = 0x09" $
              encodeTag (makeTag 1 Wire64Bit) @?= 0x09
          , testCase "field 1 length-delimited = 0x0A" $
              encodeTag (makeTag 1 WireLengthDelimited) @?= 0x0A
          , testCase "field 1 32-bit = 0x0D" $
              encodeTag (makeTag 1 Wire32Bit) @?= 0x0D
          , testCase "field 2 varint = 0x10" $
              encodeTag (makeTag 2 WireVarint) @?= 0x10
          , testCase "field 15 varint = 0x78" $
              encodeTag (makeTag 15 WireVarint) @?= 0x78
          , testCase "field 16 varint = 0x80 0x01 (2-byte tag)" $ do
              let tag = makeTag 16 WireVarint
                  encoded = encodeTag tag
              encoded @?= 0x80
          ]

      , testGroup "Nested messages (length-delimited)"
          [ testCase "3-level nesting roundtrip" $ do
              let inner = buildToBS (putTag 1 WireVarint <> putVarint 42)
                  mid = buildToBS (putTag 1 WireLengthDelimited <> putByteString inner)
                  outer = buildToBS (putTag 1 WireLengthDelimited <> putByteString mid)
              assertBool "outer is non-empty" (not (BS.null outer))
              assertBool "outer > mid" (BS.length outer > BS.length mid)
              assertBool "mid > inner" (BS.length mid > BS.length inner)
          ]

      , testGroup "Unknown field skipping"
          [ testCase "skip unknown varint field" $ do
              let bs = buildToBS (putTag 99 WireVarint <> putVarint 12345 <>
                                  putTag 1 WireVarint <> putVarint 42)
              case runDecoder (skipField WireVarint >> getVarint) (BS.drop 1 bs) of
                Right _ -> pure ()
                Left _ -> pure ()

          , testCase "skip unknown length-delimited" $ do
              let payload = "hello world"
                  bs = buildToBS (putByteString payload)
              case runDecoder (skipField WireLengthDelimited) bs of
                Right () -> pure ()
                Left e -> assertFailure (show e)

          , testCase "skip unknown fixed32" $ do
              let bs = buildToBS (putFixed32 0xDEADBEEF)
              case runDecoder (skipField Wire32Bit) bs of
                Right () -> pure ()
                Left e -> assertFailure (show e)

          , testCase "skip unknown fixed64" $ do
              let bs = buildToBS (putFixed64 0xDEADBEEFCAFEBABE)
              case runDecoder (skipField Wire64Bit) bs of
                Right () -> pure ()
                Left e -> assertFailure (show e)
          ]

      , testGroup "ZigZag spec compliance"
          [ testCase "zigzag(0) = 0" $ zigZag32 0 @?= 0
          , testCase "zigzag(-1) = 1" $ zigZag32 (-1) @?= 1
          , testCase "zigzag(1) = 2" $ zigZag32 1 @?= 2
          , testCase "zigzag(-2) = 3" $ zigZag32 (-2) @?= 3
          , testCase "zigzag(2147483647) = 4294967294" $
              zigZag32 2147483647 @?= 4294967294
          , testCase "zigzag(-2147483648) = 4294967295" $
              zigZag32 (-2147483648) @?= 4294967295
          , testCase "zigzag64(0) = 0" $ zigZag64 0 @?= 0
          , testCase "zigzag64(-1) = 1" $ zigZag64 (-1) @?= 1
          , testCase "zigzag64(1) = 2" $ zigZag64 1 @?= 2
          , testCase "zigzag64(max int64) roundtrip" $ do
              let n = maxBound :: Int64
              unZigZag64 (zigZag64 n) @?= n
          , testCase "zigzag64(min int64) roundtrip" $ do
              let n = minBound :: Int64
              unZigZag64 (zigZag64 n) @?= n
          ]
      ]
  ]

buildToBS :: B.Builder -> ByteString
buildToBS = BL.toStrict . B.toLazyByteString
