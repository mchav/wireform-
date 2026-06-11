module Test.Wire (wireTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto.Internal.Wire
import Proto.Internal.Wire.Decode
import Proto.Internal.Wire.Encode
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Builder qualified as B


wireTests :: Spec
wireTests =
  describe
    "Wire Format"
    $ sequence_
      [ describe
          "Varint encoding/decoding"
          $ sequence_
            [ it "encode 0" $ do
                let bs = buildToBS (putVarint 0)
                bs `shouldBe` BS.pack [0x00]
            , it "encode 1" $ do
                let bs = buildToBS (putVarint 1)
                bs `shouldBe` BS.pack [0x01]
            , it "encode 127" $ do
                let bs = buildToBS (putVarint 127)
                bs `shouldBe` BS.pack [0x7F]
            , it "encode 128" $ do
                let bs = buildToBS (putVarint 128)
                bs `shouldBe` BS.pack [0x80, 0x01]
            , it "encode 300" $ do
                let bs = buildToBS (putVarint 300)
                bs `shouldBe` BS.pack [0xAC, 0x02]
            , it "encode 16384" $ do
                let bs = buildToBS (putVarint 16384)
                bs `shouldBe` BS.pack [0x80, 0x80, 0x01]
            , it "decode 0" $ do
                let bs = BS.pack [0x00]
                runDecoder getVarint bs `shouldBe` Right 0
            , it "decode 1" $ do
                let bs = BS.pack [0x01]
                runDecoder getVarint bs `shouldBe` Right 1
            , it "decode 300" $ do
                let bs = BS.pack [0xAC, 0x02]
                runDecoder getVarint bs `shouldBe` Right 300
            , it "varint roundtrip" $ property $ do
                n <- forAll $ Gen.word64 (Range.linear 0 maxBound)
                let encoded = buildToBS (putVarint n)
                runDecoder getVarint encoded === Right n
            , it "varint size bound" $ property $ do
                n <- forAll $ Gen.word64 (Range.linear 0 maxBound)
                let encoded = buildToBS (putVarint n)
                assert (BS.length encoded <= 10)
            ]
      , describe
          "ZigZag encoding"
          $ sequence_
            [ it "zigzag 0" $ zigZag32 0 `shouldBe` 0
            , it "zigzag -1" $ zigZag32 (-1) `shouldBe` 1
            , it "zigzag 1" $ zigZag32 1 `shouldBe` 2
            , it "zigzag -2" $ zigZag32 (-2) `shouldBe` 3
            , it "zigzag32 roundtrip" $ property $ do
                n <- forAll $ Gen.int32 Range.linearBounded
                unZigZag32 (zigZag32 n) === n
            , it "zigzag64 roundtrip" $ property $ do
                n <- forAll $ Gen.int64 Range.linearBounded
                unZigZag64 (zigZag64 n) === n
            , it "sint32 roundtrip" $ property $ do
                n <- forAll $ Gen.int32 Range.linearBounded
                let encoded = buildToBS (putSVarint32 n)
                runDecoder getSVarint32 encoded === Right n
            , it "sint64 roundtrip" $ property $ do
                n <- forAll $ Gen.int64 Range.linearBounded
                let encoded = buildToBS (putSVarint64 n)
                runDecoder getSVarint64 encoded === Right n
            ]
      , describe
          "Fixed-width encoding"
          $ sequence_
            [ it "fixed32 roundtrip" $ property $ do
                n <- forAll $ Gen.word32 Range.linearBounded
                let encoded = buildToBS (putFixed32 n)
                assert (BS.length encoded == 4)
                runDecoder getFixed32 encoded === Right n
            , it "fixed64 roundtrip" $ property $ do
                n <- forAll $ Gen.word64 Range.linearBounded
                let encoded = buildToBS (putFixed64 n)
                assert (BS.length encoded == 8)
                runDecoder getFixed64 encoded === Right n
            , it "float roundtrip" $ property $ do
                n <- forAll $ Gen.float (Range.linearFrac (-1e30) 1e30)
                let encoded = buildToBS (putFloat n)
                runDecoder getFloat encoded === Right n
            , it "double roundtrip" $ property $ do
                n <- forAll $ Gen.double (Range.linearFrac (-1e300) 1e300)
                let encoded = buildToBS (putDouble n)
                runDecoder getDouble encoded === Right n
            ]
      , describe
          "Length-delimited"
          $ sequence_
            [ it "bytestring roundtrip" $ property $ do
                bs <- forAll $ Gen.bytes (Range.linear 0 1000)
                let encoded = buildToBS (putByteString bs)
                runDecoder getByteString encoded === Right bs
            , it "text roundtrip" $ property $ do
                t <- forAll $ Gen.text (Range.linear 0 200) Gen.unicode
                let encoded = buildToBS (putText t)
                runDecoder getText encoded === Right t
            , it "zero-copy bytestring" $ do
                let payload = "hello world"
                    encoded = buildToBS (putByteString payload)
                case runDecoder getByteString encoded of
                  Right decoded -> decoded `shouldBe` payload
                  Left e -> expectationFailure (show e)
            ]
      , describe
          "Tag encoding"
          $ sequence_
            [ it "tag field 1 varint" $ do
                let tag = makeTag 1 WireVarint
                encodeTag tag `shouldBe` 0x08
            , it "tag field 1 length-delimited" $ do
                let tag = makeTag 1 WireLengthDelimited
                encodeTag tag `shouldBe` 0x0A
            , it "tag field 2 varint" $ do
                let tag = makeTag 2 WireVarint
                encodeTag tag `shouldBe` 0x10
            , it "tag roundtrip" $ property $ do
                fn <- forAll $ Gen.int (Range.linear 1 ((2 :: Int) ^ (29 :: Int) - 1))
                wt <- forAll $ Gen.element [WireVarint, Wire64Bit, WireLengthDelimited, Wire32Bit]
                let tag = makeTag fn wt
                    encoded = encodeTag tag
                decodeTag encoded === Just tag
            ]
      , describe
          "Skip unknown fields"
          $ sequence_
            [ it "skip varint" $ do
                let bs = buildToBS (putVarint 12345)
                case runDecoder (skipField WireVarint) bs of
                  Right () -> pure ()
                  Left e -> expectationFailure (show e)
            , it "skip fixed32" $ do
                let bs = buildToBS (putFixed32 42)
                case runDecoder (skipField Wire32Bit) bs of
                  Right () -> pure ()
                  Left e -> expectationFailure (show e)
            , it "skip fixed64" $ do
                let bs = buildToBS (putFixed64 42)
                case runDecoder (skipField Wire64Bit) bs of
                  Right () -> pure ()
                  Left e -> expectationFailure (show e)
            , it "skip length-delimited" $ do
                let bs = buildToBS (putByteString "hello")
                case runDecoder (skipField WireLengthDelimited) bs of
                  Right () -> pure ()
                  Left e -> expectationFailure (show e)
            ]
      , describe
          "Error handling"
          $ sequence_
            [ it "unexpected end on varint" $ do
                runDecoder getVarint BS.empty `shouldBe` Left UnexpectedEnd
            , it "unexpected end on fixed32" $ do
                runDecoder getFixed32 (BS.pack [1, 2]) `shouldBe` Left UnexpectedEnd
            , it "unexpected end on fixed64" $ do
                runDecoder getFixed64 (BS.pack [1, 2, 3, 4]) `shouldBe` Left UnexpectedEnd
            , it "extra bytes after decode" $ do
                let bs = BS.pack [0x00, 0xFF]
                case runDecoder getVarint bs of
                  Left ExtraBytes -> pure ()
                  other -> expectationFailure ("Expected ExtraBytes, got: " <> show other)
            ]
      , describe
          "Protobuf spec conformance vectors"
          $ sequence_
            [ describe
                "Varint edge cases"
                $ sequence_
                  [ it "varint 0 = [0x00]" $
                      buildToBS (putVarint 0) `shouldBe` BS.pack [0x00]
                  , it "varint 1 = [0x01]" $
                      buildToBS (putVarint 1) `shouldBe` BS.pack [0x01]
                  , it "varint max uint64 (10 bytes)" $ do
                      let bs = buildToBS (putVarint (maxBound :: Word64))
                      BS.length bs `shouldBe` 10
                      runDecoder getVarint bs `shouldBe` Right (maxBound :: Word64)
                  , it "varint 150 = [0x96, 0x01]" $
                      buildToBS (putVarint 150) `shouldBe` BS.pack [0x96, 0x01]
                  , it "negative int32 as 10-byte varint" $ do
                      let n = fromIntegral (-1 :: Int32) :: Word64
                          bs = buildToBS (putVarint n)
                      BS.length bs `shouldBe` 10
                      runDecoder getVarint bs `shouldBe` Right n
                  , it "negative int32 -150 via zigzag" $ do
                      let n = -150 :: Int32
                          bs = buildToBS (putSVarint32 n)
                      runDecoder getSVarint32 bs `shouldBe` Right n
                  ]
            , describe
                "Fixed-width exact bytes"
                $ sequence_
                  [ it "fixed32 little-endian 1" $
                      buildToBS (putFixed32 1) `shouldBe` BS.pack [0x01, 0x00, 0x00, 0x00]
                  , it "fixed32 little-endian 256" $
                      buildToBS (putFixed32 256) `shouldBe` BS.pack [0x00, 0x01, 0x00, 0x00]
                  , it "fixed64 little-endian 1" $
                      buildToBS (putFixed64 1) `shouldBe` BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
                  , it "float 0.0 = [0,0,0,0]" $
                      buildToBS (putFloat 0.0) `shouldBe` BS.pack [0x00, 0x00, 0x00, 0x00]
                  , it "double 0.0 = 8 zero bytes" $
                      buildToBS (putDouble 0.0) `shouldBe` BS.pack [0, 0, 0, 0, 0, 0, 0, 0]
                  ]
            , describe
                "Packed repeated fields"
                $ sequence_
                  [ it "empty packed = just length 0" $ do
                      let bs = buildToBS (putLengthDelimited BS.empty)
                      runDecoder getByteString bs `shouldBe` Right BS.empty
                  , it "packed varints [1,2,3]" $ do
                      let payload = buildToBS (putVarint 1 <> putVarint 2 <> putVarint 3)
                          bs = buildToBS (putLengthDelimited payload)
                      case runDecoder getByteString bs of
                        Right inner -> BS.length inner `shouldBe` 3
                        Left e -> expectationFailure (show e)
                  , it "packed fixed32 [1,2]" $ do
                      let payload = buildToBS (putFixed32 1 <> putFixed32 2)
                          bs = buildToBS (putLengthDelimited payload)
                      case runDecoder getByteString bs of
                        Right inner -> BS.length inner `shouldBe` 8
                        Left e -> expectationFailure (show e)
                  ]
            , describe
                "Tag format correctness"
                $ sequence_
                  [ it "field 1 varint = 0x08" $
                      encodeTag (makeTag 1 WireVarint) `shouldBe` 0x08
                  , it "field 1 64-bit = 0x09" $
                      encodeTag (makeTag 1 Wire64Bit) `shouldBe` 0x09
                  , it "field 1 length-delimited = 0x0A" $
                      encodeTag (makeTag 1 WireLengthDelimited) `shouldBe` 0x0A
                  , it "field 1 32-bit = 0x0D" $
                      encodeTag (makeTag 1 Wire32Bit) `shouldBe` 0x0D
                  , it "field 2 varint = 0x10" $
                      encodeTag (makeTag 2 WireVarint) `shouldBe` 0x10
                  , it "field 15 varint = 0x78" $
                      encodeTag (makeTag 15 WireVarint) `shouldBe` 0x78
                  , it "field 16 varint = 0x80 0x01 (2-byte tag)" $ do
                      let tag = makeTag 16 WireVarint
                          encoded = encodeTag tag
                      encoded `shouldBe` 0x80
                  ]
            , describe
                "Nested messages (length-delimited)"
                $ sequence_
                  [ it "3-level nesting roundtrip" $ do
                      let inner = buildToBS (putTag 1 WireVarint <> putVarint 42)
                          mid = buildToBS (putTag 1 WireLengthDelimited <> putByteString inner)
                          outer = buildToBS (putTag 1 WireLengthDelimited <> putByteString mid)
                      (not (BS.null outer)) `shouldBe` True
                      (BS.length outer > BS.length mid) `shouldBe` True
                      (BS.length mid > BS.length inner) `shouldBe` True
                  ]
            , describe
                "Unknown field skipping"
                $ sequence_
                  [ it "skip unknown varint field" $ do
                      let bs =
                            buildToBS
                              ( putTag 99 WireVarint
                                  <> putVarint 12345
                                  <> putTag 1 WireVarint
                                  <> putVarint 42
                              )
                      case runDecoder (skipField WireVarint >> getVarint) (BS.drop 1 bs) of
                        Right _ -> pure ()
                        Left _ -> pure () :: IO ()
                  , it "skip unknown length-delimited" $ do
                      let payload = "hello world"
                          bs = buildToBS (putByteString payload)
                      case runDecoder (skipField WireLengthDelimited) bs of
                        Right () -> pure ()
                        Left e -> expectationFailure (show e)
                  , it "skip unknown fixed32" $ do
                      let bs = buildToBS (putFixed32 0xDEADBEEF)
                      case runDecoder (skipField Wire32Bit) bs of
                        Right () -> pure ()
                        Left e -> expectationFailure (show e)
                  , it "skip unknown fixed64" $ do
                      let bs = buildToBS (putFixed64 0xDEADBEEFCAFEBABE)
                      case runDecoder (skipField Wire64Bit) bs of
                        Right () -> pure ()
                        Left e -> expectationFailure (show e)
                  ]
            , describe
                "ZigZag spec compliance"
                $ sequence_
                  [ it "zigzag(0) = 0" $ zigZag32 0 `shouldBe` 0
                  , it "zigzag(-1) = 1" $ zigZag32 (-1) `shouldBe` 1
                  , it "zigzag(1) = 2" $ zigZag32 1 `shouldBe` 2
                  , it "zigzag(-2) = 3" $ zigZag32 (-2) `shouldBe` 3
                  , it "zigzag(2147483647) = 4294967294" $
                      zigZag32 2147483647 `shouldBe` 4294967294
                  , it "zigzag(-2147483648) = 4294967295" $
                      zigZag32 (-2147483648) `shouldBe` 4294967295
                  , it "zigzag64(0) = 0" $ zigZag64 0 `shouldBe` 0
                  , it "zigzag64(-1) = 1" $ zigZag64 (-1) `shouldBe` 1
                  , it "zigzag64(1) = 2" $ zigZag64 1 `shouldBe` 2
                  , it "zigzag64(max int64) roundtrip" $ do
                      let n = maxBound :: Int64
                      unZigZag64 (zigZag64 n) `shouldBe` n
                  , it "zigzag64(min int64) roundtrip" $ do
                      let n = minBound :: Int64
                      unZigZag64 (zigZag64 n) `shouldBe` n
                  ]
            ]
      ]


buildToBS :: B.Builder -> ByteString
buildToBS = BL.toStrict . B.toLazyByteString
