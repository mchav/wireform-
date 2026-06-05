module Test.Roundtrip (roundtripTests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto.Decode
import Proto.Encode
import Proto.Internal.SizedBuilder qualified as SB
import Proto.Internal.Wire (Tag (..), WireType (..))
import Proto.Internal.Wire.Decode
import Proto.Internal.Wire.Encode
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Builder qualified as B


roundtripTests :: Spec
roundtripTests =
  describe
    "Roundtrip Encoding/Decoding" $ sequence_
    [ describe
        "Hand-crafted message roundtrip" $ sequence_
        [ it "simple message encode/decode" $ do
            let encoded =
                  buildToBS $
                    putTag 1 WireVarint
                      <> putVarint 42
                      <> putTag 2 WireLengthDelimited
                      <> putText "hello"
                      <> putTag 3 WireVarint
                      <> putVarint 1

            case runDecoder decodeSimpleMsg encoded of
              Left e -> expectationFailure (show e)
              Right (val, name, active) -> do
                val `shouldBe` 42
                name `shouldBe` "hello"
                active `shouldBe` True
        , it "message with missing optional fields" $ do
            let encoded =
                  buildToBS $
                    putTag 1 WireVarint <> putVarint 99

            case runDecoder decodeSimpleMsgDefaults encoded of
              Left e -> expectationFailure (show e)
              Right (val, name, active) -> do
                val `shouldBe` 99
                name `shouldBe` ""
                active `shouldBe` False
        , it "message with unknown fields" $ do
            let encoded =
                  buildToBS $
                    putTag 1 WireVarint
                      <> putVarint 42
                      <> putTag 99 WireVarint
                      <> putVarint 999
                      <> putTag 2 WireLengthDelimited
                      <> putText "hello"
                      <> putTag 100 Wire32Bit
                      <> putFixed32 0
                      <> putTag 3 WireVarint
                      <> putVarint 1

            case runDecoder decodeSimpleMsg encoded of
              Left e -> expectationFailure (show e)
              Right (val, name, active) -> do
                val `shouldBe` 42
                name `shouldBe` "hello"
                active `shouldBe` True
        , it "message with repeated field" $ do
            let encoded =
                  buildToBS $
                    putTag 1 WireVarint
                      <> putVarint 1
                      <> putTag 1 WireVarint
                      <> putVarint 2
                      <> putTag 1 WireVarint
                      <> putVarint 3

            case runDecoder decodeRepeatedMsg encoded of
              Left e -> expectationFailure (show e)
              Right vals -> vals `shouldBe` [1, 2, 3]
        , it "packed repeated field" $ do
            let payload = buildToBS (putVarint 10 <> putVarint 20 <> putVarint 30)
                encoded =
                  buildToBS $
                    putTag 1 WireLengthDelimited <> putLengthDelimited payload

            case runDecoder decodePackedRepeatedMsg encoded of
              Left e -> expectationFailure (show e)
              Right vals -> VU.toList vals `shouldBe` [10, 20, 30]
        , it "nested message encode/decode" $ do
            let innerPayload =
                  buildToBS $
                    putTag 1 WireLengthDelimited <> putText "inner value"
                encoded =
                  buildToBS $
                    putTag 1 WireVarint
                      <> putVarint 1
                      <> putTag 2 WireLengthDelimited
                      <> putLengthDelimited innerPayload

            case runDecoder decodeNestedMsg encoded of
              Left e -> expectationFailure (show e)
              Right (outerVal, innerText) -> do
                outerVal `shouldBe` 1
                innerText `shouldBe` "inner value"
        ]
    , describe
        "Field type roundtrips" $ sequence_
        [ it "int32 field roundtrip" $ property $ do
            n <- forAll $ Gen.int32 Range.linearBounded
            let encoded = buildToBS $ putTag 1 WireVarint <> putVarintSigned (fromIntegral n)
            case runDecoder (getTag >> getVarintSigned) encoded of
              Left e -> do
                annotate (show e)
                failure
              Right v -> fromIntegral v === n
        , it "fixed32 field roundtrip" $ property $ do
            n <- forAll $ Gen.word32 Range.linearBounded
            let encoded = buildToBS $ putTag 1 Wire32Bit <> putFixed32 n
            case runDecoder (getTag >> getFixed32) encoded of
              Left e -> do
                annotate (show e)
                failure
              Right v -> v === n
        , it "string field roundtrip" $ property $ do
            t <- forAll $ Gen.text (Range.linear 0 500) Gen.unicode
            let encoded = buildToBS $ putTag 1 WireLengthDelimited <> putText t
            case runDecoder (getTag >> getText) encoded of
              Left e -> do
                annotate (show e)
                failure
              Right v -> v === t
        , it "bytes field roundtrip" $ property $ do
            bs <- forAll $ Gen.bytes (Range.linear 0 500)
            let encoded = buildToBS $ putTag 1 WireLengthDelimited <> putByteString bs
            case runDecoder (getTag >> getByteString) encoded of
              Left e -> do
                annotate (show e)
                failure
              Right v -> v === bs
        ]
    , describe
        "Multi-field message roundtrip" $ sequence_
        [ it "multi-field roundtrip" $ property $ do
            v1 <- forAll $ Gen.word64 (Range.linear 0 maxBound)
            v2 <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
            v3 <- forAll Gen.bool
            v4 <- forAll $ Gen.double (Range.linearFrac (-1e10) 1e10)

            let encoded =
                  buildToBS $
                    putTag 1 WireVarint
                      <> putVarint v1
                      <> putTag 2 WireLengthDelimited
                      <> putText v2
                      <> putTag 3 WireVarint
                      <> putVarint (if v3 then 1 else 0)
                      <> putTag 4 Wire64Bit
                      <> putDouble v4

            case runDecoder (decodeMultiField v1 v2 v3 v4) encoded of
              Left e -> do
                annotate (show e)
                failure
              Right () -> success
        ]
    , describe
        "Packed encoding helpers" $ sequence_
        [ it "packed varint roundtrip" $ property $ do
            vals <- forAll $ Gen.list (Range.linear 0 50) (Gen.word64 (Range.linear 0 maxBound))
            let vec = VU.fromList vals
                encoded = buildToBS (encodePackedVarint 1 vec)
            if VU.null vec
              then assert (BS.null encoded)
              else case runDecoder (getTag >> decodePackedVarint) encoded of
                Left e -> do
                  annotate (show e)
                  failure
                Right decoded -> VU.toList decoded === vals
        , it "packed fixed32 roundtrip" $ property $ do
            vals <- forAll $ Gen.list (Range.linear 0 50) (Gen.word32 Range.linearBounded)
            let vec = VU.fromList vals
                encoded = buildToBS (encodePackedFixed32 1 vec)
            if VU.null vec
              then assert (BS.null encoded)
              else case runDecoder (getTag >> decodePackedFixed32) encoded of
                Left e -> do
                  annotate (show e)
                  failure
                Right decoded -> VU.toList decoded === vals
        , it "packed double roundtrip" $ property $ do
            vals <- forAll $ Gen.list (Range.linear 0 50) (Gen.double (Range.linearFrac (-1e100) 1e100))
            let vec = VU.fromList vals
                encoded = buildToBS (encodePackedDouble 1 vec)
            if VU.null vec
              then assert (BS.null encoded)
              else case runDecoder (getTag >> decodePackedDouble) encoded of
                Left e -> do
                  annotate (show e)
                  failure
                Right decoded -> VU.toList decoded === vals
        , it "packed sint32 roundtrip" $ property $ do
            vals <- forAll $ Gen.list (Range.linear 0 50) (Gen.int32 Range.linearBounded)
            let vec = VU.fromList vals
                encoded = buildToBS (encodePackedSVarint32 1 vec)
            if VU.null vec
              then assert (BS.null encoded)
              else case runDecoder (getTag >> decodePackedSVarint32) encoded of
                Left e -> do
                  annotate (show e)
                  failure
                Right decoded -> VU.toList decoded === vals
        ]
    , describe
        "MessageEncode/MessageDecode typeclass roundtrip" $ sequence_
        [ it "TestMsg roundtrip" $ property $ do
            v <- forAll $ Gen.word64 (Range.linear 0 1000000)
            t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
            b <- forAll Gen.bool
            let msg = TestMsg v t b
                encoded = encodeMessage msg
            case decodeMessage encoded of
              Left e -> do
                annotate (show e)
                failure
              Right decoded -> decoded === msg
        , it "TestMsg with submessage roundtrip" $ property $ do
            outerVal <- forAll $ Gen.word64 (Range.linear 0 1000)
            innerVal <- forAll $ Gen.word64 (Range.linear 0 1000)
            innerName <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
            let inner = TestMsg innerVal innerName True
                outer = TestOuter outerVal (Just inner)
                encoded = encodeMessage outer
            case decodeMessage encoded of
              Left e -> do
                annotate (show e)
                failure
              Right decoded -> decoded === outer
        , it "TestMsg size calculation matches encoding" $ do
            let msg = TestMsg 42 "hello" True
                encoded = encodeMessage msg
                calculatedSize = messageSize msg
            BS.length encoded `shouldBe` calculatedSize
        , it "TestMsg size always matches" $ property $ do
            v <- forAll $ Gen.word64 (Range.linear 0 maxBound)
            t <- forAll $ Gen.text (Range.linear 0 200) Gen.alphaNum
            b <- forAll Gen.bool
            let msg = TestMsg v t b
                encoded = encodeMessage msg
            BS.length encoded === messageSize msg
        ]
    , describe
        "Lazy submessage decoding" $ sequence_
        [ it "lazy message captures bytes" $ do
            let inner = TestMsg 42 "lazy" True
                innerBS = encodeMessage inner
                encoded =
                  buildToBS $
                    putTag 1 WireVarint
                      <> putVarint 1
                      <> putTag 2 WireLengthDelimited
                      <> putLengthDelimited innerBS

            case runDecoder decodeLazyOuter encoded of
              Left e -> expectationFailure (show e)
              Right (outerVal, lazyInner) -> do
                outerVal `shouldBe` 1
                case forceLazyMessage lazyInner of
                  Left e -> expectationFailure (show e)
                  Right msg -> msg `shouldBe` TestMsg 42 "lazy" True
        ]
    , describe
        "Unknown field preservation" $ sequence_
        [ it "capture and re-encode unknown fields" $ do
            let encoded =
                  buildToBS $
                    putTag 1 WireVarint
                      <> putVarint 42
                      <> putTag 99 WireVarint
                      <> putVarint 999
                      <> putTag 100 Wire32Bit
                      <> putFixed32 0xDEADBEEF
                      <> putTag 101 WireLengthDelimited
                      <> putByteString "unknown data"
                      <> putTag 2 WireLengthDelimited
                      <> putText "hello"

            case runDecoder decodeWithUnknowns encoded of
              Left e -> expectationFailure (show e)
              Right (val, name, unknowns) -> do
                val `shouldBe` 42
                name `shouldBe` "hello"
                length unknowns `shouldBe` 3

                let reencoded =
                      buildToBS $
                        putTag 1 WireVarint
                          <> putVarint 42
                          <> putTag 2 WireLengthDelimited
                          <> putText "hello"
                          <> encodeUnknownFields unknowns
                BS.length reencoded > 0 `shouldBe` True
        ]
    , describe
        "Size calculation" $ sequence_
        [ it "varintSize correct" $ property $ do
            n <- forAll $ Gen.word64 (Range.linear 0 maxBound)
            let encoded = buildToBS (putVarint n)
            BS.length encoded === varintSize n
        , it "tagSize correct" $ property $ do
            fn <- forAll $ Gen.int (Range.linear 1 10000)
            let tagVal = fromIntegral fn * 8
                encoded = buildToBS (putVarint tagVal)
            BS.length encoded === tagSize fn
        ]
    , describe
        "SizedBuilder (fused size+builder)" $ sequence_
        [ it "sizedFieldVarint size matches" $ property $ do
            fn <- forAll $ Gen.int (Range.linear 1 100)
            val <- forAll $ Gen.word64 (Range.linear 0 maxBound)
            let sb = sizedFieldVarint fn val
                bs = SB.toByteString sb
            BS.length bs === SB.size sb
        , it "sizedFieldString size matches" $ property $ do
            fn <- forAll $ Gen.int (Range.linear 1 100)
            t <- forAll $ Gen.text (Range.linear 0 200) Gen.alphaNum
            let sb = sizedFieldString fn t
                bs = SB.toByteString sb
            BS.length bs === SB.size sb
        , it "sizedFieldMessage size matches" $ property $ do
            val <- forAll $ Gen.word64 (Range.linear 0 1000)
            name <- forAll $ Gen.text (Range.linear 0 50) Gen.alphaNum
            let innerSB = sizedFieldVarint 1 val <> sizedFieldString 2 name
                outerSB = sizedFieldMessage 1 innerSB
                bs = SB.toByteString outerSB
            BS.length bs === SB.size outerSB
        , it "sizedFieldBool size matches" $ property $ do
            fn <- forAll $ Gen.int (Range.linear 1 100)
            b <- forAll Gen.bool
            let sb = sizedFieldBool fn b
                bs = SB.toByteString sb
            BS.length bs === SB.size sb
        , it "sizedFieldDouble size matches" $ property $ do
            fn <- forAll $ Gen.int (Range.linear 1 100)
            d <- forAll $ Gen.double (Range.linearFrac (-1e100) 1e100)
            let sb = sizedFieldDouble fn d
                bs = SB.toByteString sb
            BS.length bs === SB.size sb
        ]
    ]


-- Test message type using the encode/decode typeclasses
data TestMsg = TestMsg
  { tmValue :: {-# UNPACK #-} !Word64
  , tmName :: !Text
  , tmActive :: !Bool
  }
  deriving stock (Show, Eq)


instance MessageEncode TestMsg where
  buildMessage msg =
    (if tmValue msg /= 0 then encodeFieldVarint 1 (tmValue msg) else mempty)
      <> (if tmName msg /= "" then encodeFieldString 2 (tmName msg) else mempty)
      <> (if tmActive msg then encodeFieldBool 3 True else mempty)


instance MessageSize TestMsg where
  messageSize msg =
    (if tmValue msg /= 0 then fieldVarintSize 1 (tmValue msg) else 0)
      + (if tmName msg /= "" then fieldTextSize 2 (tmName msg) else 0)
      + (if tmActive msg then fieldBoolSize 3 else 0)


instance MessageDecode TestMsg where
  messageDecoder = loop 0 "" False
    where
      loop !val !name !active = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (TestMsg val name active)
          Just (Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop v name active
            2 -> getText >>= \v -> loop val v active
            3 -> getVarint >>= \v -> loop val name (v /= 0)
            _ -> skipField wt >> loop val name active


data TestOuter = TestOuter
  { toValue :: {-# UNPACK #-} !Word64
  , toInner :: !(Maybe TestMsg)
  }
  deriving stock (Show, Eq)


instance MessageEncode TestOuter where
  buildMessage msg =
    (if toValue msg /= 0 then encodeFieldVarint 1 (toValue msg) else mempty)
      <> maybe mempty (encodeFieldMessageSized 2) (toInner msg)


instance MessageSize TestOuter where
  messageSize msg =
    (if toValue msg /= 0 then fieldVarintSize 1 (toValue msg) else 0)
      + maybe 0 (fieldMessageSize 2 . messageSize) (toInner msg)


instance MessageDecode TestOuter where
  messageDecoder = loop 0 Nothing
    where
      loop !val !inner = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (TestOuter val inner)
          Just (Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop v inner
            2 -> do
              msg <- decodeFieldMessage
              loop val (Just msg)
            _ -> skipField wt >> loop val inner


-- Decoder for lazy outer message
decodeLazyOuter :: Decoder (Word64, LazyMessage TestMsg)
decodeLazyOuter = loop 0 (LazyMessage BS.empty (Left UnexpectedEnd))
  where
    loop !val !inner = do
      mt <- getTagOr
      case mt of
        Nothing -> pure (val, inner)
        Just (Tag fn wt) -> case fn of
          1 -> getVarint >>= \v -> loop v inner
          2 -> do
            lm <- decodeFieldLazyMessage
            loop val lm
          _ -> skipField wt >> loop val inner


-- Hand-rolled decoders for basic tests

decodeSimpleMsg :: Decoder (Word64, Text, Bool)
decodeSimpleMsg = loop 0 "" False
  where
    loop !val !name !active = do
      mt <- getTagOr
      case mt of
        Nothing -> pure (val, name, active)
        Just (Tag fn wt) -> case fn of
          1 -> getVarint >>= \v -> loop v name active
          2 -> getText >>= \v -> loop val v active
          3 -> getVarint >>= \v -> loop val name (v /= 0)
          _ -> skipField wt >> loop val name active


decodeSimpleMsgDefaults :: Decoder (Word64, Text, Bool)
decodeSimpleMsgDefaults = decodeSimpleMsg


decodeRepeatedMsg :: Decoder [Word64]
decodeRepeatedMsg = loop []
  where
    loop !acc = do
      mt <- getTagOr
      case mt of
        Nothing -> pure (reverse acc)
        Just (Tag fn wt) -> case fn of
          1 -> getVarint >>= \v -> loop (v : acc)
          _ -> skipField wt >> loop acc


decodePackedRepeatedMsg :: Decoder (VU.Vector Word64)
decodePackedRepeatedMsg = do
  mt <- getTagOr
  case mt of
    Nothing -> pure VU.empty
    Just (Tag 1 WireLengthDelimited) -> decodePackedVarint
    Just (Tag _ wt) -> skipField wt >> decodePackedRepeatedMsg


decodeNestedMsg :: Decoder (Word64, Text)
decodeNestedMsg = loop 0 ""
  where
    loop !outerVal !innerText = do
      mt <- getTagOr
      case mt of
        Nothing -> pure (outerVal, innerText)
        Just (Tag fn wt) -> case fn of
          1 -> getVarint >>= \v -> loop v innerText
          2 -> do
            bs <- getLengthDelimited
            case runDecoder decodeInner bs of
              Left e -> decodeFail (SubMessageError e)
              Right t -> loop outerVal t
          _ -> skipField wt >> loop outerVal innerText

    decodeInner = loop' ""
      where
        loop' !t = do
          mt <- getTagOr
          case mt of
            Nothing -> pure t
            Just (Tag fn wt) -> case fn of
              1 -> getText >>= \v -> loop' v
              _ -> skipField wt >> loop' t


decodeMultiField :: Word64 -> Text -> Bool -> Double -> Decoder ()
decodeMultiField expV1 expV2 expV3 expV4 = do
  _tag1 <- getTag
  v1 <- getVarint
  checkEq v1 expV1 "v1 mismatch"
  _tag2 <- getTag
  v2 <- getText
  checkEq v2 expV2 "v2 mismatch"
  _tag3 <- getTag
  v3raw <- getVarint
  let v3 = v3raw /= 0
  checkEq v3 expV3 "v3 mismatch"
  _tag4 <- getTag
  v4 <- getDouble
  checkEq v4 expV4 "v4 mismatch"
  where
    checkEq :: Eq a => a -> a -> String -> Decoder ()
    checkEq actual expected msg =
      if actual == expected
        then pure ()
        else decodeFail (CustomError msg)


decodeWithUnknowns :: Decoder (Word64, Text, [UnknownField])
decodeWithUnknowns = loop 0 "" []
  where
    loop !val !name !unknowns = do
      mt <- getTagOr
      case mt of
        Nothing -> pure (val, name, reverse unknowns)
        Just (Tag fn wt) -> case fn of
          1 -> getVarint >>= \v -> loop v name unknowns
          2 -> getText >>= \v -> loop val v unknowns
          _ -> do
            uf <- captureUnknownField fn wt
            loop val name (uf : unknowns)


buildToBS :: B.Builder -> ByteString
buildToBS = BL.toStrict . B.toLazyByteString
