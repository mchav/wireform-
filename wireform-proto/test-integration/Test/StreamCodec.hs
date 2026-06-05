module Test.StreamCodec (streamCodecTests) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Word (Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto.Decode (MessageDecode (..))
import Proto.Decode.Stream (
  IDecode (..),
  decodeMessageIncremental,
  decodeMessageLazy,
  decodeMessageStream,
 )
import Proto.Encode (MessageEncode (..), MessageSize (..), encodeMessage, encodeMessageLazy, encodeMessageSized, encodeMessageStream, encodeMessageStreamSized)
import Proto.Internal.Wire (Tag (..), WireType (..))
import Proto.Internal.Wire.Decode (DecodeError (..), Decoder, getTagOr, getText, getVarint, skipField)
import Proto.Internal.Wire.Encode (fieldBoolSize, fieldTextSize, fieldVarintSize, putTag, putText, putVarint)
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Builder qualified as B


streamCodecTests :: Spec
streamCodecTests =
  describe
    "Streaming & Lazy Codecs" $ sequence_
    [ lazyEncodeTests
    , lazyDecodeTests
    , streamRoundtripTests
    , incrementalDecodeTests
    ]


-- -----------------------------------------------------------------------
-- Lazy single-message encoding
-- -----------------------------------------------------------------------

lazyEncodeTests :: Spec
lazyEncodeTests =
  describe
    "Lazy encoding" $ sequence_
    [ it "encodeMessageLazy matches strict" $ property $ do
        msg <- genSMsg
        BL.toStrict (encodeMessageLazy msg) === encodeMessage msg
    , it "encodeMessageLazy matches strict" $ property $ do
        msg <- genSMsg
        BL.toStrict (encodeMessageLazy msg) === encodeMessageSized msg
    , it "encodeMessageLazy matches encodeMessageLazy" $ property $ do
        msg <- genSMsg
        encodeMessageLazy msg === encodeMessageLazy msg
    ]


-- -----------------------------------------------------------------------
-- Lazy single-message decoding
-- -----------------------------------------------------------------------

lazyDecodeTests :: Spec
lazyDecodeTests =
  describe
    "Lazy decoding" $ sequence_
    [ it "decodeMessageLazy roundtrip" $ property $ do
        msg <- genSMsg
        let lbs = encodeMessageLazy msg
        decodeMessageLazy lbs === Right msg
    , it "decodeMessageLazy empty" $ do
        let lbs = BL.empty
        decodeMessageLazy lbs `shouldBe` Right (SMsg 0 "" False)
    , it "decodeMessageLazy multi-chunk" $ do
        let strict = encodeMessage (SMsg 42 "hello" True)
            (a, b) = BS.splitAt (BS.length strict `div` 2) strict
            lbs = BL.fromChunks [a, b]
        decodeMessageLazy lbs `shouldBe` Right (SMsg 42 "hello" True)
    ]


-- -----------------------------------------------------------------------
-- Stream framing roundtrip
-- -----------------------------------------------------------------------

streamRoundtripTests :: Spec
streamRoundtripTests =
  describe
    "Stream framing roundtrip" $ sequence_
    [ it "stream roundtrip (no size)" $ property $ do
        msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
        let encoded = encodeMessageStream msgs
            decoded = decodeMessageStream encoded
        fmap fromRight' decoded === msgs
    , it "stream roundtrip (sized)" $ property $ do
        msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
        let encoded = encodeMessageStreamSized msgs
            decoded = decodeMessageStream encoded
        fmap fromRight' decoded === msgs
    , it "sized stream matches non-sized stream" $ property $ do
        msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
        encodeMessageStreamSized msgs === encodeMessageStream msgs
    , it "empty stream" $ do
        let encoded = encodeMessageStream ([] :: [SMsg])
        BL.null encoded `shouldBe` True
        decodeMessageStream @SMsg encoded `shouldBe` []
    , it "single-message stream" $ do
        let msg = SMsg 99 "solo" True
            encoded = encodeMessageStream [msg]
            decoded = decodeMessageStream encoded
        decoded `shouldBe` [Right msg]
    , it "stream decode truncated length" $ do
        let lbs = BL.pack [0x80]
        case decodeMessageStream @SMsg lbs of
          [Left UnexpectedEnd] -> pure ()
          other -> expectationFailure ("Expected [Left UnexpectedEnd], got: " <> show other)
    , it "stream decode truncated payload" $ do
        let lbs = BL.pack [0x0A, 0x01]
        case decodeMessageStream @SMsg lbs of
          [Left UnexpectedEnd] -> pure ()
          other -> expectationFailure ("Expected [Left UnexpectedEnd], got: " <> show other)
    , it "stream framing matches manual framing" $ property $ do
        msgs <- forAll $ Gen.list (Range.linear 1 10) genSMsg'
        let autoFramed = encodeMessageStream msgs
            manualFramed = BL.fromChunks $ do
              msg <- msgs
              let payload = encodeMessage msg
              pure $ buildToBS (putVarint (fromIntegral (BS.length payload)) <> B.byteString payload)
        autoFramed === BL.fromStrict (BL.toStrict manualFramed)
    ]


-- -----------------------------------------------------------------------
-- Incremental decoder
-- -----------------------------------------------------------------------

incrementalDecodeTests :: Spec
incrementalDecodeTests =
  describe
    "Incremental decoder" $ sequence_
    [ it "single message fed all at once" $ property $ do
        msg <- genSMsg
        let framed = frameMessage msg
        case feed decodeMessageIncremental framed of
          IDone decoded leftover -> do
            decoded === msg
            BS.null leftover === True
          other -> do
            annotate (show other)
            failure
    , it "single message fed byte-by-byte" $ property $ do
        msg <- genSMsg
        let framed = frameMessage msg
            chunks = fmap BS.singleton (BS.unpack framed)
        case feedAll decodeMessageIncremental chunks of
          IDone decoded leftover -> do
            decoded === msg
            BS.null leftover === True
          other -> do
            annotate (show other)
            failure
    , it "preserves leftover bytes" $ property $ do
        msg <- genSMsg
        extra <- forAll $ Gen.bytes (Range.linear 1 50)
        let framed = frameMessage msg <> extra
        case feed decodeMessageIncremental framed of
          IDone decoded leftover -> do
            decoded === msg
            leftover === extra
          other -> do
            annotate (show other)
            failure
    , it "two messages fed together" $ property $ do
        msg1 <- genSMsg
        msg2 <- genSMsg
        let framed = frameMessage msg1 <> frameMessage msg2
        case feed decodeMessageIncremental framed of
          IDone decoded1 leftover1 -> do
            decoded1 === msg1
            case feed decodeMessageIncremental leftover1 of
              IDone decoded2 leftover2 -> do
                decoded2 === msg2
                BS.null leftover2 === True
              other -> do
                annotate ("second: " <> show other)
                failure
          other -> do
            annotate ("first: " <> show other)
            failure
    , it "split at arbitrary byte boundary" $ property $ do
        msg <- genSMsg
        let framed = frameMessage msg
        splitPos <- forAll $ Gen.int (Range.linear 0 (BS.length framed))
        let (chunk1, chunk2) = BS.splitAt splitPos framed
            dec0 = feed decodeMessageIncremental chunk1
        case dec0 of
          IDone decoded _ -> decoded === msg
          IPartial _ ->
            case feed dec0 chunk2 of
              IDone decoded leftover -> do
                decoded === msg
                BS.null leftover === True
              other -> do
                annotate (show other)
                failure
          other -> do
            annotate (show other)
            failure
    , it "empty message" $ do
        let framed = BS.singleton 0
        case feed decodeMessageIncremental framed of
          IDone decoded leftover -> do
            decoded `shouldBe` SMsg 0 "" False
            BS.null leftover `shouldBe` True
          other -> expectationFailure (show other)
    , it "Nothing at start yields IFail" $ do
        case decodeMessageIncremental @SMsg of
          IPartial k -> case k Nothing of
            IFail UnexpectedEnd _ -> pure ()
            other -> expectationFailure ("Expected IFail UnexpectedEnd, got: " <> show other)
          other -> expectationFailure ("Expected IPartial, got: " <> show other)
    , it "truncated varint" $ do
        let chunk = BS.pack [0x80, 0x80]
        case feed decodeMessageIncremental chunk of
          IPartial k -> case k Nothing of
            IFail UnexpectedEnd _ -> pure ()
            other -> expectationFailure ("Expected IFail UnexpectedEnd, got: " <> show (other :: IDecode SMsg))
          other -> expectationFailure ("Expected IPartial, got: " <> show (other :: IDecode SMsg))
    , it "truncated payload" $ do
        let chunk = BS.pack [0x0A, 0x01]
        case feed decodeMessageIncremental chunk of
          IPartial k -> case k Nothing of
            IFail UnexpectedEnd _ -> pure ()
            other -> expectationFailure ("Expected IFail, got: " <> show (other :: IDecode SMsg))
          other -> expectationFailure ("Expected IPartial, got: " <> show (other :: IDecode SMsg))
    ]


-- -----------------------------------------------------------------------
-- Incremental encoder
-- -----------------------------------------------------------------------

-- -----------------------------------------------------------------------
-- Test message type
-- -----------------------------------------------------------------------

data SMsg = SMsg
  { smValue :: {-# UNPACK #-} !Word64
  , smName :: !Text
  , smActive :: !Bool
  }
  deriving stock (Show, Eq)


instance MessageEncode SMsg where
  buildMessage msg =
    (if smValue msg /= 0 then putTag 1 WireVarint <> putVarint (smValue msg) else mempty)
      <> (if smName msg /= "" then putTag 2 WireLengthDelimited <> putText (smName msg) else mempty)
      <> (if smActive msg then putTag 3 WireVarint <> putVarint 1 else mempty)


instance MessageSize SMsg where
  messageSize msg =
    (if smValue msg /= 0 then fieldVarintSize 1 (smValue msg) else 0)
      + (if smName msg /= "" then fieldTextSize 2 (smName msg) else 0)
      + (if smActive msg then fieldBoolSize 3 else 0)


instance MessageDecode SMsg where
  messageDecoder = loop 0 "" False
    where
      loop :: Word64 -> Text -> Bool -> Decoder SMsg
      loop !val !name !active = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (SMsg val name active)
          Just (Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop v name active
            2 -> getText >>= \v -> loop val v active
            3 -> getVarint >>= \v -> loop val name (v /= 0)
            _ -> skipField wt >> loop val name active


genSMsg :: PropertyT IO SMsg
genSMsg = do
  v <- forAll $ Gen.word64 (Range.linear 0 1000000)
  t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
  b <- forAll Gen.bool
  pure (SMsg v t b)


genSMsg' :: Gen SMsg
genSMsg' =
  SMsg
    <$> Gen.word64 (Range.linear 0 1000000)
    <*> Gen.text (Range.linear 0 100) Gen.alphaNum
    <*> Gen.bool


fromRight' :: Either DecodeError a -> a
fromRight' (Right a) = a
fromRight' (Left e) = error ("unexpected decode error: " <> show e)


buildToBS :: B.Builder -> BS.ByteString
buildToBS = BL.toStrict . B.toLazyByteString


frameMessage :: (MessageEncode a) => a -> BS.ByteString
frameMessage msg =
  let payload = encodeMessage msg
  in buildToBS (putVarint (fromIntegral (BS.length payload)) <> B.byteString payload)


feed :: IDecode a -> BS.ByteString -> IDecode a
feed (IPartial k) bs = k (Just bs)
feed done _ = done


feedAll :: IDecode a -> [BS.ByteString] -> IDecode a
feedAll dec [] = case dec of
  IPartial k -> k Nothing
  other -> other
feedAll dec (c : cs) = case dec of
  IPartial k -> feedAll (k (Just c)) cs
  other -> other
