module Test.StreamCodec (streamCodecTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Word (Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Proto.Decode (MessageDecode (..), decodeMessage)
import Proto.Decode.Stream (decodeMessageLazy, decodeMessageStream)
import Proto.Encode (MessageEncode (..), MessageSize (..), encodeMessage, encodeMessageSized)
import Proto.Encode.Lazy
  ( encodeMessageLazy
  , encodeMessageLazySized
  , encodeMessageStream
  , encodeMessageStreamSized
  )
import Proto.Wire (WireType (..))
import Proto.Wire.Decode (DecodeError (..), Decoder, getTagOr, getVarint, getText, runDecoder, skipField)
import Proto.Wire.Encode (fieldBoolSize, fieldTextSize, fieldVarintSize, putTag, putVarint, putText)

streamCodecTests :: TestTree
streamCodecTests = testGroup "Streaming & Lazy Codecs"
  [ lazyEncodeTests
  , lazyDecodeTests
  , streamRoundtripTests
  ]

-- -----------------------------------------------------------------------
-- Lazy single-message encoding
-- -----------------------------------------------------------------------

lazyEncodeTests :: TestTree
lazyEncodeTests = testGroup "Lazy encoding"
  [ testProperty "encodeMessageLazy matches strict" $ property $ do
      msg <- genSMsg
      BL.toStrict (encodeMessageLazy msg) === encodeMessage msg

  , testProperty "encodeMessageLazySized matches strict" $ property $ do
      msg <- genSMsg
      BL.toStrict (encodeMessageLazySized msg) === encodeMessageSized msg

  , testProperty "encodeMessageLazySized matches encodeMessageLazy" $ property $ do
      msg <- genSMsg
      encodeMessageLazySized msg === encodeMessageLazy msg
  ]

-- -----------------------------------------------------------------------
-- Lazy single-message decoding
-- -----------------------------------------------------------------------

lazyDecodeTests :: TestTree
lazyDecodeTests = testGroup "Lazy decoding"
  [ testProperty "decodeMessageLazy roundtrip" $ property $ do
      msg <- genSMsg
      let lbs = encodeMessageLazy msg
      decodeMessageLazy lbs === Right msg

  , testCase "decodeMessageLazy empty" $ do
      let lbs = BL.empty
      decodeMessageLazy lbs @?= Right (SMsg 0 "" False)

  , testCase "decodeMessageLazy multi-chunk" $ do
      let strict = encodeMessage (SMsg 42 "hello" True)
          (a, b) = BS.splitAt (BS.length strict `div` 2) strict
          lbs = BL.fromChunks [a, b]
      decodeMessageLazy lbs @?= Right (SMsg 42 "hello" True)
  ]

-- -----------------------------------------------------------------------
-- Stream framing roundtrip
-- -----------------------------------------------------------------------

streamRoundtripTests :: TestTree
streamRoundtripTests = testGroup "Stream framing roundtrip"
  [ testProperty "stream roundtrip (no size)" $ property $ do
      msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
      let encoded = encodeMessageStream msgs
          decoded = decodeMessageStream encoded
      fmap fromRight' decoded === msgs

  , testProperty "stream roundtrip (sized)" $ property $ do
      msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
      let encoded = encodeMessageStreamSized msgs
          decoded = decodeMessageStream encoded
      fmap fromRight' decoded === msgs

  , testProperty "sized stream matches non-sized stream" $ property $ do
      msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
      encodeMessageStreamSized msgs === encodeMessageStream msgs

  , testCase "empty stream" $ do
      let encoded = encodeMessageStream ([] :: [SMsg])
      BL.null encoded @?= True
      decodeMessageStream @SMsg encoded @?= []

  , testCase "single-message stream" $ do
      let msg = SMsg 99 "solo" True
          encoded = encodeMessageStream [msg]
          decoded = decodeMessageStream encoded
      decoded @?= [Right msg]

  , testCase "stream decode truncated length" $ do
      let lbs = BL.pack [0x80]
      case decodeMessageStream @SMsg lbs of
        [Left UnexpectedEnd] -> pure ()
        other -> assertFailure ("Expected [Left UnexpectedEnd], got: " <> show other)

  , testCase "stream decode truncated payload" $ do
      let lbs = BL.pack [0x0A, 0x01]
      case decodeMessageStream @SMsg lbs of
        [Left UnexpectedEnd] -> pure ()
        other -> assertFailure ("Expected [Left UnexpectedEnd], got: " <> show other)

  , testProperty "stream framing matches manual framing" $ property $ do
      msgs <- forAll $ Gen.list (Range.linear 1 10) genSMsg'
      let autoFramed = encodeMessageStream msgs
          manualFramed = BL.fromChunks $ do
            msg <- msgs
            let payload = encodeMessage msg
            pure $ buildToBS (putVarint (fromIntegral (BS.length payload)) <> B.byteString payload)
      autoFramed === BL.fromStrict (BL.toStrict manualFramed)
  ]

-- -----------------------------------------------------------------------
-- Test message type
-- -----------------------------------------------------------------------

data SMsg = SMsg
  { smValue  :: {-# UNPACK #-} !Word64
  , smName   :: !Text
  , smActive :: !Bool
  } deriving stock (Show, Eq)

instance MessageEncode SMsg where
  buildMessage msg =
    (if smValue msg /= 0 then putTag 1 WireVarint <> putVarint (smValue msg) else mempty) <>
    (if smName msg /= "" then putTag 2 WireLengthDelimited <> putText (smName msg) else mempty) <>
    (if smActive msg then putTag 3 WireVarint <> putVarint 1 else mempty)

instance MessageSize SMsg where
  messageSize msg =
    (if smValue msg /= 0 then fieldVarintSize 1 (smValue msg) else 0) +
    (if smName msg /= "" then fieldTextSize 2 (smName msg) else 0) +
    (if smActive msg then fieldBoolSize 3 else 0)

instance MessageDecode SMsg where
  messageDecoder = loop 0 "" False
    where
      loop :: Word64 -> Text -> Bool -> Decoder SMsg
      loop !val !name !active = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (SMsg val name active)
          Just (Proto.Wire.Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop v name active
            2 -> getText >>= \v -> loop val v active
            3 -> getVarint >>= \v -> loop val name (v /= 0)
            _ -> skipField wt >> loop val name active

-- -----------------------------------------------------------------------
-- Helpers
-- -----------------------------------------------------------------------

genSMsg :: PropertyT IO SMsg
genSMsg = do
  v <- forAll $ Gen.word64 (Range.linear 0 1000000)
  t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
  b <- forAll $ Gen.bool
  pure (SMsg v t b)

genSMsg' :: Gen SMsg
genSMsg' = SMsg
  <$> Gen.word64 (Range.linear 0 1000000)
  <*> Gen.text (Range.linear 0 100) Gen.alphaNum
  <*> Gen.bool

fromRight' :: Either DecodeError a -> a
fromRight' (Right a) = a
fromRight' (Left e) = error ("unexpected decode error: " <> show e)

buildToBS :: B.Builder -> BS.ByteString
buildToBS = BL.toStrict . B.toLazyByteString
