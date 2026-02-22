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

import Proto.Decode (MessageDecode (..))
import Proto.Decode.Stream
  ( IDecode (..)
  , decodeMessageIncremental
  , decodeMessageLazy
  , decodeMessageStream
  )
import Proto.Encode (MessageEncode (..), MessageSize (..), encodeMessage, encodeMessageSized)
import Proto.Encode.Lazy
  ( IEncode (..)
  , encodeMessageLazy
  , encodeMessageLazySized
  , encodeMessageStream
  , encodeMessageStreamSized
  , newStreamEncoder
  , newStreamEncoderSized
  )
import Proto.Wire (Tag (..), WireType (..))
import Proto.Wire.Decode (DecodeError (..), Decoder, getTagOr, getVarint, getText, skipField)
import Proto.Wire.Encode (fieldBoolSize, fieldTextSize, fieldVarintSize, putTag, putVarint, putText)

streamCodecTests :: TestTree
streamCodecTests = testGroup "Streaming & Lazy Codecs"
  [ lazyEncodeTests
  , lazyDecodeTests
  , streamRoundtripTests
  , incrementalDecodeTests
  , incrementalEncodeTests
  , incrementalRoundtripTests
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
-- Incremental decoder
-- -----------------------------------------------------------------------

incrementalDecodeTests :: TestTree
incrementalDecodeTests = testGroup "Incremental decoder"
  [ testProperty "single message fed all at once" $ property $ do
      msg <- genSMsg
      let framed = frameMessage msg
      case feed decodeMessageIncremental framed of
        IDone decoded leftover -> do
          decoded === msg
          BS.null leftover === True
        other -> do
          annotate (show other)
          failure

  , testProperty "single message fed byte-by-byte" $ property $ do
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

  , testProperty "preserves leftover bytes" $ property $ do
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

  , testProperty "two messages fed together" $ property $ do
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

  , testProperty "split at arbitrary byte boundary" $ property $ do
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

  , testCase "empty message" $ do
      let framed = BS.singleton 0
      case feed decodeMessageIncremental framed of
        IDone decoded leftover -> do
          decoded @?= SMsg 0 "" False
          BS.null leftover @?= True
        other -> assertFailure (show other)

  , testCase "Nothing at start yields IFail" $ do
      case decodeMessageIncremental @SMsg of
        IPartial k -> case k Nothing of
          IFail UnexpectedEnd _ -> pure ()
          other -> assertFailure ("Expected IFail UnexpectedEnd, got: " <> show other)
        other -> assertFailure ("Expected IPartial, got: " <> show other)

  , testCase "truncated varint" $ do
      let chunk = BS.pack [0x80, 0x80]
      case feed decodeMessageIncremental chunk of
        IPartial k -> case k Nothing of
          IFail UnexpectedEnd _ -> pure ()
          other -> assertFailure ("Expected IFail UnexpectedEnd, got: " <> show (other :: IDecode SMsg))
        other -> assertFailure ("Expected IPartial, got: " <> show (other :: IDecode SMsg))

  , testCase "truncated payload" $ do
      let chunk = BS.pack [0x0A, 0x01]
      case feed decodeMessageIncremental chunk of
        IPartial k -> case k Nothing of
          IFail UnexpectedEnd _ -> pure ()
          other -> assertFailure ("Expected IFail, got: " <> show (other :: IDecode SMsg))
        other -> assertFailure ("Expected IPartial, got: " <> show (other :: IDecode SMsg))
  ]

-- -----------------------------------------------------------------------
-- Incremental encoder
-- -----------------------------------------------------------------------

incrementalEncodeTests :: TestTree
incrementalEncodeTests = testGroup "Incremental encoder"
  [ testProperty "single message produces correct frame" $ property $ do
      msg <- genSMsg
      let expected = frameMessage msg
      case newStreamEncoder of
        IEncReady f -> case f (Just msg) of
          IEncChunk bs next -> do
            bs === expected
            case next of
              IEncReady _ -> success
              other -> do
                annotate ("Expected IEncReady, got: " <> show other)
                failure
          other -> do
            annotate ("Expected IEncChunk, got: " <> show other)
            failure
        other -> do
          annotate ("Expected IEncReady, got: " <> show other)
          failure

  , testProperty "sized encoder matches non-sized" $ property $ do
      msg <- genSMsg
      let plain = stepEncoder newStreamEncoder (Just msg)
          sized = stepEncoder newStreamEncoderSized (Just msg)
      case (plain, sized) of
        (IEncChunk bs1 _, IEncChunk bs2 _) -> bs1 === bs2
        other -> do
          annotate (show other)
          failure

  , testCase "Nothing produces IEncDone" $ do
      case newStreamEncoder @SMsg of
        IEncReady f -> case f Nothing of
          IEncDone -> pure ()
          other -> assertFailure ("Expected IEncDone, got: " <> show other)
        other -> assertFailure ("Expected IEncReady, got: " <> show other)

  , testProperty "multiple messages" $ property $ do
      msgs <- forAll $ Gen.list (Range.linear 1 15) genSMsg'
      let chunks = collectEncoder newStreamEncoder msgs
          expected = BL.toStrict (encodeMessageStream msgs)
      BS.concat chunks === expected
  ]

-- -----------------------------------------------------------------------
-- Incremental roundtrip (encode → decode)
-- -----------------------------------------------------------------------

incrementalRoundtripTests :: TestTree
incrementalRoundtripTests = testGroup "Incremental encode-decode roundtrip"
  [ testProperty "stream via incremental encoder → incremental decoder" $ property $ do
      msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
      let encoded = BS.concat (collectEncoder newStreamEncoder msgs)
          decoded = decodeAllIncremental encoded
      decoded === fmap Right msgs

  , testProperty "stream via incremental encoder (sized) → incremental decoder" $ property $ do
      msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
      let encoded = BS.concat (collectEncoder newStreamEncoderSized msgs)
          decoded = decodeAllIncremental encoded
      decoded === fmap Right msgs

  , testProperty "incremental encoder matches lazy stream encoder" $ property $ do
      msgs <- forAll $ Gen.list (Range.linear 0 20) genSMsg'
      let fromIncremental = BS.concat (collectEncoder newStreamEncoder msgs)
          fromLazy = BL.toStrict (encodeMessageStream msgs)
      fromIncremental === fromLazy
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
          Just (Tag fn wt) -> case fn of
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

-- | Frame a message with a varint length prefix (strict).
frameMessage :: (MessageEncode a) => a -> BS.ByteString
frameMessage msg =
  let payload = encodeMessage msg
  in buildToBS (putVarint (fromIntegral (BS.length payload)) <> B.byteString payload)

-- | Feed a single chunk to an incremental decoder.
feed :: IDecode a -> BS.ByteString -> IDecode a
feed (IPartial k) bs = k (Just bs)
feed done _           = done

-- | Feed a list of chunks, then signal EOF.
feedAll :: IDecode a -> [BS.ByteString] -> IDecode a
feedAll dec [] = case dec of
  IPartial k -> k Nothing
  other      -> other
feedAll dec (c : cs) = case dec of
  IPartial k -> feedAll (k (Just c)) cs
  other      -> other

-- | Step an encoder with one input.
stepEncoder :: IEncode a -> Maybe a -> IEncode a
stepEncoder (IEncReady f) ma = f ma
stepEncoder other _           = other

-- | Collect all output chunks from an incremental encoder fed a list of messages.
collectEncoder :: IEncode a -> [a] -> [BS.ByteString]
collectEncoder enc msgs = go enc msgs
  where
    go (IEncReady f) (m : ms) = drainChunks (f (Just m)) ms
    go (IEncReady f) []       = drainChunks (f Nothing) []
    go (IEncChunk bs k) ms    = bs : go k ms
    go IEncDone _             = []

    drainChunks (IEncChunk bs k) ms = bs : drainChunks k ms
    drainChunks other ms            = go other ms

-- | Decode all messages from a strict ByteString using the incremental decoder.
decodeAllIncremental :: MessageDecode a => BS.ByteString -> [Either DecodeError a]
decodeAllIncremental bs
  | BS.null bs = []
  | otherwise = case feed decodeMessageIncremental bs of
      IDone a leftover -> Right a : decodeAllIncremental leftover
      IFail e _        -> [Left e]
      IPartial k       -> case k Nothing of
        IDone a leftover -> Right a : decodeAllIncremental leftover
        IFail e _        -> [Left e]
        IPartial _       -> [Left (CustomError "unexpected IPartial after EOF")]
