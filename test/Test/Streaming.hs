module Test.Streaming (streamingTests) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word64)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified MsgPack.Value as MV
import MsgPack.Encode (encode)
import qualified MsgPack.Stream as MS

import qualified CBOR.Value as C
import CBOR.Encode (encode)
import qualified CBOR.Stream as CS

streamingTests :: TestTree
streamingTests = testGroup "Streaming Decode"
  [ msgpackStreamTests
  , cborStreamTests
  ]

------------------------------------------------------------------------
-- MsgPack streaming
------------------------------------------------------------------------

msgpackStreamTests :: TestTree
msgpackStreamTests = testGroup "MsgPack Stream"
  [ testProperty "single value all-at-once" $ property $ do
      val <- forAll genMsgPackValue
      let bs = MsgPack.Encode.encode val
      case MS.streamDecode bs of
        MS.Done decoded leftover -> do
          decoded === val
          BS.null leftover === True
        MS.Partial _ -> do
          annotate "unexpected Partial"
          failure
        MS.Fail e -> do
          annotate e
          failure

  , testProperty "single value byte-by-byte" $ property $ do
      val <- forAll genMsgPackValue
      let bs = MsgPack.Encode.encode val
          chunks = map BS.singleton (BS.unpack bs)
      case feedAll (MS.streamDecode BS.empty) chunks of
        MS.Done decoded leftover -> do
          decoded === val
          BS.null leftover === True
        MS.Fail e -> do
          annotate e
          failure
        MS.Partial _ -> do
          annotate "still partial after all bytes"
          failure

  , testProperty "preserves leftover" $ property $ do
      val <- forAll genMsgPackValue
      extra <- forAll $ Gen.bytes (Range.linear 1 20)
      let bs = MsgPack.Encode.encode val <> extra
      case MS.streamDecode bs of
        MS.Done decoded leftover -> do
          decoded === val
          leftover === extra
        other -> do
          annotate (show other)
          failure

  , testProperty "two values concatenated" $ property $ do
      v1 <- forAll genMsgPackValue
      v2 <- forAll genMsgPackValue
      let bs = MsgPack.Encode.encode v1 <> MsgPack.Encode.encode v2
      case MS.streamDecode bs of
        MS.Done d1 rest1 -> do
          d1 === v1
          case MS.streamDecode rest1 of
            MS.Done d2 rest2 -> do
              d2 === v2
              BS.null rest2 === True
            other -> do
              annotate ("second: " ++ show other)
              failure
        other -> do
          annotate ("first: " ++ show other)
          failure

  , testCase "empty input returns Partial" $
      case MS.streamDecode BS.empty of
        MS.Partial _ -> pure ()
        other -> assertFailure $ "expected Partial, got " ++ show other

  , testCase "Partial then empty fails" $
      case MS.streamDecode BS.empty of
        MS.Partial k -> case k BS.empty of
          MS.Fail _ -> pure ()
          other -> assertFailure $ "expected Fail, got " ++ show other
        other -> assertFailure $ "expected Partial, got " ++ show other

  , testProperty "split at arbitrary boundary" $ property $ do
      val <- forAll genMsgPackValue
      let bs = MsgPack.Encode.encode val
      splitPos <- forAll $ Gen.int (Range.linear 0 (BS.length bs))
      let (chunk1, chunk2) = BS.splitAt splitPos bs
      case MS.streamDecode chunk1 of
        MS.Done decoded _ -> decoded === val
        MS.Partial k -> case k chunk2 of
          MS.Done decoded leftover -> do
            decoded === val
            BS.null leftover === True
          MS.Partial _ -> do
            annotate "still partial after feeding rest"
            failure
          MS.Fail e -> do
            annotate e
            failure
        MS.Fail e -> do
          annotate e
          failure
  ]

------------------------------------------------------------------------
-- CBOR streaming
------------------------------------------------------------------------

cborStreamTests :: TestTree
cborStreamTests = testGroup "CBOR Stream"
  [ testProperty "single value all-at-once" $ property $ do
      val <- forAll genCBORValue
      let bs = CBOR.Encode.encode val
      case CS.streamDecode bs of
        CS.Done decoded leftover -> do
          decoded === val
          BS.null leftover === True
        CS.Partial _ -> do
          annotate "unexpected Partial"
          failure
        CS.Fail e -> do
          annotate e
          failure

  , testProperty "single value byte-by-byte" $ property $ do
      val <- forAll genCBORValue
      let bs = CBOR.Encode.encode val
          chunks = map BS.singleton (BS.unpack bs)
      case feedAllCBOR (CS.streamDecode BS.empty) chunks of
        CS.Done decoded leftover -> do
          decoded === val
          BS.null leftover === True
        CS.Fail e -> do
          annotate e
          failure
        CS.Partial _ -> do
          annotate "still partial after all bytes"
          failure

  , testProperty "preserves leftover" $ property $ do
      val <- forAll genCBORValue
      extra <- forAll $ Gen.bytes (Range.linear 1 20)
      let bs = CBOR.Encode.encode val <> extra
      case CS.streamDecode bs of
        CS.Done decoded leftover -> do
          decoded === val
          leftover === extra
        other -> do
          annotate (show other)
          failure

  , testProperty "two values concatenated" $ property $ do
      v1 <- forAll genCBORValue
      v2 <- forAll genCBORValue
      let bs = CBOR.Encode.encode v1 <> CBOR.Encode.encode v2
      case CS.streamDecode bs of
        CS.Done d1 rest1 -> do
          d1 === v1
          case CS.streamDecode rest1 of
            CS.Done d2 rest2 -> do
              d2 === v2
              BS.null rest2 === True
            other -> do
              annotate ("second: " ++ show other)
              failure
        other -> do
          annotate ("first: " ++ show other)
          failure

  , testCase "empty input returns Partial" $
      case CS.streamDecode BS.empty of
        CS.Partial _ -> pure ()
        other -> assertFailure $ "expected Partial, got " ++ show other

  , testCase "Partial then empty fails" $
      case CS.streamDecode BS.empty of
        CS.Partial k -> case k BS.empty of
          CS.Fail _ -> pure ()
          other -> assertFailure $ "expected Fail, got " ++ show other
        other -> assertFailure $ "expected Partial, got " ++ show other

  , testProperty "split at arbitrary boundary" $ property $ do
      val <- forAll genCBORValue
      let bs = CBOR.Encode.encode val
      splitPos <- forAll $ Gen.int (Range.linear 0 (BS.length bs))
      let (chunk1, chunk2) = BS.splitAt splitPos bs
      case CS.streamDecode chunk1 of
        CS.Done decoded _ -> decoded === val
        CS.Partial k -> case k chunk2 of
          CS.Done decoded leftover -> do
            decoded === val
            BS.null leftover === True
          CS.Partial _ -> do
            annotate "still partial after feeding rest"
            failure
          CS.Fail e -> do
            annotate e
            failure
        CS.Fail e -> do
          annotate e
          failure
  ]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

feedAll :: MS.DecodeStep a -> [BS.ByteString] -> MS.DecodeStep a
feedAll step [] = case step of
  MS.Partial k -> k BS.empty
  other -> other
feedAll step (c:cs) = case step of
  MS.Partial k -> feedAll (k c) cs
  other -> other

feedAllCBOR :: CS.DecodeStep a -> [BS.ByteString] -> CS.DecodeStep a
feedAllCBOR step [] = case step of
  CS.Partial k -> k BS.empty
  other -> other
feedAllCBOR step (c:cs) = case step of
  CS.Partial k -> feedAllCBOR (k c) cs
  other -> other

genMsgPackValue :: Gen MV.Value
genMsgPackValue = Gen.choice
  [ pure MV.Nil
  , MV.Bool <$> Gen.bool
  , MV.Word <$> Gen.word64 (Range.linear 0 0xFFFF)
  , MV.Int <$> Gen.int64 (Range.linear (-1000) (-1))
  , MV.String <$> Gen.text (Range.linear 0 50) Gen.alphaNum
  , MV.Binary <$> Gen.bytes (Range.linear 0 50)
  , MV.Array <$> (V.fromList <$> Gen.list (Range.linear 0 5) genMsgPackLeaf)
  , MV.Map <$> (V.fromList <$> Gen.list (Range.linear 0 5) genMsgPackPair)
  ]

genMsgPackLeaf :: Gen MV.Value
genMsgPackLeaf = Gen.choice
  [ pure MV.Nil
  , MV.Bool <$> Gen.bool
  , MV.Word <$> Gen.word64 (Range.linear 0 0xFF)
  , MV.Int <$> Gen.int64 (Range.linear (-100) (-1))
  , MV.String <$> Gen.text (Range.linear 0 20) Gen.alphaNum
  ]

genMsgPackPair :: Gen (MV.Value, MV.Value)
genMsgPackPair = (,) <$> genMsgPackLeaf <*> genMsgPackLeaf

genCBORValue :: Gen C.Value
genCBORValue = Gen.choice
  [ C.UInt <$> Gen.word64 (Range.linear 0 0xFFFF)
  , C.NInt <$> Gen.word64 (Range.linear 0 0xFF)
  , C.Bool <$> Gen.bool
  , pure C.Null
  , C.TextString <$> Gen.text (Range.linear 0 50) Gen.alphaNum
  , C.ByteString <$> Gen.bytes (Range.linear 0 50)
  , C.Array <$> (V.fromList <$> Gen.list (Range.linear 0 5) genCBORLeaf)
  , C.Map <$> (V.fromList <$> Gen.list (Range.linear 0 5) genCBORPair)
  , C.Tag <$> Gen.word64 (Range.linear 0 100) <*> genCBORLeaf
  ]

genCBORLeaf :: Gen C.Value
genCBORLeaf = Gen.choice
  [ C.UInt <$> Gen.word64 (Range.linear 0 0xFF)
  , C.NInt <$> Gen.word64 (Range.linear 0 0xFF)
  , C.Bool <$> Gen.bool
  , pure C.Null
  , C.TextString <$> Gen.text (Range.linear 0 20) Gen.alphaNum
  ]

genCBORPair :: Gen (C.Value, C.Value)
genCBORPair = (,) <$> genCBORLeaf <*> genCBORLeaf
