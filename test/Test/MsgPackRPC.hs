module Test.MsgPackRPC (msgPackRPCTests) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import MsgPack.Encode (encode)
import MsgPack.RPC (RPCMessage(..), encodeRPC, decodeRPC)
import qualified MsgPack.Value as MV

msgPackRPCTests :: Spec
msgPackRPCTests = describe "MsgPack RPC" $ sequence_
  [ requestRoundtrip
  , responseRoundtrip
  , notificationRoundtrip
  , unitTests
  , errorTests
  ]

requestRoundtrip :: Spec
requestRoundtrip = describe "Request roundtrip" $ sequence_
  [ it "request roundtrip" $ property $ do
      msgid <- forAll $ Gen.word32 Range.linearBounded
      method <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
      nparams <- forAll $ Gen.int (Range.linear 0 10)
      params <- forAll $ V.replicateM nparams genSimpleValue
      let msg = RPCRequest msgid method params
      decodeRPC (encodeRPC msg) === Right msg
  ]

responseRoundtrip :: Spec
responseRoundtrip = describe "Response roundtrip" $ sequence_
  [ it "response with result" $ property $ do
      msgid <- forAll $ Gen.word32 Range.linearBounded
      result <- forAll genNonNilValue
      let msg = RPCResponse msgid Nothing (Just result)
      decodeRPC (encodeRPC msg) === Right msg

  , it "response with error" $ property $ do
      msgid <- forAll $ Gen.word32 Range.linearBounded
      err <- forAll genNonNilValue
      let msg = RPCResponse msgid (Just err) Nothing
      decodeRPC (encodeRPC msg) === Right msg

  , it "response with both nil" $ property $ do
      msgid <- forAll $ Gen.word32 Range.linearBounded
      let msg = RPCResponse msgid Nothing Nothing
      decodeRPC (encodeRPC msg) === Right msg
  ]

notificationRoundtrip :: Spec
notificationRoundtrip = describe "Notification roundtrip" $ sequence_
  [ it "notification roundtrip" $ property $ do
      method <- forAll $ Gen.text (Range.linear 1 50) Gen.alphaNum
      nparams <- forAll $ Gen.int (Range.linear 0 10)
      params <- forAll $ V.replicateM nparams genSimpleValue
      let msg = RPCNotification method params
      decodeRPC (encodeRPC msg) === Right msg
  ]

unitTests :: Spec
unitTests = describe "Unit tests" $ sequence_
  [ it "request with no params" $ do
      let msg = RPCRequest 1 "add" V.empty
      decodeRPC (encodeRPC msg) `shouldBe` Right msg

  , it "request with params" $ do
      let msg = RPCRequest 42 "echo" (V.fromList [MV.String "hello", MV.Word 123])
      decodeRPC (encodeRPC msg) `shouldBe` Right msg

  , it "response success" $ do
      let msg = RPCResponse 1 Nothing (Just (MV.Word 42))
      decodeRPC (encodeRPC msg) `shouldBe` Right msg

  , it "response error" $ do
      let msg = RPCResponse 1 (Just (MV.String "oops")) Nothing
      decodeRPC (encodeRPC msg) `shouldBe` Right msg

  , it "notification" $ do
      let msg = RPCNotification "update" (V.fromList [MV.Bool True])
      decodeRPC (encodeRPC msg) `shouldBe` Right msg
  ]

errorTests :: Spec
errorTests = describe "Error cases" $ sequence_
  [ it "empty input" $
      case decodeRPC BS.empty of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error on empty input"

  , it "non-array top level" $
      case decodeRPC (encode (MV.Word 42)) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error on non-array"

  , it "wrong array length" $
      case decodeRPC (encode (MV.Array (V.fromList [MV.Word 0, MV.Word 1]))) of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error on wrong array length"
  ]

genSimpleValue :: Gen MV.Value
genSimpleValue = Gen.choice
  [ pure MV.Nil
  , MV.Bool <$> Gen.bool
  , MV.Word <$> Gen.word64 (Range.linear 0 10000)
  , MV.Int <$> Gen.int64 (Range.linear (-10000) (-1))
  , MV.String <$> Gen.text (Range.linear 0 50) Gen.alphaNum
  ]

genNonNilValue :: Gen MV.Value
genNonNilValue = Gen.choice
  [ MV.Bool <$> Gen.bool
  , MV.Word <$> Gen.word64 (Range.linear 0 10000)
  , MV.Int <$> Gen.int64 (Range.linear (-10000) (-1))
  , MV.String <$> Gen.text (Range.linear 0 50) Gen.alphaNum
  ]
