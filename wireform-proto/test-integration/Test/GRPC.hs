module Test.GRPC (grpcTests) where

import qualified Data.ByteString as BS
import Proto.GRPC
import Test.Syd


grpcTests :: Spec
grpcTests =
  describe "gRPC Framing" $
    sequence_
      [ singleMessageRoundtrip
      , emptyMessageRoundtrip
      , multiMessageRoundtrip
      , errorCases
      , frameStructure
      ]


singleMessageRoundtrip :: Spec
singleMessageRoundtrip =
  describe "Single message roundtrip" $
    sequence_
      [ it "simple payload" $ do
          let payload = BS.pack [0x08, 0x96, 0x01]
              framed = grpcFrame payload
          grpcUnframe framed `shouldBe` Right payload
      , it "large payload" $ do
          let payload = BS.replicate 10000 0xAB
              framed = grpcFrame payload
          grpcUnframe framed `shouldBe` Right payload
      , it "single byte payload" $ do
          let payload = BS.singleton 0xFF
              framed = grpcFrame payload
          grpcUnframe framed `shouldBe` Right payload
      ]


emptyMessageRoundtrip :: Spec
emptyMessageRoundtrip = it "Empty message roundtrip" $ do
  let payload = BS.empty
      framed = grpcFrame payload
  BS.length framed `shouldBe` 5
  grpcUnframe framed `shouldBe` Right payload


multiMessageRoundtrip :: Spec
multiMessageRoundtrip =
  describe "Multiple messages" $
    sequence_
      [ it "three messages" $ do
          let msgs = [BS.pack [1, 2, 3], BS.pack [4, 5], BS.pack [6]]
              framed = grpcFrameMany msgs
          grpcUnframeMany framed `shouldBe` Right msgs
      , it "empty list" $ do
          let framed = grpcFrameMany []
          grpcUnframeMany framed `shouldBe` Right []
      , it "single in many" $ do
          let msgs = [BS.pack [0xDE, 0xAD]]
              framed = grpcFrameMany msgs
          grpcUnframeMany framed `shouldBe` Right msgs
      , it "mixed sizes including empty" $ do
          let msgs = [BS.empty, BS.pack [1], BS.replicate 256 0x42, BS.empty]
              framed = grpcFrameMany msgs
          grpcUnframeMany framed `shouldBe` Right msgs
      ]


errorCases :: Spec
errorCases =
  describe "Error cases" $
    sequence_
      [ it "too short for header" $ do
          case grpcUnframe (BS.pack [0x00, 0x00]) of
            Left _ -> return ()
            Right _ -> expectationFailure "expected error for truncated header"
      , it "truncated payload" $ do
          case grpcUnframe (BS.pack [0x00, 0x00, 0x00, 0x05, 0x01, 0x02]) of
            Left _ -> return ()
            Right _ -> expectationFailure "expected error for truncated payload"
      , it "trailing data" $ do
          let framed = grpcFrame (BS.pack [1, 2, 3])
              withTrailing = framed <> BS.singleton 0xFF
          case grpcUnframe withTrailing of
            Left _ -> return ()
            Right _ -> expectationFailure "expected error for trailing data"
      , it "empty input to unframeMany" $ do
          grpcUnframeMany BS.empty `shouldBe` Right []
      ]


frameStructure :: Spec
frameStructure = it "Frame header structure" $ do
  let payload = BS.pack [0x08, 0x96, 0x01]
      framed = grpcFrame payload
  BS.length framed `shouldBe` 8
  BS.index framed 0 `shouldBe` 0x00
  BS.index framed 1 `shouldBe` 0x00
  BS.index framed 2 `shouldBe` 0x00
  BS.index framed 3 `shouldBe` 0x00
  BS.index framed 4 `shouldBe` 0x03
