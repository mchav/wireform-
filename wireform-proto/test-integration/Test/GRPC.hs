module Test.GRPC (grpcTests) where

import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Proto.GRPC

grpcTests :: TestTree
grpcTests = testGroup "gRPC Framing"
  [ singleMessageRoundtrip
  , emptyMessageRoundtrip
  , multiMessageRoundtrip
  , errorCases
  , frameStructure
  ]

singleMessageRoundtrip :: TestTree
singleMessageRoundtrip = testGroup "Single message roundtrip"
  [ testCase "simple payload" $ do
      let payload = BS.pack [0x08, 0x96, 0x01]
          framed = grpcFrame payload
      grpcUnframe framed @?= Right payload

  , testCase "large payload" $ do
      let payload = BS.replicate 10000 0xAB
          framed = grpcFrame payload
      grpcUnframe framed @?= Right payload

  , testCase "single byte payload" $ do
      let payload = BS.singleton 0xFF
          framed = grpcFrame payload
      grpcUnframe framed @?= Right payload
  ]

emptyMessageRoundtrip :: TestTree
emptyMessageRoundtrip = testCase "Empty message roundtrip" $ do
  let payload = BS.empty
      framed = grpcFrame payload
  BS.length framed @?= 5
  grpcUnframe framed @?= Right payload

multiMessageRoundtrip :: TestTree
multiMessageRoundtrip = testGroup "Multiple messages"
  [ testCase "three messages" $ do
      let msgs = [BS.pack [1,2,3], BS.pack [4,5], BS.pack [6]]
          framed = grpcFrameMany msgs
      grpcUnframeMany framed @?= Right msgs

  , testCase "empty list" $ do
      let framed = grpcFrameMany []
      grpcUnframeMany framed @?= Right []

  , testCase "single in many" $ do
      let msgs = [BS.pack [0xDE, 0xAD]]
          framed = grpcFrameMany msgs
      grpcUnframeMany framed @?= Right msgs

  , testCase "mixed sizes including empty" $ do
      let msgs = [BS.empty, BS.pack [1], BS.replicate 256 0x42, BS.empty]
          framed = grpcFrameMany msgs
      grpcUnframeMany framed @?= Right msgs
  ]

errorCases :: TestTree
errorCases = testGroup "Error cases"
  [ testCase "too short for header" $ do
      case grpcUnframe (BS.pack [0x00, 0x00]) of
        Left _ -> return ()
        Right _ -> assertFailure "expected error for truncated header"

  , testCase "truncated payload" $ do
      case grpcUnframe (BS.pack [0x00, 0x00, 0x00, 0x05, 0x01, 0x02]) of
        Left _ -> return ()
        Right _ -> assertFailure "expected error for truncated payload"

  , testCase "trailing data" $ do
      let framed = grpcFrame (BS.pack [1,2,3])
          withTrailing = framed <> BS.singleton 0xFF
      case grpcUnframe withTrailing of
        Left _ -> return ()
        Right _ -> assertFailure "expected error for trailing data"

  , testCase "empty input to unframeMany" $ do
      grpcUnframeMany BS.empty @?= Right []
  ]

frameStructure :: TestTree
frameStructure = testCase "Frame header structure" $ do
  let payload = BS.pack [0x08, 0x96, 0x01]
      framed = grpcFrame payload
  BS.length framed @?= 8
  BS.index framed 0 @?= 0x00
  BS.index framed 1 @?= 0x00
  BS.index framed 2 @?= 0x00
  BS.index framed 3 @?= 0x00
  BS.index framed 4 @?= 0x03
