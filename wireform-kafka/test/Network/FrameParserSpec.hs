{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tests for the wireform-parser-based Kafka response frame
reader ('Kafka.Network.FrameParser').
-}
module Network.FrameParserSpec (tests) where

import Data.Binary.Put qualified as BP
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Int (Int32)
import Kafka.Network.FrameParser qualified as FP
import Test.Syd
import Wireform.Network (chunkedReceiveFn, withReceiveBufTransport)
import Wireform.Parser.Driver (LoopControl (..))
import Wireform.Transport.Config (defaultTransportConfig)


{- | Build a single Kafka response frame: [4-byte BE length]
[4-byte BE correlation id] [body].  Length covers id+body.
-}
frameBytes :: Int32 -> BS.ByteString -> BS.ByteString
frameBytes cid body =
  let !payload = BL.toStrict $ BP.runPut $ do
        BP.putInt32be cid
        BP.putByteString body
      !lenHdr =
        BL.toStrict $
          BP.runPut $
            BP.putInt32be (fromIntegral (BS.length payload))
  in lenHdr <> payload


tests :: Spec
tests =
  describe "Kafka.Network.FrameParser" $
    sequence_
      [ it "reads a single frame off the magic ring" $ do
          let bs = frameBytes 7 "hello"
          recvFn <- chunkedReceiveFn [bs]
          got <- newIORef Nothing
          r <- withReceiveBufTransport defaultTransportConfig recvFn $ \t ->
            FP.runKafkaFrameLoop t $ \(cid, body) -> do
              -- 'takeBs' hands back a slice of the magic-ring memory
              -- which becomes invalid as soon as 'withReceiveBufTransport'
              -- tears the ring down.  Force the copy now via a bang so
              -- the IORef holds a heap-allocated bytestring that
              -- outlives the transport scope.
              let !bodyCopy = BS.copy body
              writeIORef got (Just (cid, bodyCopy))
              pure Stop
          case r of
            Right () -> pure ()
            Left e -> expectationFailure ("loop error: " <> show e)
          Just (cid, body) <- readIORef got
          cid `shouldBe` 7
          body `shouldBe` "hello"
      , it "reads two frames split across recv chunks" $ do
          let f1 = frameBytes 11 "abc"
              f2 = frameBytes 22 "defg"
              combined = f1 <> f2
              (l, r) = BS.splitAt (BS.length f1 + 4) combined
          recvFn <- chunkedReceiveFn [l, r]
          acc <- newIORef ([] :: [(Int32, BS.ByteString)])
          _ <- withReceiveBufTransport defaultTransportConfig recvFn $ \t ->
            FP.runKafkaFrameLoop t $ \(cid, body) -> do
              let !bodyCopy = BS.copy body
              modifyIORef acc ((cid, bodyCopy) :)
              xs <- readIORef acc
              pure (if length xs >= 2 then Stop else Continue)
          observed <- reverse <$> readIORef acc
          observed `shouldBe` [(11, "abc"), (22, "defg")]
      , it "raises FrameTooShort on undersized length prefix" $ do
          let !payload = BL.toStrict $ BP.runPut $ BP.putInt32be 2 -- < 4
              !len = BS.length payload
              _ = len
          recvFn <- chunkedReceiveFn [payload]
          r <- withReceiveBufTransport defaultTransportConfig recvFn $ \t ->
            FP.runKafkaFrameLoop t $ \_ -> pure Continue
          case r of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected FrameTooShort"
      ]
