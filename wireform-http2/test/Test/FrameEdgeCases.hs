{-# LANGUAGE OverloadedStrings #-}
module Test.FrameEdgeCases (tests) where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.Word
import Test.Syd
import Test.QuickCheck

import Network.HTTP2.Frame
import Network.HTTP2.Types

tests :: Spec
tests = describe "Frame edge cases" $ sequence_
  [ describe "payload roundtrips" $ sequence_
      [ it "PRIORITY frame" $ do
          let payload = PriorityFrame (Priority False 0 16)
              hdr = FrameHeader 5 FramePriority 0 3
              encoded = encodeFramePayload 0 payload
          decodeFramePayload hdr encoded `shouldBe` Right payload
      , it "PUSH_PROMISE frame" $ do
          let payload = PushPromiseFrame 4 "header-block"
              hdr = FrameHeader (fromIntegral (4 + BS.length "header-block")) FramePushPromise 0 1
              encoded = encodeFramePayload 0 payload
          decodeFramePayload hdr encoded `shouldBe` Right payload
      , it "CONTINUATION frame" $ do
          let payload = ContinuationFrame "more-headers"
              hdr = FrameHeader (fromIntegral (BS.length "more-headers")) FrameContinuation flagEndHeaders 3
              encoded = encodeFramePayload 0 payload
          decodeFramePayload hdr encoded `shouldBe` Right payload
      , it "SETTINGS ACK (empty)" $ do
          let payload = SettingsFrame []
              hdr = FrameHeader 0 FrameSettings flagAck 0
              encoded = encodeFramePayload 0 payload
          decodeFramePayload hdr encoded `shouldBe` Right payload
      ]
  , describe "frame header boundary values" $ sequence_
      [ it "maximum payload length (2^24 - 1)" $ do
          let hdr = FrameHeader 16777215 FrameData 0 1
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded `shouldBe` Right hdr
      , it "zero payload length" $ do
          let hdr = FrameHeader 0 FrameData flagEndStream 1
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded `shouldBe` Right hdr
      , it "maximum stream ID (2^31 - 1)" $ do
          let hdr = FrameHeader 0 FramePing 0 0x7FFFFFFF
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded `shouldBe` Right hdr
      , it "all flags set" $ do
          let hdr = FrameHeader 0 FrameHeaders 0xFF 1
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded `shouldBe` Right hdr
      ]
  , describe "error codes" $ sequence_
      [ it "all standard error codes roundtrip through RST_STREAM" $ do
          let errors = [ NoError, ProtocolError, InternalError
                       , FlowControlError, SettingsTimeout
                       , StreamClosed, FrameSizeError
                       , RefusedStream, Cancel
                       , CompressionError, ConnectError
                       , EnhanceYourCalm, InadequateSecurity
                       , HTTP11Required
                       ]
          mapM_ (\e -> do
            let rst = RSTStreamFrame e
                hdr = FrameHeader 4 FrameRSTStream 0 1
                encoded = encodeFramePayload 0 rst
            decodeFramePayload hdr encoded `shouldBe` Right rst
            ) errors
      , it "GOAWAY with empty debug data" $ do
          let goaway = GoAwayFrame 0 NoError ""
              hdr = FrameHeader 8 FrameGoAway 0 0
              encoded = encodeFramePayload 0 goaway
          decodeFramePayload hdr encoded `shouldBe` Right goaway
      ]
  , describe "full frame encode/decode" $ sequence_
      [ it "arbitrary DATA frames roundtrip" $ property $ \bs ->
          let body = BS.pack bs
              frame = Frame
                (FrameHeader (fromIntegral (BS.length body)) FrameData 0 1)
                (DataFrame body)
              encoded = encodeFrame frame
          in case decodeFrameHeader encoded of
               Left _ -> False
               Right hdr ->
                 case decodeFramePayload hdr (BS.drop frameHeaderLength encoded) of
                   Left _ -> False
                   Right payload -> Frame hdr payload == frame
      , it "WINDOW_UPDATE values roundtrip" $ property $ \w ->
          let val = (w :: Word32) `mod` 0x7FFFFFFF + 1
              frame = Frame
                (FrameHeader 4 FrameWindowUpdate 0 1)
                (WindowUpdateFrame val)
              encoded = encodeFrame frame
          in case decodeFrameHeader encoded of
               Left _ -> False
               Right hdr ->
                 case decodeFramePayload hdr (BS.drop frameHeaderLength encoded) of
                   Left _ -> False
                   Right payload -> Frame hdr payload == frame
      ]
  ]
