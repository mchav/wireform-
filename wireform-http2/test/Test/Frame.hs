module Test.Frame (tests) where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.Word
import Test.Syd
import Test.QuickCheck

import Network.HTTP2.Frame
import Network.HTTP2.Types

tests :: Spec
tests = describe "Frame" $ sequence_
  [ describe "Header encode/decode" $ sequence_
      [ it "roundtrip DATA frame header" $ do
          let hdr = FrameHeader 100 FrameData 0 1
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded `shouldBe` Right hdr
      , it "roundtrip SETTINGS frame header" $ do
          let hdr = FrameHeader 12 FrameSettings 0 0
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded `shouldBe` Right hdr
      , it "roundtrip HEADERS frame header with flags" $ do
          let hdr = FrameHeader 50 FrameHeaders (flagEndHeaders .|. flagEndStream) 3
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded `shouldBe` Right hdr
      , it "stream ID high bit masked" $ do
          let hdr = FrameHeader 0 FramePing 0 0x80000001
              encoded = encodeFrameHeader hdr
          Right decoded <- pure (decodeFrameHeader encoded)
          fhStreamId decoded `shouldBe` 1
      , it "roundtrip arbitrary frame headers" $ property $ \len typ flags sid ->
          let hdr = FrameHeader (len `mod` 16777216) (word8ToFT typ) flags (sid `mod` 0x80000000)
              encoded = encodeFrameHeader hdr
          in decodeFrameHeader encoded == Right hdr
      ]
  , describe "Payload encode/decode" $ sequence_
      [ it "DATA frame" $ do
          let payload = DataFrame "hello world"
              hdr = FrameHeader (fromIntegral (BS.length "hello world")) FrameData 0 1
              encoded = encodeFramePayload 0 payload
          decodeFramePayload hdr encoded `shouldBe` Right payload
      , it "SETTINGS frame" $ do
          let params = [(0x3, 128), (0x4, 65535)]
              payload = SettingsFrame params
              hdr = FrameHeader 12 FrameSettings 0 0
              encoded = encodeFramePayload 0 payload
          decodeFramePayload hdr encoded `shouldBe` Right payload
      , it "PING frame" $ do
          let ping = PingFrame (BS.pack [1,2,3,4,5,6,7,8])
              hdr = FrameHeader 8 FramePing 0 0
              encoded = encodeFramePayload 0 ping
          decodeFramePayload hdr encoded `shouldBe` Right ping
      , it "WINDOW_UPDATE frame" $ do
          let wup = WindowUpdateFrame 1024
              hdr = FrameHeader 4 FrameWindowUpdate 0 1
              encoded = encodeFramePayload 0 wup
          decodeFramePayload hdr encoded `shouldBe` Right wup
      , it "RST_STREAM frame" $ do
          let rst = RSTStreamFrame Cancel
              hdr = FrameHeader 4 FrameRSTStream 0 1
              encoded = encodeFramePayload 0 rst
          decodeFramePayload hdr encoded `shouldBe` Right rst
      , it "GOAWAY frame" $ do
          let goaway = GoAwayFrame 5 ProtocolError "debug info"
              hdr = FrameHeader (fromIntegral (8 + BS.length "debug info")) FrameGoAway 0 0
              encoded = encodeFramePayload 0 goaway
          decodeFramePayload hdr encoded `shouldBe` Right goaway
      , it "Full frame roundtrip" $ do
          let frame = Frame
                (FrameHeader 11 FrameData flagEndStream 1)
                (DataFrame "hello world")
              encoded = encodeFrame frame
          case decodeFrameHeader encoded of
            Left err -> expectationFailure (show err)
            Right hdr ->
              case decodeFramePayload hdr (BS.drop frameHeaderLength encoded) of
                Left err -> expectationFailure (show err)
                Right payload -> Frame hdr payload `shouldBe` frame
      ]
  , describe "Connection preface" $ sequence_
      [ it "correct length" $
          BS.length connectionPreface `shouldBe` 24
      , it "correct content" $
          connectionPreface `shouldBe` "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
      ]
  ]

word8ToFT :: Word8 -> FrameType
word8ToFT = \case
  0 -> FrameData
  1 -> FrameHeaders
  2 -> FramePriority
  3 -> FrameRSTStream
  4 -> FrameSettings
  5 -> FramePushPromise
  6 -> FramePing
  7 -> FrameGoAway
  8 -> FrameWindowUpdate
  9 -> FrameContinuation
  w -> FrameUnknown w
