module Test.Frame (tests) where

import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Network.HTTP2.Frame
import Network.HTTP2.Types

tests :: TestTree
tests = testGroup "Frame"
  [ testGroup "Header encode/decode"
      [ testCase "roundtrip DATA frame header" $ do
          let hdr = FrameHeader 100 FrameData 0 1
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded @?= Right hdr
      , testCase "roundtrip SETTINGS frame header" $ do
          let hdr = FrameHeader 12 FrameSettings 0 0
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded @?= Right hdr
      , testCase "roundtrip HEADERS frame header with flags" $ do
          let hdr = FrameHeader 50 FrameHeaders (flagEndHeaders .|. flagEndStream) 3
              encoded = encodeFrameHeader hdr
          decodeFrameHeader encoded @?= Right hdr
      , testCase "stream ID high bit masked" $ do
          let hdr = FrameHeader 0 FramePing 0 0x80000001
              encoded = encodeFrameHeader hdr
          Right decoded <- pure (decodeFrameHeader encoded)
          fhStreamId decoded @?= 1
      , testProperty "roundtrip arbitrary frame headers" $ \len typ flags sid ->
          let hdr = FrameHeader (len `mod` 16777216) (word8ToFT typ) flags (sid `mod` 0x80000000)
              encoded = encodeFrameHeader hdr
          in decodeFrameHeader encoded == Right hdr
      ]
  , testGroup "Payload encode/decode"
      [ testCase "DATA frame" $ do
          let payload = DataFrame "hello world"
              hdr = FrameHeader (fromIntegral (BS.length "hello world")) FrameData 0 1
              encoded = encodeFramePayload 0 payload
          decodeFramePayload hdr encoded @?= Right payload
      , testCase "SETTINGS frame" $ do
          let params = [(0x3, 128), (0x4, 65535)]
              payload = SettingsFrame params
              hdr = FrameHeader 12 FrameSettings 0 0
              encoded = encodeFramePayload 0 payload
          decodeFramePayload hdr encoded @?= Right payload
      , testCase "PING frame" $ do
          let ping = PingFrame (BS.pack [1,2,3,4,5,6,7,8])
              hdr = FrameHeader 8 FramePing 0 0
              encoded = encodeFramePayload 0 ping
          decodeFramePayload hdr encoded @?= Right ping
      , testCase "WINDOW_UPDATE frame" $ do
          let wup = WindowUpdateFrame 1024
              hdr = FrameHeader 4 FrameWindowUpdate 0 1
              encoded = encodeFramePayload 0 wup
          decodeFramePayload hdr encoded @?= Right wup
      , testCase "RST_STREAM frame" $ do
          let rst = RSTStreamFrame Cancel
              hdr = FrameHeader 4 FrameRSTStream 0 1
              encoded = encodeFramePayload 0 rst
          decodeFramePayload hdr encoded @?= Right rst
      , testCase "GOAWAY frame" $ do
          let goaway = GoAwayFrame 5 ProtocolError "debug info"
              hdr = FrameHeader (fromIntegral (8 + BS.length "debug info")) FrameGoAway 0 0
              encoded = encodeFramePayload 0 goaway
          decodeFramePayload hdr encoded @?= Right goaway
      , testCase "Full frame roundtrip" $ do
          let frame = Frame
                (FrameHeader 11 FrameData flagEndStream 1)
                (DataFrame "hello world")
              encoded = encodeFrame frame
          case decodeFrameHeader encoded of
            Left err -> assertFailure (show err)
            Right hdr ->
              case decodeFramePayload hdr (BS.drop frameHeaderLength encoded) of
                Left err -> assertFailure (show err)
                Right payload -> Frame hdr payload @?= frame
      ]
  , testGroup "Connection preface"
      [ testCase "correct length" $
          BS.length connectionPreface @?= 24
      , testCase "correct content" $
          connectionPreface @?= "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
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
