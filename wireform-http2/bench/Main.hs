module Main (main) where

import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word

import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Types

main :: IO ()
main = defaultMain
  [ bgroup "frame"
      [ bench "encode DATA frame header" $
          nf encodeFrameHeader (FrameHeader 1024 FrameData 0 1)
      , bench "decode DATA frame header" $
          nf decodeFrameHeader sampleFrameHeader
      , bench "encode SETTINGS frame" $
          nf encodeFrame sampleSettingsFrame
      , bench "encode/decode roundtrip" $
          nf (\f -> decodeFrameHeader (encodeFrameHeader (frameHeader f))) sampleSettingsFrame
      ]
  , bgroup "hpack"
      [ bgroup "huffman"
          [ bench "encode www.example.com" $
              nf huffmanEncode "www.example.com"
          , bench "decode www.example.com" $
              nf huffmanDecode (huffmanEncode "www.example.com")
          , bench "encode 256-byte header value" $
              nf huffmanEncode longHeaderValue
          , bench "decode 256-byte header value" $
              nf huffmanDecode (huffmanEncode longHeaderValue)
          ]
      , bgroup "header-block"
          [ bench "encode typical request headers" $
              nfIO (encodeTypicalRequest 4096)
          , bench "decode typical request headers" $
              nfIO (decodeTypicalRequest 4096)
          , bench "encode 100 sequential requests (shared table)" $
              nfIO (encode100Requests 4096)
          ]
      ]
  ]

sampleFrameHeader :: ByteString
sampleFrameHeader = encodeFrameHeader (FrameHeader 1024 FrameData 0 1)

sampleSettingsFrame :: Frame
sampleSettingsFrame = Frame
  (FrameHeader 18 FrameSettings 0 0)
  (SettingsFrame [(0x3, 100), (0x4, 65535), (0x5, 16384)])

longHeaderValue :: ByteString
longHeaderValue = BS.replicate 256 0x61

typicalRequestHeaders :: [(ByteString, ByteString)]
typicalRequestHeaders =
  [ (":method", "GET")
  , (":path", "/api/v1/users?page=1&limit=20")
  , (":scheme", "https")
  , (":authority", "api.example.com")
  , ("accept", "application/json")
  , ("authorization", "Bearer eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJ1c2VyMSJ9.signature")
  , ("user-agent", "wireform-http2/0.1")
  , ("accept-encoding", "gzip, deflate")
  ]

encodeTypicalRequest :: Int -> IO ByteString
encodeTypicalRequest tableSize = do
  dt <- newDynamicTable tableSize
  encodeHeaderBlock defaultEncodeStrategy dt typicalRequestHeaders

decodeTypicalRequest :: Int -> IO (Either DecodeError [(ByteString, ByteString)])
decodeTypicalRequest tableSize = do
  encDt <- newDynamicTable tableSize
  decDt <- newDynamicTable tableSize
  encoded <- encodeHeaderBlock defaultEncodeStrategy encDt typicalRequestHeaders
  decodeHeaderBlock decDt encoded

encode100Requests :: Int -> IO [ByteString]
encode100Requests tableSize = do
  dt <- newDynamicTable tableSize
  let go 0 acc = pure (reverse acc)
      go n acc = do
        encoded <- encodeHeaderBlock defaultEncodeStrategy dt typicalRequestHeaders
        go (n - 1 :: Int) (encoded : acc)
  go 100 []
