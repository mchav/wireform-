-- | Comparison benchmark: wireform-http2 frame and HPACK operations
-- against absolute reference numbers from nghttp2 and the http2 Haskell package.
--
-- Reference points (from published benchmarks):
-- - nghttp2 HPACK encode: ~0.5μs for typical request headers
-- - nghttp2 HPACK decode: ~0.3μs for typical request headers
-- - http2 (Haskell) HPACK encode: ~3-5μs for typical request headers
-- - http2 (Haskell) HPACK decode: ~2-4μs for typical request headers
-- - http2 (Haskell) frame encode: ~50-100ns
--
-- Our targets:
-- - Frame header encode: <20ns (sub-allocation-boundary)
-- - Frame header decode: <30ns
-- - HPACK encode (typical 8-header request): <2μs
-- - HPACK decode (typical 8-header request): <3μs
-- - Huffman encode 256B: <300ns
-- - Huffman decode 256B: <1μs
module Main (main) where

import Control.DeepSeq (NFData(..), deepseq)
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word

import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Types

main :: IO ()
main = do
  -- Prepare encoded data for decode benchmarks
  encDt <- newDynamicTable 4096
  encodedTypical <- encodeHeaderBlock defaultEncodeStrategy encDt typicalRequestHeaders
  encodedLarge <- encodeHeaderBlock defaultEncodeStrategy encDt largeRequestHeaders

  -- Pre-encode huffman data
  let huffEncoded15 = huffmanEncode "www.example.com"
      huffEncoded256 = huffmanEncode (BS.replicate 256 0x61)
      huffEncodedMixed = huffmanEncode mixedContent

  defaultMain
    [ bgroup "wireform-http2: frame"
        [ bench "encode header (9 bytes)" $
            nf encodeFrameHeader (FrameHeader 1024 FrameData flagEndStream 1)
        , bench "decode header (9 bytes)" $
            nf decodeFrameHeader sampleFrameHeader
        , bench "encode SETTINGS frame (full)" $
            nf encodeFrame sampleSettingsFrame
        , bench "encode WINDOW_UPDATE frame" $
            nf encodeFrame windowUpdateFrame
        ]
    , bgroup "wireform-http2: huffman"
        [ bench "encode 15B (www.example.com)" $
            nf huffmanEncode "www.example.com"
        , bench "decode 15B" $
            nf huffmanDecode huffEncoded15
        , bench "encode 256B (uniform)" $
            nf huffmanEncode (BS.replicate 256 0x61)
        , bench "decode 256B" $
            nf huffmanDecode huffEncoded256
        , bench "encode 128B (mixed ASCII)" $
            nf huffmanEncode mixedContent
        , bench "decode 128B (mixed)" $
            nf huffmanDecode huffEncodedMixed
        , bench "encode length only 256B" $
            nf huffmanEncodeLength (BS.replicate 256 0x61)
        ]
    , bgroup "wireform-http2: hpack"
        [ bench "encode typical request (8 headers, fresh table)" $
            nfIO (encodeWithFreshTable typicalRequestHeaders)
        , bench "decode typical request (8 headers, fresh table)" $
            nfIO (decodeWithFreshTable encodedTypical)
        , bench "encode large request (15 headers, fresh table)" $
            nfIO (encodeWithFreshTable largeRequestHeaders)
        , bench "decode large request (15 headers, fresh table)" $
            nfIO (decodeWithFreshTable encodedLarge)
        , bench "encode 50 requests (shared table, amortized)" $
            nfIO (encodeNRequests 50 typicalRequestHeaders)
        , bench "encode 200 requests (shared table, hot)" $
            nfIO (encodeNRequests 200 typicalRequestHeaders)
        ]
    , bgroup "wireform-http2: throughput"
        [ bench "encode+decode roundtrip (typical request)" $
            nfIO (roundtripHeaders typicalRequestHeaders)
        , bench "1000 frame headers encode" $
            nf encode1000FrameHeaders ()
        ]
    ]

sampleFrameHeader :: ByteString
sampleFrameHeader = encodeFrameHeader (FrameHeader 1024 FrameData flagEndStream 1)

sampleSettingsFrame :: Frame
sampleSettingsFrame = Frame
  (FrameHeader 18 FrameSettings 0 0)
  (SettingsFrame [(0x3, 100), (0x4, 65535), (0x5, 16384)])

windowUpdateFrame :: Frame
windowUpdateFrame = Frame
  (FrameHeader 4 FrameWindowUpdate 0 1)
  (WindowUpdateFrame 32768)

typicalRequestHeaders :: [(ByteString, ByteString)]
typicalRequestHeaders =
  [ (":method", "GET")
  , (":path", "/api/v1/users?page=1&limit=20")
  , (":scheme", "https")
  , (":authority", "api.example.com")
  , ("accept", "application/json")
  , ("authorization", "Bearer eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJ1c2VyMSJ9.sig")
  , ("user-agent", "wireform-http2/0.1")
  , ("accept-encoding", "gzip, deflate")
  ]

largeRequestHeaders :: [(ByteString, ByteString)]
largeRequestHeaders = typicalRequestHeaders <>
  [ ("cache-control", "no-cache")
  , ("x-request-id", "550e8400-e29b-41d4-a716-446655440000")
  , ("x-forwarded-for", "192.168.1.100")
  , ("x-forwarded-proto", "https")
  , ("content-type", "application/json")
  , ("origin", "https://app.example.com")
  , ("referer", "https://app.example.com/dashboard")
  ]

mixedContent :: ByteString
mixedContent = "The quick brown fox jumps over the lazy dog. HTTP/2 is great! /api/v1/resource?key=value&foo=bar123"

encodeWithFreshTable :: [(ByteString, ByteString)] -> IO ByteString
encodeWithFreshTable headers = do
  dt <- newDynamicTable 4096
  encodeHeaderBlock defaultEncodeStrategy dt headers

decodeWithFreshTable :: ByteString -> IO (Either DecodeError [(ByteString, ByteString)])
decodeWithFreshTable encoded = do
  dt <- newDynamicTable 4096
  decodeHeaderBlock dt encoded

encodeNRequests :: Int -> [(ByteString, ByteString)] -> IO [ByteString]
encodeNRequests n headers = do
  dt <- newDynamicTable 4096
  go n []
  where
    go 0 acc = pure (reverse acc)
    go remaining acc = do
      dt <- newDynamicTable 4096
      encoded <- encodeHeaderBlock defaultEncodeStrategy dt headers
      go (remaining - 1) (encoded : acc)

roundtripHeaders :: [(ByteString, ByteString)] -> IO (Either DecodeError [(ByteString, ByteString)])
roundtripHeaders headers = do
  encDt <- newDynamicTable 4096
  decDt <- newDynamicTable 4096
  encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
  decodeHeaderBlock decDt encoded

encode1000FrameHeaders :: () -> [ByteString]
encode1000FrameHeaders _ =
  let go 0 acc = acc
      go n acc = go (n - 1 :: Int)
        (encodeFrameHeader (FrameHeader (fromIntegral n) FrameData 0 (fromIntegral n)) : acc)
  in go 1000 []
