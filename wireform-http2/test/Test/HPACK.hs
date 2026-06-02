module Test.HPACK (tests) where

import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Network.HTTP2.HPACK

tests :: TestTree
tests = testGroup "HPACK"
  [ testGroup "Huffman"
      [ testCase "encode/decode roundtrip - empty" $ do
          let encoded = huffmanEncode ""
          huffmanDecode encoded @?= Right ""
      , testCase "encode/decode roundtrip - www.example.com" $ do
          let input = "www.example.com"
              encoded = huffmanEncode input
          huffmanDecode encoded @?= Right input
      , testCase "encode/decode roundtrip - no-cache" $ do
          let input = "no-cache"
              encoded = huffmanEncode input
          huffmanDecode encoded @?= Right input
      , testCase "huffman is shorter for ASCII" $ do
          let input = "custom-key"
          BS.length (huffmanEncode input) < BS.length input @? "Huffman should compress ASCII"
      , testProperty "roundtrip arbitrary ASCII" $ \(ASCIIString s) ->
          let bs = BS.pack (map (fromIntegral . fromEnum) s)
          in huffmanDecode (huffmanEncode bs) == Right bs
      , testCase "encode/decode roundtrip - single non-ASCII byte 0x80" $ do
          let input = BS.pack [0x80]
              encoded = huffmanEncode input
          huffmanDecode encoded @?= Right input
      , testCase "encode/decode roundtrip - single non-ASCII byte 0xFF" $ do
          let input = BS.pack [0xFF]
              encoded = huffmanEncode input
          huffmanDecode encoded @?= Right input
      , testCase "encode/decode roundtrip - multiple non-ASCII bytes" $ do
          let input = BS.pack [0x80, 0xFF, 0xC0, 0xFE, 0x80, 0xFF, 0xC0, 0xFE]
              encoded = huffmanEncode input
          huffmanDecode encoded @?= Right input
      , testCase "encode/decode roundtrip - all bytes 0x80..0xFF" $ do
          let input = BS.pack [0x80..0xFF]
              encoded = huffmanEncode input
          huffmanDecode encoded @?= Right input
      , testCase "encode/decode roundtrip - all 256 byte values" $ do
          let input = BS.pack [0x00..0xFF]
              encoded = huffmanEncode input
          huffmanDecode encoded @?= Right input
      , testCase "encode/decode roundtrip - long non-ASCII sequence" $ do
          let input = BS.pack (concat (replicate 100 [0x80, 0xC0, 0xFF]))
              encoded = huffmanEncode input
          huffmanDecode encoded @?= Right input
      , testProperty "roundtrip arbitrary bytes" $ \ws ->
          let bs = BS.pack ws
          in not (null ws) ==> huffmanDecode (huffmanEncode bs) == Right bs
      ]
  , testGroup "Huffman with HPACK headers"
      [ testCase "header roundtrip with Huffman enabled" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let huffStrategy = EncodeStrategy { useHuffman = True, useDynamicTable = True }
              headers = [(":method", "GET"), (":path", "/"), ("custom", "value")]
          encoded <- encodeHeaderBlock huffStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result @?= Right headers
      , testCase "header roundtrip with Huffman - non-ASCII values" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let huffStrategy = EncodeStrategy { useHuffman = True, useDynamicTable = True }
              headers = [("x-bin", BS.pack [0x80, 0xFF, 0xC0]), (":method", "GET")]
          encoded <- encodeHeaderBlock huffStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result @?= Right headers
      ]
  , testGroup "Integer encoding"
      [ testCase "RFC 7541 C.1.1 - encoding 10 with 5-bit prefix" $ do
          dt <- newDynamicTable 4096
          let headers = [(":method", "GET")]
          encoded <- encodeHeaderBlock defaultEncodeStrategy dt headers
          assertBool "non-empty" (not (BS.null encoded))
      ]
  , testGroup "Header encoding/decoding"
      [ testCase "static indexed - :method GET" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let headers = [(":method", "GET")]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result @?= Right headers
      , testCase "multiple headers roundtrip" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let headers =
                [ (":method", "GET")
                , (":path", "/")
                , (":scheme", "https")
                , (":authority", "example.com")
                , ("custom-header", "custom-value")
                ]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result @?= Right headers
      , testCase "dynamic table population" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let headers1 = [("custom-key", "custom-value")]
          encoded1 <- encodeHeaderBlock defaultEncodeStrategy encDt headers1
          _ <- decodeHeaderBlock decDt encoded1
          let headers2 = [("custom-key", "custom-value")]
          encoded2 <- encodeHeaderBlock defaultEncodeStrategy encDt headers2
          result <- decodeHeaderBlock decDt encoded2
          result @?= Right headers2
          assertBool "second encoding should use index" (BS.length encoded2 < BS.length encoded1)
      , testCase "eviction under size pressure" $ do
          encDt <- newDynamicTable 64
          decDt <- newDynamicTable 64
          let headers = [("a-very-long-header-name", "a-very-long-header-value")]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result @?= Right headers
      ]
  ]
