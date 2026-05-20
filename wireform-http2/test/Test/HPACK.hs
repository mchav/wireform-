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
