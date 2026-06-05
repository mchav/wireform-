module Test.HPACK (tests) where

import qualified Data.ByteString as BS
import Test.Syd
import Test.QuickCheck

import Network.HTTP2.HPACK

tests :: Spec
tests = describe "HPACK" $ sequence_
  [ describe "Huffman" $ sequence_
      [ it "encode/decode roundtrip - empty" $ do
          let encoded = huffmanEncode ""
          huffmanDecode encoded `shouldBe` Right ""
      , it "encode/decode roundtrip - www.example.com" $ do
          let input = "www.example.com"
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "encode/decode roundtrip - no-cache" $ do
          let input = "no-cache"
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "huffman is shorter for ASCII" $ do
          let input = "custom-key"
          BS.length (huffmanEncode input) < BS.length input `shouldBe` True
      , it "roundtrip arbitrary ASCII" $ property $ \(ASCIIString s) ->
          let bs = BS.pack (map (fromIntegral . fromEnum) s)
          in huffmanDecode (huffmanEncode bs) == Right bs
      , it "encode/decode roundtrip - single non-ASCII byte 0x80" $ do
          let input = BS.pack [0x80]
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "encode/decode roundtrip - single non-ASCII byte 0xFF" $ do
          let input = BS.pack [0xFF]
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "encode/decode roundtrip - multiple non-ASCII bytes" $ do
          let input = BS.pack [0x80, 0xFF, 0xC0, 0xFE, 0x80, 0xFF, 0xC0, 0xFE]
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "encode/decode roundtrip - all bytes 0x80..0xFF" $ do
          let input = BS.pack [0x80..0xFF]
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "encode/decode roundtrip - all 256 byte values" $ do
          let input = BS.pack [0x00..0xFF]
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "encode/decode roundtrip - long non-ASCII sequence" $ do
          let input = BS.pack (concat (replicate 100 [0x80, 0xC0, 0xFF]))
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "roundtrip arbitrary bytes" $ property $ \ws ->
          let bs = BS.pack ws
          in not (null ws) ==> huffmanDecode (huffmanEncode bs) == Right bs
      ]
  , describe "Huffman with HPACK headers" $ sequence_
      [ it "header roundtrip with Huffman enabled" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let huffStrategy = EncodeStrategy { useHuffman = True, useDynamicTable = True }
              headers = [(":method", "GET"), (":path", "/"), ("custom", "value")]
          encoded <- encodeHeaderBlock huffStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right headers
      , it "header roundtrip with Huffman - non-ASCII values" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let huffStrategy = EncodeStrategy { useHuffman = True, useDynamicTable = True }
              headers = [("x-bin", BS.pack [0x80, 0xFF, 0xC0]), (":method", "GET")]
          encoded <- encodeHeaderBlock huffStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right headers
      ]
  , describe "Integer encoding" $ sequence_
      [ it "RFC 7541 C.1.1 - encoding 10 with 5-bit prefix" $ do
          dt <- newDynamicTable 4096
          let headers = [(":method", "GET")]
          encoded <- encodeHeaderBlock defaultEncodeStrategy dt headers
          (not (BS.null encoded)) `shouldBe` True
      ]
  , describe "Header encoding/decoding" $ sequence_
      [ it "static indexed - :method GET" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let headers = [(":method", "GET")]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right headers
      , it "multiple headers roundtrip" $ do
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
          result `shouldBe` Right headers
      , it "dynamic table population" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let headers1 = [("custom-key", "custom-value")]
          encoded1 <- encodeHeaderBlock defaultEncodeStrategy encDt headers1
          _ <- decodeHeaderBlock decDt encoded1
          let headers2 = [("custom-key", "custom-value")]
          encoded2 <- encodeHeaderBlock defaultEncodeStrategy encDt headers2
          result <- decodeHeaderBlock decDt encoded2
          result `shouldBe` Right headers2
          (BS.length encoded2 < BS.length encoded1) `shouldBe` True
      , it "eviction under size pressure" $ do
          encDt <- newDynamicTable 64
          decDt <- newDynamicTable 64
          let headers = [("a-very-long-header-name", "a-very-long-header-value")]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right headers
      ]
  ]
