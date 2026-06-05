{-# LANGUAGE OverloadedStrings #-}
module Test.HPACKEdgeCases (tests) where

import qualified Data.ByteString as BS
import Test.Syd
import Test.QuickCheck

import Network.HTTP2.HPACK

tests :: Spec
tests = describe "HPACK edge cases" $ sequence_
  [ describe "Huffman edge cases" $ sequence_
      [ it "single character" $ do
          let encoded = huffmanEncode "a"
          huffmanDecode encoded `shouldBe` Right "a"
      , it "long repetitive input" $ do
          let input = BS.replicate 1024 0x61
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "all printable ASCII" $ do
          let input = BS.pack [32..126]
              encoded = huffmanEncode input
          huffmanDecode encoded `shouldBe` Right input
      , it "roundtrip printable ASCII strings" $ property $ \(ASCIIString s) ->
          not (null s) ==>
            let input = BS.pack (map (fromIntegral . fromEnum) s)
                encoded = huffmanEncode input
            in huffmanDecode encoded == Right input
      ]
  , describe "Header block edge cases" $ sequence_
      [ it "empty header list" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt []
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right []
      , it "header with empty value" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let headers = [(":path", ""), ("x-empty", "")]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right headers
      , it "header with long value (4 KiB)" $ do
          encDt <- newDynamicTable 8192
          decDt <- newDynamicTable 8192
          let longVal = BS.replicate 4096 0x78
              headers = [("x-long", longVal)]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right headers
      , it "many headers saturate dynamic table" $ do
          encDt <- newDynamicTable 256
          decDt <- newDynamicTable 256
          let headers =
                [ (BS.pack [0x78, fromIntegral i], BS.pack [0x76, fromIntegral i])
                | i <- [1 :: Int .. 20]
                ]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right headers
      , it "sequential encodes share dynamic table state" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let h1 = [("x-session", "abc123")]
          e1 <- encodeHeaderBlock defaultEncodeStrategy encDt h1
          _ <- decodeHeaderBlock decDt e1
          let h2 = [("x-session", "abc123"), (":method", "POST")]
          e2 <- encodeHeaderBlock defaultEncodeStrategy encDt h2
          r2 <- decodeHeaderBlock decDt e2
          r2 `shouldBe` Right h2
          (BS.length e2 <= BS.length e1 + 3) `shouldBe` True
      , it "pseudo-headers come first" $ do
          encDt <- newDynamicTable 4096
          decDt <- newDynamicTable 4096
          let headers =
                [ (":method", "GET")
                , (":path", "/index.html")
                , (":scheme", "https")
                , (":authority", "example.com")
                , ("accept", "text/html")
                , ("user-agent", "test")
                ]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right headers
      ]
  , describe "Dynamic table sizing" $ sequence_
      [ it "zero-size table still works (no indexing)" $ do
          encDt <- newDynamicTable 0
          decDt <- newDynamicTable 0
          let headers = [(":method", "GET"), (":path", "/")]
          encoded <- encodeHeaderBlock defaultEncodeStrategy encDt headers
          result <- decodeHeaderBlock decDt encoded
          result `shouldBe` Right headers
      ]
  ]
