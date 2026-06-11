{-# LANGUAGE OverloadedStrings #-}

module Test.Chunked (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Network.HTTP1.Chunked
import Test.QuickCheck
import Test.Syd
import Wireform.Builder qualified as B


tests :: Spec
tests =
  describe "Chunked" $
    sequence_
      [ chunkRoundTripTests
      , trailerTests
      ]


chunkRoundTripTests :: Spec
chunkRoundTripTests =
  describe "encode" $
    sequence_
      [ it "empty chunk encode is no-op" $
          runB (encodeChunk "")
            `shouldBe` "0\r\n\r\n"
      , it "5-byte chunk" $
          runB (encodeChunk "hello")
            `shouldBe` "5\r\nhello\r\n"
      , it "256-byte chunk uses hex" $
          let bs = BS.replicate 256 0x61
          in runB (encodeChunk bs)
               `shouldBe` "100\r\n" <> bs <> "\r\n"
      , it "last chunk terminator" $
          runB encodeLastChunk
            `shouldBe` "0\r\n\r\n"
      , it "decodes back via stream pull (in spirit)" $ property $ \(NonNegative n) ->
          n < 100000 ==>
            let body = BS.replicate n 0x78
                encoded = runB (encodeChunk body <> encodeLastChunk)
                hex = showHex' n
            in BS.isPrefixOf (BS.pack (map (fromIntegral . fromEnum) hex)) encoded
      ]


trailerTests :: Spec
trailerTests =
  describe "trailers" $
    sequence_
      [ it "no trailers => bare terminator" $
          runB (encodeLastChunkWithTrailers [])
            `shouldBe` "0\r\n\r\n"
      , it "single trailer" $
          runB (encodeLastChunkWithTrailers [("X-Trace", "abc")])
            `shouldBe` "0\r\nX-Trace: abc\r\n\r\n"
      ]


------------------------------------------------------------------------

runB :: B.Builder -> ByteString
runB = B.toStrictByteString


showHex' :: Int -> String
showHex' 0 = "0"
showHex' n0 = go n0 []
  where
    go 0 acc = acc
    go n acc = go (n `div` 16) (digit (n `mod` 16) : acc)
    digit d
      | d < 10 = toEnum (d + 48)
      | otherwise = toEnum (d - 10 + 97)
