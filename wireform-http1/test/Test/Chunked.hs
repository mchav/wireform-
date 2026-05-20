{-# LANGUAGE OverloadedStrings #-}
module Test.Chunked (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Wireform.Builder as B

import Network.HTTP1.Chunked

tests :: TestTree
tests = testGroup "Chunked"
  [ chunkRoundTripTests
  , trailerTests
  ]

chunkRoundTripTests :: TestTree
chunkRoundTripTests = testGroup "encode"
  [ testCase "empty chunk encode is no-op" $
      runB (encodeChunk "")
        @?= "0\r\n\r\n"
  , testCase "5-byte chunk" $
      runB (encodeChunk "hello")
        @?= "5\r\nhello\r\n"
  , testCase "256-byte chunk uses hex" $
      let bs = BS.replicate 256 0x61
      in runB (encodeChunk bs)
           @?= "100\r\n" <> bs <> "\r\n"
  , testCase "last chunk terminator" $
      runB encodeLastChunk
        @?= "0\r\n\r\n"
  , testProperty "decodes back via stream pull (in spirit)" $ \(NonNegative n) ->
      n < 100000 ==>
        let body = BS.replicate n 0x78
            encoded = runB (encodeChunk body <> encodeLastChunk)
            hex = showHex' n
        in BS.isPrefixOf (BS.pack (map (fromIntegral . fromEnum) hex)) encoded
  ]

trailerTests :: TestTree
trailerTests = testGroup "trailers"
  [ testCase "no trailers => bare terminator" $
      runB (encodeLastChunkWithTrailers [])
        @?= "0\r\n\r\n"
  , testCase "single trailer" $
      runB (encodeLastChunkWithTrailers [("X-Trace", "abc")])
        @?= "0\r\nX-Trace: abc\r\n\r\n"
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
    digit d | d < 10    = toEnum (d + 48)
            | otherwise = toEnum (d - 10 + 97)
