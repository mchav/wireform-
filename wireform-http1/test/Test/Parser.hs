{-# LANGUAGE OverloadedStrings #-}
module Test.Parser (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Network.HTTP1.Parser
import Network.HTTP1.Types

tests :: TestTree
tests = testGroup "Parser"
  [ requestLineTests
  , statusLineTests
  , headerBlockTests
  , requestFramingTests
  , responseFramingTests
  , chunkHeaderTests
  , smugglingGuardTests
  ]

------------------------------------------------------------------------

requestLineTests :: TestTree
requestLineTests = testGroup "request line"
  [ testCase "GET / HTTP/1.1" $
      parseRequestLine "GET / HTTP/1.1"
        @?= Right (GET, "/", HTTP_1_1)
  , testCase "POST with absolute path" $
      parseRequestLine "POST /api/v1/things HTTP/1.1"
        @?= Right (POST, "/api/v1/things", HTTP_1_1)
  , testCase "extension method" $
      parseRequestLine "PROPFIND / HTTP/1.1"
        @?= Right (MethodOther "PROPFIND", "/", HTTP_1_1)
  , testCase "missing target" $
      parseRequestLine "GET  HTTP/1.1"
        @?= Left ParseBadRequestLine
  , testCase "unsupported version" $
      parseRequestLine "GET / HTTP/2.0"
        @?= Left ParseUnsupportedVersion
  ]

statusLineTests :: TestTree
statusLineTests = testGroup "status line"
  [ testCase "200 OK" $
      fmap dropReason (parseStatusLine "HTTP/1.1 200 OK")
        @?= Right (HTTP_1_1, Status 200)
  , testCase "404 Not Found" $
      fmap dropReason (parseStatusLine "HTTP/1.1 404 Not Found")
        @?= Right (HTTP_1_1, NotFound)
  , testCase "non-numeric code" $
      parseStatusLine "HTTP/1.1 OK OK" @?= Left ParseBadStatusLine
  ]
  where
    dropReason (a, b, _) = (a, b)

------------------------------------------------------------------------

headerBlockTests :: TestTree
headerBlockTests = testGroup "header block"
  [ testCase "single header" $
      parseHeaderBlock "Host: example.com"
        @?= Right [("Host", "example.com")]
  , testCase "two headers, CRLF separator" $
      parseHeaderBlock "Host: example.com\r\nAccept: */*"
        @?= Right [("Host", "example.com"), ("Accept", "*/*")]
  , testCase "OWS trimming on value" $
      parseHeaderBlock "X-Test:   spaces and tabs\t  "
        @?= Right [("X-Test", "spaces and tabs")]
  , testCase "rejects bare LF in value" $
      parseHeaderBlock "X-Bad: line1\nline2"
        @?= Left ParseInvalidHeaderValue
  , testCase "rejects empty name" $
      parseHeaderBlock ": value"
        @?= Left ParseBadHeaderName
  , testCase "rejects obs-fold" $
      parseHeaderBlock " continuation"
        @?= Left ParseInvalidHeaderValue
  , testCase "rejects NUL in value" $
      parseHeaderBlock "X: bad\x00stuff"
        @?= Left ParseInvalidHeaderValue
  , testCase "accepts obs-text (high bytes)" $
      let high = BS.pack [0x58, 0x3a, 0x20, 0xc3, 0xa9]
      in parseHeaderBlock high @?= Right [("X", BS.pack [0xc3, 0xa9])]
  ]

------------------------------------------------------------------------

requestFramingTests :: TestTree
requestFramingTests = testGroup "request framing"
  [ testCase "GET no body" $ runReq "GET / HTTP/1.1\r\nHost: x\r\n\r\n"
      @?= Right (GET, NoBody)
  , testCase "POST with content-length" $
      runReq "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 4\r\n\r\n"
        @?= Right (POST, ContentLength 4)
  , testCase "POST chunked" $
      runReq "POST / HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n"
        @?= Right (POST, Chunked)
  , testCase "TE with non-chunked-last is rejected" $
      runReq "POST / HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: gzip\r\n\r\n"
        @?= Left ParseChunkedNotFinal
  ]
  where
    runReq bs = fmap (\(r, f) -> (requestMethod r, f)) (parseHeadAndFraming bs)

parseHeadAndFraming :: ByteString -> Either ParseError (Request, Framing)
parseHeadAndFraming bs = case BS.breakSubstring "\r\n\r\n" bs of
  (block, rest)
    | BS.null rest -> parseRequest block
    | otherwise    -> parseRequest block

------------------------------------------------------------------------

responseFramingTests :: TestTree
responseFramingTests = testGroup "response framing"
  [ testCase "204 has no body even if CL present" $
      runResp GET "HTTP/1.1 204 No Content\r\nContent-Length: 5\r\n\r\n"
        @?= Right NoBody
  , testCase "HEAD has no body" $
      runResp HEAD "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n"
        @?= Right NoBody
  , testCase "1.0 no framing => close-delimited" $
      runResp GET "HTTP/1.0 200 OK\r\n\r\n"
        @?= Right CloseDelimited
  , testCase "1.1 no framing => zero-length" $
      runResp GET "HTTP/1.1 200 OK\r\n\r\n"
        @?= Right NoBody
  ]
  where
    runResp m bs = case BS.breakSubstring "\r\n\r\n" bs of
      (block, _) -> fmap snd (parseResponse m block)

------------------------------------------------------------------------

chunkHeaderTests :: TestTree
chunkHeaderTests = testGroup "chunked TE"
  [ testCase "0 size" $
      parseChunkHeader "0\r\n" 0
        @?= Right (Just (ChunkHeader 0 3))
  , testCase "hex size" $
      parseChunkHeader "ff\r\n" 0
        @?= Right (Just (ChunkHeader 0xff 4))
  , testCase "with extension" $
      parseChunkHeader "10;ext=value\r\n" 0
        @?= Right (Just (ChunkHeader 0x10 14))
  , testCase "needs more input" $
      parseChunkHeader "10" 0
        @?= Right Nothing
  , testCase "rejects bare CR in extension" $
      parseChunkHeader "10\rnotlf" 0
        @?= Left ParseBadChunkHeader
  , testProperty "decimal sizes round-trip via hex" $
      \n -> n >= 0 && n < (2^(40::Int)) ==>
        let hex = showHex' n ""
            input = BS.pack (map (fromIntegral . fromEnum) hex) <> "\r\n"
        in parseChunkHeader input 0
             == Right (Just (ChunkHeader (fromIntegral (n :: Int)) (length hex + 2)))
  ]

showHex' :: Int -> String -> String
showHex' n acc
  | n < 16    = digit n : acc
  | otherwise = showHex' (n `div` 16) (digit (n `mod` 16) : acc)
  where
    digit d | d < 10    = toEnum (d + 48)
            | otherwise = toEnum (d - 10 + 97)

------------------------------------------------------------------------
-- Request smuggling guards
------------------------------------------------------------------------

smugglingGuardTests :: TestTree
smugglingGuardTests = testGroup "request smuggling guards"
  [ testCase "CL + TE both present" $
      runFraming "POST / HTTP/1.1\r\nContent-Length: 4\r\nTransfer-Encoding: chunked\r\n\r\n"
        @?= Left ParseLengthAndTransferEncoding
  , testCase "duplicate disagreeing CL" $
      runFraming "POST / HTTP/1.1\r\nContent-Length: 4\r\nContent-Length: 5\r\n\r\n"
        @?= Left ParseLengthConflict
  , testCase "duplicate agreeing CL is fine" $
      runFraming "POST / HTTP/1.1\r\nContent-Length: 4\r\nContent-Length: 4\r\n\r\n"
        @?= Right (ContentLength 4)
  , testCase "non-numeric CL rejected" $
      runFraming "POST / HTTP/1.1\r\nContent-Length: lots\r\n\r\n"
        @?= Left ParseInvalidLength
  ]
  where
    runFraming :: ByteString -> Either ParseError Framing
    runFraming bs = case BS.breakSubstring "\r\n\r\n" bs of
      (block, _) -> fmap snd (parseRequest block)
