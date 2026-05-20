{-# LANGUAGE OverloadedStrings #-}
module Test.Encode (tests) where

import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP1.Encode
import Network.HTTP1.Status
import Network.HTTP1.Types
import Network.HTTP1.Version

tests :: TestTree
tests = testGroup "Encode"
  [ requestHeadTests
  , responseHeadTests
  ]

requestHeadTests :: TestTree
requestHeadTests = testGroup "request head"
  [ testCase "GET no body" $
      encodeRequestHead (Request GET "/" HTTP_1_1 [("Host","example.com")] BodyEmpty)
        @?= "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"
  , testCase "POST with bytes body adds CL" $
      encodeRequestHead (Request POST "/" HTTP_1_1 [("Host","x")] (BodyBytes "abcd"))
        @?= "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 4\r\n\r\n"
  , testCase "POST stream HTTP/1.1 adds TE chunked" $
      encodeRequestHead (Request POST "/" HTTP_1_1 [("Host","x")] (BodyStream (pure Nothing)))
        @?= "POST / HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n"
  , testCase "explicit CL is not duplicated" $
      encodeRequestHead (Request POST "/" HTTP_1_1
        [("Host","x"),("Content-Length","2")] (BodyBytes "ok"))
        @?= "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 2\r\n\r\n"
  ]

responseHeadTests :: TestTree
responseHeadTests = testGroup "response head"
  [ testCase "200 with bytes body" $
      encodeResponseHead (Response OK HTTP_1_1
        [("Content-Type", "text/plain")] (BodyBytes "hi"))
        @?= "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 2\r\n\r\n"
  , testCase "204 strips framing" $
      encodeResponseHead (Response NoContent HTTP_1_1 [] BodyEmpty)
        @?= "HTTP/1.1 204 No Content\r\n\r\n"
  , testCase "1.0 stream forces close header" $
      encodeResponseHead (Response OK HTTP_1_0 [] (BodyStream (pure Nothing)))
        @?= "HTTP/1.0 200 OK\r\nConnection: close\r\n\r\n"
  ]
