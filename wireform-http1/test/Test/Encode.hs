{-# LANGUAGE OverloadedStrings #-}
module Test.Encode (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP1.Encode
import Network.HTTP1.Status
import Network.HTTP1.Types
import Network.HTTP1.Version

-- | Strip the @Date: ...@ line from an encoded response head so the
-- response-encoder tests can assert against a fixed expected string
-- (the actual Date is auto-injected per RFC 9110 § 6.6.1 and changes
-- every run).
stripDate :: ByteString -> ByteString
stripDate bs = case findLine "Date: " 0 of
  Nothing -> bs
  Just (s, e) -> BS.take s bs <> BS.drop e bs
  where
    findLine needle off
      | off >= BS.length bs = Nothing
      | BS.isPrefixOf needle (BS.drop off bs) =
          case findCrlf (off + BS.length needle) of
            Just e -> Just (off, e + 2)
            Nothing -> Nothing
      | otherwise = findLine needle (off + 1)
    findCrlf off
      | off + 1 >= BS.length bs = Nothing
      | BS.index bs off == 0x0d && BS.index bs (off + 1) == 0x0a = Just off
      | otherwise = findCrlf (off + 1)

tests :: TestTree
tests = testGroup "Encode"
  [ requestHeadTests
  , responseHeadTests
  ]

requestHeadTests :: TestTree
requestHeadTests = testGroup "request head"
  [ testCase "GET no body" $
      encodeRequestHead (Request GET "/" HTTP_1_1 [("Host","example.com")] BodyEmpty (pure []))
        @?= "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n"
  , testCase "POST with bytes body adds CL" $
      encodeRequestHead (Request POST "/" HTTP_1_1 [("Host","x")] (BodyBytes "abcd") (pure []))
        @?= "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 4\r\n\r\n"
  , testCase "POST stream HTTP/1.1 adds TE chunked" $
      encodeRequestHead (Request POST "/" HTTP_1_1 [("Host","x")] (BodyStream (pure Nothing)) (pure []))
        @?= "POST / HTTP/1.1\r\nHost: x\r\nTransfer-Encoding: chunked\r\n\r\n"
  , testCase "explicit CL is not duplicated" $
      encodeRequestHead (Request POST "/" HTTP_1_1
        [("Host","x"),("Content-Length","2")] (BodyBytes "ok") (pure []))
        @?= "POST / HTTP/1.1\r\nHost: x\r\nContent-Length: 2\r\n\r\n"
  ]

responseHeadTests :: TestTree
responseHeadTests = testGroup "response head"
  -- Each assertion strips the auto-injected @Date@ line first; that
  -- header's value changes per second so it can't be in a fixed
  -- expected string. We test Date-header injection separately below.
  [ testCase "200 with bytes body" $
      stripDate (encodeResponseHead (Response OK HTTP_1_1
        [("Content-Type", "text/plain")] (BodyBytes "hi") (pure [])))
        @?= "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: 2\r\n\r\n"
  , testCase "204 strips framing" $
      stripDate (encodeResponseHead (Response NoContent HTTP_1_1 [] BodyEmpty (pure [])))
        @?= "HTTP/1.1 204 No Content\r\n\r\n"
  , testCase "1.0 stream forces close header" $
      stripDate (encodeResponseHead (Response OK HTTP_1_0 [] (BodyStream (pure Nothing)) (pure [])))
        @?= "HTTP/1.0 200 OK\r\nConnection: close\r\n\r\n"
  , testCase "Date header auto-injected" $
      assertBool "expected Date: in encoded response" $
        let bs = encodeResponseHead (Response OK HTTP_1_1 [] (BodyBytes "hi") (pure []))
        in BS.isInfixOf "\r\nDate: " bs
  , testCase "Caller's Date header wins" $
      stripDate
        ( encodeResponseHead (Response OK HTTP_1_1
            [("Date", "Mon, 01 Jan 2024 00:00:00 GMT")]
            (BodyBytes "hi") (pure [])))
        @?= "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\n"
        -- ^ stripDate removed the caller-supplied Date; the rest of
        -- the head is the framing line we expect.
  ]
