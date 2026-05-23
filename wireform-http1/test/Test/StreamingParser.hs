{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Test.StreamingParser (tests) where

import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Wireform.Network (chunkedRecvFn, withRecvBufTransport)
import Wireform.Parser (Parser)
import Wireform.Parser.Driver (runParser)
import Wireform.Parser.Error (ParseError)
import Wireform.Parser.Internal (Stream)
import Wireform.Transport.Config (defaultTransportConfig)

import Network.HTTP1.Method (Method (..))
import Network.HTTP1.Parser (Framing (..))
import Network.HTTP1.StreamingParser
  ( StreamParseError
  , chunkSizeLineParser
  , headerBlockParser
  , requestHeadParser
  , requestLineParser
  , responseHeadParser
  , statusLineParser
  )
import Network.HTTP1.Status (Status (..))
import Network.HTTP1.Types (Request (..), Response (..))
import Network.HTTP1.Version (Version (..))

runOn
  :: forall a
   . [BS.ByteString]
  -> Parser Stream StreamParseError a
  -> IO (Either (ParseError StreamParseError) a)
runOn chunks parser = do
  recvFn <- chunkedRecvFn chunks
  withRecvBufTransport defaultTransportConfig recvFn $ \t ->
    runParser t parser

tests :: TestTree
tests = testGroup "StreamingParser"
  [ testCase "requestLineParser parses GET / HTTP/1.1" $ do
      r <- runOn ["GET / HTTP/1.1\r\n"] requestLineParser
      case r of
        Right (m, t, v) -> do
          m @?= GET
          t @?= "/"
          v @?= HTTP_1_1
        Left e -> assertFailure ("parse failed: " <> show e)

  , testCase "requestLineParser stitches across chunks" $ do
      r <- runOn ["GE", "T /foo H", "TTP/1.1\r\n"] requestLineParser
      case r of
        Right (m, t, v) -> do
          m @?= GET
          t @?= "/foo"
          v @?= HTTP_1_1
        Left e -> assertFailure ("parse failed: " <> show e)

  , testCase "statusLineParser parses 200 OK" $ do
      r <- runOn ["HTTP/1.1 200 OK\r\n"] statusLineParser
      case r of
        Right (v, Status c, _reason) -> do
          v @?= HTTP_1_1
          c @?= 200
        Left e -> assertFailure ("parse failed: " <> show e)

  , testCase "statusLineParser raises ParseUnsupportedVersion on HTTP/1.2" $ do
      r <- runOn ["HTTP/1.2 200 OK\r\n"] statusLineParser
      case r of
        Right _ -> assertFailure "expected unsupported-version error"
        Left _ -> pure ()

  , testCase "headerBlockParser parses a simple block" $ do
      r <- runOn ["host: example.com\r\nx-foo: bar\r\n\r\n"] headerBlockParser
      case r of
        Right hs -> hs @?= [("host", "example.com"), ("x-foo", "bar")]
        Left e   -> assertFailure ("parse failed: " <> show e)

  , testCase "headerBlockParser rejects obs-fold (leading SP / HTAB)" $ do
      r <- runOn [" host: example.com\r\n\r\n"] headerBlockParser
      case r of
        Right _ -> assertFailure "expected obs-fold rejection"
        Left _ -> pure ()

  , testCase "requestHeadParser parses request + framing" $ do
      let bs = "POST /api HTTP/1.1\r\nhost: x\r\ncontent-length: 12\r\n\r\n"
      r <- runOn [bs] requestHeadParser
      case r of
        Right (req, framing) -> do
          requestMethod req  @?= POST
          requestTarget req  @?= "/api"
          requestVersion req @?= HTTP_1_1
          framing            @?= ContentLength 12
        Left e -> assertFailure ("parse failed: " <> show e)

  , testCase "requestHeadParser raises ParseMissingHost on bare 1.1 GET" $ do
      let bs = "GET / HTTP/1.1\r\nx-y: z\r\n\r\n"
      r <- runOn [bs] requestHeadParser
      case r of
        Right _ -> assertFailure "expected missing-Host error"
        Left _ -> pure ()

  , testCase "responseHeadParser handles HEAD framing" $ do
      let bs = "HTTP/1.1 200 OK\r\ncontent-length: 999\r\n\r\n"
      r <- runOn [bs] (responseHeadParser HEAD)
      case r of
        Right (_resp, framing) -> framing @?= NoBody
        Left e -> assertFailure ("parse failed: " <> show e)

  , testCase "chunkSizeLineParser parses hex + extensions" $ do
      r <- runOn ["10;name=val\r\n"] chunkSizeLineParser
      case r of
        Right n -> n @?= 0x10
        Left e  -> assertFailure ("parse failed: " <> show e)

  , testCase "chunkSizeLineParser parses simple size" $ do
      r <- runOn ["1a\r\n"] chunkSizeLineParser
      case r of
        Right n -> n @?= 0x1a
        Left e  -> assertFailure ("parse failed: " <> show e)
  ]
