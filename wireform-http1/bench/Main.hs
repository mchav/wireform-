{-# LANGUAGE OverloadedStrings #-}

{- | Micro-benchmarks for the wireform-http1 hot paths.

These cover the parser, the encoder, and the chunked TE codec. The
full end-to-end \"server hello world\" benchmark lives in
@bench-server@ and should be driven with @wrk@ or @h2load@.
-}
module Main (main) where

import Criterion.Main
import Data.ByteString qualified as BS
import Network.HTTP1.Encode
import Network.HTTP1.Parser
import Network.HTTP1.Types


main :: IO ()
main =
  defaultMain
    [ bgroup
        "parser"
        [ bench "parseRequest (GET small)" $
            nf parseRequest smallGet
        , bench "parseRequest (POST with 10 headers)" $
            nf parseRequest postBig
        , bench "parseHeaderBlock (10 headers)" $
            nf parseHeaderBlock headersBlock
        , bench "parseChunkHeader" $
            nf (\bs -> parseChunkHeader bs 0) "ff\r\n"
        ]
    , bgroup
        "encode"
        [ bench "encodeRequestHead (GET small)" $
            nf encodeRequestHead getReq
        , bench "encodeResponseHead (200 small)" $
            nf encodeResponseHead okResp
        ]
    , bgroup
        "date"
        [ -- The Date header is auto-injected per response.  We exercise
          -- it via 'encodeResponseHead' above; this group measures the
          -- raw lookup cost so a regression on the tick-thread cache
          -- shows up as a discrete benchmark instead of a barely-visible
          -- shift in the encode benchmark.
          bench "cachedHttpDate" $ nf cachedHttpDate ()
        ]
    ]
  where
    smallGet :: BS.ByteString
    smallGet = "GET / HTTP/1.1\r\nHost: example.com\r\nAccept: */*"

    postBig :: BS.ByteString
    postBig =
      BS.intercalate
        "\r\n"
        [ "POST /api/v1/things HTTP/1.1"
        , "Host: example.com"
        , "User-Agent: curl/8.4.0"
        , "Accept: */*"
        , "Content-Type: application/json"
        , "Content-Length: 0"
        , "Cache-Control: no-cache"
        , "Pragma: no-cache"
        , "X-Forwarded-For: 1.2.3.4"
        , "X-Forwarded-Proto: https"
        , "X-Request-Id: 0123456789abcdef"
        ]

    headersBlock :: BS.ByteString
    headersBlock =
      BS.intercalate
        "\r\n"
        [ "Host: example.com"
        , "User-Agent: curl/8.4.0"
        , "Accept: */*"
        , "Content-Type: application/json"
        , "Content-Length: 4"
        , "Cache-Control: no-cache"
        , "Pragma: no-cache"
        , "X-Forwarded-For: 1.2.3.4"
        , "X-Forwarded-Proto: https"
        , "X-Request-Id: 0123456789abcdef"
        ]

    getReq = Request GET "/" HTTP_1_1 [("Host", "example.com"), ("Accept", "*/*")] BodyEmpty (pure [])
    okResp =
      Response
        OK
        HTTP_1_1
        [("Content-Type", "text/plain"), ("Server", "wireform-http1")]
        (BodyBytes "Hello, world!\n")
        (pure [])
