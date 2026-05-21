{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Standalone HTTP/2 server for manual testing with h2spec or curl.
--
-- Uses the wireform-http2 from-scratch implementation
-- (@Network.HTTP2.Server@). Listens on port 8080 and replies @200
-- text\/plain "hello from wireform-http2"@ to every request.
module Main where

import qualified Data.ByteString.Char8 as BC
import System.IO (hPutStrLn, stderr)

import Network.HTTP2.Server

main :: IO ()
main = do
  hPutStrLn stderr "wireform-http2 server listening on :8080"
  runServer defaultServerConfig
    { serverHost = "0.0.0.0"
    , serverPort = "8080"
    , serverHandler = \_req respond ->
        respond defaultResponse
          { responseStatus  = 200
          , responseHeaders = [("content-type", "text/plain")]
          , responseBody    = ResponseBodyBS (BC.pack "hello from wireform-http2")
          }
    }
