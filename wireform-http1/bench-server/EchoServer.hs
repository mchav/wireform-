{- | Echo server for HTTP\/1.x conformance probing.

For any HTTP method, returns the request body verbatim as the
response body with a @Content-Type: application\/octet-stream@ header.
This is the shape that the open conformance suites we wire up
expect:

  * @h1spec@ (uNetworking/h1spec, RFC 9112-focused, 33 tests)
  * @Http11Probe@ (MDA2AV/Http11Probe, RFC 9110+9112+security, 215 tests)

Both suites send a mix of well-formed and deliberately-malformed
requests over raw TCP and validate the response status against
ranges allowed by the RFC. The echo body lets them check that
chunked transfer-encoding, content-length framing, and body
delimitation round-trip correctly.
-}
module Main (main) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)

import Network.HTTP1.Server
import Network.HTTP1.Status
import Network.HTTP1.Types

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let port = case args of
        (p : _) -> p
        []      -> "8000"
  let cfg = defaultServerConfig
        { serverHost = "0.0.0.0"
        , serverPort = port
        , serverHandler = echoHandler
        , serverListenBacklog = 4096
        }
  putStrLn $ "wireform-http1-echo-server: listening on port " <> port
  runServer cfg

-- | Read whatever the request body is, return it verbatim.
echoHandler :: Handler
echoHandler req = do
  body <- drainAll (requestBody req)
  pure Response
    { responseStatus  = OK
    , responseVersion = HTTP_1_1
    , responseHeaders =
        [ ("Content-Type", "application/octet-stream")
        , ("Server", "wireform-http1")
        ]
    , responseBody = BodyBytes body
    }

-- | Pull every chunk out of a 'Body' producer and concatenate.
-- Defensive against a misbehaving body (catches exceptions and
-- returns whatever was read).
drainAll :: Body -> IO BS.ByteString
drainAll BodyEmpty = pure BS.empty
drainAll (BodyBytes bs) = pure bs
drainAll (BodyPreEncoded _) = pure BS.empty
drainAll (BodyStream producer) = go []
  where
    go acc = do
      r <- try @SomeException producer
      case r of
        Left _ -> pure (BS.concat (reverse acc))
        Right Nothing -> pure (BS.concat (reverse acc))
        Right (Just chunk) -> go (chunk : acc)
