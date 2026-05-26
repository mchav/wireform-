{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Echo server for the Autobahn|Testsuite conformance run.

Listens on @127.0.0.1:9001@ and bounces every incoming text or
binary message back to the client.  The Autobahn @fuzzingclient@
will connect to this server and run the full RFC 6455 test suite
against it.  Configured in @test-conformance/config/fuzzingclient.json@.

This is a separate executable rather than a test-suite target so
the Autobahn runner (which lives outside cabal) can start and
stop it directly with a known port and no test harness noise.
-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BS8
import qualified System.Environment as Env
import System.IO

import Network.WebSocket.Message
import Network.WebSocket.Server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  port <- maybe "9001" id <$> Env.lookupEnv "WIREFORM_AUTOBAHN_PORT"
  putStrLn $ "wireform-websocket Autobahn echo server: listening on 127.0.0.1:" <> port
  runWebSocketServer defaultWebSocketServerConfig
    { wscHost              = "127.0.0.1"
    , wscPort              = port
    , wscHandler           = echo
    , wscOnException       = \req e -> do
        BS8.hPutStrLn stderr ("handler exception: " <> BS8.pack (show e))
        BS8.hPutStrLn stderr ("  for: " <> BS8.pack (show req))
    , wscOnHandshakeError  = \e ->
        BS8.hPutStrLn stderr ("handshake rejected: " <> BS8.pack (show e))
    , wscSelectSubProtocol = \_ -> Nothing
    , wscForkConnection    = forkIO
    }

-- | The simplest possible echo: 'receiveMessage' already drives
-- ping \u2192 pong autopilot and surfaces peer close via
-- 'WebSocketPeerClosed', so the only work the handler does is
-- echo each data message verbatim.
echo :: WebSocketHandler
echo _req conn = loop
  where
    loop = do
      r <- try @SomeException (receiveMessage conn defaultMessageLimit)
      case r of
        Left _                    -> pure ()
        Right (TextMessage t)     -> sendTextMessage   conn t   >> loop
        Right (BinaryMessage bs)  -> sendBinaryMessage conn bs  >> loop
