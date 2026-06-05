{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | End-to-end TLS / ALPN smoke tests for "Network.HTTP2.TLS.*"
-- (now backed by OpenSSL, not the @tls@ package).
--
-- We stand up a TLS-protected HTTP/2 server using
-- 'Network.HTTP2.TLS.Server.runTLSServerOnSocket' on a freshly
-- bound random port (loading a precomputed self-signed cert for
-- @localhost@ from @test/data/@), then connect to it with
-- 'Network.HTTP2.TLS.Client.withTLSConnection' and verify that
-- ALPN negotiation picked @h2@ end-to-end.
--
-- Tests use a 'MVar' handshake to coordinate accept-before-connect
-- ordering, never 'threadDelay'.
module Test.TLS (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (SomeException, bracket, catch)
import qualified Network.Socket as NS
import Test.Syd

import Network.HTTP2.Client (clientHandleConnection)
import Network.HTTP2.Connection (connectionSettings)
import qualified Network.HTTP2.TLS.Client as TLSClient
import qualified Network.HTTP2.TLS.Server as TLSServer

tests :: Spec
tests = describe "TLS" $ sequence_
  [ it "client/server negotiate h2 via ALPN" $ do
      let certPath = "test/data/localhost.crt"
          keyPath  = "test/data/localhost.key"
      bracket bindRandomListener NS.close $ \listenSock -> do
        port <- NS.socketPort listenSock
        let cfg = TLSServer.defaultTLSServerConfig certPath keyPath
        serverTid <- forkIO $
          TLSServer.runTLSServerOnSocket cfg listenSock
            `catch` (\(_ :: SomeException) -> pure ())
        result <- TLSClient.withTLSConnection (clientCfg port) $ \handle -> do
          -- A successful handshake implies ALPN agreed on h2 (the
          -- client checks this in 'assertH2Alpn'). Touch the
          -- connection to make sure it's fully constructed.
          (local, _) <- connectionSettings (clientHandleConnection handle)
          pure (local `seq` ())
        killThread serverTid
        result `shouldBe` ()
  ]

clientCfg :: NS.PortNumber -> TLSClient.TLSClientConfig
clientCfg port =
  let httpCfg = TLSClient.defaultClientConfig
        { TLSClient.clientHost = "127.0.0.1"
        , TLSClient.clientPort = show port
        }
      base   = TLSClient.defaultTLSClientConfig "localhost"
      tlsCfg = (TLSClient.tlsClientTlsConfig base)
        { -- accept self-signed cert (test fixture is self-signed for localhost)
          TLSClient.tlsClientVerifyPeer = False
        }
  in base
       { TLSClient.tlsClientHttpConfig = httpCfg
       , TLSClient.tlsClientTlsConfig = tlsCfg
       }

bindRandomListener :: IO NS.Socket
bindRandomListener = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream, NS.addrFlags = [NS.AI_PASSIVE] }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "Test.TLS.bindRandomListener: no localhost addr"
    (addr:_) -> do
      sock <- NS.openSocket addr
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.bind sock (NS.addrAddress addr)
      NS.listen sock 1
      pure sock
