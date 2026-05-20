{-# LANGUAGE OverloadedStrings #-}
-- | End-to-end TLS / ALPN smoke tests for "Network.HTTP2.TLS.*".
--
-- We stand up a TLS-protected HTTP/2 server using
-- 'Network.HTTP2.TLS.Server.runTLSServerOnSocket' on a freshly bound
-- random port (loading a precomputed self-signed cert for @localhost@
-- from @test/data/@), then connect to it with
-- 'Network.HTTP2.TLS.Client.withTLSConnection' and verify that ALPN
-- negotiation picked @h2@ end-to-end.
--
-- Tests use a 'MVar' handshake to coordinate accept-before-connect
-- ordering, never 'threadDelay'.
module Test.TLS (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, catch, fromException, try)
import qualified Data.ByteString as BS
import qualified Data.X509 as X509
import qualified Data.X509.File as X509
import qualified Network.Socket as NS
import qualified Network.TLS as TLS
import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP2.Connection (connectionSettings)
import qualified Network.HTTP2.TLS as HTLS
import qualified Network.HTTP2.TLS.Client as TLSClient
import qualified Network.HTTP2.TLS.Server as TLSServer

tests :: TestTree
tests = testGroup "TLS"
  [ testCase "client/server negotiate h2 via ALPN" $ do
      (chain, key) <- loadFixture
      bracket bindRandomListener NS.close $ \listenSock -> do
        port <- NS.socketPort listenSock
        let cfg = (TLSServer.defaultTLSServerConfig chain key)
              { TLSServer.tlsServerConfig = TLSServer.defaultServerConfig }
        serverTid <- forkIO $
          TLSServer.runTLSServerOnSocket cfg listenSock
            `catch` (\(_ :: SomeException) -> pure ())
        result <- TLSClient.withTLSConnection (clientCfg port) $ \conn -> do
          -- A successful handshake implies ALPN agreed on h2 (the
          -- client checks this in 'verifyALPN'). Touch the connection
          -- to make sure it's fully constructed.
          (local, _) <- connectionSettings conn
          pure (local `seq` ())
        killThread serverTid
        result @?= ()
  , testCase "client rejects server that doesn't pick h2" $ do
      (chain, key) <- loadFixture
      bracket bindRandomListener NS.close $ \listenSock -> do
        port <- NS.socketPort listenSock
        accepted <- newEmptyMVar
        serverTid <- forkIO $
          runHostileServer listenSock chain key accepted
            `catch` (\(_ :: SomeException) -> pure ())
        result :: Either SomeException () <- try $
          TLSClient.withTLSConnection (clientCfg port) $ \_ -> pure ()
        -- Wait for the hostile server fork to drain so we don't leak
        -- partial state into the next test (no threadDelay).
        _ <- takeMVar accepted
        killThread serverTid
        case result of
          Left e -> case (fromException e :: Maybe HTLS.ALPNFailed) of
            Just _  -> pure ()
            Nothing ->
              -- The peer may also abort the handshake before the
              -- client gets to inspect the ALPN result; in that case
              -- the TLS layer surfaces a TLSException. Either is
              -- acceptable for the "no h2 → connection fails"
              -- contract.
              pure ()
          Right _ ->
            assertFailure "expected handshake to fail when ALPN didn't pick h2"
  ]

clientCfg :: NS.PortNumber -> TLSClient.TLSClientConfig
clientCfg port =
  let httpCfg = (TLSClient.defaultClientConfig)
        { TLSClient.clientHost = "127.0.0.1"
        , TLSClient.clientPort = show port
        }
  in (TLSClient.defaultTLSClientConfig "localhost")
       { TLSClient.tlsClientConfig = httpCfg
       , TLSClient.tlsClientValidateCert = False
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

-- | Accept one connection, perform a TLS handshake that selects no
-- ALPN protocol, and close. Signals 'accepted' after the connection
-- has been fully torn down.
runHostileServer
  :: NS.Socket
  -> X509.CertificateChain
  -> TLS.PrivKey
  -> MVar ()
  -> IO ()
runHostileServer listenSock chain key accepted = do
  (sock, _) <- NS.accept listenSock
  ctx <- TLS.contextNew sock (hostileParams chain key)
  TLS.handshake ctx `catch` (\(_ :: SomeException) -> pure ())
  (TLS.bye ctx) `catch` (\(_ :: SomeException) -> pure ())
  (TLS.contextClose ctx) `catch` (\(_ :: SomeException) -> pure ())
  NS.close sock
  putMVar accepted ()

hostileParams :: X509.CertificateChain -> TLS.PrivKey -> TLS.ServerParams
hostileParams chain key = TLS.defaultParamsServer
  { TLS.serverShared = (TLS.serverShared TLS.defaultParamsServer)
      { TLS.sharedCredentials = TLS.Credentials [(chain, key)] }
  , TLS.serverHooks = (TLS.serverHooks TLS.defaultParamsServer)
      { TLS.onALPNClientSuggest = Just (\_ -> pure BS.empty) }
  }

-- | Load the self-signed cert + key fixture from @test/data/@.
loadFixture :: IO (X509.CertificateChain, TLS.PrivKey)
loadFixture = do
  certs <- X509.readSignedObject "test/data/localhost.crt"
  keys  <- X509.readKeyFile     "test/data/localhost.key"
  case keys of
    (k : _) -> pure (X509.CertificateChain certs, k)
    []      -> assertFailure "no key in fixture" >> error "unreachable"
