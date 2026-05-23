{- | TLS-with-ALPN negotiation glue for "Network.HTTP.Client" \/
"Network.HTTP.Server".

The TLS handshake is shared between HTTP\/1.x and HTTP\/2; once it
completes the ALPN-negotiated protocol determines which runtime
drives the connection.  Backed by OpenSSL through
"Wireform.Network.TLS.OpenSSL".

If ALPN picks a protocol that isn't covered by the configured
'VersionRange' (e.g. the server only offered @http\/1.1@ but the
client requested 'http2Only'), 'VersionOutOfRange' is raised; if it
picks something we don't understand at all, 'TlsNoAlpnOverlap' is
raised.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.TLS
  ( -- * Client
    TlsClient (..)
  , withTlsClient
    -- * Server
  , runTlsServer
    -- * Errors
  , TlsHandshakeError (..)
  ) where

import Control.Concurrent (forkIO)
import Control.Exception
  (Exception, SomeException, bracket, catch, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Network.Socket as NS

import Wireform.Network.TLS.OpenSSL
  ( SslCtx
  , SslConn
  , freeConn
  , freeCtx
  , getAlpn
  , newClient
  , newServer
  , setAlpnClient
  , setAlpnServer
  , setClientHostnameVerify
  )
import Wireform.Network.TLS.Config
  ( TlsClientConfig (..)
  , TlsServerConfig (..)
  , buildClientCtx
  , buildServerCtx
  , defaultTlsClientConfig
  , defaultTlsServerConfig
  )

import qualified Network.HTTP1.Connection as H1C
import qualified Network.HTTP1.Client as H1
import qualified Network.HTTP2.Client as H2
import qualified Network.HTTP2.Server as H2
import qualified Network.HTTP2.TLS as H2TLS

import Network.HTTP.VersionRange
import qualified Network.HTTP.Types.Version as U

-- | Errors raised by the TLS adapter that aren't already
-- 'Wireform.Network.TLS.OpenSSL.OpenSslError' or
-- 'Network.HTTP2.TLS.ALPNFailed'.
data TlsHandshakeError
  = TlsNoAlpnOverlap !(Maybe ByteString) !VersionRange
    -- ^ The peer didn't pick anything from our ALPN list (the
    -- peer-selected protocol, if any, is in the first field).
  | TlsCertNotFound !FilePath
  | TlsHostnameMismatch !String !String
  deriving stock (Show)

instance Exception TlsHandshakeError

-- | A live TLS-protected client connection.
data TlsClient
  = TlsClientHttp2 !H2.ClientHandle
  | TlsClientHttp1 !H1.ClientConnection

------------------------------------------------------------------------
-- Client
------------------------------------------------------------------------

-- | Connect to @host:port@ over TLS, advertise the supplied
-- 'VersionRange' via ALPN, and run @action@ with the
-- per-protocol connection handle.
withTlsClient
  :: String        -- ^ host
  -> String        -- ^ port
  -> String        -- ^ TLS server name (SNI \/ X.509)
  -> Bool          -- ^ validate cert
  -> VersionRange
  -> (TlsClient -> IO a)
  -> IO a
withTlsClient host port serverName validateCert range action = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
      alpnList = versionAlpnProtocols range
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> error "Network.HTTP.TLS.withTlsClient: no addresses for host"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.connect sock (NS.addrAddress addr)
        NS.setSocketOption sock NS.NoDelay 1
        bracket (buildCtxClient validateCert alpnList) freeCtx $ \ctx -> do
          conn <- newClient ctx sock (Just (BS8.pack serverName))
          _ <- if validateCert
                 then setClientHostnameVerify conn (BS8.pack serverName)
                 else pure ()
          neg <- getAlpn conn
          dispatch host port range conn neg sock action

dispatch
  :: String -> String -> VersionRange
  -> SslConn -> Maybe ByteString -> NS.Socket
  -> (TlsClient -> IO a)
  -> IO a
dispatch host port range conn neg sock action =
  case neg >>= versionForAlpn of
    Nothing -> do
      freeConn conn
      throwIO (TlsNoAlpnOverlap neg range)
    Just v
      | not (versionAllowed v range) -> do
          freeConn conn
          throwIO (VersionOutOfRange (Just v) range)
      | v == U.HTTP2 -> do
          transport <- H2TLS.tlsTransport conn
          let h2cfg = H2.defaultClientConfig
                { H2.clientHost = host
                , H2.clientPort = port
                }
          r <- H2.withConnectionOnTransport h2cfg transport (Just sock)
                 (action . TlsClientHttp2)
          freeConn conn
          pure r
      | otherwise ->
          bracket
            (H1C.newConnectionFromTls conn)
            H1C.closeConnection
            (\c -> action (TlsClientHttp1 (H1.ClientConnection c)))

buildCtxClient :: Bool -> [ByteString] -> IO SslCtx
buildCtxClient validateCert alpnList = do
  let cfg = defaultTlsClientConfig
        { tlsClientVerifyPeer = validateCert
        , tlsClientAlpn       = alpnList
        }
  ctx <- buildClientCtx cfg
  case alpnList of
    [] -> pure ()
    _  -> setAlpnClient ctx alpnList
  pure ctx

------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------

-- | Bind a TLS-protected listener.  ALPN advertises the protocols in
-- the supplied 'VersionRange'; the server picks the
-- highest-preference protocol the client also supports.
-- Connections that don't negotiate an in-range protocol are dropped.
runTlsServer
  :: String          -- ^ host
  -> String          -- ^ port
  -> FilePath        -- ^ certificate chain (PEM)
  -> FilePath        -- ^ private key (PEM)
  -> VersionRange
  -> (U.Version -> SslConn -> IO ())
     -- ^ HTTP\/1.x dispatch (called on ALPN @http\/1.0@ or @http\/1.1@).
  -> (U.Version -> H2.ServerConfig)
     -- ^ HTTP\/2 'ServerConfig' factory (called on ALPN @h2@); the
     -- resulting server runs on the live TLS connection.
  -> IO ()
runTlsServer host port certPath keyPath range http1Handler http2CfgFor = do
  let cfg = (defaultTlsServerConfig certPath keyPath)
        { tlsServerAlpn = versionAlpnProtocols range
        }
  ctx <- buildServerCtx cfg
  let _ = http1Handler  -- ensure no -Wunused-matches
  -- ^ ALPN selector picks the first server-advertised protocol the
  -- client also offered; setAlpnServer takes the cfg's tlsServerAlpn
  -- which buildServerCtx already wired into the SSL_CTX.
  setAlpnServer ctx (tlsServerAlpn cfg)
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> error "Network.HTTP.TLS.runTlsServer: no bind address"
    (addr:_) -> bracket (NS.openSocket addr) NS.close $ \listenSock -> do
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.setSocketOption listenSock NS.NoDelay 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 128
      acceptLoop listenSock ctx `catch` (\(_ :: SomeException) -> freeCtx ctx)
      freeCtx ctx
  where
    acceptLoop listenSock ctx = do
      (clientSock, _) <- NS.accept listenSock
      NS.setSocketOption clientSock NS.NoDelay 1
      _ <- forkIO $ handleConn ctx clientSock
        `catch` (\(_ :: SomeException) -> NS.close clientSock)
      acceptLoop listenSock ctx

    handleConn ctx sock = do
      conn <- newServer ctx sock
      neg <- getAlpn conn
      case neg >>= versionForAlpn of
        Nothing -> do
          freeConn conn
          NS.close sock
        Just v
          | not (versionAllowed v range) -> do
              freeConn conn
              NS.close sock
          | v == U.HTTP2 -> do
              transport <- H2TLS.tlsTransport conn
              H2.runServerOnTransport (http2CfgFor v) transport
              freeConn conn
          | otherwise -> do
              http1Handler v conn
              freeConn conn
