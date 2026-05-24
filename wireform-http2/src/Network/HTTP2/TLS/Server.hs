{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | HTTP\/2-over-TLS server with ALPN @h2@ negotiation, OpenSSL-backed.
--
-- Binds a TCP listener, performs the OpenSSL TLS handshake on each
-- accepted connection (advertising @h2@ via ALPN), then drives the
-- existing wireform-http2 'ServerConfig' machinery over the
-- resulting 'Wireform.Network.TLS.OpenSSL.SslConn'.
module Network.HTTP2.TLS.Server
  ( TLSServerConfig (..)
  , defaultTLSServerConfig
  , runTLSServer
  , runTLSServerOnSocket
    -- * Re-exports from "Network.HTTP2.Server"
  , ServerConfig (..)
  , defaultServerConfig
  , Request (..)
  , Response (..)
  , ResponseBody (..)
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, catch)
import qualified Network.Socket as NS

import Wireform.Network.TLS.OpenSSL
  ( SslCtx
  , freeCtx
  , newServer
  , setAlpnServer
  )
import Wireform.Network.TLS.Config
  ( TlsServerConfig (..)
  , buildServerCtx
  , defaultTlsServerConfig
  )

import Network.HTTP2.Server
import Network.HTTP2.TLS

-- | TLS-specific server configuration.  Wraps a 'TlsServerConfig'
-- (the OpenSSL knobs — cert\/key paths, ALPN, min protocol version)
-- alongside the HTTP\/2 'ServerConfig'.
data TLSServerConfig = TLSServerConfig
  { tlsServerHttpConfig :: !ServerConfig
  , tlsServerTlsConfig  :: !TlsServerConfig
  }

defaultTLSServerConfig
  :: FilePath        -- ^ cert chain (PEM)
  -> FilePath        -- ^ private key (PEM)
  -> TLSServerConfig
defaultTLSServerConfig cert key = TLSServerConfig
  { tlsServerHttpConfig = defaultServerConfig
  , tlsServerTlsConfig  = (defaultTlsServerConfig cert key)
      { tlsServerAlpn = [h2ProtocolId]
      }
  }

-- | Run a TLS-protected HTTP\/2 server.
--
-- Binds to @serverHost cfg : serverPort cfg@, accepts connections,
-- performs the TLS handshake (selecting @h2@ via ALPN), and drives
-- the HTTP\/2 server loop.  Connections that fail to negotiate
-- @h2@ are dropped after the handshake.
runTLSServer :: TLSServerConfig -> IO ()
runTLSServer cfg = do
  ctx <- buildCtx cfg
  let httpCfg = tlsServerHttpConfig cfg
      hints   = NS.defaultHints
                  { NS.addrFlags = [NS.AI_PASSIVE]
                  , NS.addrSocketType = NS.Stream
                  }
  addrs <- NS.getAddrInfo (Just hints)
                          (Just (serverHost httpCfg))
                          (Just (serverPort httpCfg))
  case addrs of
    [] -> error "Network.HTTP2.TLS.Server.runTLSServer: no bind address"
    (addr:_) -> bracket (NS.openSocket addr) NS.close $ \listenSock -> do
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.setSocketOption listenSock NS.NoDelay 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 128
      acceptLoop cfg ctx listenSock
        `catch` (\(_ :: SomeException) -> freeCtx ctx)
      freeCtx ctx

-- | Run the TLS server on an already-bound listening socket.
runTLSServerOnSocket :: TLSServerConfig -> NS.Socket -> IO ()
runTLSServerOnSocket cfg listenSock = do
  ctx <- buildCtx cfg
  acceptLoop cfg ctx listenSock `catch` (\(_ :: SomeException) -> freeCtx ctx)
  freeCtx ctx

acceptLoop :: TLSServerConfig -> SslCtx -> NS.Socket -> IO ()
acceptLoop cfg ctx listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- forkIO $
    handleTLSConnection cfg ctx clientSock
      `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop cfg ctx listenSock

handleTLSConnection :: TLSServerConfig -> SslCtx -> NS.Socket -> IO ()
handleTLSConnection cfg ctx sock = do
  conn <- newServer ctx sock
  assertH2Alpn conn
  transport <- tlsTransport conn
  runServerOnTransport (tlsServerHttpConfig cfg) transport

-- | Build the OpenSSL server context (PEM cert + key + ALPN
-- selector that picks @h2@).
buildCtx :: TLSServerConfig -> IO SslCtx
buildCtx cfg = do
  ctx <- buildServerCtx (tlsServerTlsConfig cfg)
  setAlpnServer ctx (tlsServerAlpn (tlsServerTlsConfig cfg))
  pure ctx
