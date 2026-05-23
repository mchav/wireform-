{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | HTTP\/2-over-TLS client with ALPN @h2@ negotiation, OpenSSL-backed.
--
-- Opens a TCP connection, performs the OpenSSL TLS handshake
-- (requesting @h2@ via ALPN), then drives the existing
-- wireform-http2 'Connection' machinery over the resulting
-- 'Wireform.Network.TLS.OpenSSL.SslConn'.
module Network.HTTP2.TLS.Client
  ( TLSClientConfig (..)
  , defaultTLSClientConfig
  , withTLSConnection
    -- * Re-exports from "Network.HTTP2.Client"
  , ClientConfig (..)
  , defaultClientConfig
  , ClientRequest (..)
  , ClientResponse (..)
  , ClientHandle
  , sendRequest
    -- * Re-exports from "Wireform.Network.TLS.Config"
  , TlsClientConfig (..)
  ) where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS8
import qualified Network.Socket as NS

import Wireform.Network.TLS.OpenSSL
  ( freeCtx
  , freeConn
  , newClient
  , setClientHostnameVerify
  )
import Wireform.Network.TLS.Config
  ( TlsClientConfig (..)
  , buildClientCtx
  , defaultTlsClientConfig
  )

import Network.HTTP2.Client
  ( ClientConfig (..)
  , ClientHandle
  , ClientRequest (..)
  , ClientResponse (..)
  , defaultClientConfig
  , sendRequest
  , withConnectionOnTransport
  )
import Network.HTTP2.TLS

-- | TLS-specific client configuration.
data TLSClientConfig = TLSClientConfig
  { tlsClientHttpConfig  :: !ClientConfig
  , tlsClientServerName  :: !String
    -- ^ Server name for SNI + cert hostname verification.  For
    -- most callers this is the same as 'clientHost'.
  , tlsClientTlsConfig   :: !TlsClientConfig
  }

defaultTLSClientConfig :: String -> TLSClientConfig
defaultTLSClientConfig serverName = TLSClientConfig
  { tlsClientHttpConfig = defaultClientConfig
  , tlsClientServerName = serverName
  , tlsClientTlsConfig  = defaultTlsClientConfig
      { tlsClientAlpn = [h2ProtocolId]
      }
  }

-- | Open a TLS-protected HTTP\/2 connection.
--
-- Connects to @clientHost cfg : clientPort cfg@, performs the TLS
-- handshake (ALPN @h2@), and runs the supplied action with the
-- live 'ClientHandle'.  Throws 'ALPNFailed' if the server refused
-- to speak @h2@; throws 'Wireform.Network.TLS.OpenSSL.OpenSslError'
-- if the handshake itself fails.
withTLSConnection :: TLSClientConfig -> (ClientHandle -> IO a) -> IO a
withTLSConnection cfg action = do
  let httpCfg = tlsClientHttpConfig cfg
      hints   = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrs <- NS.getAddrInfo (Just hints)
                          (Just (clientHost httpCfg))
                          (Just (clientPort httpCfg))
  case addrs of
    [] -> error "Network.HTTP2.TLS.Client.withTLSConnection: no addresses for host"
    (addr:_) -> bracket (NS.openSocket addr) NS.close $ \sock -> do
      NS.connect sock (NS.addrAddress addr)
      NS.setSocketOption sock NS.NoDelay 1
      ctx <- buildClientCtx (tlsClientTlsConfig cfg)
      conn <- newClient ctx sock (Just (BS8.pack (tlsClientServerName cfg)))
      _ <- if tlsClientVerifyPeer (tlsClientTlsConfig cfg)
             then setClientHostnameVerify conn
                    (BS8.pack (tlsClientServerName cfg))
             else pure ()
      assertH2Alpn conn
      transport <- tlsTransport conn
      r <- withConnectionOnTransport httpCfg transport (Just sock) action
      freeConn conn
      freeCtx ctx
      pure r
