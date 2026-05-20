-- | HTTP/2-over-TLS server with ALPN @h2@ negotiation.
--
-- The @tls@-backed analogue of "Network.HTTP2.Server": binds a TCP
-- listener, performs the TLS handshake on each accepted connection
-- (advertising @h2@ via ALPN), then drives the existing wireform-http2
-- 'ServerConfig' machinery over the resulting TLS context.
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
import Data.ByteString (ByteString)
import Data.X509 (CertificateChain)
import qualified Network.Socket as NS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import Network.HTTP2.Server
import Network.HTTP2.TLS

-- | TLS-specific server configuration.
data TLSServerConfig = TLSServerConfig
  { tlsServerConfig :: !ServerConfig
  , tlsServerCertChain :: !CertificateChain
    -- ^ Server certificate chain to present to clients.
  , tlsServerPrivateKey :: !TLS.PrivKey
    -- ^ Private key matching the leaf certificate.
  , tlsServerParamsOverride :: TLS.ServerParams -> TLS.ServerParams
    -- ^ Escape hatch for callers who need to tweak the
    -- 'TLS.ServerParams' (e.g. require client certs, add additional
    -- supported groups).
  }

defaultTLSServerConfig
  :: CertificateChain
  -> TLS.PrivKey
  -> TLSServerConfig
defaultTLSServerConfig chain key = TLSServerConfig
  { tlsServerConfig = defaultServerConfig
  , tlsServerCertChain = chain
  , tlsServerPrivateKey = key
  , tlsServerParamsOverride = id
  }

-- | Run a TLS-protected HTTP/2 server.
--
-- Binds to @serverHost cfg : serverPort cfg@, accepts connections,
-- performs the TLS handshake (selecting @h2@ via ALPN), and drives the
-- HTTP/2 server loop. Connections that fail to negotiate @h2@ are
-- dropped after the handshake (we do not fall back to HTTP/1.1).
runTLSServer :: TLSServerConfig -> IO ()
runTLSServer cfg = do
  let httpCfg = tlsServerConfig cfg
      hints   = NS.defaultHints
                  { NS.addrFlags = [NS.AI_PASSIVE]
                  , NS.addrSocketType = NS.Stream
                  }
  addrs <- NS.getAddrInfo (Just hints) (Just (serverHost httpCfg)) (Just (serverPort httpCfg))
  case addrs of
    [] -> error "Network.HTTP2.TLS.Server.runTLSServer: no bind address"
    (addr:_) -> bracket (NS.openSocket addr) NS.close $ \listenSock -> do
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.setSocketOption listenSock NS.NoDelay 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 128
      acceptLoop cfg listenSock

-- | Run the TLS server on an already-bound listening socket. Useful
-- when the caller wants to choose the port via @bind 0@ and then hand
-- the live socket off to the server (e.g. in tests that need to know
-- the port up front, without a race window).
runTLSServerOnSocket :: TLSServerConfig -> NS.Socket -> IO ()
runTLSServerOnSocket = acceptLoop

acceptLoop :: TLSServerConfig -> NS.Socket -> IO ()
acceptLoop cfg listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- forkIO $
    handleTLSConnection cfg clientSock
      `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop cfg listenSock

handleTLSConnection :: TLSServerConfig -> NS.Socket -> IO ()
handleTLSConnection cfg sock = do
  ctx <- TLS.contextNew sock (buildServerParams cfg)
  TLS.handshake ctx
  negotiated <- TLS.getNegotiatedProtocol ctx
  case negotiated of
    Just p | p == h2ProtocolId -> do
      transport <- tlsTransport ctx
      runServerOnTransport (tlsServerConfig cfg) transport
    _ -> do
      (TLS.bye ctx) `catch` swallow
      (TLS.contextClose ctx) `catch` swallow
      NS.close sock
  where
    swallow :: SomeException -> IO ()
    swallow _ = pure ()

-- | Build 'TLS.ServerParams' that advertise @h2@ via ALPN and accept
-- @h2@ when offered by the client.
buildServerParams :: TLSServerConfig -> TLS.ServerParams
buildServerParams cfg = tlsServerParamsOverride cfg base
  where
    base = TLS.defaultParamsServer
      { TLS.serverShared = (TLS.serverShared TLS.defaultParamsServer)
          { TLS.sharedCredentials = TLS.Credentials
              [(tlsServerCertChain cfg, tlsServerPrivateKey cfg)]
          }
      , TLS.serverSupported = (TLS.serverSupported TLS.defaultParamsServer)
          { TLS.supportedCiphers = TLS.ciphersuite_default
          }
      , TLS.serverHooks = (TLS.serverHooks TLS.defaultParamsServer)
          { TLS.onALPNClientSuggest = Just selectALPN
          }
      }

-- | ALPN selector. Pick @h2@ when offered; otherwise return the empty
-- bytestring (no protocol selected) and drop the connection in
-- 'handleTLSConnection'.
selectALPN :: [ByteString] -> IO ByteString
selectALPN offered
  | h2ProtocolId `elem` offered = pure h2ProtocolId
  | otherwise = pure mempty
