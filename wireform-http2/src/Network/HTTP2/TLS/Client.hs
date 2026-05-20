-- | HTTP/2-over-TLS client with ALPN @h2@ negotiation.
--
-- This is the @tls@-backed analogue of "Network.HTTP2.Client": it
-- opens a TCP connection, performs the TLS handshake (requesting
-- @h2@ via ALPN), then drives the existing wireform-http2
-- 'Connection' machinery over the resulting TLS context.
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
  ) where

import Control.Exception (bracket, throwIO)
import qualified Data.ByteString.Char8 as BS8
import qualified Network.Socket as NS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

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

-- | TLS-specific client configuration. The non-TLS HTTP/2 knobs come
-- from the embedded 'ClientConfig'.
data TLSClientConfig = TLSClientConfig
  { tlsClientConfig :: !ClientConfig
  , tlsClientServerName :: !String
    -- ^ Server name for SNI + X509 hostname validation. For most
    -- callers this is the same as 'clientHost'.
  , tlsClientValidateCert :: !Bool
    -- ^ Whether to validate the server certificate. Defaults to
    -- 'True'; flip to 'False' only for self-signed test servers.
  , tlsClientParamsOverride :: TLS.ClientParams -> TLS.ClientParams
    -- ^ Last-mile escape hatch: mutate the 'TLS.ClientParams' built by
    -- 'defaultTLSClientConfig' (e.g. to inject a custom validation
    -- cache, additional ciphers, or a key logger).
  }

defaultTLSClientConfig :: String -> TLSClientConfig
defaultTLSClientConfig serverName = TLSClientConfig
  { tlsClientConfig = defaultClientConfig
  , tlsClientServerName = serverName
  , tlsClientValidateCert = True
  , tlsClientParamsOverride = id
  }

-- | Open a TLS-protected HTTP/2 connection.
--
-- Connects to @clientHost cfg : clientPort cfg@, performs the TLS
-- handshake (ALPN @h2@), and runs the supplied action with the live
-- 'ClientHandle'. Throws 'ALPNFailed' if the server refused to speak
-- @h2@; throws 'TLS.TLSException' if the handshake itself fails.
withTLSConnection :: TLSClientConfig -> (ClientHandle -> IO a) -> IO a
withTLSConnection cfg action = do
  let httpCfg = tlsClientConfig cfg
      hints   = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrs <- NS.getAddrInfo (Just hints) (Just (clientHost httpCfg)) (Just (clientPort httpCfg))
  case addrs of
    [] -> error "Network.HTTP2.TLS.Client.withTLSConnection: no addresses for host"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.connect sock (NS.addrAddress addr)
        NS.setSocketOption sock NS.NoDelay 1
        ctx <- TLS.contextNew sock (buildClientParams cfg)
        TLS.handshake ctx
        verifyALPN ctx
        transport <- tlsTransport ctx
        withConnectionOnTransport httpCfg transport (Just sock) action

-- | Build a 'TLS.ClientParams' that requests @h2@ via ALPN and
-- honours 'tlsClientValidateCert'.
buildClientParams :: TLSClientConfig -> TLS.ClientParams
buildClientParams cfg = tlsClientParamsOverride cfg base
  where
    serverName = tlsClientServerName cfg
    base0 = TLS.defaultParamsClient serverName (BS8.pack "")
    base = base0
      { TLS.clientShared = (TLS.clientShared base0)
          { TLS.sharedValidationCache =
              if tlsClientValidateCert cfg
                then TLS.sharedValidationCache (TLS.clientShared base0)
                else TLS.ValidationCache
                       (\_ _ _ -> pure TLS.ValidationCachePass)
                       (\_ _ _ -> pure ())
          }
      , TLS.clientSupported = (TLS.clientSupported base0)
          { TLS.supportedCiphers = TLS.ciphersuite_default
          }
      , TLS.clientHooks = (TLS.clientHooks base0)
          { TLS.onSuggestALPN = pure (Just [h2ProtocolId])
          }
      , TLS.clientUseServerNameIndication = True
      }

verifyALPN :: TLS.Context -> IO ()
verifyALPN ctx = do
  negotiated <- TLS.getNegotiatedProtocol ctx
  case negotiated of
    Just p | p == h2ProtocolId -> pure ()
    other -> throwIO (ALPNFailed other)
