{-# LANGUAGE BlockArguments #-}

{- | OpenSSL-flavoured TLS configuration records.

Designed to model the slice of TLS configuration the per-protocol
packages ('wireform-kafka', 'wireform-http1', 'wireform-http2')
actually need: which CAs to trust, whether to verify the peer,
what hostname to assert (SNI + cert pinning), what ALPN protocols
to advertise, and whether to present a client certificate.

These records compile down to a configured 'Wireform.Network.TLS.OpenSSL.SslCtx';
the 'applyClient' / 'applyServer' helpers do the imperative
setup the underlying 'SslCtx' surface wants.
-}
module Wireform.Network.TLS.Config (
  -- * Client config
  TlsClientConfig (..),
  defaultTlsClientConfig,
  buildClientCtx,

  -- * Server config
  TlsServerConfig (..),
  defaultTlsServerConfig,
  buildServerCtx,
) where

import Data.ByteString (ByteString)
import Wireform.Network.TLS.OpenSSL (
  SslCtx,
  TlsProtoVersion (..),
  loadCaBundle,
  newClientCtx,
  newServerCtx,
  setAlpnClient,
  setAlpnServer,
  setCipherSuites,
  setMinProto,
  useClientCert,
 )


------------------------------------------------------------------------
-- Client
------------------------------------------------------------------------

{- | Configuration for a TLS client context.  Pass to
'buildClientCtx' to get a configured 'SslCtx'; then pair with
'Wireform.Network.TLS.OpenSSL.newClient' to drive a handshake.
-}
data TlsClientConfig = TlsClientConfig
  { tlsClientVerifyPeer :: !Bool
  {- ^ When 'True' (default), the OpenSSL system trust store is
  used + 'tlsClientCaBundle' is layered on top.  When 'False',
  any cert is accepted (test setups / pinned-fingerprint flows).
  -}
  , tlsClientCaBundle :: !(Maybe FilePath)
  -- ^ Additional PEM trust roots (on top of the system store).
  , tlsClientCertificate :: !(Maybe (FilePath, FilePath))
  -- ^ @(certChain, privateKey)@ for mTLS.  Both PEM.
  , tlsClientSni :: !(Maybe ByteString)
  {- ^ SNI (Server Name Indication) hostname.  When 'Nothing',
  the caller's connect-time hostname is used (passed into
  'Wireform.Network.TLS.OpenSSL.newClient' directly).
  -}
  , tlsClientVerifyHostname :: !(Maybe ByteString)
  {- ^ When 'Just', also pin the cert's CN \/ SAN to the given
  hostname (calls
  'Wireform.Network.TLS.OpenSSL.setClientHostnameVerify').
  Defaults to 'Nothing': use 'tlsClientVerifyPeer' alone for
  system trust store + cert chain validation.
  -}
  , tlsClientAlpn :: ![ByteString]
  {- ^ ALPN protocols to advertise, in preference order.
  e.g. @[\"h2\", \"http\/1.1\"]@.  Empty list disables ALPN.
  -}
  , tlsClientMinVersion :: !TlsProtoVersion
  , tlsClientCipherSuites :: !(Maybe ByteString)
  {- ^ OpenSSL cipher-string for TLS 1.2 and earlier.  'Nothing'
  keeps libssl defaults.
  -}
  }
  deriving stock (Show)


defaultTlsClientConfig :: TlsClientConfig
defaultTlsClientConfig =
  TlsClientConfig
    { tlsClientVerifyPeer = True
    , tlsClientCaBundle = Nothing
    , tlsClientCertificate = Nothing
    , tlsClientSni = Nothing
    , tlsClientVerifyHostname = Nothing
    , tlsClientAlpn = []
    , tlsClientMinVersion = Tls12
    , tlsClientCipherSuites = Nothing
    }


{- | Build + configure a client 'SslCtx' from the given config.
The caller owns the returned context and must eventually
'Wireform.Network.TLS.OpenSSL.freeCtx' it (after every connection
created from it has been freed).
-}
buildClientCtx :: TlsClientConfig -> IO SslCtx
buildClientCtx cfg = do
  ctx <- newClientCtx (tlsClientVerifyPeer cfg)
  case tlsClientCaBundle cfg of
    Just p -> loadCaBundle ctx p
    Nothing -> pure ()
  case tlsClientCertificate cfg of
    Just (c, k) -> useClientCert ctx c k
    Nothing -> pure ()
  case tlsClientAlpn cfg of
    [] -> pure ()
    xs -> setAlpnClient ctx xs
  setMinProto ctx (tlsClientMinVersion cfg)
  case tlsClientCipherSuites cfg of
    Just s -> setCipherSuites ctx s
    Nothing -> pure ()
  pure ctx


------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------

-- | Configuration for a TLS server context.
data TlsServerConfig = TlsServerConfig
  { tlsServerCertificate :: !FilePath
  -- ^ PEM cert chain.
  , tlsServerPrivateKey :: !FilePath
  -- ^ PEM private key.
  , tlsServerAlpn :: ![ByteString]
  {- ^ ALPN protocols offered to the client, in preference order.
  First match wins.  Empty list disables ALPN.
  -}
  , tlsServerMinVersion :: !TlsProtoVersion
  , tlsServerCipherSuites :: !(Maybe ByteString)
  }
  deriving stock (Show)


defaultTlsServerConfig
  :: FilePath
  -- ^ cert chain (PEM)
  -> FilePath
  -- ^ private key (PEM)
  -> TlsServerConfig
defaultTlsServerConfig cert key =
  TlsServerConfig
    { tlsServerCertificate = cert
    , tlsServerPrivateKey = key
    , tlsServerAlpn = []
    , tlsServerMinVersion = Tls12
    , tlsServerCipherSuites = Nothing
    }


buildServerCtx :: TlsServerConfig -> IO SslCtx
buildServerCtx cfg = do
  ctx <- newServerCtx (tlsServerCertificate cfg) (tlsServerPrivateKey cfg)
  case tlsServerAlpn cfg of
    [] -> pure ()
    xs -> setAlpnServer ctx xs
  setMinProto ctx (tlsServerMinVersion cfg)
  case tlsServerCipherSuites cfg of
    Just s -> setCipherSuites ctx s
    Nothing -> pure ()
  pure ctx
