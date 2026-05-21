{- | Low-level single-connection HTTP client with version negotiation.

A 'ConnectionConfig' carries a 'VersionRange' that declares which
on-wire HTTP versions the connection is willing to speak.
'withConnection' opens a connection that honours the range and
exposes a single 'sendOn' API regardless of whether the peer ends
up speaking HTTP\/1.x or HTTP\/2.  If the peer forces us out of
range a 'Network.HTTP.VersionRange.VersionOutOfRange' is thrown.

This is the byte-level transport that the higher-level
"Network.HTTP.Client" sits on top of via
'Network.HTTP.Client.Base.baseTransport'. Most callers should use
the high-level API; reach for this module only when you want to
manage a single connection's lifetime by hand.

Transport matrix:

* __Plaintext, HTTP\/1.x only__ ('http1Only' or 'preferHttp1' on
  plaintext) — request and response both end-to-end.
* __Plaintext, HTTP\/2 only__ ('http2Only' on plaintext; h2c prior
  knowledge) — request, response headers, and a buffered response
  body all work, with the connection multiplexing concurrent
  requests on separate streams.
* __Plaintext, mixed range__ — falls back to the preferred version.
  The h2c @Upgrade:@ dance (RFC 7540 § 3.2) is intentionally not
  implemented: RFC 9113 deprecated it.  For mixed-protocol clients
  use TLS-ALPN.
* __TLS__ — handshake done with ALPN advertising the protocols in
  'connectionVersionRange'; the negotiated protocol drives the
  per-version runtime.  Both HTTP\/2 and HTTP\/1.x over TLS work
  end-to-end.  If ALPN ends up picking a version that isn't in the
  range, 'VersionOutOfRange' is raised.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Connection
  ( -- * Configuration
    ConnectionConfig (..)
  , defaultConnectionConfig
  , TlsConnectionConfig (..)
  , defaultTlsConnectionConfig
    -- * Connecting
  , Connection
  , negotiatedVersion
  , withConnection
    -- * Sending requests
  , sendOn
  , withResponseOn
    -- * Errors
  , ConnectionError (..)
  ) where

import Control.Exception (Exception, throwIO)
import qualified Network.HTTP1.Client as H1
import qualified Network.HTTP1.Parser as H1
import qualified Network.HTTP1.Types as H1
import qualified Network.HTTP2.Client as H2

import Network.HTTP.Message
import qualified Network.HTTP.TLS as TLS
import Network.HTTP.VersionRange
import qualified Network.HTTP.Internal.Convert as Conv
import qualified Network.HTTP.Types.Body as U
import qualified Network.HTTP.Types.Method as U
import qualified Network.HTTP.Types.Status as U
import qualified Network.HTTP.Types.Version as U

data ConnectionConfig = ConnectionConfig
  { connectionHost         :: !String
  , connectionPort         :: !String
  , connectionVersionRange :: !VersionRange
    -- ^ Acceptable on-wire versions, in preference order.
  , connectionTls          :: !(Maybe TlsConnectionConfig)
    -- ^ 'Just' to do TLS + ALPN; 'Nothing' for plaintext.
  }

data TlsConnectionConfig = TlsConnectionConfig
  { tlsServerName    :: !String
    -- ^ SNI \/ X.509 hostname (defaults to 'connectionHost').
  , tlsValidateCert  :: !Bool
  }

defaultConnectionConfig :: ConnectionConfig
defaultConnectionConfig = ConnectionConfig
  { connectionHost = "127.0.0.1"
  , connectionPort = "80"
  , connectionVersionRange = http1Only
  , connectionTls = Nothing
  }

defaultTlsConnectionConfig :: String -> TlsConnectionConfig
defaultTlsConnectionConfig serverName = TlsConnectionConfig
  { tlsServerName = serverName
  , tlsValidateCert = True
  }

data ConnectionError
  = ConnectionUnsupportedRange !VersionRange
  | ConnectionParseError !H1.ParseError
  deriving stock (Show)

instance Exception ConnectionError

-- | An opaque connection handle.  Inspect with 'negotiatedVersion'.
data Connection
  = Http1Connection !H1.ClientConnection !U.Version
  | Http2Connection !H2.ClientHandle !U.Version

negotiatedVersion :: Connection -> U.Version
negotiatedVersion = \case
  Http1Connection _ v -> v
  Http2Connection _ v -> v

-- | Open a connection, run the action, close the connection.
--
-- The protocol used is determined by 'connectionVersionRange' and
-- 'connectionTls': TLS uses ALPN with the range's protocols (or
-- fails with 'VersionOutOfRange'); plaintext picks the preferred
-- version in the range.
withConnection :: ConnectionConfig -> (Connection -> IO a) -> IO a
withConnection cfg action = case connectionTls cfg of
  Just tlsCfg -> withTlsConnection cfg tlsCfg action
  Nothing ->
    let preferred = preferredVersion (connectionVersionRange cfg)
    in if preferred == U.HTTP2
         then withPlaintextHttp2 cfg action
         else withPlaintextHttp1 cfg action

withTlsConnection :: ConnectionConfig -> TlsConnectionConfig -> (Connection -> IO a) -> IO a
withTlsConnection cfg tlsCfg action =
  TLS.withTlsClient
    (connectionHost cfg)
    (connectionPort cfg)
    (tlsServerName tlsCfg)
    (tlsValidateCert tlsCfg)
    (connectionVersionRange cfg)
    $ \case
        TLS.TlsClientHttp2 handle -> action (Http2Connection handle U.HTTP2)
        TLS.TlsClientHttp1 conn   -> action (Http1Connection conn U.HTTP1_1)

withPlaintextHttp1 :: ConnectionConfig -> (Connection -> IO a) -> IO a
withPlaintextHttp1 cfg action = do
  let h1cfg = H1.defaultClientConfig
        { H1.clientHost = connectionHost cfg
        , H1.clientPort = connectionPort cfg
        }
      ver = case preferredVersion (connectionVersionRange cfg) of
        U.HTTP1_0 -> U.HTTP1_0
        _         -> U.HTTP1_1
  H1.withClientConnection h1cfg $ \conn ->
    action (Http1Connection conn ver)

withPlaintextHttp2 :: ConnectionConfig -> (Connection -> IO a) -> IO a
withPlaintextHttp2 cfg action = do
  let h2cfg = H2.defaultClientConfig
        { H2.clientHost = connectionHost cfg
        , H2.clientPort = connectionPort cfg
        }
  H2.withConnection h2cfg $ \handle ->
    action (Http2Connection handle U.HTTP2)

-- | Send a request on the connection. The 'requestVersion' field on
-- the input is ignored; the on-wire version is whatever
-- 'withConnection' negotiated.
sendOn :: Connection -> Request -> IO Response
sendOn (Http1Connection conn ver) req = do
  let req1 = (Conv.toHttp1Request req) { H1.requestVersion = Conv.toHttp1Version ver }
  result <- H1.sendRequestOn conn req1
  case result of
    Left err   -> throwIO (ConnectionParseError err)
    Right resp -> pure (Conv.fromHttp1Response resp)

sendOn (Http2Connection handle _) req = do
  h2resp <- H2.sendRequest handle (requestToH2 req)
  pure (h2ResponseToUnified h2resp)

-- | Bracket-style request that cancels the underlying stream if the
-- action throws (HTTP\/2) or closes the connection if the action
-- throws on HTTP\/1.x.  Use this whenever the caller might bail
-- mid-request (timeouts, racing requests, cooperative cancellation).
--
-- For HTTP\/2 we emit @RST_STREAM(CANCEL)@ to the peer on abnormal
-- exit, so a cancelled download stops costing bandwidth.  For
-- HTTP\/1.x there's no per-stream cancellation; if the action
-- throws we let the exception propagate and the surrounding
-- 'withConnection' bracket closes the connection.
withResponseOn :: Connection -> Request -> (Response -> IO a) -> IO a
withResponseOn (Http1Connection conn ver) req action = do
  let req1 = (Conv.toHttp1Request req) { H1.requestVersion = Conv.toHttp1Version ver }
  result <- H1.sendRequestOn conn req1
  case result of
    Left err   -> throwIO (ConnectionParseError err)
    Right resp -> action (Conv.fromHttp1Response resp)
withResponseOn (Http2Connection handle _) req action = do
  let h2req = requestToH2 req
  H2.withResponse handle h2req $ \h2resp ->
    action (h2ResponseToUnified h2resp)

requestToH2 :: Request -> H2.ClientRequest
requestToH2 req = H2.ClientRequest
  { H2.crMethod    = U.fromMethod (requestMethod req)
  , H2.crPath      = requestTarget req
  , H2.crScheme    = case requestScheme req of
      SchemeHttp  -> "http"
      SchemeHttps -> "https"
  , H2.crAuthority = maybe "" id (requestAuthority req)
  , H2.crHeaders   = Conv.toHttp2Headers (requestHeaders req)
  , H2.crBody      = case requestBody req of
      U.BodyEmpty    -> H2.ReqBodyNone
      U.BodyBytes bs -> H2.ReqBodyBytes bs
      U.BodyStream p -> H2.ReqBodyStream p
  }

h2ResponseToUnified :: H2.ClientResponse -> Response
h2ResponseToUnified h2resp = Response
  { responseStatus  = U.Status (fromIntegral (H2.crStatus h2resp))
  , responseVersion = U.HTTP2
  , responseHeaders = Conv.fromHttp2Headers (H2.crResponseHeaders h2resp)
  , responseBody    = U.BodyStream (H2.crResponseBody h2resp)
  , responseTrailers = Conv.fromHttp2Headers <$> H2.crResponseTrailers h2resp
      -- Body + trailers are both pull-shaped; both are only valid
      -- for the lifetime of the surrounding 'withConnection' bracket --
      -- consume them before exiting.
    }
