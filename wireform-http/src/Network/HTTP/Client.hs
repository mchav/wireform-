{- | Unified HTTP client with version negotiation.

A 'ClientConfig' carries a 'VersionRange' that declares which on-wire
HTTP versions the client is willing to speak.  'withClient' opens a
connection that honours the range and exposes a single
'sendRequest' API regardless of whether the peer ends up speaking
HTTP\/1.x or HTTP\/2.  If the peer forces the client out of range a
'Network.HTTP.VersionRange.VersionOutOfRange' is thrown.

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
  'clientVersionRange'; the negotiated protocol drives the
  per-version runtime.  Both HTTP\/2 and HTTP\/1.x over TLS work
  end-to-end.  If ALPN ends up picking a version that isn't in the
  range, 'VersionOutOfRange' is raised.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Client
  ( -- * Configuration
    ClientConfig (..)
  , defaultClientConfig
  , TlsClientConfig (..)
  , defaultTlsClientConfig
    -- * Connecting
  , Client
  , clientNegotiatedVersion
  , withClient
    -- * Sending requests
  , sendRequest
    -- * Errors
  , ClientError (..)
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

data ClientConfig = ClientConfig
  { clientHost         :: !String
  , clientPort         :: !String
  , clientVersionRange :: !VersionRange
    -- ^ Acceptable on-wire versions, in preference order.
  , clientTls          :: !(Maybe TlsClientConfig)
    -- ^ 'Just' to do TLS + ALPN; 'Nothing' for plaintext.
  }

data TlsClientConfig = TlsClientConfig
  { tlsClientServerName    :: !String
    -- ^ SNI \/ X.509 hostname (defaults to 'clientHost').
  , tlsClientValidateCert  :: !Bool
  }

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
  { clientHost = "127.0.0.1"
  , clientPort = "80"
  , clientVersionRange = http1Only
  , clientTls = Nothing
  }

defaultTlsClientConfig :: String -> TlsClientConfig
defaultTlsClientConfig serverName = TlsClientConfig
  { tlsClientServerName = serverName
  , tlsClientValidateCert = True
  }

data ClientError
  = ClientUnsupportedRange !VersionRange
  | ClientParseError !H1.ParseError
  deriving stock (Show)

instance Exception ClientError

-- | An opaque connection handle.  Inspect with 'clientNegotiatedVersion'.
data Client
  = Http1Client !H1.ClientConnection !U.Version
  | Http2Client !H2.ClientHandle !U.Version

clientNegotiatedVersion :: Client -> U.Version
clientNegotiatedVersion = \case
  Http1Client _ v -> v
  Http2Client _ v -> v

-- | Open a connection, run the action, close the connection.
--
-- The protocol used is determined by 'clientVersionRange' and
-- 'clientTls': TLS uses ALPN with the range's protocols (or fails
-- with 'VersionOutOfRange'); plaintext picks the preferred version
-- in the range.
withClient :: ClientConfig -> (Client -> IO a) -> IO a
withClient cfg action = case clientTls cfg of
  Just tlsCfg -> withTlsClient cfg tlsCfg action
  Nothing ->
    let preferred = preferredVersion (clientVersionRange cfg)
    in if preferred == U.HTTP2
         then withPlaintextHttp2 cfg action
         else withPlaintextHttp1 cfg action

withTlsClient :: ClientConfig -> TlsClientConfig -> (Client -> IO a) -> IO a
withTlsClient cfg tlsCfg action =
  TLS.withTlsClient
    (clientHost cfg)
    (clientPort cfg)
    (tlsClientServerName tlsCfg)
    (tlsClientValidateCert tlsCfg)
    (clientVersionRange cfg)
    $ \case
        TLS.TlsClientHttp2 handle -> action (Http2Client handle U.HTTP2)
        TLS.TlsClientHttp1 conn   -> action (Http1Client conn U.HTTP1_1)

withPlaintextHttp1 :: ClientConfig -> (Client -> IO a) -> IO a
withPlaintextHttp1 cfg action = do
  let h1cfg = H1.defaultClientConfig
        { H1.clientHost = clientHost cfg
        , H1.clientPort = clientPort cfg
        }
      ver = case preferredVersion (clientVersionRange cfg) of
        U.HTTP1_0 -> U.HTTP1_0
        _         -> U.HTTP1_1
  H1.withClientConnection h1cfg $ \conn ->
    action (Http1Client conn ver)

withPlaintextHttp2 :: ClientConfig -> (Client -> IO a) -> IO a
withPlaintextHttp2 cfg action = do
  let h2cfg = H2.defaultClientConfig
        { H2.clientHost = clientHost cfg
        , H2.clientPort = clientPort cfg
        }
  H2.withConnection h2cfg $ \handle ->
    action (Http2Client handle U.HTTP2)

-- | Send a request on the connection. The 'requestVersion' field on
-- the input is ignored; the on-wire version is whatever 'withClient'
-- negotiated.
sendRequest :: Client -> Request -> IO Response
sendRequest (Http1Client conn ver) req = do
  let req1 = (Conv.toHttp1Request req) { H1.requestVersion = Conv.toHttp1Version ver }
  result <- H1.sendRequestOn conn req1
  case result of
    Left err   -> throwIO (ClientParseError err)
    Right resp -> pure (Conv.fromHttp1Response resp)

sendRequest (Http2Client handle _) req = do
  let h2req = H2.ClientRequest
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
  h2resp <- H2.sendRequest handle h2req
  pure Response
    { responseStatus  = U.Status (fromIntegral (H2.crStatus h2resp))
    , responseVersion = U.HTTP2
    , responseHeaders = Conv.fromHttp2Headers (H2.crResponseHeaders h2resp)
    , responseBody    = U.BodyStream (H2.crResponseBody h2resp)
      -- The HTTP/2 body is a chunk pull-producer; the producer is
      -- only valid for the lifetime of the surrounding 'withClient'
      -- bracket -- consume the body before exiting.
    }
