{- | Unified HTTP client with version negotiation.

A 'ClientConfig' carries a 'VersionRange' that declares which on-wire
HTTP versions the client is willing to speak.  'withClient' opens a
connection that honours the range and exposes a single
'sendRequest' API regardless of whether the peer ends up speaking
HTTP\/1.x or HTTP\/2.  If the peer forces the client out of range a
'Network.HTTP.VersionRange.VersionOutOfRange' is thrown.

Transport matrix (this commit):

* __Plaintext, HTTP\/1.x only__ ('http1Only', 'preferHttp1' on plaintext)
  — fully functional, request and response both work.
* __Plaintext, HTTP\/2 only__ ('http2Only' on plaintext, \"h2c prior
  knowledge\") — the request is sent on a freshly opened HTTP\/2
  connection; response collection is currently a stub awaiting the
  matching @recvResponse@ helper in @wireform-http2@.  Use this path
  for gRPC bring-up and override 'sendRequest' with a direct
  'Network.HTTP2.Client' call if you need the response now.
* __Plaintext, mixed range__ — falls back to HTTP\/1.1.  The h2c
  upgrade dance (RFC 7540 § 3.2) requires recv-buffer-leftover access
  in @wireform-http1@; the hook will be added in a follow-up.
* __TLS__ — not wired in this commit.  Route through
  "Network.HTTP2.TLS.Client" for now; a TLS adapter that picks the
  ALPN list from the 'VersionRange' will land alongside the
  receive-side HTTP\/2 client.

The shape of this API is deliberately stable across those gaps; only
the implementation matures.
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
import qualified Network.HTTP2.Connection as H2C

import Network.HTTP.Message
import Network.HTTP.VersionRange
import qualified Network.HTTP.Internal.Convert as Conv
import qualified Network.HTTP.Types.Body as U
import qualified Network.HTTP.Types.Method as U
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
  | ClientHttp2ResponseUnavailable
    -- ^ HTTP\/2 response collection isn't yet wired through this API.
  | ClientParseError !H1.ParseError
  deriving stock (Show)

instance Exception ClientError

-- | An opaque connection handle.  Inspect with 'clientNegotiatedVersion'.
data Client
  = Http1Client !H1.ClientConnection !U.Version
  | Http2Client !H2C.Connection !U.Version

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
withClient cfg action
  | Just _ <- clientTls cfg =
      throwIO (ClientUnsupportedRange (clientVersionRange cfg))
  | otherwise =
      let preferred = preferredVersion (clientVersionRange cfg)
      in if preferred == U.HTTP2
           then withPlaintextHttp2 cfg action
           else withPlaintextHttp1 cfg action

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
  H2.withConnection h2cfg $ \conn ->
    action (Http2Client conn U.HTTP2)

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

sendRequest (Http2Client conn _) req = do
  let h2req = H2.ClientRequest
        { H2.crMethod    = U.fromMethod (requestMethod req)
        , H2.crPath      = requestTarget req
        , H2.crScheme    = case requestScheme req of
            SchemeHttp  -> "http"
            SchemeHttps -> "https"
        , H2.crAuthority = maybe "" id (requestAuthority req)
        , H2.crHeaders   = Conv.toHttp2Headers (requestHeaders req)
        , H2.crBody      = case requestBody req of
            U.BodyEmpty    -> Nothing
            U.BodyBytes bs -> Just bs
            U.BodyStream _ -> Nothing
              -- TODO: streaming request bodies on HTTP/2 (needs DATA
              -- frame pump in Network.HTTP2.Client).
        }
  _sid <- H2.sendRequest conn h2req
  -- TODO: wire response collection. The HTTP/2 client recv loop
  -- in @wireform-http2@ doesn't yet make per-stream response
  -- headers/body available to user code. Drop to
  -- 'Network.HTTP2.Engine.Client.run' if you need the response
  -- today.
  throwIO ClientHttp2ResponseUnavailable
