{- | Unified HTTP server with version negotiation.

A 'ServerConfig' carries a 'VersionRange' that declares which on-wire
HTTP versions the server is willing to speak.  'runServer' binds a
TCP listener and dispatches each accepted connection to the
appropriate per-version runtime; connections that don't match the
range are dropped during negotiation.

Negotiation (this commit):

* __Plaintext__ — dispatched on the preferred version of the range:
  'http1Only' \/ 'preferHttp1' runs the HTTP\/1.x server; 'http2Only'
  \/ 'preferHttp2' runs the HTTP\/2 server (and requires the client
  to send the @PRI * HTTP\/2.0@ preface up front, i.e.
  prior-knowledge h2c).  An h2c @Upgrade:@ handshake (RFC 7540 § 3.2)
  is not yet implemented; clients that want both protocols against
  the same plaintext listener should advertise their support over
  TLS-ALPN instead.
* __TLS__ — not wired in this commit.  Use
  "Network.HTTP2.TLS.Server" directly for HTTP\/2 + ALPN; a TLS
  adapter that drives the 'VersionRange' will land in a follow-up.

The handler is a plain @'Request' -> IO 'Response'@: the HTTP\/2
server's continuation shape is adapted internally.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Server
  ( -- * Configuration
    ServerConfig (..)
  , defaultServerConfig
  , TlsServerConfig (..)
    -- * Running
  , runServer
    -- * Handler
  , Handler
    -- * Errors
  , ServerError (..)
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (Exception, throwIO)

import qualified Network.HTTP1.Server as H1
import qualified Network.HTTP1.Types as H1
import qualified Network.HTTP2.Server as H2

import Network.HTTP.Message
import Network.HTTP.VersionRange
import qualified Network.HTTP.Internal.Convert as Conv
import qualified Network.HTTP.Types.Body as U
import qualified Network.HTTP.Types.Status as U
import qualified Network.HTTP.Types.Version as U

-- | The user's request handler.  Operates on unified
-- 'Network.HTTP.Message.Request' \/ 'Response' so it's portable
-- between HTTP\/1.x and HTTP\/2.
type Handler = Request -> IO Response

data ServerConfig = ServerConfig
  { serverHost         :: !String
  , serverPort         :: !String
  , serverVersionRange :: !VersionRange
    -- ^ Versions the server is willing to speak.  The preferred
    -- version drives the plaintext dispatch.
  , serverHandler      :: !Handler
  , serverTls          :: !(Maybe TlsServerConfig)
  , serverForkConnection :: IO () -> IO ThreadId
    -- ^ How to fork the per-connection thread.  Defaults to 'forkIO';
    -- use 'Control.Concurrent.forkOn' for pinned-core scheduling.
  }

data TlsServerConfig = TlsServerConfig
  { tlsServerCertPath :: !FilePath
  , tlsServerKeyPath  :: !FilePath
  }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverHost = "0.0.0.0"
  , serverPort = "8080"
  , serverVersionRange = http1Only
  , serverHandler = \_ -> pure stubResponse
  , serverTls = Nothing
  , serverForkConnection = forkIO
  }
  where
    stubResponse = Response
      { responseStatus  = U.status200
      , responseVersion = U.HTTP1_1
      , responseHeaders = []
      , responseBody    = U.BodyEmpty
      }

data ServerError
  = ServerUnsupportedRange !VersionRange
  deriving stock (Show)

instance Exception ServerError

-- | Bind a TCP listener and serve until killed.
--
-- Currently TLS-bound 'ServerConfig's throw 'ServerUnsupportedRange'.
runServer :: ServerConfig -> IO ()
runServer cfg
  | Just _ <- serverTls cfg =
      throwIO (ServerUnsupportedRange (serverVersionRange cfg))
  | otherwise =
      let preferred = preferredVersion (serverVersionRange cfg)
      in if preferred == U.HTTP2
           then runHttp2 cfg
           else runHttp1 cfg

runHttp1 :: ServerConfig -> IO ()
runHttp1 cfg = H1.runServer h1cfg
  where
    h1cfg = H1.defaultServerConfig
      { H1.serverHost = serverHost cfg
      , H1.serverPort = serverPort cfg
      , H1.serverForkConnection = serverForkConnection cfg
      , H1.serverHandler = wrapHttp1Handler (serverHandler cfg)
      }

wrapHttp1Handler :: Handler -> H1.Request -> IO H1.Response
wrapHttp1Handler handler h1req = do
  let req = Conv.fromHttp1Request SchemeHttp h1req
  resp <- handler req
  -- Mirror the request's version on the response, matching the
  -- behaviour the http1 server's own defaultServerConfig has.
  let h1resp = Conv.toHttp1Response resp
  pure h1resp { H1.responseVersion = H1.requestVersion h1req }

runHttp2 :: ServerConfig -> IO ()
runHttp2 cfg = H2.runServer h2cfg
  where
    h2cfg = H2.defaultServerConfig
      { H2.serverHost = serverHost cfg
      , H2.serverPort = serverPort cfg
      , H2.serverForkConnection = serverForkConnection cfg
      , H2.serverHandler = wrapHttp2Handler (serverHandler cfg)
      }

wrapHttp2Handler :: Handler -> H2.Request -> (H2.Response -> IO ()) -> IO ()
wrapHttp2Handler handler h2req respond = do
  let req = Conv.fromHttp2Request h2req
  resp <- handler req
  respond (Conv.toHttp2Response resp)
