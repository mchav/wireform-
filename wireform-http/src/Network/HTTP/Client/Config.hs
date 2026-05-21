{- | High-level client configuration.

'withClient' assembles a 'Transport' from a 'ClientConfig' by
composing the configured middleware on top of the base transport.
The defaults are aimed at being production-reasonable without
forcing the caller to think about them — TLS, JSON, and a sane
retry policy are all available, just not enabled by default
(because some of them have side effects callers should opt into).

For full control, build the middleware stack manually and use
'Network.HTTP.Client.Base.baseTransport' directly.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Config
  ( ClientConfig (..)
  , defaultClientConfig
  , withClient
  ) where

import Control.Exception (bracket_)

import qualified Network.HTTP.VersionRange as VR

import Network.HTTP.Client.Base
import Network.HTTP.Client.Compression (withDecompression)
import Network.HTTP.Client.Cookies (CookieJar, withCookies)
import Network.HTTP.Client.Middleware
import Network.HTTP.Client.Tracing (TracingConfig (..), defaultTracingConfig, withTracing)
import Network.HTTP.Client.Transport

data ClientConfig = ClientConfig
  { ccVersionRange :: !VR.VersionRange
    -- ^ Which on-wire versions to accept. Defaults to 'preferHttp1'.
  , ccLogger       :: !(Maybe Logger)
  , ccAuth         :: !(Maybe AuthScheme)
  , ccTimeout      :: !(Maybe Duration)
  , ccRetryPolicy  :: !(Maybe RetryPolicy)
  , ccCookieJar    :: !(Maybe CookieJar)
  , ccTracing      :: !TracingConfig
    -- ^ OpenTelemetry tracing. Enabled by default (uses the global
    --   'OpenTelemetry.Trace.TracerProvider'; a no-op until an SDK
    --   is installed).
  , ccDecompress   :: !Bool
    -- ^ Honour @Content-Encoding@ on responses (brotli \/ gzip \/
    --   deflate). Defaults to 'True'; set 'False' to pass
    --   compressed bytes through to the caller.
  , ccExtra        :: !([Transport IO -> Transport IO])
    -- ^ Escape hatch for additional middleware to wrap the stack with.
    -- Applied outermost-first, after the standard set below.
  }

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
  { ccVersionRange = VR.preferHttp1
  , ccLogger       = Nothing
  , ccAuth         = Nothing
  , ccTimeout      = Nothing
  , ccRetryPolicy  = Nothing
  , ccCookieJar    = Nothing
  , ccTracing      = defaultTracingConfig
  , ccDecompress   = True
  , ccExtra        = []
  }

-- | Build a transport from the config, hand it to the action, and
-- clean up. The current implementation has no shared resources to
-- clean up — every request opens a fresh connection in the base
-- transport — but 'bracket_' is here so that future pooling has a
-- place to plug in without changing the public API.
withClient :: ClientConfig -> (Transport IO -> IO a) -> IO a
withClient cfg action =
  bracket_ (pure ()) (pure ()) $ action (assemble cfg)

assemble :: ClientConfig -> Transport IO
assemble cfg =
  let base       = baseTransport (ccVersionRange cfg)
      decompress = if ccDecompress cfg then withDecompression else id
      cookies    = maybe id withCookies   (ccCookieJar cfg)
      retry_     = maybe id withRetry     (ccRetryPolicy cfg)
      timeout_   = maybe id withTimeout   (ccTimeout cfg)
      auth       = maybe id withAuth      (ccAuth cfg)
      logger     = maybe id withLogging   (ccLogger cfg)
      tracing    = withTracing (ccTracing cfg)
      extras     = foldr (.) id (ccExtra cfg)
  -- Tracing sits outermost so the span covers retries, the timeout,
  -- auth header injection, and cookie processing. Decompression
  -- sits closest to the base transport so cookies / retry / etc.
  -- see decoded bodies (and so retried attempts each decompress
  -- independently).
  in extras
     . tracing
     . logger
     . cookies
     . retry_
     . timeout_
     . auth
     . decompress
     $ base
