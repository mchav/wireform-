{-# LANGUAGE OverloadedStrings #-}

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
module Network.HTTP.Client.Config (
  ClientConfig (..),
  defaultClientConfig,
  withClient,
) where

import Control.Exception (bracket)
import Network.HTTP.Client.Base (baseTransportVia)
import Network.HTTP.Client.Compression (withDecompression)
import Network.HTTP.Client.Cookies (CookieJar, withCookies)
import Network.HTTP.Client.Middleware
import Network.HTTP.Client.Pool (ConnectionPool, PoolConfig, closePool, newPool, pooledTransport)
import Network.HTTP.Client.Pool qualified as Pool
import Network.HTTP.Client.Proxy (ProxyConfig)
import Network.HTTP.Client.Proxy qualified as Pxy
import Network.HTTP.Client.Tracing (TracingConfig (..), defaultTracingConfig, withTracing)
import Network.HTTP.Client.Transport
import Network.HTTP.VersionRange qualified as VR


data ClientConfig = ClientConfig
  { ccVersionRange :: !VR.VersionRange
  -- ^ Which on-wire versions to accept. Defaults to 'preferHttp1'.
  , ccLogger :: !(Maybe Logger)
  , ccAuth :: !(Maybe AuthScheme)
  , ccTimeout :: !(Maybe Duration)
  , ccRetryPolicy :: !(Maybe RetryPolicy)
  , ccCookieJar :: !(Maybe CookieJar)
  , ccTracing :: !TracingConfig
  {- ^ OpenTelemetry tracing. Enabled by default (uses the global
  'OpenTelemetry.Trace.TracerProvider'; a no-op until an SDK
  is installed).
  -}
  , ccDecompress :: !Bool
  {- ^ Honour @Content-Encoding@ on responses (brotli \/ gzip \/
  deflate). Defaults to 'True'; set 'False' to pass
  compressed bytes through to the caller.
  -}
  , ccPoolConfig :: !(Maybe PoolConfig)
  {- ^ When 'Just', 'withClient' allocates a 'ConnectionPool' for
  the duration of the action and routes the base transport
  through it. Defaults to a sensible 'PoolConfig'; pass
  'Nothing' to bypass pooling entirely.
  -}
  , ccProxyConfig :: !ProxyConfig
  {- ^ Proxy selection. Defaults to 'Pxy.noProxyConfig'.  When set,
  the base \/ pooled transport routes every request through
  the configured proxy (HTTP-via-proxy rewrites the
  request-line via 'Network.HTTP.Client.Proxy.withProxy';
  HTTPS-via-proxy uses a CONNECT tunnel). If you want to
  resolve this from the environment, call
  'Pxy.resolveProxyFromEnv' before building the config.
  -}
  , ccExtra :: !([Transport IO -> Transport IO])
  {- ^ Escape hatch for additional middleware to wrap the stack with.
  Applied outermost-first, after the standard set below.
  -}
  }


defaultClientConfig :: ClientConfig
defaultClientConfig =
  ClientConfig
    { ccVersionRange = VR.preferHttp1
    , ccLogger = Nothing
    , ccAuth = Nothing
    , ccTimeout = Nothing
    , ccRetryPolicy = Nothing
    , ccCookieJar = Nothing
    , ccTracing = defaultTracingConfig
    , ccDecompress = True
    , ccPoolConfig = Just Pool.defaultPoolConfig
    , ccProxyConfig = Pxy.noProxyConfig
    , ccExtra = []
    }


{- | Build a transport from the config, hand it to the action, and
clean up.  When 'ccPoolConfig' is 'Just' the action runs against
a 'ConnectionPool' allocated for its lifetime; the pool is torn
down (closing all idle connections) when the action returns.
-}
withClient :: ClientConfig -> (Transport IO -> IO a) -> IO a
withClient cfg action = case ccPoolConfig cfg of
  Nothing -> action (assemble cfg Nothing)
  Just poolCfg ->
    -- Plumb the client-level ProxyConfig into the pool so its
    -- connection routing and target-keying see the same proxy
    -- decisions as the non-pooled path.
    let poolCfg' = poolCfg {Pool.proxyConfig = ccProxyConfig cfg}
    in bracket (newPool poolCfg') closePool $ \pool ->
         action (assemble cfg (Just pool))


assemble :: ClientConfig -> Maybe ConnectionPool -> Transport IO
assemble cfg mPool =
  let base = case mPool of
        Just pool -> pooledTransport pool
        Nothing ->
          baseTransportVia
            (ccVersionRange cfg)
            (ccProxyConfig cfg)
      decompress = if ccDecompress cfg then withDecompression else id
      cookies = maybe id withCookies (ccCookieJar cfg)
      retry_ = maybe id withRetry (ccRetryPolicy cfg)
      timeout_ = maybe id withTimeout (ccTimeout cfg)
      auth = maybe id withAuth (ccAuth cfg)
      logger = maybe id withLogging (ccLogger cfg)
      tracing = withTracing (ccTracing cfg)
      extras = foldr (.) id (ccExtra cfg)
      proxy_ = Pxy.withProxy (ccProxyConfig cfg)
  in -- Tracing sits outermost so the span covers retries, the timeout,
     -- auth header injection, and cookie processing. Decompression
     -- sits closest to the base transport so cookies / retry / etc.
     -- see decoded bodies (and so retried attempts each decompress
     -- independently).
     extras
       . tracing
       . logger
       . cookies
       . retry_
       . timeout_
       . auth
       . decompress
       . proxy_
       $ base
