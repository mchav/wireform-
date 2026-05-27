{- | Proxy resolution.

* 'ProxyConfig' \u2014 a value-level description of which targets go
  through which proxy.
* 'resolveProxyFromEnv' \u2014 build a 'ProxyConfig' from the
  @HTTP_PROXY@ \/ @HTTPS_PROXY@ \/ @NO_PROXY@ environment variables,
  the de-facto convention used by curl, wget, requests, reqwest.
* 'shouldBypass' \u2014 same matching rules as @NO_PROXY@.

== Routing

The connection layer now reads 'ProxyConfig' end-to-end:

* HTTP targets are dialled at the proxy directly; this middleware
  rewrites the request line to absolute form
  (RFC 9112 \u00a73.2.2) so the proxy can route it.
* HTTPS targets go through a @CONNECT@ tunnel set up by
  'Network.HTTP.Client.Proxy.Connect.connectThroughProxy', after
  which the TLS handshake runs over the tunnel.
* 'shouldBypass' is consulted before either path; matches skip
  the proxy entirely.

Both the pooled transport ('Network.HTTP.Client.Pool.pooledTransport')
and the one-shot base transport
('Network.HTTP.Client.Base.baseTransportVia') route through the
proxy when the surrounding 'Network.HTTP.Client.Config.ClientConfig'
sets 'ccProxyConfig'. The pool keys its idle connections on the
resolved proxy so different proxies do not share a pool.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Proxy
  ( -- * Configuration
    ProxyConfig (..)
  , Proxy (..)
  , defaultProxyConfig
  , noProxyConfig
    -- * Environment-based resolution
  , resolveProxyFromEnv
    -- * Middleware
  , withProxy
    -- * Predicates
  , shouldBypass
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)

import qualified Network.HTTP.Client.Request as WReq
import qualified Network.HTTP.Client.URI     as WURI
import           Network.HTTP.Client.Transport

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | A proxy endpoint. The scheme determines whether the proxy
-- itself speaks HTTP or HTTPS; the target's scheme is independent
-- (a proxy can carry HTTPS-target traffic via CONNECT).
data Proxy = Proxy
  { proxyScheme :: !WURI.Scheme
  , proxyHost   :: !ByteString
  , proxyPort   :: !Int
  }
  deriving stock (Eq, Show)

data ProxyConfig = ProxyConfig
  { proxyForHttp  :: !(Maybe Proxy)
    -- ^ Used for @http:\/\/@ targets.
  , proxyForHttps :: !(Maybe Proxy)
    -- ^ Used for @https:\/\/@ targets (handed off to a CONNECT
    --   tunnel by the connection layer).
  , proxyBypass   :: ![ByteString]
    -- ^ Hosts (or comma-separated suffix patterns from @NO_PROXY@)
    --   that should bypass the proxy. A leading dot makes the
    --   pattern subdomain-only (@.example.com@ matches
    --   @api.example.com@ but not @example.com@); otherwise the
    --   pattern matches both the bare host and any subdomain.
  }
  deriving stock (Eq, Show)

defaultProxyConfig :: ProxyConfig
defaultProxyConfig = noProxyConfig

noProxyConfig :: ProxyConfig
noProxyConfig = ProxyConfig Nothing Nothing []

-- ---------------------------------------------------------------------------
-- Environment resolution
-- ---------------------------------------------------------------------------

-- | Resolve a 'ProxyConfig' from @HTTP_PROXY@ \/ @HTTPS_PROXY@ \/
-- @NO_PROXY@. Lower-case variants (@http_proxy@ etc.) are accepted
-- as well \u2014 @curl@'s convention.
resolveProxyFromEnv :: IO ProxyConfig
resolveProxyFromEnv = do
  http  <- firstEnv ["http_proxy", "HTTP_PROXY"]
  https <- firstEnv ["https_proxy", "HTTPS_PROXY"]
  nop   <- firstEnv ["no_proxy", "NO_PROXY"]
  pure ProxyConfig
    { proxyForHttp  = http  >>= parseProxy
    , proxyForHttps = https >>= parseProxy
    , proxyBypass   = case nop of
        Just s  -> filter (not . BS.null) (map (trim . BS8.pack) (splitComma s))
        Nothing -> []
    }
  where
    firstEnv [] = pure Nothing
    firstEnv (k : ks) = do
      v <- lookupEnv k
      case v of
        Just s | not (null s) -> pure (Just s)
        _ -> firstEnv ks
    splitComma s = words [if c == ',' then ' ' else c | c <- s]
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09

parseProxy :: String -> Maybe Proxy
parseProxy s = case WURI.parseURI (TE.encodeUtf8 (T.pack s')) of
  Right u  -> Just Proxy
    { proxyScheme = WURI.uriScheme u
    , proxyHost   = WURI.uriHost u
    , proxyPort   = WURI.uriPort u
    }
  Left _ -> Nothing
  where
    -- @curl@ accepts proxies as @host:port@ without scheme; default
    -- to http:// in that case.
    s' = case s of
      _ | "http://"  `prefixOf` s -> s
        | "https://" `prefixOf` s -> s
        | otherwise               -> "http://" <> s
    prefixOf p t = take (length p) (map toLower t) == p

-- ---------------------------------------------------------------------------
-- Predicates
-- ---------------------------------------------------------------------------

shouldBypass :: ProxyConfig -> ByteString -> Bool
shouldBypass cfg host = any (matches host) (proxyBypass cfg)
  where
    matches h pat = case BS.uncons pat of
      Just (0x2E, suffix) ->
        BS.length h > BS.length suffix
          && suffix `BS.isSuffixOf` h
          && BS.index h (BS.length h - BS.length suffix - 1) == 0x2E
      _ -> h == pat
        || (pat `BS.isSuffixOf` h
            && BS.length h > BS.length pat
            && BS.index h (BS.length h - BS.length pat - 1) == 0x2E)

-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

-- | Rewrite outgoing requests to go through the configured proxy.
--
-- For plain-HTTP targets, this:
--
-- 1. retargets the request URI to absolute form
--    (@http:\/\/host\/path@), as required by RFC 9112 \u00a73.2.2;
-- 2. attaches a @Forwarded@-suitable @Host@ header in case the
--    proxy forwards downstream.
--
-- For HTTPS targets the middleware is a no-op: HTTPS-via-proxy
-- requires a CONNECT tunnel that's set up at the connection layer
-- (see "Network.HTTP.Client.Proxy.Connect"), not by rewriting the
-- request.
withProxy :: ProxyConfig -> Middleware IO
withProxy cfg inner = Transport $ \req -> do
  case WURI.renderRequestURI (WReq.requestURI req) of
    Left _  -> sendRaw inner req
    Right u
      | shouldBypass cfg (WURI.uriHost u) -> sendRaw inner req
      | otherwise -> case WURI.uriScheme u of
          WURI.SchemeHttps -> sendRaw inner req
          WURI.SchemeHttp  -> case proxyForHttp cfg of
            Nothing -> sendRaw inner req
            Just _proxy ->
              let absForm = WURI.renderURI u
                  rewritten = req
                    { WReq.requestURI =
                        WURI.staticURI (TE.decodeUtf8 absForm)
                    }
              in sendRaw inner rewritten
