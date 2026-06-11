{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Redirect-following middleware (RFC 9110 \u00a715.4).

The middleware re-issues a request when the response status is one of
the redirect codes and a @Location@ header is present, up to a
configured maximum hop count. The body is buffered upfront so 307 \/
308 (which require replay) work; 303 demotes to GET as the spec
mandates; 301 \/ 302 are handled per the configured policy
('rpRewriteToGet') because both \"replay\" and \"demote\" are seen in
the wild.

Cross-origin hops have @Authorization@ stripped by default to avoid
leaking credentials to a redirect target the caller didn't authorise.

The middleware operates against the wire-level
@'Request' 'BodyStream'@ shape (i.e. it sits below 'send'\\'s body
encoding in the stack) so it can preserve the materialised request
bytes across hops.
-}
module Network.HTTP.Client.Redirect (
  -- * Configuration
  RedirectPolicy (..),
  defaultRedirectPolicy,

  -- * Middleware
  withRedirects,

  -- * Errors
  TooManyRedirects (..),
  RedirectLoop (..),
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client.BodyStream (
  bodyStreamBytes,
  drainPopper,
  streamFromStrict,
 )
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S


-- ---------------------------------------------------------------------------
-- Policy
-- ---------------------------------------------------------------------------

data RedirectPolicy = RedirectPolicy
  { rpMaxRedirects :: !Int
  {- ^ Refuse to follow more than this many hops; the @Int@-th
  redirect throws 'TooManyRedirects'.
  -}
  , rpRewriteToGet :: !Bool
  {- ^ When 'True', @301@ and @302@ rewrite the method to @GET@ and
  drop the body (the historical browser behaviour). When
  'False', they replay the original method and body. RFC 9110
  recommends preserving the method but every browser does it
  the other way.
  -}
  , rpStripAuthCrossOrigin :: !Bool
  {- ^ Drop @Authorization@ on cross-origin hops. @True@ by default
  (it's how curl behaves with @--location@; sttp, requests,
  and reqwest all do the same).
  -}
  , rpStripCookieCrossOrigin :: !Bool
  {- ^ Drop the request-side @Cookie@ header on cross-origin hops.
  @True@ by default. Cross-origin cookie leakage is at least
  as severe as 'Authorization', and the cookie jar
  middleware re-attaches the right cookies for the new
  target on the next hop anyway.
  -}
  , rpDetectLoops :: !Bool
  {- ^ Reject redirect loops (the resolved target URI was already
  visited on this request). @True@ by default — this catches
  misconfigured servers that bounce indefinitely between two
  URIs without ever exceeding 'rpMaxRedirects'.
  -}
  , rpFollowOn :: !(S.Status -> Bool)
  {- ^ Which statuses to follow. Defaults to 301\/302\/303\/307\/308;
  override to e.g. include 300 (Multiple Choices) when the
  server reliably returns a Location for it.
  -}
  }


defaultRedirectPolicy :: RedirectPolicy
defaultRedirectPolicy =
  RedirectPolicy
    { rpMaxRedirects = 10
    , rpRewriteToGet = True
    , rpStripAuthCrossOrigin = True
    , rpStripCookieCrossOrigin = True
    , rpDetectLoops = True
    , rpFollowOn = isRedirectStatus
    }


isRedirectStatus :: S.Status -> Bool
isRedirectStatus s = case S.statusCode s of
  301 -> True
  302 -> True
  303 -> True
  307 -> True
  308 -> True
  _ -> False


-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data TooManyRedirects = TooManyRedirects
  { trVisited :: ![ByteString]
  {- ^ The URIs visited up to and including the one that tripped
  the limit, in order.
  -}
  }
  deriving stock (Show)


instance Exception TooManyRedirects


{- | Thrown when 'rpDetectLoops' spots a 'Location' that the
middleware has already resolved to within the current redirect
chain. The @rlVisited@ list is the chain in visit order; the
last entry is the one that closed the loop.
-}
data RedirectLoop = RedirectLoop {rlVisited :: ![ByteString]}
  deriving stock (Show)


instance Exception RedirectLoop


-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

withRedirects :: RedirectPolicy -> Middleware IO
withRedirects policy inner = Transport $ \req0 -> do
  buffered <- bodyStreamBytes (WReq.body req0)
  let go visited 0 _ _ =
        throwIO (TooManyRedirects (reverse visited))
      go visited n req body = do
        bs <- streamFromStrict body
        let attempt = req {WReq.body = bs}
        raw <- sendRaw inner attempt
        if not (rpFollowOn policy (statusCode raw))
          then pure raw
          else case H.lookupHeader H.hLocation (Network.HTTP.Client.Response.headers raw) of
            Nothing -> pure raw
            Just loc -> do
              -- Drain the redirect's body so the connection / mock can advance.
              drainPopper (bodyPopper raw)
              currentURI <- case WURI.renderRequestURI (WReq.requestURI req) of
                Right u -> pure u
                Left _ -> pure dummyURI
              let resolved = resolveLocation currentURI loc
                  resolvedKey = WURI.renderURI (WURI.normalizeURI resolved)
                  origin0 = origin currentURI
                  origin1 = origin resolved
                  crossOrig = origin0 /= origin1
                  rewriteGet =
                    rpRewriteToGet policy
                      && (S.statusCode (statusCode raw) `elem` [301, 302])
                  toGet =
                    rewriteGet
                      || S.statusCode (statusCode raw) == 303
                  newMethod
                    | toGet = M.mGet
                    | otherwise = WReq.method req
                  newBody
                    | toGet = BS.empty
                    | otherwise = body
                  newHeaders0
                    | crossOrig && rpStripAuthCrossOrigin policy =
                        H.deleteHeader H.hAuthorization (WReq.headers req)
                    | otherwise = WReq.headers req
                  newHeaders0a
                    | crossOrig && rpStripCookieCrossOrigin policy =
                        H.deleteHeader H.hCookie newHeaders0
                    | otherwise = newHeaders0
                  -- If we just demoted to GET, drop body-related headers.
                  newHeaders1
                    | toGet =
                        H.deleteHeader
                          H.hContentLength
                          ( H.deleteHeader
                              H.hContentType
                              (H.deleteHeader H.hContentEncoding newHeaders0a)
                          )
                    | otherwise = newHeaders0a
              when (rpDetectLoops policy && resolvedKey `elem` visited) $
                throwIO (RedirectLoop (reverse (resolvedKey : visited)))
              let req' =
                    req
                      { WReq.requestURI = WURI.staticURI (TE.decodeUtf8 (WURI.renderURI resolved))
                      , WReq.method = newMethod
                      , WReq.headers = newHeaders1
                      }
              go (resolvedKey : visited) (n - 1) req' newBody
  go [] (rpMaxRedirects policy) req0 buffered
  where
    dummyURI =
      WURI.URI
        { WURI.uriScheme = WURI.SchemeHttp
        , WURI.uriAuthority = WURI.Authority "" Nothing
        , WURI.uriUserinfoBytes = Nothing
        , WURI.uriPath = "/"
        , WURI.uriQuery = ""
        , WURI.uriFragment = ""
        }


{- | Resolve a redirect Location against the request URI per RFC
3986 §5.2 \"Transform References\".

* Absolute Locations win outright.
* Network-relative (@\/\/host\/path@) inherits the scheme.
* Path-relative inherits scheme and authority and is resolved
  against the base path with dot-segment removal.
-}
resolveLocation :: WURI.URI -> ByteString -> WURI.URI
resolveLocation base loc
  | "http://" `BS.isPrefixOf` loc || "https://" `BS.isPrefixOf` loc =
      case WURI.parseURI loc of
        Right u -> u {WURI.uriPath = WURI.removeDotSegments (WURI.uriPath u)}
        Left _ -> base
  | "//" `BS.isPrefixOf` loc =
      let withScheme = case WURI.uriScheme base of
            WURI.SchemeHttp -> "http:" <> loc
            WURI.SchemeHttps -> "https:" <> loc
      in case WURI.parseURI withScheme of
           Right u -> u {WURI.uriPath = WURI.removeDotSegments (WURI.uriPath u)}
           Left _ -> base
  | "/" `BS.isPrefixOf` loc =
      let (path, rest) = BS.break (\b -> b == 0x3F || b == 0x23) loc
          (qry, frag) = case BS.uncons rest of
            Just (0x3F, q) ->
              let (q', f) = BS.break (== 0x23) q
              in (q', dropHash f)
            Just (0x23, f) -> (BS.empty, f)
            _ -> (BS.empty, BS.empty)
      in base
           { WURI.uriPath = WURI.removeDotSegments path
           , WURI.uriQuery = qry
           , WURI.uriFragment = frag
           }
  | otherwise =
      -- Path-relative: merge against the base path's directory and
      -- run the result through removeDotSegments so @..@ / @.@
      -- collapse correctly.
      let basePath = WURI.uriPath base
          stripped = BS.reverse (BS.dropWhile (/= 0x2F) (BS.reverse basePath))
          basePath' = if BS.null stripped then "/" else stripped
          (path, rest) = BS.break (\b -> b == 0x3F || b == 0x23) loc
          (qry, frag) = case BS.uncons rest of
            Just (0x3F, q) ->
              let (q', f) = BS.break (== 0x23) q
              in (q', dropHash f)
            Just (0x23, f) -> (BS.empty, f)
            _ -> (BS.empty, BS.empty)
      in base
           { WURI.uriPath = WURI.removeDotSegments (basePath' <> path)
           , WURI.uriQuery = qry
           , WURI.uriFragment = frag
           }
  where
    dropHash b = case BS.uncons b of
      Just (0x23, r) -> r
      _ -> b


origin :: WURI.URI -> (WURI.Scheme, ByteString, Int)
origin u = (WURI.uriScheme u, WURI.uriHost u, WURI.uriPort u)
