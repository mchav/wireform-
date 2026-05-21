{- | Cookie jar and middleware.

Implements a small subset of RFC 6265: scheme, host, path, expiry,
and the 'secure' flag are honoured for matching. @SameSite@ and
@Partitioned@ flags are parsed but not enforced (they exist for
browsers — an HTTP client doesn't have a top-level navigation
context to compare against). 'HttpOnly' is also parsed but
ignored, again because it's a browser-context restriction.

The jar is intentionally stored in an 'STM.TVar' for cheap
concurrent reads.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Cookies
  ( CookieJar
  , Cookie (..)
  , SameSite (..)
  , newCookieJar
  , insertCookie
  , getCookies
  , clearCookies
  , pruneExpired
  , withCookies
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.Char (toLower)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Short as ST
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)

import FlatParse.Basic (Result (..), runParser)
import qualified Wireform.Builder as WB

import qualified Network.HTTP.Headers.Cookie as Hermes
import qualified Network.HTTP.Headers.SetCookie as Hermes

import qualified Network.HTTP.Types.Header as H

import Network.HTTP.Client.Request
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI

-- ---------------------------------------------------------------------------
-- Cookie data
-- ---------------------------------------------------------------------------

data SameSite = SameSiteStrict | SameSiteLax | SameSiteNone
  deriving stock (Eq, Show)

data Cookie = Cookie
  { cookieName     :: !ByteString
  , cookieValue    :: !ByteString
  , cookieDomain   :: !ByteString
  , cookiePath     :: !ByteString
  , cookieExpires  :: !(Maybe UTCTime)
  , cookieSecure   :: !Bool
  , cookieHttpOnly :: !Bool
  , cookieSameSite :: !SameSite
  }
  deriving stock (Eq, Show)

-- The map key is @(domain, path, name)@: per RFC 6265, two cookies
-- with the same name but different domain or path are distinct.
type CookieKey = (ByteString, ByteString, ByteString)

newtype CookieJar = CookieJar (TVar (Map CookieKey Cookie))

newCookieJar :: IO CookieJar
newCookieJar = CookieJar <$> newTVarIO Map.empty

cookieKey :: Cookie -> CookieKey
cookieKey c = (cookieDomain c, cookiePath c, cookieName c)

insertCookie :: CookieJar -> Cookie -> IO ()
insertCookie (CookieJar var) c =
  atomically $ modifyTVar' var (Map.insert (cookieKey c) c)

getCookies :: CookieJar -> IO [Cookie]
getCookies (CookieJar var) = Map.elems <$> readTVarIO var

clearCookies :: CookieJar -> IO ()
clearCookies (CookieJar var) = atomically $ writeTVar var Map.empty

pruneExpired :: CookieJar -> IO ()
pruneExpired (CookieJar var) = do
  now <- getCurrentTime
  atomically $ modifyTVar' var (Map.filter (notExpired now))

notExpired :: UTCTime -> Cookie -> Bool
notExpired now c = case cookieExpires c of
  Nothing -> True
  Just e  -> e > now

-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

-- | Cookie middleware: attach matching @Cookie@ header values to
-- outgoing requests, parse @Set-Cookie@ on responses and store into
-- the jar.
withCookies :: CookieJar -> Middleware IO
withCookies (CookieJar var) inner = Transport $ \req -> do
  now <- getCurrentTime
  all_ <- readTVarIO var
  case renderRequestURI (requestURI req) of
    Left _ -> sendRaw inner req
    Right resolved -> do
      let matching = matchCookies now resolved (Map.elems all_)
          req' = case matching of
                   [] -> req
                   _  -> req
                          { Network.HTTP.Client.Request.headers =
                              H.insertHeader H.hCookie (renderCookieHeader matching)
                                (Network.HTTP.Client.Request.headers req)
                          }
      raw <- sendRaw inner req'
      let setCookies = parseSetCookieHeaders now resolved (Network.HTTP.Client.Response.headers raw)
      atomically $ modifyTVar' var $ \m ->
        List.foldl' (\acc c -> Map.insert (cookieKey c) c acc) m setCookies
      pure raw

-- | Render an outgoing @Cookie@ header value via hermes's
-- 'Hermes.renderCookie', so the serialisation matches the same RFC
-- 6265 grammar as the response-side parser.
renderCookieHeader :: [Cookie] -> ByteString
renderCookieHeader cs =
  let mk c = Hermes.CookiePair
        { Hermes.cookieName  = bytesToShort (cookieName  c)
        , Hermes.cookieValue = bytesToShort (cookieValue c)
        }
  in WB.toStrictByteString (Hermes.renderCookie (Hermes.Cookie (map mk cs)))

-- | UTF-8 'ByteString' to 'ShortText'. Cookie names and values are
-- ASCII / token-safe in practice, but fall back to a lossless
-- encoding for any non-UTF-8 bytes so we never throw at the
-- serialisation boundary.
bytesToShort :: ByteString -> ST.ShortText
bytesToShort bs = case ST.fromByteString bs of
  Just t  -> t
  Nothing -> ST.fromText (TE.decodeUtf8With (\_ _ -> Just '\xfffd') bs)

-- RFC 6265 § 5.4 (simplified): a cookie matches if its domain
-- matches the request host, its path is a prefix of the request
-- path, the scheme is HTTPS if 'secure' is set, and it hasn't
-- expired.
matchCookies :: UTCTime -> URI -> [Cookie] -> [Cookie]
matchCookies now uri_ = filter ok
  where
    host = BS8.map toLower (uriHost uri_)
    pathBytes = uriPath uri_
    secureReq = uriScheme uri_ == SchemeHttps
    ok c =
      notExpired now c
        && domainMatches host (cookieDomain c)
        && pathMatches pathBytes (cookiePath c)
        && (not (cookieSecure c) || secureReq)

domainMatches :: ByteString -> ByteString -> Bool
domainMatches host dom
  | host == dom = True
  | BS.length host > BS.length dom + 1
  , dom `BS.isSuffixOf` host
  , BS.index host (BS.length host - BS.length dom - 1) == 0x2E = True
  | otherwise = False

pathMatches :: ByteString -> ByteString -> Bool
pathMatches reqPath cookiePathBs
  | reqPath == cookiePathBs = True
  | cookiePathBs `BS.isPrefixOf` reqPath
  , BS.length reqPath > BS.length cookiePathBs
  , BS.index reqPath (BS.length cookiePathBs) == 0x2F = True
  | cookiePathBs == "/" = True
  | otherwise = False

-- ---------------------------------------------------------------------------
-- Set-Cookie parsing (delegates to hermes)
-- ---------------------------------------------------------------------------

parseSetCookieHeaders :: UTCTime -> URI -> [H.Header] -> [Cookie]
parseSetCookieHeaders now ctx hdrs =
  [ c
  | (n, v) <- hdrs
  , n == H.hSetCookie
  , Just c <- [parseSetCookie now ctx v]
  ]

-- | Parse a single @Set-Cookie@ header value via
-- 'Hermes.setCookieParser' and project the result into the
-- wireform 'Cookie' shape, applying the RFC 6265 defaults for any
-- attributes the response omitted.
parseSetCookie :: UTCTime -> URI -> ByteString -> Maybe Cookie
parseSetCookie now ctx raw =
  case runParser Hermes.setCookieParser raw of
    OK sc _ -> Just (fromHermesSetCookie now ctx sc)
    _       -> Nothing

fromHermesSetCookie :: UTCTime -> URI -> Hermes.SetCookie -> Cookie
fromHermesSetCookie now ctx sc =
  let stBytes      = TE.encodeUtf8 . ST.toText
      domainBytes  = maybe (BS8.map toLower (uriHost ctx))
                           (BS8.map toLower . stripLeadingDot . stBytes)
                           (Hermes.setCookieDomain sc)
      pathBytes    = maybe (defaultPath (uriPath ctx)) stBytes
                           (Hermes.setCookiePath sc)
      -- RFC 6265 §5.2.2: when both Expires and Max-Age are given,
      -- Max-Age wins. Hermes parses both into separate fields, so we
      -- combine here.
      expires      = case Hermes.setCookieMaxAge sc of
        Just s  -> Just (addUTCTime (fromIntegral s :: NominalDiffTime) now)
        Nothing -> Hermes.setCookieExpires sc
  in Cookie
       { cookieName     = stBytes (Hermes.setCookieName sc)
       , cookieValue    = stBytes (Hermes.setCookieValue sc)
       , cookieDomain   = domainBytes
       , cookiePath     = pathBytes
       , cookieExpires  = expires
       , cookieSecure   = Hermes.setCookieSecure sc
       , cookieHttpOnly = Hermes.setCookieHttpOnly sc
       , cookieSameSite = case Hermes.setCookieSameSite sc of
           Just Hermes.SameSiteStrict -> SameSiteStrict
           Just Hermes.SameSiteLax    -> SameSiteLax
           Just Hermes.SameSiteNone   -> SameSiteNone
           Nothing                    -> SameSiteLax
       }

defaultPath :: ByteString -> ByteString
defaultPath p
  | BS.null p                   = "/"
  | not ("/" `BS.isPrefixOf` p) = "/"
  | otherwise =
      let stripped = BS.reverse (BS.dropWhile (/= 0x2F) (BS.reverse p))
          cleaned  = if BS.null stripped then "/" else stripped
      in if BS.length cleaned > 1
           then BS.reverse (BS.dropWhile (== 0x2F) (BS.reverse cleaned))
           else cleaned

stripLeadingDot :: ByteString -> ByteString
stripLeadingDot d = case BS.uncons d of
  Just (0x2E, rest) -> rest
  _                 -> d
