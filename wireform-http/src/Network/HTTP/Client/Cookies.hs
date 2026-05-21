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
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word8)

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

renderCookieHeader :: [Cookie] -> ByteString
renderCookieHeader =
  BS.intercalate "; " . map (\c -> cookieName c <> "=" <> cookieValue c)

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
-- Set-Cookie parsing
-- ---------------------------------------------------------------------------

parseSetCookieHeaders :: UTCTime -> URI -> [H.Header] -> [Cookie]
parseSetCookieHeaders now ctx hdrs =
  [ c
  | (n, v) <- hdrs
  , n == H.hSetCookie
  , Just c <- [parseSetCookie now ctx v]
  ]

parseSetCookie :: UTCTime -> URI -> ByteString -> Maybe Cookie
parseSetCookie now ctx raw = do
  let pieces = map (BS.dropWhile (== 0x20)) (BS.split 0x3B raw)
  case pieces of
    []          -> Nothing
    (nv : rest) -> do
      let (n0, v0) = BS.break (== 0x3D) nv
      case BS.uncons v0 of
        Nothing -> Nothing
        Just (0x3D, val) -> do
          let attrs = parseAttrs rest
              base = Cookie
                { cookieName     = BS.dropWhileEnd (== 0x20) n0
                , cookieValue    = val
                , cookieDomain   = BS8.map toLower (uriHost ctx)
                , cookiePath     = defaultPath (uriPath ctx)
                , cookieExpires  = Nothing
                , cookieSecure   = False
                , cookieHttpOnly = False
                , cookieSameSite = SameSiteLax
                }
          pure (List.foldl' applyAttr base attrs)
        _ -> Nothing

  where
    defaultPath p
      | BS.null p          = "/"
      | not ("/" `BS.isPrefixOf` p) = "/"
      | otherwise          =
          let stripped = BS.reverse (BS.dropWhile (/= 0x2F) (BS.reverse p))
              cleaned  = if BS.null stripped then "/" else stripped
          in if BS.length cleaned > 1
               then BS.reverse (BS.dropWhile (== 0x2F) (BS.reverse cleaned))
               else cleaned

    parseAttrs = map parseAttr
    parseAttr a =
      let (k0, v0) = BS.break (== 0x3D) a
          k = BS8.map toLower (BS.dropWhileEnd (== 0x20) k0)
          v = case BS.uncons v0 of
                Just (0x3D, vv) -> BS.dropWhile (== 0x20) vv
                _               -> ""
      in (k, v)
    applyAttr c (k, v)
      | k == "domain"   = c { cookieDomain = BS8.map toLower (stripLeadingDot v) }
      | k == "path"     = c { cookiePath   = if BS.null v then cookiePath c else v }
      | k == "secure"   = c { cookieSecure = True }
      | k == "httponly" = c { cookieHttpOnly = True }
      | k == "max-age"  = case BS8.readInteger v of
          Just (i, _) -> c { cookieExpires = Just (addUTCTime (fromInteger i :: NominalDiffTime) now) }
          Nothing     -> c
      | k == "samesite" = c { cookieSameSite = parseSameSite v }
      | otherwise       = c
    stripLeadingDot d = case BS.uncons d of
      Just (0x2E, rest) -> rest
      _                 -> d
    parseSameSite v = case BS8.map toLower v of
      "strict" -> SameSiteStrict
      "none"   -> SameSiteNone
      _        -> SameSiteLax

-- Drop -Wunused on Word8.
_unusedWord :: Word8
_unusedWord = 0
