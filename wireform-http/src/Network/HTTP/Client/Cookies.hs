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
  , newCookieJarWithPSL
  , insertCookie
  , insertCookieChecked
  , getCookies
  , clearCookies
  , pruneExpired
  , withCookies
    -- * Validation and public-suffix hook
  , CookieError (..)
  , validateCookieName
  , validateCookieValue
  , PublicSuffixCheck
  , noPublicSuffix
    -- * Limits
  , CookieJarLimits (..)
  , defaultCookieJarLimits
  , newCookieJarWith
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

import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
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
    -- ^ Effective domain. Defaulted from the request host when
    --   the response carried no @Domain=...@ attribute.
  , cookieDomainExplicit :: !Bool
    -- ^ 'True' when the response carried a @Domain=...@ attribute
    --   (defaulted from the request host otherwise). Needed by
    --   the @__Host-@ prefix check.
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

-- | Predicate run on @Set-Cookie@'s @Domain=...@: returns 'True' for
-- a domain that's on the public suffix list (e.g. @co.uk@,
-- @github.io@) and so MUST NOT have cookies set against it.
-- Defaults to 'noPublicSuffix' which never reports a hit; wire your
-- own (e.g. backed by @publicsuffix@ data) for browser-grade
-- behaviour.
type PublicSuffixCheck = ByteString -> Bool

-- | The trivial 'PublicSuffixCheck' that never rejects.  Use this
-- when the application is talking to a known endpoint set and
-- doesn't need PSL enforcement.
noPublicSuffix :: PublicSuffixCheck
noPublicSuffix = const False

-- | Per-cookie size limits applied at ingest. The defaults match
-- the browser-ecosystem convergence point; override for backends
-- that need to exceed them.
data CookieJarLimits = CookieJarLimits
  { cjlMaxNameValueBytes :: !Int
    -- ^ Max combined byte size of @name=value@. Default 4096.
  , cjlMaxAttributeBytes :: !Int
    -- ^ Max byte size of any single attribute (Domain, Path, …).
    --   Default 1024.
  }
  deriving stock (Eq, Show)

defaultCookieJarLimits :: CookieJarLimits
defaultCookieJarLimits = CookieJarLimits
  { cjlMaxNameValueBytes = 4096
  , cjlMaxAttributeBytes = 1024
  }

data CookieJar = CookieJar
  { cjStore  :: !(TVar (Map CookieKey Cookie))
  , cjPSL    :: !PublicSuffixCheck
  , cjLimits :: !CookieJarLimits
  }

newCookieJar :: IO CookieJar
newCookieJar = newCookieJarWith noPublicSuffix defaultCookieJarLimits

-- | Allocate a 'CookieJar' with a custom public-suffix check.
newCookieJarWithPSL :: PublicSuffixCheck -> IO CookieJar
newCookieJarWithPSL psl = newCookieJarWith psl defaultCookieJarLimits

-- | Allocate a 'CookieJar' with both a custom PSL check and size
-- limits.
newCookieJarWith :: PublicSuffixCheck -> CookieJarLimits -> IO CookieJar
newCookieJarWith psl limits = do
  s <- newTVarIO Map.empty
  pure CookieJar { cjStore = s, cjPSL = psl, cjLimits = limits }

cookieKey :: Cookie -> CookieKey
cookieKey c = (cookieDomain c, cookiePath c, cookieName c)

insertCookie :: CookieJar -> Cookie -> IO ()
insertCookie jar c =
  atomically $ modifyTVar' (cjStore jar) (Map.insert (cookieKey c) c)

-- | Insert with full RFC 6265bis validation: name and value must pass
-- the cookie-octet grammar, and the cookie's @Domain@ must not be
-- on the configured public suffix list.
insertCookieChecked :: CookieJar -> Cookie -> IO (Either CookieError ())
insertCookieChecked jar c = case validateCookie jar c of
  Left err -> pure (Left err)
  Right () -> Right () <$ insertCookie jar c

getCookies :: CookieJar -> IO [Cookie]
getCookies jar = Map.elems <$> readTVarIO (cjStore jar)

clearCookies :: CookieJar -> IO ()
clearCookies jar = atomically $ writeTVar (cjStore jar) Map.empty

pruneExpired :: CookieJar -> IO ()
pruneExpired jar = do
  now <- getCurrentTime
  atomically $ modifyTVar' (cjStore jar) (Map.filter (notExpired now))

-- ---------------------------------------------------------------------------
-- Validation
-- ---------------------------------------------------------------------------

data CookieError
  = CookieNameInvalid !ByteString
  | CookieValueInvalid !ByteString
  | CookieDomainIsPublicSuffix !ByteString
  | CookieDomainNotMatching !ByteString !ByteString
    -- ^ @Domain=…@ is not a parent of the request host. First
    --   field is the request host; second is the offending
    --   attribute value.
  | CookieSameSiteNoneRequiresSecure !ByteString
    -- ^ @SameSite=None@ without @Secure@ is rejected by every
    --   modern UA; the field carries the cookie's name.
  | CookieSecurePrefixWithoutSecure !ByteString
    -- ^ @__Secure-@-prefixed cookie without the @Secure@ flag.
  | CookieHostPrefixViolation !ByteString
    -- ^ @__Host-@-prefixed cookie missing one of the constraints
    --   (must have @Secure@, no @Domain@, @Path=\/@).
  | CookieTooLarge !Int !Int
    -- ^ Cookie size exceeds the configured limit (size, limit).
  deriving stock (Eq, Show)

-- | RFC 6265bis cookie-name grammar: a token (RFC 9110 \u00a75.6.2).
validateCookieName :: ByteString -> Either CookieError ()
validateCookieName bs
  | BS.null bs = Left (CookieNameInvalid bs)
  | BS.all isToken bs = Right ()
  | otherwise = Left (CookieNameInvalid bs)
  where
    isToken w =
         (w >= 0x30 && w <= 0x39)            -- 0-9
      || (w >= 0x41 && w <= 0x5A)            -- A-Z
      || (w >= 0x61 && w <= 0x7A)            -- a-z
      || w == 0x21
      || (w >= 0x23 && w <= 0x27)
      || w == 0x2A || w == 0x2B
      || w == 0x2D || w == 0x2E
      || w == 0x5E || w == 0x5F || w == 0x60
      || w == 0x7C || w == 0x7E

-- | RFC 6265bis cookie-octet: %x21 \/ %x23-2B \/ %x2D-3A \/ %x3C-5B \/
-- %x5D-7E (anything printable ASCII minus CTL, whitespace, double
-- quote, comma, semicolon, and backslash).
validateCookieValue :: ByteString -> Either CookieError ()
validateCookieValue bs
  | BS.all isCookieOctet bs = Right ()
  | otherwise = Left (CookieValueInvalid bs)
  where
    isCookieOctet w =
         w == 0x21
      || (w >= 0x23 && w <= 0x2B)
      || (w >= 0x2D && w <= 0x3A)
      || (w >= 0x3C && w <= 0x5B)
      || (w >= 0x5D && w <= 0x7E)

validateCookie :: CookieJar -> Cookie -> Either CookieError ()
validateCookie jar c = do
  validateCookieName  (cookieName  c)
  validateCookieValue (cookieValue c)
  validateSize jar c
  validatePrefixes c
  validateSameSite c
  if cjPSL jar (cookieDomain c)
    then Left (CookieDomainIsPublicSuffix (cookieDomain c))
    else Right ()

-- | RFC 6265bis: the @__Secure-@ prefix requires @Secure@; the
-- @__Host-@ prefix additionally requires no @Domain@ and
-- @Path=\/@.
validatePrefixes :: Cookie -> Either CookieError ()
validatePrefixes c
  | "__Host-" `BS.isPrefixOf` cookieName c =
      if cookieSecure c
         && cookiePath c == "/"
         && not (cookieDomainExplicit c)
        then Right ()
        else Left (CookieHostPrefixViolation (cookieName c))
  | "__Secure-" `BS.isPrefixOf` cookieName c =
      if cookieSecure c
        then Right ()
        else Left (CookieSecurePrefixWithoutSecure (cookieName c))
  | otherwise = Right ()

validateSameSite :: Cookie -> Either CookieError ()
validateSameSite c = case cookieSameSite c of
  SameSiteNone | not (cookieSecure c) ->
    Left (CookieSameSiteNoneRequiresSecure (cookieName c))
  _ -> Right ()

validateSize :: CookieJar -> Cookie -> Either CookieError ()
validateSize jar c =
  let nv  = BS.length (cookieName c) + 1 + BS.length (cookieValue c)
      lim = cjlMaxNameValueBytes (cjLimits jar)
  in if nv > lim then Left (CookieTooLarge nv lim) else Right ()

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
withCookies jar inner = Transport $ \req -> do
  now <- getCurrentTime
  all_ <- readTVarIO (cjStore jar)
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
      let setCookies =
            [ c
            | c <- parseSetCookieHeaders now resolved
                     (Network.HTTP.Client.Response.headers raw)
            -- Drop cookies whose Domain is a public suffix; this is the
            -- principal protection the PSL gives the cookie subsystem.
            , not (cjPSL jar (cookieDomain c))
            -- Validate the cookie's name and value at the boundary;
            -- silently ignore malformed entries.
            , Right () <- [validateCookieName  (cookieName  c)]
            , Right () <- [validateCookieValue (cookieValue c)]
            ]
      atomically $ modifyTVar' (cjStore jar) $ \m ->
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
      hasDomain    = case Hermes.setCookieDomain sc of
        Just _  -> True
        Nothing -> False
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
       , cookieDomainExplicit = hasDomain
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
