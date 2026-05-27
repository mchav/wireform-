{- | URI representation and helpers for the wireform HTTP client.

Two layers:

* 'URI' — a parsed URI with explicit scheme \/ authority \/ path \/ query
  \/ fragment fields. The base transport extracts the host and port
  from here for connection routing.
* 'RequestURI' — a URI built from an 'UriTemplate' plus a bag of
  variable bindings. Templates are kept lazy: bindings accumulate
  via 'bindVar' and the template is rendered to bytes when the
  request is sent. Middleware that wants to inspect the URI calls
  'renderRequestURI' or 'requestURI'.

A 'BaseURL' is the prefix half of a 'URI' (scheme + authority + path
prefix) and gets composed with a relative request URI by the
'withBaseURL' middleware.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.URI
  ( -- * Parsed URIs
    URI (..)
  , Scheme (..)
  , Authority (..)
  , parseURI
  , renderURI
  , uriHost
  , uriPort
  , uriPathAndQuery
    -- * Userinfo and IPv6 helpers
  , uriUserinfo
  , isIPv6Host
  , renderHostPort
    -- * IDN
  , isIdnaSafe
  , validateHost
  , hostToAscii
  , parseURIIdna
    -- * Query helpers
  , addQueryParam
  , addQueryParams
  , setQueryParams
  , queryParams
    -- * Normalization (RFC 3986 §6)
  , normalizeURI
  , removeDotSegments
  , normalizePercentEncoding
    -- * Reference resolution (RFC 3986 §5)
  , resolveReference
    -- * Base URLs
  , BaseURL (..)
  , parseBaseURL
  , baseURL
  , unsafeBaseURL
  , renderBaseURL
  , resolveAgainst
    -- * URI templates (re-exports from uri-templater)
  , UriTemplate
  , uri
  , parseTemplate
  , BoundValue
  , ToTemplateValue (..)
    -- * RequestURI: template + bindings carried on a Request
  , RequestURI (..)
  , templateURI
  , staticURI
  , bindVar
  , bindVars
  , renderRequestURI
  , requestURIToText
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.URI.Template (ToTemplateValue (..), UriTemplate, parseTemplate, uri)
import Network.URI.Template.Internal (BoundValue)
import Network.URI.Template.Types (WrappedValue (..))
import qualified Network.URI.Template as Template

import qualified Data.Text.IDN as IDN

import qualified Network.HTTP.PercentEncoding as PE

-- | URL scheme. Only HTTP and HTTPS are reified here. The base
-- transport keys connection routing on this.
data Scheme = SchemeHttp | SchemeHttps
  deriving stock (Eq, Show)

-- | Network-level authority: host + optional explicit port.
data Authority = Authority
  { authHost :: !ByteString
  , authPort :: !(Maybe Int)
  }
  deriving stock (Eq, Show)

-- | A parsed absolute URI. The wireform client only ever operates on
-- absolute URIs at the wire boundary — relative URIs are composed
-- with a 'BaseURL' upstream via 'withBaseURL'.
data URI = URI
  { uriScheme    :: !Scheme
  , uriAuthority :: !Authority
  , uriUserinfoBytes :: !(Maybe ByteString)
    -- ^ Userinfo as bytes (\"user\" or \"user:password\"), without
    --   any percent-decoding. RFC 3986 \u00a73.2.1 reserves this slot
    --   in the URI; HTTP transports treat it as input to Basic auth
    --   rather than putting it on the wire.
  , uriPath      :: !ByteString
    -- ^ Path component starting with @\"/\"@ (or empty for an
    --   authority-only URI like @http:\/\/host@; we normalise to
    --   @\"/\"@ when sending).
  , uriQuery     :: !ByteString
    -- ^ Query component without the leading @\"?\"@. Empty if absent.
  , uriFragment  :: !ByteString
    -- ^ Fragment without the leading @\"#\"@. Empty if absent.
  }
  deriving stock (Eq, Show)

defaultPort :: Scheme -> Int
defaultPort SchemeHttp  = 80
defaultPort SchemeHttps = 443

-- | The effective port for this URI (explicit if set, otherwise the
-- scheme default).
uriPort :: URI -> Int
uriPort u = case authPort (uriAuthority u) of
  Just p  -> p
  Nothing -> defaultPort (uriScheme u)

-- | The effective host (lowercased ASCII bytes). For an IPv6 literal
-- this returns the address bytes /without/ surrounding brackets;
-- 'renderHostPort' adds them back.
uriHost :: URI -> ByteString
uriHost = authHost . uriAuthority

-- | Userinfo as bytes (without any percent-decoding), or 'Nothing'.
uriUserinfo :: URI -> Maybe ByteString
uriUserinfo = uriUserinfoBytes

-- | True if @host@ looks like an IPv6 literal (contains @:@). Used by
-- the renderer to decide whether to add brackets.
isIPv6Host :: ByteString -> Bool
isIPv6Host = BS.elem 0x3A

-- | Render a @host[:port]@ pair, bracketing the host if it's IPv6.
-- The port is omitted when it equals the scheme default.
renderHostPort :: Scheme -> Authority -> ByteString
renderHostPort sch (Authority h mPort) =
  let hostBs = if isIPv6Host h then "[" <> h <> "]" else h
      portBs = case mPort of
        Just p
          | p == defaultPort sch -> ""
          | otherwise            -> ":" <> BS8.pack (show p)
        Nothing -> ""
  in hostBs <> portBs

-- | True if every byte in the host is ASCII.
isIdnaSafe :: ByteString -> Bool
isIdnaSafe = BS.all (< 0x80)

-- | Validate (and IDN-encode) a host bytestring for transport.
-- Accepts:
--
-- * IPv6 literals (any bytes; the bracket structure is enforced
--   upstream by 'parseURI').
-- * ASCII-only labels: returned unchanged.
-- * Non-ASCII U-labels: converted to IDNA A-labels (@xn--...@) via
--   'Data.Text.IDN.toASCII' (RFC 5890\u20135893 \/ RFC 3492). The
--   bytes are interpreted as UTF-8.
--
-- Returns 'Left' with a diagnostic message if conversion fails (bad
-- UTF-8, prohibited code points, bidi violations, etc.).
validateHost :: ByteString -> Either String ByteString
validateHost h
  | isIPv6Host h = Right h
  | isIdnaSafe h = Right h
  | otherwise   = hostToAscii (TE.decodeUtf8 h)

-- | Convert a Unicode host name to its IDNA A-label form via the
-- @idn@ library. Uses 'IDN.toASCII' which does the per-label
-- punycoding and IDNA2008 validation.
hostToAscii :: Text -> Either String ByteString
hostToAscii t = case IDN.toASCII t of
  Right ascii -> Right (TE.encodeUtf8 ascii)
  Left  e     -> Left ("IDNA conversion failed: " <> show e)

-- | Like 'parseURI' but IDN-encodes the host when it contains
-- non-ASCII bytes. Input is 'Text' so the UTF-8 boundary is
-- explicit.
parseURIIdna :: Text -> Either String URI
parseURIIdna t = do
  u <- parseURI (TE.encodeUtf8 t)
  let auth = uriAuthority u
      h    = authHost auth
  if isIPv6Host h || isIdnaSafe h
    then Right u
    else do
      h' <- hostToAscii (TE.decodeUtf8 h)
      Right u { uriAuthority = auth { authHost = h' } }

-- | Concatenation of path and query suitable for the HTTP\/1.1
-- request-target or HTTP\/2 @:path@ pseudo-header.
uriPathAndQuery :: URI -> ByteString
uriPathAndQuery u =
  let p = if BS.null (uriPath u) then "/" else uriPath u
      q = uriQuery u
  in if BS.null q then p else p <> "?" <> q

-- | Render a 'URI' back to its canonical absolute form.
renderURI :: URI -> ByteString
renderURI u =
  let scheme = case uriScheme u of
        SchemeHttp  -> "http://"
        SchemeHttps -> "https://"
      ui = case uriUserinfoBytes u of
        Just bs -> bs <> "@"
        Nothing -> ""
      hostBs = let h = authHost (uriAuthority u)
               in if isIPv6Host h then "[" <> h <> "]" else h
      portBs = case authPort (uriAuthority u) of
        Nothing -> ""
        Just p  -> ":" <> BS8.pack (show p)
      path  = if BS.null (uriPath u) then "/" else uriPath u
      query = if BS.null (uriQuery u) then "" else "?" <> uriQuery u
      frag  = if BS.null (uriFragment u) then "" else "#" <> uriFragment u
  in scheme <> ui <> hostBs <> portBs <> path <> query <> frag

-- | Parse an absolute URI of the form
-- @scheme:\/\/[userinfo\@]authority\/path?query#fragment@.
--
-- Limitations: only HTTP\/HTTPS schemes; no percent-decoding of any
-- component (callers can run that themselves via
-- "Network.HTTP.PercentEncoding"). Returns 'Left' with a human-readable
-- reason on failure.
parseURI :: ByteString -> Either String URI
parseURI bs = do
  (scheme, rest1) <- splitScheme bs
  rest2           <- stripPrefixBS "//" rest1 `orFail` "URI must be authority-form"
  -- Userinfo: split on the rightmost '@' that occurs before any '/', '?', or '#'.
  let (preCutoff, _) = BS.break (\b -> b == 0x2F || b == 0x3F || b == 0x23) rest2
      atIdx          = BS.elemIndexEnd 0x40 preCutoff
      (userinfo, hostStart) = case atIdx of
        Just i  ->
          let (ui, rest) = BS.splitAt i rest2
          in (Just ui, BS.drop 1 rest)
        Nothing -> (Nothing, rest2)
      (authBs, pathQF) = splitAuthority hostStart
  auth <- parseAuthority authBs
  let (pathBs, qfBs) = BS.break (\b -> b == 0x3F || b == 0x23) pathQF
      (queryBs0, fragBs0) =
        case BS.uncons qfBs of
          Just (0x3F, q) ->
            let (q', rest) = BS.break (== 0x23) q
            in (q', dropHash rest)
          Just (0x23, f) -> ("", f)
          _              -> ("", "")
  pure URI
    { uriScheme        = scheme
    , uriAuthority     = auth
    , uriUserinfoBytes = userinfo
    , uriPath          = if BS.null pathBs then "/" else pathBs
    , uriQuery         = queryBs0
    , uriFragment      = fragBs0
    }
  where
    dropHash b = case BS.uncons b of
      Just (0x23, r) -> r
      _              -> b
    orFail Nothing msg = Left msg
    orFail (Just x) _  = Right x

    -- Authority ends at the first '/', '?', or '#' /outside/ a
    -- bracketed IPv6 literal. We only have to honour bracketing for
    -- the colon-as-port-separator below, but for the authority
    -- terminator we still split on those three byte values because
    -- they can't appear inside an IPv6 literal anyway.
    splitAuthority = BS.break (\b -> b == 0x2F || b == 0x3F || b == 0x23)

splitScheme :: ByteString -> Either String (Scheme, ByteString)
splitScheme bs =
  let (sBs, rest) = BS.break (== 0x3A) bs
      lowered    = BS8.pack (map toLower (BS8.unpack sBs))
  in case BS.uncons rest of
       Just (0x3A, body)
         | lowered == "http"  -> Right (SchemeHttp,  body)
         | lowered == "https" -> Right (SchemeHttps, body)
         | otherwise          -> Left  ("unsupported URI scheme: " <> BS8.unpack sBs)
       _ -> Left "URI missing scheme"

parseAuthority :: ByteString -> Either String Authority
parseAuthority bs0
  | BS.null bs0 = Left "URI missing host authority"
    -- IPv6-literal: @[...]@ optionally followed by @:port@.
  | Just (0x5B, _) <- BS.uncons bs0 =
      case BS.elemIndex 0x5D bs0 of
        Nothing -> Left "URI: unterminated IPv6 authority"
        Just j  ->
          let host = BS.take j (BS.drop 1 bs0)        -- drop '[' and ']'
              tail_ = BS.drop (j + 1) bs0
          in case BS.uncons tail_ of
               Nothing -> Right (Authority host Nothing)
               Just (0x3A, portBs) -> case BS8.readInt portBs of
                 Just (n, leftover) | BS.null leftover ->
                   Right (Authority host (Just n))
                 _ -> Left ("URI: invalid IPv6 port: " <> BS8.unpack portBs)
               _ -> Left ("URI: junk after IPv6 authority: " <> BS8.unpack tail_)
    -- Plain host or IPv4 literal: split on the rightmost ':' (if any).
  | otherwise = case BS.elemIndexEnd 0x3A bs0 of
      Just i | i > 0 ->
        let (h, p0) = BS.splitAt i bs0
            p       = BS.drop 1 p0
        in case BS8.readInt p of
             Just (n, leftover) | BS.null leftover ->
               Right (Authority h (Just n))
             _ -> Right (Authority bs0 Nothing)
      _ -> Right (Authority bs0 Nothing)

stripPrefixBS :: ByteString -> ByteString -> Maybe ByteString
stripPrefixBS p bs
  | p `BS.isPrefixOf` bs = Just (BS.drop (BS.length p) bs)
  | otherwise            = Nothing

-- ---------------------------------------------------------------------------
-- BaseURL
-- ---------------------------------------------------------------------------

-- | A URL prefix that scopes a transport. Built up of a scheme, an
-- authority, and an optional path prefix (e.g. @\/api\/v2@). Request
-- URIs are composed against this by 'resolveAgainst'.
data BaseURL = BaseURL
  { baseScheme    :: !Scheme
  , baseAuthority :: !Authority
  , basePath      :: !ByteString
  }
  deriving stock (Eq, Show)

-- | Parse a 'BaseURL' from text. Strips any trailing slash from the
-- path so resolution doesn't double-slash.
parseBaseURL :: Text -> Either String BaseURL
parseBaseURL t = do
  u <- parseURI (TE.encodeUtf8 t)
  let p = uriPath u
      p' = if p == "/" then "" else BS.reverse (BS.dropWhile (== 0x2F) (BS.reverse p))
  pure BaseURL
    { baseScheme    = uriScheme u
    , baseAuthority = uriAuthority u
    , basePath      = p'
    }

-- | Total alias for 'parseBaseURL'.
baseURL :: Text -> Either String BaseURL
baseURL = parseBaseURL

-- | 'parseBaseURL' that 'error's on failure. Suitable for literals at
-- application startup.
unsafeBaseURL :: Text -> BaseURL
unsafeBaseURL t = case parseBaseURL t of
  Right b  -> b
  Left err -> error ("unsafeBaseURL: " <> err <> ": " <> T.unpack t)

renderBaseURL :: BaseURL -> ByteString
renderBaseURL b = renderURI URI
  { uriScheme        = baseScheme b
  , uriAuthority     = baseAuthority b
  , uriUserinfoBytes = Nothing
  , uriPath          = if BS.null (basePath b) then "/" else basePath b
  , uriQuery         = ""
  , uriFragment      = ""
  }

-- | Resolve a request URI against a base.
--
-- * If the request URI is already absolute (i.e. has a scheme), it
--   wins — base is ignored. This matches how @httpx@, @sttp@, and
--   browser-style URL resolution behave.
-- * Otherwise the request path is appended to the base path. The
--   query and fragment from the request URI carry over.
resolveAgainst :: BaseURL -> URI -> URI
resolveAgainst base req =
  let basePath' = basePath base
      reqPath   = uriPath req
      joined
        | BS.null basePath' = if BS.null reqPath then "/" else reqPath
        | BS.null reqPath || reqPath == "/" = basePath' <> "/"
        | "/" `BS.isPrefixOf` reqPath = basePath' <> reqPath
        | otherwise = basePath' <> "/" <> reqPath
  in URI
       { uriScheme        = baseScheme base
       , uriAuthority     = baseAuthority base
       , uriUserinfoBytes = uriUserinfoBytes req
       , uriPath          = joined
       , uriQuery         = uriQuery req
       , uriFragment      = uriFragment req
       }

-- ---------------------------------------------------------------------------
-- RequestURI: template + bindings
-- ---------------------------------------------------------------------------

-- | A request URI is a template plus the variable bindings that have
-- been attached so far. Bindings are accumulated lazily: the
-- template is only rendered to bytes when the request is sent (or
-- when middleware explicitly asks for it).
--
-- Bindings later in the list shadow earlier ones for the same name.
data RequestURI = RequestURI
  { uriTemplate  :: !UriTemplate
  , uriBindings  :: ![BoundValue]
  }

instance Show RequestURI where
  show ru = "RequestURI " <> show (Template.renderTemplate (uriTemplate ru))

-- | Build a 'RequestURI' from a 'UriTemplate'.
templateURI :: UriTemplate -> RequestURI
templateURI t = RequestURI { uriTemplate = t, uriBindings = [] }

-- | Build a 'RequestURI' from a literal string. Use this when you have
-- no variables to interpolate; for templates with variables, prefer
-- the @[uri|...|]@ quasi-quoter.
staticURI :: Text -> RequestURI
staticURI t = case parseTemplate (T.unpack t) of
  Right tpl -> templateURI tpl
  Left err  -> error ("staticURI: invalid template: " <> show err)

-- | Attach a variable binding. The newest binding for a given name
-- wins at render time (uri-templater\'s renderer is a left-to-right
-- lookup, so newer bindings need to be earlier in the list).
bindVar :: ToTemplateValue a => Text -> a -> RequestURI -> RequestURI
bindVar name val ru = ru
  { uriBindings = (name, WrappedValue (toTemplateValue val)) : uriBindings ru
  }
{-# INLINE bindVar #-}

-- | Bulk variant of 'bindVar'. Bindings are prepended in reverse, so
-- the first entry in the input list shadows later ones with the
-- same name.
bindVars :: [BoundValue] -> RequestURI -> RequestURI
bindVars new ru = ru { uriBindings = new <> uriBindings ru }

-- | Render the 'RequestURI' to a 'Text' URI. Variables not bound in
-- the request are left unsubstituted (uri-templater's missing-variable
-- behaviour) — that means an unbound variable becomes the empty
-- string in the rendered URI.
requestURIToText :: RequestURI -> Text
requestURIToText ru = Template.renderText (uriTemplate ru) (uriBindings ru)

-- | Render and parse the 'RequestURI' to a fully resolved 'URI'.
--
-- Returns 'Left' if the rendered URI is not a syntactically valid
-- absolute URI (which generally means the caller forgot to attach a
-- 'withBaseURL' middleware to a relative-URI request).
renderRequestURI :: RequestURI -> Either String URI
renderRequestURI ru =
  let txt = requestURIToText ru
      bs  = TE.encodeUtf8 txt
  in parseURI bs

-- ---------------------------------------------------------------------------
-- Query helpers (post-render)
-- ---------------------------------------------------------------------------

-- | Append a single @(key, value)@ to a 'URI''s query, percent-encoding
-- both. The pair is added with @&@ separator if the query is
-- non-empty.
addQueryParam :: ByteString -> ByteString -> URI -> URI
addQueryParam k v u =
  let kv = PE.encodeQueryComponent k <> "=" <> PE.encodeQueryComponent v
      q  = uriQuery u
      q' = if BS.null q then kv else q <> "&" <> kv
  in u { uriQuery = q' }

-- | Append several @(key, value)@ pairs.
addQueryParams :: [(ByteString, ByteString)] -> URI -> URI
addQueryParams kvs u = foldl (\acc (k, v) -> addQueryParam k v acc) u kvs

-- | Replace the query string with the supplied pairs (percent-encoded).
setQueryParams :: [(ByteString, ByteString)] -> URI -> URI
setQueryParams kvs u = u { uriQuery = PE.renderQueryString kvs }

-- | Decode the query string into a list of @(key, value)@ pairs,
-- decoding percent-escapes and the @+@-as-space convention. Pairs
-- with no @=@ surface with an empty value.
queryParams :: URI -> [(ByteString, ByteString)]
queryParams = PE.decodeQueryString . uriQuery

-- ---------------------------------------------------------------------------
-- Normalization (RFC 3986 §6)
-- ---------------------------------------------------------------------------

-- | RFC 3986 §6 syntax-based normalization. Idempotent.
--
-- * Scheme and host are already lowercased at parse time.
-- * Default ports collapse to 'Nothing'.
-- * The path is run through 'removeDotSegments'.
-- * Percent-escapes have their hex digits uppercased and their
--   unreserved-set bytes decoded ('normalizePercentEncoding').
normalizeURI :: URI -> URI
normalizeURI u =
  let auth   = uriAuthority u
      port'  = case authPort auth of
        Just p  | p == defaultPort (uriScheme u) -> Nothing
        other                                    -> other
      path0  = if BS.null (uriPath u) then "/" else uriPath u
      path1  = removeDotSegments path0
      pathN  = normalizePercentEncoding path1
      queryN = normalizePercentEncoding (uriQuery u)
      fragN  = normalizePercentEncoding (uriFragment u)
  in u
       { uriAuthority = auth { authPort = port' }
       , uriPath      = pathN
       , uriQuery     = queryN
       , uriFragment  = fragN
       }

-- | RFC 3986 §5.2.4 \"Remove Dot Segments\". Takes a path like
-- @\/a\/b\/..\/c\/.\/d@ and returns @\/a\/c\/d@.
--
-- Implemented as a forward-walking segment stack rather than the
-- spec's input/output buffer — same fixpoint, simpler proof of
-- termination.
removeDotSegments :: ByteString -> ByteString
removeDotSegments path
  | BS.null path = path
  | otherwise =
      let absolute = "/" `BS.isPrefixOf` path
          segs     = BS.split 0x2F (if absolute then BS.drop 1 path else path)
          go acc [] = reverse acc
          go acc (s : ss)
            | s == "."  = go acc                              ss
            | s == ".." = go (drop 1 acc)                     ss
            | otherwise = go (s : acc)                        ss
          out = go [] segs
          joined = BS.intercalate "/" out
      in if absolute
           then "/" <> joined
           else joined

-- | RFC 3986 §6.2.2.1 \/ §6.2.2.2: uppercase the hex digits of
-- percent-escapes, and decode any escape that names a byte from the
-- unreserved set (those escapes are gratuitous). Other escapes are
-- preserved as-is.
normalizePercentEncoding :: ByteString -> ByteString
normalizePercentEncoding bs0 = BS.pack (go (BS.unpack bs0))
  where
    isUnreservedByte w =
         (w >= 0x41 && w <= 0x5A)   -- A-Z
      || (w >= 0x61 && w <= 0x7A)   -- a-z
      || (w >= 0x30 && w <= 0x39)   -- 0-9
      || w == 0x2D                   -- '-'
      || w == 0x2E                   -- '.'
      || w == 0x5F                   -- '_'
      || w == 0x7E                   -- '~'
    upperHex w
      | w >= 0x61 && w <= 0x66 = w - 0x20
      | otherwise              = w
    go [] = []
    go (0x25 : a : b : rest)
      | Just hi <- hexVal a, Just lo <- hexVal b =
          let byte = hi * 16 + lo
          in if isUnreservedByte byte
               then byte : go rest
               else 0x25 : upperHex a : upperHex b : go rest
    go (w : ws) = w : go ws
    hexVal w
      | w >= 0x30 && w <= 0x39 = Just (w - 0x30)
      | w >= 0x41 && w <= 0x46 = Just (w - 0x41 + 10)
      | w >= 0x61 && w <= 0x66 = Just (w - 0x61 + 10)
      | otherwise              = Nothing

-- ---------------------------------------------------------------------------
-- Reference resolution (RFC 3986 §5)
-- ---------------------------------------------------------------------------

-- | RFC 3986 §5.2 \"Transform References\" given a base URI and a
-- reference (parsed as a URI for convenience; only its non-empty
-- fields are consulted, mirroring the algorithm in §5.2.2).
--
-- This handles the cases an HTTP client cares about: reference
-- with its own authority (network-path), reference with an
-- absolute path, and reference with a relative path. The
-- dot-segment fixup runs on the resolved path.
resolveReference
  :: URI                -- ^ base URI (must be absolute)
  -> URI                -- ^ reference
  -> URI
resolveReference base ref =
  let refHost = authHost (uriAuthority ref)
  in if not (BS.null refHost)
       then ref { uriPath = removeDotSegments (uriPath ref) }
       else
         let path' = case uriPath ref of
               p | BS.null p -> uriPath base
                 | "/" `BS.isPrefixOf` p -> removeDotSegments p
                 | otherwise -> removeDotSegments (mergePath base p)
             query' = if BS.null (uriPath ref) && BS.null (uriQuery ref)
                        then uriQuery base
                        else uriQuery ref
         in URI
              { uriScheme        = uriScheme base
              , uriAuthority     = uriAuthority base
              , uriUserinfoBytes = uriUserinfoBytes base
              , uriPath          = path'
              , uriQuery         = query'
              , uriFragment      = uriFragment ref
              }
  where
    mergePath b p
      | BS.null (uriPath b) = "/" <> p
      | otherwise =
          let bp     = uriPath b
              prefix = BS.reverse (BS.dropWhile (/= 0x2F) (BS.reverse bp))
          in prefix <> p

