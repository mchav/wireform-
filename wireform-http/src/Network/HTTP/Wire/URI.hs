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
module Network.HTTP.Wire.URI
  ( -- * Parsed URIs
    URI (..)
  , Scheme (..)
  , Authority (..)
  , parseURI
  , renderURI
  , uriHost
  , uriPort
  , uriPathAndQuery
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

-- | The effective host (lowercased ASCII bytes).
uriHost :: URI -> ByteString
uriHost = authHost . uriAuthority

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
      hostPort = authHost (uriAuthority u) <> case authPort (uriAuthority u) of
        Nothing -> ""
        Just p  -> ":" <> BS8.pack (show p)
      path = if BS.null (uriPath u) then "/" else uriPath u
      query = if BS.null (uriQuery u) then "" else "?" <> uriQuery u
      frag  = if BS.null (uriFragment u) then "" else "#" <> uriFragment u
  in scheme <> hostPort <> path <> query <> frag

-- | Parse an absolute URI of the form @scheme:\/\/authority\/path?query#fragment@.
--
-- Deliberately conservative: only HTTP\/HTTPS schemes, only host
-- authorities (no @user\@host@), and no percent-decoding of any
-- component. Returns 'Left' with a human-readable reason on failure.
parseURI :: ByteString -> Either String URI
parseURI bs = do
  (scheme, rest1) <- splitScheme bs
  rest2           <- stripPrefixBS "//" rest1 `orFail` "URI must be authority-form"
  let (authBs, pathQF) = BS.break (\b -> b == 0x2F || b == 0x3F || b == 0x23) rest2 -- '/' '?' '#'
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
    { uriScheme    = scheme
    , uriAuthority = auth
    , uriPath      = if BS.null pathBs then "/" else pathBs
    , uriQuery     = queryBs0
    , uriFragment  = fragBs0
    }
  where
    dropHash b = case BS.uncons b of
      Just (0x23, r) -> r
      _              -> b
    orFail Nothing msg = Left msg
    orFail (Just x) _  = Right x

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
parseAuthority bs
  | BS.null bs = Left "URI missing host authority"
  | otherwise = case BS.elemIndexEnd 0x3A bs of
      Just i | i > 0 ->
        let (h, p0) = BS.splitAt i bs
            p       = BS.drop 1 p0
        in case BS8.readInt p of
             Just (n, leftover) | BS.null leftover ->
               Right (Authority h (Just n))
             _ -> Right (Authority bs Nothing)
      _ -> Right (Authority bs Nothing)

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
  { uriScheme    = baseScheme b
  , uriAuthority = baseAuthority b
  , uriPath      = if BS.null (basePath b) then "/" else basePath b
  , uriQuery     = ""
  , uriFragment  = ""
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
       { uriScheme    = baseScheme base
       , uriAuthority = baseAuthority base
       , uriPath      = joined
       , uriQuery     = uriQuery req
       , uriFragment  = uriFragment req
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

