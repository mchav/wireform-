{- | HTTP header fields.

A condensed take on hermes' @Network.HTTP.Headers@: we keep the
'CI ByteString' name type (case-insensitive matching with byte-level
storage) but represent the field set as a flat association list
rather than hermes' 'HashMap'. The list shape is what
'Network.HTTP1.Types.Headers' and the HPACK header lists in
'Network.HTTP2' already use, so the conversion between the unified
API and either underlying implementation is a no-op.

The @h*@ constants in this module cover the IANA permanent header
field names; the list isn't exhaustive (hermes has every IANA
registration), just the ones the wireform HTTP stack needs to
inspect at the negotiation layer. Add more as they come up.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Types.Header
  ( HeaderName
  , HeaderValue
  , Header
  , Headers
    -- * Lookups
  , lookupHeader
  , lookupHeaders
  , hasHeader
  , insertHeader
  , addHeader
  , deleteHeader
    -- * Common names
  , hHost
  , hContentLength
  , hContentType
  , hTransferEncoding
  , hConnection
  , hUpgrade
  , hHTTP2Settings
  , hServer
  , hUserAgent
  , hAccept
  , hAcceptEncoding
  , hLocation
  , hAuthorization
  , hCookie
  , hSetCookie
  , hTE
  , hTrailer
  , hDate
  ) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI, mk)

-- | Header field name. Comparison and hashing are case-insensitive.
type HeaderName = CI ByteString

-- | Header field value, on-the-wire bytes.
type HeaderValue = ByteString

type Header = (HeaderName, HeaderValue)

type Headers = [Header]

-- | First-match lookup. Returns the field value if present, 'Nothing'
-- otherwise.
lookupHeader :: HeaderName -> Headers -> Maybe HeaderValue
lookupHeader name = go
  where
    go [] = Nothing
    go ((k, v) : rest)
      | k == name = Just v
      | otherwise = go rest

-- | All values for a given header name, in original order. Useful for
-- multi-valued fields such as @Set-Cookie@ or @Via@.
lookupHeaders :: HeaderName -> Headers -> [HeaderValue]
lookupHeaders name = foldr step []
  where
    step (k, v) acc
      | k == name = v : acc
      | otherwise = acc

hasHeader :: HeaderName -> Headers -> Bool
hasHeader name = any ((== name) . fst)

-- | Replace every existing entry of @name@ with the new value. If
-- @name@ wasn't present, append it.
insertHeader :: HeaderName -> HeaderValue -> Headers -> Headers
insertHeader name value = go False
  where
    go seen [] = if seen then [] else [(name, value)]
    go seen ((k, v) : rest)
      | k == name =
          if seen
            then go True rest
            else (k, value) : go True rest
      | otherwise = (k, v) : go seen rest

-- | Append a new value without disturbing any existing entry of the
-- same name. Use this for multi-valued fields.
addHeader :: HeaderName -> HeaderValue -> Headers -> Headers
addHeader name value hs = hs <> [(name, value)]

deleteHeader :: HeaderName -> Headers -> Headers
deleteHeader name = filter ((/= name) . fst)

hHost, hContentLength, hContentType, hTransferEncoding, hConnection,
  hUpgrade, hHTTP2Settings, hServer, hUserAgent, hAccept, hAcceptEncoding,
  hLocation, hAuthorization, hCookie, hSetCookie, hTE, hTrailer, hDate
  :: HeaderName
hHost             = mk "Host"
hContentLength    = mk "Content-Length"
hContentType      = mk "Content-Type"
hTransferEncoding = mk "Transfer-Encoding"
hConnection       = mk "Connection"
hUpgrade          = mk "Upgrade"
hHTTP2Settings    = mk "HTTP2-Settings"
hServer           = mk "Server"
hUserAgent        = mk "User-Agent"
hAccept           = mk "Accept"
hAcceptEncoding   = mk "Accept-Encoding"
hLocation         = mk "Location"
hAuthorization    = mk "Authorization"
hCookie           = mk "Cookie"
hSetCookie        = mk "Set-Cookie"
hTE               = mk "TE"
hTrailer          = mk "Trailer"
hDate             = mk "Date"
