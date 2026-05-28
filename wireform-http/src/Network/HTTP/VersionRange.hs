{- | Constraints on which HTTP versions a client or server is willing
to negotiate.

A 'VersionRange' is an ordered, non-empty list of acceptable
versions. Order is /preference/: ALPN protocols are advertised in
this order, the cleartext client picks its first entry, and the
server uses it as the tiebreaker when the peer offers several.

A small set of named ranges covers the common cases:

* 'anyVersion'   — anything we know how to speak (HTTP\/1.0 — HTTP\/2;
                   HTTP\/3 requires a QUIC transport and is not yet
                   included here)
* 'http2Only'    — strict HTTP\/2; fail otherwise. This is the right
                   setting for gRPC, which assumes HTTP\/2 framing.
* 'http1Only'    — strict HTTP\/1.x.
* 'preferHttp2'  — try HTTP\/2 first, fall back to HTTP\/1.1.
* 'preferHttp1'  — invert that.
* 'http3Only'    — HTTP\/3 over QUIC only; requires a QUIC transport
                   that is not yet shipped. Useful to declare intent
                   and for testing ALPN plumbing.

The result of negotiation is checked against the range; a peer that
forces us off-range raises 'VersionOutOfRange'.

== HTTP\/3 (§4.5 audit note)

'HTTP3' is a valid 'Version' constant and 'alpnForVersion HTTP3'
returns @Just \"h3\"@ (RFC 9114 §3.2).  However, HTTP\/3 runs over
QUIC, not TLS, so including it in the ALPN list for a TLS
connection will confuse peers.  Use 'http3Only' only when a QUIC
transport is available; the TLS dispatch layer raises
'TlsNoAlpnOverlap' if the server negotiates @h3@ over TLS.
-}
module Network.HTTP.VersionRange
  ( VersionRange
  , versionRange
  , versionRangeList
  , versionAllowed
  , preferredVersion
  , versionAlpnProtocols
    -- * Named ranges
  , anyVersion
  , http2Only
  , http1Only
  , preferHttp2
  , preferHttp1
  , http2OrHttp11
  , http3Only
    -- * ALPN identifiers
  , alpnH2
  , alpnHttp11
  , alpnHttp10
  , alpnH3
  , alpnForVersion
  , versionForAlpn
    -- * Errors
  , VersionOutOfRange (..)
  ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Network.HTTP.Types.Version

-- | A non-empty ordered list of acceptable versions. The first entry
-- is the most preferred.
newtype VersionRange = VersionRange (NonEmpty Version)
  deriving stock (Eq, Show)

-- | Build a 'VersionRange'. The constructor is exported through this
-- smart-constructor so we can deduplicate the list while preserving
-- preference order.
versionRange :: NonEmpty Version -> VersionRange
versionRange = VersionRange . dedupe
  where
    dedupe (x :| xs) = x :| go [x] xs
    go _ [] = []
    go seen (y : ys)
      | y `elem` seen = go seen ys
      | otherwise     = y : go (y : seen) ys

versionRangeList :: VersionRange -> NonEmpty Version
versionRangeList (VersionRange vs) = vs

versionAllowed :: Version -> VersionRange -> Bool
versionAllowed v (VersionRange vs) = v `elem` NE.toList vs

preferredVersion :: VersionRange -> Version
preferredVersion (VersionRange (v :| _)) = v

-- | The ALPN protocol identifier list to advertise for a range, in
-- preference order. Versions without an ALPN identifier (e.g.
-- HTTP\/0.9) are dropped.
versionAlpnProtocols :: VersionRange -> [ByteString]
versionAlpnProtocols (VersionRange vs) =
  [p | v <- NE.toList vs, Just p <- [alpnForVersion v]]

-- | Anything we know how to speak over TLS: HTTP\/2 first, then
-- 1.1, then 1.0.  HTTP\/3 is omitted because it runs over QUIC, not
-- TLS; add 'http3Only' ranges only with a QUIC transport.
anyVersion :: VersionRange
anyVersion = VersionRange (HTTP2 :| [HTTP1_1, HTTP1_0])

-- | Strict HTTP\/2. The negotiation layer will refuse to fall back to
-- HTTP\/1.x. Use this when the application protocol (e.g. gRPC)
-- depends on HTTP\/2 framing.
http2Only :: VersionRange
http2Only = VersionRange (HTTP2 :| [])

-- | Strict HTTP\/1.x. Accept either 1.1 or 1.0, refuse HTTP\/2.
http1Only :: VersionRange
http1Only = VersionRange (HTTP1_1 :| [HTTP1_0])

preferHttp2 :: VersionRange
preferHttp2 = VersionRange (HTTP2 :| [HTTP1_1, HTTP1_0])

preferHttp1 :: VersionRange
preferHttp1 = VersionRange (HTTP1_1 :| [HTTP2, HTTP1_0])

-- | HTTP\/2 with HTTP\/1.1 fallback only (no HTTP\/1.0). This is what
-- modern browsers offer.
http2OrHttp11 :: VersionRange
http2OrHttp11 = VersionRange (HTTP2 :| [HTTP1_1])

-- | HTTP\/3 over QUIC only.  The ALPN token is @h3@ (RFC 9114
-- §3.2).  No wireform-http QUIC transport exists yet; this range
-- is provided so that callers can declare intent and so that ALPN
-- plumbing can be tested.  Using this range with the TLS client
-- transport will raise 'TlsHandshakeError' at runtime if the peer
-- unexpectedly negotiates @h3@ over TLS.
http3Only :: VersionRange
http3Only = VersionRange (HTTP3 :| [])

-- ALPN identifiers ----------------------------------------------------

-- | The ALPN protocol identifier for HTTP\/2 over TLS (RFC 7540 § 3.1).
alpnH2 :: ByteString
alpnH2 = "h2"

-- | The ALPN protocol identifier for HTTP\/1.1.
alpnHttp11 :: ByteString
alpnHttp11 = "http/1.1"

-- | The ALPN protocol identifier for HTTP\/1.0. Almost never used
-- on TLS in practice; included for completeness.
alpnHttp10 :: ByteString
alpnHttp10 = "http/1.0"

-- | The ALPN protocol identifier for HTTP\/3 over QUIC (RFC 9114 §3.2).
-- This token is returned by 'alpnForVersion' for 'HTTP3' and is
-- recognised by 'versionForAlpn'.  Include it in a 'VersionRange'
-- only when a QUIC transport is available; TLS connections do not
-- support HTTP\/3.
alpnH3 :: ByteString
alpnH3 = "h3"

alpnForVersion :: Version -> Maybe ByteString
alpnForVersion v
  | v == HTTP3   = Just alpnH3
  | v == HTTP2   = Just alpnH2
  | v == HTTP1_1 = Just alpnHttp11
  | v == HTTP1_0 = Just alpnHttp10
  | otherwise    = Nothing

versionForAlpn :: ByteString -> Maybe Version
versionForAlpn bs
  | bs == alpnH3     = Just HTTP3
  | bs == alpnH2     = Just HTTP2
  | bs == alpnHttp11 = Just HTTP1_1
  | bs == alpnHttp10 = Just HTTP1_0
  | otherwise        = Nothing

-- | Raised when negotiation produced a version that isn't covered by
-- the configured 'VersionRange'.
data VersionOutOfRange = VersionOutOfRange
  { vorNegotiated :: !(Maybe Version)
    -- ^ What we ended up with (or 'Nothing' if negotiation failed
    -- entirely, e.g. ALPN returned no overlap).
  , vorAllowed    :: !VersionRange
  }
  deriving stock (Eq, Show)

instance Exception VersionOutOfRange
