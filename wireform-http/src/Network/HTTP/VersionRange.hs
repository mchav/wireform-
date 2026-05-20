{- | Constraints on which HTTP versions a client or server is willing
to negotiate.

A 'VersionRange' is an ordered, non-empty list of acceptable
versions. Order is /preference/: ALPN protocols are advertised in
this order, the cleartext client picks its first entry, and the
server uses it as the tiebreaker when the peer offers several.

A small set of named ranges covers the common cases:

* 'anyVersion'   — anything we know how to speak
* 'http2Only'    — strict HTTP\/2; fail otherwise. This is the right
                   setting for gRPC, which assumes HTTP\/2 framing.
* 'http1Only'    — strict HTTP\/1.x.
* 'preferHttp2'  — try HTTP\/2 first, fall back to HTTP\/1.1.
* 'preferHttp1'  — invert that.

The result of negotiation is checked against the range; a peer that
forces us off-range raises 'VersionOutOfRange'.
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
    -- * ALPN identifiers
  , alpnH2
  , alpnHttp11
  , alpnHttp10
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

-- | Anything we know how to speak: HTTP\/2 first, then 1.1, then 1.0.
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

alpnForVersion :: Version -> Maybe ByteString
alpnForVersion v
  | v == HTTP2   = Just alpnH2
  | v == HTTP1_1 = Just alpnHttp11
  | v == HTTP1_0 = Just alpnHttp10
  | otherwise    = Nothing

versionForAlpn :: ByteString -> Maybe Version
versionForAlpn bs
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
