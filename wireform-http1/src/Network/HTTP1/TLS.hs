{- | TLS bring-up for wireform-http1.

HTTP\/1.1 over TLS is the @http\/1.1@ ALPN identifier (RFC 7301).
This module is the bridge between OpenSSL and the wireform-http1
'Connection': a freshly-handshaked 'SslConn' becomes a magic-ring
'Connection' via 'newConnectionFromTls', with this module supplying
the ALPN constants and assertion helper.

The transport layer (one receive ring + one send ring, both pinned
to the same OpenSSL @SSL*@) lives entirely in
"Wireform.Network.TLS.OpenSSL" + the wireform-network duplex
machinery; nothing TLS-specific needs to live here beyond the ALPN
plumbing.
-}
module Network.HTTP1.TLS
  ( -- * ALPN
    http11ProtocolId
  , ALPNFailed (..)
  , assertHttp11Alpn
  ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)

import Wireform.Network.TLS.OpenSSL (SslConn, getAlpn)

-- | The ALPN protocol identifier for HTTP\/1.1 over TLS, as
-- registered in the IANA TLS Application-Layer Protocol Negotiation
-- registry (RFC 7301 § 6).
http11ProtocolId :: ByteString
http11ProtocolId = "http/1.1"

-- | Thrown when the peer refused to negotiate the @http\/1.1@ ALPN
-- protocol.  The wrapped 'Maybe ByteString' is whatever protocol
-- the peer did pick, or 'Nothing' if no ALPN extension was
-- negotiated at all.
newtype ALPNFailed = ALPNFailed (Maybe ByteString)
  deriving stock (Eq, Show)

instance Exception ALPNFailed

-- | After a handshake, assert that the negotiated ALPN protocol
-- is @http\/1.1@ (or no ALPN at all, which is treated as a fall
-- through to HTTP\/1.1 by long-standing convention).  Throws
-- 'ALPNFailed' otherwise.
assertHttp11Alpn :: SslConn -> IO ()
assertHttp11Alpn conn = do
  m <- getAlpn conn
  case m of
    Nothing                       -> pure ()
    Just p | p == http11ProtocolId -> pure ()
           | otherwise             -> throwIO (ALPNFailed (Just p))
