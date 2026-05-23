{- | TLS bring-up for wireform-http1.

HTTP\/1.1 over TLS is the @http\/1.1@ ALPN identifier (RFC 7301).
This module is the bridge between the @tls@ package and wireform-http1's
'Transport': it wraps a 'TLS.Context' as a 'Transport' that
'Network.HTTP1.Connection.newConnectionFromTransport' can drive
without knowing anything about TLS.

Bytes flow from the TLS context's 'TLS.recvData' through
'bufferedRecvTransport' (small holdover buffer + memcpy) into the
'tRecvBuf' callback, then into the magic-ring transport that the
connection layer manages.  The extra memcpy at the bridge is
unavoidable because the @tls@ package only exposes its decrypted
plaintext as 'ByteString' chunks; on a real CPU this is in the
sub-microsecond regime (~16 KiB per TLS record) and is dwarfed by
the AES / GCM crypto cost on the same call site.

Vectored writes are flattened into a single 'TLS.sendData' call:
the @tls@ package re-chunks to TLS record size internally, so doing
it ourselves would be wasted work.

Mirrors 'Network.HTTP2.TLS.tlsTransport' module-for-module.
-}
module Network.HTTP1.TLS
  ( -- * ALPN
    http11ProtocolId
  , ALPNFailed (..)
    -- * Transport bridge
  , tlsTransport
  ) where

import Control.Exception (Exception, SomeException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.TLS as TLS

import Network.HTTP1.Transport

-- | The ALPN protocol identifier for HTTP\/1.1 over TLS, as
-- registered in the IANA TLS Application-Layer Protocol Negotiation
-- registry (RFC 7301 § 6).
http11ProtocolId :: ByteString
http11ProtocolId = "http/1.1"

-- | Thrown when the peer refused to negotiate the @http\/1.1@ ALPN
-- protocol.  The wrapped 'Maybe ByteString' is whatever protocol the
-- peer did pick, or 'Nothing' if no ALPN extension was negotiated
-- at all.
newtype ALPNFailed = ALPNFailed (Maybe ByteString)
  deriving stock (Eq, Show)

instance Exception ALPNFailed

-- | Bridge a freshly-handshaked 'TLS.Context' onto wireform-http1's
-- 'Transport'.  Caller is responsible for performing the handshake
-- (and verifying ALPN, if used) before constructing the transport.
--
-- The returned transport's 'tClose' issues a graceful TLS @bye@
-- before tearing down the context; both calls swallow exceptions
-- from already-closed peers because close happens during
-- finalisation where re-throwing is unhelpful.
tlsTransport :: TLS.Context -> IO Transport
tlsTransport ctx =
  bufferedRecvTransport
    (\bs -> TLS.sendData ctx (LBS.fromStrict bs))
    (\bss -> TLS.sendData ctx (LBS.fromChunks bss))
    (TLS.recvData ctx)
    close
  where
    close = do
      TLS.bye ctx `catch` swallow
      TLS.contextClose ctx `catch` swallow

    swallow :: SomeException -> IO ()
    swallow _ = pure ()
