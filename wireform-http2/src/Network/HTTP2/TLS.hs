-- | TLS / ALPN bring-up for wireform-http2.
--
-- HTTP/2 over TLS (the @h2@ ALPN identifier from RFC 7540 §3.1) is the
-- shape every browser and gRPC client speaks. This module is the bridge
-- between the @tls@ package and wireform-http2's 'Transport': it
-- wraps a 'TLS.Context' as a 'Transport' that the existing
-- 'Network.HTTP2.Connection' machinery can drive without knowing
-- anything about TLS.
--
-- The actual client / server entry points live in
-- "Network.HTTP2.TLS.Client" and "Network.HTTP2.TLS.Server"; this
-- module just holds the pieces they share.
module Network.HTTP2.TLS
  ( -- * ALPN
    h2ProtocolId
  , ALPNFailed (..)
    -- * Transport bridge
  , tlsTransport
  ) where

import Control.Exception (Exception, SomeException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.TLS as TLS

import Network.HTTP2.Transport

-- | The ALPN protocol identifier for HTTP/2 over TLS, as registered in
-- the IANA TLS Application-Layer Protocol Negotiation registry.
h2ProtocolId :: ByteString
h2ProtocolId = "h2"

-- | Thrown when the peer refused to negotiate the @h2@ ALPN protocol.
--
-- The wrapped 'Maybe ByteString' is whatever protocol the peer did
-- pick, or 'Nothing' if no ALPN extension was negotiated at all.
newtype ALPNFailed = ALPNFailed (Maybe ByteString)
  deriving stock (Eq, Show)

instance Exception ALPNFailed

-- | Bridge a freshly-handshaked 'TLS.Context' onto wireform-http2's
-- 'Transport'. Caller is responsible for performing the handshake (and
-- verifying ALPN) before constructing the transport.
--
-- Vectored writes are flattened into a single 'TLS.sendData' call: the
-- @tls@ package re-chunks to TLS record size internally, so doing it
-- ourselves would be wasted work.
tlsTransport :: TLS.Context -> IO Transport
tlsTransport ctx =
  bufferedRecvTransport
    (\bs -> TLS.sendData ctx (LBS.fromStrict bs))
    (\bss -> TLS.sendData ctx (LBS.fromChunks bss))
    (TLS.recvData ctx)
    close
  where
    -- 'TLS.bye' may fail if the peer already dropped the connection;
    -- swallow those because the close happens during finalisation
    -- where re-throwing is unhelpful.
    close = do
      (TLS.bye ctx) `catch` swallow
      (TLS.contextClose ctx) `catch` swallow

    swallow :: SomeException -> IO ()
    swallow _ = pure ()
