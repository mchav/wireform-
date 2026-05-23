-- | TLS / ALPN bring-up for wireform-http2 (new stack).
--
-- HTTP\/2 over TLS (the @h2@ ALPN identifier from RFC 7540 §3.1) is
-- the shape every browser and gRPC client speaks.  This module
-- bridges OpenSSL — accessed through "Wireform.Network.TLS.OpenSSL"
-- — to wireform-http2's local 'Transport' record so the existing
-- 'Network.HTTP2.Connection' machinery can drive a TLS session
-- without knowing anything about TLS.
--
-- The actual client \/ server entry points live in
-- "Network.HTTP2.TLS.Client" and "Network.HTTP2.TLS.Server"; this
-- module just holds the pieces they share (the ALPN constant + the
-- 'SslConn'-to-'Transport' bridge).
--
-- The vendored grapesy engine in @Network.HTTP2.Engine.*@ keeps its
-- own 'tls'-backed TLS bridge ("Network.HTTP2.Engine.TLS.*"); the
-- two paths are independent.
module Network.HTTP2.TLS
  ( -- * ALPN
    h2ProtocolId
  , ALPNFailed (..)
  , assertH2Alpn
    -- * Transport bridge
  , tlsTransport
  ) where

import Control.Exception (Exception, throwIO)
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr (castPtr, plusPtr)

import Wireform.Network.TLS.OpenSSL
  ( SslConn
  , freeConn
  , getAlpn
  , tlsReceiveFn
  , tlsSendFn
  )

import Network.HTTP2.Transport

-- | The ALPN protocol identifier for HTTP\/2 over TLS, as
-- registered in the IANA TLS Application-Layer Protocol Negotiation
-- registry.
h2ProtocolId :: ByteString
h2ProtocolId = "h2"

-- | Thrown when the peer refused to negotiate the @h2@ ALPN
-- protocol.
newtype ALPNFailed = ALPNFailed (Maybe ByteString)
  deriving stock (Eq, Show)

instance Exception ALPNFailed

-- | After a handshake, throw 'ALPNFailed' unless the peer selected
-- @h2@.
assertH2Alpn :: SslConn -> IO ()
assertH2Alpn conn = do
  m <- getAlpn conn
  case m of
    Just p | p == h2ProtocolId -> pure ()
    other                       -> throwIO (ALPNFailed other)

-- | Bridge a freshly-handshaked 'SslConn' onto wireform-http2's
-- local 'Transport' record.  Caller is responsible for the
-- handshake (and ALPN assertion via 'assertH2Alpn').
--
-- The send path uses OpenSSL's @SSL_write_ex@ directly (no
-- intermediate 'ByteString' copy beyond the slice we're handed);
-- the recv path uses 'SSL_read_ex' to write plaintext straight
-- into the caller-supplied buffer.  This is the OpenSSL analogue
-- of what the vendored Engine does with the pure-Haskell @tls@
-- package.
tlsTransport :: SslConn -> IO Transport
tlsTransport conn = pure Transport
  { tSendAll  = sendAll
  , tSendMany = sendMany
  , tRecvBuf  = recvBuf
  , tClose    = (freeConn conn) `E.catch` (\(_ :: E.SomeException) -> pure ())
  }
  where
    sendFn = tlsSendFn conn
    recvFn = tlsReceiveFn conn

    sendAll bs
      | BS.null bs = pure ()
      | otherwise  = BSU.unsafeUseAsCStringLen bs $ \(src, len) ->
          drain (castPtr src) len
      where
        drain _ 0 = pure ()
        drain p n = do
          k <- sendFn p n
          if k <= 0
            then ioError (userError "tlsTransport: SSL_write_ex returned 0")
            else drain (p `plusPtr` k) (n - k)

    sendMany [] = pure ()
    sendMany xs = mapM_ sendAll xs
    -- ^ OpenSSL doesn't expose a vector-IO write; the underlying
    -- TLS record framing forces one record per call anyway, so
    -- writev would offer no syscall saving.  Mirrors the @tls@-
    -- backed bridge in spirit.

    recvBuf = recvFn
