-- | I/O transport abstraction for HTTP/2 connections.
--
-- HTTP/2 doesn't care whether the underlying byte stream is a raw TCP socket
-- or a TLS-wrapped connection; the protocol is the same either way. This
-- module captures that with a small 'Transport' record of send / receive
-- callbacks, decoupling 'Network.HTTP2.Connection' from
-- 'Network.Socket'.
--
-- The send side carries a raw pointer-based 'SendFn' rather than a
-- 'ByteString'-based callback: the connection layer builds a
-- 'Wireform.Transport.Send.SendTransport' (magic ring) on top of it in
-- 'Network.HTTP2.Connection.mkConnection', so all frame data flows
-- through the ring and is drained to the wire via this callback.
--
-- The default 'socketTransport' uses 'Network.Socket.sendBuf' /
-- 'Network.Socket.recvBuf' for zero-copy operation; the TLS transport
-- in "Network.HTTP2.TLS" is built the same way on top of OpenSSL's
-- @SSL_write_ex@ / @SSL_read_ex@.
module Network.HTTP2.Transport
  ( Transport (..)
  , SendFn
  , socketTransport
  ) where

import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Network.Socket (Socket)
import qualified Network.Socket as NS

-- | A primitive @send()@-style callback: write up to @n@ bytes
-- starting at the supplied pointer, return the number of bytes
-- actually written (must be > 0 on success).
type SendFn = Ptr Word8 -> Int -> IO Int

-- | An abstract bidirectional byte transport for an HTTP/2 connection.
--
-- All callbacks may block. The transport is not assumed to be
-- thread-safe: 'Network.HTTP2.Connection' protects concurrent sends with
-- its own send lock, and the read path is single-threaded by construction.
data Transport = Transport
  { tSendFn :: !SendFn
    -- ^ Raw pointer-based send callback (SendFn from wireform-network).
    -- The inline send transport in mkConnection drains via this.
  , tRecvBuf :: !(Ptr Word8 -> Int -> IO Int)
    -- ^ Receive up to @n@ bytes directly into the supplied buffer.
    -- Returns the number of bytes read; @0@ indicates orderly EOF.
  , tShutdownWrite :: !(IO ())
    -- ^ Half-close the write side (e.g. shutdown(SHUT_WR) or TLS close_notify).
  , tClose :: !(IO ())
    -- ^ Close the underlying transport.
  }

-- | Build a 'Transport' that talks directly to a plain TCP socket.
{-# INLINE socketTransport #-}
socketTransport :: Socket -> Transport
socketTransport sock = Transport
  { tSendFn = NS.sendBuf sock
  , tRecvBuf = NS.recvBuf sock
  , tShutdownWrite = NS.shutdown sock NS.ShutdownSend
  , tClose = NS.close sock
  }
