-- | I/O transport abstraction for HTTP/2 connections.
--
-- HTTP/2 doesn't care whether the underlying byte stream is a raw TCP socket
-- or a TLS-wrapped connection; the protocol is the same either way. This
-- module captures that with a small 'Transport' record of send / receive
-- callbacks, decoupling 'Network.HTTP2.Connection' from
-- 'Network.Socket'.
--
-- The default 'socketTransport' uses pinned-buffer @recvBuf@ + scatter-gather
-- @sendMany@ for zero-copy operation; the TLS transport in
-- "Network.HTTP2.TLS.Client" / "Network.HTTP2.TLS.Server" is built the same
-- way on top of @tls@'s 'recvData' / 'sendData', with a small chunk buffer
-- to bridge tls's BS-returning recv to our Ptr-filling recv.
module Network.HTTP2.Transport
  ( Transport (..)
  , socketTransport
  , bufferedRecvTransport
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS

-- | An abstract bidirectional byte transport for an HTTP/2 connection.
--
-- All callbacks may block. The transport is not assumed to be
-- thread-safe: 'Network.HTTP2.Connection' protects concurrent sends with
-- its own send lock, and the read path is single-threaded by construction.
data Transport = Transport
  { tSendAll :: !(ByteString -> IO ())
    -- ^ Send the full payload, looping internally on short writes.
  , tSendMany :: !([ByteString] -> IO ())
    -- ^ Vectored send. For transports that don't support writev (e.g. TLS),
    -- this typically just concatenates and calls 'tSendAll'.
  , tRecvBuf :: !(Ptr Word8 -> Int -> IO Int)
    -- ^ Receive up to @n@ bytes directly into the supplied buffer.
    -- Returns the number of bytes read; @0@ indicates orderly EOF.
  , tClose :: !(IO ())
    -- ^ Close the underlying transport.
  }

-- | Build a 'Transport' that talks directly to a plain TCP socket.
{-# INLINE socketTransport #-}
socketTransport :: Socket -> Transport
socketTransport sock = Transport
  { tSendAll = NBS.sendAll sock
  , tSendMany = NBS.sendMany sock
  , tRecvBuf = NS.recvBuf sock
  , tClose = NS.close sock
  }

-- | Wrap a chunk-returning recv function (e.g. @tls@'s @recvData@) as a
-- 'Transport' suitable for HTTP/2.
--
-- This is the bridge between "recv returns whatever chunk it has" and
-- "fill exactly N bytes of this Ptr". A small holdover buffer caches
-- bytes that arrived in the same chunk but were not consumed by a
-- previous read.
bufferedRecvTransport
  :: (ByteString -> IO ())
  -- ^ Send all.
  -> ([ByteString] -> IO ())
  -- ^ Send many; pass @sendAll . BS.concat@ if the underlying
  -- transport doesn't support vectored writes.
  -> IO ByteString
  -- ^ Receive the next chunk from the wire. Empty BS = EOF.
  -> IO ()
  -- ^ Close.
  -> IO Transport
bufferedRecvTransport sendAll sendMany recvChunk close = do
  leftover <- newIORef BS.empty
  pure Transport
    { tSendAll = sendAll
    , tSendMany = sendMany
    , tRecvBuf = \ptr n -> bufferedFill leftover recvChunk ptr n
    , tClose = close
    }

-- | Copy from the leftover chunk (if any), then pull a single fresh
-- chunk from the underlying stream and copy as much of it as fits.
--
-- Returning a short read here is fine: the magic-ring transport
-- ('Wireform.Network.newRecvBufTransport') that 'tRecvBuf' feeds
-- into loops until it has what the parser needs.
bufferedFill
  :: IORef ByteString
  -> IO ByteString
  -> Ptr Word8
  -> Int
  -> IO Int
bufferedFill leftoverRef recvChunk dst want = do
  leftover <- readIORef leftoverRef
  if not (BS.null leftover)
    then do
      let take' = min want (BS.length leftover)
          (consumed, rest) = BS.splitAt take' leftover
      writeIORef leftoverRef rest
      copyByteStringInto dst consumed
      pure take'
    else do
      chunk <- recvChunk
      if BS.null chunk
        then pure 0
        else do
          let take' = min want (BS.length chunk)
              (consumed, rest) = BS.splitAt take' chunk
          writeIORef leftoverRef rest
          copyByteStringInto dst consumed
          pure take'

{-# INLINE copyByteStringInto #-}
copyByteStringInto :: Ptr Word8 -> ByteString -> IO ()
copyByteStringInto dst bs =
  let (fp, off, len) = BSI.toForeignPtr bs
  in withForeignPtr fp $ \src ->
       copyBytes dst (src `plusPtr` off) len
