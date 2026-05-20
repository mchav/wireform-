{- | I\/O transport abstraction for HTTP\/1.x connections.

A 'Transport' is the small set of byte-stream operations the rest of
the HTTP\/1.x stack needs from its underlying socket / TLS context.
The default 'socketTransport' is a thin wrapper around
"Network.Socket"; "Network.HTTP1.TLS" (planned) and any other
non-socket transport can build its own.

This mirrors @Network.HTTP2.Transport@; we keep them separate
modules for now so the HTTP\/1.x stack doesn't pick up the HTTP\/2
HPACK / frame deps when a caller only wants HTTP\/1.x.
-}
module Network.HTTP1.Transport
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

-- | Bidirectional byte transport for an HTTP\/1.x connection.
--
-- All callbacks may block.  The transport is single-reader \/
-- single-writer: the recv loop is the only reader, and the send path
-- runs synchronously on the connection thread.
data Transport = Transport
  { tSendAll :: !(ByteString -> IO ())
    -- ^ Send the full payload, looping on short writes.
  , tSendMany :: !([ByteString] -> IO ())
    -- ^ Vectored send.  TLS transports fall back to concat + 'tSendAll'.
  , tRecvBuf :: !(Ptr Word8 -> Int -> IO Int)
    -- ^ Receive into the supplied buffer.  Returns the number of
    -- bytes read; @0@ means orderly EOF.
  , tClose :: !(IO ())
  , tSocket :: !(Maybe Socket)
    -- ^ The raw socket if the transport is socket-backed.  The
    -- @sendfile(2)@ fast path checks this; transports without an
    -- underlying socket (TLS, in-memory test transports) leave it
    -- 'Nothing' and the server falls back to a userspace copy.
  }

-- | Bridge a 'Socket' onto a 'Transport'.
{-# INLINE socketTransport #-}
socketTransport :: Socket -> Transport
socketTransport sock = Transport
  { tSendAll = NBS.sendAll sock
  , tSendMany = NBS.sendMany sock
  , tRecvBuf = NS.recvBuf sock
  , tClose = NS.close sock
  , tSocket = Just sock
  }

-- | Wrap a chunk-returning recv function (e.g. @tls@'s @recvData@) as
-- a 'Transport'.  A small holdover buffer bridges the chunk-returning
-- shape to the @Ptr@-filling 'tRecvBuf' that the recv ring buffer
-- expects.
bufferedRecvTransport
  :: (ByteString -> IO ())
  -> ([ByteString] -> IO ())
  -> IO ByteString  -- ^ recv next chunk (empty = EOF)
  -> IO ()          -- ^ close
  -> IO Transport
bufferedRecvTransport sendAll sendMany recvChunk close = do
  leftover <- newIORef BS.empty
  pure Transport
    { tSendAll = sendAll
    , tSendMany = sendMany
    , tRecvBuf = \ptr n -> bufferedFill leftover recvChunk ptr n
    , tClose = close
    , tSocket = Nothing
    }

-- | One step of the chunk \/ buffer bridge: copy from the leftover
-- chunk if any, then pull a single fresh chunk and copy as much as
-- fits.  Short reads are fine here — the ring buffer loops until it
-- has what it needs.
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
      let take_ = min (BS.length leftover) want
          (taken, rest) = BS.splitAt take_ leftover
      writeIORef leftoverRef rest
      let (fp, off, len) = BSI.toForeignPtr taken
      withForeignPtr fp $ \src ->
        copyBytes dst (src `plusPtr` off) len
      pure len
    else do
      chunk <- recvChunk
      if BS.null chunk
        then pure 0
        else do
          let take_ = min (BS.length chunk) want
              (taken, rest) = BS.splitAt take_ chunk
          writeIORef leftoverRef rest
          let (fp, off, len) = BSI.toForeignPtr taken
          withForeignPtr fp $ \src ->
            copyBytes dst (src `plusPtr` off) len
          pure len
