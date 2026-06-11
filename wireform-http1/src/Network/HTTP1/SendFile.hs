{- | Kernel-side zero-copy file send via @sendfile(2)@.

The Linux @sendfile@ syscall pushes bytes from one file descriptor
directly to another without bouncing through user-space buffers. For
HTTP responses backed by files on disk (static-file servers, video
chunks, large pre-rendered payloads) this means:

  * No @read(2)@ + @write(2)@ pair per chunk.
  * No userspace buffer allocation at all.
  * The kernel can often DMA pages directly from the file-system
    cache to the NIC.

This is the same optimisation @nginx@'s @sendfile on@ and @h2o@'s
@file.dir@ handler use; it's the primary reason both servers post
high static-file numbers on benchmarks.

== Portability

This module currently targets Linux. The signature of @sendfile(2)@
differs on BSD / macOS (extra arguments, different semantics) and on
Windows @TransmitFile@ is the equivalent. A portability shim is a
straightforward addition but is reserved for when we actually need it.

On non-Linux systems the FFI binding will still compile (the symbol
is looked up at link time) but calls will fail at runtime. The server
falls back gracefully: 'sendBodyFile' rethrows the error, which the
server's outer 'try' catches and turns into a 500 + close.
-}
module Network.HTTP1.SendFile (
  sendFile,
  sendFileFd,
  sendMore,
) where

import Control.Exception (throwIO)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word64)
import Foreign.C.Error (Errno (..), eINTR, errnoToIOError, getErrno)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Ptr qualified
import Foreign.Storable (poke)
import Network.Socket (Socket, withFdSocket)
import System.Posix.Types (COff (..), CSsize (..), Fd (..))


{- | Linux @sendfile(2)@.

@ssize_t sendfile(int out_fd, int in_fd, off_t *offset, size_t count);@

A @safe@ ccall (not @unsafe@): @sendfile@ may block waiting for
socket buffer space and we want the GHC RTS to schedule other
Haskell threads on the same capability while we're parked.
-}
foreign import ccall safe "sys/sendfile.h sendfile"
  c_sendfile :: CInt -> CInt -> Ptr COff -> CSize -> IO CSsize


{- | Send @length@ bytes starting at @offset@ from a file descriptor
to a socket. Loops until all bytes are sent or 'EOF' is hit on the
file. Throws an IOError on any non-@EINTR@ failure.

The socket must be connected. We do not touch the socket's
@SO_LINGER@ or @TCP_CORK@ state — applications that want
header + body to land in a single TCP segment should set
@TCP_CORK@ themselves around the head + 'sendFile' pair.
-}
sendFile :: Socket -> Fd -> Word64 -> Word64 -> IO ()
sendFile sock fileFd offset0 totalLen =
  withFdSocket sock $ \sockFdInt -> do
    let sockFd = fromIntegral sockFdInt :: CInt
    sendFileFd sockFd fileFd offset0 totalLen


{- | Same as 'sendFile' but takes a raw socket file descriptor. Useful
when you already have one (e.g. from a custom event loop).
-}
sendFileFd :: CInt -> Fd -> Word64 -> Word64 -> IO ()
sendFileFd sockFd (Fd fileFdInt) = loop
  where
    -- The per-call cap matters: very large counts can saturate the
    -- send buffer and starve other connections sharing the
    -- capability. 1 MiB per call is what nginx uses for its
    -- 'sendfile_max_chunk' default.
    !chunkCap = 1024 * 1024 :: Word64

    loop !_offset 0 = pure ()
    loop !offset !remaining = alloca $ \offPtr -> do
      poke offPtr (fromIntegral offset)
      let want = min remaining chunkCap
      n <- c_sendfile sockFd (fromIntegral fileFdInt) offPtr (fromIntegral want)
      if n < 0
        then do
          err <- getErrno
          if err == eINTR
            then loop offset remaining
            else throwSendfileError err
        else
          if n == 0
            -- File ended before we expected (e.g. the user-supplied
            -- length lied). Bail; the caller is responsible for
            -- having stat'd the file size correctly.
            then pure ()
            else loop (offset + fromIntegral n) (remaining - fromIntegral n)


throwSendfileError :: Errno -> IO a
throwSendfileError err =
  throwIO $ errnoToIOError "sendfile" err Nothing Nothing
{-# NOINLINE throwSendfileError #-}


------------------------------------------------------------------------
-- send(MSG_MORE)
------------------------------------------------------------------------

{- | Linux-only @send(2)@ with the @MSG_MORE@ flag. Tells the kernel
"I have more data coming on this socket; do not transmit until the
next non-MORE send (or sendfile) flushes". Effect-equivalent to
@TCP_CORK@ but without the per-request setsockopt syscall pair.

The server uses this for the head of a sendfile response so the
head + body land in one TCP segment. Returns when all bytes have
been sent, looping on @EINTR@. Throws on any other failure.
-}
sendMore :: Socket -> BS.ByteString -> IO ()
sendMore sock bs
  | BS.null bs = pure ()
  | otherwise = withFdSocket sock $ \sockFdInt ->
      BSU.unsafeUseAsCStringLen bs $ \(p, len) ->
        loop (fromIntegral sockFdInt) (castPtr p) len
  where
    loop !sockFd !p !remaining
      | remaining <= 0 = pure ()
      | otherwise = do
          n <- c_send_more sockFd p (fromIntegral remaining)
          if n < 0
            -- Our C wrapper already loops on EINTR; any other failure
            -- is propagated as -errno.
            then throwSendfileError (Errno (fromIntegral (-n)))
            else
              let !consumed = fromIntegral n
              in loop sockFd (p `plusPtr'` consumed) (remaining - consumed)

    plusPtr' :: Ptr a -> Int -> Ptr a
    plusPtr' p n = Foreign.Ptr.plusPtr p n


foreign import ccall safe "hs_http1_send_more"
  c_send_more :: CInt -> Ptr () -> CSize -> IO CSsize
