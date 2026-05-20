-- | Pinned ring buffer for zero-allocation receives.
-- Uses the supplied @recvBuf@-style callback to receive directly into our
-- pinned buffer, bypassing the ByteString allocation that
-- 'Network.Socket.ByteString.recv' does.
-- Frame reads are served as zero-copy slices (fromForeignPtr) of this buffer.
module Network.HTTP2.Internal.RecvBuffer
  ( RecvBuffer
  , newRecvBuffer
  , recvBufferRead
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word
import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr
import Foreign.Ptr

foreign import ccall unsafe "string.h memmove"
  c_memmove :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

data RecvBuffer = RecvBuffer
  { rbBuffer :: !(ForeignPtr Word8)
  , rbCapacity :: !Int
  , rbReadPos :: !(IORef Int)
  , rbWritePos :: !(IORef Int)
  }

bufferCapacity :: Int
bufferCapacity = 131072

newRecvBuffer :: IO RecvBuffer
newRecvBuffer = do
  fp <- BSI.mallocByteString bufferCapacity
  rp <- newIORef 0
  wp <- newIORef 0
  pure RecvBuffer
    { rbBuffer = fp
    , rbCapacity = bufferCapacity
    , rbReadPos = rp
    , rbWritePos = wp
    }

-- | Read exactly @n@ bytes. Zero-copy when data doesn't require compaction.
--
-- The first argument is a @recvBuf@-style callback that fills the given
-- pointer with up to @n@ bytes and returns the count (0 on EOF). Both the
-- socket and the TLS transport satisfy this shape; see
-- "Network.HTTP2.Transport".
recvBufferRead :: RecvBuffer -> (Ptr Word8 -> Int -> IO Int) -> Int -> IO ByteString
recvBufferRead rb recv n = do
  rp <- readIORef (rbReadPos rb)
  wp <- readIORef (rbWritePos rb)
  let available = wp - rp
  if available >= n
    then sliceBuffer rb rp n
    else do
      ensureSpace rb rp wp
      fillUntil rb recv n

-- Zero-copy slice: return a ByteString backed by the pinned buffer.
{-# INLINE sliceBuffer #-}
sliceBuffer :: RecvBuffer -> Int -> Int -> IO ByteString
sliceBuffer rb rp n = do
  writeIORef (rbReadPos rb) (rp + n)
  pure $! BSI.fromForeignPtr (rbBuffer rb) rp n

-- Compact unread data to the front if we're past halfway.
ensureSpace :: RecvBuffer -> Int -> Int -> IO ()
ensureSpace rb rp wp = do
  let unread = wp - rp
  if rp > 0
    then do
      if unread > 0
        -- memmove (not memcpy): the source and destination regions
        -- overlap whenever @rp < unread@.  memcpy is undefined on
        -- overlapping ranges and glibc's implementation can corrupt
        -- bytes depending on alignment / size.
        then withForeignPtr (rbBuffer rb) $ \base -> do
          _ <- c_memmove base (base `plusPtr` rp) (fromIntegral unread)
          pure ()
        else pure ()
      writeIORef (rbReadPos rb) 0
      writeIORef (rbWritePos rb) unread
    else pure ()

-- Receive until we have at least n bytes available.
fillUntil :: RecvBuffer -> (Ptr Word8 -> Int -> IO Int) -> Int -> IO ByteString
fillUntil rb recv n = do
  rp <- readIORef (rbReadPos rb)
  wp <- readIORef (rbWritePos rb)
  let available = wp - rp
  if available >= n
    then sliceBuffer rb rp n
    else do
      let space = rbCapacity rb - wp
          toRecv = max (n - available) (min space 65536)
      if toRecv <= 0
        then do
          ensureSpace rb rp wp
          fillUntil rb recv n
        else do
          bytesRead <- withForeignPtr (rbBuffer rb) $ \base ->
            recv (base `plusPtr` wp) toRecv
          if bytesRead <= 0
            then
              if available > 0
                then sliceBuffer rb rp (min n available)
                else pure BS.empty
            else do
              writeIORef (rbWritePos rb) (wp + bytesRead)
              fillUntil rb recv n
