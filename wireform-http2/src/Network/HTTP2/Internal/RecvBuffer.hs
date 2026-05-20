-- | Pinned ring buffer for zero-allocation receives.
-- Uses Network.Socket.recvBuf to receive directly into our pinned buffer,
-- bypassing the ByteString allocation that Network.Socket.ByteString.recv does.
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
import Foreign.ForeignPtr
import Foreign.Ptr
import Network.Socket (Socket, recvBuf)

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

-- | Read exactly n bytes. Zero-copy when data doesn't require compaction.
recvBufferRead :: RecvBuffer -> Socket -> Int -> IO ByteString
recvBufferRead rb sock n = do
  rp <- readIORef (rbReadPos rb)
  wp <- readIORef (rbWritePos rb)
  let available = wp - rp
  if available >= n
    then sliceBuffer rb rp n
    else do
      ensureSpace rb rp wp
      fillUntil rb sock n

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
        then withForeignPtr (rbBuffer rb) $ \base ->
          BSI.memcpy base (base `plusPtr` rp) unread
        else pure ()
      writeIORef (rbReadPos rb) 0
      writeIORef (rbWritePos rb) unread
    else pure ()

-- Receive until we have at least n bytes available.
fillUntil :: RecvBuffer -> Socket -> Int -> IO ByteString
fillUntil rb sock n = do
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
          -- Buffer full, compact and retry
          ensureSpace rb rp wp
          fillUntil rb sock n
        else do
          -- Receive directly into pinned buffer — no allocation!
          bytesRead <- withForeignPtr (rbBuffer rb) $ \base ->
            recvBuf sock (base `plusPtr` wp) toRecv
          if bytesRead <= 0
            then
              if available > 0
                then sliceBuffer rb rp (min n available)
                else pure BS.empty
            else do
              writeIORef (rbWritePos rb) (wp + bytesRead)
              fillUntil rb sock n
