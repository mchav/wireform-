{- | Pinned linear send buffer for zero-allocation encodes.

A 'SendBuffer' is a single pinned byte buffer. Encoders write directly
into it (via @Wireform.Builder@'s 'hPutBuilderLen' over a memory sink,
or via our own pointer-walking primitives) and then a single @send()@
flushes it. No per-frame allocation, no @bytestring-builder@-style
chunk lists.

The buffer is purely a scratch area: it holds no state across requests,
so the same buffer can be reused for the response right after the
request finishes processing. This matches h2o's @h2o_buffer_t@ pattern.
-}
module Network.HTTP1.Internal.SendBuffer
  ( SendBuffer (..)
  , newSendBuffer
  , newSendBufferSized
  , withSendBuffer
  , sendBuilderAll
  ) where

import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as NBS

import qualified Wireform.Builder as B

data SendBuffer = SendBuffer
  { sbBuffer   :: !(ForeignPtr Word8)
  , sbCapacity :: !Int
  }

defaultSendCapacity :: Int
defaultSendCapacity = 65536

newSendBuffer :: IO SendBuffer
newSendBuffer = newSendBufferSized defaultSendCapacity

newSendBufferSized :: Int -> IO SendBuffer
newSendBufferSized cap = do
  fp <- BSI.mallocByteString cap
  pure SendBuffer { sbBuffer = fp, sbCapacity = cap }

-- | Run an action against the raw write pointer; the action returns the
-- number of bytes it wrote. The bytes are then handed to 'NBS.sendAll'.
withSendBuffer :: SendBuffer -> Socket -> (Ptr Word8 -> Int -> IO Int) -> IO ()
withSendBuffer SendBuffer{sbBuffer = fp, sbCapacity = cap} sock fill = do
  written <- withForeignPtr fp $ \p -> fill p cap
  if written <= 0
    then pure ()
    else do
      let bs = BSI.fromForeignPtr fp 0 written
      NBS.sendAll sock bs

-- | Materialise a 'B.Builder' into a strict 'ByteString' and send it on
-- the socket.
--
-- We pass a 4 KiB initial capacity hint to @Wireform.Builder@. A
-- typical small HTTP\/1.1 response head + body fits comfortably in
-- that, so the common case is a single pinned allocation + a single
-- @send()@ — no chunk-list, no growth, no extra copy.
sendBuilderAll :: Socket -> B.Builder -> IO ()
sendBuilderAll sock b = do
  let !bs = B.toStrictByteStringWith 4096 b
  NBS.sendAll sock bs
{-# INLINE sendBuilderAll #-}
