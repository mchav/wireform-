{- | Pinned linear send buffer for zero-allocation encodes.

A 'SendBuffer' is a single pinned byte buffer. Encoders write directly
into it (via @Wireform.Builder@'s 'hPutBuilderLen' over a memory sink,
or via our own pointer-walking primitives) and then a single send
flushes it. No per-frame allocation, no @bytestring-builder@-style
chunk lists.

The buffer is purely a scratch area: it holds no state across requests,
so the same buffer can be reused for the response right after the
request finishes processing. This matches h2o's @h2o_buffer_t@ pattern.

The actual byte-pushing primitive (@send()@, TLS write, in-memory
test sink, …) is supplied by the caller as a @ByteString -> IO ()@
callback; the buffer doesn't know about sockets.
-}
module Network.HTTP1.Internal.SendBuffer
  ( SendBuffer (..)
  , newSendBuffer
  , newSendBufferSized
  , withSendBuffer
  , sendBuilderAll
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)

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
-- number of bytes it wrote. The bytes are then handed to the supplied
-- @sendAll@ callback.
withSendBuffer
  :: SendBuffer
  -> (ByteString -> IO ())
  -- ^ @sendAll@ — push all the bytes downstream.
  -> (Ptr Word8 -> Int -> IO Int)
  -- ^ Encoder fill.
  -> IO ()
withSendBuffer SendBuffer{sbBuffer = fp, sbCapacity = cap} sendAll fill = do
  written <- withForeignPtr fp $ \p -> fill p cap
  if written <= 0
    then pure ()
    else sendAll (BSI.fromForeignPtr fp 0 written)

-- | Materialise a 'B.Builder' into a strict 'ByteString' and push it
-- to the supplied @sendAll@ callback.
--
-- A 4 KiB initial capacity hint is passed to @Wireform.Builder@: a
-- typical small HTTP\/1.1 response head + body fits comfortably in
-- that, so the common case is a single pinned allocation + a single
-- send — no chunk-list, no growth, no extra copy.
sendBuilderAll :: (ByteString -> IO ()) -> B.Builder -> IO ()
sendBuilderAll sendAll b = do
  let !bs = B.toStrictByteStringWith 4096 b
  sendAll bs
{-# INLINE sendBuilderAll #-}
