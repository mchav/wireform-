{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Network.RingTransport
Description : Bridge crypton-connection 'Network.Connection' onto a wireform ring 'Transport'
Copyright   : (c) 2026
License     : BSD-3-Clause
Maintainer  : kafka-native

The Kafka client historically reads framed responses by issuing a pair
of @connectionGetExact@ calls per frame (length prefix + body). That
costs one allocation per chunk crossed and a copy out of the kernel
buffer into a heap 'ByteString' even when the parser only needs to
peek at the correlation id.

This module bridges the existing 'NC.Connection' (which already carries
the TLS + SASL state) onto a 'Wireform.Transport': bytes flow from
@connectionGet@ straight into the magic ring's backing memory, and the
streaming parser in 'Kafka.Network.FrameParser' walks the ring with no
intermediate copies.

The bridge keeps a small holdover buffer so a short read from
@connectionGet@ doesn't desync the @recvBuf@-style callback shape that
'withRecvBufTransport' expects.
-}
module Kafka.Network.RingTransport
  ( withConnectionTransport
  , connectionRecvFn
  ) where

import Control.Exception (SomeException, throwIO, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import qualified Network.Connection as NC

import Wireform.Network
  ( RecvFn
  , Transport
  , TransportConfig
  , withRecvBufTransport
  )

-- | Run an action with a 'Transport' that receives bytes by pulling
-- chunks from the given 'NC.Connection'.  The transport does NOT
-- close the underlying connection — the caller owns its lifetime
-- (same convention as 'withRecvTransport').
--
-- The recv path uses 'connectionGet' which returns whatever chunk
-- the underlying TCP / TLS layer has ready; a short read is fine
-- because the ring buffer loops until the parser has what it needs.
withConnectionTransport
  :: TransportConfig
  -> NC.Connection
  -> (Transport -> IO a)
  -> IO a
withConnectionTransport cfg conn action = do
  recvFn <- connectionRecvFn conn
  withRecvBufTransport cfg recvFn action

-- | Adapt a 'NC.Connection' to the 'RecvFn' shape
-- ('withRecvBufTransport' expects @Ptr -> Int -> IO Int@).
--
-- 'NC.connectionGet' returns an arbitrarily-sized chunk per call; we
-- keep a small @IORef@-held holdover for the bytes that arrived in the
-- same chunk but didn't fit into the caller's buffer.  A subsequent
-- 'RecvFn' call drains the holdover first, only hitting the network
-- when the holdover is empty.
--
-- A zero-byte read from 'NC.connectionGet' is treated as EOF
-- ('withRecvBufTransport' turns that into 'EndOfInput').
--
-- == Per-recv allocation
--
-- The request to 'NC.connectionGet' is capped at 64 KiB.  Two reasons:
--
--   * For TLS connections the underlying @tls@ package will yield at
--     most one record (max 16 KiB) per call regardless of what we
--     ask for, so a bigger request doesn't get us more data.
--   * For plain TCP connections @crypton-connection@ allocates a
--     pinned buffer of the requested size /up front/ and then trims
--     to the actual byte count returned by the kernel.  A 1 MiB
--     request (the magic ring's default size) would waste 1 MiB of
--     pinned allocation per recv call, even when the kernel only
--     has a few hundred bytes ready.  64 KiB is the same cap the
--     classic 'connectionGetExact' path used in 'readFrame'.
connectionRecvFn :: NC.Connection -> IO RecvFn
connectionRecvFn conn = do
  leftover <- newIORef BS.empty
  pure $ \ptr want -> bufferedFill leftover ptr want
  where
    bufferedFill :: IORef BS.ByteString -> Ptr Word8 -> Int -> IO Int
    bufferedFill ref dst want = do
      held <- readIORef ref
      if not (BS.null held)
        then do
          let !take_  = min want (BS.length held)
              !taken  = BS.take take_ held
              !rest   = BS.drop take_ held
          writeIORef ref rest
          copyBSInto dst taken
          pure take_
        else do
          let !askFor = min recvChunkCap (max want minRecvChunk)
          chunkE <- try (NC.connectionGet conn askFor)
          case chunkE of
            Left (e :: SomeException) -> throwIO e
            Right chunk
              | BS.null chunk -> pure 0
              | otherwise -> do
                  let !take_  = min want (BS.length chunk)
                      !taken  = BS.take take_ chunk
                      !rest   = BS.drop take_ chunk
                  writeIORef ref rest
                  copyBSInto dst taken
                  pure take_

-- | Hard cap on a single 'NC.connectionGet' request.  Matches the
-- 'connectionGetExact' chunking the pre-migration pipeline used,
-- and the TLS record size limit so the underlying @tls@ package
-- never returns more than this in a single 'recvData' anyway.
recvChunkCap :: Int
recvChunkCap = 64 * 1024

-- | Floor on the recv request — if the parser only wants 4 bytes
-- (e.g. a frame length prefix) it's still worth asking the OS for
-- 'minRecvChunk' so a single Kafka response that won't ever be
-- split across recvs lands in one call.
minRecvChunk :: Int
minRecvChunk = 4 * 1024

{-# INLINE copyBSInto #-}
copyBSInto :: Ptr Word8 -> BS.ByteString -> IO ()
copyBSInto dst bs =
  let (fp, off, len) = BSI.toForeignPtr bs
  in withForeignPtr fp $ \src ->
       copyBytes dst (src `plusPtr` off) len
