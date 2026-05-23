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
          chunkE <- try (NC.connectionGet conn (max want 4096))
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

{-# INLINE copyBSInto #-}
copyBSInto :: Ptr Word8 -> BS.ByteString -> IO ()
copyBSInto dst bs =
  let (fp, off, len) = BSI.toForeignPtr bs
  in withForeignPtr fp $ \src ->
       copyBytes dst (src `plusPtr` off) len
