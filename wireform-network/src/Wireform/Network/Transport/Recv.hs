{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}

-- | Cross-platform recv-based transport.
--
-- Single-threaded design: the parser thread itself calls @recv@.
-- @threadWaitRead@ parks on the GHC IO manager (epoll\/kqueue\/IOCP)
-- without burning a thread.
module Wireform.Network.Transport.Recv
  ( withRecvTransport
  ) where

import Control.Exception (SomeException, try)
import Data.Bits ((.&.))
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Network.Socket (Socket)
import Network.Socket.ByteString (recv)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

import Wireform.Ring.Internal
import Wireform.Transport
import Wireform.Transport.Config

------------------------------------------------------------------------
-- Transport state
------------------------------------------------------------------------

data RecvState
  = RecvOpen
  | RecvClosedEof
  | RecvClosedErr !SomeException

------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------

-- | Create a recv-based transport for the given socket.
-- Reads from the socket into a magic ring buffer.
-- On exit (normal or exceptional), the ring is destroyed.
-- The socket is NOT closed — the caller owns it.
withRecvTransport :: TransportConfig -> Socket -> (Transport -> IO a) -> IO a
withRecvTransport cfg sock action =
  withMagicRing (ringSizeHint cfg) \ring -> do
    let !base = ringBase ring
        !msk  = ringMask ring
        !sz   = ringSize ring

    headRef  <- newIORef (0 :: Word64)
    tailRef  <- newIORef (0 :: Word64)
    stateRef <- newIORef RecvOpen

    let loadHead = readIORef headRef

        advanceTail pos = writeIORef tailRef pos

        waitData pos = do
          st <- readIORef stateRef
          case st of
            RecvClosedEof   -> pure EndOfInput
            RecvClosedErr e -> pure (TransportError e)
            RecvOpen        -> doRecv pos

        doRecv pos = do
          h <- readIORef headRef
          if h > pos
            then pure (MoreData h)
            else do
              t <- readIORef tailRef
              let !writeOff  = fromIntegral h .&. msk
                  !writePtr  = base `plusPtr` writeOff
                  !available = sz - fromIntegral (h - t)
                  !maxRecv   = min available (sz - writeOff)
              if maxRecv <= 0
                then pure (MoreData h)
                else do
                  result <- try (recvBufSock sock writePtr maxRecv)
                  case result of
                    Left (exc :: SomeException) -> do
                      writeIORef stateRef (RecvClosedErr exc)
                      pure (TransportError exc)
                    Right n
                      | n == 0 -> do
                          writeIORef stateRef RecvClosedEof
                          pure EndOfInput
                      | otherwise -> do
                          let !newHead = h + fromIntegral n
                          writeIORef headRef newHead
                          pure (MoreData newHead)

        transport = Transport
          { transportRing        = ring
          , transportLoadHead    = loadHead
          , transportAdvanceTail = advanceTail
          , transportWaitData    = waitData
          , transportClose       = writeIORef stateRef RecvClosedEof
          }

    action transport

------------------------------------------------------------------------
-- Low-level recv into a buffer
------------------------------------------------------------------------

recvBufSock :: Socket -> Ptr Word8 -> Int -> IO Int
recvBufSock sock _ptr maxLen = do
  bs <- recv sock (fromIntegral maxLen)
  let !n = BS.length bs
  if n == 0
    then pure 0
    else do
      BSI.unsafeUseAsCStringLen bs \(src, len) ->
        BSI.memcpy _ptr (castPtr src) len
      pure n
