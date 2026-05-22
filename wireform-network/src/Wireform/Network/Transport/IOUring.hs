{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

-- | Linux io_uring-based transport.
--
-- Uses io_uring for async recv directly into the magic ring buffer.
-- Integrates with the GHC IO manager via eventfd.
--
-- __Requires__: Linux kernel 5.1+, liburing-dev.
-- Build with the @iouring@ flag.
module Wireform.Network.Transport.IOUring
  (
#if defined(HAVE_IOURING)
    withIOUringTransport
#endif
  ) where

#if defined(HAVE_IOURING)

import Control.Concurrent (threadWaitRead)
import Control.Exception (SomeException, bracket, try)
import Data.Bits ((.&.))
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, plusPtr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Network.Socket (Socket, fdSocket)
import System.Posix.Types (Fd (..))

import Wireform.Ring.Internal
import Wireform.Transport
import Wireform.Transport.Config

------------------------------------------------------------------------
-- C FFI bindings
------------------------------------------------------------------------

-- Opaque io_uring state (allocated on C heap)
data IOUringState

foreign import ccall unsafe "hs_iouring_create"
  c_iouring_create :: CInt -> CInt -> CInt
                   -> Ptr Word8 -> CSize
                   -> Ptr IOUringState -> IO CInt

foreign import ccall unsafe "hs_iouring_submit_recv"
  c_iouring_submit_recv :: Ptr IOUringState -> CSize -> IO CInt

foreign import ccall unsafe "hs_iouring_wait_cqe"
  c_iouring_wait_cqe :: Ptr IOUringState -> Ptr Word64 -> IO CInt

foreign import ccall unsafe "hs_iouring_peek_cqe"
  c_iouring_peek_cqe :: Ptr IOUringState -> Ptr Word64 -> IO CInt

foreign import ccall unsafe "hs_iouring_get_eventfd"
  c_iouring_get_eventfd :: Ptr IOUringState -> IO CInt

foreign import ccall unsafe "hs_iouring_get_head"
  c_iouring_get_head :: Ptr IOUringState -> IO Word64

foreign import ccall unsafe "hs_iouring_destroy"
  c_iouring_destroy :: Ptr IOUringState -> IO ()

------------------------------------------------------------------------
-- Transport implementation
------------------------------------------------------------------------

-- | Create an io_uring-based transport.
--
-- Uses a single io_uring instance with configurable queue depth.
-- Each recv completion writes data directly into the magic ring.
-- The parser thread parks on the eventfd via @threadWaitRead@ when
-- waiting for completions.
withIOUringTransport :: TransportConfig -> Socket -> (Transport -> IO a) -> IO a
withIOUringTransport cfg sock action =
  withMagicRing (ringSizeHint cfg) \ring -> do
    let !base = ringBase ring
        !sz   = ringSize ring
        !msk  = ringMask ring
        !depth = ioUringQueueDepth (ioUring cfg)
        !sqpollIdle = case ioUringSQPoll (ioUring cfg) of
              NoSQPoll -> 0
              SQPollWithIdle ms -> ms

    sockFd <- fdSocket sock

    -- Allocate the C-side io_uring state
    let stateSize = 1024  -- generous; struct hs_iouring is ~300 bytes
    bracket (mallocBytes stateSize) free \statePtr -> do
      rc <- c_iouring_create (fromIntegral sockFd)
                             (fromIntegral depth)
                             (fromIntegral sqpollIdle)
                             base (fromIntegral sz)
                             (castPtr statePtr)
      if rc < 0
        then error ("io_uring_create failed: " <> show rc)
        else do
          let uringPtr = castPtr statePtr :: Ptr IOUringState

          eventFd <- c_iouring_get_eventfd uringPtr
          stateRef <- newIORef TSOpen
          tailRef  <- newIORef (0 :: Word64)

          -- Submit initial recv
          _ <- c_iouring_submit_recv uringPtr (fromIntegral sz)

          let loadHead = c_iouring_get_head uringPtr

              advanceTail pos = writeIORef tailRef pos

              waitData pos = do
                st <- readIORef stateRef
                case st of
                  TSClosedEof   -> pure EndOfInput
                  TSClosedErr e -> pure (TransportError e)
                  TSOpen        -> doWait pos

              doWait pos = do
                h <- loadHead
                if h > pos
                  then pure (MoreData h)
                  else do
                    -- Try non-blocking peek first
                    headPtr <- mallocBytes 8
                    peekRc <- c_iouring_peek_cqe uringPtr headPtr
                    if peekRc >= 0
                      then do
                        newH <- peek headPtr
                        free headPtr
                        if peekRc == 0
                          then do
                            writeIORef stateRef TSClosedEof
                            pure EndOfInput
                          else do
                            -- Submit replacement recv
                            t <- readIORef tailRef
                            let available = fromIntegral sz - fromIntegral (newH - t)
                            _ <- c_iouring_submit_recv uringPtr (fromIntegral available)
                            pure (MoreData newH)
                      else do
                        free headPtr
                        -- Park on eventfd
                        threadWaitRead (Fd eventFd)
                        -- Drain eventfd
                        headPtr2 <- mallocBytes 8
                        waitRc <- c_iouring_wait_cqe uringPtr headPtr2
                        if waitRc > 0
                          then do
                            newH <- peek headPtr2
                            free headPtr2
                            t <- readIORef tailRef
                            let available = fromIntegral sz - fromIntegral (newH - t)
                            _ <- c_iouring_submit_recv uringPtr (fromIntegral available)
                            pure (MoreData newH)
                          else if waitRc == 0
                            then do
                              free headPtr2
                              writeIORef stateRef TSClosedEof
                              pure EndOfInput
                            else do
                              free headPtr2
                              let exc = userError ("io_uring CQE error: " <> show waitRc)
                              writeIORef stateRef (TSClosedErr (toException exc))
                              pure (TransportError (toException exc))

              transport = Transport
                { transportRing        = ring
                , transportLoadHead    = loadHead
                , transportAdvanceTail = advanceTail
                , transportWaitData    = waitData
                , transportClose       = do
                    writeIORef stateRef TSClosedEof
                    c_iouring_destroy uringPtr
                }

          action transport `finally` c_iouring_destroy uringPtr
  where
    finally a cleanup = do
      r <- try @SomeException a
      cleanup
      case r of
        Left e  -> throwIO e
        Right v -> pure v
    toException = toException
    throwIO = Control.Exception.throwIO

data TransportState
  = TSOpen
  | TSClosedEof
  | TSClosedErr !SomeException

#endif
