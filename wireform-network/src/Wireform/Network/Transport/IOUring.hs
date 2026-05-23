{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Linux io_uring-based transport.
--
-- Receives data directly into the magic ring buffer via io_uring
-- async recv.  Integrates with the GHC IO manager via eventfd.
--
-- __Requires__: Linux kernel 5.1+, liburing-dev.
-- Build with @cabal build -f iouring@.
module Wireform.Network.Transport.IOUring
  (
#if defined(HAVE_IOURING)
    withIOUringTransport
#endif
  ) where

#if defined(HAVE_IOURING)

import Control.Concurrent (threadWaitRead)
import Control.Exception (SomeException, bracket, toException, throwIO, try)
import Data.Bits ((.&.))
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, mallocBytes, free, callocBytes)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (peek)
import Network.Socket (Socket, withFdSocket)
import System.Posix.Types (Fd (..))

import Wireform.Ring.Internal
import Wireform.Transport
import Wireform.Transport.Config

------------------------------------------------------------------------
-- C struct size — must match cbits/iouring_glue.c struct hs_iouring
------------------------------------------------------------------------

-- sizeof(struct io_uring) is ~160 bytes on x86_64 with liburing 2.x.
-- struct hs_iouring adds ~56 bytes of fields around it.
-- We allocate generously and zero-initialize.
hsIouringSize :: Int
hsIouringSize = 512

------------------------------------------------------------------------
-- C FFI bindings
------------------------------------------------------------------------

data IOUringState

foreign import ccall unsafe "hs_iouring_create"
  c_iouring_create :: CInt -> CInt -> CInt
                   -> Ptr Word8 -> CSize
                   -> Ptr IOUringState -> IO CInt

foreign import ccall unsafe "hs_iouring_submit_recv"
  c_iouring_submit_recv :: Ptr IOUringState -> CSize -> IO CInt

foreign import ccall safe "hs_iouring_wait_cqe"
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
-- Transport state
------------------------------------------------------------------------

data IOUringTState
  = IOpen
  | IClosedEof
  | IClosedErr !SomeException

------------------------------------------------------------------------
-- Transport implementation
------------------------------------------------------------------------

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

    -- Allocate C-side state (zero-initialized)
    bracket (callocBytes hsIouringSize) free \rawPtr -> do
      let uringPtr = castPtr rawPtr :: Ptr IOUringState

      -- Create io_uring instance
      rc <- withFdSocket sock \fd ->
        c_iouring_create (fromIntegral fd)
                         (fromIntegral depth)
                         (fromIntegral sqpollIdle)
                         base (fromIntegral sz)
                         uringPtr
      if rc < 0
        then throwIO (userError $ "io_uring_create failed with code " <> show rc)
        else do
          destroyedRef <- newIORef False
          let destroy = do
                alreadyDestroyed <- readIORef destroyedRef
                if alreadyDestroyed
                  then pure ()
                  else do
                    writeIORef destroyedRef True
                    c_iouring_destroy uringPtr

          eventFd <- c_iouring_get_eventfd uringPtr
          stateRef <- newIORef IOpen
          tailRef  <- newIORef (0 :: Word64)

          -- Submit initial recv SQE
          _ <- c_iouring_submit_recv uringPtr (fromIntegral sz)

          let loadHead :: IO Word64
              loadHead = c_iouring_get_head uringPtr

              advanceTail :: Word64 -> IO ()
              advanceTail pos = writeIORef tailRef pos

              waitData :: Word64 -> IO WaitResult
              waitData pos = do
                st <- readIORef stateRef
                case st of
                  IClosedEof   -> pure EndOfInput
                  IClosedErr e -> pure (TransportError e)
                  IOpen        -> doWait pos

              doWait :: Word64 -> IO WaitResult
              doWait pos = do
                h <- loadHead
                if h > pos
                  then pure (MoreData h)
                  else do
                    -- Try non-blocking peek first
                    alloca \headPtr -> do
                      peekRc <- c_iouring_peek_cqe uringPtr headPtr
                      if peekRc > 0
                        then handleCompletion headPtr peekRc
                        else if peekRc == 0
                          then do
                            writeIORef stateRef IClosedEof
                            pure EndOfInput
                          else do
                            -- No completion ready — park on eventfd
                            threadWaitRead (Fd eventFd)
                            alloca \headPtr2 -> do
                              waitRc <- c_iouring_wait_cqe uringPtr headPtr2
                              if waitRc > 0
                                then handleCompletion headPtr2 waitRc
                                else if waitRc == 0
                                  then do
                                    writeIORef stateRef IClosedEof
                                    pure EndOfInput
                                  else do
                                    let exc = toException (userError $ "io_uring CQE error: " <> show waitRc)
                                    writeIORef stateRef (IClosedErr exc)
                                    pure (TransportError exc)

              handleCompletion :: Ptr Word64 -> CInt -> IO WaitResult
              handleCompletion headPtr _bytesRc = do
                newH <- peek headPtr
                -- Submit replacement recv bounded by available ring space
                t <- readIORef tailRef
                let !available = fromIntegral sz - fromIntegral (newH - t)
                when (available > 0) $
                  void $ c_iouring_submit_recv uringPtr (fromIntegral available)
                pure (MoreData newH)

              transport = Transport
                { transportRingBaseField = base
                , transportRingSizeField = sz
                , transportRingMaskField = msk
                , transportLoadHead      = loadHead
                , transportAdvanceTail   = advanceTail
                , transportWaitData      = waitData
                , transportClose         = do
                    writeIORef stateRef IClosedEof
                    destroy
                }

          action transport `finally` destroy
  where
    finally :: IO a -> IO () -> IO a
    finally a cleanup = do
      r <- try @SomeException a
      cleanup
      case r of
        Left e  -> throwIO e
        Right v -> pure v

    when :: Bool -> IO () -> IO ()
    when True  m = m
    when False _ = pure ()

    void :: IO a -> IO ()
    void m = m >> pure ()

#endif
