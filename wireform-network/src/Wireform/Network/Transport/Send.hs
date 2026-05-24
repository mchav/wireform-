{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cross-platform send-side magic-ring transport.
--
-- Dual of "Wireform.Network.Transport.Receive": the encoder writes
-- bytes into the magic ring, the consumer (a synchronous @sendmsg@
-- loop, an io_uring SQE, an in-memory test sink, ...) drains them
-- to the wire.
--
-- The inline implementation here drives the wire from the same
-- thread that publishes head: 'sendPublishHead' synchronously calls
-- @sendmsg@ until everything published has been flushed.  That
-- matches the existing single-threaded HTTP/1 / Kafka / HTTP/2
-- connection-loop shape and removes the need for a background
-- worker in the common case.
module Wireform.Network.Transport.Send
  ( withSendTransport
  , withSendBufTransport
  , newSendBufTransport
  , SendFn
  , chunkedSendFn
  , sinkSendFn

    -- * Internal builder (used by Duplex)
  , buildSendTransport
  ) where

import Control.Exception (SomeException, try, toException, IOException)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Network.Socket (Socket)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as NBS

import Wireform.Ring.Internal
import Wireform.Transport.Send
import Wireform.Transport.Config

-- | A primitive @send()@-style callback: write up to @n@ bytes
-- starting at the supplied pointer, return the number of bytes
-- actually written (must be > 0 if not throwing).  The callback may
-- block.  It is called by the producer thread only.
--
-- Implementations should /not/ short-write on success: the inline
-- drain loop expects each invocation to drain everything passed in
-- (it loops on short writes itself).  Throw 'IOException' on
-- transport failure.
type SendFn = Ptr Word8 -> Int -> IO Int

-- | Create a send transport for the given socket.  The socket is
-- NOT closed — the caller owns it.  'sendShutdownWrite' issues
-- @shutdown(SHUT_WR)@.
withSendTransport :: TransportConfig -> Socket -> (SendTransport -> IO a) -> IO a
withSendTransport cfg sock action =
  withSendBufTransport cfg
                       (doRawSend sock)
                       (S.shutdown sock S.ShutdownSend)
                       action

-- | Create a send transport backed by an arbitrary 'SendFn'.  The
-- ring is bracket-scoped — on exit the ring's mmap is released.
withSendBufTransport
  :: TransportConfig
  -> SendFn
  -> IO ()         -- ^ shutdownWrite action (e.g. socket SHUT_WR / TLS close_notify)
  -> (SendTransport -> IO a)
  -> IO a
withSendBufTransport cfg sendBuf shut action =
  withMagicRing (ringSizeHint cfg) \ring -> do
    t <- buildSendTransport ring sendBuf shut
    action t

-- | IO-style ('bracket'-free) constructor.  The caller is
-- responsible for calling 'sendClose' (which flushes any remaining
-- bytes /and/ unmaps the ring).
newSendBufTransport
  :: TransportConfig
  -> SendFn
  -> IO ()        -- ^ shutdownWrite action
  -> IO SendTransport
newSendBufTransport cfg sendBuf shut = do
  ring <- newMagicRing (ringSizeHint cfg)
  t0   <- buildSendTransport ring sendBuf shut
  pure t0 { sendClose = sendClose t0 *> destroyMagicRing ring }

-- | Internal: build a send transport over an existing ring + send
-- callback + shutdownWrite action.
--
-- The inline drain is synchronous: 'sendPublishHead' loops calling
-- 'SendFn' on the ring's @[oldHead, newHead)@ slice until everything
-- has hit the wire (or an error is observed).  Because the drain
-- runs on the publishing thread, 'sendLoadTail' always returns the
-- last successfully drained position, and 'sendWaitSpace' is a pure
-- read.
buildSendTransport :: MagicRing s -> SendFn -> IO () -> IO SendTransport
buildSendTransport ring sendBuf shutdownAction = do
  let !base = ringBase ring
      !msk  = ringMask ring
      !sz   = ringSize ring

  headRef  <- newIORef (0 :: Word64)
  tailRef  <- newIORef (0 :: Word64)
  stateRef <- newIORef SOpen
  shutDoneRef <- newIORef False

  let loadHead = readIORef headRef
      loadTail = readIORef tailRef

      -- Drain [tl, hd) inline.  Wraps are handled by splitting the
      -- drain at the ring boundary.
      drainTo !hd = do
        tl <- readIORef tailRef
        loop tl
        where
          loop !cur
            | cur >= hd = pure ()
            | otherwise = do
                let !off       = fromIntegral cur .&. msk
                    !chunkEnd  = min (fromIntegral hd) (off + (fromIntegral hd - off))
                    -- Don't cross the physical ring boundary in one syscall:
                    -- the kernel sees a flat buffer (the double mapping is a
                    -- userspace illusion) so a chunk that crosses base+sz
                    -- would mis-address.  Clamp to (sz - off).
                    !want      = min (fromIntegral hd - fromIntegral cur)
                                     (sz - off)
                    !ptr       = base `plusPtr` off
                    _unused    = chunkEnd  -- silence -Wunused-local-binds
                result <- try @IOException (sendBuf ptr want)
                case result of
                  Left exc -> do
                    writeIORef stateRef (SClosedErr (toException exc))
                    pure ()
                  Right n
                    | n <= 0    -> do
                        let !exc = userError "sendBuf returned 0"
                        writeIORef stateRef (SClosedErr (toException exc))
                        pure ()
                    | otherwise -> do
                        let !newTail = cur + fromIntegral n
                        writeIORef tailRef newTail
                        loop newTail

      publishHead newHead = do
        st <- readIORef stateRef
        case st of
          SClosedEof   -> pure ()
          SClosedErr _ -> pure ()
          SOpen        -> do
            writeIORef headRef newHead
            drainTo newHead

      waitSpace pos = do
        st <- readIORef stateRef
        case st of
          SClosedEof   -> pure SendPeerClosed
          SClosedErr e -> pure (SendFailed e)
          SOpen        -> do
            -- Inline transport: every published byte has been
            -- drained, so loadTail == loadHead.  Therefore the
            -- only reason to be in waitSpace is that 'pos' is more
            -- than ringSize ahead of tail, which is impossible
            -- under the encoder API (reserveSend caps at ringSize)
            -- — but we handle it gracefully by reporting current
            -- tail as the new room.
            tl <- readIORef tailRef
            pure (SendSpaceAvailable tl)

      flush = do
        h <- readIORef headRef
        drainTo h

      shutdown_ = do
        already <- readIORef shutDoneRef
        if already then pure ()
          else do
            writeIORef shutDoneRef True
            r <- try @SomeException shutdownAction
            case r of
              Left _  -> pure ()
              Right _ -> pure ()

      close = do
        st <- readIORef stateRef
        case st of
          SOpen -> do
            h <- readIORef headRef
            drainTo h
            writeIORef stateRef SClosedEof
          _ -> pure ()

  pure SendTransport
    { sendRingBase     = base
    , sendRingSize     = sz
    , sendRingMask     = msk
    , sendLoadTail     = loadTail
    , sendLoadHead     = loadHead
    , sendPublishHead  = publishHead
    , sendWaitSpace    = waitSpace
    , sendFlush        = flush
    , sendShutdownWrite = shutdown_
    , sendClose        = close
    }

data SendState
  = SOpen
  | SClosedEof
  | SClosedErr !SomeException

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Send bytes from the ring directly into the socket.  Uses
-- 'Network.Socket.sendBuf' which writes from the supplied pointer
-- with no intermediate ByteString allocation.
doRawSend :: Socket -> Ptr Word8 -> Int -> IO Int
doRawSend sock ptr len = S.sendBuf sock ptr len

-- | Build a 'SendFn' that pushes each successful send into an
-- 'IORef' as a freshly-copied 'ByteString'.  Useful as a test
-- fixture: the test inspects the collected list to assert what was
-- written.  Always succeeds; never throws.
sinkSendFn :: IORef [BS.ByteString] -> SendFn
sinkSendFn ref ptr len = do
  bs <- BSI.create len $ \dst -> copyBytes dst ptr len
  atomicModifyIORef' ref $ \acc -> (bs : acc, ())
  pure len

-- | Build a 'SendFn' that drains into an in-memory queue (FIFO),
-- forwarding everything written so a paired 'chunkedReceiveFn'
-- could consume it.  Used by the in-memory duplex pipe in
-- "Wireform.Network.Transport.Pipe".
chunkedSendFn :: IORef [BS.ByteString] -> SendFn
chunkedSendFn ref ptr len = do
  bs <- BSI.create len $ \dst -> copyBytes dst ptr len
  atomicModifyIORef' ref $ \acc -> (acc ++ [bs], ())
  pure len
{-# DEPRECATED chunkedSendFn "chunkedSendFn appends to the tail of the list (O(n)); prefer Pipe.newDuplexPipe for an actual queue." #-}
