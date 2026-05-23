{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Cross-platform recv-based transport.
--
-- Single-threaded design: the parser thread itself calls @recv@.
-- The GHC IO manager (epoll\/kqueue\/IOCP) handles readiness
-- notification, so the thread parks without burning CPU.
module Wireform.Network.Transport.Recv
  ( withRecvTransport
  , withRecvBufTransport
  , newRecvBufTransport
  , RecvFn
  , chunkedRecvFn
  ) where

import Control.Exception (SomeException, try, toException, IOException)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Network.Socket (Socket)
import qualified Network.Socket as S

import Wireform.Ring.Internal
import Wireform.Transport
import Wireform.Transport.Config

-- | A primitive @recv()@-style callback: fill at most @n@ bytes
-- starting at the supplied pointer, return the number of bytes
-- written (@0@ on orderly EOF).  This is the lingua franca shape
-- 'withRecvBufTransport' bridges to a 'Transport': callers wrap
-- their underlying socket / TLS context / crypton-connection /
-- in-memory pipe behind a function of this type.
--
-- The callback may block.  It is called by the parser thread only.
type RecvFn = Ptr Word8 -> Int -> IO Int

data RecvState
  = RecvOpen
  | RecvClosedEof
  | RecvClosedErr !SomeException

-- | Create a recv-based transport for the given socket.
-- The socket is NOT closed — the caller owns it.
withRecvTransport :: TransportConfig -> Socket -> (Transport -> IO a) -> IO a
withRecvTransport cfg sock = withRecvBufTransport cfg (doRawRecv sock)

-- | Create a recv-based transport backed by an arbitrary recv callback.
--
-- The callback fills bytes directly into the ring's backing memory
-- (no intermediate 'ByteString' allocation or copy).  Use this to
-- adapt non-socket sources — TLS contexts (@tls@'s @recvData@ wrapped
-- in a chunk-buffer), @crypton-connection@ 'Network.Connection' (via
-- 'connectionGetChunk' + a leftover holdover), in-memory pipes,
-- shared-memory rings, etc.
--
-- The recv callback semantics match POSIX @recv@:
--
--   * Block until at least one byte is available, then return up to
--     the supplied @n@.
--   * Return @0@ on orderly EOF.
--   * Throw 'IOException' on transport failure (sticky from the
--     transport's perspective: subsequent waits return
--     'TransportError').
--
-- The transport never closes the underlying source; the caller
-- owns its lifetime.
withRecvBufTransport
  :: TransportConfig
  -> RecvFn
  -> (Transport -> IO a)
  -> IO a
withRecvBufTransport cfg recvIntoBuf action =
  withMagicRing (ringSizeHint cfg) \ring -> do
    t <- mkTransport ring recvIntoBuf
    action t

-- | IO-style ('bracket'-free) constructor.  Allocates a fresh magic
-- ring + wires the supplied recv callback through it; the caller is
-- responsible for calling 'transportClose' (which both flips the
-- transport's EOF flag /and/ unmaps the magic ring) when the
-- connection terminates.
--
-- Preferred over 'withRecvBufTransport' when the transport's
-- lifetime is tied to a long-lived stateful object (e.g.
-- 'Network.HTTP1.Connection') rather than a single nested action.
--
-- After 'transportClose' the ring's memory is gone; any
-- 'BS.ByteString' slices into the ring that the caller still holds
-- become dangling pointers.  Copy them before close.
newRecvBufTransport
  :: TransportConfig
  -> RecvFn
  -> IO Transport
newRecvBufTransport cfg recvIntoBuf = do
  ring <- newMagicRing (ringSizeHint cfg)
  t0   <- mkTransport ring recvIntoBuf
  pure t0 { transportClose = transportClose t0 *> destroyMagicRing ring }

-- | Internal: build the transport over an existing ring + recv
-- callback.  Used by both 'withRecvBufTransport' and
-- 'newRecvBufTransport' so they share the ring-state machinery
-- verbatim.
mkTransport :: MagicRing -> RecvFn -> IO Transport
mkTransport ring recvIntoBuf = do
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
                -- Don't cross the ring boundary in a single recv
                !maxRecv   = min available (sz - writeOff)
            if maxRecv <= 0
              then pure (MoreData h)
              else do
                result <- try @IOException (recvIntoBuf writePtr maxRecv)
                case result of
                  Left exc -> do
                    writeIORef stateRef (RecvClosedErr (toException exc))
                    pure (TransportError (toException exc))
                  Right n
                    | n == 0 -> do
                        writeIORef stateRef RecvClosedEof
                        pure EndOfInput
                    | otherwise -> do
                        let !newHead = h + fromIntegral n
                        writeIORef headRef newHead
                        pure (MoreData newHead)

  pure Transport
    { transportRing        = ring
    , transportLoadHead    = loadHead
    , transportAdvanceTail = advanceTail
    , transportWaitData    = waitData
    , transportClose       = writeIORef stateRef RecvClosedEof
    }

-- | Receive bytes from the socket directly into the ring.
-- Uses Network.Socket.recvBuf so the kernel writes straight into the
-- ring pointer — no intermediate ByteString allocation or copy.
doRawRecv :: Socket -> Ptr Word8 -> Int -> IO Int
doRawRecv sock ptr maxLen = S.recvBuf sock ptr maxLen

-- | Build a 'RecvFn' that delivers the supplied chunks one at a
-- time, signalling EOF (returning @0@) once they are exhausted.
--
-- Intended primarily as a /test fixture/: callers in dependent
-- packages can hand this to 'withRecvBufTransport' to drive the
-- streaming parser surface from a pre-baked byte stream without
-- spinning up a real socket pair.  Chunk boundaries are honoured
-- so the parser's suspend / resume path gets exercised.
--
-- Example:
--
-- @
-- recvFn <- chunkedRecvFn [\"\\x05\", \"hello\", \"\\x05\", \"world\"]
-- withRecvBufTransport defaultTransportConfig recvFn $ \\t ->
--   runParserLoop t myParser handler
-- @
chunkedRecvFn :: [BS.ByteString] -> IO RecvFn
chunkedRecvFn chunks0 = do
  ref <- newIORef chunks0
  pure $ \dst want -> do
    cs <- readIORef ref
    case cs of
      [] -> pure 0
      c : rest -> do
        let !take_    = min want (BS.length c)
            !taken    = BS.take take_ c
            !leftover = BS.drop take_ c
        writeIORef ref (if BS.null leftover then rest else leftover : rest)
        copyBSInto dst taken
        pure take_
  where
    copyBSInto :: Ptr Word8 -> BS.ByteString -> IO ()
    copyBSInto dst bs =
      let (fp, off, len) = BSI.toForeignPtr bs
      in withForeignPtr fp $ \src ->
           copyBytes dst (src `plusPtr` off) len
