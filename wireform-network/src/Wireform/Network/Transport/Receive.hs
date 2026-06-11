{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Cross-platform receive-based magic-ring transport.

Single-threaded design: the parser thread itself calls @recv@.
The GHC IO manager (epoll\/kqueue\/IOCP) handles readiness
notification, so the thread parks without burning CPU.
-}
module Wireform.Network.Transport.Receive (
  withReceiveTransport,
  withReceiveBufTransport,
  newReceiveBufTransport,
  ReceiveFn,
  chunkedReceiveFn,
  ReceiveRingExhausted (..),

  -- * Internal builder (used by Duplex)
  buildReceiveTransport,
) where

import Control.Exception (Exception, IOException, SomeException, toException, try)
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.IORef
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Network.Socket (Socket)
import Network.Socket qualified as S
import Wireform.Ring.Internal
import Wireform.Transport.Config
import Wireform.Transport.Receive


{- | A primitive @recv()@-style callback: fill at most @n@ bytes
starting at the supplied pointer, return the number of bytes
written (@0@ on orderly EOF).  This is the lingua franca shape
'withReceiveBufTransport' bridges to a 'ReceiveTransport': callers
wrap their underlying socket / TLS context / in-memory pipe behind
a function of this type.

The callback may block.  It is called by the parser thread only.
-}
type ReceiveFn = Ptr Word8 -> Int -> IO Int


{- | The producer cannot make progress because the ring is full and
the consumer has not yet released any tail.  Surfaced as a sticky
'Wireform.Transport.Receive.ReceiveFailed' (and ultimately a
'Wireform.Parser.Error.ParseTransportError') instead of letting
the producer/consumer pair spin forever.

In practice this either means the parser is trying to consume a
single message that does not fit in the ring (raise 'ringSizeHint'
in the 'TransportConfig'), or the parser is consuming the ring
without ever advancing tail (insert a 'checkpoint' between
messages).  The wireform-core driver already detects the first
case as 'ParseRingOverflow' before suspension; this exception is
the fallback that catches the second case.
-}
data ReceiveRingExhausted = ReceiveRingExhausted
  { receiveRingExhaustedSize :: !Int
  -- ^ Physical ring size (N).
  , receiveRingExhaustedHead :: !Word64
  -- ^ Producer head when the stall was detected.
  , receiveRingExhaustedTail :: !Word64
  -- ^ Consumer tail when the stall was detected.
  }
  deriving stock (Show, Typeable)


instance Exception ReceiveRingExhausted


data ReceiveState
  = ReceiveOpen
  | ReceiveClosedEof
  | ReceiveClosedErr !SomeException


{- | Create a receive-based transport for the given socket.
The socket is NOT closed — the caller owns it.
-}
withReceiveTransport :: TransportConfig -> Socket -> (ReceiveTransport -> IO a) -> IO a
withReceiveTransport cfg sock = withReceiveBufTransport cfg (doRawRecv sock)


{- | Create a receive-based transport backed by an arbitrary
'ReceiveFn' callback.  The callback fills bytes directly into the
ring's backing memory (no intermediate 'ByteString' allocation or
copy).  Use this to adapt non-socket sources — TLS contexts (the
OpenSSL @SSL_read_ex@ bridge in
"Wireform.Network.TLS.OpenSSL"), in-memory pipes,
shared-memory rings, etc.
-}
withReceiveBufTransport
  :: TransportConfig
  -> ReceiveFn
  -> (ReceiveTransport -> IO a)
  -> IO a
withReceiveBufTransport cfg recvIntoBuf action =
  withMagicRing (ringSizeHint cfg) \ring -> do
    t <- buildReceiveTransport ring recvIntoBuf
    action t


{- | IO-style ('bracket'-free) constructor.  Allocates a fresh magic
ring + wires the supplied recv callback through it; the caller is
responsible for calling 'receiveClose' (which both flips the
transport's EOF flag /and/ unmaps the magic ring) when the
connection terminates.
-}
newReceiveBufTransport
  :: TransportConfig
  -> ReceiveFn
  -> IO ReceiveTransport
newReceiveBufTransport cfg recvIntoBuf = do
  ring <- newMagicRing (ringSizeHint cfg)
  t0 <- buildReceiveTransport ring recvIntoBuf
  pure t0 {receiveClose = receiveClose t0 *> destroyMagicRing ring}


{- | Internal: build the transport over an existing ring + recv
callback.  Used by both 'withReceiveBufTransport' and
'newReceiveBufTransport' so they share the ring-state machinery
verbatim.  Also reused by 'Wireform.Network.Transport.Duplex'.
-}
buildReceiveTransport :: MagicRing s -> ReceiveFn -> IO ReceiveTransport
buildReceiveTransport ring recvIntoBuf = do
  let !base = ringBase ring
      !msk = ringMask ring
      !sz = ringSize ring

  headRef <- newIORef (0 :: Word64)
  tailRef <- newIORef (0 :: Word64)
  stateRef <- newIORef ReceiveOpen

  let loadHead = readIORef headRef

      advanceTail pos = writeIORef tailRef pos

      waitData pos = do
        st <- readIORef stateRef
        case st of
          ReceiveClosedEof -> pure ReceiveEndOfInput
          ReceiveClosedErr e -> pure (ReceiveFailed e)
          ReceiveOpen -> doRecv pos

      doRecv pos = do
        h <- readIORef headRef
        if h > pos
          then pure (ReceiveMoreData h)
          else do
            t <- readIORef tailRef
            let !writeOff = fromIntegral h .&. msk
                !writePtr = base `plusPtr` writeOff
                !available = sz - fromIntegral (h - t)
                -- Don't cross the ring boundary in a single recv
                !maxRecv = min available (sz - writeOff)
            if maxRecv <= 0
              then do
                -- Ring is full (head - tail == ringSize) AND the
                -- consumer is asking us to advance past 'pos' >= head.
                -- We cannot satisfy this: there is no room to recv
                -- into and the consumer is stalled (otherwise it
                -- would have advanced tail by now).
                let !exc =
                      toException
                        ( ReceiveRingExhausted
                            { receiveRingExhaustedSize = sz
                            , receiveRingExhaustedHead = h
                            , receiveRingExhaustedTail = t
                            }
                        )
                writeIORef stateRef (ReceiveClosedErr exc)
                pure (ReceiveFailed exc)
              else do
                result <- try @IOException (recvIntoBuf writePtr maxRecv)
                case result of
                  Left exc -> do
                    writeIORef stateRef (ReceiveClosedErr (toException exc))
                    pure (ReceiveFailed (toException exc))
                  Right n
                    | n == 0 -> do
                        writeIORef stateRef ReceiveClosedEof
                        pure ReceiveEndOfInput
                    | otherwise -> do
                        let !newHead = h + fromIntegral n
                        writeIORef headRef newHead
                        pure (ReceiveMoreData newHead)

  pure
    ReceiveTransport
      { receiveRingBase = base
      , receiveRingSize = sz
      , receiveRingMask = msk
      , receiveLoadHead = loadHead
      , receiveAdvanceTail = advanceTail
      , receiveWaitData = waitData
      , receiveClose = writeIORef stateRef ReceiveClosedEof
      }


{- | Receive bytes from the socket directly into the ring.
Uses Network.Socket.recvBuf so the kernel writes straight into the
ring pointer — no intermediate ByteString allocation or copy.
-}
doRawRecv :: Socket -> Ptr Word8 -> Int -> IO Int
doRawRecv sock ptr maxLen = S.recvBuf sock ptr maxLen


{- | Build a 'ReceiveFn' that delivers the supplied chunks one at a
time, signalling EOF (returning @0@) once they are exhausted.

Intended primarily as a /test fixture/: callers in dependent
packages can hand this to 'withReceiveBufTransport' to drive the
streaming parser surface from a pre-baked byte stream without
spinning up a real socket pair.
-}
chunkedReceiveFn :: [BS.ByteString] -> IO ReceiveFn
chunkedReceiveFn chunks0 = do
  ref <- newIORef chunks0
  pure $ \dst want -> do
    cs <- readIORef ref
    case cs of
      [] -> pure 0
      c : rest -> do
        let !take_ = min want (BS.length c)
            !taken = BS.take take_ c
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
