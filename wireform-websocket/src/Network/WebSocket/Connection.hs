{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | A live WebSocket connection on top of a wireform-core duplex
transport.

This module exposes:

* 'Connection' \u2014 an owned pair of
  'Wireform.Transport.Send.SendTransport' \/
  'Wireform.Transport.Receive.ReceiveTransport' (the same shape
  every wireform-http connection sits on) plus per-connection
  state for masking direction and close handshake.
* Frame-level I\/O: 'receiveFrame', 'sendFrame'.
* Control frame helpers: 'sendPing', 'sendPong', 'sendClose',
  'sendCloseWith'.
* 'closeConnection' \u2014 idempotent, half-closes the wire and
  releases the rings.

The connection is /side-aware/.  Server connections must send
unmasked frames and reject unmasked frames from the client;
client connections must mask outbound frames and reject masked
inbound frames (RFC 6455 \u00a75.1, \u00a75.3).  See
'connectionRole'.

The receive path runs the 'Wireform.Parser.Stream' frame parser
through 'Wireform.Parser.Driver.runParser', so the per-frame hot
path is exactly the same shape as every other format in the
workspace.
-}
module Network.WebSocket.Connection
  ( -- * Role
    Role (..)
  , peerRole

    -- * Connection
  , Connection
  , newConnection
  , newConnectionUnlocked
  , connectionRole
  , connectionReceive
  , connectionSend
  , connectionPmd
  , attachCleanup
  , attachPmd
  , closeConnection

    -- * Frame I\/O
  , receiveFrame
  , sendFrame
  , sendDataFrame
  , trySendFrame

    -- * Send batching
  , withFrameBatch

    -- * Control frames
  , sendPing
  , sendPong
  , sendClose
  , sendCloseWith
  , failConnection
  , CloseCode (..)
  , normalClosure
  , goingAway
  , protocolError
  , unsupportedData
  , invalidPayload
  , policyViolation
  , messageTooBig
  , internalError

    -- * Errors
  , WebSocketError (..)
  ) where

import Control.Concurrent.MVar
import Control.Exception (Exception, SomeException, mask_, throwIO, try)
import Control.Monad (unless, when)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word8, Word16, Word64)

import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (pokeByteOff)
import qualified Data.ByteString.Unsafe as BSU

import qualified Wireform.FFI as FFI
import Wireform.Parser.Driver
  ( InternalResult (..), runParserInternal )
import Wireform.Parser.Error (ParseError (..))
import qualified Wireform.Transport.Receive as TR
import Wireform.Transport.Receive (ReceiveTransport)
import Wireform.Transport.Send (SendTransport, sendBuilderDirect,
  sendPublishHead, sendRingSize, sendShutdownWrite, reserveSend,
  withSendCork)
import qualified Wireform.Network as N

import Network.WebSocket.Connection.Role
import Network.WebSocket.Frame
import Network.WebSocket.PerMessageDeflate (PmdContext, freePmdContext)

------------------------------------------------------------------------
-- Connection
------------------------------------------------------------------------

data Connection = Connection
  { connDuplex :: !N.DuplexTransport
  , connRole   :: !Role
  , connLimit  :: !PayloadLimit
  , connRecvLock :: !(Maybe (MVar ()))
    -- ^ Serialise frame receives.  The wireform parser is
    -- inherently single-threaded per ring; the lock ensures a
    -- concurrent 'receiveFrame' from two threads doesn't trample
    -- each other's parse state.
    --
    -- 'Nothing' means /single-threaded use/: the caller has
    -- promised no other thread will call 'receiveFrame' on this
    -- connection.  The hot path then skips the @MVar@
    -- take \/ put round-trip (~700 ns on Linux per direction).
    -- Use 'newConnectionUnlocked' to construct such a connection.
  , connSendLock :: !(Maybe (MVar ()))
    -- ^ Serialise frame sends.  A 'Wireform.Transport.Send'
    -- reservation must be committed before the next reservation
    -- starts, so we lock the send side too.  'Nothing' has the
    -- same meaning as on 'connRecvLock'.
  , connRecvPos :: !(IORef Word64)
    -- ^ Logical consumer position in the recv ring.  Updated
    -- after every successful frame parse so the next call to
    -- 'receiveFrame' picks up where the previous one left off.
    -- Without this, calling 'runParser' twice in succession
    -- would re-read 'receiveLoadHead' \u2014 which is the
    -- /producer/ position \u2014 and silently skip any frames
    -- buffered between the consumer position and the head.
  , connSentClose :: !(IORef Bool)
    -- ^ Set after the first close frame goes out.  Subsequent
    -- 'sendClose' / 'sendCloseWith' calls become no-ops so a
    -- protocol-error close from the receive path doesn't get
    -- doubled by the server runner's polite-close path.
  , connCleanup  :: !(IORef (IO ()))
    -- ^ Extra teardown actions queued by the connection's
    -- builder (e.g. OpenSSL 'SslConn' \/ 'SslCtx' release, the
    -- raw socket close on the client side).  Each
    -- 'attachCleanup' /prepends/ its action, so callers run in
    -- last-attached-first-executed order at 'closeConnection'
    -- time \u2014 the same shape nested 'bracket's unwind.
  , connPmd      :: !(IORef (Maybe PmdContext))
    -- ^ Permessage-deflate context (RFC 7692).  'Nothing' means
    -- the extension was not negotiated and RSV1 is forbidden.
    -- Set via 'attachPmd' during the post-handshake setup; the
    -- matching 'freePmdContext' is queued onto 'connCleanup'
    -- so 'closeConnection' tears it down.
  , connClosed   :: !(IORef Bool)
  }

-- | Build a 'Connection' from a duplex transport and the role this
-- side plays.  Takes ownership of the duplex; 'closeConnection'
-- releases it.  Installs the per-direction locks so it is safe to
-- call 'sendFrame' and 'receiveFrame' from different threads.
newConnection
  :: Role
  -> PayloadLimit
  -> N.DuplexTransport
  -> IO Connection
newConnection role lim duplex = do
  rLock <- newMVar ()
  sLock <- newMVar ()
  buildConnection role lim duplex (Just rLock) (Just sLock)

-- | Variant of 'newConnection' that skips the per-direction
-- 'MVar' locks.  The caller promises that no two threads ever
-- concurrently call 'sendFrame' or 'receiveFrame' (or any of the
-- higher-level helpers built on them) on the same 'Connection'.
-- In return, the hot path on each side saves the ~700 ns
-- 'takeMVar' \/ 'putMVar' round-trip per frame.
--
-- The typical chat-server pattern \u2014 one thread per
-- connection, alternating recv \/ send in a loop \u2014 is
-- already single-threaded and benefits unconditionally.  For
-- broadcast \/ fan-out shapes where one thread sends and a
-- different thread reads, use 'newConnection' instead.
newConnectionUnlocked
  :: Role
  -> PayloadLimit
  -> N.DuplexTransport
  -> IO Connection
newConnectionUnlocked role lim duplex =
  buildConnection role lim duplex Nothing Nothing

buildConnection
  :: Role
  -> PayloadLimit
  -> N.DuplexTransport
  -> Maybe (MVar ())
  -> Maybe (MVar ())
  -> IO Connection
buildConnection role lim duplex rLock sLock = do
  pos0      <- TR.receiveLoadHead (N.duplexReceive duplex)
  posRef    <- newIORef pos0
  closeSent <- newIORef False
  cleanup   <- newIORef (pure ())
  pmdRef    <- newIORef Nothing
  cref      <- newIORef False
  pure Connection
    { connDuplex    = duplex
    , connRole      = role
    , connLimit     = lim
    , connRecvLock  = rLock
    , connSendLock  = sLock
    , connRecvPos   = posRef
    , connSentClose = closeSent
    , connCleanup   = cleanup
    , connPmd       = pmdRef
    , connClosed    = cref
    }

connectionRole :: Connection -> Role
connectionRole = connRole

connectionReceive :: Connection -> ReceiveTransport
connectionReceive = N.duplexReceive . connDuplex

connectionSend :: Connection -> SendTransport
connectionSend = N.duplexSend . connDuplex

-- | Tear down the connection.  Idempotent.  Half-closes the
-- underlying transport (the receive side stays open so the peer's
-- close frame can still arrive); 'N.closeDuplexTransport' releases
-- the rings.  Then any cleanups attached with 'attachCleanup' run
-- in last-attached-first order \u2014 matching how nested
-- 'bracket's would unwind.  Callers that already issued an
-- application-level 'sendClose' need not call this immediately
-- \u2014 keep reading until 'receiveFrame' raises
-- 'WebSocketPeerClosed' first to drain a polite shutdown.
closeConnection :: Connection -> IO ()
closeConnection conn = mask_ $ do
  wasClosed <- atomicModifyIORef' (connClosed conn) (\b -> (True, b))
  unless wasClosed $ do
    _ <- try @SomeException $
      sendShutdownWrite (connectionSend conn)
    _ <- try @SomeException $ N.closeDuplexTransport (connDuplex conn)
    runCleanups <- readIORef (connCleanup conn)
    _ <- try @SomeException runCleanups
    pure ()

-- | Permessage-deflate context (RFC 7692) attached to this
-- connection, or 'Nothing' if the extension was not negotiated.
-- Reads through an 'IORef' so callers that need to introspect a
-- live connection (debug tooling, runtime logging) can see the
-- current state without races against 'attachPmd'.
connectionPmd :: Connection -> IO (Maybe PmdContext)
connectionPmd = readIORef . connPmd

-- | Attach a 'PmdContext' to a fresh connection.  Idempotent in
-- the sense that re-attaching frees the prior context first; in
-- normal use this is only called once, immediately after the
-- WebSocket handshake completes and the negotiated parameters are
-- known.  The cleanup that releases the C-level @z_stream@s is
-- queued onto 'connCleanup', so 'closeConnection' frees them.
attachPmd :: Connection -> PmdContext -> IO ()
attachPmd conn ctx = mask_ $ do
  -- Replace any prior context.  The only path that re-attaches in
  -- practice is a test fixture that swaps contexts to verify
  -- failure modes; freeing the old one keeps that path leak-free.
  mPrev <- atomicModifyIORef' (connPmd conn) (\old -> (Just ctx, old))
  case mPrev of
    Nothing   -> pure ()
    Just prev -> do
      _ <- try @SomeException (freePmdContext prev)
      pure ()
  -- See 'attachCleanup' for the LIFO ordering note.
  atomicModifyIORef' (connCleanup conn) $ \prev ->
    (freePmdContext ctx `safeFinally` prev, ())
  where
    safeFinally a b = do
      _ <- try @SomeException a
      _ <- try @SomeException b
      pure ()

-- | Stack a teardown action onto the connection.  Runs at
-- 'closeConnection' time after the duplex has been released.
-- Use this to release any transport \/ TLS state that lives
-- /outside/ the duplex transport (e.g. an OpenSSL @SSL_CTX@ that
-- the client connect path allocated).
attachCleanup :: Connection -> IO () -> IO ()
attachCleanup conn act = atomicModifyIORef' (connCleanup conn) $ \prev ->
  (act `safeFinally` prev, ())
  where
    -- Run @a@; whether it succeeds or throws, also run @b@.  Both
    -- exceptions are best-effort swallowed because we are in the
    -- cleanup path.
    safeFinally a b = do
      _ <- try @SomeException a
      _ <- try @SomeException b
      pure ()

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

data WebSocketError
  = WebSocketProtocolError !String
  | WebSocketFrameError    !FrameError
  | WebSocketDecodeError   !String
  | WebSocketTransportError !String
  | WebSocketPeerClosed    !(Maybe CloseCode) !ByteString
  deriving stock (Show)

instance Exception WebSocketError

------------------------------------------------------------------------
-- Receive
------------------------------------------------------------------------

-- | Read one frame from the connection, applying the per-direction
-- masking rules from RFC 6455 \u00a75.1:
--
--   * Server-role connections require every incoming frame to be
--     masked.
--   * Client-role connections require every incoming frame to be
--     unmasked.
--
-- A 'Close' frame received here is /not/ acted on; the high-level
-- "Network.WebSocket.Message" layer drives the close handshake.
--
-- Position bookkeeping: each call resumes from where the previous
-- one left off via 'runParserInternal' + 'connRecvPos', so two
-- successive frames buffered on the wire are both parsed.  Using
-- 'runParser' directly here would lose the second frame because it
-- re-reads the producer head as the start position.
-- | Run @act@ holding the connection's recv lock, or directly
-- when the connection was constructed lock-free.  Inlined at the
-- call site so the unlocked path doesn't allocate a closure for
-- the action argument.
withRecvLock :: Connection -> IO a -> IO a
withRecvLock conn act = case connRecvLock conn of
  Nothing -> act
  Just mv -> withMVar mv (\_ -> act)
{-# INLINE withRecvLock #-}

withSendLock :: Connection -> IO a -> IO a
withSendLock conn act = case connSendLock conn of
  Nothing -> act
  Just mv -> withMVar mv (\_ -> act)
{-# INLINE withSendLock #-}

receiveFrame :: Connection -> IO Frame
receiveFrame conn = case connRecvLock conn of
  Nothing -> receiveFrameRaw conn
  Just mv -> withMVar mv (\_ -> receiveFrameRaw conn)
{-# INLINE receiveFrame #-}

-- | The lock-free receive body.  Kept in its own top-level
-- binding so the locked and unlocked entry points in
-- 'receiveFrame' share it without each duplicating the parser
-- machinery; the @case connRecvLock@ dispatch is local to
-- 'receiveFrame' and does not allocate a closure that captures
-- this body.
receiveFrameRaw :: Connection -> IO Frame
receiveFrameRaw conn = do
  pos <- readIORef (connRecvPos conn)
  r   <- runParserInternal (connectionReceive conn)
                           (parseFrame (connLimit conn))
                           pos
  case r of
    IRDone newPos f -> do
      writeIORef (connRecvPos conn) newPos
      checkInboundMask conn f
      checkOpcode conn f
      pure f
    IRFail _              -> throwIO (parseErrorToWS (ParseFail 0))
    IRErr  _ e            -> throwIO (parseErrorToWS (ParseErr 0 e))
    IRUnexpectedEof _ n   -> throwIO (parseErrorToWS (ParseUnexpectedEof 0 n))
    IRTransportError exc  -> throwIO (parseErrorToWS (ParseTransportError exc))
    IRCleanEof            -> throwIO (parseErrorToWS (ParseUnexpectedEof 0 0))
    IRRingOverflow _ n sz -> throwIO (parseErrorToWS (ParseRingOverflow 0 n sz))

parseErrorToWS :: ParseError FrameError -> WebSocketError
parseErrorToWS = \case
  ParseFail _              -> WebSocketDecodeError "frame parse failed"
  ParseErr _ e             -> WebSocketFrameError e
  ParseUnexpectedEof _ _   -> WebSocketTransportError "unexpected EOF"
  ParseTransportError exc  -> WebSocketTransportError (show exc)
  ParseRingOverflow _ n sz ->
    WebSocketProtocolError ("frame larger than receive ring: "
                             <> show n <> " > " <> show sz)

checkInboundMask :: Connection -> Frame -> IO ()
checkInboundMask conn f = case connRole conn of
  Server -> case frameMask f of
    Just _  -> pure ()
    Nothing -> failConnection conn protocolError
                 "client-to-server frame is not masked"
  Client -> case frameMask f of
    Nothing -> pure ()
    Just _  -> failConnection conn protocolError
                 "server-to-client frame must not be masked"

-- | Validate the incoming frame against the RFC 6455 framing
-- rules the connection layer is responsible for (the parser
-- already validated the wire format).  RSV1 is allowed when
-- @permessage-deflate@ is negotiated (RFC 7692 \u00a76.1) and
-- the frame is a data frame (RSV1 on control frames is a
-- protocol error per RFC 7692 \u00a76.1); RSV1 on a continuation
-- frame is rejected at the message-reassembly layer
-- ('Network.WebSocket.Message') because that layer knows the
-- message boundary.  RSV2 and RSV3 are always protocol errors
-- (no extension in this implementation claims them).
checkOpcode :: Connection -> Frame -> IO ()
checkOpcode conn f = do
  pmd <- readIORef (connPmd conn)
  let pmdActive = case pmd of { Just _ -> True ; Nothing -> False }
      rsv1Ok    = pmdActive && isData (frameOpcode f)
  when ((frameRsv1 f && not rsv1Ok) || frameRsv2 f || frameRsv3 f) $
    failConnection conn protocolError
      "RSV bit set but no extension negotiated"
  case frameOpcode f of
    OpReservedNonControl w ->
      failConnection conn protocolError
        ("reserved non-control opcode: " <> show w)
    OpReservedControl w ->
      failConnection conn protocolError
        ("reserved control opcode: " <> show w)
    _ | opcodeIsControl (frameOpcode f) ->
          if BS.length (framePayload f) > 125
            then failConnection conn protocolError
                   "control frame payload exceeds 125 bytes"
            else if not (frameFin f)
              then failConnection conn protocolError
                     "control frame must have FIN=1"
              else pure ()
      | otherwise -> pure ()

------------------------------------------------------------------------
-- Send
------------------------------------------------------------------------

-- | Send a frame, applying per-direction masking rules:
--
--   * Server-role connections always send /unmasked/ frames; any
--     'frameMask' set by the caller is dropped.
--   * Client-role connections always send /masked/ frames; if the
--     caller did not provide a mask, a fresh one is rolled with
--     'randomMask'.
--
-- @OpClose@ is idempotent: only the first close frame goes out
-- (RFC 6455 \u00a75.5.1).  Callers can spam @sendClose@ from
-- multiple cleanup paths without doubling up the close on the
-- wire.
sendFrame :: Connection -> Frame -> IO ()
sendFrame conn f0 = case frameOpcode f0 of
  OpClose -> do
    alreadySent <- atomicModifyIORef' (connSentClose conn) (\b -> (True, b))
    unless alreadySent (dispatch f0)
  _ -> dispatch f0
  where
    dispatch f1 = do
      f <- applyOutboundMask conn f1
      case connSendLock conn of
        Nothing -> sendOneFrame (connectionSend conn) f
        Just mv -> withMVar mv (\_ -> sendOneFrame (connectionSend conn) f)

-- | Allocation-free fast path for the common case: a single
-- non-fragmented, RSV-zero, non-control frame with the given
-- opcode and payload.  Skips the 'Frame' record allocation and
-- the 'applyOutboundMask' dispatch.
--
-- Used by 'Network.WebSocket.Message.sendTextMessage' /
-- 'sendBinaryMessage' so the per-message hot path doesn't touch
-- the 'Frame' record at all.  Falls through to 'sendFrame' for
-- control opcodes (close idempotency etc. still applies).
sendDataFrame :: Connection -> Opcode -> ByteString -> IO ()
sendDataFrame conn op payload
  | opcodeIsControl op =
      -- Build a Frame only for control opcodes — sendFrame
      -- handles the idempotency check and validation.
      sendFrame conn Frame
        { frameFin     = True
        , frameRsv1    = False
        , frameRsv2    = False
        , frameRsv3    = False
        , frameOpcode  = op
        , frameMask    = Nothing
        , framePayload = payload
        }
  | otherwise = do
      -- Roll a mask (client side) or skip it (server side)
      -- without touching the Frame record.
      mMask <- case connRole conn of
        Server -> pure Nothing
        Client -> Just <$> randomMask  -- one xoshiro256++ FFI call
      case connSendLock conn of
        Nothing -> sendDataFrameBytes (connectionSend conn) op mMask payload
        Just mv -> withMVar mv (\_ ->
          sendDataFrameBytes (connectionSend conn) op mMask payload)

-- | Inner worker for 'sendDataFrame'.  All Frame fields are
-- baked in (FIN=1, RSV=0).  This is the function the message
-- API's hot path compiles down to.
sendDataFrameBytes
  :: SendTransport
  -> Opcode
  -> Maybe Mask
  -> ByteString
  -> IO ()
sendDataFrameBytes t op mMask payload = do
  let !plen   = BS.length payload
      !maskBit = case mMask of { Just _ -> 0x80 ; Nothing -> 0x00 } :: Word8
      !maskLen = case mMask of { Just _ -> 4    ; Nothing -> 0    } :: Int
      !lenField
        | plen <= 125     = 1
        | plen <= 0xFFFF  = 3
        | otherwise       = 9
      !hdrLen = 1 + lenField + maskLen
      !needed = hdrLen + plen
      !b1     = 0x80 .|. (opcodeToWord op .&. 0x0F)  -- FIN=1, RSV=0
  if needed > sendRingSize t
    then sendBuilderDirect t (buildFrame Frame
           { frameFin     = True
           , frameRsv1    = False
           , frameRsv2    = False
           , frameRsv3    = False
           , frameOpcode  = op
           , frameMask    = mMask
           , framePayload = payload
           })
    else do
      (p, newHead) <- reserveSend t needed
      pokeByteOff p 0 b1
      case lenField of
        1 -> pokeByteOff p 1 (maskBit .|. fromIntegral plen :: Word8)
        3 -> do
          pokeByteOff p 1 (maskBit .|. 126 :: Word8)
          pokeByteOff p 2 (fromIntegral (plen `shiftR` 8) :: Word8)
          pokeByteOff p 3 (fromIntegral  plen             :: Word8)
        _ -> do
          let !plen64 = fromIntegral plen :: Word64
          pokeByteOff p 1 (maskBit .|. 127 :: Word8)
          pokeByteOff p 2 (fromIntegral (plen64 `shiftR` 56) :: Word8)
          pokeByteOff p 3 (fromIntegral (plen64 `shiftR` 48) :: Word8)
          pokeByteOff p 4 (fromIntegral (plen64 `shiftR` 40) :: Word8)
          pokeByteOff p 5 (fromIntegral (plen64 `shiftR` 32) :: Word8)
          pokeByteOff p 6 (fromIntegral (plen64 `shiftR` 24) :: Word8)
          pokeByteOff p 7 (fromIntegral (plen64 `shiftR` 16) :: Word8)
          pokeByteOff p 8 (fromIntegral (plen64 `shiftR`  8) :: Word8)
          pokeByteOff p 9 (fromIntegral  plen64              :: Word8)
      case mMask of
        Nothing -> pure ()
        Just (Mask w) -> do
          let !moff = 1 + lenField
          pokeByteOff p  moff      (fromIntegral (w `shiftR` 24) :: Word8)
          pokeByteOff p (moff + 1) (fromIntegral (w `shiftR` 16) :: Word8)
          pokeByteOff p (moff + 2) (fromIntegral (w `shiftR`  8) :: Word8)
          pokeByteOff p (moff + 3) (fromIntegral  w              :: Word8)
      BSU.unsafeUseAsCStringLen payload $ \(src, _) ->
        copyBytes (p `plusPtr` hdrLen) (castPtr src) plen
      case mMask of
        Nothing       -> pure ()
        Just (Mask w) ->
          FFI.xorRepeatingKey (p `plusPtr` hdrLen) plen w
      sendPublishHead t newHead
{-# INLINE sendDataFrameBytes #-}

-- | Write a frame to the send ring in one shot when the frame
-- fits.  Falls back to 'sendBuilderDirect' for frames larger than
-- the ring (where chunked publishes are unavoidable).
--
-- The fits-in-ring path issues one ring reservation, pokes the
-- header bytes directly into the reserved span, copies the
-- payload right after, then publishes.  No intermediate
-- 'ByteString' list, no @[hdr, payload]@ cons cells.  For
-- unmasked server frames the payload @memcpy@ is the only
-- per-byte cost in user space; the masked client path additionally
-- runs the SIMD XOR (via 'Wireform.FFI.xorRepeatingKey') while
-- copying into the ring.
sendOneFrame :: SendTransport -> Frame -> IO ()
sendOneFrame t f = do
  let !payloadBS = framePayload f
      !plen      = BS.length payloadBS
      !hdrLen    = frameHeaderLength f
      !needed    = hdrLen + plen
  if needed <= sendRingSize t
    then do
      (ringPtr, newHead) <- reserveSend t needed
      _ <- writeFrameHeader f ringPtr
      BSU.unsafeUseAsCStringLen payloadBS $ \(src, _) ->
        copyBytes (ringPtr `plusPtr` hdrLen) (castPtr src) plen
      case frameMask f of
        Nothing       -> pure ()
        Just (Mask w) ->
          FFI.xorRepeatingKey (ringPtr `plusPtr` hdrLen) plen w
      sendPublishHead t newHead
    else
      -- Larger than the ring; fall through the chunked builder
      -- path.  This is the only correctness-preserving option
      -- because no single ring reservation can hold the frame.
      sendBuilderDirect t (buildFrame f)

-- | Batch all frame sends inside @action@ into one underlying
-- 'sendmsg' / 'SSL_write' syscall (or as few as possible \u2014
-- the cork breaks when the ring genuinely fills).  RFC 6455
-- doesn't say anything about this; it's a pure throughput
-- optimisation for applications that emit multiple frames in
-- one logical operation (chat broadcasts, market-data fan-out,
-- file uploads emitted as a sequence of binary frames).
--
-- Drives 'Wireform.Transport.Send.withSendCork' under the hood;
-- a single publish covers everything written inside the
-- callback.  No correctness change: control frames inside the
-- batch are still delivered in order, the close-idempotency
-- check still runs, masking still applies.
withFrameBatch :: Connection -> (Connection -> IO a) -> IO a
withFrameBatch conn action =
  withSendCork (connectionSend conn) $ \corkedSend ->
    -- Splice the corked SendTransport back onto the connection
    -- so the user's nested 'sendFrame' / 'sendDataFrame' calls
    -- pick it up.  The duplex receive side is untouched.
    let corkedDuplex = (connDuplex conn) { N.duplexSend = corkedSend }
    in action conn { connDuplex = corkedDuplex }

-- | Non-throwing variant: catches 'IOException's from the
-- underlying transport (peer reset etc.) and converts them to a
-- 'WebSocketError'.  Cheap wrapper over 'sendFrame'.
trySendFrame :: Connection -> Frame -> IO (Either WebSocketError ())
trySendFrame conn f = do
  r <- try @IOError (sendFrame conn f)
  pure $ case r of
    Right () -> Right ()
    Left e   -> Left (WebSocketTransportError (show e))

applyOutboundMask :: Connection -> Frame -> IO Frame
applyOutboundMask conn f = case connRole conn of
  Server -> pure f { frameMask = Nothing }
  Client -> case frameMask f of
    Just _  -> pure f
    Nothing -> do
      m <- randomMask  -- one xoshiro256++ FFI call, thread-local
      pure f { frameMask = Just m }

------------------------------------------------------------------------
-- Control frames
------------------------------------------------------------------------

sendPing :: Connection -> ByteString -> IO ()
sendPing conn payload = sendFrame conn Frame
  { frameFin     = True
  , frameRsv1    = False
  , frameRsv2    = False
  , frameRsv3    = False
  , frameOpcode  = OpPing
  , frameMask    = Nothing
  , framePayload = payload
  }

sendPong :: Connection -> ByteString -> IO ()
sendPong conn payload = sendFrame conn Frame
  { frameFin     = True
  , frameRsv1    = False
  , frameRsv2    = False
  , frameRsv3    = False
  , frameOpcode  = OpPong
  , frameMask    = Nothing
  , framePayload = payload
  }

-- | Send a 'Close' frame with no body.  Equivalent to
-- @'sendCloseWith' conn 'normalClosure' ""@.
sendClose :: Connection -> IO ()
sendClose conn = sendCloseWith conn normalClosure ""

-- | Send a 'Close' frame with a status code and optional UTF-8
-- reason payload (RFC 6455 \u00a75.5.1).  Reason is truncated to
-- 123 bytes so the close frame stays within the 125-byte control
-- frame limit.
-- | Fail the connection per RFC 6455 \u00a77.1.7: best-effort send
-- a close frame with the supplied status code and reason, then
-- raise 'WebSocketProtocolError' so the caller's handler unwinds.
-- The close send is wrapped in 'try' because the peer may have
-- already torn the wire down.
failConnection :: Connection -> CloseCode -> String -> IO a
failConnection conn code reason = do
  _ <- try @SomeException (sendCloseWith conn code BS.empty)
  throwIO (WebSocketProtocolError reason)

sendCloseWith :: Connection -> CloseCode -> ByteString -> IO ()
sendCloseWith conn code reason = do
  let r = BS.take 123 reason
      payload = BS.pack
        [ fromIntegral (closeCodeWord code `shiftR` 8 .&. 0xFF)
        , fromIntegral (closeCodeWord code .&. 0xFF)
        ] <> r
  sendFrame conn Frame
    { frameFin     = True
    , frameRsv1    = False
    , frameRsv2    = False
    , frameRsv3    = False
    , frameOpcode  = OpClose
    , frameMask    = Nothing
    , framePayload = payload
    }

------------------------------------------------------------------------
-- Close codes
------------------------------------------------------------------------

-- | RFC 6455 \u00a77.4 status codes.  Symbolic for the common
-- ones; 'CloseCode' wraps the raw 'Word16' for everything else.
newtype CloseCode = CloseCode { closeCodeWord :: Word16 }
  deriving stock (Eq, Show)

normalClosure, goingAway, protocolError, unsupportedData,
  invalidPayload, policyViolation, messageTooBig,
  internalError :: CloseCode
normalClosure   = CloseCode 1000
goingAway       = CloseCode 1001
protocolError   = CloseCode 1002
unsupportedData = CloseCode 1003
invalidPayload  = CloseCode 1007
policyViolation = CloseCode 1008
messageTooBig   = CloseCode 1009
internalError   = CloseCode 1011
