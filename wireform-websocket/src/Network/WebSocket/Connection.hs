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
  , connectionRole
  , connectionReceive
  , connectionSend
  , closeConnection

    -- * Frame I\/O
  , receiveFrame
  , sendFrame
  , trySendFrame

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
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word16, Word64)

import Wireform.Parser.Driver
  ( InternalResult (..), runParserInternal )
import Wireform.Parser.Error (ParseError (..))
import qualified Wireform.Transport.Receive as TR
import Wireform.Transport.Receive (ReceiveTransport)
import Wireform.Transport.Send (SendTransport, sendBuilderDirect,
  sendShutdownWrite)
import qualified Wireform.Network as N

import Network.WebSocket.Frame

------------------------------------------------------------------------
-- Role
------------------------------------------------------------------------

-- | Which side of the connection we are.  Determines masking and
-- handshake direction.
data Role = Server | Client
  deriving stock (Eq, Show)

peerRole :: Role -> Role
peerRole Server = Client
peerRole Client = Server

------------------------------------------------------------------------
-- Connection
------------------------------------------------------------------------

data Connection = Connection
  { connDuplex :: !N.DuplexTransport
  , connRole   :: !Role
  , connLimit  :: !PayloadLimit
  , connRecvLock :: !(MVar ())
    -- ^ Serialise frame receives.  The wireform parser is
    -- inherently single-threaded per ring; the lock ensures a
    -- concurrent 'receiveFrame' from two threads doesn't trample
    -- each other's parse state.
  , connSendLock :: !(MVar ())
    -- ^ Serialise frame sends.  A 'Wireform.Transport.Send'
    -- reservation must be committed before the next reservation
    -- starts, so we lock the send side too.
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
  , connClosed   :: !(IORef Bool)
  }

-- | Build a 'Connection' from a duplex transport and the role this
-- side plays.  Takes ownership of the duplex; 'closeConnection'
-- releases it.
newConnection
  :: Role
  -> PayloadLimit
  -> N.DuplexTransport
  -> IO Connection
newConnection role lim duplex = do
  rLock     <- newMVar ()
  sLock     <- newMVar ()
  pos0      <- TR.receiveLoadHead (N.duplexReceive duplex)
  posRef    <- newIORef pos0
  closeSent <- newIORef False
  cref      <- newIORef False
  pure Connection
    { connDuplex    = duplex
    , connRole      = role
    , connLimit     = lim
    , connRecvLock  = rLock
    , connSendLock  = sLock
    , connRecvPos   = posRef
    , connSentClose = closeSent
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
-- the rings.  Callers that already issued an application-level
-- 'sendClose' need not call this immediately \u2014 keep reading
-- until 'receiveFrame' raises 'WebSocketPeerClosed' first to drain
-- a polite shutdown.
closeConnection :: Connection -> IO ()
closeConnection conn = mask_ $ do
  wasClosed <- atomicModifyIORef' (connClosed conn) (\b -> (True, b))
  unless wasClosed $ do
    _ <- try @SomeException $
      sendShutdownWrite (connectionSend conn)
    N.closeDuplexTransport (connDuplex conn)

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
receiveFrame :: Connection -> IO Frame
receiveFrame conn = withMVar (connRecvLock conn) $ \_ -> do
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
-- already validated the wire format).  No extension is currently
-- negotiated, so any non-zero RSV bit is a protocol error
-- (RFC 6455 \u00a75.2).  Each violation fails the connection with
-- a close frame carrying status 1002 (protocol error) before the
-- exception is raised.
checkOpcode :: Connection -> Frame -> IO ()
checkOpcode conn f = do
  when (frameRsv1 f || frameRsv2 f || frameRsv3 f) $
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
      withMVar (connSendLock conn) $ \_ ->
        sendBuilderDirect (connectionSend conn) (buildFrame f)

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
      m <- randomMask
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
