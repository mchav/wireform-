{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Higher-level message API: text \/ binary messages reassembled
across continuation frames, with built-in handling for the
control frames callers usually want autopiloted (ping \u2192 pong,
peer-close \u2192 echo + raise).

A 'Message' is either text (UTF-8 validated up front) or binary
\u2014 reassembled from the underlying frame sequence per
RFC 6455 \u00a75.4.  The receive loop transparently:

* replies to peer pings with the corresponding pong,
* records the peer's close frame and re-raises it to the caller
  as 'WebSocketPeerClosed' after echoing a courtesy close back,
* enforces the @MessageLimit@ across a fragmented message (sum
  of payload bytes; rejects with 'WebSocketProtocolError' if the
  client tries to smuggle a huge upload past the per-frame
  'PayloadLimit' by chunking).
-}
module Network.WebSocket.Message
  ( -- * Messages
    Message (..)
  , MessageLimit (..)
  , defaultMessageLimit

    -- * Receive
  , receiveMessage
  , receiveDataMessage

    -- * Send
  , sendTextMessage
  , sendBinaryMessage

    -- * Loops
  , forEachMessage
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word16)

import Network.WebSocket.Connection
import Network.WebSocket.Frame

------------------------------------------------------------------------
-- Message
------------------------------------------------------------------------

data Message
  = TextMessage   !Text
  | BinaryMessage !ByteString
  deriving stock (Eq, Show)

newtype MessageLimit = MessageLimit { unMessageLimit :: Int }
  deriving stock (Eq, Show)

-- | 32 MiB.  Sum-of-fragments cap (RFC 6455 doesn't define one;
-- this is a safety net for the autopilot).
defaultMessageLimit :: MessageLimit
defaultMessageLimit = MessageLimit (32 * 1024 * 1024)

------------------------------------------------------------------------
-- Receive
------------------------------------------------------------------------

-- | Read one full data message.  Drives the connection's
-- 'receiveFrame' loop:
--
-- * @OpText@ \/ @OpBinary@ starts a new message; subsequent
--   @OpContinuation@ frames extend it until @frameFin = True@.
-- * @OpPing@ is auto-replied with a matching @OpPong@.
-- * @OpPong@ is dropped silently (callers that want it can drop
--   down to 'receiveFrame').
-- * @OpClose@ is echoed back with the same status code, then
--   raised as 'WebSocketPeerClosed'.
-- * Non-control opcode arriving where a continuation is expected
--   (or vice versa) is a protocol error.
receiveMessage :: Connection -> MessageLimit -> IO Message
receiveMessage = receiveDataMessage

-- | Same as 'receiveMessage'; kept around so callers that only
-- want data frames have a name to import.
receiveDataMessage :: Connection -> MessageLimit -> IO Message
receiveDataMessage conn (MessageLimit lim) = do
  firstFrame <- nextDataFrame conn
  let !op0 = frameOpcode firstFrame
      !payload0 = framePayload firstFrame
  when (BS.length payload0 > lim) tooBig
  if frameFin firstFrame
    then finalise op0 payload0
    else do
      buf <- newIORef (BSB.byteString payload0)
      sizeRef <- newIORef (BS.length payload0)
      go buf sizeRef op0
  where
    tooBig = throwIO (WebSocketProtocolError "message exceeds limit")
    go !buf !sizeRef !op0 = do
      f <- nextDataFrame conn
      case frameOpcode f of
        OpContinuation -> do
          let !plen = BS.length (framePayload f)
          curSize <- readIORef sizeRef
          let !newSize = curSize + plen
          when (newSize > lim) tooBig
          writeIORef sizeRef newSize
          modifyIORef' buf (<> BSB.byteString (framePayload f))
          if frameFin f
            then do
              b <- readIORef buf
              finalise op0 (BSL.toStrict (BSB.toLazyByteString b))
            else go buf sizeRef op0
        other -> throwIO (WebSocketProtocolError
            ("expected continuation, got " <> show other))
    finalise OpText   bs = case TE.decodeUtf8' bs of
      Right t -> pure (TextMessage t)
      Left e  -> throwIO (WebSocketProtocolError
        ("invalid UTF-8 in text message: " <> show e))
    finalise OpBinary bs = pure (BinaryMessage bs)
    finalise other    _  = throwIO (WebSocketProtocolError
        ("unexpected data opcode " <> show other))

-- | Read frames until a data frame appears, auto-handling ping
-- and close along the way.
nextDataFrame :: Connection -> IO Frame
nextDataFrame conn = loop
  where
    loop = do
      f <- receiveFrame conn
      case frameOpcode f of
        OpPing -> do
          sendPong conn (framePayload f)
          loop
        OpPong -> loop
        OpClose -> do
          let (mCode, reason) = parseCloseBody (framePayload f)
              code = maybe normalClosure CloseCode mCode
          -- Echo (best-effort; ignore failure if peer already
          -- went away).
          _ <- trySendFrame conn Frame
                 { frameFin     = True
                 , frameRsv1    = False
                 , frameRsv2    = False
                 , frameRsv3    = False
                 , frameOpcode  = OpClose
                 , frameMask    = Nothing
                 , framePayload = framePayload f
                 }
          throwIO (WebSocketPeerClosed (Just code) reason)
        _ -> pure f

parseCloseBody :: ByteString -> (Maybe Word16, ByteString)
parseCloseBody bs
  | BS.length bs < 2 = (Nothing, BS.empty)
  | otherwise        =
      let !hi = fromIntegral (BS.index bs 0) :: Word16
          !lo = fromIntegral (BS.index bs 1) :: Word16
          !code = (hi * 256) + lo
      in (Just code, BS.drop 2 bs)

------------------------------------------------------------------------
-- Send
------------------------------------------------------------------------

-- | Send a text message as a single frame.  Use 'sendFrame'
-- directly for fragmented or interleaved control frames.
sendTextMessage :: Connection -> Text -> IO ()
sendTextMessage conn t = sendFrame conn Frame
  { frameFin     = True
  , frameRsv1    = False
  , frameRsv2    = False
  , frameRsv3    = False
  , frameOpcode  = OpText
  , frameMask    = Nothing
  , framePayload = TE.encodeUtf8 t
  }

sendBinaryMessage :: Connection -> ByteString -> IO ()
sendBinaryMessage conn bs = sendFrame conn Frame
  { frameFin     = True
  , frameRsv1    = False
  , frameRsv2    = False
  , frameRsv3    = False
  , frameOpcode  = OpBinary
  , frameMask    = Nothing
  , framePayload = bs
  }

------------------------------------------------------------------------
-- Loops
------------------------------------------------------------------------

-- | Run @handler@ for every message that arrives, until the peer
-- closes or the handler throws.
forEachMessage
  :: Connection
  -> MessageLimit
  -> (Message -> IO ())
  -> IO ()
forEachMessage conn lim handler = loop
  where
    loop = do
      m <- receiveMessage conn lim
      handler m
      loop

-- Silence -Wunused-imports of T (used only via TE).
_tUnused :: Text -> Int
_tUnused = T.length
