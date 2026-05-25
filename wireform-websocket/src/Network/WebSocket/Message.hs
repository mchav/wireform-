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
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16)

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
  -- A continuation frame with no message in progress is a
  -- protocol error.
  case op0 of
    OpContinuation -> failConnection conn protocolError
      "continuation frame with no message in progress"
    _ -> pure ()
  when (BS.length payload0 > lim) tooBig
  if frameFin firstFrame
    then finalise op0 payload0
    else do
      buf <- newIORef (BSB.byteString payload0)
      sizeRef <- newIORef (BS.length payload0)
      go buf sizeRef op0
  where
    tooBig = failConnection conn messageTooBig
      "message exceeds configured limit"
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
        other -> failConnection conn protocolError
            ("expected continuation, got " <> show other)
    finalise OpText   bs = case TE.decodeUtf8' bs of
      Right t -> pure (TextMessage t)
      Left _  -> failConnection conn invalidPayload
        "invalid UTF-8 in text message"
    finalise OpBinary bs = pure (BinaryMessage bs)
    finalise other    _  = failConnection conn protocolError
        ("unexpected data opcode " <> show other)

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
              -- RFC 6455 sec 7.4: a close payload with length 1
              -- is malformed (status code must be 2 bytes or 0).
              malformed = BS.length (framePayload f) == 1
              badCode  = case mCode of
                Nothing -> False
                Just c  -> not (validCloseCode c)
              badReason = case TE.decodeUtf8' reason of
                Right _ -> False
                Left  _ -> True
              echoCode
                | malformed || badCode || badReason = closeCodeWord protocolError
                | otherwise = case mCode of
                    Just c  -> c
                    Nothing -> closeCodeWord normalClosure
              echoPayload
                | malformed || badCode || badReason =
                    let !hi = fromIntegral (echoCode `shiftR` 8 .&. 0xFF) :: Word8
                        !lo = fromIntegral (echoCode .&. 0xFF) :: Word8
                    in BS.pack [hi, lo]
                | otherwise = framePayload f
          _ <- trySendFrame conn Frame
                 { frameFin     = True
                 , frameRsv1    = False
                 , frameRsv2    = False
                 , frameRsv3    = False
                 , frameOpcode  = OpClose
                 , frameMask    = Nothing
                 , framePayload = echoPayload
                 }
          let returnedCode
                | malformed || badCode || badReason = Just (closeCodeWord protocolError)
                | otherwise                          = mCode
          throwIO (WebSocketPeerClosed (CloseCode <$> returnedCode) reason)
        _ -> pure f

parseCloseBody :: ByteString -> (Maybe Word16, ByteString)
parseCloseBody bs
  | BS.length bs < 2 = (Nothing, BS.empty)
  | otherwise        =
      let !hi = fromIntegral (BS.index bs 0) :: Word16
          !lo = fromIntegral (BS.index bs 1) :: Word16
          !code = (hi * 256) + lo
      in (Just code, BS.drop 2 bs)

-- | RFC 6455 \u00a77.4: an endpoint MUST NOT send a close status
-- code that is one of the prohibited reserved values.
-- Specifically:
--
--   * 0\u20131000 are forbidden (the 4-digit space starts at 1000).
--   * 1004, 1005, 1006 are reserved for protocol-internal use.
--   * 1014\u20132999 are reserved (1014 was assigned for
--     bad-gateway in newer drafts but the RFC 6455 wording still
--     marks it).  The Autobahn suite treats 1015 and 1100\u20132999
--     as protocol errors.
--   * 5000+ are forbidden.
validCloseCode :: Word16 -> Bool
validCloseCode c
  | c <  1000             = False
  | c >= 1000 && c < 1004 = True
  | c >= 1004 && c < 1007 = False
  | c >= 1007 && c < 1012 = True
  | c >= 1012 && c < 3000 = False
  | c >= 3000 && c < 5000 = True
  | otherwise             = False

------------------------------------------------------------------------
-- Send
------------------------------------------------------------------------

-- | Send a text message as a single non-fragmented frame.
--
-- Hot-path: goes through 'sendDataFrame' which skips the 'Frame'
-- record allocation entirely.  Use 'sendFrame' directly for
-- fragmented messages or for setting explicit RSV bits (e.g.
-- when a permessage-deflate extension is in play).
sendTextMessage :: Connection -> Text -> IO ()
sendTextMessage conn t = sendDataFrame conn OpText (TE.encodeUtf8 t)

sendBinaryMessage :: Connection -> ByteString -> IO ()
sendBinaryMessage conn = sendDataFrame conn OpBinary

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

