{- | Connection-level state shared by client and server.

A 'Connection' wraps:

  * the underlying socket,
  * a pinned recv 'RecvBuffer' (zero-allocation @recv()@),
  * a pinned send 'SendBuffer' (zero-allocation encode + @send()@),
  * a closed-flag so finalizers don't double-close.

The body-reading helpers ('readBody', 'drainBody') run the framing
state machine inferred by 'Network.HTTP1.Parser.requestFraming' \/
'responseFraming' and feed the next request \/ response on the wire as
soon as the previous body is consumed (HTTP\/1.1 keep-alive +
pipelining).
-}
module Network.HTTP1.Connection
  ( Connection
  , newConnection
  , connectionSocket
  , connectionRecvBuffer
  , connectionSendBuffer
  , closeConnection
    -- * Framing-aware body
  , readBody
  , drainBody
  , ProtocolException (..)
    -- * Send helpers
  , sendBuilder
    -- * Re-exports
  , module Network.HTTP1.Internal.RecvBuffer
  , module Network.HTTP1.Internal.SendBuffer
  ) where

import Control.Exception (Exception, SomeException, throwIO, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import qualified Network.Socket as NS

import qualified Wireform.Builder as B

import Network.HTTP1.Internal.RecvBuffer
import Network.HTTP1.Internal.SendBuffer
import Network.HTTP1.Parser (Framing (..), ParseError (..), parseChunkSize)
import Network.HTTP1.Types (Body (..))

-- | Thrown by a streaming-body producer when the wire bytes violate
-- the framing the parser inferred (e.g. malformed chunk-size line,
-- premature EOF in chunked TE, oversized chunk). The server catches
-- this around the user 'Handler' and emits a 400 \/ 502 \/ … response
-- before closing the connection.
newtype ProtocolException = ProtocolException ParseError
  deriving stock (Eq, Show, Generic)

instance Exception ProtocolException

data Connection = Connection
  { connSocket :: !Socket
  , connRecv   :: !RecvBuffer
  , connSend   :: !SendBuffer
  , connClosed :: !(IORef Bool)
  }

newConnection :: Socket -> IO Connection
newConnection sock = Connection sock <$> newRecvBuffer <*> newSendBuffer <*> newIORef False

connectionSocket :: Connection -> Socket
connectionSocket = connSocket

connectionRecvBuffer :: Connection -> RecvBuffer
connectionRecvBuffer = connRecv

connectionSendBuffer :: Connection -> SendBuffer
connectionSendBuffer = connSend

closeConnection :: Connection -> IO ()
closeConnection conn = do
  wasClosed <- atomicModifyIORef' (connClosed conn) (\c -> (True, c))
  if wasClosed
    then pure ()
    else do
      _ <- try @SomeException (NS.close (connSocket conn))
      pure ()

-- | Build a streaming-body producer for the framing the parser told us.
--
-- The producer is a closure over a mutable @remaining@ ref so it can
-- be handed to user code as a @Body@. End of stream returns @Nothing@.
readBody :: Connection -> Framing -> IO Body
readBody _ NoBody = pure BodyEmpty
readBody _ (ContentLength 0) = pure BodyEmpty
readBody conn (ContentLength n) = do
  remRef <- newIORef n
  pure $ BodyStream $ do
    rem' <- readIORef remRef
    if rem' == 0
      then pure Nothing
      else do
        let want = min rem' 16384
        chunk <- recvBufferReadAtMost (connRecv conn) (connSocket conn) (fromIntegral want)
        if BS.null chunk
          then pure Nothing  -- premature EOF; caller decides if that's an error
          else do
            writeIORef remRef (rem' - fromIntegral (BS.length chunk))
            pure (Just chunk)
readBody conn Chunked = do
  stateRef <- newIORef (ChunkPending 0)
  pure $ BodyStream (readChunkedStep conn stateRef)
readBody conn CloseDelimited = pure $ BodyStream $ do
  chunk <- recvBufferReadAtMost (connRecv conn) (connSocket conn) 16384
  if BS.null chunk
    then pure Nothing
    else pure (Just chunk)

-- | State of an in-flight chunked body read.
data ChunkState
  = ChunkPending !Word64
    -- ^ N bytes remaining in the current chunk; 0 = need new header.
  | ChunkDone
    -- ^ Saw the 0-size terminator; subsequent calls return Nothing.

-- | One step of chunked decoding: emit the next slice of body bytes
-- (or Nothing on terminator). Pulls more from the recv buffer as needed.
--
-- Critically, all reads here go through 'recvBufferReadUntilCRLF' /
-- 'recvBufferRead' / 'recvBufferReadAtMost', which only consume from
-- the ring buffer what they actually return — there's no over-read,
-- so chunk bodies arrive cleanly after their size lines.
readChunkedStep :: Connection -> IORef ChunkState -> IO (Maybe ByteString)
readChunkedStep conn ref = do
  st <- readIORef ref
  case st of
    ChunkDone -> pure Nothing
    ChunkPending 0 -> do
      mLine <- recvBufferReadUntilCRLF (connRecv conn) (connSocket conn) 4096
      case mLine of
        Nothing -> throwIO (ProtocolException ParseUnexpectedEof)
        Just lineBs ->
          case parseChunkSize lineBs of
            Left e -> throwIO (ProtocolException e)
            Right 0 -> do
              -- After the 0-size chunk we must consume the trailer
              -- section, terminated by a blank line. The size-line
              -- CRLF has already been consumed.
              drainTrailers conn
              writeIORef ref ChunkDone
              pure Nothing
            Right sz -> do
              writeIORef ref (ChunkPending sz)
              readChunkedStep conn ref
    ChunkPending n -> do
      let want = min n 16384
      slice <- recvBufferReadAtMost (connRecv conn) (connSocket conn) (fromIntegral want)
      if BS.null slice
        then throwIO (ProtocolException ParseUnexpectedEof)
        else do
          let consumed = BS.length slice
              n' = n - fromIntegral consumed
          if n' == 0
            then do
              -- After the chunk data the wire MUST carry a CRLF before
              -- the next size line. Read it and verify; reject if not.
              term <- recvBufferRead (connRecv conn) (connSocket conn) 2
              if BS.length term < 2 || BS.index term 0 /= 0x0d || BS.index term 1 /= 0x0a
                then throwIO (ProtocolException ParseBadChunkHeader)
                else writeIORef ref (ChunkPending 0)
            else writeIORef ref (ChunkPending n')
          pure (Just slice)

-- | After a 0-size chunk we have a (possibly empty) trailer section
-- terminated by a blank line. Each trailer is just a header field; we
-- read lines until we hit one of length zero (the blank line).
--
-- Bare-LF line terminators inside the trailer section are forbidden
-- (RFC 9112 § 2.2; a smuggling vector); 'recvBufferReadUntilCRLF'
-- returns 'Nothing' on EOF without seeing CRLF, which we turn into a
-- protocol error so the caller can respond 400 + close.
drainTrailers :: Connection -> IO ()
drainTrailers conn = loop
  where
    loop = do
      mLine <- recvBufferReadUntilCRLFStrict
                 (connRecv conn) (connSocket conn) 8192
      case mLine of
        Nothing -> throwIO (ProtocolException ParseInvalidHeaderValue)
        Just (Left ()) ->
          -- Bare LF in the trailer section. RFC 9112 § 2.2 lets us
          -- accept it; we choose to reject because the same bytes
          -- desync proxies that don't (HAProxy CVE-2023-25725 style).
          throwIO (ProtocolException ParseInvalidHeaderValue)
        Just (Right bs)
          | BS.null bs -> pure ()
          | BS.any badByte bs ->
              throwIO (ProtocolException ParseInvalidHeaderValue)
          | otherwise -> loop
    badByte b = b == 0x0a || b == 0x00 || b == 0x0d

-- | Discard the current body without delivering it to the application.
-- Required when a handler returns early on a keep-alive connection —
-- we have to consume the body so the next request lines up.
drainBody :: Body -> IO ()
drainBody = \case
  BodyEmpty -> pure ()
  BodyBytes _ -> pure ()
  BodyPreEncoded _ -> pure ()
  BodyStream producer -> loop producer
  where
    loop producer = do
      mc <- producer
      case mc of
        Nothing -> pure ()
        Just _ -> loop producer

-- | Send a 'B.Builder' on the connection's socket. Goes through the
-- pinned send buffer when the encoded size fits; falls back to
-- @NBS.sendAll@ otherwise.
sendBuilder :: Connection -> B.Builder -> IO ()
sendBuilder conn = sendBuilderAll (connSocket conn)
