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
    -- * Send helpers
  , sendBuilder
    -- * Re-exports
  , module Network.HTTP1.Internal.RecvBuffer
  , module Network.HTTP1.Internal.SendBuffer
  ) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word64)
import Network.Socket (Socket)
import qualified Network.Socket as NS

import qualified Wireform.Builder as B

import Network.HTTP1.Internal.RecvBuffer
import Network.HTTP1.Internal.SendBuffer
import Network.HTTP1.Parser (Framing (..), parseChunkSize)
import Network.HTTP1.Types (Body (..))

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
        Nothing -> pure Nothing
        Just lineBs ->
          case parseChunkSize lineBs of
            Left _ -> pure Nothing  -- protocol error; surface as EOS
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
        then pure Nothing
        else do
          let consumed = BS.length slice
              n' = n - fromIntegral consumed
          if n' == 0
            then do
              -- consume the trailing CRLF after the chunk data
              _ <- recvBufferRead (connRecv conn) (connSocket conn) 2
              writeIORef ref (ChunkPending 0)
            else writeIORef ref (ChunkPending n')
          pure (Just slice)

-- | After a 0-size chunk we have a (possibly empty) trailer section
-- terminated by a blank line. Each trailer is just a header field; we
-- read lines until we hit one of length zero (the blank line).
drainTrailers :: Connection -> IO ()
drainTrailers conn = loop
  where
    loop = do
      mLine <- recvBufferReadUntilCRLF (connRecv conn) (connSocket conn) 8192
      case mLine of
        Nothing -> pure ()
        Just bs
          | BS.null bs -> pure ()
          | otherwise  -> loop

-- | Discard the current body without delivering it to the application.
-- Required when a handler returns early on a keep-alive connection —
-- we have to consume the body so the next request lines up.
drainBody :: Body -> IO ()
drainBody BodyEmpty = pure ()
drainBody (BodyBytes _) = pure ()
drainBody (BodyStream producer) = loop
  where
    loop = do
      mc <- producer
      case mc of
        Nothing -> pure ()
        Just _ -> loop

-- | Send a 'B.Builder' on the connection's socket. Goes through the
-- pinned send buffer when the encoded size fits; falls back to
-- @NBS.sendAll@ otherwise.
sendBuilder :: Connection -> B.Builder -> IO ()
sendBuilder conn = sendBuilderAll (connSocket conn)
