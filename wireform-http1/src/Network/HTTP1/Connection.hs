{- | Connection-level state shared by client and server.

A 'Connection' wraps:

  * the underlying byte 'Transport' (a raw socket by default; can also
    be a TLS context, an in-memory test sink, …),
  * a pinned recv 'RecvBuffer' (zero-allocation @recv()@),
  * a pinned send 'SendBuffer' (zero-allocation encode + send),
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
  , newConnectionFromTransport
  , connectionTransport
  , connectionSocket
  , connectionRecvBuffer
  , connectionSendBuffer
  , closeConnection
    -- * Framing-aware body
  , readBody
  , readBodyAndTrailers
  , drainBody
  , ProtocolException (..)
    -- * Send helpers
  , sendBuilder
    -- * Re-exports
  , module Network.HTTP1.Internal.RecvBuffer
  , module Network.HTTP1.Internal.SendBuffer
  , module Network.HTTP1.Transport
  ) where

import Control.Concurrent.MVar
  (MVar, newEmptyMVar, newMVar, readMVar, tryPutMVar)
import Control.Exception (Exception, SomeException, throwIO, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import Network.HTTP1.Headers (Header)

import qualified Wireform.Builder as B

import Network.HTTP1.Internal.RecvBuffer
import Network.HTTP1.Internal.SendBuffer
import Network.HTTP1.Parser (Framing (..), ParseError (..), parseChunkSize)
import Network.HTTP1.Transport
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
  { connTransport :: !Transport
  , connRecv      :: !RecvBuffer
  , connSend      :: !SendBuffer
  , connClosed    :: !(IORef Bool)
  }

-- | Build a 'Connection' from a raw 'Socket'.  The socket is wrapped
-- with 'socketTransport'.
newConnection :: Socket -> IO Connection
newConnection sock = newConnectionFromTransport (socketTransport sock)

-- | Build a 'Connection' from an arbitrary 'Transport'.  This is the
-- entry point used by the TLS bridge and any other non-socket transport.
newConnectionFromTransport :: Transport -> IO Connection
newConnectionFromTransport t =
  Connection t <$> newRecvBuffer <*> newSendBuffer <*> newIORef False

connectionTransport :: Connection -> Transport
connectionTransport = connTransport

-- | The underlying socket, if this connection is socket-backed.  TLS
-- and other non-socket transports return 'Nothing'; callers using the
-- @sendfile(2)@ fast path branch on this.
connectionSocket :: Connection -> Maybe Socket
connectionSocket = tSocket . connTransport

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
      _ <- try @SomeException (tClose (connTransport conn))
      pure ()

-- | Build a streaming-body producer for the framing the parser told
-- us about.  Discards any trailer block that comes with a chunked
-- body; use 'readBodyAndTrailers' if you want to keep them.
readBody :: Connection -> Framing -> IO Body
readBody conn framing = fst <$> readBodyAndTrailers conn framing

-- | Build a streaming-body producer plus a blocking action that
-- returns the trailer block (or @[]@ when the framing carries no
-- trailers / they were empty).  The trailers action blocks until
-- the body has been fully drained; @drainBody@ also pulls them
-- through.
readBodyAndTrailers
  :: Connection -> Framing -> IO (Body, IO [Header])
readBodyAndTrailers _ NoBody = do
  -- No body, no trailers; pre-fill so a reader doesn't block.
  mv <- newMVar []
  pure (BodyEmpty, readMVar mv)
readBodyAndTrailers _ (ContentLength 0) = do
  mv <- newMVar []
  pure (BodyEmpty, readMVar mv)
readBodyAndTrailers conn (ContentLength n) = do
  remRef <- newIORef n
  trailersMV <- newEmptyMVar
  let producer = do
        rem' <- readIORef remRef
        if rem' == 0
          then do
            _ <- tryPutMVar trailersMV []
            pure Nothing
          else do
            let want = min rem' 16384
            chunk <- recvBufferReadAtMost
                       (connRecv conn) (tRecvBuf (connTransport conn))
                       (fromIntegral want)
            if BS.null chunk
              then do
                _ <- tryPutMVar trailersMV []
                pure Nothing  -- premature EOF
              else do
                let newRem = rem' - fromIntegral (BS.length chunk)
                writeIORef remRef newRem
                when' (newRem == 0) $ do
                  _ <- tryPutMVar trailersMV []
                  pure ()
                pure (Just chunk)
  pure (BodyStream producer, readMVar trailersMV)
readBodyAndTrailers conn Chunked = do
  stateRef <- newIORef (ChunkPending 0)
  trailersMV <- newEmptyMVar
  let producer = readChunkedStep conn stateRef trailersMV
  pure (BodyStream producer, readMVar trailersMV)
readBodyAndTrailers conn CloseDelimited = do
  trailersMV <- newEmptyMVar
  let producer = do
        chunk <- recvBufferReadAtMost
                   (connRecv conn) (tRecvBuf (connTransport conn)) 16384
        if BS.null chunk
          then do
            _ <- tryPutMVar trailersMV []
            pure Nothing
          else pure (Just chunk)
  pure (BodyStream producer, readMVar trailersMV)

-- | Local 'when' helper that doesn't pull in Control.Monad.
when' :: Bool -> IO () -> IO ()
when' True m = m
when' False _ = pure ()

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
readChunkedStep
  :: Connection
  -> IORef ChunkState
  -> MVar [Header]
  -> IO (Maybe ByteString)
readChunkedStep conn ref trailersMV = do
  st <- readIORef ref
  case st of
    ChunkDone -> pure Nothing
    ChunkPending 0 -> do
      mLine <- recvBufferReadUntilCRLF (connRecv conn) recv 4096
      case mLine of
        Nothing -> throwIO (ProtocolException ParseUnexpectedEof)
        Just lineBs ->
          case parseChunkSize lineBs of
            Left e -> throwIO (ProtocolException e)
            Right 0 -> do
              -- After the 0-size chunk we read the (possibly empty)
              -- trailer section, terminated by a blank line, and
              -- park the parsed fields on the trailers MVar.
              trs <- readTrailers conn
              _ <- tryPutMVar trailersMV trs
              writeIORef ref ChunkDone
              pure Nothing
            Right sz -> do
              writeIORef ref (ChunkPending sz)
              readChunkedStep conn ref trailersMV
    ChunkPending n -> do
      let want = min n 16384
      slice <- recvBufferReadAtMost (connRecv conn) recv (fromIntegral want)
      if BS.null slice
        then throwIO (ProtocolException ParseUnexpectedEof)
        else do
          let consumed = BS.length slice
              n' = n - fromIntegral consumed
          if n' == 0
            then do
              -- After the chunk data the wire MUST carry a CRLF before
              -- the next size line. Read it and verify; reject if not.
              term <- recvBufferRead (connRecv conn) recv 2
              if BS.length term < 2 || BS.index term 0 /= 0x0d || BS.index term 1 /= 0x0a
                then throwIO (ProtocolException ParseBadChunkHeader)
                else writeIORef ref (ChunkPending 0)
            else writeIORef ref (ChunkPending n')
          pure (Just slice)
  where
    recv = tRecvBuf (connTransport conn)

-- | After a 0-size chunk we have a (possibly empty) trailer section
-- terminated by a blank line.  Each trailer is just a header field;
-- we read lines until one of length zero (the blank line) and parse
-- each into a (name, value) pair.
--
-- Bare-LF line terminators inside the trailer section are forbidden
-- (RFC 9112 § 2.2; a smuggling vector); they are turned into a
-- protocol error so the caller can respond 400 + close.
readTrailers :: Connection -> IO [Header]
readTrailers conn = go []
  where
    recv = tRecvBuf (connTransport conn)
    go acc = do
      mLine <- recvBufferReadUntilCRLFStrict
                 (connRecv conn) recv 8192
      case mLine of
        Nothing -> throwIO (ProtocolException ParseInvalidHeaderValue)
        Just (Left ()) ->
          throwIO (ProtocolException ParseInvalidHeaderValue)
        Just (Right bs)
          | BS.null bs -> pure (reverse acc)
          | BS.any badByte bs ->
              throwIO (ProtocolException ParseInvalidHeaderValue)
          | otherwise -> case parseTrailerLine bs of
              Nothing  -> throwIO (ProtocolException ParseInvalidHeaderValue)
              Just hdr -> go (hdr : acc)
    badByte b = b == 0x0a || b == 0x00 || b == 0x0d

-- | Parse a single @name: value@ trailer line.  The same syntax
-- the request parser uses for header fields, minus
-- whitespace-folded continuations (RFC 9112 § 5.2 forbids those
-- here anyway).  Returns 'Nothing' on a malformed line.
parseTrailerLine :: ByteString -> Maybe Header
parseTrailerLine bs = do
  let (rawName, rest0) = BS.break (== 0x3A {- ':' -}) bs
  case BS.uncons rest0 of
    Just (0x3A, rest1) ->
      let value = stripOWS rest1
      in if BS.null rawName then Nothing else Just (rawName, value)
    _ -> Nothing
  where
    stripOWS = BS.dropWhile isOWS . BS.reverse . BS.dropWhile isOWS . BS.reverse
    isOWS b = b == 0x20 || b == 0x09

-- | Discard the current body without delivering it to the application.
-- Required when a handler returns early on a keep-alive connection —
-- we have to consume the body so the next request lines up.
drainBody :: Body -> IO ()
drainBody = \case
  BodyEmpty -> pure ()
  BodyBytes _ -> pure ()
  BodyPreEncoded _ -> pure ()
  BodyFile _ -> pure ()
  BodyStream producer -> loop producer
  where
    loop producer = do
      mc <- producer
      case mc of
        Nothing -> pure ()
        Just _ -> loop producer

-- | Send a 'B.Builder' on the connection's transport. Goes through
-- the connection's @sendAll@ callback (raw socket or TLS write).
sendBuilder :: Connection -> B.Builder -> IO ()
sendBuilder conn = sendBuilderAll (tSendAll (connTransport conn))
