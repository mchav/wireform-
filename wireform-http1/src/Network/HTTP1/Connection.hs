{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Connection-level state shared by client and server.

A 'Connection' wraps:

  * the underlying byte 'Transport' (a raw socket by default; can also
    be a TLS context, an in-memory test sink, …),
  * a magic-ring 'Wireform.Transport.Transport' that pulls bytes
    straight from @recv@ into a double-mapped ring (zero-allocation
    receive path; replaces the previous pinned 'RecvBuffer'),
  * a pinned send 'SendBuffer' (zero-allocation encode + send),
  * a cursor 'IORef' that tracks how far the recv side has consumed
    so successive @read*@ helpers can chain without round-tripping
    through 'transportLoadHead' between iterations,
  * a closed-flag so finalizers don't double-close.

The body-reading helpers ('readBody', 'drainBody') run the framing
state machine inferred by 'Network.HTTP1.Parser.requestFraming' \/
'responseFraming' and feed the next request \/ response on the wire
as soon as the previous body is consumed (HTTP\/1.1 keep-alive +
pipelining).
-}
module Network.HTTP1.Connection
  ( Connection
  , newConnection
  , newConnectionFromTransport
  , newConnectionFromTransportWithRingSize
  , defaultRingSize
  , connectionTransport
  , connectionSocket
  , connectionRingTransport
  , connectionCursor
  , connectionReadCursor
  , connectionAdvanceCursor
  , connectionSendBuffer
  , closeConnection
    -- * Head readers (zero-copy, SIMD on the ring)
  , readRequestHead
  , readResponseHead
    -- * Framing-aware body
  , readBody
  , readBodyAndTrailers
  , drainBody
  , ProtocolException (..)
    -- * Send helpers
  , sendBuilder
    -- * Re-exports
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
import Wireform.Network (newRecvBufTransport)
import qualified Wireform.Transport as WT
import qualified Wireform.Transport.Config as WC
import Wireform.Transport.Config (defaultTransportConfig)

import Network.HTTP1.Internal.SendBuffer
import Network.HTTP1.Method (Method)
import Network.HTTP1.Parser (Framing (..), ParseError (..))
import qualified Network.HTTP1.StreamingReader as SR
import Network.HTTP1.Transport
import Network.HTTP1.Types
  ( Body (..)
  , Request
  , Response
  )

-- | Thrown by a streaming-body producer when the wire bytes violate
-- the framing the parser inferred (e.g. malformed chunk-size line,
-- premature EOF in chunked TE, oversized chunk). The server catches
-- this around the user 'Handler' and emits a 400 \/ 502 \/ … response
-- before closing the connection.
newtype ProtocolException = ProtocolException ParseError
  deriving stock (Eq, Show, Generic)

instance Exception ProtocolException

data Connection = Connection
  { connTransport    :: !Transport
  , connRingTransport :: !WT.Transport
    -- ^ Magic-ring transport plumbed onto @tRecvBuf connTransport@.
    -- Owns its own 'Wireform.Ring.Internal.MagicRing' which is
    -- destroyed on 'closeConnection'.
  , connCursor       :: !(IORef Word64)
    -- ^ Position in the ring past the last byte consumed by any
    -- of the @read*@ helpers.  Chained through them so we don't
    -- pay a 'transportLoadHead' round-trip per call.
  , connSend         :: !SendBuffer
  , connClosed       :: !(IORef Bool)
  }

-- | Build a 'Connection' from a raw 'Socket'.  The socket is
-- wrapped with 'socketTransport'.
newConnection :: Socket -> IO Connection
newConnection sock = newConnectionFromTransport (socketTransport sock)

-- | Default magic-ring size: 256 KiB.  Easily fits the largest
-- header block (h2o caps requests at 32 KiB by default) plus
-- several chunked-TE body chunks (16 KiB cap each) plus some
-- breathing room, while keeping per-connection virtual memory
-- modest.  Set explicitly via
-- 'newConnectionFromTransportWithRingSize' for connections that
-- need a larger ring (very-large @Content-Length@ bodies that the
-- application wants to read in one shot).
defaultRingSize :: Int
defaultRingSize = 256 * 1024

-- | Build a 'Connection' from an arbitrary 'Transport'.  This is
-- the entry point used by the TLS bridge and any other non-socket
-- transport.  The transport's 'tRecvBuf' is plumbed onto a
-- magic-ring 'Wireform.Transport.Transport'; 'tSendAll' /
-- 'tSendMany' / 'tClose' / 'tSocket' continue to serve the send +
-- metadata side as before.
--
-- Uses 'defaultRingSize' for the magic ring.
newConnectionFromTransport :: Transport -> IO Connection
newConnectionFromTransport = newConnectionFromTransportWithRingSize defaultRingSize

-- | Like 'newConnectionFromTransport' but lets the caller pick the
-- magic-ring size.  See 'defaultRingSize' for the reasoning.
newConnectionFromTransportWithRingSize :: Int -> Transport -> IO Connection
newConnectionFromTransportWithRingSize ringSz t = do
  let !cfg = defaultTransportConfig { WC.ringSizeHint = ringSz }
  ringT  <- newRecvBufTransport cfg (tRecvBuf t)
  cursor <- newIORef 0
  sb     <- newSendBuffer
  closed <- newIORef False
  pure Connection
    { connTransport     = t
    , connRingTransport = ringT
    , connCursor        = cursor
    , connSend          = sb
    , connClosed        = closed
    }

connectionTransport :: Connection -> Transport
connectionTransport = connTransport

-- | The underlying socket, if this connection is socket-backed.
-- TLS and other non-socket transports return 'Nothing'; callers
-- using the @sendfile(2)@ fast path branch on this.
connectionSocket :: Connection -> Maybe Socket
connectionSocket = tSocket . connTransport

-- | The magic-ring transport plumbed onto the connection's recv
-- side.  Exposed for callers that want to drive a custom streaming
-- parser against the ring (e.g. a benchmark, a long-lived parsing
-- loop that wants 'Wireform.Transport.Transport' directly).  Most
-- code should use 'readRequestHead' / 'readResponseHead' /
-- 'readBody' instead.
connectionRingTransport :: Connection -> WT.Transport
connectionRingTransport = connRingTransport

-- | The 'IORef' tracking how far the recv path has consumed.
-- Exposed alongside 'connectionRingTransport' so a custom parser
-- loop can chain reads.
connectionCursor :: Connection -> IORef Word64
connectionCursor = connCursor

-- | Read the cursor.
connectionReadCursor :: Connection -> IO Word64
connectionReadCursor = readIORef . connCursor

-- | Bump the cursor to the supplied position and tell the ring
-- transport it can recycle bytes up to that point.
connectionAdvanceCursor :: Connection -> Word64 -> IO ()
connectionAdvanceCursor conn pos = do
  writeIORef (connCursor conn) pos
  WT.transportAdvanceTail (connRingTransport conn) pos

connectionSendBuffer :: Connection -> SendBuffer
connectionSendBuffer = connSend

closeConnection :: Connection -> IO ()
closeConnection conn = do
  wasClosed <- atomicModifyIORef' (connClosed conn) (\c -> (True, c))
  if wasClosed
    then pure ()
    else do
      -- Order matters: close the magic-ring transport first (frees
      -- its mmap), then close the underlying socket / TLS context.
      _ <- try @SomeException (WT.transportClose (connRingTransport conn))
      _ <- try @SomeException (tClose (connTransport conn))
      pure ()

------------------------------------------------------------------------
-- Head readers
------------------------------------------------------------------------

-- | Read one request head off the wire (request line + header block,
-- terminated by @CRLFCRLF@).  Walks the magic ring directly with
-- the SIMD CRLFCRLF scanner + delegates to the classic
-- 'Network.HTTP1.Parser.parseRequest' for the structural parse.
readRequestHead
  :: Connection
  -> IO (Either SR.ReadError (Request, Framing))
readRequestHead conn = do
  pos <- readIORef (connCursor conn)
  r   <- SR.readRequestHeadFrom (connRingTransport conn) pos
  case r of
    Right (ok, newPos) -> do
      writeIORef (connCursor conn) newPos
      pure (Right ok)
    Left e -> pure (Left e)

-- | Read one response head off the wire.  Takes the request method
-- so the framing inference applies the HEAD \/ CONNECT special
-- cases.
readResponseHead
  :: Connection
  -> Method
  -> IO (Either SR.ReadError (Response, Framing))
readResponseHead conn reqMethod = do
  pos <- readIORef (connCursor conn)
  r   <- SR.readResponseHeadFrom (connRingTransport conn) pos reqMethod
  case r of
    Right (ok, newPos) -> do
      writeIORef (connCursor conn) newPos
      pure (Right ok)
    Left e -> pure (Left e)

------------------------------------------------------------------------
-- Body
------------------------------------------------------------------------

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
--
-- == Body chunk lifetime
--
-- The 'ByteString' chunks the 'BodyStream' producer yields are
-- zero-copy slices into the connection's magic ring.  They become
-- dangling pointers as soon as the producer is called again (the
-- next call advances the ring tail, which can recycle the bytes
-- the slice referenced).  Applications that retain a chunk past
-- the next producer call MUST 'BS.copy' first.
readBodyAndTrailers
  :: Connection -> Framing -> IO (Body, IO [Header])
readBodyAndTrailers _ NoBody = do
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
            let want = fromIntegral (min rem' 16384) :: Int
            pos <- readIORef (connCursor conn)
            r <- SR.readUpTo (connRingTransport conn) pos want
            case r of
              Left _ -> do
                _ <- tryPutMVar trailersMV []
                pure Nothing  -- premature EOF
              Right (chunk, newPos) -> do
                -- Force a heap copy so the chunk stays valid past
                -- 'connectionAdvanceCursor' (which releases the
                -- ring bytes for the recv path to reuse).
                let !chunkCopy = BS.copy chunk
                connectionAdvanceCursor conn newPos
                let newRem = rem' - fromIntegral (BS.length chunkCopy)
                writeIORef remRef newRem
                when' (newRem == 0) $ do
                  _ <- tryPutMVar trailersMV []
                  pure ()
                pure (Just chunkCopy)
  pure (BodyStream producer, readMVar trailersMV)
readBodyAndTrailers conn Chunked = do
  stateRef <- newIORef (ChunkPending 0)
  trailersMV <- newEmptyMVar
  let producer = readChunkedStep conn stateRef trailersMV
  pure (BodyStream producer, readMVar trailersMV)
readBodyAndTrailers conn CloseDelimited = do
  trailersMV <- newEmptyMVar
  let producer = do
        pos <- readIORef (connCursor conn)
        r <- SR.readUpTo (connRingTransport conn) pos 16384
        case r of
          Left _ -> do
            _ <- tryPutMVar trailersMV []
            pure Nothing
          Right (chunk, newPos) -> do
            let !chunkCopy = BS.copy chunk
            connectionAdvanceCursor conn newPos
            pure (Just chunkCopy)
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
-- (or Nothing on terminator).  Reads the chunk-size line via the
-- SIMD CRLF scanner, then pulls exactly @sz@ chunk-data bytes off
-- the ring, then verifies the trailing CRLF before looping.
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
      pos <- readIORef (connCursor conn)
      lineE <- SR.readChunkSizeLineFrom (connRingTransport conn) pos
      case lineE of
        Left e -> throwIO (ProtocolException (readErrorToParseError e))
        Right (sz, newPos) -> do
          writeIORef (connCursor conn) newPos
          if sz == 0
            then do
              trs <- readTrailers conn
              _ <- tryPutMVar trailersMV trs
              writeIORef ref ChunkDone
              pure Nothing
            else do
              writeIORef ref (ChunkPending sz)
              readChunkedStep conn ref trailersMV
    ChunkPending n -> do
      let want = fromIntegral (min n 16384) :: Int
      pos <- readIORef (connCursor conn)
      r <- SR.readUpTo (connRingTransport conn) pos want
      case r of
        Left _ -> throwIO (ProtocolException ParseUnexpectedEof)
        Right (slice, newPos) -> do
          let !sliceCopy = BS.copy slice
              consumed = BS.length sliceCopy
              n' = n - fromIntegral consumed
          if n' == 0
            then do
              -- The wire MUST carry a CRLF before the next size line.
              termE <- SR.readExact (connRingTransport conn) newPos 2
              case termE of
                Left _ -> throwIO (ProtocolException ParseBadChunkHeader)
                Right (term, afterTerm)
                  | BS.length term < 2
                      || BS.index term 0 /= 0x0d
                      || BS.index term 1 /= 0x0a ->
                      throwIO (ProtocolException ParseBadChunkHeader)
                  | otherwise -> do
                      connectionAdvanceCursor conn afterTerm
                      writeIORef ref (ChunkPending 0)
            else do
              connectionAdvanceCursor conn newPos
              writeIORef ref (ChunkPending n')
          pure (Just sliceCopy)

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
    go acc = do
      pos <- readIORef (connCursor conn)
      r   <- SR.readUntilCRLFStrict (connRingTransport conn) pos 8192
      case r of
        Left e -> throwIO (ProtocolException (readErrorToParseError e))
        Right (Nothing, _) ->
          throwIO (ProtocolException ParseInvalidHeaderValue)
        Right (Just SR.StrictBareLf, _) ->
          throwIO (ProtocolException ParseInvalidHeaderValue)
        Right (Just (SR.StrictLine bs), newPos) -> do
          writeIORef (connCursor conn) newPos
          if BS.null bs
            then pure (reverse acc)
            else if BS.any badByte bs
                   then throwIO (ProtocolException ParseInvalidHeaderValue)
                   else case parseTrailerLine bs of
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

------------------------------------------------------------------------
-- Internal: SR.ReadError → Parser.ParseError mapping for
-- protocol-exception throwing inside the body machinery.
------------------------------------------------------------------------

readErrorToParseError :: SR.ReadError -> ParseError
readErrorToParseError = \case
  SR.ReadParse e            -> e
  SR.ReadMessageTooLong _   -> ParseMessageTooLong
  SR.ReadUnexpectedEof      -> ParseUnexpectedEof
  SR.ReadTransportError _   -> ParseUnexpectedEof
