module Network.HTTP2.Connection
  ( Connection (..)
  , SendBuffer (..)
  , ConnectionConfig (..)
  , ConnectionRole (..)
  , ConnectionError (..)
  , newConnection
  , newConnectionFromTransport
  , sendFrame
  , sendFrameUnlocked
  , sendFrameZeroCopy
  , sendFrames
  , sendFramesUnlocked
  , sendFramesZeroCopy
  , sendHeaderBlock
  , recvFrame
  , recvFrameRaw
  , closeConnection
  , connectionSettings
    -- * Re-exports
  , module Network.HTTP2.Connection.Settings
  , module Network.HTTP2.Connection.FlowControl
  , module Network.HTTP2.Connection.StreamTable
  , module Network.HTTP2.Transport
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, catch, SomeException)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Network.Socket (Socket)

import Network.HTTP2.Connection.FlowControl
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Connection.StreamTable
import Network.HTTP2.Frame
import Network.HTTP2.Frame.Encode (encodeFrameInto)
import Network.HTTP2.HPACK
import Network.HTTP2.Internal.RecvBuffer
import Network.HTTP2.Transport
import Network.HTTP2.Types

data ConnectionRole = RoleClient | RoleServer
  deriving stock (Eq, Show)

-- | Static configuration for opening a connection.
--
-- A connection can be opened either over a raw socket (the common case;
-- pass 'ccSocket') or over an arbitrary 'Transport' (e.g. a TLS-wrapped
-- stream; pass 'ccTransport'). Exactly one of those two fields must be
-- 'Just'.
data ConnectionConfig = ConnectionConfig
  { ccRole :: !ConnectionRole
  , ccSettings :: !Settings
  , ccSocket :: !(Maybe Socket)
  , ccTransport :: !(Maybe Transport)
  , ccOnGoAway :: StreamId -> ErrorCode -> ByteString -> IO ()
  }

data ConnectionError = ConnectionError
  { ceErrorCode :: !ErrorCode
  , ceMessage :: !ByteString
  , ceStreamId :: !StreamId
  }
  deriving stock (Eq, Show)

instance Exception ConnectionError

-- | Pre-allocated pinned buffer for zero-copy frame sends.
-- Frames are encoded directly into this buffer, then sent with a single write.
data SendBuffer = SendBuffer
  { sbBuffer :: !(ForeignPtr Word8)
  , sbCapacity :: !Int
  }

sendBufferSize :: Int
sendBufferSize = 65536

newSendBuffer :: IO SendBuffer
newSendBuffer = do
  fp <- BSI.mallocByteString sendBufferSize
  pure SendBuffer { sbBuffer = fp, sbCapacity = sendBufferSize }

data Connection = Connection
  { connRole :: !ConnectionRole
  , connTransport :: !Transport
  , connSocket :: !(Maybe Socket)
    -- ^ The raw socket, when the transport was built from one. Higher
    -- layers (e.g. server accept loops that want to know the peer addr)
    -- can use this; TLS connections may leave it 'Nothing'.
  , connLocalSettings :: !(IORef Settings)
  , connRemoteSettings :: !(IORef Settings)
  , connStreamTable :: !StreamTable
  , connSendFlowControl :: !FlowControl
  , connRecvFlowControl :: !FlowControl
  , connHpackEncoder :: !(MVar DynamicTable)
  , connHpackDecoder :: !(MVar DynamicTable)
  , connSendLock :: !(MVar ())
  , connRecvBuffer :: !RecvBuffer
  , connLastStreamId :: !(IORef StreamId)
  , connClosed :: !(IORef Bool)
  , connOnGoAway :: StreamId -> ErrorCode -> ByteString -> IO ()
  , connSendBuffer :: !SendBuffer
  }

-- | Build a 'Connection' from either a 'Socket' (the common case) or an
-- arbitrary 'Transport'. See 'newConnectionFromTransport' for the
-- transport-only variant.
newConnection :: ConnectionConfig -> IO Connection
newConnection cfg = case (ccTransport cfg, ccSocket cfg) of
  (Just t, mSock) -> mkConnection (ccRole cfg) (ccSettings cfg) (ccOnGoAway cfg) t mSock
  (Nothing, Just sock) ->
    mkConnection (ccRole cfg) (ccSettings cfg) (ccOnGoAway cfg) (socketTransport sock) (Just sock)
  (Nothing, Nothing) ->
    error "Network.HTTP2.Connection.newConnection: ConnectionConfig has neither ccTransport nor ccSocket"

-- | Build a 'Connection' over a generic 'Transport'. Use this when the
-- connection lives on top of something other than a bare TCP socket
-- (notably TLS).
newConnectionFromTransport
  :: ConnectionRole
  -> Settings
  -> (StreamId -> ErrorCode -> ByteString -> IO ())
  -> Transport
  -> IO Connection
newConnectionFromTransport role settings onGoAway t =
  mkConnection role settings onGoAway t Nothing

mkConnection
  :: ConnectionRole
  -> Settings
  -> (StreamId -> ErrorCode -> ByteString -> IO ())
  -> Transport
  -> Maybe Socket
  -> IO Connection
mkConnection role settings onGoAway transport mSock = do
  localSettings <- newIORef settings
  remoteSettings <- newIORef defaultSettings
  streamTable <- newStreamTable (role == RoleServer)
  sendFC <- atomically $ newFlowControl 65535
  recvFC <- atomically $ newFlowControl 65535
  encoder <- newDynamicTable 4096 >>= newMVar
  decoder <- newDynamicTable 4096 >>= newMVar
  sendLock <- newMVar ()
  recvBuf <- newRecvBuffer
  lastStreamId <- newIORef 0
  closed <- newIORef False
  sendBuf <- newSendBuffer
  pure Connection
    { connRole = role
    , connTransport = transport
    , connSocket = mSock
    , connLocalSettings = localSettings
    , connRemoteSettings = remoteSettings
    , connStreamTable = streamTable
    , connSendFlowControl = sendFC
    , connRecvFlowControl = recvFC
    , connHpackEncoder = encoder
    , connHpackDecoder = decoder
    , connSendLock = sendLock
    , connRecvBuffer = recvBuf
    , connLastStreamId = lastStreamId
    , connClosed = closed
    , connOnGoAway = onGoAway
    , connSendBuffer = sendBuf
    }

-- | Send a frame. Encodes and sends in one operation.
-- Uses a send lock to ensure frames aren't interleaved between connections.
sendFrame :: Connection -> Frame -> IO ()
sendFrame conn frame = do
  let bs = encodeFrame frame
  withMVar (connSendLock conn) $ \_ ->
    tSendAll (connTransport conn) bs

-- | Send a frame without acquiring the send lock.
-- Only safe when the caller is the sole writer (e.g. single-threaded connection loop).
{-# INLINE sendFrameUnlocked #-}
sendFrameUnlocked :: Connection -> Frame -> IO ()
sendFrameUnlocked conn frame = tSendAll (connTransport conn) (encodeFrame frame)

-- | Send multiple frames in a single write (reduces syscall overhead).
sendFrames :: Connection -> [Frame] -> IO ()
sendFrames conn frames = do
  let bss = map encodeFrame frames
  withMVar (connSendLock conn) $ \_ ->
    tSendMany (connTransport conn) bss

-- | Emit an encoded HPACK header block as a HEADERS frame followed
-- by zero or more CONTINUATION frames, splitting at the peer's
-- @SETTINGS_MAX_FRAME_SIZE@.  END_HEADERS is set on the final frame;
-- the @endStream@ flag is set on the initial HEADERS frame only.
--
-- A header block that fits within one frame is sent as a single
-- HEADERS with @END_HEADERS@ set, matching the pre-CONTINUATION
-- code path bit-for-bit.
--
-- The frames are sent atomically (with the connection send lock held)
-- so concurrent senders on other streams can't interleave a frame
-- between our HEADERS and its CONTINUATION block, which the wire
-- protocol forbids (RFC 9113 §6.10).
sendHeaderBlock
  :: Connection
  -> StreamId
  -> Bool         -- ^ set END_STREAM on the initial HEADERS frame
  -> FrameFlags   -- ^ extra flags to OR into the initial HEADERS frame
  -> ByteString   -- ^ encoded HPACK header block
  -> Int          -- ^ peer SETTINGS_MAX_FRAME_SIZE
  -> IO ()
sendHeaderBlock conn sid endStream extraFlags block maxFrame = do
  let n = BS.length block
  if n <= maxFrame
    then do
      let flags = flagEndHeaders
                .|. extraFlags
                .|. (if endStream then flagEndStream else 0)
          frame = Frame
            (FrameHeader (fromIntegral n) FrameHeaders flags sid)
            (HeadersFrame Nothing block)
      sendFrame conn frame
    else do
      let (head1, rest) = BS.splitAt maxFrame block
          initialFlags  = extraFlags
                       .|. (if endStream then flagEndStream else 0)
          frames        = headFrame head1 initialFlags : contFrames rest
      sendFrames conn frames
  where
    headFrame bs flags = Frame
      (FrameHeader (fromIntegral (BS.length bs)) FrameHeaders flags sid)
      (HeadersFrame Nothing bs)
    contFrames bs
      | BS.length bs <= maxFrame =
          [Frame
            (FrameHeader (fromIntegral (BS.length bs)) FrameContinuation flagEndHeaders sid)
            (ContinuationFrame bs)]
      | otherwise =
          let (chunk, rest) = BS.splitAt maxFrame bs
              f = Frame
                (FrameHeader (fromIntegral maxFrame) FrameContinuation 0 sid)
                (ContinuationFrame chunk)
          in f : contFrames rest

-- | Send multiple frames without the send lock. Combines into one writev.
{-# INLINE sendFramesUnlocked #-}
sendFramesUnlocked :: Connection -> [Frame] -> IO ()
sendFramesUnlocked conn frames =
  tSendMany (connTransport conn) (map encodeFrame frames)

-- | Zero-copy send: encode frames directly into the connection's pinned
-- send buffer, then send from that buffer. Avoids per-frame allocation.
-- Only safe for single-threaded connection loops.
{-# INLINE sendFrameZeroCopy #-}
sendFrameZeroCopy :: Connection -> Frame -> IO ()
sendFrameZeroCopy conn frame = do
  let SendBuffer fp _cap = connSendBuffer conn
  withForeignPtr fp $ \ptr -> do
    written <- encodeFrameInto frame ptr
    let bs = BSI.fromForeignPtr fp 0 written
    tSendAll (connTransport conn) bs

-- | Zero-copy batch send: encode multiple frames into the send buffer
-- contiguously, then send the whole buffer in one syscall.
{-# INLINE sendFramesZeroCopy #-}
sendFramesZeroCopy :: Connection -> [Frame] -> IO ()
sendFramesZeroCopy conn frames = do
  let SendBuffer fp _cap = connSendBuffer conn
  withForeignPtr fp $ \basePtr -> do
    totalWritten <- writeFrames basePtr 0 frames
    let bs = BSI.fromForeignPtr fp 0 totalWritten
    tSendAll (connTransport conn) bs
  where
    writeFrames _ offset [] = pure offset
    writeFrames ptr offset (f:fs) = do
      written <- encodeFrameInto f (ptr `plusPtr` offset)
      writeFrames ptr (offset + written) fs

recvFrame :: Connection -> IO (Either FrameDecodeError Frame)
recvFrame conn = do
  headerBytes <- recvExact conn frameHeaderLength
  if BS.length headerBytes < frameHeaderLength
    then pure (Left FrameTooShort)
    else case decodeFrameHeader headerBytes of
      Left err -> pure (Left err)
      Right hdr -> do
        let payloadLen = fromIntegral (fhLength hdr)
        if payloadLen == 0
          then case decodeFramePayload hdr BS.empty of
            Left err -> pure (Left err)
            Right fp -> pure (Right (Frame hdr fp))
          else do
            payload <- recvExact conn payloadLen
            if BS.length payload < payloadLen
              then pure (Left FrameTooShort)
              else case decodeFramePayload hdr payload of
                Left err -> pure (Left err)
                Right fp -> pure (Right (Frame hdr fp))

-- | Receive a frame header + raw payload without constructing FramePayload.
-- Avoids the ADT allocation for frames where the caller only needs the raw bytes
-- (e.g. HEADERS where the payload IS the HPACK block, DATA where it IS the body).
-- Returns Nothing on connection close.
{-# INLINE recvFrameRaw #-}
recvFrameRaw :: Connection -> IO (Maybe (FrameHeader, ByteString))
recvFrameRaw conn = do
  headerBytes <- recvExact conn frameHeaderLength
  if BS.length headerBytes < frameHeaderLength
    then pure Nothing
    else case decodeFrameHeader headerBytes of
      Left _ -> pure Nothing
      Right hdr -> do
        let payloadLen = fromIntegral (fhLength hdr)
        if payloadLen == 0
          then pure (Just (hdr, BS.empty))
          else do
            payload <- recvExact conn payloadLen
            if BS.length payload < payloadLen
              then pure Nothing
              else pure (Just (hdr, payload))

-- | Receive exactly n bytes using the connection's pinned ring buffer.
-- Returns a zero-copy slice when data doesn't wrap around the ring,
-- or a fresh copy on wrap-around (rare in practice).
{-# INLINE recvExact #-}
recvExact :: Connection -> Int -> IO ByteString
recvExact conn n =
  recvBufferRead (connRecvBuffer conn) (tRecvBuf (connTransport conn)) n

closeConnection :: Connection -> ErrorCode -> ByteString -> IO ()
closeConnection conn code msg = do
  alreadyClosed <- atomicModifyIORef' (connClosed conn) (\c -> (True, c))
  if alreadyClosed
    then pure ()
    else do
      lastId <- readIORef (connLastStreamId conn)
      let goaway = Frame
            (FrameHeader 0 FrameGoAway 0 0)
            (GoAwayFrame lastId code msg)
      sendFrame conn goaway
        `catch` (\(_ :: SomeException) -> pure ())

connectionSettings :: Connection -> IO (Settings, Settings)
connectionSettings conn = do
  local <- readIORef (connLocalSettings conn)
  remote <- readIORef (connRemoteSettings conn)
  pure (local, remote)
