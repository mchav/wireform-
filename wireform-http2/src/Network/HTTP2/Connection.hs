module Network.HTTP2.Connection
  ( Connection (..)
  , SendBuffer (..)
  , ConnectionConfig (..)
  , ConnectionRole (..)
  , ConnectionError (..)
  , newConnection
  , newConnectionFromTransport
  , sendFrame
  , sendFrameZeroCopy
  , sendFrames
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

import qualified Wireform.Transport as WT
import Wireform.Network (newRecvBufTransport)
import Wireform.Transport.Config (defaultTransportConfig)

import Network.HTTP2.Connection.FlowControl
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Connection.StreamTable
import Network.HTTP2.Frame
import Network.HTTP2.Frame.Encode (encodeFrameInto)
import qualified Network.HTTP2.Frame.StreamingReader as SR
import Network.HTTP2.HPACK
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
  , connRingTransport :: !WT.Transport
    -- ^ Magic-ring transport plumbed onto @tRecvBuf connTransport@.
    -- Owns its own 'Wireform.Ring.Internal.MagicRing' (destroyed
    -- on 'closeConnection') and is the sole receive side for the
    -- recv loop.  Replaces the previous pinned 'RecvBuffer'.
  , connRingCursor :: !(IORef Word64)
    -- ^ Position past the last byte consumed by 'recvFrame' /
    -- 'recvFrameRaw'.  Chained through the StreamingReader so we
    -- don't pay a 'transportLoadHead' round-trip per frame.
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
  ringT <- newRecvBufTransport defaultTransportConfig (tRecvBuf transport)
  ringCursor <- newIORef 0
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
    , connRingTransport = ringT
    , connRingCursor = ringCursor
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
--
-- __Unsafe__: concurrent callers will interleave frame bytes on the wire.
-- Only exposed for benchmarks that provably run single-threaded per connection.
-- Production code should use 'sendFrame'.
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
--
-- __Unsafe__: see 'sendFrameUnlocked'.
{-# INLINE sendFramesUnlocked #-}
sendFramesUnlocked :: Connection -> [Frame] -> IO ()
sendFramesUnlocked conn frames =
  tSendMany (connTransport conn) (map encodeFrame frames)

-- | Zero-copy send: encode a frame directly into the connection's pinned
-- send buffer, then send from that buffer. Avoids per-frame allocation.
-- Acquires the send lock so this is safe for concurrent use.
{-# INLINE sendFrameZeroCopy #-}
sendFrameZeroCopy :: Connection -> Frame -> IO ()
sendFrameZeroCopy conn frame = do
  let SendBuffer fp _cap = connSendBuffer conn
  withMVar (connSendLock conn) $ \_ ->
    withForeignPtr fp $ \ptr -> do
      written <- encodeFrameInto frame ptr
      let bs = BSI.fromForeignPtr fp 0 written
      tSendAll (connTransport conn) bs

-- | Zero-copy batch send: encode multiple frames into the send buffer
-- contiguously, then send the whole buffer in one syscall.
-- Acquires the send lock so this is safe for concurrent use.
{-# INLINE sendFramesZeroCopy #-}
sendFramesZeroCopy :: Connection -> [Frame] -> IO ()
sendFramesZeroCopy conn frames = do
  let SendBuffer fp _cap = connSendBuffer conn
  withMVar (connSendLock conn) $ \_ ->
    withForeignPtr fp $ \basePtr -> do
      totalWritten <- writeFrames basePtr 0 frames
      let bs = BSI.fromForeignPtr fp 0 totalWritten
      tSendAll (connTransport conn) bs
  where
    writeFrames _ offset [] = pure offset
    writeFrames ptr offset (f:fs) = do
      written <- encodeFrameInto f (ptr `plusPtr` offset)
      writeFrames ptr (offset + written) fs

-- | Receive a typed frame off the wire.  Walks the magic ring via
-- 'Network.HTTP2.Frame.StreamingReader.readFrameFrom' (single
-- 'transportLoadHead' per frame, zero-copy payload slice into the
-- ring) and runs 'decodeFramePayload' for per-type validation.
--
-- Bytes of the payload slice are valid only until the connection's
-- ring tail next advances past them, which 'readFrameFrom' does on
-- success — copy via 'BS.copy' if you need the bytes past the
-- next recv loop iteration.
recvFrame :: Connection -> IO (Either FrameDecodeError Frame)
recvFrame conn = do
  pos <- readIORef (connRingCursor conn)
  r   <- SR.readFrameFrom (connRingTransport conn) pos
  case r of
    Right (fr, newPos) -> do
      writeIORef (connRingCursor conn) newPos
      pure (Right fr)
    Left (SR.ReadDecode e)        -> pure (Left e)
    Left SR.ReadUnexpectedEof     -> pure (Left FrameTooShort)
    Left (SR.ReadTransportError _) -> pure (Left FrameTooShort)

-- | Receive a frame header + raw payload without constructing the
-- typed 'FramePayload'.  Used by the engine layer for DATA / HEADERS
-- where the payload bytes IS what the caller wants.  Returns
-- 'Nothing' on connection close (clean EOF or transport error).
{-# INLINE recvFrameRaw #-}
recvFrameRaw :: Connection -> IO (Maybe (FrameHeader, ByteString))
recvFrameRaw conn = do
  pos <- readIORef (connRingCursor conn)
  r   <- SR.readFrameFrom (connRingTransport conn) pos
  case r of
    Right (Frame hdr (FramePayloadRaw bs), newPos) -> do
      writeIORef (connRingCursor conn) newPos
      pure (Just (hdr, bs))
    Left _ -> pure Nothing

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
      -- Tear down the magic ring (frees its mmap).  Any frame
      -- payload slices the caller still holds become dangling
      -- pointers; they should have been 'BS.copy'd inside the
      -- per-frame handler.
      WT.transportClose (connRingTransport conn)
        `catch` (\(_ :: SomeException) -> pure ())

connectionSettings :: Connection -> IO (Settings, Settings)
connectionSettings conn = do
  local <- readIORef (connLocalSettings conn)
  remote <- readIORef (connRemoteSettings conn)
  pure (local, remote)
