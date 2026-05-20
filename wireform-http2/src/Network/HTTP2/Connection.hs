module Network.HTTP2.Connection
  ( Connection (..)
  , SendBuffer (..)
  , ConnectionConfig (..)
  , ConnectionRole (..)
  , ConnectionError (..)
  , newConnection
  , sendFrame
  , sendFrameUnlocked
  , sendFrameZeroCopy
  , sendFrames
  , sendFramesUnlocked
  , sendFramesZeroCopy
  , recvFrame
  , recvFrameRaw
  , closeConnection
  , connectionSettings
    -- * Re-exports
  , module Network.HTTP2.Connection.Settings
  , module Network.HTTP2.Connection.FlowControl
  , module Network.HTTP2.Connection.StreamTable
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, throwIO, catch, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as NBS

import Network.HTTP2.Connection.FlowControl
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Connection.StreamTable
import Network.HTTP2.Frame
import Network.HTTP2.Frame.Encode (encodeFrameInto)
import Network.HTTP2.HPACK
import Network.HTTP2.Internal.RecvBuffer
import Network.HTTP2.Types

data ConnectionRole = RoleClient | RoleServer
  deriving stock (Eq, Show)

data ConnectionConfig = ConnectionConfig
  { ccRole :: !ConnectionRole
  , ccSettings :: !Settings
  , ccSocket :: !Socket
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
  , connSocket :: !Socket
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

newConnection :: ConnectionConfig -> IO Connection
newConnection cfg = do
  localSettings <- newIORef (ccSettings cfg)
  remoteSettings <- newIORef defaultSettings
  streamTable <- newStreamTable (ccRole cfg == RoleServer)
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
    { connRole = ccRole cfg
    , connSocket = ccSocket cfg
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
    , connOnGoAway = ccOnGoAway cfg
    , connSendBuffer = sendBuf
    }

-- | Send a frame. Encodes and sends in one operation.
-- Uses a send lock to ensure frames aren't interleaved between connections.
sendFrame :: Connection -> Frame -> IO ()
sendFrame conn frame = do
  let bs = encodeFrame frame
  withMVar (connSendLock conn) $ \_ ->
    NBS.sendAll (connSocket conn) bs

-- | Send a frame without acquiring the send lock.
-- Only safe when the caller is the sole writer (e.g. single-threaded connection loop).
{-# INLINE sendFrameUnlocked #-}
sendFrameUnlocked :: Connection -> Frame -> IO ()
sendFrameUnlocked conn frame = NBS.sendAll (connSocket conn) (encodeFrame frame)

-- | Send multiple frames in a single write (reduces syscall overhead).
sendFrames :: Connection -> [Frame] -> IO ()
sendFrames conn frames = do
  let bss = map encodeFrame frames
  withMVar (connSendLock conn) $ \_ ->
    NBS.sendMany (connSocket conn) bss

-- | Send multiple frames without the send lock. Combines into one writev.
{-# INLINE sendFramesUnlocked #-}
sendFramesUnlocked :: Connection -> [Frame] -> IO ()
sendFramesUnlocked conn frames =
  NBS.sendMany (connSocket conn) (map encodeFrame frames)

-- | Zero-copy send: encode frames directly into the connection's pinned
-- send buffer, then send from that buffer. Avoids per-frame allocation.
-- Only safe for single-threaded connection loops.
{-# INLINE sendFrameZeroCopy #-}
sendFrameZeroCopy :: Connection -> Frame -> IO ()
sendFrameZeroCopy conn frame = do
  let SendBuffer fp cap = connSendBuffer conn
  withForeignPtr fp $ \ptr -> do
    written <- encodeFrameInto frame ptr
    let bs = BSI.fromForeignPtr fp 0 written
    NBS.sendAll (connSocket conn) bs

-- | Zero-copy batch send: encode multiple frames into the send buffer
-- contiguously, then send the whole buffer in one syscall.
{-# INLINE sendFramesZeroCopy #-}
sendFramesZeroCopy :: Connection -> [Frame] -> IO ()
sendFramesZeroCopy conn frames = do
  let SendBuffer fp cap = connSendBuffer conn
  withForeignPtr fp $ \basePtr -> do
    totalWritten <- writeFrames basePtr 0 frames
    let bs = BSI.fromForeignPtr fp 0 totalWritten
    NBS.sendAll (connSocket conn) bs
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
recvExact conn n = recvBufferRead (connRecvBuffer conn) (connSocket conn) n

closeConnection :: Connection -> ErrorCode -> ByteString -> IO ()
closeConnection conn code msg = do
  isClosed <- readIORef (connClosed conn)
  if isClosed
    then pure ()
    else do
      writeIORef (connClosed conn) True
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
