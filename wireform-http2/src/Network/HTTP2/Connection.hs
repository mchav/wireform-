module Network.HTTP2.Connection
  ( Connection (..)
  , ConnectionConfig (..)
  , ConnectionRole (..)
  , ConnectionError (..)
  , newConnection
  , sendFrame
  , sendFrameUnlocked
  , sendFrames
  , sendFramesUnlocked
  , recvFrame
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
import Data.IORef
import Data.Word
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as NBS

import Network.HTTP2.Connection.FlowControl
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Connection.StreamTable
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
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
  , connRecvBuffer :: !(IORef ByteString)
  , connLastStreamId :: !(IORef StreamId)
  , connClosed :: !(IORef Bool)
  , connOnGoAway :: StreamId -> ErrorCode -> ByteString -> IO ()
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
  recvBuf <- newIORef BS.empty
  lastStreamId <- newIORef 0
  closed <- newIORef False
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

-- | Buffered receive. Reads large chunks from the socket and serves
-- smaller requests from the buffer to minimize syscall overhead.
recvExact :: Connection -> Int -> IO ByteString
recvExact conn n = do
  buf <- readIORef (connRecvBuffer conn)
  let bufLen = BS.length buf
  if bufLen >= n
    then do
      let (result, rest) = BS.splitAt n buf
      writeIORef (connRecvBuffer conn) rest
      pure result
    else do
      -- Read a large chunk to amortize syscall cost
      let toRead = max (n - bufLen) 65536
      chunk <- NBS.recv (connSocket conn) toRead
      if BS.null chunk
        then do
          writeIORef (connRecvBuffer conn) BS.empty
          pure buf  -- Return whatever we had (likely short)
        else do
          let combined = if BS.null buf then chunk else buf <> chunk
          if BS.length combined >= n
            then do
              let (result, rest) = BS.splitAt n combined
              writeIORef (connRecvBuffer conn) rest
              pure result
            else recvExactSlow conn n combined

-- Slow path: need multiple recv calls to satisfy the request
recvExactSlow :: Connection -> Int -> ByteString -> IO ByteString
recvExactSlow conn n partial = do
  let needed = n - BS.length partial
  chunk <- NBS.recv (connSocket conn) (max needed 65536)
  if BS.null chunk
    then do
      writeIORef (connRecvBuffer conn) BS.empty
      pure partial
    else do
      let combined = partial <> chunk
      if BS.length combined >= n
        then do
          let (result, rest) = BS.splitAt n combined
          writeIORef (connRecvBuffer conn) rest
          pure result
        else recvExactSlow conn n combined

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
