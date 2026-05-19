module Network.HTTP2.Connection
  ( Connection (..)
  , ConnectionConfig (..)
  , ConnectionRole (..)
  , ConnectionError (..)
  , newConnection
  , sendFrame
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

sendFrame :: Connection -> Frame -> IO ()
sendFrame conn frame = do
  let bs = encodeFrame frame
  withMVar (connSendLock conn) $ \_ ->
    sendAll (connSocket conn) bs

sendAll :: Socket -> ByteString -> IO ()
sendAll sock bs
  | BS.null bs = pure ()
  | otherwise = do
      sent <- NBS.send sock bs
      if sent >= BS.length bs
        then pure ()
        else sendAll sock (BS.drop sent bs)

recvFrame :: Connection -> IO (Either FrameDecodeError Frame)
recvFrame conn = do
  headerBytes <- recvExact conn frameHeaderLength
  case decodeFrameHeader headerBytes of
    Left err -> pure (Left err)
    Right hdr -> do
      payload <- recvExact conn (fromIntegral (fhLength hdr))
      case decodeFramePayload hdr payload of
        Left err -> pure (Left err)
        Right fp -> pure (Right (Frame hdr fp))

recvExact :: Connection -> Int -> IO ByteString
recvExact conn n = do
  buf <- readIORef (connRecvBuffer conn)
  if BS.length buf >= n
    then do
      let (result, rest) = BS.splitAt n buf
      writeIORef (connRecvBuffer conn) rest
      pure result
    else do
      more <- recvLoop (connSocket conn) (n - BS.length buf) [buf]
      let full = BS.concat (reverse more)
      let (result, rest) = BS.splitAt n full
      writeIORef (connRecvBuffer conn) rest
      pure result

recvLoop :: Socket -> Int -> [ByteString] -> IO [ByteString]
recvLoop sock needed acc
  | needed <= 0 = pure acc
  | otherwise = do
      chunk <- NBS.recv sock (max needed 4096)
      if BS.null chunk
        then pure acc
        else recvLoop sock (needed - BS.length chunk) (chunk : acc)

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
