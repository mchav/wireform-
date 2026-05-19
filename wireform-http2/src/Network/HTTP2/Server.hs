module Network.HTTP2.Server
  ( ServerConfig (..)
  , defaultServerConfig
  , Request (..)
  , Response (..)
  , ResponseBody (..)
  , runServer
  , runServerOnSocket
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, catch, SomeException, throwIO, finally)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS

import Network.HTTP2.Connection
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Types

data ServerConfig = ServerConfig
  { serverSettings :: !Settings
  , serverHost :: !String
  , serverPort :: !String
  , serverHandler :: Request -> (Response -> IO ()) -> IO ()
  }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverSettings = defaultSettings
  , serverHost = "0.0.0.0"
  , serverPort = "8080"
  , serverHandler = \_ respond -> respond defaultResponse
  }

data Request = Request
  { requestMethod :: !ByteString
  , requestPath :: !ByteString
  , requestScheme :: !ByteString
  , requestAuthority :: !ByteString
  , requestHeaders :: ![(ByteString, ByteString)]
  , requestBody :: !(IO ByteString)
  , requestStreamId :: !StreamId
  }

data Response = Response
  { responseStatus :: !Int
  , responseHeaders :: ![(ByteString, ByteString)]
  , responseBody :: !ResponseBody
  }

data ResponseBody
  = ResponseBodyEmpty
  | ResponseBodyBS !ByteString
  | ResponseBodyStream (IO (Maybe ByteString))

defaultResponse :: Response
defaultResponse = Response
  { responseStatus = 200
  , responseHeaders = []
  , responseBody = ResponseBodyEmpty
  }

runServer :: ServerConfig -> IO ()
runServer cfg = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just (serverHost cfg)) (Just (serverPort cfg))
  case addrs of
    [] -> error "No address found"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.NoDelay 1
        NS.bind sock (NS.addrAddress addr)
        NS.listen sock 128
        acceptLoop cfg sock

acceptLoop :: ServerConfig -> Socket -> IO ()
acceptLoop cfg listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- forkIO $ handleClient cfg clientSock `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop cfg listenSock

runServerOnSocket :: ServerConfig -> Socket -> IO ()
runServerOnSocket cfg sock = handleClient cfg sock

handleClient :: ServerConfig -> Socket -> IO ()
handleClient cfg sock = do
  preface <- recvExactSock sock (BS.length connectionPreface)
  if preface /= connectionPreface
    then NS.close sock
    else do
      conn <- newConnection ConnectionConfig
        { ccRole = RoleServer
        , ccSettings = serverSettings cfg
        , ccSocket = sock
        , ccOnGoAway = \_ _ _ -> pure ()
        }
      sendServerPreface conn (serverSettings cfg)
      connectionLoop cfg conn `finally` NS.close sock

sendServerPreface :: Connection -> Settings -> IO ()
sendServerPreface conn settings = do
  let params = encodeSettings settings
      frame = Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
  sendFrame conn frame

connectionLoop :: ServerConfig -> Connection -> IO ()
connectionLoop cfg conn = do
  result <- recvFrame conn
  case result of
    Left _err ->
      closeConnection conn ProtocolError "frame decode error"
    Right frame -> do
      handleFrame' cfg conn frame
      connectionLoop cfg conn

handleFrame' :: ServerConfig -> Connection -> Frame -> IO ()
handleFrame' cfg conn (Frame hdr payload) = case payload of
  SettingsFrame params
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise -> do
        case applySettingsParams defaultSettings params of
          Left _ -> closeConnection conn ProtocolError "invalid settings"
          Right newSettings -> do
            writeIORef (connRemoteSettings conn) newSettings
            let ack = Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
            sendFrame conn ack

  PingFrame opaqueData
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise -> do
        let pong = Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame opaqueData)
        sendFrame conn pong

  WindowUpdateFrame increment
    | fhStreamId hdr == 0 ->
        atomically $ do
          _ <- releaseWindow (connSendFlowControl conn) (fromIntegral increment)
          pure ()
    | otherwise -> pure ()

  GoAwayFrame lastId code debug ->
    connOnGoAway conn lastId code debug

  HeadersFrame _mpri headerBlock
    | testFlag (fhFlags hdr) flagEndHeaders -> do
        decoder <- readMVar (connHpackDecoder conn)
        result <- decodeHeaderBlock decoder headerBlock
        case result of
          Left _ -> closeConnection conn CompressionError "HPACK decode failed"
          Right headers -> do
            writeIORef (connLastStreamId conn) (fhStreamId hdr)
            let req = buildRequest (fhStreamId hdr) headers (testFlag (fhFlags hdr) flagEndStream)
            _ <- forkIO $ serverHandler cfg req $ \resp ->
              sendResponse conn (fhStreamId hdr) resp
            pure ()
    | otherwise -> pure ()

  DataFrame _body -> do
    let len = fhLength hdr
    if len > 0
      then do
        let windowUpdate = Frame
              (FrameHeader 4 FrameWindowUpdate 0 0)
              (WindowUpdateFrame len)
        sendFrame conn windowUpdate
        let streamWindowUpdate = Frame
              (FrameHeader 4 FrameWindowUpdate 0 (fhStreamId hdr))
              (WindowUpdateFrame len)
        sendFrame conn streamWindowUpdate
      else pure ()

  RSTStreamFrame _ -> pure ()

  _ -> pure ()

buildRequest :: StreamId -> [(ByteString, ByteString)] -> Bool -> Request
buildRequest sid headers _endStream =
  let findHeader name = maybe "" id (lookup name headers)
  in Request
    { requestMethod = findHeader ":method"
    , requestPath = findHeader ":path"
    , requestScheme = findHeader ":scheme"
    , requestAuthority = findHeader ":authority"
    , requestHeaders = filter (\(k, _) -> not (BS.isPrefixOf ":" k)) headers
    , requestBody = pure ""
    , requestStreamId = sid
    }

sendResponse :: Connection -> StreamId -> Response -> IO ()
sendResponse conn sid resp = do
  encoder <- readMVar (connHpackEncoder conn)
  let statusBS = BS.pack (map (fromIntegral . fromEnum) (show (responseStatus resp)))
      statusHdr = (":status", statusBS)
      allHeaders = statusHdr : responseHeaders resp
  headerBlock <- encodeHeaderBlock defaultEncodeStrategy encoder allHeaders
  case responseBody resp of
    ResponseBodyEmpty -> do
      let frame = Frame
            (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders
              (flagEndHeaders .|. flagEndStream) sid)
            (HeadersFrame Nothing headerBlock)
      sendFrame conn frame
    ResponseBodyBS body -> do
      let headersFrame = Frame
            (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders
              flagEndHeaders sid)
            (HeadersFrame Nothing headerBlock)
      sendFrame conn headersFrame
      let dataFrame = Frame
            (FrameHeader (fromIntegral (BS.length body)) FrameData flagEndStream sid)
            (DataFrame body)
      sendFrame conn dataFrame
    ResponseBodyStream producer -> do
      let headersFrame = Frame
            (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders
              flagEndHeaders sid)
            (HeadersFrame Nothing headerBlock)
      sendFrame conn headersFrame
      streamBody conn sid producer

streamBody :: Connection -> StreamId -> IO (Maybe ByteString) -> IO ()
streamBody conn sid producer = do
  mChunk <- producer
  case mChunk of
    Nothing -> do
      let emptyData = Frame
            (FrameHeader 0 FrameData flagEndStream sid)
            (DataFrame "")
      sendFrame conn emptyData
    Just chunk -> do
      let dataFrame = Frame
            (FrameHeader (fromIntegral (BS.length chunk)) FrameData 0 sid)
            (DataFrame chunk)
      sendFrame conn dataFrame
      streamBody conn sid producer

recvExactSock :: Socket -> Int -> IO ByteString
recvExactSock sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 4096)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)
