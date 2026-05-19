module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (readMVar, newMVar, modifyMVar_)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, bracket, finally)
import Data.Bits ((.|.), (.&.), testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import System.IO (hPutStrLn, stderr, hFlush)

import Network.HTTP2.Connection hiding (StreamState(..), ClosedReason(..))
import Network.HTTP2.Connection.Settings
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Types (StreamId, FrameType(..), ErrorCode(..), Settings(..), defaultSettings, Priority(..))

data StreamInfo = StreamInfo
  { siState :: !StreamSt
  , siSendWindow :: !Int
  } deriving stock (Eq, Show)

data StreamSt
  = StIdle
  | StOpen
  | StHalfClosedRemote
  | StHalfClosedLocal
  | StClosed
  deriving stock (Eq, Show)

data ServerState = ServerState
  { ssStreams :: !(Map StreamId StreamInfo)
  , ssLastStreamId :: !StreamId
  , ssLocalSettings :: !Settings
  , ssRemoteSettings :: !Settings
  , ssExpectingContinuation :: !(Maybe StreamId)
  , ssContinuationBuffer :: !ByteString
  , ssGoAwaySent :: !Bool
  , ssConnSendWindow :: !Int
  }

main :: IO ()
main = do
  let port = "9090"
  hPutStrLn stderr $ "Starting HTTP/2 conformance server on port " <> port
  hFlush stderr
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just port)
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
        hPutStrLn stderr "Listening..."
        hFlush stderr
        acceptLoop sock

acceptLoop :: Socket -> IO ()
acceptLoop listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- forkIO $ handleClient clientSock
    `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop listenSock

handleClient :: Socket -> IO ()
handleClient sock = do
  preface <- recvExact sock 24
  if preface /= connectionPreface
    then do
      conn <- newConnection ConnectionConfig
        { ccRole = RoleServer
        , ccSettings = defaultSettings
        , ccSocket = sock
        , ccOnGoAway = \_ _ _ -> pure ()
        }
      closeConnection conn ProtocolError ""
      NS.close sock
    else do
      conn <- newConnection ConnectionConfig
        { ccRole = RoleServer
        , ccSettings = defaultSettings
            { settingsMaxConcurrentStreams = Just 100
            }
        , ccSocket = sock
        , ccOnGoAway = \_ _ _ -> pure ()
        }
      let initSettings = defaultSettings { settingsMaxConcurrentStreams = Just 100 }
      sendServerSettings conn initSettings
      stRef <- newIORef ServerState
        { ssStreams = Map.empty
        , ssLastStreamId = 0
        , ssLocalSettings = initSettings
        , ssRemoteSettings = defaultSettings
        , ssExpectingContinuation = Nothing
        , ssContinuationBuffer = BS.empty
        , ssGoAwaySent = False
        , ssConnSendWindow = 65535
        }
      serverLoop conn stRef `finally` NS.close sock

sendServerSettings :: Connection -> Settings -> IO ()
sendServerSettings conn settings = do
  let params = encodeSettings settings
      frame = Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
  sendFrame conn frame

serverLoop :: Connection -> IORef ServerState -> IO ()
serverLoop conn stRef = do
  result <- recvFrame conn
  case result of
    Left _ -> pure ()
    Right frame -> do
      shouldContinue <- processFrame conn stRef frame
      if shouldContinue
        then serverLoop conn stRef
        else pure ()

processFrame :: Connection -> IORef ServerState -> Frame -> IO Bool
processFrame conn stRef (Frame hdr payload) = do
  st <- readIORef stRef
  let maxFrameSize = settingsMaxFrameSize (ssLocalSettings st)
  if fhLength hdr > maxFrameSize
    then do
      connError conn stRef FrameSizeError
      pure False
    else processFrame' conn stRef st hdr payload

processFrame' :: Connection -> IORef ServerState -> ServerState -> FrameHeader -> FramePayload -> IO Bool
processFrame' conn stRef st hdr payload =
  case ssExpectingContinuation st of
    Just expectedSid -> case payload of
      ContinuationFrame fragment
        | fhStreamId hdr == expectedSid -> do
            let buf = ssContinuationBuffer st <> fragment
            if testFlag (fhFlags hdr) flagEndHeaders
              then do
                writeIORef stRef st
                  { ssExpectingContinuation = Nothing
                  , ssContinuationBuffer = BS.empty
                  }
                processHeaders conn stRef (fhStreamId hdr) buf
              else do
                writeIORef stRef st { ssContinuationBuffer = buf }
                pure True
        | otherwise -> do
            connError conn stRef ProtocolError
            pure False
      _ -> do
        connError conn stRef ProtocolError
        pure False
    Nothing -> case payload of
      ContinuationFrame _ -> do
        connError conn stRef ProtocolError
        pure False
      _ -> processFramePayload conn stRef hdr payload

processFramePayload :: Connection -> IORef ServerState -> FrameHeader -> FramePayload -> IO Bool
processFramePayload conn stRef hdr payload = case payload of
  SettingsFrame params
    | testFlag (fhFlags hdr) flagAck -> pure True
    | fhStreamId hdr /= 0 -> do
        connError conn stRef ProtocolError
        pure False
    | otherwise -> do
        st <- readIORef stRef
        case validateAndApplySettings (ssRemoteSettings st) params of
          Left code -> do
            connError conn stRef code
            pure False
          Right newSettings -> do
            -- Update window sizes for existing streams if initial window changed
            writeIORef stRef st { ssRemoteSettings = newSettings }
            let ack = Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
            sendFrame conn ack
            pure True

  PingFrame opaqueData
    | fhStreamId hdr /= 0 -> do
        connError conn stRef ProtocolError
        pure False
    | BS.length opaqueData /= 8 -> do
        connError conn stRef FrameSizeError
        pure False
    | testFlag (fhFlags hdr) flagAck -> pure True
    | otherwise -> do
        let pong = Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame opaqueData)
        sendFrame conn pong
        pure True

  WindowUpdateFrame increment
    | fhStreamId hdr == 0 -> do
        if increment == 0
          then do
            connError conn stRef ProtocolError
            pure False
          else do
            st <- readIORef stRef
            let newWindow = ssConnSendWindow st + fromIntegral increment
            if newWindow > 2147483647
              then do
                connError conn stRef FlowControlError
                pure False
              else do
                writeIORef stRef st { ssConnSendWindow = newWindow }
                pure True
    | otherwise -> do
        st <- readIORef stRef
        let streamSt = getStreamState st (fhStreamId hdr)
        case streamSt of
          StIdle
            | fhStreamId hdr `mod` 2 == 0 -> do
                connError conn stRef ProtocolError
                pure False
            | fhStreamId hdr > ssLastStreamId st -> do
                connError conn stRef ProtocolError
                pure False
            | otherwise ->
                if increment == 0
                  then do
                    streamError conn (fhStreamId hdr) ProtocolError
                    pure True
                  else pure True
          StHalfClosedRemote ->
            if increment == 0
              then do
                streamError conn (fhStreamId hdr) ProtocolError
                pure True
              else do
                checkStreamWindowOverflow conn stRef (fhStreamId hdr) increment
          StClosed -> pure True
          _ ->
            if increment == 0
              then do
                streamError conn (fhStreamId hdr) ProtocolError
                pure True
              else do
                checkStreamWindowOverflow conn stRef (fhStreamId hdr) increment

  GoAwayFrame _ _ _ -> pure False

  HeadersFrame mpri headerBlock
    | fhStreamId hdr == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | fhStreamId hdr `mod` 2 == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | otherwise -> do
        st <- readIORef stRef
        -- Check self-dependency
        case mpri of
          Just pri | priorityDependency pri == fhStreamId hdr -> do
            streamError conn (fhStreamId hdr) ProtocolError
            pure True
          _ -> do
            -- Stream ID must be greater than last
            if fhStreamId hdr <= ssLastStreamId st && ssLastStreamId st /= 0
              then do
                connError conn stRef ProtocolError
                pure False
              else do
                -- Check concurrent stream limit
                let maxConcurrent = maybe 100 id (settingsMaxConcurrentStreams (ssLocalSettings st))
                    openCount = Map.size (Map.filter (\si -> siState si == StOpen || siState si == StHalfClosedRemote) (ssStreams st))
                if fromIntegral openCount >= maxConcurrent
                  then do
                    streamError conn (fhStreamId hdr) RefusedStream
                    modifyIORef' stRef $ \s -> s
                      { ssLastStreamId = fhStreamId hdr
                      , ssStreams = Map.insert (fhStreamId hdr) (StreamInfo StClosed 0) (ssStreams s)
                      }
                    pure True  -- RST_STREAM is valid per RFC; h2spec also accepts this
                  else do
                    let streamSt = getStreamState st (fhStreamId hdr)
                    case streamSt of
                      StHalfClosedRemote -> do
                        streamError conn (fhStreamId hdr) StreamClosed
                        pure True
                      StClosed -> do
                        connError conn stRef StreamClosed
                        pure False
                      StOpen ->
                        if not (testFlag (fhFlags hdr) flagEndStream)
                          then do
                            streamError conn (fhStreamId hdr) ProtocolError
                            pure True
                          else do
                            if testFlag (fhFlags hdr) flagEndHeaders
                              then processTrailers conn stRef (fhStreamId hdr) headerBlock
                              else do
                                modifyIORef' stRef $ \s -> s
                                  { ssExpectingContinuation = Just (fhStreamId hdr)
                                  , ssContinuationBuffer = headerBlock
                                  }
                                pure True
                      _ -> do
                        writeIORef stRef st { ssLastStreamId = fhStreamId hdr }
                        if testFlag (fhFlags hdr) flagEndHeaders
                          then processHeaders conn stRef (fhStreamId hdr) headerBlock
                          else do
                            modifyIORef' stRef $ \s -> s
                              { ssExpectingContinuation = Just (fhStreamId hdr)
                              , ssContinuationBuffer = headerBlock
                              }
                            pure True

  DataFrame _body
    | fhStreamId hdr == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | otherwise -> do
        st <- readIORef stRef
        let streamSt = getStreamState st (fhStreamId hdr)
        case streamSt of
          StIdle -> do
            connError conn stRef ProtocolError
            pure False
          StHalfClosedRemote -> do
            streamError conn (fhStreamId hdr) StreamClosed
            pure True
          StClosed -> do
            connError conn stRef StreamClosed
            pure False
          _ -> do
            -- Send WINDOW_UPDATE for connection and stream
            let len = fhLength hdr
            if len > 0
              then do
                let windowUpdate = Frame
                      (FrameHeader 4 FrameWindowUpdate 0 0)
                      (WindowUpdateFrame len)
                sendFrame conn windowUpdate
                let streamWU = Frame
                      (FrameHeader 4 FrameWindowUpdate 0 (fhStreamId hdr))
                      (WindowUpdateFrame len)
                sendFrame conn streamWU
              else pure ()
            -- Update stream state on END_STREAM
            if testFlag (fhFlags hdr) flagEndStream
              then modifyIORef' stRef $ \s -> s
                { ssStreams = Map.insert (fhStreamId hdr)
                    (StreamInfo StHalfClosedRemote 65535)
                    (ssStreams s)
                }
              else pure ()
            pure True

  RSTStreamFrame _code
    | fhStreamId hdr == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | otherwise -> do
        st <- readIORef stRef
        let streamSt = getStreamState st (fhStreamId hdr)
        case streamSt of
          StIdle -> do
            connError conn stRef ProtocolError
            pure False
          _ -> do
            modifyIORef' stRef $ \s -> s
              { ssStreams = Map.insert (fhStreamId hdr)
                  (StreamInfo StClosed 0) (ssStreams s)
              }
            pure True

  PriorityFrame pri
    | fhStreamId hdr == 0 -> do
        connError conn stRef ProtocolError
        pure False
    | priorityDependency pri == fhStreamId hdr -> do
        streamError conn (fhStreamId hdr) ProtocolError
        pure True
    | otherwise -> pure True

  PushPromiseFrame _ _ -> do
    connError conn stRef ProtocolError
    pure False

  UnknownFrame _ _ -> pure True

  _ -> pure True

processHeaders :: Connection -> IORef ServerState -> StreamId -> ByteString -> IO Bool
processHeaders conn stRef sid headerBlock = do
  decoder <- readMVar (connHpackDecoder conn)
  st <- readIORef stRef
  let tableSize = fromIntegral (settingsHeaderTableSize (ssLocalSettings st))
  result <- decodeHeaderBlockWithMaxSize decoder tableSize headerBlock
  case result of
    Left _ -> do
      connError conn stRef CompressionError
      pure False
    Right headers -> do
      case validateRequestHeaders headers of
        Left _ -> do
          streamError conn sid ProtocolError
          modifyIORef' stRef $ \s -> s
            { ssStreams = Map.insert sid (StreamInfo StClosed 0) (ssStreams s)
            }
          pure True
        Right () -> do
          st2 <- readIORef stRef
          let initWin = fromIntegral (settingsInitialWindowSize (ssRemoteSettings st2))
          modifyIORef' stRef $ \s -> s
            { ssStreams = Map.insert sid (StreamInfo StOpen initWin) (ssStreams s)
            }
          sendSimpleResponse conn stRef sid
          pure True

validateRequestHeaders :: [(ByteString, ByteString)] -> Either ByteString ()
validateRequestHeaders headers = do
  let (pseudos, regulars) = span (\(k, _) -> BS.isPrefixOf ":" k) headers
  -- Pseudo-headers must come before regular headers
  let remainingPseudos = filter (\(k, _) -> BS.isPrefixOf ":" k) regulars
  if not (null remainingPseudos)
    then Left "pseudo-header after regular header"
    else pure ()
  -- Check for required pseudo-headers
  let pseudoNames = map fst pseudos
  let hasMethod = ":method" `elem` pseudoNames
      hasScheme = ":scheme" `elem` pseudoNames
      hasPath = ":path" `elem` pseudoNames
  if not hasMethod then Left "missing :method"
  else if not hasScheme then Left "missing :scheme"
  else if not hasPath then Left "missing :path"
  else pure ()
  -- Check for duplicated pseudo-headers
  let checkDuplicates [] = Right ()
      checkDuplicates (x:xs)
        | x `elem` xs = Left "duplicated pseudo-header"
        | otherwise = checkDuplicates xs
  checkDuplicates pseudoNames
  -- Check :path is not empty
  case lookup ":path" pseudos of
    Just p | BS.null p -> Left "empty :path"
    _ -> pure ()
  -- Check for unknown pseudo-headers
  let validPseudos = [":method", ":scheme", ":path", ":authority"]
  let unknownPseudos = filter (\n -> n `notElem` validPseudos) pseudoNames
  if not (null unknownPseudos)
    then Left "unknown pseudo-header"
    else pure ()
  -- Check for response pseudo-headers
  if ":status" `elem` pseudoNames
    then Left "response pseudo-header in request"
    else pure ()
  -- Check for uppercase header names
  let hasUppercase bs = BS.any (\w -> w >= 0x41 && w <= 0x5A) bs
  let uppercaseHeaders = filter (\(k, _) -> hasUppercase k) (pseudos <> regulars)
  if not (null uppercaseHeaders)
    then Left "uppercase header name"
    else pure ()
  -- Check for connection-specific headers
  let connectionHeaders = ["connection", "keep-alive", "proxy-connection", "transfer-encoding", "upgrade"]
  let hasConnectionHeader = any (\(k, _) -> k `elem` connectionHeaders) regulars
  if hasConnectionHeader
    then Left "connection-specific header"
    else pure ()
  -- TE header must only have "trailers" value
  case lookup "te" regulars of
    Just v | v /= "trailers" -> Left "TE with value other than trailers"
    _ -> pure ()
  pure ()

processTrailers :: Connection -> IORef ServerState -> StreamId -> ByteString -> IO Bool
processTrailers conn stRef sid headerBlock = do
  decoder <- readMVar (connHpackDecoder conn)
  st <- readIORef stRef
  let tableSize = fromIntegral (settingsHeaderTableSize (ssLocalSettings st))
  result <- decodeHeaderBlockWithMaxSize decoder tableSize headerBlock
  case result of
    Left _ -> do
      connError conn stRef CompressionError
      pure False
    Right headers -> do
      let hasPseudo = any (\(k, _) -> BS.isPrefixOf ":" k) headers
      if hasPseudo
        then do
          streamError conn sid ProtocolError
          modifyIORef' stRef $ \s -> s
            { ssStreams = Map.insert sid (StreamInfo StClosed 0) (ssStreams s)
            }
          pure True
        else do
          modifyIORef' stRef $ \s -> s
            { ssStreams = Map.insert sid (StreamInfo StHalfClosedRemote 65535) (ssStreams s)
            }
          pure True

sendSimpleResponse :: Connection -> IORef ServerState -> StreamId -> IO ()
sendSimpleResponse conn stRef sid = do
  encoder <- readMVar (connHpackEncoder conn)
  let responseBody = "ok"
      bodyLen = BS.length responseBody
      headers = [(":status", "200"), ("content-type", "text/plain"),
                 ("content-length", BS.pack (map (fromIntegral . fromEnum) (show bodyLen)))]
  headerBlock <- encodeHeaderBlock defaultEncodeStrategy encoder headers
  let headersFrame = Frame
        (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders
          flagEndHeaders sid)
        (HeadersFrame Nothing headerBlock)
  sendFrame conn headersFrame
  -- Respect flow control: use the remote peer's initial window size for new streams
  st <- readIORef stRef
  let initWindow = fromIntegral (settingsInitialWindowSize (ssRemoteSettings st))
      streamWindow = maybe initWindow siSendWindow (Map.lookup sid (ssStreams st))
      connWindow = ssConnSendWindow st
      maxSend = min streamWindow (min connWindow bodyLen)
  sendFlowControlled conn stRef sid responseBody maxSend

sendFlowControlled :: Connection -> IORef ServerState -> StreamId -> ByteString -> Int -> IO ()
sendFlowControlled conn stRef sid body maxSend = do
  let bodyLen = BS.length body
  if maxSend <= 0 || bodyLen <= 0
    then do
      modifyIORef' stRef $ \s -> s
        { ssStreams = Map.insert sid (StreamInfo StHalfClosedLocal (max 0 maxSend)) (ssStreams s)
        }
    else do
      let sendLen = min bodyLen maxSend
          chunk = BS.take sendLen body
          remaining = BS.drop sendLen body
          isLast = BS.null remaining
          flags = if isLast then flagEndStream else 0
          dataFrame = Frame
            (FrameHeader (fromIntegral sendLen) FrameData flags sid)
            (DataFrame chunk)
      sendFrame conn dataFrame
      if isLast
        then modifyIORef' stRef $ \s -> s
          { ssStreams = Map.insert sid (StreamInfo StClosed 0) (ssStreams s)
          , ssConnSendWindow = ssConnSendWindow s - sendLen
          }
        else modifyIORef' stRef $ \s -> s
          { ssStreams = Map.insert sid
              (StreamInfo StHalfClosedLocal (maxSend - sendLen)) (ssStreams s)
          , ssConnSendWindow = ssConnSendWindow s - sendLen
          }

connError :: Connection -> IORef ServerState -> ErrorCode -> IO ()
connError conn stRef code = do
  st <- readIORef stRef
  if ssGoAwaySent st
    then pure ()
    else do
      writeIORef stRef st { ssGoAwaySent = True }
      closeConnection conn code ""

streamError :: Connection -> StreamId -> ErrorCode -> IO ()
streamError conn sid code = do
  let rst = Frame
        (FrameHeader 4 FrameRSTStream 0 sid)
        (RSTStreamFrame code)
  sendFrame conn rst

checkStreamWindowOverflow :: Connection -> IORef ServerState -> StreamId -> Word32 -> IO Bool
checkStreamWindowOverflow conn stRef sid increment = do
  st <- readIORef stRef
  let mInfo = Map.lookup sid (ssStreams st)
      currentWindow = maybe 65535 siSendWindow mInfo
      newWindow = currentWindow + fromIntegral increment
  if newWindow > 2147483647
    then do
      streamError conn sid FlowControlError
      pure True
    else do
      modifyIORef' stRef $ \s -> s
        { ssStreams = Map.alter (Just . maybe (StreamInfo StOpen newWindow)
            (\i -> i { siSendWindow = newWindow })) sid (ssStreams s)
        }
      pure True

getStreamState :: ServerState -> StreamId -> StreamSt
getStreamState st sid = case Map.lookup sid (ssStreams st) of
  Just info -> siState info
  Nothing
    | sid <= ssLastStreamId st -> StClosed
    | otherwise -> StIdle

validateAndApplySettings :: Settings -> [(Word16, Word32)] -> Either ErrorCode Settings
validateAndApplySettings = go
  where
    go s [] = Right s
    go s ((ident, val):rest) = case ident of
      0x1 -> go s { settingsHeaderTableSize = val } rest
      0x2 -> if val > 1
               then Left ProtocolError
               else go s { settingsEnablePush = val /= 0 } rest
      0x3 -> go s { settingsMaxConcurrentStreams = Just val } rest
      0x4 -> if val > 2147483647
               then Left FlowControlError
               else go s { settingsInitialWindowSize = val } rest
      0x5 -> if val < 16384 || val > 16777215
               then Left ProtocolError
               else go s { settingsMaxFrameSize = val } rest
      0x6 -> go s { settingsMaxHeaderListSize = Just val } rest
      _   -> go s rest

recvExact :: Socket -> Int -> IO ByteString
recvExact sock n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- NBS.recv sock (min remaining 4096)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)
