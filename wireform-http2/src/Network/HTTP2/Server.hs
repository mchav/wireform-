module Network.HTTP2.Server
  ( ServerConfig (..)
  , defaultServerConfig
  , Request (..)
  , Response (..)
  , ResponseBody (..)
  , runServer
  , runServerOnSocket
  , runServerOnTransport
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, catch, SomeException, finally)
import Control.Monad (when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Network.Socket (Socket)
import qualified Network.Socket as NS

import qualified Data.ByteString.Internal as BSI

import Network.HTTP2.Connection
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Types

import Network.HTTP2.Frame.Decode (FrameDecodeError (..))
import Network.HTTP2.Frame.Types (flagPadded, flagPriority)

data ServerConfig = ServerConfig
  { serverSettings :: !Settings
  , serverHost :: !String
  , serverPort :: !String
  , serverHandler :: Request -> (Response -> IO ()) -> IO ()
  , serverForkConnection :: IO () -> IO ThreadId
    -- ^ How to fork a new thread for each accepted connection.
    -- Default: 'forkIO'.
    -- Use 'forkOn n' for pinned-core scheduling,
    -- or 'forkOS' for bound OS threads.
  , serverForkStream :: IO () -> IO ThreadId
    -- ^ How to fork a new thread for each concurrent stream handler.
    -- Default: 'forkIO'.
  }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverSettings = defaultSettings
  , serverHost = "0.0.0.0"
  , serverPort = "8080"
  , serverHandler = \_ respond -> respond defaultResponse
  , serverForkConnection = forkIO
  , serverForkStream = forkIO
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
  , responseTrailers :: ![(ByteString, ByteString)]
    -- ^ Optional HTTP/2 trailer block (RFC 9113 §8.1). When non-empty,
    -- the server sends the body without END_STREAM and then a final
    -- HEADERS frame carrying these fields with END_STREAM set.
    --
    -- Trailers are how gRPC delivers @grpc-status@ + @grpc-message@
    -- at the tail of a response; anything outside gRPC almost never
    -- uses them, so this defaults to @[]@.
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
  , responseTrailers = []
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
  _ <- serverForkConnection cfg $
    handleClient cfg clientSock `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop cfg listenSock

runServerOnSocket :: ServerConfig -> Socket -> IO ()
runServerOnSocket cfg sock = handleClient cfg sock

-- | Run the HTTP/2 server over an arbitrary 'Transport' (e.g. a TLS
-- context). The transport must already be live: the caller is
-- responsible for the TLS handshake / ALPN negotiation.
runServerOnTransport :: ServerConfig -> Transport -> IO ()
runServerOnTransport cfg transport = handleTransport cfg transport

handleClient :: ServerConfig -> Socket -> IO ()
handleClient cfg sock = do
  let transport = socketTransport sock
  handleTransport cfg transport `finally` gracefulClose sock

-- | Half-close the write side first (so any pending GOAWAY / RST
-- frames make it out), then close the socket.
gracefulClose :: Socket -> IO ()
gracefulClose sock = do
  (NS.shutdown sock NS.ShutdownSend) `catch` (\(_ :: SomeException) -> pure ())
  NS.close sock

handleTransport :: ServerConfig -> Transport -> IO ()
handleTransport cfg transport = do
  preface <- recvExactTransport transport (BS.length connectionPreface)
  if preface /= connectionPreface
    then pure ()
    else do
      conn <- newConnectionFromTransport
                RoleServer
                (serverSettings cfg)
                (\_ _ _ -> pure ())
                transport
      sendServerPreface conn (serverSettings cfg)
      connectionLoop cfg conn

-- | Receive exactly @n@ bytes from a 'Transport'. Used only to read the
-- connection preface; the per-connection 'RecvBuffer' takes over after.
recvExactTransport :: Transport -> Int -> IO ByteString
recvExactTransport t n = go n []
  where
    go 0 acc = pure (BS.concat (reverse acc))
    go remaining acc = do
      chunk <- chunkFromTransport t (min remaining 4096)
      if BS.null chunk
        then pure (BS.concat (reverse acc))
        else go (remaining - BS.length chunk) (chunk : acc)

-- | Pull a chunk of up to @n@ bytes via the transport's @recvBuf@.
chunkFromTransport :: Transport -> Int -> IO ByteString
chunkFromTransport t n = BSI.createUptoN n $ \ptr -> tRecvBuf t ptr n

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
    Left err ->
      -- Map decoder errors to the RFC 9113 error code the peer expects.
      closeConnection conn (decodeErrorToCode err) (decodeErrorMessage err)
    Right frame -> do
      ok <- handleFrame' cfg conn frame
      if ok then connectionLoop cfg conn else pure ()

-- | Map a 'FrameDecodeError' to the HTTP\/2 error code dictated by
-- RFC 9113. Most malformed-payload conditions are
-- @FRAME_SIZE_ERROR@; the rest fall back to @PROTOCOL_ERROR@.
decodeErrorToCode :: FrameDecodeError -> ErrorCode
decodeErrorToCode FrameTooShort                = FrameSizeError
decodeErrorToCode PayloadTooShort              = FrameSizeError
decodeErrorToCode InvalidSettingsLength        = FrameSizeError
decodeErrorToCode InvalidWindowUpdateIncrement = ProtocolError
decodeErrorToCode InvalidPadding               = ProtocolError
decodeErrorToCode InvalidStreamId              = ProtocolError

decodeErrorMessage :: FrameDecodeError -> ByteString
decodeErrorMessage = BS.pack . map (fromIntegral . fromEnum) . show

-- | Process a single frame. Returns 'True' to keep reading,
-- 'False' to tear the connection down (a GOAWAY was already sent).
handleFrame' :: ServerConfig -> Connection -> Frame -> IO Bool
handleFrame' cfg conn (Frame hdr payload) = case payload of
  SettingsFrame params
    | testFlag (fhFlags hdr) flagAck -> pure True
    | fhStreamId hdr /= 0 -> do
        closeConnection conn ProtocolError "SETTINGS on non-zero stream"
        pure False
    | otherwise -> do
        case applySettingsParams defaultSettings params of
          Left _ -> do
            closeConnection conn ProtocolError "invalid settings"
            pure False
          Right newSettings -> do
            writeIORef (connRemoteSettings conn) newSettings
            let ack = Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
            sendFrame conn ack
            pure True

  PingFrame opaqueData
    | fhStreamId hdr /= 0 -> do
        closeConnection conn ProtocolError "PING on non-zero stream"
        pure False
    | fhLength hdr /= 8 -> do
        closeConnection conn FrameSizeError "PING payload length must be 8"
        pure False
    | testFlag (fhFlags hdr) flagAck -> pure True
    | otherwise -> do
        let pong = Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame opaqueData)
        sendFrame conn pong
        pure True

  WindowUpdateFrame increment
    | fhLength hdr /= 4 -> do
        closeConnection conn FrameSizeError "WINDOW_UPDATE length must be 4"
        pure False
    | increment == 0 -> do
        -- Connection error if 0 on stream 0; stream error otherwise.
        -- We always treat as connection error here for simplicity.
        if fhStreamId hdr == 0
          then do
            closeConnection conn ProtocolError "WINDOW_UPDATE with zero increment"
            pure False
          else do
            sendRstStream conn (fhStreamId hdr) ProtocolError
            pure True
    | fhStreamId hdr == 0 -> do
        atomically $ do
          _ <- releaseWindow (connSendFlowControl conn) (fromIntegral increment)
          pure ()
        pure True
    | otherwise -> pure True  -- per-stream window update, ignored at this layer

  GoAwayFrame lastId code debug -> do
    connOnGoAway conn lastId code debug
    pure True

  HeadersFrame _mpri rawBlock
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "HEADERS on stream 0"
        pure False
    | not (testFlag (fhFlags hdr) flagEndHeaders) -> do
        -- CONTINUATION-fragmented HEADERS not supported.
        closeConnection conn ProtocolError "fragmented HEADERS unsupported"
        pure False
    | otherwise -> case stripPaddingAndPriority FrameHeaders (fhFlags hdr) rawBlock of
        Nothing -> do
          closeConnection conn ProtocolError "malformed HEADERS frame"
          pure False
        Just headerBlock -> do
          decoder <- readMVar (connHpackDecoder conn)
          result <- decodeHeaderBlock decoder headerBlock
          case result of
            Left _ -> do
              closeConnection conn CompressionError "HPACK decode failed"
              pure False
            Right headers -> do
              writeIORef (connLastStreamId conn) (fhStreamId hdr)
              let req = buildRequest (fhStreamId hdr) headers (testFlag (fhFlags hdr) flagEndStream)
              _ <- serverForkStream cfg $
                serverHandler cfg req $ \resp ->
                  sendResponse conn (fhStreamId hdr) resp
              pure True

  DataFrame body
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "DATA on stream 0"
        pure False
    | otherwise -> case stripPaddingAndPriority FrameData (fhFlags hdr) body of
        Nothing -> do
          closeConnection conn ProtocolError "malformed DATA frame"
          pure False
        Just _ -> do
          let len = fhLength hdr
          when (len > 0) $ do
            sendFrame conn $ Frame
              (FrameHeader 4 FrameWindowUpdate 0 0) (WindowUpdateFrame len)
            sendFrame conn $ Frame
              (FrameHeader 4 FrameWindowUpdate 0 (fhStreamId hdr)) (WindowUpdateFrame len)
          pure True

  RSTStreamFrame _
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "RST_STREAM on stream 0"
        pure False
    | fhLength hdr /= 4 -> do
        closeConnection conn FrameSizeError "RST_STREAM length must be 4"
        pure False
    | otherwise -> pure True

  _ -> pure True

-- | Send a stream-scoped RST_STREAM with the given error code.
sendRstStream :: Connection -> StreamId -> ErrorCode -> IO ()
sendRstStream conn sid code =
  sendFrame conn $ Frame
    (FrameHeader 4 FrameRSTStream 0 sid)
    (RSTStreamFrame code)

-- | Strip the PADDED + PRIORITY prefixes from a frame payload per
-- RFC 9113. PADDED is recognised on DATA / HEADERS / PUSH_PROMISE
-- / CONTINUATION; PRIORITY is recognised on HEADERS only.
--
-- Returns 'Nothing' if the frame is malformed (pad length exceeds
-- payload, priority prefix missing, etc.).
stripPaddingAndPriority
  :: FrameType -> FrameFlags -> ByteString -> Maybe ByteString
stripPaddingAndPriority ft flags bs0 = do
  bs1 <- stripPadding flags bs0
  if ft == FrameHeaders && testFlag flags flagPriority
    then stripPriority bs1
    else Just bs1
  where
    stripPadding fl bs
      | not (testFlag fl flagPadded) = Just bs
      | BS.null bs = Nothing
      | otherwise =
          let padLen = fromIntegral (BS.head bs)
              total = BS.length bs
          in if padLen + 1 > total
               then Nothing
               else Just (BS.take (total - padLen - 1) (BS.drop 1 bs))

    stripPriority bs
      | BS.length bs >= 5 = Just (BS.drop 5 bs)
      | otherwise = Nothing

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
      trailers = responseTrailers resp
      hasTrailers = not (null trailers)
  headerBlock <- encodeHeaderBlock defaultEncodeStrategy encoder allHeaders
  case responseBody resp of
    ResponseBodyEmpty
      | hasTrailers -> do
          -- HEADERS (status, !END_STREAM) → HEADERS (trailers, END_STREAM)
          sendHeadersFrame conn sid headerBlock False
          sendTrailers conn sid trailers
      | otherwise ->
          sendHeadersFrame conn sid headerBlock True
    ResponseBodyBS body -> do
      sendHeadersFrame conn sid headerBlock False
      let dataEndStream = not hasTrailers
          dataFlags = if dataEndStream then flagEndStream else 0
          dataFrame = Frame
            (FrameHeader (fromIntegral (BS.length body)) FrameData dataFlags sid)
            (DataFrame body)
      sendFrame conn dataFrame
      if hasTrailers
        then sendTrailers conn sid trailers
        else pure ()
    ResponseBodyStream producer -> do
      sendHeadersFrame conn sid headerBlock False
      streamBody conn sid producer hasTrailers
      if hasTrailers
        then sendTrailers conn sid trailers
        else pure ()

-- | Send a HEADERS frame with END_HEADERS set (and optionally
-- END_STREAM). Used for both the response head and the trailer block.
sendHeadersFrame :: Connection -> StreamId -> ByteString -> Bool -> IO ()
sendHeadersFrame conn sid headerBlock endStream = do
  let flags = flagEndHeaders .|. (if endStream then flagEndStream else 0)
      frame = Frame
        (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders flags sid)
        (HeadersFrame Nothing headerBlock)
  sendFrame conn frame

-- | Emit the trailer block as a final HEADERS frame with END_STREAM.
sendTrailers :: Connection -> StreamId -> [(ByteString, ByteString)] -> IO ()
sendTrailers conn sid trailers = do
  encoder <- readMVar (connHpackEncoder conn)
  block <- encodeHeaderBlock defaultEncodeStrategy encoder trailers
  sendHeadersFrame conn sid block True

streamBody :: Connection -> StreamId -> IO (Maybe ByteString) -> Bool -> IO ()
streamBody conn sid producer hasTrailers = do
  mChunk <- producer
  case mChunk of
    Nothing -> do
      -- End of body. If trailers follow, send an empty DATA *without*
      -- END_STREAM and let the caller's trailer HEADERS carry it;
      -- otherwise close the stream with an empty DATA + END_STREAM.
      let endStreamHere = not hasTrailers
          flags = if endStreamHere then flagEndStream else 0
          emptyData = Frame
            (FrameHeader 0 FrameData flags sid)
            (DataFrame "")
      sendFrame conn emptyData
    Just chunk -> do
      let dataFrame = Frame
            (FrameHeader (fromIntegral (BS.length chunk)) FrameData 0 sid)
            (DataFrame chunk)
      sendFrame conn dataFrame
      streamBody conn sid producer hasTrailers

