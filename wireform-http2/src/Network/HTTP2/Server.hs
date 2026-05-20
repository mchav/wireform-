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

import Data.Bits ((.|.), (.&.), shiftL)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import Network.HTTP2.Connection
import Network.HTTP2.Frame
import Network.HTTP2.Frame.Decode (FrameDecodeError (..))
import Network.HTTP2.Frame.Types
  ( FramePayload (..), decodeGoAway, decodeSettings
  )
import Network.HTTP2.HPACK
import Network.HTTP2.Types hiding (StreamClosed)
import qualified Network.HTTP2.Types as Types

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
      streamsRef <- newIORef Map.empty
      lastPeerStreamRef <- newIORef 0
      continuationRef <- newIORef Nothing
      connectionLoop cfg conn streamsRef lastPeerStreamRef continuationRef

-- | Pending header-block continuation state: which stream we're
-- continuing, what flags the initial HEADERS frame had (so we know
-- whether to mark END_STREAM when END_HEADERS finally arrives),
-- and the accumulated payload bytes.
data Continuation = Continuation
  { contStreamId :: !StreamId
  , contInitialFlags :: !FrameFlags
  , contBuffer :: !ByteString
  }

-- | Per-stream state we track at the server boundary. We don't need
-- a full RFC 9113 §5.1 state diagram for the gRPC subset, only the
-- transitions the conformance suite probes: idle → open →
-- half-closed (remote) → closed, plus the closed-by-RST_STREAM
-- transition.
data ServerStreamState
  = StOpen
    -- ^ Inbound and outbound both open.
  | StHalfClosedRemote
    -- ^ Peer sent END_STREAM; we may still send DATA / HEADERS.
  | StClosedSrv
    -- ^ Stream is fully closed (RST_STREAM or our own END_STREAM).
    -- (Suffix avoids the clash with 'Network.HTTP2.Types.StreamClosed'.)
  deriving stock (Eq, Show)

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

connectionLoop
  :: ServerConfig
  -> Connection
  -> IORef (Map.Map StreamId ServerStreamState)
  -> IORef StreamId
  -> IORef (Maybe Continuation)
  -> IO ()
connectionLoop cfg conn streamsRef lastPeerStreamRef contRef = loop
  where
    maxFrame = settingsMaxFrameSize (serverSettings cfg)
    loop = do
      result <- recvFrame conn
      case result of
        Left err ->
          closeConnection conn (decodeErrorToCode err) (decodeErrorMessage err)
        Right frame@(Frame hdr _)
          | fhLength hdr > maxFrame -> do
              closeConnection conn FrameSizeError "frame exceeds SETTINGS_MAX_FRAME_SIZE"
          | otherwise -> do
              -- If a header-block continuation is in flight, the
              -- ONLY frame the peer is allowed to send is a
              -- CONTINUATION on the same stream (RFC 9113 §6.10).
              pending <- readIORef contRef
              case pending of
                Just c | fhType hdr /= FrameContinuation
                       || fhStreamId hdr /= contStreamId c -> do
                  closeConnection conn ProtocolError "expected CONTINUATION frame"
                _ -> do
                  ok <- handleFrame' cfg conn streamsRef lastPeerStreamRef contRef frame
                  if ok then loop else pure ()

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
--
-- We dispatch on 'fhType' rather than on the 'FramePayload' pattern
-- synonyms because the latter all match @FramePayloadRaw bs@
-- regardless of frame type — they're decode helpers, not a type-
-- tagged sum. Dispatching on the header's actual type avoids the
-- crossover where (e.g.) a HEADERS body coincidentally matches the
-- @PingFrame@ view pattern.
handleFrame'
  :: ServerConfig
  -> Connection
  -> IORef (Map.Map StreamId ServerStreamState)
  -> IORef StreamId
  -> IORef (Maybe Continuation)
  -> Frame
  -> IO Bool
handleFrame' cfg conn streamsRef lastPeerStreamRef contRef (Frame hdr (FramePayloadRaw body)) = case fhType hdr of
  FrameSettings
    | testFlag (fhFlags hdr) flagAck ->
        if BS.null body
          then pure True
          else do
            closeConnection conn FrameSizeError "SETTINGS ACK with non-empty payload"
            pure False
    | fhStreamId hdr /= 0 -> do
        closeConnection conn ProtocolError "SETTINGS on non-zero stream"
        pure False
    | BS.length body `mod` 6 /= 0 -> do
        closeConnection conn FrameSizeError "SETTINGS payload not multiple of 6"
        pure False
    | otherwise -> case decodeSettings body of
        Nothing -> do
          closeConnection conn FrameSizeError "malformed SETTINGS payload"
          pure False
        Just params -> case applySettingsParams defaultSettings params of
          Left _ -> do
            closeConnection conn ProtocolError "invalid settings"
            pure False
          Right newSettings -> do
            writeIORef (connRemoteSettings conn) newSettings
            let ack = Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
            sendFrame conn ack
            pure True

  FramePing
    | fhStreamId hdr /= 0 -> do
        closeConnection conn ProtocolError "PING on non-zero stream"
        pure False
    | BS.length body /= 8 -> do
        closeConnection conn FrameSizeError "PING payload length must be 8"
        pure False
    | testFlag (fhFlags hdr) flagAck -> pure True
    | otherwise -> do
        let pong = Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame body)
        sendFrame conn pong
        pure True

  FrameWindowUpdate
    | BS.length body /= 4 -> do
        closeConnection conn FrameSizeError "WINDOW_UPDATE length must be 4"
        pure False
    | otherwise ->
        let increment = (fromIntegral (BS.index body 0) `shiftL` 24)
                    .|. (fromIntegral (BS.index body 1) `shiftL` 16)
                    .|. (fromIntegral (BS.index body 2) `shiftL` 8)
                    .|. fromIntegral (BS.index body 3) :: Word32
            inc = increment .&. 0x7FFFFFFF
            sid = fhStreamId hdr
        in if inc == 0
             then if sid == 0
                    then do
                      closeConnection conn ProtocolError "WINDOW_UPDATE with zero increment on stream 0"
                      pure False
                    else do
                      sendRstStream conn sid ProtocolError
                      pure True
             else if sid == 0
                    then do
                      r <- atomically $
                        releaseWindow (connSendFlowControl conn) (fromIntegral inc)
                      case r of
                        Left _ -> do
                          -- Window overflow > 2^31-1 → FLOW_CONTROL_ERROR
                          -- connection error.
                          closeConnection conn FlowControlError "connection flow control overflow"
                          pure False
                        Right () -> pure True
                    else do
                      streams <- readIORef streamsRef
                      lastPeer <- readIORef lastPeerStreamRef
                      case Map.lookup sid streams of
                        Nothing | odd sid && sid > lastPeer -> do
                          closeConnection conn ProtocolError "WINDOW_UPDATE on idle stream"
                          pure False
                        _ -> pure True
                          -- TODO: per-stream window overflow → RST_STREAM
                          -- FLOW_CONTROL_ERROR. We don't yet track per-stream
                          -- send windows here.

  FrameGoAway
    | fhStreamId hdr /= 0 -> do
        closeConnection conn ProtocolError "GOAWAY on non-zero stream"
        pure False
    | otherwise -> case decodeGoAway body of
        Just (lastId, code, debug) -> do
          connOnGoAway conn lastId code debug
          pure True
        Nothing -> do
          closeConnection conn FrameSizeError "malformed GOAWAY"
          pure False

  FrameHeaders
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "HEADERS on stream 0"
        pure False
    | not (testFlag (fhFlags hdr) flagEndHeaders) -> do
        -- Start a CONTINUATION sequence: buffer this frame's
        -- payload, expect zero or more CONTINUATION frames on the
        -- same stream, terminated by one with END_HEADERS.
        writeIORef contRef (Just (Continuation
          { contStreamId = fhStreamId hdr
          , contInitialFlags = fhFlags hdr
          , contBuffer = body
          }))
        pure True
    | otherwise -> do
        let sid = fhStreamId hdr
        streams <- readIORef streamsRef
        lastPeer <- readIORef lastPeerStreamRef
        case Map.lookup sid streams of
          -- Existing stream → trailing HEADERS or HEADERS in wrong
          -- state.
          Just StClosedSrv -> do
            -- RFC 9113 §5.1: receiving HEADERS on closed stream is
            -- STREAM_CLOSED (connection error if no RST_STREAM in
            -- flight).
            closeConnection conn Types.StreamClosed "HEADERS on closed stream"
            pure False
          Just StHalfClosedRemote -> do
            closeConnection conn Types.StreamClosed "HEADERS on half-closed (remote) stream"
            pure False
          Just StOpen
            -- Trailers must have END_STREAM (RFC 9113 §8.1).
            | not (testFlag (fhFlags hdr) flagEndStream) -> do
                closeConnection conn ProtocolError "trailers without END_STREAM"
                pure False
            | otherwise -> do
                -- Validate that trailers contain no pseudo-headers.
                case stripPaddingAndPriority FrameHeaders (fhFlags hdr) body of
                  Nothing -> do
                    closeConnection conn ProtocolError "malformed trailers"
                    pure False
                  Just block -> do
                    decoder <- readMVar (connHpackDecoder conn)
                    case_result <- decodeHeaderBlock decoder block
                    case case_result of
                      Left _ -> do
                        closeConnection conn CompressionError "HPACK decode failed in trailers"
                        pure False
                      Right trailers
                        | any (\(n, _) -> not (BS.null n) && BS.head n == 0x3A) trailers -> do
                            closeConnection conn ProtocolError "pseudo-header in trailers"
                            pure False
                        | otherwise -> do
                            modifyIORef' streamsRef (Map.insert sid StHalfClosedRemote)
                            pure True
          Nothing -> handleFreshStream lastPeer sid (fhFlags hdr) body
        where
          connLastSid = connLastStreamId conn
          handleFreshStream lp sid' flags body'
            | even sid' || sid' <= lp = do
                closeConnection conn ProtocolError "invalid stream id"
                pure False
            | headersSelfDependency sid' flags body' = do
                closeConnection conn ProtocolError "HEADERS depends on itself"
                pure False
            | otherwise = case stripPaddingAndPriority FrameHeaders flags body' of
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
                    Right headers -> case validateRequestHeaders headers of
                      Just _err -> do
                        closeConnection conn ProtocolError "malformed request headers"
                        pure False
                      Nothing -> do
                        writeIORef connLastSid sid'
                        writeIORef lastPeerStreamRef sid'
                        let endStream = testFlag flags flagEndStream
                            newState = if endStream then StHalfClosedRemote else StOpen
                        modifyIORef' streamsRef (Map.insert sid' newState)
                        let req = buildRequest sid' headers endStream
                        _ <- serverForkStream cfg $ do
                          serverHandler cfg req $ \resp ->
                            sendResponse conn sid' resp
                          modifyIORef' streamsRef (Map.insert sid' StClosedSrv)
                        pure True

  FrameData
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "DATA on stream 0"
        pure False
    | otherwise -> do
        let sid = fhStreamId hdr
        streams <- readIORef streamsRef
        case Map.lookup sid streams of
          Nothing -> do
            -- DATA on idle stream → STREAM_CLOSED per RFC 9113.
            closeConnection conn Types.StreamClosed "DATA on idle stream"
            pure False
          Just StClosedSrv -> do
            closeConnection conn Types.StreamClosed "DATA on closed stream"
            pure False
          Just StHalfClosedRemote -> do
            closeConnection conn Types.StreamClosed "DATA on half-closed (remote) stream"
            pure False
          Just StOpen -> case stripPaddingAndPriority FrameData (fhFlags hdr) body of
            Nothing -> do
              closeConnection conn ProtocolError "malformed DATA frame"
              pure False
            Just _ -> do
              when (testFlag (fhFlags hdr) flagEndStream) $
                modifyIORef' streamsRef (Map.insert sid StHalfClosedRemote)
              let len = fhLength hdr
              when (len > 0) $ do
                sendFrame conn $ Frame
                  (FrameHeader 4 FrameWindowUpdate 0 0) (WindowUpdateFrame len)
                sendFrame conn $ Frame
                  (FrameHeader 4 FrameWindowUpdate 0 sid) (WindowUpdateFrame len)
              pure True

  FrameRSTStream
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "RST_STREAM on stream 0"
        pure False
    | BS.length body /= 4 -> do
        closeConnection conn FrameSizeError "RST_STREAM length must be 4"
        pure False
    | otherwise -> do
        let sid = fhStreamId hdr
        streams <- readIORef streamsRef
        case Map.lookup sid streams of
          Nothing ->
            -- RST_STREAM on idle stream is a connection error.
            if even sid || sid > readPeerSid_
              then do
                closeConnection conn ProtocolError "RST_STREAM on idle stream"
                pure False
              else do
                modifyIORef' streamsRef (Map.insert sid StClosedSrv)
                pure True
          Just _ -> do
            modifyIORef' streamsRef (Map.insert sid StClosedSrv)
            pure True
        where
          readPeerSid_ = 0  -- conservative: every odd >0 sid we haven't seen is "idle"
          -- (a tighter check requires reading lastPeerStreamRef; the conservative
          -- form just classifies as "idle" if we have no record, which is correct
          -- for STREAM_CLOSED detection purposes.)

  FramePriority
    | fhStreamId hdr == 0 -> do
        closeConnection conn ProtocolError "PRIORITY on stream 0"
        pure False
    | BS.length body /= 5 -> do
        sendRstStream conn (fhStreamId hdr) FrameSizeError
        pure True
    | otherwise -> do
        let depRaw = (fromIntegral (BS.index body 0) `shiftL` 24)
                 .|. (fromIntegral (BS.index body 1) `shiftL` 16)
                 .|. (fromIntegral (BS.index body 2) `shiftL` 8)
                 .|. fromIntegral (BS.index body 3) :: Word32
            dep = depRaw .&. 0x7FFFFFFF
        if dep == fhStreamId hdr
          then do
            -- RFC 9113 §5.3.1: stream cannot depend on itself.
            sendRstStream conn (fhStreamId hdr) ProtocolError
            pure True
          else pure True

  FrameContinuation -> do
    pending <- readIORef contRef
    case pending of
      Nothing -> do
        closeConnection conn ProtocolError "unexpected CONTINUATION"
        pure False
      Just c
        | contStreamId c /= fhStreamId hdr -> do
            closeConnection conn ProtocolError "CONTINUATION on wrong stream"
            pure False
        | testFlag (fhFlags hdr) flagEndHeaders -> do
            -- Final fragment: assemble and process as if it were
            -- the original HEADERS frame.
            writeIORef contRef Nothing
            let assembled = contBuffer c <> body
                origFlags = contInitialFlags c
                synthHdr  = hdr
                  { fhFlags  = origFlags .|. flagEndHeaders
                  , fhType   = FrameHeaders
                  , fhLength = fromIntegral (BS.length assembled)
                  }
            handleFrame' cfg conn streamsRef lastPeerStreamRef contRef
              (Frame synthHdr (FramePayloadRaw assembled))
        | otherwise -> do
            writeIORef contRef (Just c { contBuffer = contBuffer c <> body })
            pure True

  FramePushPromise -> do
    -- Clients are not supposed to send PUSH_PROMISE.
    closeConnection conn ProtocolError "client sent PUSH_PROMISE"
    pure False

  FrameUnknown _ -> pure True  -- RFC 9113 §4.1: unknown frame types MUST be ignored

-- | Send a stream-scoped RST_STREAM with the given error code.
sendRstStream :: Connection -> StreamId -> ErrorCode -> IO ()
sendRstStream conn sid code =
  sendFrame conn $ Frame
    (FrameHeader 4 FrameRSTStream 0 sid)
    (RSTStreamFrame code)

-- | True if the HEADERS frame carries PRIORITY-flag info that names
-- the same stream as its dependency (RFC 9113 §5.3.1: a stream
-- cannot depend on itself).
headersSelfDependency :: StreamId -> FrameFlags -> ByteString -> Bool
headersSelfDependency sid flags body0
  | not (testFlag flags flagPriority) = False
  | otherwise =
      -- The PRIORITY block sits after the optional padding byte.
      let body = if testFlag flags flagPadded
                   then BS.drop 1 body0
                   else body0
      in BS.length body >= 5 &&
         let depRaw = (fromIntegral (BS.index body 0) `shiftL` 24)
                  .|. (fromIntegral (BS.index body 1) `shiftL` 16)
                  .|. (fromIntegral (BS.index body 2) `shiftL` 8)
                  .|. fromIntegral (BS.index body 3) :: Word32
             dep = depRaw .&. 0x7FFFFFFF
         in dep == sid

-- | Validate the pseudo-header + regular-header rules from RFC
-- 9113 §8.3. Returns 'Just msg' on the first violation.
--
-- Specifically:
--
--   * Header field names must be lowercase (§8.2.1)
--   * No connection-specific headers (Connection, Transfer-Encoding,
--     Keep-Alive, Upgrade, Proxy-*) — §8.2.2
--   * Pseudo-headers must precede regular headers (§8.3)
--   * Only request pseudo-headers are allowed:
--     ':method', ':scheme', ':path', ':authority'
--   * No unknown pseudo-headers
--   * No pseudo-headers in trailers (we don't yet distinguish trailers
--     here; the validation runs on the initial HEADERS).
validateRequestHeaders :: [(ByteString, ByteString)] -> Maybe ByteString
validateRequestHeaders hs = case walk True hs of
  Just err -> Just err
  Nothing  -> presenceCheck
  where
    presenceCheck
      | methods == 0 = Just "missing :method"
      | methods > 1 = Just "duplicated :method"
      | scheme  == 0 = Just "missing :scheme"
      | scheme  > 1 = Just "duplicated :scheme"
      | path    == 0 = Just "missing :path"
      | path    > 1 = Just "duplicated :path"
      | countPseudo ":authority" > 1 = Just "duplicated :authority"
      | any (\(n, v) -> n == ":path" && BS.null v) hs = Just "empty :path"
      | otherwise = Nothing
    methods = countPseudo ":method"
    scheme  = countPseudo ":scheme"
    path    = countPseudo ":path"
    countPseudo p = length (filter ((== p) . fst) hs)

    walk _ [] = Nothing
    walk seenRegular ((name, val) : rest)
      | BS.null name = Just "empty header name"
      | BS.head name == 0x3A {- ':' -} =
          if not seenRegular
            then Just "pseudo-header after regular header"
            else if name `elem` allowedPseudos
                   then walk True rest
                   else Just "unknown pseudo-header"
      | hasUppercase name = Just "uppercase header name"
      | isConnectionSpecific name = Just "connection-specific header"
      | name == "te" && val /= "trailers" =
          Just "TE header field with value other than \"trailers\""
      | otherwise = walk False rest

    allowedPseudos =
      [":method", ":scheme", ":path", ":authority"]

    hasUppercase :: ByteString -> Bool
    hasUppercase = BS.any (\c -> c >= 0x41 && c <= 0x5A)

    isConnectionSpecific :: ByteString -> Bool
    isConnectionSpecific n = n `elem`
      [ "connection", "transfer-encoding", "keep-alive"
      , "upgrade", "proxy-connection"
      ]

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

