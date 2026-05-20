module Network.HTTP2.Client
  ( ClientConfig (..)
  , defaultClientConfig
  , ClientRequest (..)
  , ClientResponse (..)
  , RequestBody (..)
    -- * Connection
  , ClientHandle
  , clientHandleConnection
  , withConnection
  , withConnectionOnTransport
    -- * Requests
  , sendRequest
  , sendRequestStreamId
    -- * Errors
  , ClientStreamError (..)
    -- * Low-level pieces, re-used by TLS bring-up
  , sendClientPreface
  , clientRecvLoop
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, bracket, finally, throwIO)
import Data.Bits ((.|.), (.&.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import qualified Network.Socket as NS

import Network.HTTP2.Connection
import Network.HTTP2.Frame
import Network.HTTP2.Frame.Types (decodeGoAway, decodeSettings)
import Network.HTTP2.HPACK
import Network.HTTP2.Types

data ClientConfig = ClientConfig
  { clientSettings :: !Settings
  , clientHost :: !String
  , clientPort :: !String
  }

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
  { clientSettings = defaultSettings
  , clientHost = "127.0.0.1"
  , clientPort = "80"
  }

data ClientRequest = ClientRequest
  { crMethod :: !ByteString
  , crPath :: !ByteString
  , crScheme :: !ByteString
  , crAuthority :: !ByteString
  , crHeaders :: ![(ByteString, ByteString)]
  , crBody :: !RequestBody
  }

-- | Outbound request body.
--
-- @ReqBodyStream@ is a pull producer that yields chunks until it
-- returns 'Nothing'. The client splits each chunk to the peer's
-- @SETTINGS_MAX_FRAME_SIZE@ and blocks on the connection-level
-- send flow-control window (HTTP\/2 §6.9) before pushing each
-- DATA frame; back-pressure on the peer back-propagates to the
-- producer naturally.
data RequestBody
  = ReqBodyNone
  | ReqBodyBytes !ByteString
  | ReqBodyStream !(IO (Maybe ByteString))

data ClientResponse = ClientResponse
  { crStatus :: !Int
  , crResponseHeaders :: ![(ByteString, ByteString)]
  , crResponseBody :: !ByteString
  }
  deriving stock (Eq, Show)

-- | Per-stream inbox the recv loop pushes into.
data StreamInbox = StreamInbox
  { siHeaders :: !(MVar (Either ClientStreamError ResponseHead))
    -- ^ Filled exactly once when the response @HEADERS@ block arrives
    -- (or when the stream is reset before any headers).
  , siBody :: !(TBQueue BodyItem)
    -- ^ Chunks streamed in by DATA frames, terminated by 'BodyEnd' or
    -- 'BodyError'.
  }

data ResponseHead = ResponseHead
  { rhStatus :: !Int
  , rhHeaders :: ![(ByteString, ByteString)]
  }

data BodyItem
  = BodyChunk !ByteString
  | BodyEnd
  | BodyError !ClientStreamError

-- | Errors that can be observed on a single response stream.
data ClientStreamError
  = ClientStreamReset !ErrorCode
    -- ^ The peer sent @RST_STREAM@.
  | ClientStreamProtocolError !ByteString
    -- ^ The peer violated the HTTP\/2 wire protocol on this stream.
  | ClientStreamConnectionClosed
    -- ^ The connection went away before the stream completed.
  deriving stock (Eq, Show)

instance Exception ClientStreamError

-- | A live client connection plus the bookkeeping needed to collect
-- per-stream responses. Acquire with 'withConnection' or
-- 'withConnectionOnTransport'.
data ClientHandle = ClientHandle
  { chConnection :: !Connection
  , chStreams :: !(IORef (Map.Map StreamId StreamInbox))
  , chPending :: !(IORef (Maybe Continuation))
  }

clientHandleConnection :: ClientHandle -> Connection
clientHandleConnection = chConnection

-- | Pending header-block continuation state.
data Continuation = Continuation
  { contStreamId :: !StreamId
  , contInitialFlags :: !FrameFlags
  , contBuffer :: !ByteString
  }

withConnection :: ClientConfig -> (ClientHandle -> IO a) -> IO a
withConnection cfg action = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrs <- NS.getAddrInfo (Just hints) (Just (clientHost cfg)) (Just (clientPort cfg))
  case addrs of
    [] -> error "No address found"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.connect sock (NS.addrAddress addr)
        NS.setSocketOption sock NS.NoDelay 1
        let transport = socketTransport sock
        withConnectionOnTransport cfg transport (Just sock) action

-- | Run the client over an already-prepared 'Transport'. Useful for
-- HTTP/2-over-TLS bring-up where the caller has already done the TLS
-- handshake / ALPN negotiation.
--
-- The optional 'NS.Socket' is the original socket (if any), retained on
-- the 'Connection' so higher-level code can inspect peer addresses.
withConnectionOnTransport
  :: ClientConfig
  -> Transport
  -> Maybe NS.Socket
  -> (ClientHandle -> IO a)
  -> IO a
withConnectionOnTransport cfg transport mSock action = do
  conn <- newConnection ConnectionConfig
    { ccRole = RoleClient
    , ccSettings = clientSettings cfg
    , ccSocket = mSock
    , ccTransport = Just transport
    , ccOnGoAway = \_ _ _ -> pure ()
    }
  sendClientPreface conn (clientSettings cfg)
  streamsRef <- newIORef Map.empty
  pendingRef <- newIORef Nothing
  let handle = ClientHandle conn streamsRef pendingRef
  _ <- forkIO $ clientRecvLoop handle `finally` failOutstanding handle
  action handle `finally` closeConnection conn NoError ""

sendClientPreface :: Connection -> Settings -> IO ()
sendClientPreface conn settings = do
  let preface = connectionPreface
      params = encodeSettings settings
      settingsFrame = Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
  tSendMany (connTransport conn) [preface, encodeFrame settingsFrame]

------------------------------------------------------------------------
-- Recv loop
------------------------------------------------------------------------

-- | Frame receive loop. Runs in its own thread (forked by
-- 'withConnectionOnTransport'). Dispatch is on the 'FrameType' carried
-- in the header — the 'FramePayload' pattern synonyms all match
-- 'FramePayloadRaw' regardless of frame type, so dispatching by
-- pattern would conflate unrelated frame shapes.
clientRecvLoop :: ClientHandle -> IO ()
clientRecvLoop handle = loop
  where
    conn = chConnection handle
    loop = do
      result <- recvFrame conn
      case result of
        Left _ -> pure ()
        Right frame -> do
          ok <- handleClientFrame handle frame
          if ok then loop else pure ()

handleClientFrame :: ClientHandle -> Frame -> IO Bool
handleClientFrame handle (Frame hdr (FramePayloadRaw body)) = case fhType hdr of
  FrameSettings
    | testFlag (fhFlags hdr) flagAck -> pure True
    | otherwise -> case decodeSettings body of
        Nothing -> do
          closeConnection (chConnection handle) FrameSizeError "malformed SETTINGS payload"
          pure False
        Just params -> case applySettingsParams defaultSettings params of
          Left _ -> do
            closeConnection (chConnection handle) ProtocolError "invalid settings"
            pure False
          Right newSettings -> do
            writeIORef (connRemoteSettings (chConnection handle)) newSettings
            let ack = Frame (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])
            sendFrame (chConnection handle) ack
            pure True

  FramePing
    | testFlag (fhFlags hdr) flagAck -> pure True
    | otherwise -> do
        let pong = Frame (FrameHeader 8 FramePing flagAck 0) (PingFrame body)
        sendFrame (chConnection handle) pong
        pure True

  FrameWindowUpdate
    | BS.length body /= 4 -> pure True
    | fhStreamId hdr == 0 -> do
        let inc = readWord32 body .&. 0x7FFFFFFF
        atomically $ do
          _ <- releaseWindow (connSendFlowControl (chConnection handle))
                              (fromIntegral inc)
          pure ()
        pure True
    | otherwise -> pure True

  FrameGoAway -> do
    case decodeGoAway body of
      Just (lastId, code, debug) ->
        connOnGoAway (chConnection handle) lastId code debug
      Nothing -> pure ()
    pure True

  FrameRSTStream
    | BS.length body /= 4 -> pure True
    | otherwise -> do
        let sid = fhStreamId hdr
            code = word32ToErrorCodeLocal (readWord32 body)
        failStream handle sid (ClientStreamReset code)
        pure True

  FrameHeaders -> do
    let sid = fhStreamId hdr
        flags = fhFlags hdr
    if not (testFlag flags flagEndHeaders)
      then do
        writeIORef (chPending handle) (Just Continuation
          { contStreamId = sid
          , contInitialFlags = flags
          , contBuffer = body
          })
        pure True
      else case stripHeaderPadding flags body of
        Nothing -> do
          failStream handle sid (ClientStreamProtocolError "malformed HEADERS frame")
          pure True
        Just block -> deliverHeaders handle sid flags block

  FrameContinuation -> do
    pending <- readIORef (chPending handle)
    case pending of
      Nothing -> pure True
      Just c
        | contStreamId c /= fhStreamId hdr -> pure True
        | testFlag (fhFlags hdr) flagEndHeaders -> do
            writeIORef (chPending handle) Nothing
            let assembled = contBuffer c <> body
                origFlags = contInitialFlags c
            case stripHeaderPadding origFlags assembled of
              Nothing -> do
                failStream handle (fhStreamId hdr)
                  (ClientStreamProtocolError "malformed HEADERS continuation")
                pure True
              Just block ->
                deliverHeaders handle (fhStreamId hdr) origFlags block
        | otherwise -> do
            writeIORef (chPending handle)
              (Just c { contBuffer = contBuffer c <> body })
            pure True

  FrameData -> do
    let sid = fhStreamId hdr
        endStream = testFlag (fhFlags hdr) flagEndStream
    case stripDataPadding (fhFlags hdr) body of
      Nothing -> do
        failStream handle sid (ClientStreamProtocolError "malformed DATA frame")
        pure True
      Just payload -> do
        inboxes <- readIORef (chStreams handle)
        case Map.lookup sid inboxes of
          Nothing -> pure True
          Just inbox -> do
            atomically $ writeTBQueue (siBody inbox) (BodyChunk payload)
            let len = fhLength hdr
            sendFrame (chConnection handle) $
              Frame (FrameHeader 4 FrameWindowUpdate 0 0) (WindowUpdateFrame len)
            sendFrame (chConnection handle) $
              Frame (FrameHeader 4 FrameWindowUpdate 0 sid) (WindowUpdateFrame len)
            if endStream
              then do
                atomically $ writeTBQueue (siBody inbox) BodyEnd
                modifyIORef' (chStreams handle) (Map.delete sid)
              else pure ()
            pure True

  _ -> pure True

-- | Decode a HEADERS block, populate the inbox, and (if END_STREAM is
-- set on the original HEADERS frame) close the stream.
deliverHeaders
  :: ClientHandle -> StreamId -> FrameFlags -> ByteString -> IO Bool
deliverHeaders handle sid flags block = do
  decoder <- readMVar (connHpackDecoder (chConnection handle))
  res <- decodeHeaderBlock decoder block
  case res of
    Left _ -> do
      closeConnection (chConnection handle) CompressionError "HPACK decode failed"
      pure False
    Right headers -> do
      let (status, rest) = splitStatus headers
      inboxes <- readIORef (chStreams handle)
      case Map.lookup sid inboxes of
        Nothing -> pure True
        Just inbox -> do
          _ <- tryPutMVar (siHeaders inbox) (Right ResponseHead
            { rhStatus = status
            , rhHeaders = rest
            })
          if testFlag flags flagEndStream
            then do
              atomically $ writeTBQueue (siBody inbox) BodyEnd
              modifyIORef' (chStreams handle) (Map.delete sid)
            else pure ()
          pure True

splitStatus
  :: [(ByteString, ByteString)] -> (Int, [(ByteString, ByteString)])
splitStatus = go 0 []
  where
    go acc rest [] = (acc, reverse rest)
    go acc rest ((n, v) : xs)
      | n == ":status" = case BS8.readInt v of
          Just (k, _) -> go k rest xs
          Nothing     -> go acc rest xs
      | not (BS.null n) && BS.head n == 0x3A {- ':' -} = go acc rest xs
      | otherwise = go acc ((n, v) : rest) xs

-- | Notify any open inbox that the stream failed. Idempotent.
failStream :: ClientHandle -> StreamId -> ClientStreamError -> IO ()
failStream handle sid err = do
  inboxes <- readIORef (chStreams handle)
  case Map.lookup sid inboxes of
    Nothing -> pure ()
    Just inbox -> do
      _ <- tryPutMVar (siHeaders inbox) (Left err)
      atomically $ writeTBQueue (siBody inbox) (BodyError err)
      modifyIORef' (chStreams handle) (Map.delete sid)

-- | When the connection terminates, mark every outstanding stream as
-- 'ClientStreamConnectionClosed' so that blocked callers wake up.
failOutstanding :: ClientHandle -> IO ()
failOutstanding handle = do
  inboxes <- readIORef (chStreams handle)
  mapM_ failOne (Map.toList inboxes)
  writeIORef (chStreams handle) Map.empty
  where
    failOne (_, inbox) = do
      _ <- tryPutMVar (siHeaders inbox) (Left ClientStreamConnectionClosed)
      atomically $ writeTBQueue (siBody inbox) (BodyError ClientStreamConnectionClosed)

------------------------------------------------------------------------
-- Sending
------------------------------------------------------------------------

-- | Send a request and synchronously wait for the full response.
-- Throws 'ClientStreamError' if the stream is reset or the connection
-- drops before the response completes.
sendRequest :: ClientHandle -> ClientRequest -> IO ClientResponse
sendRequest handle req = do
  (_sid, inbox) <- registerAndSend handle req
  headersResult <- takeMVar (siHeaders inbox)
  case headersResult of
    Left err -> throwIO err
    Right rh -> do
      body <- drainBody (siBody inbox)
      pure ClientResponse
        { crStatus = rhStatus rh
        , crResponseHeaders = rhHeaders rh
        , crResponseBody = body
        }

-- | Lower-level send that returns only the 'StreamId' (the response
-- arrives via the recv loop). Provided for callers that want
-- fire-and-forget behaviour; most users should prefer 'sendRequest'.
sendRequestStreamId :: ClientHandle -> ClientRequest -> IO StreamId
sendRequestStreamId handle req = do
  (sid, _inbox) <- registerAndSend handle req
  pure sid

registerAndSend
  :: ClientHandle -> ClientRequest -> IO (StreamId, StreamInbox)
registerAndSend handle req = do
  let conn = chConnection handle
  sid <- atomically $ do
    streams <- readTVar (stNextStreamId (connStreamTable conn))
    writeTVar (stNextStreamId (connStreamTable conn)) (streams + 2)
    pure streams
  inbox <- StreamInbox <$> newEmptyMVar <*> atomically (newTBQueue 64)
  modifyIORef' (chStreams handle) (Map.insert sid inbox)
  encoder <- readMVar (connHpackEncoder conn)
  let pseudoHeaders =
        [ (":method", crMethod req)
        , (":path", crPath req)
        , (":scheme", crScheme req)
        , (":authority", crAuthority req)
        ]
      allHeaders = pseudoHeaders <> crHeaders req
  headerBlock <- encodeHeaderBlock defaultEncodeStrategy encoder allHeaders
  let bodyKind = crBody req
      endStreamOnHeaders = case bodyKind of
        ReqBodyNone -> True
        ReqBodyBytes bs -> BS.null bs
        ReqBodyStream _ -> False
      flags = flagEndHeaders .|. (if endStreamOnHeaders then flagEndStream else 0)
      headersFrame = Frame
        (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders flags sid)
        (HeadersFrame Nothing headerBlock)
  sendFrame conn headersFrame
  case bodyKind of
    ReqBodyNone -> pure ()
    ReqBodyBytes b
      | BS.null b -> pure ()
      | otherwise -> sendBodyOneShot conn sid b
    ReqBodyStream producer -> sendBodyStream conn sid producer
  pure (sid, inbox)

-- | Send a known-length body in a single DATA frame (chunked to the
-- peer's MAX_FRAME_SIZE), terminating with END_STREAM.  The
-- connection-level flow window is respected.
sendBodyOneShot :: Connection -> StreamId -> ByteString -> IO ()
sendBodyOneShot conn sid b = do
  maxFrame <- peerMaxFrameSize conn
  loop b maxFrame
  where
    loop bs maxFrame
      | BS.length bs <= maxFrame = sendDataFrame conn sid True bs
      | otherwise = do
          let (chunk, rest) = BS.splitAt maxFrame bs
          sendDataFrame conn sid False chunk
          loop rest maxFrame

-- | Pump a streaming-body producer onto the wire as a sequence of DATA
-- frames, terminated by an empty DATA + END_STREAM.  Each chunk is
-- split to the peer's MAX_FRAME_SIZE and gated on the connection-level
-- send flow window.
sendBodyStream
  :: Connection -> StreamId -> IO (Maybe ByteString) -> IO ()
sendBodyStream conn sid producer = loop
  where
    loop = do
      mChunk <- producer
      case mChunk of
        Nothing -> sendDataFrame conn sid True BS.empty
        Just bs
          | BS.null bs -> loop
          | otherwise -> do
              maxFrame <- peerMaxFrameSize conn
              sendChunkChopped conn sid bs maxFrame
              loop

sendChunkChopped :: Connection -> StreamId -> ByteString -> Int -> IO ()
sendChunkChopped conn sid bs maxFrame
  | BS.null bs = pure ()
  | BS.length bs <= maxFrame = sendDataFrame conn sid False bs
  | otherwise = do
      let (chunk, rest) = BS.splitAt maxFrame bs
      sendDataFrame conn sid False chunk
      sendChunkChopped conn sid rest maxFrame

-- | Send one DATA frame, blocking until the connection-level send
-- flow window has room (HTTP\/2 §6.9).  Splits oversized frames
-- with the caller; we just sit on the wire window here.
sendDataFrame :: Connection -> StreamId -> Bool -> ByteString -> IO ()
sendDataFrame conn sid endStream bs = do
  let n = BS.length bs
  -- An empty terminator DATA frame doesn't consume flow window.
  if n == 0
    then sendIt
    else do
      atomically $ do
        ok <- consumeWindow (connSendFlowControl conn) (fromIntegral n)
        if ok then pure () else retry
      sendIt
  where
    sendIt = sendFrame conn $ Frame
      (FrameHeader (fromIntegral (BS.length bs)) FrameData
        (if endStream then flagEndStream else 0) sid)
      (DataFrame bs)

peerMaxFrameSize :: Connection -> IO Int
peerMaxFrameSize conn = do
  s <- readIORef (connRemoteSettings conn)
  -- The peer-advertised value is a Word32; we clamp to Int range
  -- (the protocol minimum / maximum are both well below 2^31).
  pure (fromIntegral (settingsMaxFrameSize s))

drainBody :: TBQueue BodyItem -> IO ByteString
drainBody q = go []
  where
    go acc = do
      item <- atomically $ readTBQueue q
      case item of
        BodyChunk bs -> go (bs : acc)
        BodyEnd      -> pure $ BS.concat (reverse acc)
        BodyError e  -> throwIO e

------------------------------------------------------------------------
-- Frame helpers
------------------------------------------------------------------------

-- | Strip the @Padded@ prefix/suffix from a DATA / HEADERS payload.
-- Returns 'Nothing' when the pad length byte claims more padding than
-- the frame contains.
stripDataPadding :: FrameFlags -> ByteString -> Maybe ByteString
stripDataPadding flags bs
  | not (testFlag flags flagPadded) = Just bs
  | BS.null bs = Nothing
  | otherwise =
      let padLen = fromIntegral (BS.head bs)
          total  = BS.length bs
      in if padLen + 1 > total
           then Nothing
           else Just (BS.take (total - padLen - 1) (BS.drop 1 bs))

-- | Strip the optional padding /and/ PRIORITY (flag 'flagPriority')
-- prefixes from a HEADERS payload.
stripHeaderPadding :: FrameFlags -> ByteString -> Maybe ByteString
stripHeaderPadding flags bs0 = do
  bs1 <- stripDataPadding flags bs0
  if testFlag flags flagPriority
    then if BS.length bs1 >= 5 then Just (BS.drop 5 bs1) else Nothing
    else Just bs1

readWord32 :: ByteString -> Word32
readWord32 bs =
  (fromIntegral (BS.index bs 0) `shiftL` 24)
    .|. (fromIntegral (BS.index bs 1) `shiftL` 16)
    .|. (fromIntegral (BS.index bs 2) `shiftL` 8)
    .|. fromIntegral (BS.index bs 3)

word32ToErrorCodeLocal :: Word32 -> ErrorCode
word32ToErrorCodeLocal w = case w of
  0x0 -> NoError
  0x1 -> ProtocolError
  0x2 -> InternalError
  0x3 -> FlowControlError
  0x4 -> SettingsTimeout
  0x5 -> StreamClosed
  0x6 -> FrameSizeError
  0x7 -> RefusedStream
  0x8 -> Cancel
  0x9 -> CompressionError
  0xa -> ConnectError
  0xb -> EnhanceYourCalm
  0xc -> InadequateSecurity
  0xd -> HTTP11Required
  other -> UnknownError other
