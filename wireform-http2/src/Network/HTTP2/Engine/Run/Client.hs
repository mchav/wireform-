{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Client-side frame I\/O loop for the wireform-http2 gRPC engine.
--
-- Drives 'Network.HTTP2.Engine.Client.run'. Like the server runtime,
-- this is intentionally narrower than @http2@ 5.3.x: handles HEADERS
-- + DATA + trailing HEADERS, half-close in both directions, basic
-- WINDOW_UPDATE / PING / SETTINGS bookkeeping. No connection
-- preserving, no PUSH_PROMISE, no concurrent-stream rate limiting.
{-# LANGUAGE RankNTypes #-}
module Network.HTTP2.Engine.Run.Client
  ( RunEnv (..)
  , EngineAux (..)
  , runClient
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
  ( Exception, SomeException, catch, finally, throwIO, toException, try
  )
import Control.Monad (forever, unless, when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import qualified Network.HTTP.Types as HTTP

import Network.HTTP2.Connection (Connection, closeConnection,
  connHpackDecoder, connHpackEncoder, newConnectionFromTransport,
  sendFrame)
import qualified Network.HTTP2.Connection as Conn
import Network.HTTP2.Connection.Settings (encodeSettings)
import Network.HTTP2.Frame
import Network.HTTP2.HPACK
import Network.HTTP2.Transport (Transport (..))
import Network.HTTP2.Types (FrameType (..))
import qualified Network.HTTP2.Types as H2
import qualified Network.HTTP2.Types as Wire

import Network.HTTP2.Engine.Types

-- | Connection-scoped configuration for the client engine.
data RunEnv = RunEnv
  { envAuthority :: !Authority
  , envSendAll :: !(ByteString -> IO ())
  , envReadN :: !(Int -> IO ByteString)
  , envConnectionWindow :: !Int
  , envInitialWindowSize :: !Int
  , envMaxConcurrentStreams :: !(Maybe Word32)
  , envScheme :: !Scheme
  }

-- | Mirror of 'Aux' from Engine.Client.
data EngineAux = EngineAux
  { engineAuxPossibleStreams :: !(IO Int)
  }

data PeerCancelled = PeerCancelled
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Drive a client connection. The supplied callback receives a
-- 'SendRequest'-shaped helper and an 'EngineAux'; the returned
-- value bubbles out.
runClient
  :: RunEnv
  -> ((forall r. OutObj -> (InpObj -> IO r) -> IO r) -> EngineAux -> IO a)
  -> IO a
runClient env client = do
  let transport = engineTransport env
  conn <- newConnectionFromTransport
            Conn.RoleClient
            wireSettings
            (\_ _ _ -> pure ())
            transport
  envSendAll env connectionPreface
  sendClientPrefaceSettings conn wireSettings
  streamsRef <- newIORef Map.empty
  nextSidRef <- newIORef 1
  -- Spawn the frame receiver; it terminates when the connection drops.
  _ <- forkIO $ recvLoop env conn streamsRef
                  `catch` (\(_ :: SomeException) -> pure ())
  let sendRequest req k = doSendRequest env conn streamsRef nextSidRef req k
      aux = EngineAux { engineAuxPossibleStreams = pure 100 }
  client sendRequest aux `finally` closeConnection conn Wire.NoError ""
  where
    wireSettings = H2.defaultSettings
      { H2.settingsInitialWindowSize = fromIntegral (envInitialWindowSize env)
      , H2.settingsMaxConcurrentStreams = envMaxConcurrentStreams env
      }

engineTransport :: RunEnv -> Transport
engineTransport env =
  Transport
    { tSendAll = envSendAll env
    , tSendMany = \bss -> envSendAll env (BS.concat bss)
    , tRecvBuf = \ptr n -> do
        bs <- envReadN env n
        if BS.null bs
          then pure 0
          else do
            let (fp, off, len) = BSI.toForeignPtr bs
            withForeignPtr fp $ \src ->
              BSI.memcpy ptr (src `plusPtr` off) len
            pure len
    , tClose = pure ()
    }

sendClientPrefaceSettings :: Connection -> H2.Settings -> IO ()
sendClientPrefaceSettings conn s = do
  let params = encodeSettings s
      frame = Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
  sendFrame conn frame

-- | Per-stream worker mailbox for inbound (response side) data.
data StreamMb = StreamMb
  { smHeadersVar :: !(MVar [(ByteString, ByteString)])
  , smInputQueue :: !(TBQueue InputItem)
  , smTrailerSlot :: !(IORef (Maybe TokenHeaderTable))
  }

data InputItem = InputChunk !ByteString | InputEnd

-- | Open a new stream, send the request headers/body, and call @k@
-- with the inbound response object once headers arrive.
doSendRequest
  :: RunEnv
  -> Connection
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> IORef H2.StreamId
  -> OutObj
  -> (InpObj -> IO r)
  -> IO r
doSendRequest env conn streamsRef nextSidRef (OutObj hdrs body trailerMaker) k = do
  sid <- atomicModifyIORef' nextSidRef $ \s -> (s + 2, s)
  headersVar <- newEmptyMVar
  inputQ <- atomically (newTBQueue 64)
  trailerRef <- newIORef Nothing
  let mb = StreamMb headersVar inputQ trailerRef
  modifyIORef' streamsRef (Map.insert sid mb)

  -- Inject :scheme and :authority before the user-supplied headers
  -- (these are required pseudo-headers on the request side).
  let augmented = (":scheme", envScheme env)
                : (":authority", BS.empty)  -- placeholder; will be set below
                : ciHeadersToRaw hdrs
      finalHeaders = injectAuthority (envAuthority env) augmented

  block <- withMVar (connHpackEncoder conn) $ \encoder ->
    encodeHeaderBlock defaultEncodeStrategy encoder finalHeaders

  case body of
    OutBodyNone -> do
      sendHeadersFrame conn sid block True
    OutBodyBuilder b -> do
      sendHeadersFrame conn sid block False
      let bs = LBS.toStrict (BSB.toLazyByteString b)
      sendDataFrame conn sid bs True
    OutBodyStreaming f -> do
      sendHeadersFrame conn sid block False
      runStreamingBody conn sid trailerMaker $ \iface ->
        f (outBodyPush iface) (outBodyFlush iface)
    OutBodyStreamingIface f -> do
      sendHeadersFrame conn sid block False
      runStreamingBody conn sid trailerMaker f
    OutBodyFile _ ->
      error "Network.HTTP2.Engine.Run.Client: OutBodyFile not supported"

  -- Wait for the response headers, then construct the inbound object
  -- and hand it to the caller.
  respHeaders <- takeMVar headersVar
  let inpObj = InpObj
        { inpObjHeaders = tokeniseHeaders respHeaders
        , inpObjBodySize = Nothing
        , inpObjBody = do
            item <- atomically $ readTBQueue inputQ
            pure $ case item of
              InputChunk bs -> (bs, False)
              InputEnd      -> (BS.empty, True)
        , inpObjTrailers = trailerRef
        }
  k inpObj `finally` modifyIORef' streamsRef (Map.delete sid)

injectAuthority :: Authority -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]
injectAuthority auth = map fix
  where
    fix (":authority", _) = (":authority", BSI.packChars auth)
    fix kv = kv

runStreamingBody
  :: Connection
  -> H2.StreamId
  -> TrailersMaker
  -> (OutBodyIface -> IO ())
  -> IO ()
runStreamingBody conn sid _tmInit body = do
  finalisedRef <- newIORef False
  let pushOne bs = unless (BS.null bs) $ sendDataFrame conn sid bs False
      pushFinal bs = do
        already <- readIORef finalisedRef
        unless already $ do
          writeIORef finalisedRef True
          sendDataFrame conn sid bs True
      cancel _ = do
        already <- readIORef finalisedRef
        unless already $ do
          writeIORef finalisedRef True
          sendRstStream conn sid Wire.Cancel
      iface = OutBodyIface
        { outBodyUnmask = id
        , outBodyPush = \b -> pushOne (LBS.toStrict (BSB.toLazyByteString b))
        , outBodyPushFinal = \b -> pushFinal (LBS.toStrict (BSB.toLazyByteString b))
        , outBodyCancel = cancel
        , outBodyFlush = pure ()
        }
  r <- try (body iface)
  case r of
    Left (e :: SomeException) -> do
      already <- readIORef finalisedRef
      unless already $ do
        writeIORef finalisedRef True
        sendRstStream conn sid Wire.InternalError
      throwIO e
    Right () -> do
      already <- readIORef finalisedRef
      unless already $ do
        writeIORef finalisedRef True
        sendDataFrame conn sid BS.empty True

-- | Response frame dispatch loop.
recvLoop
  :: RunEnv
  -> Connection
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> IO ()
recvLoop env conn streamsRef = loop
  where
    loop = do
      mFrame <- pumpFrame env
      case mFrame of
        Nothing -> pure ()
        Just (Frame hdr payload) -> do
          handleClientFrame env conn streamsRef hdr payload
          loop

pumpFrame :: RunEnv -> IO (Maybe Frame)
pumpFrame env = do
  hdrBs <- envReadN env frameHeaderLength
  if BS.length hdrBs < frameHeaderLength
    then pure Nothing
    else case decodeFrameHeader hdrBs of
      Left _ -> pure Nothing
      Right hdr -> do
        let plen = fromIntegral (fhLength hdr)
        payloadBs <- if plen == 0 then pure BS.empty else envReadN env plen
        case decodeFramePayload hdr payloadBs of
          Left _ -> pure Nothing
          Right p -> pure (Just (Frame hdr p))

handleClientFrame
  :: RunEnv
  -> Connection
  -> IORef (Map.Map H2.StreamId StreamMb)
  -> FrameHeader
  -> FramePayload
  -> IO ()
handleClientFrame env conn streamsRef hdr payload = case payload of
  SettingsFrame _
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise -> sendFrame conn $ Frame
        (FrameHeader 0 FrameSettings flagAck 0) (SettingsFrame [])

  PingFrame opaque
    | testFlag (fhFlags hdr) flagAck -> pure ()
    | otherwise -> sendFrame conn $ Frame
        (FrameHeader 8 FramePing flagAck 0) (PingFrame opaque)

  WindowUpdateFrame _ -> pure ()
  GoAwayFrame{}       -> pure ()

  HeadersFrame _ block
    | testFlag (fhFlags hdr) flagEndHeaders -> do
        decoder <- readMVar (connHpackDecoder conn)
        result <- decodeHeaderBlock decoder block
        case result of
          Left _ -> closeConnection conn Wire.CompressionError "hpack decode"
          Right headers -> do
            streams <- readIORef streamsRef
            case Map.lookup (fhStreamId hdr) streams of
              Just mb -> do
                empty <- isEmptyMVar (smHeadersVar mb)
                if empty
                  then do
                    -- Initial response headers
                    putMVar (smHeadersVar mb) headers
                    when (testFlag (fhFlags hdr) flagEndStream) $ do
                      writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders []))
                      atomically $ writeTBQueue (smInputQueue mb) InputEnd
                  else do
                    -- Trailing HEADERS (response trailers)
                    writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders headers))
                    atomically $ writeTBQueue (smInputQueue mb) InputEnd
              Nothing -> pure ()
    | otherwise -> closeConnection conn Wire.ProtocolError "fragmented HEADERS"

  DataFrame body -> do
    streams <- readIORef streamsRef
    case Map.lookup (fhStreamId hdr) streams of
      Just mb -> do
        unless (BS.null body) $
          atomically $ writeTBQueue (smInputQueue mb) (InputChunk body)
        when (testFlag (fhFlags hdr) flagEndStream) $ do
          writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders []))
          atomically $ writeTBQueue (smInputQueue mb) InputEnd
        let len = fhLength hdr
        when (len > 0) $ do
          sendFrame conn $ Frame
            (FrameHeader 4 FrameWindowUpdate 0 0) (WindowUpdateFrame len)
          sendFrame conn $ Frame
            (FrameHeader 4 FrameWindowUpdate 0 (fhStreamId hdr))
            (WindowUpdateFrame len)
      Nothing -> pure ()

  RSTStreamFrame _ -> do
    streams <- readIORef streamsRef
    case Map.lookup (fhStreamId hdr) streams of
      Just mb -> do
        writeIORef (smTrailerSlot mb) (Just (tokeniseHeaders []))
        atomically $ writeTBQueue (smInputQueue mb) InputEnd
      Nothing -> pure ()

  _ -> pure ()

ciHeadersToRaw :: [HTTP.Header] -> [(ByteString, ByteString)]
ciHeadersToRaw = map (\(k, v) -> (CI.original k, v))

sendHeadersFrame :: Connection -> H2.StreamId -> ByteString -> Bool -> IO ()
sendHeadersFrame conn sid block endStream =
  let flags = flagEndHeaders .|. (if endStream then flagEndStream else 0)
      frame = Frame
        (FrameHeader (fromIntegral (BS.length block)) FrameHeaders flags sid)
        (HeadersFrame Nothing block)
   in sendFrame conn frame

sendDataFrame :: Connection -> H2.StreamId -> ByteString -> Bool -> IO ()
sendDataFrame conn sid body endStream =
  let flags = if endStream then flagEndStream else 0
      frame = Frame
        (FrameHeader (fromIntegral (BS.length body)) FrameData flags sid)
        (DataFrame body)
   in sendFrame conn frame

sendRstStream :: Connection -> H2.StreamId -> Wire.ErrorCode -> IO ()
sendRstStream conn sid code =
  let frame = Frame
        (FrameHeader 4 FrameRSTStream 0 sid)
        (RSTStreamFrame code)
   in sendFrame conn frame `catch` (\(_ :: SomeException) -> pure ())
