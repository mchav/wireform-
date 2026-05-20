module Network.HTTP2.Client
  ( ClientConfig (..)
  , defaultClientConfig
  , ClientRequest (..)
  , ClientResponse (..)
  , withConnection
  , withConnectionOnTransport
  , sendRequest
    -- * Low-level pieces, re-used by TLS bring-up
  , sendClientPreface
  , clientRecvLoop
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, finally)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Network.Socket as NS

import Network.HTTP2.Connection
import Network.HTTP2.Frame
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
  , crBody :: !(Maybe ByteString)
  }

data ClientResponse = ClientResponse
  { crStatus :: !Int
  , crResponseHeaders :: ![(ByteString, ByteString)]
  , crResponseBody :: !ByteString
  }
  deriving stock (Eq, Show)

withConnection :: ClientConfig -> (Connection -> IO a) -> IO a
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
  -> (Connection -> IO a)
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
  _ <- forkIO $ clientRecvLoop conn
  action conn `finally` closeConnection conn NoError ""

sendClientPreface :: Connection -> Settings -> IO ()
sendClientPreface conn settings = do
  let preface = connectionPreface
      params = encodeSettings settings
      settingsFrame = Frame
        (FrameHeader (fromIntegral (length params * 6)) FrameSettings 0 0)
        (SettingsFrame params)
  tSendMany (connTransport conn) [preface, encodeFrame settingsFrame]

clientRecvLoop :: Connection -> IO ()
clientRecvLoop conn = do
  result <- recvFrame conn
  case result of
    Left _ -> pure ()
    Right (Frame hdr payload) -> do
      handleClientFrame conn hdr payload
      clientRecvLoop conn

handleClientFrame :: Connection -> FrameHeader -> FramePayload -> IO ()
handleClientFrame conn hdr payload = case payload of
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

  _ -> pure ()

sendRequest :: Connection -> ClientRequest -> IO StreamId
sendRequest conn req = do
  sid <- atomically $ do
    streams <- readTVar (stNextStreamId (connStreamTable conn))
    writeTVar (stNextStreamId (connStreamTable conn)) (streams + 2)
    pure streams
  encoder <- readMVar (connHpackEncoder conn)
  let pseudoHeaders =
        [ (":method", crMethod req)
        , (":path", crPath req)
        , (":scheme", crScheme req)
        , (":authority", crAuthority req)
        ]
      allHeaders = pseudoHeaders <> crHeaders req
  headerBlock <- encodeHeaderBlock defaultEncodeStrategy encoder allHeaders
  let endStream = case crBody req of
        Nothing -> True
        Just _ -> False
      flags = flagEndHeaders .|. if endStream then flagEndStream else 0
      headersFrame = Frame
        (FrameHeader (fromIntegral (BS.length headerBlock)) FrameHeaders flags sid)
        (HeadersFrame Nothing headerBlock)
  sendFrame conn headersFrame
  case crBody req of
    Nothing -> pure ()
    Just body -> do
      let dataFrame = Frame
            (FrameHeader (fromIntegral (BS.length body)) FrameData flagEndStream sid)
            (DataFrame body)
      sendFrame conn dataFrame
  pure sid
