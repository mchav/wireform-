{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | End-to-end echo test.

Binds an ephemeral TCP port, runs the standalone WebSocket
server, connects a client, exchanges a few messages, and tears
both sides down.  Exercises the full pipeline:

* server-side accept loop + handshake parser,
* receive transport (magic ring) driving the streaming frame
  parser,
* send transport with the builder-direct path,
* client-side handshake roundtrip,
* server- vs client-direction masking.
-}
module Test.Echo (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Network.Socket as NS
import System.Timeout (timeout)

import Test.Tasty
import Test.Tasty.HUnit

import Network.WebSocket.Client
import Network.WebSocket.Handshake
import Network.WebSocket.Message
import Network.WebSocket.Server

tests :: TestTree
tests = testGroup "Echo"
  [ echoText
  , echoBinary
  , echoTextTls
  , echoViaURI
  , subProtocolNegotiation
  , unofferedSubProtocolRejected
  ]

------------------------------------------------------------------------
-- Echo: text
------------------------------------------------------------------------

echoText :: TestTree
echoText = testCase "client <-> echo server: text message" $ do
  r <- timeout 10_000_000 $ withEchoServer $ \port -> do
    let cfg = defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo"
    withWebSocketClient cfg $ \conn -> do
      sendTextMessage conn "hello, websocket"
      msg <- receiveMessage conn defaultMessageLimit
      case msg of
        TextMessage t -> t @?= "hello, websocket"
        other         -> assertFailure ("expected TextMessage, got " <> show other)
  case r of
    Just () -> pure ()
    Nothing -> assertFailure "echo text timed out after 10s"

echoBinary :: TestTree
echoBinary = testCase "client <-> echo server: binary message" $ do
  r <- timeout 10_000_000 $ withEchoServer $ \port -> do
    let cfg = defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo"
        payload = BS.pack [0 .. 99]
    withWebSocketClient cfg $ \conn -> do
      sendBinaryMessage conn payload
      msg <- receiveMessage conn defaultMessageLimit
      case msg of
        BinaryMessage bs -> bs @?= payload
        other            -> assertFailure ("expected BinaryMessage, got " <> show other)
  case r of
    Just () -> pure ()
    Nothing -> assertFailure "echo binary timed out after 10s"

------------------------------------------------------------------------
-- Echo over TLS (wss://)
------------------------------------------------------------------------

echoTextTls :: TestTree
echoTextTls = testCase "wss:// client <-> echo server: text message" $ do
  r <- timeout 20_000_000 $ withEchoServerTls $ \port -> do
    let cfg = (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
          { wcTls = Just (wsTlsDefault
              { wctVerifyPeer = False   -- self-signed in fixture
              })
          }
    withWebSocketClient cfg $ \conn -> do
      sendTextMessage conn "tls hello"
      msg <- receiveMessage conn defaultMessageLimit
      case msg of
        TextMessage t -> t @?= "tls hello"
        other         -> assertFailure ("expected TextMessage, got " <> show other)
  case r of
    Just () -> pure ()
    Nothing -> assertFailure "TLS echo test timed out after 20s"

------------------------------------------------------------------------
-- URI-based connect
------------------------------------------------------------------------

echoViaURI :: TestTree
echoViaURI = testCase "withWebSocketClientURI: ws:// round-trip" $ do
  r <- timeout 10_000_000 $ withEchoServer $ \port -> do
    let uri = "ws://127.0.0.1:" <> BS8.pack (show port) <> "/echo"
    withWebSocketClientURI uri $ \conn -> do
      sendTextMessage conn "uri-routed"
      msg <- receiveMessage conn defaultMessageLimit
      case msg of
        TextMessage t -> t @?= "uri-routed"
        other         -> assertFailure ("expected TextMessage, got " <> show other)
  case r of
    Just () -> pure ()
    Nothing -> assertFailure "URI echo timed out"

------------------------------------------------------------------------
-- Sub-protocol negotiation
------------------------------------------------------------------------

subProtocolNegotiation :: TestTree
subProtocolNegotiation =
  testCase "client sees the sub-protocol the server selected" $ do
    r <- timeout 10_000_000 $
      withEchoServerProto $ \port -> do
        let cfg = (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
              { wcSubProtocols = ["chat.v2", "chat.v1"]
              }
        withWebSocketClient' cfg $ \shr _conn ->
          shrSelectedProtocol shr @?= Just "chat.v2"
    case r of
      Just () -> pure ()
      Nothing -> assertFailure "sub-protocol test timed out"

unofferedSubProtocolRejected :: TestTree
unofferedSubProtocolRejected =
  testCase "client rejects a server-selected protocol it did not offer" $ do
    -- Server config forces the selection regardless of what the
    -- client offered — emulates a misbehaving server.
    r <- timeout 10_000_000 $
      withForcedProtoServer "imposed-protocol" $ \port -> do
        let cfg = (defaultWebSocketClientConfig "127.0.0.1" (show port) "/echo")
              { wcSubProtocols = ["something-else"]
              }
        result <- try $ withWebSocketClient cfg $ \_ -> pure ()
        case result :: Either SomeException () of
          Left _  -> pure ()  -- handshake validation throws
          Right _ -> assertFailure
            "expected client to reject server's unoffered sub-protocol"
    case r of
      Just () -> pure ()
      Nothing -> assertFailure "rejection test timed out"

-- | Echo server that picks the first offered sub-protocol via the
-- 'wscSelectSubProtocol' callback.
withEchoServerProto :: (Int -> IO a) -> IO a
withEchoServerProto = withServerCfgVariant cfg
  where
    cfg = defaultWebSocketServerConfig
      { wscHandler           = echoHandler
      , wscSelectSubProtocol = \req ->
          case wsReqProtocols req of
            (p:_) -> Just p
            []    -> Nothing
      }

-- | Echo server that always selects @forced@ as the sub-protocol,
-- regardless of what the client offered.  Lets us drive the
-- client's negative-path validation.
withForcedProtoServer :: BS.ByteString -> (Int -> IO a) -> IO a
withForcedProtoServer forced = withServerCfgVariant cfg
  where
    cfg = defaultWebSocketServerConfig
      { wscHandler           = echoHandler
      , wscSelectSubProtocol = \_ -> Just forced
      }

-- | Plumbing shared by the proto-server fixtures.
withServerCfgVariant
  :: WebSocketServerConfig
  -> (Int -> IO a)
  -> IO a
withServerCfgVariant cfg action = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  let addr = case addrs of
        []    -> error "no addr"
        (a:_) -> a
  bracket (NS.openSocket addr) NS.close $ \sock -> do
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.listen sock 16
    boundAddr <- NS.getSocketName sock
    port <- case boundAddr of
      NS.SockAddrInet p _      -> pure (fromIntegral p :: Int)
      NS.SockAddrInet6 p _ _ _ -> pure (fromIntegral p :: Int)
      _ -> error "unexpected sockaddr"
    tid <- forkIO $ runWebSocketServerOnListener cfg sock
    a <- action port
    killThread tid
    pure a
withEchoServerTls :: (Int -> IO a) -> IO a
withEchoServerTls action = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  let addr = case addrs of
        []    -> error "no addr"
        (a:_) -> a
  bracket (NS.openSocket addr) NS.close $ \sock -> do
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.listen sock 16
    boundAddr <- NS.getSocketName sock
    port <- case boundAddr of
      NS.SockAddrInet p _      -> pure (fromIntegral p :: Int)
      NS.SockAddrInet6 p _ _ _ -> pure (fromIntegral p :: Int)
      _ -> error "unexpected sockaddr"
    let cfg = defaultWebSocketServerConfig
          { wscHandler = echoHandler
          , wscTls     = Just WebSocketTlsConfig
              { wstCertPath = "test-tls/cert.pem"
              , wstKeyPath  = "test-tls/key.pem"
              , wstAlpn     = []
              }
          }
    tid <- forkIO $ runWebSocketServerOnListener cfg sock
    a <- action port
    killThread tid
    pure a

------------------------------------------------------------------------
-- Server fixture
------------------------------------------------------------------------

withEchoServer :: (Int -> IO a) -> IO a
withEchoServer action = do
  -- Bind to an ephemeral port so the test never collides with
  -- another listener on the host.
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  let addr = case addrs of
        []    -> error "no addr"
        (a:_) -> a
  bracket (NS.openSocket addr) NS.close $ \sock -> do
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock (NS.addrAddress addr)
    NS.listen sock 16
    boundAddr <- NS.getSocketName sock
    port <- case boundAddr of
      NS.SockAddrInet p _   -> pure (fromIntegral p :: Int)
      NS.SockAddrInet6 p _ _ _ -> pure (fromIntegral p :: Int)
      _ -> error "unexpected sockaddr"
    -- Spawn the server thread and an MVar to wait on its
    -- shutdown so test errors surface promptly.
    done <- newEmptyMVar
    let cfg = (defaultWebSocketServerConfig)
          { wscHandler = echoHandler
          }
    tid <- forkIO $ do
      runWebSocketServerOnListener cfg sock
        `finallyMV` putMVar done ()
    a <- action port
    killThread tid
    pure a
  where
    finallyMV act fin = act >>= \r -> fin >> pure r

echoHandler :: WebSocketHandler
echoHandler _req conn = loop
  where
    loop = do
      r <- try @SomeException $ receiveMessage conn defaultMessageLimit
      case r of
        Right (TextMessage t)    -> sendTextMessage conn t   >> loop
        Right (BinaryMessage bs) -> sendBinaryMessage conn bs >> loop
        Left _                    -> pure ()  -- peer closed
