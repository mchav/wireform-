{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | End-to-end echo throughput benchmarks comparing
'wireform-websocket' with the canonical Haskell 'websockets'
package (jaspervdj).

We exercise the entire stack \u2014 real socket, real parser /
builder, real masking \u2014 because that is what applications
actually hit.  Pure unit-level frame micro-benchmarks would be
cherry-picked; what matters is end-to-end throughput on
loopback.

Each benchmark group:

  1. Starts an echo server (the matching library's runtime) on an
     ephemeral TCP port.
  2. Opens a persistent client connection (same library).
  3. Inside the criterion measurement loop, bounces one payload
     round-trip.

@
cabal bench wireform-websocket:wireform-websocket-bench
@

Narrow with a criterion matcher:

@
cabal bench wireform-websocket:wireform-websocket-bench -- -m pattern '\/64B\/'
@
-}
module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.MVar
import Control.DeepSeq (NFData (..))
import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Socket as NS
import qualified Network.WebSockets as WS

import Criterion.Main

import Network.WebSocket.Client
import Network.WebSocket.Connection (Connection)
import Network.WebSocket.Message
import Network.WebSocket.Server

------------------------------------------------------------------------
-- Payload sizes
------------------------------------------------------------------------

-- | Round-trip benchmark payloads.  Span the common range:
--
--   * 64 B \u2014 chat-message-sized; framing overhead dominates.
--   * 1 KiB \u2014 typical JSON payload.
--   * 16 KiB \u2014 single ring-bound chunk.
--   * 256 KiB \u2014 forces multiple ring publishes.
payloadSizes :: [(String, Int)]
payloadSizes =
  [ ("64B",   64)
  , ("1KiB",  1024)
  , ("16KiB", 16 * 1024)
  , ("128KiB", 128 * 1024)
  ]

------------------------------------------------------------------------
-- Bench harness
------------------------------------------------------------------------

main :: IO ()
main = do
  let payloads = [(name, BS.replicate n 0x41) | (name, n) <- payloadSizes]
  defaultMain
    [ bgroup "wireform-websocket"
        [ withWireformFixture name payload
        | (name, payload) <- payloads
        ]
    , bgroup "websockets (jaspervdj)"
        [ withWebsocketsFixture name (BSL.fromStrict payload)
        | (name, payload) <- payloads
        ]
    ]

-- | Newtype wrapper that lets us hand a heterogeneous fixture
-- tuple to criterion's 'envWithCleanup', which insists on an
-- 'NFData' instance.  None of our fixture fields is meaningfully
-- forceable (they're IO handles and live connections), so the
-- instance is trivial \u2014 'rnf' just evaluates the constructor.
newtype Opaque a = Opaque { unOpaque :: a }
instance NFData (Opaque a) where
  rnf (Opaque _) = ()

------------------------------------------------------------------------
-- wireform-websocket fixture
------------------------------------------------------------------------

withWireformFixture :: String -> ByteString -> Benchmark
withWireformFixture name payload = env acquire $
  \ ~(Opaque (_, _, conn)) ->
    bgroup name
      [ bench "text round-trip" $ whnfIO $ do
          sendTextMessage conn (TE.decodeUtf8 payload)
          _ <- receiveMessage conn defaultMessageLimit
          pure ()
      , bench "binary round-trip" $ whnfIO $ do
          sendBinaryMessage conn payload
          _ <- receiveMessage conn defaultMessageLimit
          pure ()
      ]
  where
    acquire :: IO (Opaque (NS.Socket, ThreadId, Connection))
    acquire = do
      (sock, port) <- bindEphemeral
      tid <- forkIO $ runWebSocketServerOnListener wireformEchoCfg sock
      conn <- openWireformClient port
      -- Burn off the first 10 round-trips so the criterion samples
      -- don't see TCP slow-start, the first ring allocation, or
      -- one-shot CPU-cache cold-misses.
      _ <- warmupWireform conn (TE.decodeUtf8 payload)
      pure (Opaque (sock, tid, conn))

warmupWireform :: Connection -> T.Text -> IO ()
warmupWireform conn t = go (10 :: Int)
  where
    go 0 = pure ()
    go n = do
      sendTextMessage conn t
      _ <- receiveMessage conn defaultMessageLimit
      go (n - 1)

wireformEchoCfg :: WebSocketServerConfig
wireformEchoCfg = defaultWebSocketServerConfig
  { wscHandler       = echoHandler
  , wscRingSizeHint  = 1024 * 1024   -- 1 MiB; comfortably larger
                                     -- than 128 KiB frames plus
                                     -- header room.
  }

echoHandler :: WebSocketHandler
echoHandler _ conn = loop
  where
    loop = do
      r <- try @SomeException (receiveMessage conn defaultMessageLimit)
      case r of
        Right (TextMessage   t)  -> sendTextMessage   conn t  >> loop
        Right (BinaryMessage bs) -> sendBinaryMessage conn bs >> loop
        Left _                    -> pure ()

openWireformClient :: Int -> IO Connection
openWireformClient port = do
  -- 'withWebSocketClient' is bracketed; we hold the connection
  -- across iterations, so we manually park it on an MVar inside a
  -- forked action.  When the bench group finishes, criterion's
  -- 'release' callback closes the underlying socket; the parked
  -- thread is then orphaned and reaped at process exit.
  mv <- newEmptyMVar
  let cfg = (defaultWebSocketClientConfig "127.0.0.1" (show port) "/")
        { wcRingSizeHint = 1024 * 1024 }
  _ <- forkIO $ withWebSocketClient cfg
    (\c -> putMVar mv c >> blockForever)
  takeMVar mv

------------------------------------------------------------------------
-- websockets fixture
------------------------------------------------------------------------

withWebsocketsFixture :: String -> BSL.ByteString -> Benchmark
withWebsocketsFixture name payload = env acquire $
  \ ~(Opaque (_, _, conn)) ->
    bgroup name
      [ bench "text round-trip" $ whnfIO $ do
          WS.sendTextData conn payload
          _ <- WS.receiveData conn :: IO BSL.ByteString
          pure ()
      , bench "binary round-trip" $ whnfIO $ do
          WS.sendBinaryData conn payload
          _ <- WS.receiveData conn :: IO BSL.ByteString
          pure ()
      ]
  where
    acquire :: IO (Opaque (NS.Socket, ThreadId, WS.Connection))
    acquire = do
      (sock, port) <- bindEphemeral
      tid <- forkIO $ runWebSocketsServer sock
      conn <- openWebsocketsClient port
      _ <- warmupWebsockets conn payload
      pure (Opaque (sock, tid, conn))

warmupWebsockets :: WS.Connection -> BSL.ByteString -> IO ()
warmupWebsockets conn payload = go (10 :: Int)
  where
    go 0 = pure ()
    go n = do
      WS.sendTextData conn payload
      _ <- WS.receiveData conn :: IO BSL.ByteString
      go (n - 1)

-- | Echo server using the 'websockets' library.  We bypass
-- 'WS.runServer' because it allocates its own listening socket;
-- here we have a pre-bound socket so the bench harness picks an
-- ephemeral port deterministically.
runWebSocketsServer :: NS.Socket -> IO ()
runWebSocketsServer listenSock = acceptLoop
  where
    acceptLoop = do
      (sock, _) <- NS.accept listenSock
      _ <- forkIO $ do
        r <- try @SomeException $ do
          pending <- WS.makePendingConnection sock
                       WS.defaultConnectionOptions
          conn <- WS.acceptRequest pending
          serveLoop conn
        case r of
          Right () -> pure ()
          Left _   -> pure ()
      acceptLoop
    serveLoop conn = do
      msg <- WS.receiveDataMessage conn
      case msg of
        WS.Text bs _ -> do
          WS.sendDataMessage conn (WS.Text bs Nothing)
          serveLoop conn
        WS.Binary bs -> do
          WS.sendDataMessage conn (WS.Binary bs)
          serveLoop conn

openWebsocketsClient :: Int -> IO WS.Connection
openWebsocketsClient port = do
  mv <- newEmptyMVar
  _ <- forkIO $
    WS.runClient "127.0.0.1" port "/" $ \conn -> do
      putMVar mv conn
      blockForever
  takeMVar mv

------------------------------------------------------------------------
-- Common
------------------------------------------------------------------------

bindEphemeral :: IO (NS.Socket, Int)
bindEphemeral = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  let addr = head addrs
  sock <- NS.openSocket addr
  NS.setSocketOption sock NS.ReuseAddr 1
  NS.bind sock (NS.addrAddress addr)
  NS.listen sock 32
  boundAddr <- NS.getSocketName sock
  let port = case boundAddr of
        NS.SockAddrInet p _      -> fromIntegral p
        NS.SockAddrInet6 p _ _ _ -> fromIntegral p
        _ -> error "ephemeral bind: unexpected sockaddr"
  pure (sock, port)

-- | Sleep effectively forever (~ 290 years on a 64-bit RTS).
-- We cannot 'takeMVar' on a freshly-created empty MVar here:
-- GHC's runtime would notice the only reference to that MVar is
-- the thread blocking on it, decide the situation is hopeless,
-- and raise 'BlockedIndefinitelyOnMVar'.  The exception would
-- unwind the surrounding 'bracket' that owns the client
-- 'Connection', close the duplex transport, and make every
-- subsequent benchmark iteration trip on a dead socket.
-- 'threadDelay' parks on the IO manager, which the deadlock
-- detector does not inspect.
blockForever :: IO ()
blockForever = do
  threadDelay maxBound
  blockForever
