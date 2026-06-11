{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tests for the TLS-offload code path in
'Kafka.Network.Connection'.

The point of TLS offload is that the Haskell client opens a
/plain/ socket (TCP or Unix-domain) to a sidecar process that
terminates TLS upstream. So these tests don't speak TLS at
all — they stand up small in-process echo servers playing
the part of the sidecar, then verify the client:

  * actually routes to the offload endpoint instead of the
    broker's advertised address;
  * picks the right per-broker entry when several brokers
    are mapped to different ports;
  * uses a Unix-domain socket when the endpoint says so;
  * falls through to the broker's own address for
    /transparent/ offload (kTLS / NLB / TPROXY) — the case
    where the resolver returns 'Nothing'.

We intentionally do not exercise the SASL or retry paths
here; those have separate coverage. This suite is just the
socket-routing layer.
-}
module Network.TlsOffloadSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Kafka.Network.Connection (BrokerAddress (..))
import Kafka.Network.Connection qualified as Conn
import Kafka.Network.Connection qualified as NC
import Kafka.Network.TlsOffload qualified as Offload
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketBS
import System.Directory (
  createDirectory,
  getTemporaryDirectory,
  removeDirectoryRecursive,
  removeFile,
 )
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Test.Syd


tests :: Spec
tests =
  describe "Kafka.Network.TlsOffload" $
    sequence_
      [ it
          "static TCP offload routes every broker to the sidecar"
          static_tcp_offload_routes_to_sidecar
      , it
          "per-broker offload picks the right sidecar port"
          per_broker_offload_routing
      , it
          "transparent offload uses the broker's own address"
          transparent_offload_uses_broker_address
      , it
          "unix-socket offload connects to a UDS sidecar"
          unix_socket_offload_round_trip
      , it
          "Connection layer skips client-side TLS when offload is set"
          offload_overrides_use_tls
      ]


----------------------------------------------------------------------
-- In-process TCP echo "sidecar"
----------------------------------------------------------------------

{- | A 'Sink' is a 'TVar' the server thread appends received
bytes into. Tests block on it via 'awaitBytes' so we don't
need 'threadDelay'.
-}
type Sink = TVar ByteString


newSink :: IO Sink
newSink = newTVarIO BS.empty


{- | Bind a TCP socket on 127.0.0.1 ephemeral port. Each
accepted connection drains its bytes (until EOF) into the
'Sink'.
-}
withTcpEcho
  :: Sink
  -- ^ where to accumulate received bytes
  -> (Int -> IO a)
  -- ^ action gets the bound port
  -> IO a
withTcpEcho sink act =
  bracket
    (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
    Socket.close
    $ \srv -> do
      Socket.setSocketOption srv Socket.ReuseAddr 1
      Socket.bind
        srv
        ( Socket.SockAddrInet
            0
            (Socket.tupleToHostAddress (127, 0, 0, 1))
        )
      Socket.listen srv 4
      addr <- Socket.getSocketName srv
      let !port = case addr of
            Socket.SockAddrInet p _ -> fromIntegral p
            _ -> 0
      _ <- forkIO $ acceptLoop srv sink
      act port


acceptLoop :: Socket.Socket -> Sink -> IO ()
acceptLoop srv sink = loop
  where
    loop = do
      r <- try (Socket.accept srv) :: IO (Either SomeException (Socket.Socket, Socket.SockAddr))
      case r of
        Left _ -> pure ()
        Right (cli, _) -> do
          _ <- forkIO $ do
            _ <- try (drainInto sink cli) :: IO (Either SomeException ())
            Socket.close cli
          loop


drainInto :: Sink -> Socket.Socket -> IO ()
drainInto sink sock = go
  where
    go = do
      chunk <- SocketBS.recv sock 4096
      if BS.null chunk
        then pure ()
        else do
          atomically (modifyTVar' sink (<> chunk))
          go


----------------------------------------------------------------------
-- In-process UDS echo "sidecar"
----------------------------------------------------------------------

withUnixEcho
  :: FilePath
  -> Sink
  -> IO a
  -> IO a
withUnixEcho path sink act =
  bracket
    (Socket.socket Socket.AF_UNIX Socket.Stream 0)
    ( \s -> do
        Socket.close s
        r <- try (removeFile path) :: IO (Either IOError ())
        case r of
          Left e | isDoesNotExistError e -> pure ()
          _ -> pure ()
    )
    $ \srv -> do
      Socket.bind srv (Socket.SockAddrUnix path)
      Socket.listen srv 4
      _ <- forkIO $ acceptLoop srv sink
      act


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{- | Block until the sidecar has accumulated at least @n@
bytes. STM 'retry' parks the test thread until the server
side wakes us up via 'modifyTVar', so there is no
'threadDelay' and no busy spin.
-}
awaitBytes :: Sink -> Int -> IO ByteString
awaitBytes sink target = atomically $ do
  cur <- readTVar sink
  if BS.length cur >= target
    then pure cur
    else retry


{- | Push some bytes through a connection and force a flush by
closing it; the sidecar will see the bytes appear in the
accumulator.
-}
pushAndClose :: NC.Connection -> ByteString -> IO ()
pushAndClose conn payload = do
  NC.connectionPut conn payload
  NC.connectionClose conn


----------------------------------------------------------------------
-- Cases
----------------------------------------------------------------------

static_tcp_offload_routes_to_sidecar :: IO ()
static_tcp_offload_routes_to_sidecar = do
  sink <- newSink
  withTcpEcho sink $ \sidecarPort -> do
    let offload =
          Offload.staticTlsOffload
            (Offload.TlsOffloadTcp "127.0.0.1" (fromIntegral sidecarPort))
        cfg =
          Conn.defaultConnectionConfig
            { Conn.connTlsOffload = Just offload
            , Conn.connMaxRetries = 0
            }
        -- A broker address that is /not/ the sidecar address —
        -- the offload routing has to redirect us.
        addr = BrokerAddress "203.0.113.1" 9092
    r <- Conn.connectOffload addr cfg offload
    case r of
      Left err -> expectationFailure ("offload connect failed: " <> err)
      Right conn -> do
        pushAndClose conn "PING-static"
        got <- awaitBytes sink (BS.length "PING-static")
        got `shouldBe` "PING-static"


per_broker_offload_routing :: IO ()
per_broker_offload_routing = do
  sinkA <- newSink
  sinkB <- newSink
  withTcpEcho sinkA $ \portA ->
    withTcpEcho sinkB $ \portB -> do
      let brokerA = BrokerAddress "broker-a.kafka.invalid" 9092
          brokerB = BrokerAddress "broker-b.kafka.invalid" 9092
          mapping =
            Map.fromList
              [
                ( Offload.OffloadBrokerKey "broker-a.kafka.invalid" 9092
                , Offload.TlsOffloadTcp "127.0.0.1" (fromIntegral portA)
                )
              ,
                ( Offload.OffloadBrokerKey "broker-b.kafka.invalid" 9092
                , Offload.TlsOffloadTcp "127.0.0.1" (fromIntegral portB)
                )
              ]
          offload = Offload.perBrokerTlsOffload mapping
          cfg =
            Conn.defaultConnectionConfig
              { Conn.connTlsOffload = Just offload
              , Conn.connMaxRetries = 0
              }
      ra <- Conn.connectOffload brokerA cfg offload
      rb <- Conn.connectOffload brokerB cfg offload
      case (ra, rb) of
        (Right ca, Right cb) -> do
          pushAndClose ca "to-A"
          pushAndClose cb "to-broker-B"
          gA <- awaitBytes sinkA 4
          gB <- awaitBytes sinkB 11
          gA `shouldBe` "to-A"
          gB `shouldBe` "to-broker-B"
        _ -> expectationFailure "per-broker offload connects should both succeed"


transparent_offload_uses_broker_address :: IO ()
transparent_offload_uses_broker_address = do
  -- For "transparent" mode the resolver returns 'Nothing' and
  -- the client opens TCP to the broker's own address. We use
  -- the local sidecar /as if/ it were the broker.
  sink <- newSink
  withTcpEcho sink $ \port -> do
    let offload = Offload.transparentTlsOffload
        cfg =
          Conn.defaultConnectionConfig
            { Conn.connTlsOffload = Just offload
            , Conn.connMaxRetries = 0
            }
        addr = BrokerAddress "127.0.0.1" (fromIntegral port)
    r <- Conn.connectOffload addr cfg offload
    case r of
      Left err -> expectationFailure ("transparent offload connect failed: " <> err)
      Right conn -> do
        pushAndClose conn "PING-transparent"
        got <- awaitBytes sink (BS.length "PING-transparent")
        got `shouldBe` "PING-transparent"


{- | Inline replacement for 'withSystemTempDirectory' to keep
the test deps minimal — we only need 'directory'.
-}
withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix act = do
  root <- getTemporaryDirectory
  -- We use the test name + a Socket-bound port as a cheap
  -- unique salt; if it collides we just retry once.
  let pick salt = root </> (prefix <> "-" <> show salt)
  go pick (0 :: Int)
  where
    go pick n = do
      let dir = pick n
      r <- try (createDirectory dir) :: IO (Either IOError ())
      case r of
        Right () -> do
          a <- act dir
          _ <- try (removeDirectoryRecursive dir) :: IO (Either IOError ())
          pure a
        Left _ | n < 100 -> go pick (n + 1)
        Left e -> ioError e


unix_socket_offload_round_trip :: IO ()
unix_socket_offload_round_trip = withTempDir "kfk-offload" $ \dir -> do
  let path = dir </> "sidecar.sock"
  sink <- newSink
  withUnixEcho path sink $ do
    let offload = Offload.staticTlsOffload (Offload.TlsOffloadUnix path)
        cfg =
          Conn.defaultConnectionConfig
            { Conn.connTlsOffload = Just offload
            , Conn.connMaxRetries = 0
            }
        -- The broker address is meaningless here — the offload
        -- resolver hard-codes the UDS path.
        addr = BrokerAddress "broker.kafka.invalid" 9093
    r <- Conn.connectOffload addr cfg offload
    case r of
      Left err -> expectationFailure ("UDS offload connect failed: " <> err)
      Right conn -> do
        pushAndClose conn "via-uds"
        got <- awaitBytes sink (BS.length "via-uds")
        got `shouldBe` "via-uds"


offload_overrides_use_tls :: IO ()
offload_overrides_use_tls = do
  -- 'connUseTls = True' should be ignored when the offload
  -- config is set — we open plain TCP to the sidecar. If the
  -- override didn't kick in the client would attempt a TLS
  -- handshake against the echo server and fail.
  sink <- newSink
  withTcpEcho sink $ \port -> do
    let endpoint = Offload.TlsOffloadTcp "127.0.0.1" (fromIntegral port)
        offload = Offload.staticTlsOffload endpoint
        cfg =
          Conn.defaultConnectionConfig
            { Conn.connTlsOffload = Just offload
            , Conn.connUseTls = True
            , Conn.connTlsSettings = Nothing
            , -- Intentionally /no/ TLS settings — would
              -- otherwise produce a "TLS enabled but no
              -- TLS settings" error.
              Conn.connMaxRetries = 0
            }
        addr = BrokerAddress "broker.kafka.invalid" 9093
    cm <- Conn.createConnectionManager
    r <- Conn.getOrCreateConnection cm addr cfg
    case r of
      Left err -> expectationFailure ("offload-overrides-tls failed: " <> err)
      Right conn -> do
        NC.connectionPut conn "USE-OFFLOAD"
        NC.connectionClose conn
        got <- awaitBytes sink (BS.length "USE-OFFLOAD")
        (BS.isPrefixOf "USE-OFFLOAD" got) `shouldBe` True
    Conn.closeAllConnections cm
