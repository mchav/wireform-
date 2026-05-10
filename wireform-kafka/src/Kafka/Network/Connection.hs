{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Network.Connection
Description : TCP/TLS connection management for Kafka brokers
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module provides connection management for Kafka brokers, supporting
both plain TCP and TLS-encrypted connections.

Features:

* TCP and TLS connection support
* Connection pooling and reuse
* Automatic reconnection with exponential backoff
* Configurable timeouts
* Proper resource cleanup

Connections are established to individual Kafka brokers. The client
maintains a pool of connections to different brokers and reuses them
for multiple requests.

-}
module Kafka.Network.Connection
  ( -- * Connection Types
    Connection(..)
  , ConnectionConfig(..)
  , BrokerAddress(..)
  , BrokerAddressFamily(..)
  , DnsLookupMode(..)
    -- * Connection Management
  , connect
  , connectTls
  , disconnect
  , withConnection
    -- * Connection Manager
  , ConnectionManager
  , createConnectionManager
  , getOrCreateConnection
  , closeAllConnections
  , withBrokerLock
    -- * Connection State
  , isConnected
    -- * Default Configuration
  , defaultConnectionConfig
  , defaultTlsSettings
    -- * Backoff Utilities
  , calculateBackoffDelay
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracketOnError, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Default.Class (def)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified ListT
import Network.Connection (Connection(..), ConnectionParams(..))
import qualified Network.Connection as Conn
import Network.Socket (HostName, PortNumber)
import qualified Network.Socket as NS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import qualified StmContainers.Map as StmMap
import System.Random (randomRIO)

import qualified Kafka.Network.Auth.SASL as SASL

-- | Broker address (host and port).
data BrokerAddress = BrokerAddress
  { brokerHost :: !HostName
    -- ^ Broker hostname or IP address
  , brokerPort :: !PortNumber
    -- ^ Broker port (typically 9092 for plain, 9093 for TLS)
  } deriving (Eq, Show, Ord, Generic)

instance Hashable BrokerAddress where
  hashWithSalt salt (BrokerAddress host port) =
    salt `hashWithSalt` host `hashWithSalt` (fromIntegral port :: Int)

-- | Address-family preference for DNS lookups. Mirrors
-- librdkafka's @broker.address.family@.
data BrokerAddressFamily
  = BrokerAddressAny
  | BrokerAddressIPv4
  | BrokerAddressIPv6
  deriving (Eq, Show, Generic)

-- | DNS lookup strategy. Mirrors librdkafka's @client.dns.lookup@.
data DnsLookupMode
  = DnsResolveCanonicalBootstrapServersOnly
    -- ^ Default. Resolve every bootstrap address but do NOT
    --   re-resolve broker entries returned by metadata.
  | DnsUseAllDnsIps
    -- ^ For each broker address, walk all A/AAAA records.
  deriving (Eq, Show, Generic)

-- | Connection configuration. Field names track librdkafka's
-- @CONFIGURATION.md@; the librdkafka name appears next to the
-- Haskell field.
data ConnectionConfig = ConnectionConfig
  { connTimeout :: !Int
    -- ^ Connection timeout in seconds (default 10).
    --   librdkafka @socket.connection.setup.timeout.ms@ /1000.
  , connReadTimeout :: !Int
    -- ^ Read timeout in seconds (default 30).
    --   librdkafka @socket.timeout.ms@ /1000 (read side).
  , connWriteTimeout :: !Int
    -- ^ Write timeout in seconds (default 30). librdkafka
    --   @socket.timeout.ms@ /1000 (write side).
  , connRequestTimeoutMs :: !Int
    -- ^ Per-request timeout in ms. Default 30000.
    --   librdkafka @request.timeout.ms@.
  , connRetryDelay :: !Int
    -- ^ Initial reconnect backoff in ms. Default 100.
    --   librdkafka @reconnect.backoff.ms@.
  , connMaxRetries :: !Int
    -- ^ Maximum number of connection retries (default 3).
  , connBackoffMaxMs :: !Int
    -- ^ Maximum reconnect backoff in ms. Default 10000.
    --   librdkafka @reconnect.backoff.max.ms@.
  , connBackoffMultiplier :: !Double
    -- ^ Reconnect backoff multiplier. Default 2.0.
  , connSocketKeepalive :: !Bool
    -- ^ Enable SO_KEEPALIVE on the broker socket. Default 'False'.
    --   librdkafka @socket.keepalive.enable@.
  , connSocketNagleDisable :: !Bool
    -- ^ Disable Nagle (TCP_NODELAY). Default 'False'.
    --   librdkafka @socket.nagle.disable@.
  , connSocketSendBuffer :: !Int
    -- ^ Socket send-buffer hint in bytes. 0 means use the OS
    --   default. librdkafka @socket.send.buffer.bytes@.
  , connSocketReceiveBuffer :: !Int
    -- ^ Socket receive-buffer hint in bytes. 0 means use the OS
    --   default. librdkafka @socket.receive.buffer.bytes@.
  , connSocketMaxFails :: !Int
    -- ^ Disconnect after this many consecutive failed broker
    --   requests. Default 1. librdkafka @socket.max.fails@.
  , connMaxIdleMs :: !Int
    -- ^ Disconnect idle broker connections after this many ms.
    --   Default 540000 (9 minutes).
    --   librdkafka @connections.max.idle.ms@.
  , connMaxReauthMs :: !Int
    -- ^ For SASL connections, force a re-authentication after
    --   this many ms. Default 0 (disabled).
    --   librdkafka @connections.max.reauth.ms@.
  , connMessageMaxBytes :: !Int
    -- ^ Hard limit on message size for both produce and fetch.
    --   Default 1000000 (1 MB). librdkafka @message.max.bytes@.
  , connReceiveMessageMaxBytes :: !Int
    -- ^ Maximum size for any single response. Default 100000000
    --   (100 MB). librdkafka @receive.message.max.bytes@.
  , connMetadataMaxAgeMs :: !Int
    -- ^ Period after which the metadata cache is unconditionally
    --   refreshed. Default 900000 (15 minutes). librdkafka
    --   @topic.metadata.refresh.interval.ms@.
  , connTopicMetadataRefreshFastIntervalMs :: !Int
    -- ^ Polling interval used while metadata is in a transient
    --   error state (UNKNOWN_TOPIC, NOT_LEADER, etc.). Default 250.
    --   librdkafka @topic.metadata.refresh.fast.interval.ms@.
  , connTopicMetadataRefreshSparse :: !Bool
    -- ^ Include only the topics this client knows about in
    --   metadata requests. Default 'True'. librdkafka
    --   @topic.metadata.refresh.sparse@.
  , connBrokerAddressTtl :: !Int
    -- ^ How long DNS results are cached in ms. Default 1000.
    --   librdkafka @broker.address.ttl@.
  , connBrokerAddressFamily :: !BrokerAddressFamily
    -- ^ Resolver address-family preference. Default 'BrokerAddressAny'.
    --   librdkafka @broker.address.family@.
  , connDnsLookup :: !DnsLookupMode
    -- ^ DNS lookup strategy. Default
    --   'DnsResolveCanonicalBootstrapServersOnly'. librdkafka
    --   @client.dns.lookup@.
  , connUseTls :: !Bool
    -- ^ Whether to use TLS encryption (default: False)
  , connTlsSettings :: !(Maybe TLS.ClientParams)
    -- ^ TLS client parameters (required if connUseTls is True)
  , connSasl :: !(Maybe SASL.SaslConfig)
    -- ^ SASL authentication to perform after the TCP/TLS handshake.
    --   When 'Just', 'getOrCreateConnection' runs the broker-side
    --   SASL handshake (SaslHandshake + SaslAuthenticate loop) and
    --   only stores the connection in the pool on success. Use:
    --
    --     * 'SASL.SaslPlain' over TLS for Confluent Cloud-style
    --       username\/password.
    --     * 'SASL.SaslScram' for Apache Kafka's built-in SCRAM
    --       (also the only option on AWS MSK Provisioned with
    --       \"SASL\/SCRAM\" enabled).
    --     * 'SASL.SaslOAuthBearer' for OIDC / cloud-managed brokers.
    --     * 'SASL.SaslAwsMskIam' for AWS MSK with IAM auth.
  , connClientId :: !Text
    -- ^ Client id used in the SASL request headers (defaults to
    --   \"wireform-kafka\"). Has no effect on connections without a
    --   'connSasl' value.
  } deriving (Generic)

-- | Default connection configuration. Defaults follow librdkafka's
-- @CONFIGURATION.md@ where possible, with two exceptions:
-- 'connBackoffMaxMs' uses 10000 (Kafka 3.x JVM default) rather
-- than librdkafka's 10000, and 'connMaxIdleMs' uses 540000 to
-- match the JVM client's @connections.max.idle.ms@.
defaultConnectionConfig :: ConnectionConfig
defaultConnectionConfig = ConnectionConfig
  { connTimeout                         = 10
  , connReadTimeout                     = 30
  , connWriteTimeout                    = 30
  , connRequestTimeoutMs                = 30_000
  , connRetryDelay                      = 100
  , connMaxRetries                      = 3
  , connBackoffMaxMs                    = 10_000
  , connBackoffMultiplier               = 2.0
  , connSocketKeepalive                 = True
    -- ^ SO_KEEPALIVE on by default — matches the JVM client's
    -- 'TcpKeepAliveDelegate' (Kafka enables it for every broker
    -- socket since 2.7) so silent dead-peer cases (NAT timeouts,
    -- broker crashes between heartbeats) don't park us on a
    -- TCP send-queue forever. librdkafka leaves this off by
    -- default but every production deployment turns it on.
  , connSocketNagleDisable              = True
    -- ^ TCP_NODELAY on by default — every Kafka write we make is
    -- already a complete framed request (the producer batches at
    -- the BatchAccumulator layer; the consumer issues whole
    -- Fetch requests). With Nagle on, the kernel would hold the
    -- write up to 40 ms waiting for an ACK to coalesce a phantom
    -- second segment that's never coming, adding latency for
    -- nothing. JVM client unconditionally calls
    -- @setTcpNoDelay(true)@ on every broker socket; we mirror.
  , connSocketSendBuffer                = 0
  , connSocketReceiveBuffer             = 0
  , connSocketMaxFails                  = 1
  , connMaxIdleMs                       = 540_000
  , connMaxReauthMs                     = 0
  , connMessageMaxBytes                 = 1_000_000
  , connReceiveMessageMaxBytes          = 100_000_000
  , connMetadataMaxAgeMs                = 900_000
  , connTopicMetadataRefreshFastIntervalMs = 250
  , connTopicMetadataRefreshSparse      = True
  , connBrokerAddressTtl                = 1000
  , connBrokerAddressFamily             = BrokerAddressAny
  , connDnsLookup                       = DnsResolveCanonicalBootstrapServersOnly
  , connUseTls                          = False
  , connTlsSettings                     = Nothing
  , connSasl                            = Nothing
  , connClientId                        = T.pack "wireform-kafka"
  }

-- | Default TLS settings for secure connections.
-- Uses a reasonable set of cipher suites and TLS 1.2+.
defaultTlsSettings :: HostName -> TLS.ClientParams
defaultTlsSettings hostname = (TLS.defaultParamsClient hostname "")
  { TLS.clientSupported = def
      { TLS.supportedCiphers = TLS.ciphersuite_strong
      }
  , TLS.clientShared = def
      { TLS.sharedValidationCache = def
      }
  }

-- | Connection manager that maintains one persistent connection per broker.
-- This follows the pattern used by production Kafka clients (Java, librdkafka, Rust).
data ConnectionManager = ConnectionManager
  { connectionMap :: !(StmMap.Map BrokerAddress Connection)
  , connectionLocks :: !(StmMap.Map BrokerAddress (MVar ()))
    -- ^ Per-broker mutex that callers can take around a
    -- send + receive pair. The blocking 'Network.Connection'
    -- API is not safe to share between threads — two
    -- concurrent 'sendRequestReceiveResponse' calls on the
    -- same socket interleave their writes and read each
    -- other's response bodies.  The consumer's heartbeat
    -- thread + poll loop both target the coordinator broker,
    -- so the lock here is what stops them from corrupting
    -- the framing.
  }

-- | Create a new connection manager.
createConnectionManager :: IO ConnectionManager
createConnectionManager = ConnectionManager <$> StmMap.newIO <*> StmMap.newIO

-- | Run @action@ with the per-broker connection lock held.  See
-- 'connectionLocks' for the rationale.  Safe to nest the same
-- broker on the same thread (uses 'withMVar' which is not
-- recursive — callers must avoid that).
withBrokerLock :: ConnectionManager -> BrokerAddress -> IO a -> IO a
withBrokerLock (ConnectionManager _ lockMap) addr action = do
  lock <- atomically (StmMap.lookup addr lockMap) >>= \case
    Just l  -> pure l
    Nothing -> do
      fresh <- newMVar ()
      atomically $ do
        m <- StmMap.lookup addr lockMap
        case m of
          Just existing -> pure existing
          Nothing       -> do
            StmMap.insert fresh addr lockMap
            pure fresh
  withMVar lock (\_ -> action)

-- | Get an existing connection or create a new one for the given broker.
-- Reuses the connection if one already exists.
--
-- This implements the standard pattern:
-- - One persistent connection per broker
-- - Connections are reused for all requests to that broker
-- - Request pipelining handles concurrency on the single connection
getOrCreateConnection
  :: ConnectionManager
  -> BrokerAddress
  -> ConnectionConfig
  -> IO (Either String Connection)
getOrCreateConnection cm@(ConnectionManager connMap _) addr config = do
  -- Try to get existing connection
  existingConnM <- atomically $ StmMap.lookup addr connMap
  case existingConnM of
    Just existingConn -> do
      -- Confirm the cached connection is still alive before
      -- handing it back. If the broker (or an intermediary) has
      -- closed it underneath us, evict it and reconnect.
      alive <- isConnected existingConn
      if alive
        then return $ Right existingConn
        else do
          atomically (StmMap.delete addr connMap)
          (do
             -- recurse via 'getOrCreateConnection' so we don't
             -- duplicate the SASL bootstrap logic.
             getOrCreateConnection cm addr config)
    Nothing -> do
      -- No existing connection, create a new one
      connResult <- if connUseTls config
        then connectTls addr config
        else connect addr config
      case connResult of
        Left err -> return $ Left err
        Right newConn -> do
          -- Run SASL authentication if configured. We do this *before*
          -- caching the connection so a failed handshake doesn't leave
          -- a poisoned connection in the pool.
          authResult <- case connSasl config of
            Nothing  -> return (Right ())
            Just sc  -> do
              let host = T.pack (brokerHost addr)
              r <- SASL.authenticate newConn (connClientId config) host sc
              case r of
                Right () -> return (Right ())
                Left e   -> return (Left ("SASL authentication failed: " ++ show e))
          case authResult of
            Left err -> do
              Conn.connectionClose newConn
              return $ Left err
            Right () -> do
              -- Store the (now-authenticated) connection
              atomically $ StmMap.insert newConn addr connMap
              return $ Right newConn

-- | Close all connections managed by this connection manager.
-- This should be called when shutting down the client.
closeAllConnections :: ConnectionManager -> IO ()
closeAllConnections (ConnectionManager connMap lockMap) = do
  connections <- atomically $ do
    pairs <- ListT.toList $ StmMap.listT connMap
    return $ map snd pairs
  mapM_ disconnect connections
  atomically $ do
    StmMap.reset connMap
    StmMap.reset lockMap

-- | Calculate exponential backoff delay with jitter.
-- Returns delay in microseconds for threadDelay.
calculateBackoffDelay
  :: Int  -- ^ Attempt number (0-based)
  -> ConnectionConfig
  -> IO Int
calculateBackoffDelay attemptNum config = do
  let baseDelayMs = fromIntegral (connRetryDelay config)
      multiplier = connBackoffMultiplier config
      maxDelayMs = fromIntegral (connBackoffMaxMs config)
      -- Calculate exponential backoff: baseDelay * (multiplier ^ attempt)
      exponentialDelayMs = min maxDelayMs (baseDelayMs * (multiplier ** fromIntegral attemptNum))
  
  -- Add jitter: random value between 0.8 and 1.2 of the base delay
  -- This helps avoid thundering herd when many clients retry simultaneously
  jitterFactor <- randomRIO (0.8, 1.2)
  let finalDelayMs = exponentialDelayMs * jitterFactor
      delayMicros = round (finalDelayMs * 1000)
  
  return delayMicros

-- | Open a fresh 'NS.Socket' to the broker, apply the
-- platform-tunable socket options spelled out in
-- 'ConnectionConfig', then connect.
--
-- Why we don't use 'Conn.connectTo':
--
-- 'Conn.connectTo' opens the socket internally and gives us the
-- 'Conn.Connection' wrapper directly. That's convenient but
-- there's no hook for setting socket options /before/ 'connect',
-- and 'TCP_NODELAY' / 'SO_SNDBUF' / 'SO_RCVBUF' /must/ be set
-- before 'connect' to take effect on the initial SYN handshake
-- (the kernel uses the buffer-size option to tune the
-- advertised receive window in the SYN, and the NODELAY option
-- needs to be in place before the first user write so the
-- broker doesn't see a small write that triggers Nagle).
--
-- So we do the socket creation manually (mirroring what
-- 'Network.Connection''s @resolve\'@ does internally) + apply
-- our tunings + then hand the connected socket to
-- 'Conn.connectFromSocket' for the (optional) TLS handshake +
-- the buffered read API.
{-# INLINE openTunedSocket #-}
openTunedSocket
  :: ConnectionConfig
  -> HostName
  -> PortNumber
  -> IO NS.Socket
openTunedSocket cfg host port = do
  let !hints = NS.defaultHints
        { NS.addrFlags      = [NS.AI_ADDRCONFIG]
        , NS.addrSocketType = NS.Stream
        , NS.addrFamily     = case connBrokerAddressFamily cfg of
            BrokerAddressIPv4 -> NS.AF_INET
            BrokerAddressIPv6 -> NS.AF_INET6
            BrokerAddressAny  -> NS.AF_UNSPEC
        }
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just (show port))
  let attempt addr = bracketOnError
        (NS.socket (NS.addrFamily addr) (NS.addrSocketType addr)
                   (NS.addrProtocol addr))
        NS.close
        $ \sock -> do
            applyTcpTuning cfg sock
            NS.connect sock (NS.addrAddress addr)
            pure sock
      tryAll []     = ioError (userError ("openTunedSocket: no addrinfo for " ++ host ++ ":" ++ show port))
      tryAll [a]    = attempt a
      tryAll (a:as) = do
        r <- try (attempt a)
        case r of
          Right sock           -> pure sock
          Left (_ :: SomeException) -> tryAll as
  tryAll addrs

-- | Apply our TCP-level tuning to a socket /before/ 'connect'.
-- Each option is wrapped in a 'try' so that exotic kernels that
-- don't recognise one of them don't fail the whole connection
-- (for example, 'NS.UserTimeout' is Linux-only; on macOS the
-- 'setSocketOption' call throws, and we'd rather lose that one
-- option than the whole broker connection).
{-# INLINE applyTcpTuning #-}
applyTcpTuning :: ConnectionConfig -> NS.Socket -> IO ()
applyTcpTuning cfg sock = do
  -- TCP_NODELAY: disable Nagle's algorithm. Kafka clients always
  -- want this — every write is already a complete framed request
  -- (the producer batches at the BatchAccumulator layer, the
  -- consumer issues whole Fetch requests), so leaving Nagle on
  -- can only hurt: it would coalesce nothing useful and add
  -- ~40 ms of TX latency waiting for an in-flight ACK before
  -- releasing the next request.
  --
  -- The JVM client unconditionally calls
  -- 'socketChannel.socket().setTcpNoDelay(true)' on every
  -- broker socket; librdkafka exposes 'socket.nagle.disable' but
  -- recommends turning it on for production. We default it on
  -- here.
  when (connSocketNagleDisable cfg) $
    silently $ NS.setSocketOption sock NS.NoDelay 1

  -- SO_KEEPALIVE: have the kernel send periodic empty TCP
  -- segments so we notice silent dead-peer cases (NAT timeouts,
  -- mid-flight network partitions, broker crashes) instead of
  -- waiting until the next application-level send fails. The
  -- /interval/ between probes uses the OS default
  -- ('net.ipv4.tcp_keepalive_*' on Linux); we don't try to
  -- override it because the relevant @TCP_KEEPIDLE@ /
  -- @TCP_KEEPINTVL@ / @TCP_KEEPCNT@ options aren't surfaced
  -- portably by the 'network' package.
  when (connSocketKeepalive cfg) $
    silently $ NS.setSocketOption sock NS.KeepAlive 1

  -- SO_SNDBUF / SO_RCVBUF: caller-supplied hints. Kernel may
  -- round up to its own minimum / clamp to its configured
  -- maximum. 0 (the default) means \"use the OS default\".
  --
  -- For a high-throughput producer or large fetch.max.bytes
  -- consumer, bumping these well past the default 64 KiB lets
  -- the per-RTT bandwidth-delay product fit in-flight without
  -- back-pressuring the application thread on every small
  -- response.
  when (connSocketSendBuffer cfg > 0) $
    silently $ NS.setSocketOption sock NS.SendBuffer (connSocketSendBuffer cfg)
  when (connSocketReceiveBuffer cfg > 0) $
    silently $ NS.setSocketOption sock NS.RecvBuffer (connSocketReceiveBuffer cfg)
  where
    silently :: IO () -> IO ()
    silently act = do
      _ <- try act :: IO (Either SomeException ())
      pure ()

-- | Establish a plain TCP connection to a Kafka broker with retry logic.
connect
  :: BrokerAddress
  -> ConnectionConfig
  -> IO (Either String Connection)
connect addr config = connectWithRetry addr config 0
  where
    connectWithRetry :: BrokerAddress -> ConnectionConfig -> Int -> IO (Either String Connection)
    connectWithRetry BrokerAddress{..} cfg attemptNum = do
      result <- try $ do
        ctx  <- Conn.initConnectionContext
        sock <- openTunedSocket cfg brokerHost brokerPort
        Conn.connectFromSocket ctx sock Conn.ConnectionParams
          { Conn.connectionHostname  = brokerHost
          , Conn.connectionPort      = fromIntegral brokerPort
          , Conn.connectionUseSecure = Nothing
          , Conn.connectionUseSocks  = Nothing
          }

      case result of
        Right conn -> return $ Right conn
        Left (e :: SomeException) ->
          if attemptNum < connMaxRetries cfg
            then do
              -- Calculate backoff delay and retry
              delayMicros <- calculateBackoffDelay attemptNum cfg
              putStrLn $ "Connection attempt " ++ show (attemptNum + 1) ++
                        " to " ++ brokerHost ++ ":" ++ show brokerPort ++
                        " failed: " ++ show e ++
                        ". Retrying in " ++ show (delayMicros `div` 1000) ++ "ms..."
              threadDelay delayMicros
              connectWithRetry (BrokerAddress brokerHost brokerPort) cfg (attemptNum + 1)
            else
              return $ Left $ "Failed to connect to " ++ brokerHost ++ ":" ++
                             show brokerPort ++ " after " ++ show (attemptNum + 1) ++
                             " attempts: " ++ show e

-- | Establish a TLS-encrypted connection to a Kafka broker with retry logic.
connectTls
  :: BrokerAddress
  -> ConnectionConfig
  -> IO (Either String Connection)
connectTls addr config = 
  case connTlsSettings config of
    Nothing ->
      return $ Left "TLS enabled but no TLS settings provided"
    Just tlsParams ->
      connectTlsWithRetry addr config tlsParams 0
  where
    connectTlsWithRetry :: BrokerAddress -> ConnectionConfig -> TLS.ClientParams -> Int -> IO (Either String Connection)
    connectTlsWithRetry BrokerAddress{..} cfg tlsParams attemptNum = do
      result <- try $ do
        ctx  <- Conn.initConnectionContext
        sock <- openTunedSocket cfg brokerHost brokerPort
        -- 'connectFromSocket' performs the TLS handshake when
        -- 'connectionUseSecure' is 'Just', using our already-
        -- tuned socket as the transport.
        Conn.connectFromSocket ctx sock Conn.ConnectionParams
          { Conn.connectionHostname  = brokerHost
          , Conn.connectionPort      = fromIntegral brokerPort
          , Conn.connectionUseSecure = Just $ Conn.TLSSettings tlsParams
          , Conn.connectionUseSocks  = Nothing
          }
      
      case result of
        Right conn -> return $ Right conn
        Left (e :: SomeException) ->
          if attemptNum < connMaxRetries cfg
            then do
              -- Calculate backoff delay and retry
              delayMicros <- calculateBackoffDelay attemptNum cfg
              putStrLn $ "TLS connection attempt " ++ show (attemptNum + 1) ++ 
                        " to " ++ brokerHost ++ ":" ++ show brokerPort ++ 
                        " failed: " ++ show e ++ 
                        ". Retrying in " ++ show (delayMicros `div` 1000) ++ "ms..."
              threadDelay delayMicros
              connectTlsWithRetry (BrokerAddress brokerHost brokerPort) cfg tlsParams (attemptNum + 1)
            else
              return $ Left $ "Failed to connect (TLS) to " ++ brokerHost ++ ":" ++ 
                             show brokerPort ++ " after " ++ show (attemptNum + 1) ++ 
                             " attempts: " ++ show e

-- | Close a connection to a Kafka broker.
disconnect :: Connection -> IO ()
disconnect conn = Conn.connectionClose conn

-- | Use a connection with automatic resource management.
-- The connection is automatically closed when the action completes
-- or if an exception is thrown.
--
-- Example:
--
-- > result <- withConnection addr config $ \conn -> do
-- >   sendRequest conn request
-- >   receiveResponse conn
withConnection
  :: BrokerAddress
  -> ConnectionConfig
  -> (Connection -> IO a)
  -> IO (Either String a)
withConnection addr config action = do
  connResult <- if connUseTls config
    then connectTls addr config
    else connect addr config
  case connResult of
    Left err -> return $ Left err
    Right conn -> do
      result <- try $ action conn
      disconnect conn
      return $ case result of
        Left (e :: SomeException) -> Left $ "Connection action failed: " ++ show e
        Right val -> Right val

-- | Check whether a connection is still usable. The underlying
-- 'Network.Connection' doesn't expose a non-blocking liveness
-- probe, so we layer two checks:
--
--   1. 'connectionWaitForInput' with a 0 ms timeout — returns
--      'True' when the kernel recv buffer has either real data
--      or a queued FIN, 'False' on an alive idle connection.
--      Documented to never block when the timeout is zero.
--
--   2. If the wait reports input is available, do a
--      side-effect-free 'connectionGetChunk'' that puts the
--      chunk straight back into the buffer (so the next real
--      read still sees it). An empty chunk indicates the kernel
--      delivered EOF — the peer has closed.
--
-- A throw at any point (ECONNRESET, the socket fd was closed,
-- etc.) is treated as a dead connection.
--
-- == Caveat
--
-- The probe is best-effort. Two scenarios can still report a
-- dead connection as alive: (a) a silently half-open connection
-- (intermediary NAT timed it out without sending a FIN), and
-- (b) a peer that has closed but whose FIN hasn't yet propagated
-- to the recv buffer at probe time. In both cases the next real
-- I/O surfaces the failure and the pool eviction kicks in then.
isConnected :: Connection -> IO Bool
isConnected conn = do
  waitR <- try (Conn.connectionWaitForInput conn 0)
             :: IO (Either SomeException Bool)
  case waitR of
    Left  _      -> pure False
    Right False  -> pure True   -- alive but idle
    Right True   -> do
      r <- try (Conn.connectionGetChunk' conn (\bs -> (bs, bs)))
             :: IO (Either SomeException BS.ByteString)
      pure $ case r of
        Right bs -> not (BS.null bs)
        Left  _  -> False

