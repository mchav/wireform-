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
    -- * TLS offload (sidecar / kTLS / NLB)
    -- $tlsoffload
  , TlsOffloadConfig (..)
  , TlsOffloadEndpoint (..)
  , OffloadBrokerKey (..)
  , transparentTlsOffload
  , staticTlsOffload
  , perBrokerTlsOffload
  , customTlsOffload
  , brokerAddressToOffloadKey
  , connectOffload
    -- * Backoff Utilities
  , calculateBackoffDelay
    -- * Pluggable host resolution
  , HostResolver (..)
    -- * Connection-quota throttling
  , ConnectionQuotaState
  , newConnectionQuotaState
  , recordThrottle
  , shouldDelayConnect
    -- * Idle-expiry tracking
  , IdleConnTracker
  , newIdleConnTracker
  , recordActivity
  , isIdle
    -- * SASL timeouts
  , SaslTimeouts (..)
  , defaultSaslTimeouts
    -- * Request quality-of-service
  , QosClass (..)
  , QosWeight (..)
  , prioritise
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracketOnError, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Default.Class (def)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
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
import qualified Kafka.Network.TlsOffload as TlsOffload
import Kafka.Network.TlsOffload
  ( OffloadBrokerKey (..)
  , TlsOffloadConfig (..)
  , TlsOffloadEndpoint (..)
  , describeOffloadEndpoint
  , transparentTlsOffload
  , staticTlsOffload
  , perBrokerTlsOffload
  , customTlsOffload
  )

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
  , connTlsOffload :: !(Maybe TlsOffloadConfig)
    -- ^ TLS offload to a sidecar / Unix socket / kTLS path.
    --   When 'Just', the client side TLS handshake is skipped
    --   regardless of 'connUseTls' (the offload target is
    --   responsible for the upstream cipher work) and every
    --   broker connection is routed through the physical
    --   endpoint returned by 'TlsOffload.resolveOffloadEndpoint'.
    --   See "Kafka.Network.TlsOffload" for the supported
    --   deployment shapes (sidecar / kTLS / NLB / stunnel).
    --
    --   The connection pool is still keyed by the logical
    --   'BrokerAddress', so per-broker SASL state and request
    --   pipelining are preserved when several brokers fan in
    --   to the same physical socket destination.
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
  , connTlsOffload                      = Nothing
  }

-- $tlsoffload
--
-- The fields and constructors re-exported below let a caller
-- delegate the actual TLS handshake to a sidecar proxy, a
-- TLS-terminating load balancer, or kernel-level TLS (Linux
-- @CONFIG_TLS@). When 'connTlsOffload' is 'Just',
-- 'getOrCreateConnection':
--
--   * skips the @crypton-connection@ TLS handshake, regardless
--     of the value of 'connUseTls';
--   * resolves each broker via
--     'TlsOffload.resolveOffloadEndpoint' to find the physical
--     destination — falling back to the broker's advertised
--     address when the resolver returns 'Nothing' (transparent
--     mode);
--   * opens either a plain TCP connection or a Unix-domain
--     stream socket depending on the endpoint variant.
--
-- The connection pool is still keyed by logical
-- 'BrokerAddress', so several brokers can fan in to the same
-- sidecar listener while keeping independent SASL state and
-- request pipelining.

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
      -- No existing connection, create a new one. The offload
      -- path takes priority over both 'connUseTls' and the
      -- plain TCP path: when offload is configured, all
      -- broker traffic flows through the sidecar / kTLS path
      -- regardless of what the in-process TLS knobs say.
      connResult <- case connTlsOffload config of
        Just offload -> connectOffload addr config offload
        Nothing
          | connUseTls config -> connectTls addr config
          | otherwise         -> connect addr config
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

-- | Open a connected Unix-domain stream socket at @path@.
--
-- We avoid 'Conn.connectTo' for the same reason 'openTunedSocket'
-- does: we want to apply our socket options before connecting,
-- and the TCP tuning ('NoDelay', send/receive buffers,
-- keepalive) doesn't apply to a UDS anyway. The result socket
-- is handed to 'Conn.connectFromSocket' so the rest of the
-- pipeline keeps the same 'Conn.Connection' shape.
{-# INLINE openUnixSocket #-}
openUnixSocket :: FilePath -> IO NS.Socket
openUnixSocket path = bracketOnError
  (NS.socket NS.AF_UNIX NS.Stream 0)
  NS.close
  (\sock -> do
      NS.connect sock (NS.SockAddrUnix path)
      pure sock)

-- | Translate a logical 'BrokerAddress' into the
-- 'OffloadBrokerKey' the offload resolver expects.
brokerAddressToOffloadKey :: BrokerAddress -> OffloadBrokerKey
brokerAddressToOffloadKey BrokerAddress{..} =
  OffloadBrokerKey { offloadBrokerHost = brokerHost
                   , offloadBrokerPort = brokerPort
                   }

-- | Open a plain (non-TLS) connection through the configured
-- offload endpoint, falling back to the broker's advertised
-- address when the offload resolver declines (the
-- \"transparent\" case).
--
-- This is the per-attempt body for offloaded connections; the
-- caller adds retry / backoff via 'connectOffload'.
{-# INLINE openOffloadOnce #-}
openOffloadOnce
  :: BrokerAddress
  -> ConnectionConfig
  -> TlsOffloadConfig
  -> IO (Either String (Connection, String))
openOffloadOnce addr cfg offload = do
  let key = brokerAddressToOffloadKey addr
  mEndpoint <- TlsOffload.resolveOffloadEndpoint offload key
  result <- try $ do
    ctx <- Conn.initConnectionContext
    case mEndpoint of
      Just (TlsOffload.TlsOffloadUnix path) -> do
        sock <- openUnixSocket path
        conn <- Conn.connectFromSocket ctx sock Conn.ConnectionParams
          { Conn.connectionHostname  = brokerHost addr
          , Conn.connectionPort      = fromIntegral (brokerPort addr)
          , Conn.connectionUseSecure = Nothing
          , Conn.connectionUseSocks  = Nothing
          }
        pure (conn, describeOffloadEndpoint (TlsOffload.TlsOffloadUnix path))
      Just (TlsOffload.TlsOffloadTcp h p) -> do
        sock <- openTunedSocket cfg h p
        conn <- Conn.connectFromSocket ctx sock Conn.ConnectionParams
          { Conn.connectionHostname  = h
          , Conn.connectionPort      = fromIntegral p
          , Conn.connectionUseSecure = Nothing
          , Conn.connectionUseSocks  = Nothing
          }
        pure (conn, describeOffloadEndpoint (TlsOffload.TlsOffloadTcp h p))
      Nothing -> do
        -- Transparent offload: open plain TCP to the broker's
        -- own address. The cipher work happens out-of-band
        -- (iptables redirect, kTLS, NLB) so we don't run our
        -- own TLS handshake.
        sock <- openTunedSocket cfg (brokerHost addr) (brokerPort addr)
        conn <- Conn.connectFromSocket ctx sock Conn.ConnectionParams
          { Conn.connectionHostname  = brokerHost addr
          , Conn.connectionPort      = fromIntegral (brokerPort addr)
          , Conn.connectionUseSecure = Nothing
          , Conn.connectionUseSocks  = Nothing
          }
        pure (conn, "transparent:" <> brokerHost addr <> ":" <> show (brokerPort addr))
  case result of
    Right ok                  -> pure (Right ok)
    Left (e :: SomeException) -> pure (Left (show e))

-- | Establish a TLS-offloaded connection to a broker with the
-- standard retry / backoff loop. The bytes leaving the client
-- are plain Kafka wire; the endpoint at the other side
-- (sidecar / NLB / kTLS) is responsible for upstream TLS.
--
-- This is the public form of the offload code path —
-- 'getOrCreateConnection' uses it internally, but it's
-- exported so callers that just need a one-off offloaded
-- broker socket can avoid wiring up a 'ConnectionManager'.
connectOffload
  :: BrokerAddress
  -> ConnectionConfig
  -> TlsOffloadConfig
  -> IO (Either String Connection)
connectOffload addr config offload = go 0
  where
    go attemptNum = do
      r <- openOffloadOnce addr config offload
      case r of
        Right (conn, _label) -> pure (Right conn)
        Left err ->
          if attemptNum < connMaxRetries config
            then do
              delayMicros <- calculateBackoffDelay attemptNum config
              putStrLn $ "TLS-offload connection attempt "
                        ++ show (attemptNum + 1)
                        ++ " to " ++ brokerHost addr ++ ":"
                        ++ show (brokerPort addr)
                        ++ " (offload=" ++ T.unpack (TlsOffload.tlsOffloadLabel offload) ++ ")"
                        ++ " failed: " ++ err
                        ++ ". Retrying in " ++ show (delayMicros `div` 1000) ++ "ms..."
              threadDelay delayMicros
              go (attemptNum + 1)
            else
              pure $ Left $
                "Failed to connect (TLS-offload " ++ T.unpack (TlsOffload.tlsOffloadLabel offload)
                ++ ") to " ++ brokerHost addr ++ ":" ++ show (brokerPort addr)
                ++ " after " ++ show (attemptNum + 1)
                ++ " attempts: " ++ err

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
  connResult <- case connTlsOffload config of
    Just offload -> connectOffload addr config offload
    Nothing
      | connUseTls config -> connectTls addr config
      | otherwise         -> connect addr config
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

----------------------------------------------------------------------
-- Additional connection-layer ergonomics
--
-- Previously lived in @Kafka.Client.ConnectionExtras@ +
-- @Kafka.Client.AdminExtras@ (the 'HostResolver' record).
----------------------------------------------------------------------

-- | A pluggable hostname resolver — useful for service-mesh /
-- multi-cluster setups where DNS isn't authoritative. Returns the
-- resolved IP string(s); callers iterate the result with the same
-- shape librdkafka uses.
newtype HostResolver = HostResolver
  { resolveHost :: Text -> IO [Text]
  }

-- | Tracks broker-imposed @connection.creation.rate@ throttles so
-- the client can back off before opening another connection.
newtype ConnectionQuotaState = ConnectionQuotaState
  { cqsThrottleUntilMs :: TVar Int64 }

newConnectionQuotaState :: IO ConnectionQuotaState
newConnectionQuotaState = ConnectionQuotaState <$> newTVarIO 0

-- | Record a broker-imposed throttle (the broker's
-- @connection.creation.rate.throttle.ms@ response). The client
-- waits until @nowMs >= cqsThrottleUntilMs@ before opening another
-- connection.
recordThrottle :: ConnectionQuotaState -> Int64 -> Int -> IO ()
recordThrottle st now throttleMs = atomically $
  writeTVar (cqsThrottleUntilMs st) (now + fromIntegral throttleMs)

shouldDelayConnect :: ConnectionQuotaState -> Int64 -> IO (Maybe Int)
shouldDelayConnect st now = do
  until_ <- readTVarIO (cqsThrottleUntilMs st)
  let !wait = until_ - now
  pure $ if wait > 0 then Just (fromIntegral wait) else Nothing

-- | Tracks per-key last-activity timestamps so the connection pool
-- can age out idle entries.
newtype IdleConnTracker key = IdleConnTracker
  { ictLastActivity :: TVar (Map key Int64) }

newIdleConnTracker :: Ord key => IO (IdleConnTracker key)
newIdleConnTracker = IdleConnTracker <$> newTVarIO Map.empty

recordActivity :: Ord key => IdleConnTracker key -> key -> Int64 -> IO ()
recordActivity (IdleConnTracker v) k now = atomically $
  modifyTVar' v (Map.insert k now)

isIdle
  :: Ord key
  => IdleConnTracker key
  -> key
  -> Int64        -- ^ now (ms)
  -> Int          -- ^ idle threshold (ms)
  -> IO Bool
isIdle (IdleConnTracker v) k now thresholdMs = do
  m <- readTVarIO v
  pure $ case Map.lookup k m of
    Nothing -> True
    Just ts -> now - ts >= fromIntegral thresholdMs

-- | Bounds on the SASL handshake + idle window.
data SaslTimeouts = SaslTimeouts
  { saslConnectTimeoutMs :: !Int
    -- ^ Bound on the initial SASL handshake.
  , saslMaxIdleMs        :: !Int
    -- ^ Per-session idle window before forced re-auth.
    --   @0@ = unbounded.
  }
  deriving stock (Eq, Show, Generic)

defaultSaslTimeouts :: SaslTimeouts
defaultSaslTimeouts = SaslTimeouts
  { saslConnectTimeoutMs = 30_000
  , saslMaxIdleMs        = 0
  }

-- | Priority class used by 'prioritise' for fair-share request
-- scheduling.
data QosClass
  = QosCritical
  | QosHigh
  | QosNormal
  | QosLow
  deriving stock (Eq, Ord, Show, Generic)

newtype QosWeight = QosWeight { unQosWeight :: Int }
  deriving stock (Eq, Show, Generic)

-- | Sort a queue of @(class, payload)@ entries with critical
-- requests first, low last. The relative order within a class is
-- preserved (stable sort).
prioritise :: [(QosClass, a)] -> [(QosClass, a)]
prioritise = L.sortOn (\(c, _) -> classWeight c)
  where
    classWeight QosCritical = 0 :: Int
    classWeight QosHigh     = 1
    classWeight QosNormal   = 2
    classWeight QosLow      = 3

