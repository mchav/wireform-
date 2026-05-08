{-# LANGUAGE DeriveGeneric #-}
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
    -- * Connection State
  , isConnected
    -- * Default Configuration
  , defaultConnectionConfig
  , defaultTlsSettings
    -- * Backoff Utilities
  , calculateBackoffDelay
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.Default.Class (def)
import Data.Hashable (Hashable(hashWithSalt))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified ListT
import Network.Connection (Connection(..), ConnectionParams(..))
import qualified Network.Connection as Conn
import Network.Socket (HostName, PortNumber)
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
  , connSocketKeepalive                 = False
  , connSocketNagleDisable              = False
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
newtype ConnectionManager = ConnectionManager
  { connectionMap :: StmMap.Map BrokerAddress Connection
  }

-- | Create a new connection manager.
createConnectionManager :: IO ConnectionManager
createConnectionManager = ConnectionManager <$> StmMap.newIO

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
getOrCreateConnection (ConnectionManager connMap) addr config = do
  -- Try to get existing connection
  existingConnM <- atomically $ StmMap.lookup addr connMap
  case existingConnM of
    Just existingConn -> do
      -- TODO: Check if connection is still alive
      -- For now, assume it's alive and return it
      return $ Right existingConn
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
closeAllConnections (ConnectionManager connMap) = do
  -- Get all connections from the map
  connections <- atomically $ do
    pairs <- ListT.toList $ StmMap.listT connMap
    return $ map snd pairs
  -- Close each connection
  mapM_ disconnect connections
  -- Clear the map
  atomically $ StmMap.reset connMap

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
        ctx <- Conn.initConnectionContext
        Conn.connectTo ctx Conn.ConnectionParams
          { Conn.connectionHostname = brokerHost
          , Conn.connectionPort = fromIntegral brokerPort
          , Conn.connectionUseSecure = Nothing
          , Conn.connectionUseSocks = Nothing
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
        ctx <- Conn.initConnectionContext
        Conn.connectTo ctx Conn.ConnectionParams
          { Conn.connectionHostname = brokerHost
          , Conn.connectionPort = fromIntegral brokerPort
          , Conn.connectionUseSecure = Just $ Conn.TLSSettings tlsParams
          , Conn.connectionUseSocks = Nothing
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

-- | Check if a connection is still active.
--
-- TODO: Implement connection liveness check
-- This should:
--   - Attempt a non-blocking read or status check
--   - Return False if the connection is closed or broken
isConnected :: Connection -> IO Bool
isConnected conn = do
  -- TODO: Implement proper connection check
  -- For now, always return True (assume connected)
  return True

