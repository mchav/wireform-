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

-- | Connection configuration.
data ConnectionConfig = ConnectionConfig
  { connTimeout :: !Int
    -- ^ Connection timeout in seconds (default: 10)
  , connReadTimeout :: !Int
    -- ^ Read timeout in seconds (default: 30)
  , connWriteTimeout :: !Int
    -- ^ Write timeout in seconds (default: 30)
  , connRetryDelay :: !Int
    -- ^ Initial retry delay in milliseconds (default: 100)
  , connMaxRetries :: !Int
    -- ^ Maximum number of connection retries (default: 3)
  , connBackoffMaxMs :: !Int
    -- ^ Maximum backoff delay in milliseconds (default: 32000)
  , connBackoffMultiplier :: !Double
    -- ^ Backoff multiplier for exponential backoff (default: 2.0)
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

-- | Default connection configuration (plain TCP, no TLS).
defaultConnectionConfig :: ConnectionConfig
defaultConnectionConfig = ConnectionConfig
  { connTimeout = 10
  , connReadTimeout = 30
  , connWriteTimeout = 30
  , connRetryDelay = 100
  , connMaxRetries = 3
  , connBackoffMaxMs = 32000
  , connBackoffMultiplier = 2.0
  , connUseTls = False
  , connTlsSettings = Nothing
  , connSasl = Nothing
  , connClientId = T.pack "wireform-kafka"
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

