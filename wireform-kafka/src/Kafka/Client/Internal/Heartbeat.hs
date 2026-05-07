{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Internal.Heartbeat
Description : Consumer group heartbeat background thread
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the heartbeat background thread for consumer groups.

The heartbeat thread:
- Sends periodic Heartbeat requests to the group coordinator
- Detects session expiration or coordinator failures
- Triggers rebalance when needed
- Runs independently of the main consumer poll loop

Heartbeat interval should be less than session timeout (typically 1/3).
If heartbeats fail or stop, the consumer is removed from the group.
-}
module Kafka.Client.Internal.Heartbeat
  ( -- * Heartbeat State
    HeartbeatState(..)
  , createHeartbeatState
    -- * Heartbeat Thread
  , startHeartbeatThread
  , stopHeartbeatThread
    -- * Heartbeat Operations
  , sendHeartbeat
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Network.Connection (Connection)

import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Network.Connection as Conn
import Kafka.Network.Connection (BrokerAddress(..))
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.Generated.HeartbeatRequest as HBReq
import qualified Kafka.Protocol.Generated.HeartbeatResponse as HBResp
import qualified Kafka.Protocol.Primitives as P

-- | State for the heartbeat thread
data HeartbeatState = HeartbeatState
  { hbGroupId :: !Text
    -- ^ Consumer group ID
  , hbMemberId :: !(TVar Text)
    -- ^ Current member ID (updated after join)
  , hbGenerationId :: !(TVar Int32)
    -- ^ Current generation ID (updated after join)
  , hbCoordinatorAddr :: !(TVar (Maybe BrokerAddress))
    -- ^ Group coordinator address
  , hbIntervalMs :: !Int
    -- ^ Heartbeat interval in milliseconds
  , hbConnManager :: !Conn.ConnectionManager
    -- ^ Connection manager
  , hbVersionCache :: !AV.ApiVersionCache
    -- ^ Version cache for API negotiation
  , hbClientId :: !Text
    -- ^ Client ID
  , hbCorrelationId :: !(TVar Int32)
    -- ^ Next correlation ID
  , hbRunning :: !(TVar Bool)
    -- ^ Whether the heartbeat thread should keep running
  , hbNeedsRebalance :: !(TVar Bool)
    -- ^ Whether a rebalance is needed
  }

-- | Create a new heartbeat state
createHeartbeatState
  :: Text                        -- ^ Group ID
  -> Int                         -- ^ Heartbeat interval (ms)
  -> Conn.ConnectionManager      -- ^ Connection manager
  -> AV.ApiVersionCache          -- ^ Version cache
  -> Text                        -- ^ Client ID
  -> IO HeartbeatState
createHeartbeatState groupId intervalMs connMgr versionCache clientId = do
  memberId <- newTVarIO ""
  genId <- newTVarIO (-1)
  coordAddr <- newTVarIO Nothing
  corrId <- newTVarIO 0
  running <- newTVarIO True
  needsRebal <- newTVarIO False
  
  return HeartbeatState
    { hbGroupId = groupId
    , hbMemberId = memberId
    , hbGenerationId = genId
    , hbCoordinatorAddr = coordAddr
    , hbIntervalMs = intervalMs
    , hbConnManager = connMgr
    , hbVersionCache = versionCache
    , hbClientId = clientId
    , hbCorrelationId = corrId
    , hbRunning = running
    , hbNeedsRebalance = needsRebal
    }

-- | Start the heartbeat background thread
startHeartbeatThread :: HeartbeatState -> IO (Async ())
startHeartbeatThread state = async $ heartbeatLoop state

-- | Stop the heartbeat thread gracefully
stopHeartbeatThread :: HeartbeatState -> Async () -> IO ()
stopHeartbeatThread state thread = do
  atomically $ writeTVar (hbRunning state) False
  cancel thread

-- | Main heartbeat loop
heartbeatLoop :: HeartbeatState -> IO ()
heartbeatLoop state@HeartbeatState{..} = do
  -- Check if we should continue running
  shouldRun <- atomically $ readTVar hbRunning
  
  if not shouldRun
    then return ()  -- Exit loop
    else do
      -- Check if we have a valid member ID and coordinator
      (memberId, genId, coordAddrM) <- atomically $ do
        mid <- readTVar hbMemberId
        gid <- readTVar hbGenerationId
        caddr <- readTVar hbCoordinatorAddr
        return (mid, gid, caddr)
      
      -- Only send heartbeat if we're in a group
      if not (T.null memberId) && genId >= 0 && Just coordAddrM /= Nothing
        then do
          case coordAddrM of
            Nothing -> do
              -- No coordinator, wait and retry
              threadDelay (hbIntervalMs * 1000)
              heartbeatLoop state
            
            Just coordAddr -> do
              -- Send heartbeat
              result <- try $ sendHeartbeat state coordAddr memberId genId
              
              case result of
                Left (e :: SomeException) -> do
                  putStrLn $ "Heartbeat error: " ++ show e
                  -- Continue anyway, will retry next interval
                  
                Right (Left err) -> do
                  putStrLn $ "Heartbeat failed: " ++ err
                  -- Check if we need to rebalance based on error
                  -- TODO: Parse error codes and set needsRebalance flag
                  
                Right (Right needsRebalance) -> do
                  when needsRebalance $ do
                    atomically $ writeTVar hbNeedsRebalance True
              
              -- Wait for next heartbeat interval
              threadDelay (hbIntervalMs * 1000)
              heartbeatLoop state
        
        else do
          -- Not in a group yet, wait a bit
          threadDelay (hbIntervalMs * 1000)
          heartbeatLoop state

-- | Send a single heartbeat to the coordinator
sendHeartbeat
  :: HeartbeatState
  -> BrokerAddress      -- ^ Coordinator address
  -> Text               -- ^ Member ID
  -> Int32              -- ^ Generation ID
  -> IO (Either String Bool)  -- ^ Returns whether rebalance is needed
sendHeartbeat HeartbeatState{..} coordAddr memberId genId = do
  -- Get or create connection to coordinator
  connResult <- Conn.getOrCreateConnection hbConnManager coordAddr Conn.defaultConnectionConfig
  
  case connResult of
    Left err -> return $ Left $ "Failed to connect to coordinator: " ++ err
    
    Right conn -> do
      -- Get correlation ID
      corrId <- atomically $ do
        cid <- readTVar hbCorrelationId
        writeTVar hbCorrelationId (cid + 1)
        return cid
      
      let apiKey = 12  -- Heartbeat API key
          clientMaxVersion = 4  -- Max version we support
      
      -- Query broker's supported version
      brokerVersionM <- atomically $ AV.queryApiVersion hbVersionCache coordAddr apiKey
      
      let apiVersion = case brokerVersionM of
            Nothing -> 0  -- Fall back to v0 if unknown
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0  -- Fall back if incompatible
              Just v -> v
          
          request = HBReq.HeartbeatRequest
            { HBReq.heartbeatRequestGroupId = P.mkKafkaString hbGroupId
            , HBReq.heartbeatRequestGenerationId = genId
            , HBReq.heartbeatRequestMemberId = P.mkKafkaString memberId
            , HBReq.heartbeatRequestGroupInstanceId = P.KafkaString P.Null
            }
          
          requestBody = runPutS $ HBReq.encodeHeartbeatRequest apiVersion request
          clientIdKafka = P.mkKafkaString hbClientId
      
      -- Send request and receive response
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ "Heartbeat request failed: " ++ err
        
        Right (_, responseBody) -> do
          case runGetS (HBResp.decodeHeartbeatResponse apiVersion) responseBody of
            Left err -> return $ Left $ "Failed to parse HeartbeatResponse: " ++ err
            
            Right response -> do
              let errorCode = HBResp.heartbeatResponseErrorCode response
              
              case errorCode of
                0 -> return $ Right False  -- Success, no rebalance needed
                
                27 -> return $ Right True  -- REBALANCE_IN_PROGRESS
                
                25 -> return $ Left "Unknown member ID, need to rejoin"  -- UNKNOWN_MEMBER_ID
                
                22 -> return $ Left "Illegal generation, need to rejoin"  -- ILLEGAL_GENERATION
                
                _ -> return $ Left $ "Heartbeat error code: " ++ show errorCode

