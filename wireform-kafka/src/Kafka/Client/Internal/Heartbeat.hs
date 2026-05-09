{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
  , HeartbeatOutcome(..)
  , applyHeartbeatOutcome
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Int (Int16, Int32)
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
import qualified Kafka.Protocol.Wire.Codec as WC

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

                Right (Left outcome) -> do
                  putStrLn $ "Heartbeat needs rejoin: " ++ describe outcome
                  atomically $ applyHeartbeatOutcome state outcome

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

-- | Classified rejoin signal returned by a single 'sendHeartbeat'
-- call. The heartbeat loop pattern-matches on this so it can decide
-- whether to wipe the cached memberId before the next JoinGroup
-- (KIP-389: the broker has dropped us, so we must come back with an
-- empty memberId).
data HeartbeatOutcome
  = HeartbeatUnknownMember
    -- ^ Broker returned @UNKNOWN_MEMBER_ID (25)@. KIP-389: clear the
    --   cached memberId before rejoining.
  | HeartbeatFencedInstance
    -- ^ Broker returned @FENCED_INSTANCE_ID (82)@. KIP-345 static
    --   member that lost its slot to a newer instance. Clear memberId
    --   and rejoin so we get a fresh slot.
  | HeartbeatIllegalGeneration
    -- ^ Broker returned @ILLEGAL_GENERATION (22)@. The generation
    --   counter is stale; the memberId is still valid so don't clear
    --   it, just rejoin to pick up the new generation.
  | HeartbeatOtherError !Int16 !String
    -- ^ Any other broker-level error. Treat as "needs rejoin" but
    --   keep the memberId so the rejoin can be a no-op.
  | HeartbeatTransport !String
    -- ^ The request itself failed (network / parse error). The
    --   memberId is still valid; we'll just retry on the next tick.
  deriving (Eq, Show)

describe :: HeartbeatOutcome -> String
describe HeartbeatUnknownMember        = "UNKNOWN_MEMBER_ID (KIP-389)"
describe HeartbeatFencedInstance       = "FENCED_INSTANCE_ID (KIP-345)"
describe HeartbeatIllegalGeneration    = "ILLEGAL_GENERATION"
describe (HeartbeatOtherError ec _)    = "broker error " <> show ec
describe (HeartbeatTransport msg)      = "transport: " <> msg

-- | Update the in-memory heartbeat state in response to a non-OK
-- 'HeartbeatOutcome'. Always flips 'hbNeedsRebalance' so the next
-- 'poll' re-runs JoinGroup; additionally clears the cached
-- @memberId@ for UNKNOWN_MEMBER_ID / FENCED_INSTANCE_ID so the
-- rejoin goes out with an empty memberId (KIP-389 / KIP-345).
-- Transport-level failures don't touch any state because the broker
-- still considers us a member.
applyHeartbeatOutcome :: HeartbeatState -> HeartbeatOutcome -> STM ()
applyHeartbeatOutcome HeartbeatState{..} outcome = case outcome of
  HeartbeatTransport _ -> pure ()
  HeartbeatUnknownMember -> do
    writeTVar hbNeedsRebalance True
    writeTVar hbMemberId ""
  HeartbeatFencedInstance -> do
    writeTVar hbNeedsRebalance True
    writeTVar hbMemberId ""
  HeartbeatIllegalGeneration ->
    writeTVar hbNeedsRebalance True
  HeartbeatOtherError _ _ ->
    writeTVar hbNeedsRebalance True

-- | Send a single heartbeat to the coordinator. The 'Left' arm
-- carries a typed 'HeartbeatOutcome' so the heartbeat loop can
-- branch on UNKNOWN_MEMBER_ID / FENCED_INSTANCE_ID without parsing
-- string error messages.
sendHeartbeat
  :: HeartbeatState
  -> BrokerAddress      -- ^ Coordinator address
  -> Text               -- ^ Member ID
  -> Int32              -- ^ Generation ID
  -> IO (Either HeartbeatOutcome Bool)
sendHeartbeat HeartbeatState{..} coordAddr memberId genId = do
  -- Get or create connection to coordinator
  connResult <- Conn.getOrCreateConnection hbConnManager coordAddr Conn.defaultConnectionConfig
  
  case connResult of
    Left err -> return $ Left $ HeartbeatTransport $
      "Failed to connect to coordinator: " ++ err
    
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
          
          requestBody = WC.runEncodeVer @HBReq.HeartbeatRequest apiVersion request
          clientIdKafka = P.mkKafkaString hbClientId
      
      -- Send request and receive response
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ HeartbeatTransport $
          "Heartbeat request failed: " ++ err
        
        Right (_, responseBody) -> do
          case WC.runDecodeVer @HBResp.HeartbeatResponse apiVersion responseBody of
            Left err -> return $ Left $ HeartbeatTransport $
              "Failed to parse HeartbeatResponse: " ++ err
            
            Right response -> do
              let errorCode = HBResp.heartbeatResponseErrorCode response
              
              case errorCode of
                0  -> return $ Right False           -- success
                27 -> return $ Right True            -- REBALANCE_IN_PROGRESS
                25 -> return $ Left HeartbeatUnknownMember
                22 -> return $ Left HeartbeatIllegalGeneration
                82 -> return $ Left HeartbeatFencedInstance
                _  -> return $ Left $ HeartbeatOtherError errorCode $
                  "Heartbeat error code: " ++ show errorCode

