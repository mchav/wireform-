{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Internal.ConsumerGroup
Description : Consumer group coordination protocol implementation
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the consumer group coordination protocol for Kafka.

The consumer group protocol involves:
1. FindCoordinator - Discover which broker manages this group
2. JoinGroup - Join the group and trigger rebalancing
3. SyncGroup - Receive partition assignments after joining
4. Heartbeat - Maintain group membership (separate background thread)

Group Lifecycle:
- Consumer finds the group coordinator
- Consumer joins the group (becomes member)
- Leader receives member list and performs assignment
- Leader sends assignments via SyncGroup
- Members receive their assignments
- Members send periodic heartbeats
- On failure or new members, rebalance is triggered
-}
module Kafka.Client.Internal.ConsumerGroup
  ( -- * Group Coordinator
    GroupCoordinator(..)
  , findGroupCoordinator
    -- * Group Membership
  , JoinGroupResult(..)
  , joinGroup
  , syncGroup
  , leaveGroup
    -- * Group State
  , MemberAssignment(..)
  , GroupMemberInfo(..)
  ) where

import Control.Concurrent.STM
import Control.Monad (forM)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.Connection (Connection)

import qualified Kafka.Client.Internal.Request as Req
import Kafka.Network.Connection (BrokerAddress(..))
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.Generated.FindCoordinatorRequest as FCReq
import qualified Kafka.Protocol.Generated.FindCoordinatorResponse as FCResp
import qualified Kafka.Protocol.Generated.JoinGroupRequest as JGReq
import qualified Kafka.Protocol.Generated.JoinGroupResponse as JGResp
import qualified Kafka.Protocol.Generated.SyncGroupRequest as SGReq
import qualified Kafka.Protocol.Generated.SyncGroupResponse as SGResp
import qualified Kafka.Protocol.Generated.LeaveGroupRequest as LGReq
import qualified Kafka.Protocol.Generated.LeaveGroupResponse as LGResp
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Wire.Codec as WC

-- | Information about the group coordinator broker
data GroupCoordinator = GroupCoordinator
  { coordNodeId :: !Int32
    -- ^ Coordinator broker node ID
  , coordHost :: !Text
    -- ^ Coordinator broker host
  , coordPort :: !Int32
    -- ^ Coordinator broker port
  } deriving (Eq, Show)

-- | Result of joining a consumer group
data JoinGroupResult = JoinGroupResult
  { jgrGenerationId :: !Int32
    -- ^ Group generation ID
  , jgrMemberId :: !Text
    -- ^ Assigned member ID
  , jgrLeaderId :: !Text
    -- ^ Leader member ID (equals memberId if this is the leader)
  , jgrMembers :: ![GroupMemberInfo]
    -- ^ List of group members (only populated for leader)
  , jgrProtocolName :: !Text
    -- ^ Selected group protocol
  } deriving (Eq, Show)

-- | Information about a group member
data GroupMemberInfo = GroupMemberInfo
  { gmiMemberId :: !Text
    -- ^ Member ID
  , gmiMetadata :: !ByteString
    -- ^ Member subscription metadata
  } deriving (Eq, Show)

-- | Partition assignment for a member
data MemberAssignment = MemberAssignment
  { maTopicPartitions :: ![(Text, [Int32])]
    -- ^ Assigned topic-partitions (topic, [partition IDs])
  , maUserData :: !ByteString
    -- ^ Optional user data
  } deriving (Eq, Show)

-- | Find the group coordinator for a given consumer group
--
-- The coordinator is the broker responsible for managing group membership
-- and partition assignments for this group.
findGroupCoordinator
  :: AV.ApiVersionCache  -- ^ Version cache for version negotiation
  -> BrokerAddress       -- ^ Broker address for version lookup
  -> Connection
  -> Text                -- ^ Group ID
  -> Int32               -- ^ Correlation ID
  -> Text                -- ^ Client ID
  -> IO (Either String GroupCoordinator)
findGroupCoordinator versionCache brokerAddr conn groupId correlationId clientId = do
  let apiKey = 10  -- FindCoordinator API key
      clientMaxVersion = 4  -- Max version we support
  
  -- Query broker's supported version
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache brokerAddr apiKey
  
  let apiVersion = case brokerVersionM of
        Nothing -> 0  -- Fall back to v0 if unknown
        Just range -> case AV.selectVersion clientMaxVersion range of
          Nothing -> 0  -- Fall back if incompatible
          Just v -> v
      
      request = FCReq.FindCoordinatorRequest
        { FCReq.findCoordinatorRequestKey = P.mkKafkaString groupId
        , FCReq.findCoordinatorRequestKeyType = 0  -- 0 = consumer group
        , FCReq.findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty
        }
      
      requestBody = WC.runEncodeVer FCReq.encodeFindCoordinatorRequest apiVersion request
      clientIdKafka = P.mkKafkaString clientId
  
  -- Send request and receive response
  result <- Req.sendRequestReceiveResponse conn apiKey apiVersion correlationId clientIdKafka requestBody
  
  case result of
    Left err -> return $ Left $ "FindCoordinator request failed: " ++ err
    Right (_, responseBody) -> do
      case WC.runDecodeVer FCResp.decodeFindCoordinatorResponse apiVersion responseBody of
        Left err -> return $ Left $ "Failed to parse FindCoordinatorResponse: " ++ err
        Right response -> do
          let errorCode = FCResp.findCoordinatorResponseErrorCode response
          
          if errorCode /= 0
            then return $ Left $ "FindCoordinator error: code " ++ show errorCode
            else do
              let nodeId = FCResp.findCoordinatorResponseNodeId response
                  host = extractText $ FCResp.findCoordinatorResponseHost response
                  port = FCResp.findCoordinatorResponsePort response
              
              return $ Right GroupCoordinator
                { coordNodeId = nodeId
                , coordHost = host
                , coordPort = port
                }

-- | Join a consumer group
--
-- This initiates or participates in a group rebalance. If this is the first
-- member or if rebalance is needed, one member will be elected as leader.
joinGroup
  :: AV.ApiVersionCache  -- ^ Version cache for version negotiation
  -> BrokerAddress       -- ^ Broker address for version lookup
  -> Connection
  -> Text                -- ^ Group ID
  -> Text                -- ^ Member ID (empty for first join)
  -> Text                -- ^ Client ID  
  -> Int32               -- ^ Session timeout (ms)
  -> Int32               -- ^ Rebalance timeout (ms)
  -> Text                -- ^ Protocol type (e.g., "consumer")
  -> [(Text, ByteString)]  -- ^ Supported protocols with metadata
  -> Int32               -- ^ Correlation ID
  -> IO (Either String JoinGroupResult)
joinGroup versionCache brokerAddr conn groupId memberId clientId sessionTimeout rebalanceTimeout protocolType protocols correlationId = do
  let apiKey = 11  -- JoinGroup API key
      clientMaxVersion = 9  -- Max version we support
  
  -- Query broker's supported version
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache brokerAddr apiKey
  
  let apiVersion = case brokerVersionM of
        Nothing -> 0  -- Fall back to v0 if unknown
        Just range -> case AV.selectVersion clientMaxVersion range of
          Nothing -> 0  -- Fall back if incompatible
          Just v -> v
      
      protocolVec = V.fromList $ map (\(name, metadata) ->
        JGReq.JoinGroupRequestProtocol
          { JGReq.joinGroupRequestProtocolName = P.mkKafkaString name
          , JGReq.joinGroupRequestProtocolMetadata = P.mkKafkaBytes metadata
          }) protocols
      
      request = JGReq.JoinGroupRequest
        { JGReq.joinGroupRequestGroupId = P.mkKafkaString groupId
        , JGReq.joinGroupRequestSessionTimeoutMs = sessionTimeout
        , JGReq.joinGroupRequestRebalanceTimeoutMs = rebalanceTimeout
        , JGReq.joinGroupRequestMemberId = P.mkKafkaString memberId
        , JGReq.joinGroupRequestGroupInstanceId = P.KafkaString P.Null
        , JGReq.joinGroupRequestProtocolType = P.mkKafkaString protocolType
        , JGReq.joinGroupRequestProtocols = P.mkKafkaArray protocolVec
        , JGReq.joinGroupRequestReason = P.KafkaString P.Null
        }
      
      requestBody = WC.runEncodeVer JGReq.encodeJoinGroupRequest apiVersion request
      clientIdKafka = P.mkKafkaString clientId
  
  result <- Req.sendRequestReceiveResponse conn apiKey apiVersion correlationId clientIdKafka requestBody
  
  case result of
    Left err -> return $ Left $ "JoinGroup request failed: " ++ err
    Right (_, responseBody) -> do
      case WC.runDecodeVer JGResp.decodeJoinGroupResponse apiVersion responseBody of
        Left err -> return $ Left $ "Failed to parse JoinGroupResponse: " ++ err
        Right response -> do
          let errorCode = JGResp.joinGroupResponseErrorCode response
          
          if errorCode /= 0
            then return $ Left $ "JoinGroup error: code " ++ show errorCode
            else do
              let genId = JGResp.joinGroupResponseGenerationId response
                  protocol = extractText $ JGResp.joinGroupResponseProtocolName response
                  leader = extractText $ JGResp.joinGroupResponseLeader response
                  member = extractText $ JGResp.joinGroupResponseMemberId response
                  
                  members = case P.unKafkaArray (JGResp.joinGroupResponseMembers response) of
                    P.Null -> []
                    P.NotNull vec -> V.toList $ V.map convertMember vec
                  
                  convertMember m = GroupMemberInfo
                    { gmiMemberId = extractText $ JGResp.joinGroupResponseMemberMemberId m
                    , gmiMetadata = extractBytes $ JGResp.joinGroupResponseMemberMetadata m
                    }
              
              return $ Right JoinGroupResult
                { jgrGenerationId = genId
                , jgrMemberId = member
                , jgrLeaderId = leader
                , jgrMembers = members
                , jgrProtocolName = protocol
                }

-- | Sync group assignments after joining
--
-- The leader sends assignments for all members.
-- Followers send empty assignments and receive their own assignment.
syncGroup
  :: AV.ApiVersionCache  -- ^ Version cache for version negotiation
  -> BrokerAddress       -- ^ Broker address for version lookup
  -> Connection
  -> Text                -- ^ Group ID
  -> Int32               -- ^ Generation ID
  -> Text                -- ^ Member ID
  -> Text                -- ^ Client ID
  -> [(Text, ByteString)]  -- ^ Assignments (memberId -> assignment bytes)
  -> Int32               -- ^ Correlation ID
  -> IO (Either String ByteString)
syncGroup versionCache brokerAddr conn groupId generationId memberId clientId assignments correlationId = do
  let apiKey = 14  -- SyncGroup API key
      clientMaxVersion = 5  -- Max version we support
  
  -- Query broker's supported version
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache brokerAddr apiKey
  
  let apiVersion = case brokerVersionM of
        Nothing -> 0  -- Fall back to v0 if unknown
        Just range -> case AV.selectVersion clientMaxVersion range of
          Nothing -> 0  -- Fall back if incompatible
          Just v -> v
      
      assignmentVec = V.fromList $ map (\(mid, asgn) ->
        SGReq.SyncGroupRequestAssignment
          { SGReq.syncGroupRequestAssignmentMemberId = P.mkKafkaString mid
          , SGReq.syncGroupRequestAssignmentAssignment = P.mkKafkaBytes asgn
          }) assignments
      
      request = SGReq.SyncGroupRequest
        { SGReq.syncGroupRequestGroupId = P.mkKafkaString groupId
        , SGReq.syncGroupRequestGenerationId = generationId
        , SGReq.syncGroupRequestMemberId = P.mkKafkaString memberId
        , SGReq.syncGroupRequestGroupInstanceId = P.KafkaString P.Null
        , SGReq.syncGroupRequestProtocolType = P.KafkaString P.Null
        , SGReq.syncGroupRequestProtocolName = P.KafkaString P.Null
        , SGReq.syncGroupRequestAssignments = P.mkKafkaArray assignmentVec
        }
      
      requestBody = WC.runEncodeVer SGReq.encodeSyncGroupRequest apiVersion request
      clientIdKafka = P.mkKafkaString clientId
  
  result <- Req.sendRequestReceiveResponse conn apiKey apiVersion correlationId clientIdKafka requestBody
  
  case result of
    Left err -> return $ Left $ "SyncGroup request failed: " ++ err
    Right (_, responseBody) -> do
      case WC.runDecodeVer SGResp.decodeSyncGroupResponse apiVersion responseBody of
        Left err -> return $ Left $ "Failed to parse SyncGroupResponse: " ++ err
        Right response -> do
          let errorCode = SGResp.syncGroupResponseErrorCode response
          
          if errorCode /= 0
            then return $ Left $ "SyncGroup error: code " ++ show errorCode
            else do
              let assignment = extractBytes $ SGResp.syncGroupResponseAssignment response
              return $ Right assignment

-- | Leave a consumer group
leaveGroup
  :: AV.ApiVersionCache  -- ^ Version cache for version negotiation
  -> BrokerAddress       -- ^ Broker address for version lookup
  -> Connection
  -> Text                -- ^ Group ID
  -> Text                -- ^ Member ID
  -> Text                -- ^ Client ID
  -> Int32               -- ^ Correlation ID
  -> IO (Either String ())
leaveGroup versionCache brokerAddr conn groupId memberId clientId correlationId = do
  let apiKey = 13  -- LeaveGroup API key
      clientMaxVersion = 5  -- Max version we support
  
  -- Query broker's supported version
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache brokerAddr apiKey
  
  let apiVersion = case brokerVersionM of
        Nothing -> 0  -- Fall back to v0 if unknown
        Just range -> case AV.selectVersion clientMaxVersion range of
          Nothing -> 0  -- Fall back if incompatible
          Just v -> v
      
      -- Build member info for leaving
      memberVec = V.singleton $ LGReq.MemberIdentity
        { LGReq.memberIdentityMemberId = P.mkKafkaString memberId
        , LGReq.memberIdentityGroupInstanceId = P.KafkaString P.Null
        , LGReq.memberIdentityReason = P.KafkaString P.Null
        }
      
      request = LGReq.LeaveGroupRequest
        { LGReq.leaveGroupRequestGroupId = P.mkKafkaString groupId
        , LGReq.leaveGroupRequestMemberId = P.KafkaString P.Null  -- Deprecated in v3+
        , LGReq.leaveGroupRequestMembers = P.mkKafkaArray memberVec
        }
      
      requestBody = WC.runEncodeVer LGReq.encodeLeaveGroupRequest apiVersion request
      clientIdKafka = P.mkKafkaString clientId
  
  result <- Req.sendRequestReceiveResponse conn apiKey apiVersion correlationId clientIdKafka requestBody
  
  case result of
    Left err -> return $ Left $ "LeaveGroup request failed: " ++ err
    Right (_, responseBody) -> do
      case WC.runDecodeVer LGResp.decodeLeaveGroupResponse apiVersion responseBody of
        Left err -> return $ Left $ "Failed to parse LeaveGroupResponse: " ++ err
        Right response -> do
          let errorCode = LGResp.leaveGroupResponseErrorCode response
          
          if errorCode /= 0
            then return $ Left $ "LeaveGroup error: code " ++ show errorCode
            else return $ Right ()

-- | Extract text from a KafkaString
extractText :: P.KafkaString -> Text
extractText (P.KafkaString P.Null) = ""
extractText (P.KafkaString (P.NotNull t)) = t

-- | Extract bytes from KafkaBytes
extractBytes :: P.KafkaBytes -> ByteString
extractBytes (P.KafkaBytes P.Null) = BS.empty
extractBytes (P.KafkaBytes (P.NotNull bs)) = bs

