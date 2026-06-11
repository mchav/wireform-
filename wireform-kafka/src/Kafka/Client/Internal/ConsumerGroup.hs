{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
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
module Kafka.Client.Internal.ConsumerGroup (
  -- * Group Coordinator
  GroupCoordinator (..),
  findGroupCoordinator,

  -- * Group Membership
  JoinGroupResult (..),
  joinGroup,
  syncGroup,
  leaveGroup,

  -- * Group State
  MemberAssignment (..),
  GroupMemberInfo (..),
) where

import Control.Concurrent.STM
import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Kafka.Client.Internal.Request qualified as Req
import Kafka.Network.Connection (BrokerAddress (..), Connection)
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import "wireform-kafka-protocol" Kafka.Protocol.Generated.FindCoordinatorRequest qualified as FCReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.FindCoordinatorResponse qualified as FCResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.JoinGroupRequest qualified as JGReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.JoinGroupResponse qualified as JGResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.LeaveGroupRequest qualified as LGReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.LeaveGroupResponse qualified as LGResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.SyncGroupRequest qualified as SGReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.SyncGroupResponse qualified as SGResp
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


-- | Information about the group coordinator broker
data GroupCoordinator = GroupCoordinator
  { coordNodeId :: !Int32
  -- ^ Coordinator broker node ID
  , coordHost :: !Text
  -- ^ Coordinator broker host
  , coordPort :: !Int32
  -- ^ Coordinator broker port
  }
  deriving (Eq, Show)


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
  }
  deriving (Eq, Show)


-- | Information about a group member
data GroupMemberInfo = GroupMemberInfo
  { gmiMemberId :: !Text
  -- ^ Member ID
  , gmiMetadata :: !ByteString
  -- ^ Member subscription metadata
  }
  deriving (Eq, Show)


-- | Partition assignment for a member
data MemberAssignment = MemberAssignment
  { maTopicPartitions :: ![(Text, [Int32])]
  -- ^ Assigned topic-partitions (topic, [partition IDs])
  , maUserData :: !ByteString
  -- ^ Optional user data
  }
  deriving (Eq, Show)


{- | Find the group coordinator for a given consumer group

The coordinator is the broker responsible for managing group membership
and partition assignments for this group.
-}
findGroupCoordinator
  :: AV.ApiVersionCache
  -- ^ Version cache for version negotiation
  -> Conn.ConnectionManager
  -- ^ Connection manager (for per-broker lock)
  -> BrokerAddress
  -- ^ Broker address for version lookup
  -> Connection
  -> Text
  -- ^ Group ID
  -> Int32
  -- ^ Correlation ID
  -> Text
  -- ^ Client ID
  -> IO (Either String GroupCoordinator)
findGroupCoordinator versionCache connMgr brokerAddr conn groupId correlationId clientId = do
  let apiKey = 10 -- FindCoordinator API key
  -- v6 = trunk; we now handle both legacy v0-v3 (top-level
  -- Host/Port) and v4+ (KIP-699 Coordinators[]) shapes
  -- below, so it's safe to negotiate up.
      clientMaxVersion = 6

  -- Query broker's supported version
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache brokerAddr apiKey

  let apiVersion = case brokerVersionM of
        Nothing -> 0
        Just range -> case AV.selectVersion clientMaxVersion range of
          Nothing -> 0
          Just v -> v

      -- v0-v3 use the singular 'Key' field; v4+ use the
      -- 'CoordinatorKeys' batched array. Build both so the
      -- codegen sees the right one for the chosen version
      -- (the wirePoke for absent-version fields is a no-op).
      request
        | apiVersion >= 4 =
            FCReq.FindCoordinatorRequest
              { FCReq.findCoordinatorRequestKey = P.KafkaString P.Null
              , FCReq.findCoordinatorRequestKeyType = 0 -- 0 = consumer group
              , FCReq.findCoordinatorRequestCoordinatorKeys =
                  P.mkKafkaArray (V.singleton (P.mkKafkaString groupId))
              }
        | otherwise =
            FCReq.FindCoordinatorRequest
              { FCReq.findCoordinatorRequestKey = P.mkKafkaString groupId
              , FCReq.findCoordinatorRequestKeyType = 0
              , FCReq.findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty
              }

      requestBody = WC.runEncodeVer @FCReq.FindCoordinatorRequest apiVersion request
      clientIdKafka = P.mkKafkaString clientId

  -- Send request and receive response
  result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock connMgr brokerAddr) conn apiKey apiVersion correlationId clientIdKafka requestBody

  case result of
    Left err -> return $ Left $ "FindCoordinator request failed: " ++ err
    Right (_, responseBody) -> do
      case WC.runDecodeVer @FCResp.FindCoordinatorResponse apiVersion responseBody of
        Left err -> return $ Left $ "Failed to parse FindCoordinatorResponse: " ++ err
        Right response -> do
          -- Pick the coordinator out of whichever response shape
          -- the broker negotiated. v0-v3 carries the result on
          -- the top-level fields; v4+ moves it into the first
          -- (and for our single-key request, only) entry of the
          -- 'coordinators' array. KIP-699 also sinks the per-
          -- result error code into the array entry, so we read
          -- it from there too on v4+.
          let (errorCode, nodeId, host, port)
                | apiVersion >= 4 =
                    case P.unKafkaArray (FCResp.findCoordinatorResponseCoordinators response) of
                      P.NotNull v
                        | not (V.null v) ->
                            let !c = V.head v
                            in ( FCResp.coordinatorErrorCode c
                               , FCResp.coordinatorNodeId c
                               , extractText (FCResp.coordinatorHost c)
                               , FCResp.coordinatorPort c
                               )
                      _ ->
                        -- Empty Coordinators[] in a 0-error v4+
                        -- response would be a broker bug; surface
                        -- as an explicit \"no coordinator\" error
                        -- below.
                        (15, 0, T.empty, 0) -- 15 = COORDINATOR_NOT_AVAILABLE
                | otherwise =
                    ( FCResp.findCoordinatorResponseErrorCode response
                    , FCResp.findCoordinatorResponseNodeId response
                    , extractText (FCResp.findCoordinatorResponseHost response)
                    , FCResp.findCoordinatorResponsePort response
                    )

          if errorCode /= 0
            then return $ Left $ "FindCoordinator error: code " ++ show errorCode
            else do
              return $
                Right
                  GroupCoordinator
                    { coordNodeId = nodeId
                    , coordHost = host
                    , coordPort = port
                    }


{- | Join a consumer group

This initiates or participates in a group rebalance. If this is the first
member or if rebalance is needed, one member will be elected as leader.
-}
joinGroup
  :: AV.ApiVersionCache
  -- ^ Version cache for version negotiation
  -> Conn.ConnectionManager
  -- ^ Connection manager (for per-broker lock)
  -> BrokerAddress
  -- ^ Broker address for version lookup
  -> Connection
  -> Text
  -- ^ Group ID
  -> Text
  -- ^ Member ID (empty for first join)
  -> Text
  -- ^ Client ID
  -> Int32
  -- ^ Session timeout (ms)
  -> Int32
  -- ^ Rebalance timeout (ms)
  -> Text
  -- ^ Protocol type (e.g., "consumer")
  -> [(Text, ByteString)]
  -- ^ Supported protocols with metadata
  -> Int32
  -- ^ Correlation ID
  -> IO (Either String JoinGroupResult)
joinGroup vc cm ba conn gid mid cid st rt pt protos corrId =
  joinGroupGo vc cm ba conn gid mid cid st rt pt protos corrId 0


{- | Internal joinGroup loop with a retry counter.  Handles
'MEMBER_ID_REQUIRED' (error 79, KIP-394): the broker, on the
first JoinGroup from a dynamic member, returns a freshly-
minted member id and requires the client to re-issue the
JoinGroup with that id.  We retry once.
-}
joinGroupGo
  :: AV.ApiVersionCache
  -> Conn.ConnectionManager
  -> BrokerAddress
  -> Connection
  -> Text
  -> Text
  -> Text
  -> Int32
  -> Int32
  -> Text
  -> [(Text, ByteString)]
  -> Int32
  -> Int
  -- ^ retry attempt (0 = first call)
  -> IO (Either String JoinGroupResult)
joinGroupGo versionCache connMgr brokerAddr conn groupId memberId clientId sessionTimeout rebalanceTimeout protocolType protocols correlationId attempt = do
  let apiKey = 11 -- JoinGroup API key
  -- Bump back to v9 (= schema trunk). Versions added since
  -- v5: v6 made the protocol flexible (the codegen handles
  -- the per-version compact-vs-plain dispatch); v7 added
  -- 'ProtocolType' on the response (KIP-559) which we
  -- ignore; v8 added 'Reason' on the request (we send
  -- Null); v9 added 'SkipAssignment' on the response
  -- (KIP-848 transition) which we treat as a hint and let
  -- the assignor run anyway when False.
      clientMaxVersion = 9

  -- Query broker's supported version
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache brokerAddr apiKey

  let apiVersion = case brokerVersionM of
        Nothing -> 0 -- Fall back to v0 if unknown
        Just range -> case AV.selectVersion clientMaxVersion range of
          Nothing -> 0 -- Fall back if incompatible
          Just v -> v

      protocolVec =
        V.fromList $
          map
            ( \(name, metadata) ->
                JGReq.JoinGroupRequestProtocol
                  { JGReq.joinGroupRequestProtocolName = P.mkKafkaString name
                  , JGReq.joinGroupRequestProtocolMetadata = P.mkKafkaBytes metadata
                  }
            )
            protocols

      -- KIP-394: when this is the retry after MEMBER_ID_REQUIRED,
      -- the JVM client sets a 'Reason' string of
      -- @"need to re-join with the given member-id"@ so the
      -- coordinator's audit log records why the group rebalanced.
      -- Mirror that here; it doesn't change the broker's
      -- behaviour but makes our requests look like the JVM
      -- client's in broker-side traces.
      reasonStr
        | attempt > 0 =
            P.mkKafkaString
              "need to re-join with the given member-id"
        | otherwise = P.KafkaString P.Null

      request =
        JGReq.JoinGroupRequest
          { JGReq.joinGroupRequestGroupId = P.mkKafkaString groupId
          , JGReq.joinGroupRequestSessionTimeoutMs = sessionTimeout
          , JGReq.joinGroupRequestRebalanceTimeoutMs = rebalanceTimeout
          , JGReq.joinGroupRequestMemberId = P.mkKafkaString memberId
          , JGReq.joinGroupRequestGroupInstanceId = P.KafkaString P.Null
          , JGReq.joinGroupRequestProtocolType = P.mkKafkaString protocolType
          , JGReq.joinGroupRequestProtocols = P.mkKafkaArray protocolVec
          , JGReq.joinGroupRequestReason = reasonStr
          }

      requestBody = WC.runEncodeVer @JGReq.JoinGroupRequest apiVersion request
      clientIdKafka = P.mkKafkaString clientId

  result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock connMgr brokerAddr) conn apiKey apiVersion correlationId clientIdKafka requestBody

  case result of
    Left err -> return $ Left $ "JoinGroup request failed: " ++ err
    Right (_, responseBody) -> do
      case WC.runDecodeVer @JGResp.JoinGroupResponse apiVersion responseBody of
        Left err -> return $ Left $ "Failed to parse JoinGroupResponse: " ++ err
        Right response -> do
          let errorCode = JGResp.joinGroupResponseErrorCode response

          let assignedMember =
                extractText (JGResp.joinGroupResponseMemberId response)
          case errorCode of
            -- KIP-394 MEMBER_ID_REQUIRED: broker handed us a fresh
            -- member id on the first JoinGroup; retry once with it.
            -- Any other non-zero code is fatal at this layer.
            79
              | attempt == 0 && not (T.null assignedMember) ->
                  joinGroupGo
                    versionCache
                    connMgr
                    brokerAddr
                    conn
                    groupId
                    assignedMember
                    clientId
                    sessionTimeout
                    rebalanceTimeout
                    protocolType
                    protocols
                    (correlationId + 1)
                    (attempt + 1)
            c
              | c /= 0 ->
                  pure (Left ("JoinGroup error: code " ++ show c))
            _ -> do
              let genId = JGResp.joinGroupResponseGenerationId response
                  protocol = extractText $ JGResp.joinGroupResponseProtocolName response
                  leader = extractText $ JGResp.joinGroupResponseLeader response
                  member = extractText $ JGResp.joinGroupResponseMemberId response

                  members = case P.unKafkaArray (JGResp.joinGroupResponseMembers response) of
                    P.Null -> []
                    P.NotNull vec -> V.toList $ V.map convertMember vec

                  convertMember m =
                    GroupMemberInfo
                      { gmiMemberId = extractText $ JGResp.joinGroupResponseMemberMemberId m
                      , gmiMetadata = extractBytes $ JGResp.joinGroupResponseMemberMetadata m
                      }

              return $
                Right
                  JoinGroupResult
                    { jgrGenerationId = genId
                    , jgrMemberId = member
                    , jgrLeaderId = leader
                    , jgrMembers = members
                    , jgrProtocolName = protocol
                    }


{- | Sync group assignments after joining

The leader sends assignments for all members.
Followers send empty assignments and receive their own assignment.
-}
syncGroup
  :: AV.ApiVersionCache
  -- ^ Version cache for version negotiation
  -> Conn.ConnectionManager
  -- ^ Connection manager (for per-broker lock)
  -> BrokerAddress
  -- ^ Broker address for version lookup
  -> Connection
  -> Text
  -- ^ Group ID
  -> Int32
  -- ^ Generation ID
  -> Text
  -- ^ Member ID
  -> Text
  -- ^ Client ID
  -> Text
  -- ^ Protocol type ("consumer")
  -> Text
  {- ^ Protocol name (the assignor the broker picked
  in the JoinGroup response — required at v5+
  per KIP-559; broker rejects with
  'INCONSISTENT_GROUP_PROTOCOL' (23) if
  we send Null here on a v5 request).
  -}
  -> [(Text, ByteString)]
  -- ^ Assignments (memberId -> assignment bytes)
  -> Int32
  -- ^ Correlation ID
  -> IO (Either String ByteString)
syncGroup versionCache connMgr brokerAddr conn groupId generationId memberId clientId protocolType protocolName assignments correlationId = do
  let apiKey = 14 -- SyncGroup API key
      clientMaxVersion = 5 -- Max version we support
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache brokerAddr apiKey

  let apiVersion = case brokerVersionM of
        Nothing -> 0
        Just range -> case AV.selectVersion clientMaxVersion range of
          Nothing -> 0
          Just v -> v

      assignmentVec =
        V.fromList $
          map
            ( \(mid, asgn) ->
                SGReq.SyncGroupRequestAssignment
                  { SGReq.syncGroupRequestAssignmentMemberId = P.mkKafkaString mid
                  , SGReq.syncGroupRequestAssignmentAssignment = P.mkKafkaBytes asgn
                  }
            )
            assignments

      request =
        SGReq.SyncGroupRequest
          { SGReq.syncGroupRequestGroupId = P.mkKafkaString groupId
          , SGReq.syncGroupRequestGenerationId = generationId
          , SGReq.syncGroupRequestMemberId = P.mkKafkaString memberId
          , SGReq.syncGroupRequestGroupInstanceId = P.KafkaString P.Null
          , SGReq.syncGroupRequestProtocolType = P.mkKafkaString protocolType
          , SGReq.syncGroupRequestProtocolName = P.mkKafkaString protocolName
          , SGReq.syncGroupRequestAssignments = P.mkKafkaArray assignmentVec
          }

      requestBody = WC.runEncodeVer @SGReq.SyncGroupRequest apiVersion request
      clientIdKafka = P.mkKafkaString clientId

  result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock connMgr brokerAddr) conn apiKey apiVersion correlationId clientIdKafka requestBody

  case result of
    Left err -> return $ Left $ "SyncGroup request failed: " ++ err
    Right (_, responseBody) -> do
      case WC.runDecodeVer @SGResp.SyncGroupResponse apiVersion responseBody of
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
  :: AV.ApiVersionCache
  -- ^ Version cache for version negotiation
  -> Conn.ConnectionManager
  -- ^ Connection manager (for per-broker lock)
  -> BrokerAddress
  -- ^ Broker address for version lookup
  -> Connection
  -> Text
  -- ^ Group ID
  -> Text
  -- ^ Member ID
  -> Text
  -- ^ Client ID
  -> Int32
  -- ^ Correlation ID
  -> IO (Either String ())
leaveGroup versionCache connMgr brokerAddr conn groupId memberId clientId correlationId = do
  let apiKey = 13 -- LeaveGroup API key
      clientMaxVersion = 5 -- Max version we support

  -- Query broker's supported version
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache brokerAddr apiKey

  let apiVersion = case brokerVersionM of
        Nothing -> 0 -- Fall back to v0 if unknown
        Just range -> case AV.selectVersion clientMaxVersion range of
          Nothing -> 0 -- Fall back if incompatible
          Just v -> v

      -- Build member info for leaving
      memberVec =
        V.singleton $
          LGReq.MemberIdentity
            { LGReq.memberIdentityMemberId = P.mkKafkaString memberId
            , LGReq.memberIdentityGroupInstanceId = P.KafkaString P.Null
            , LGReq.memberIdentityReason = P.KafkaString P.Null
            }

      request =
        LGReq.LeaveGroupRequest
          { LGReq.leaveGroupRequestGroupId = P.mkKafkaString groupId
          , LGReq.leaveGroupRequestMemberId = P.KafkaString P.Null -- Deprecated in v3+
          , LGReq.leaveGroupRequestMembers = P.mkKafkaArray memberVec
          }

      requestBody = WC.runEncodeVer @LGReq.LeaveGroupRequest apiVersion request
      clientIdKafka = P.mkKafkaString clientId

  result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock connMgr brokerAddr) conn apiKey apiVersion correlationId clientIdKafka requestBody

  case result of
    Left err -> return $ Left $ "LeaveGroup request failed: " ++ err
    Right (_, responseBody) -> do
      case WC.runDecodeVer @LGResp.LeaveGroupResponse apiVersion responseBody of
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
