{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Kafka.Client.AdminClient.Extras
Description : Additional admin operations beyond the headline set wrapped
              in @Kafka.Client.AdminClient@

These are the JVM-Admin operations the v2 SDK_PARITY audit
called out as missing at the typed-Haskell surface. The
protocol-level pairs (e.g. @Kafka.Protocol.Generated.CreateAclsRequest@)
already existed; this module wraps them into operation-shaped
functions using the value types in @Kafka.Common.Acl@,
@Kafka.Common.Resource@, and @Kafka.Common.Quota@.

Coverage:

  * 'createPartitions' / 'NewPartitions'                 — KIP-195
  * 'describeCluster'                                    — KIP-700
  * 'listGroups'                                         — KIP-848 (generic groups)
  * 'createAcls' / 'describeAcls' / 'deleteAcls'         — KIP-50

Every operation returns @IO (Either String result)@; the @Right@
payload is a per-resource result (or the whole cluster snapshot
for 'describeCluster') so callers can tell a partial failure from
a transport error.
-}
module Kafka.Client.AdminClient.Extras
  ( -- * createPartitions (KIP-195)
    NewPartitions (..)
  , createPartitions
    -- * describeCluster (KIP-700)
  , describeCluster
    -- * listGroups (KIP-848)
  , GroupListing (..)
  , listGroups
    -- * ACL admin (KIP-50)
  , createAcls
  , describeAcls
  , deleteAcls
  , AclCreationResult (..)
  , AclDeletionResult (..)
    -- * Partition reassignment admin (KIP-455)
  , PartitionReassignmentSpec (..)
  , OngoingPartitionReassignment (..)
  , alterPartitionReassignments
  , listPartitionReassignments
    -- * Broker lifecycle (KIP-704)
  , unregisterBroker
    -- * Client-quota admin (KIP-546)
  , ClientQuotaEntry (..)
  , describeClientQuotas
  , alterClientQuotas
    -- * Transaction admin (KIP-664)
  , TransactionListing (..)
  , TransactionDescription (..)
  , TransactionTopicPartitions (..)
  , listTransactions
  , describeTransactions
    -- * SCRAM credential admin (KIP-554)
  , ScramMechanism (..)
  , ScramCredentialInfo (..)
  , ScramCredentialUpsertion (..)
  , ScramCredentialDeletion (..)
  , describeUserScramCredentials
  , alterUserScramCredentials
    -- * Producer-state admin (KIP-664)
  , ProducerState (..)
  , describeProducers
    -- * Log directory admin (KIP-113 / KIP-405)
  , LogDirDescription (..)
  , TopicLogDirDescription (..)
  , PartitionLogDirDescription (..)
  , ReplicaLogDirAssignment (..)
  , describeLogDirs
  , alterReplicaLogDirs
    -- * Delegation tokens (KIP-48)
  , DelegationToken (..)
  , createDelegationToken
  , renewDelegationToken
  , expireDelegationToken
  , describeDelegationToken
    -- * KRaft voter management (KIP-853)
  , RaftVoterEndpoint (..)
  , addRaftVoter
  , removeRaftVoter
    -- * KRaft quorum description
  , QuorumInfo (..)
  , PartitionQuorumInfo (..)
  , ReplicaState (..)
  , describeMetadataQuorum
    -- * Consumer group member removal (KIP-345)
  , MemberToRemove (..)
  , removeMembersFromConsumerGroup
  ) where

import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Kafka.Client.AdminClient as Adm
import Kafka.Client.AdminClient
  ( AdminClient
  , extractText
  , withNegotiatedVersion
  )
import qualified Kafka.Client.Metadata as Meta
import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Network.Connection as Conn

import qualified Kafka.Common as Common
import Kafka.Common (Node (..), Cluster (..))
import qualified Kafka.Common.Acl as Acl
import qualified Kafka.Common.Resource as Resource

import qualified Kafka.Protocol.Generated.CreatePartitionsRequest as CPReq
import qualified Kafka.Protocol.Generated.CreatePartitionsResponse as CPResp
import qualified Kafka.Protocol.Generated.DescribeClusterRequest as DSReq
import qualified Kafka.Protocol.Generated.DescribeClusterResponse as DSResp
import qualified Kafka.Protocol.Generated.ListGroupsRequest as LGReq
import qualified Kafka.Protocol.Generated.ListGroupsResponse as LGResp
import qualified Kafka.Protocol.Generated.CreateAclsRequest as CAReq
import qualified Kafka.Protocol.Generated.CreateAclsResponse as CAResp
import qualified Kafka.Protocol.Generated.DescribeAclsRequest as DAReq
import qualified Kafka.Protocol.Generated.DescribeAclsResponse as DAResp
import qualified Kafka.Protocol.Generated.DeleteAclsRequest as DelAReq
import qualified Kafka.Protocol.Generated.DeleteAclsResponse as DelAResp
import qualified Kafka.Protocol.Generated.AlterPartitionReassignmentsRequest as APRReq
import qualified Kafka.Protocol.Generated.AlterPartitionReassignmentsResponse as APRResp
import qualified Kafka.Protocol.Generated.ListPartitionReassignmentsRequest as LPRReq
import qualified Kafka.Protocol.Generated.ListPartitionReassignmentsResponse as LPRResp
import qualified Kafka.Protocol.Generated.UnregisterBrokerRequest as UBReq
import qualified Kafka.Protocol.Generated.UnregisterBrokerResponse as UBResp
import qualified Kafka.Protocol.Generated.DescribeClientQuotasRequest as DCQReq
import qualified Kafka.Protocol.Generated.DescribeClientQuotasResponse as DCQResp
import qualified Kafka.Protocol.Generated.AlterClientQuotasRequest as ACQReq
import qualified Kafka.Protocol.Generated.AlterClientQuotasResponse as ACQResp
import qualified Kafka.Protocol.Generated.ListTransactionsRequest as LTReq
import qualified Kafka.Protocol.Generated.ListTransactionsResponse as LTResp
import qualified Kafka.Protocol.Generated.DescribeTransactionsRequest as DTReq
import qualified Kafka.Protocol.Generated.DescribeTransactionsResponse as DTResp
import qualified Kafka.Protocol.Generated.DescribeUserScramCredentialsRequest as DSCReq
import qualified Kafka.Protocol.Generated.DescribeUserScramCredentialsResponse as DSCResp
import qualified Kafka.Protocol.Generated.AlterUserScramCredentialsRequest as ASCReq
import qualified Kafka.Protocol.Generated.AlterUserScramCredentialsResponse as ASCResp
import qualified Kafka.Protocol.Generated.DescribeProducersRequest as DPReq
import qualified Kafka.Protocol.Generated.DescribeProducersResponse as DPResp
import qualified Kafka.Protocol.Generated.DescribeLogDirsRequest as DLDReq
import qualified Kafka.Protocol.Generated.DescribeLogDirsResponse as DLDResp
import qualified Kafka.Protocol.Generated.AlterReplicaLogDirsRequest as ALDReq
import qualified Kafka.Protocol.Generated.AlterReplicaLogDirsResponse as ALDResp
import qualified Kafka.Protocol.Generated.CreateDelegationTokenRequest as CDTReq
import qualified Kafka.Protocol.Generated.CreateDelegationTokenResponse as CDTResp
import qualified Kafka.Protocol.Generated.RenewDelegationTokenRequest as RDTReq
import qualified Kafka.Protocol.Generated.RenewDelegationTokenResponse as RDTResp
import qualified Kafka.Protocol.Generated.ExpireDelegationTokenRequest as EDTReq
import qualified Kafka.Protocol.Generated.ExpireDelegationTokenResponse as EDTResp
import qualified Kafka.Protocol.Generated.DescribeDelegationTokenRequest as DDTReq
import qualified Kafka.Protocol.Generated.DescribeDelegationTokenResponse as DDTResp
import qualified Kafka.Protocol.Generated.AddRaftVoterRequest as ARVReq
import qualified Kafka.Protocol.Generated.AddRaftVoterResponse as ARVResp
import qualified Kafka.Protocol.Generated.RemoveRaftVoterRequest as RRVReq
import qualified Kafka.Protocol.Generated.RemoveRaftVoterResponse as RRVResp
import qualified Kafka.Protocol.Generated.DescribeQuorumRequest as DQReq
import qualified Kafka.Protocol.Generated.DescribeQuorumResponse as DQResp
import qualified Kafka.Protocol.Generated.LeaveGroupRequest as LGRReq
import qualified Kafka.Protocol.Generated.LeaveGroupResponse as LGRResp
import Data.ByteString (ByteString)
import Data.Word (Word16)
import qualified Kafka.Common.Quota as Quota
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Wire.Codec as WC

----------------------------------------------------------------------
-- createPartitions
----------------------------------------------------------------------

-- | A request to add partitions to an existing topic. Mirrors
-- @org.apache.kafka.clients.admin.NewPartitions@. The
-- @newAssignments@ field is optional; @Nothing@ asks the broker
-- to assign partitions itself.
data NewPartitions = NewPartitions
  { npTopicName     :: !Text
  , npTotalCount    :: !Int32
    -- ^ The /new total/ partition count, not the delta — matches
    -- the JVM semantics of @NewPartitions.increaseTo@.
  , npNewAssignments :: !(Maybe [[Int32]])
    -- ^ For each new partition, the broker ids to host it on.
    -- @Nothing@ delegates the assignment to the broker.
  }
  deriving stock (Eq, Show)

-- | Increase the partition count of one or more topics. Mirrors
-- @Admin.createPartitions(Map<String, NewPartitions>)@.
--
-- Returns a list of @(topicName, result)@ pairs.
createPartitions
  :: MonadIO m
  => AdminClient
  -> [NewPartitions]
  -> m (Either String [(Text, Either String ())])
createPartitions client partitions = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 37 0 3 0 $ \conn corrId apiVer -> do
        let req = CPReq.CreatePartitionsRequest
              { CPReq.createPartitionsRequestTopics =
                  P.mkKafkaArray (V.fromList (map buildTopic partitions))
              , CPReq.createPartitionsRequestTimeoutMs = 30000
              , CPReq.createPartitionsRequestValidateOnly = False
              }
            body  = WC.runEncodeVer @CPReq.CreatePartitionsRequest apiVer req
            cid   = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 37 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("CreatePartitions request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @CPResp.CreatePartitionsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse CreatePartitionsResponse: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (CPResp.createPartitionsResponseResults resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map handleTopic rs))
  where
    buildTopic NewPartitions{..} =
      let !assignments = case npNewAssignments of
            Nothing  -> V.empty
            Just xss -> V.fromList
              [ CPReq.CreatePartitionsAssignment
                  { CPReq.createPartitionsAssignmentBrokerIds =
                      P.mkKafkaArray (V.fromList ids)
                  }
              | ids <- xss
              ]
       in CPReq.CreatePartitionsTopic
            { CPReq.createPartitionsTopicName        = P.mkKafkaString npTopicName
            , CPReq.createPartitionsTopicCount       = npTotalCount
            , CPReq.createPartitionsTopicAssignments = P.mkKafkaArray assignments
            }

    handleTopic r =
      let !name  = extractText (CPResp.createPartitionsTopicResultName r)
          !code  = CPResp.createPartitionsTopicResultErrorCode r
          !msg   = extractText (CPResp.createPartitionsTopicResultErrorMessage r)
       in if code == 0
            then (name, Right ())
            else (name, Left ("Error " <> show code <> ": " <> T.unpack msg))

----------------------------------------------------------------------
-- describeCluster
----------------------------------------------------------------------

-- | Get information about the nodes in the cluster. Mirrors
-- @Admin.describeCluster()@. Returns a 'Common.Cluster' value:
-- the cluster id, the broker list, the controller (if known),
-- and the requesting principal's cluster-level authorized
-- operations.
describeCluster :: MonadIO m => AdminClient -> m (Either String Cluster)
describeCluster client = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 60 0 2 0 $ \conn corrId apiVer -> do
        let req = DSReq.DescribeClusterRequest
              { DSReq.describeClusterRequestIncludeClusterAuthorizedOperations = False
              , DSReq.describeClusterRequestEndpointType = 1
              , DSReq.describeClusterRequestIncludeFencedBrokers = False
              }
            body = WC.runEncodeVer @DSReq.DescribeClusterRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 60 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeCluster request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DSResp.DescribeClusterResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse DescribeClusterResponse: " <> e))
              Right resp ->
                if DSResp.describeClusterResponseErrorCode resp /= 0
                  then pure $ Left $
                    "DescribeCluster: " <> T.unpack
                      (extractText (DSResp.describeClusterResponseErrorMessage resp))
                  else do
                    let brokers = case P.unKafkaArray (DSResp.describeClusterResponseBrokers resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                        !nodes = V.toList (V.map decodeBroker brokers)
                        !cid'  = extractText (DSResp.describeClusterResponseClusterId resp)
                        !cidM  = if T.null cid' then Nothing else Just cid'
                        !ctlId = DSResp.describeClusterResponseControllerId resp
                        !ctl   = lookup (fromIntegral ctlId)
                                  [ (Common.nodeId n, n) | n <- nodes ]
                    pure $ Right Common.emptyCluster
                      { clusterId         = cidM
                      , clusterNodes      = nodes
                      , clusterController = ctl
                      }
  where
    decodeBroker b = Node
      { nodeId   = fromIntegral (DSResp.describeClusterBrokerBrokerId b)
      , nodeHost = extractText (DSResp.describeClusterBrokerHost b)
      , nodePort = fromIntegral (DSResp.describeClusterBrokerPort b)
      , nodeRack =
          let r = extractText (DSResp.describeClusterBrokerRack b)
           in if T.null r then Nothing else Just r
      }

----------------------------------------------------------------------
-- listGroups (KIP-848 generic)
----------------------------------------------------------------------

-- | A row from 'listGroups'. The KIP-848 generic shape carries
-- the group type ('Common.GroupType') and current state
-- ('Common.GroupState'), not just the id; for backwards
-- compatibility 'glType' / 'glState' are 'Nothing' against
-- pre-3.7 brokers.
data GroupListing = GroupListing
  { glGroupId       :: !Text
  , glProtocolType  :: !Text
  , glState         :: !(Maybe Common.GroupState)
  , glType          :: !(Maybe Common.GroupType)
  }
  deriving stock (Eq, Show)

-- | List every group on the cluster. The two filters are
-- broker-side: empty lists mean "match everything".
listGroups
  :: MonadIO m
  => AdminClient
  -> [Common.GroupState]                 -- ^ filter by state
  -> [Common.GroupType]                  -- ^ filter by group type (KIP-848)
  -> m (Either String [GroupListing])
listGroups client states types = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 16 0 5 0 $ \conn corrId apiVer -> do
        let !stateNames =
              V.fromList
                [ P.mkKafkaString (groupStateText s) | s <- states ]
            !typeNames =
              V.fromList
                [ P.mkKafkaString (groupTypeText t) | t <- types ]
            req = LGReq.ListGroupsRequest
              { LGReq.listGroupsRequestStatesFilter = P.mkKafkaArray stateNames
              , LGReq.listGroupsRequestTypesFilter  = P.mkKafkaArray typeNames
              }
            body = WC.runEncodeVer @LGReq.ListGroupsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 16 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ListGroups request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @LGResp.ListGroupsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse ListGroupsResponse: " <> e))
              Right resp ->
                if LGResp.listGroupsResponseErrorCode resp /= 0
                  then pure $ Left $
                    "ListGroups error code: "
                      <> show (LGResp.listGroupsResponseErrorCode resp)
                  else do
                    let gs = case P.unKafkaArray (LGResp.listGroupsResponseGroups resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeGroup gs))
  where
    decodeGroup g = GroupListing
      { glGroupId      = extractText (LGResp.listedGroupGroupId g)
      , glProtocolType = extractText (LGResp.listedGroupProtocolType g)
      , glState        =
          let st = extractText (LGResp.listedGroupGroupState g)
           in groupStateFromText st
      , glType         =
          let ty = extractText (LGResp.listedGroupGroupType g)
           in groupTypeFromText ty
      }

----------------------------------------------------------------------
-- ACL admin
----------------------------------------------------------------------

-- | Per-binding result of 'createAcls'.
data AclCreationResult = AclCreationResult
  { acrBinding     :: !Acl.AclBinding
  , acrError       :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | Per-filter result of 'deleteAcls' — a count of bindings
-- deleted plus an optional error message.
data AclDeletionResult = AclDeletionResult
  { adrDeletedCount :: !Int
  , adrError        :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | Create one or more ACL bindings. Mirrors
-- @Admin.createAcls(Collection<AclBinding>)@.
createAcls
  :: MonadIO m
  => AdminClient
  -> [Acl.AclBinding]
  -> m (Either String [AclCreationResult])
createAcls client bindings = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 30 1 3 1 $ \conn corrId apiVer -> do
        let !creations = V.fromList (map buildCreation bindings)
            req = CAReq.CreateAclsRequest
              { CAReq.createAclsRequestCreations = P.mkKafkaArray creations
              }
            body = WC.runEncodeVer @CAReq.CreateAclsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 30 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("CreateAcls request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @CAResp.CreateAclsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse CreateAclsResponse: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (CAResp.createAclsResponseResults resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                    results = V.zipWith handleResult (V.fromList bindings) rs
                pure (Right (V.toList results))
  where
    buildCreation (Acl.AclBinding pat entry) = CAReq.AclCreation
      { CAReq.aclCreationResourceType        =
          resourceTypeCode (Resource.rpResourceType pat)
      , CAReq.aclCreationResourceName        =
          P.mkKafkaString (Resource.rpName pat)
      , CAReq.aclCreationResourcePatternType =
          patternTypeCode (Resource.rpPatternType pat)
      , CAReq.aclCreationPrincipal           =
          P.mkKafkaString (Acl.aceePrincipal entry)
      , CAReq.aclCreationHost                =
          P.mkKafkaString (Acl.aceeHost entry)
      , CAReq.aclCreationOperation           =
          aclOperationCode (Acl.aceeOperation entry)
      , CAReq.aclCreationPermissionType      =
          aclPermissionCode (Acl.aceePermissionType entry)
      }
    handleResult b r =
      let !code = CAResp.aclCreationResultErrorCode r
          !msg  = extractText (CAResp.aclCreationResultErrorMessage r)
       in AclCreationResult b $
            if code == 0
              then Nothing
              else Just (T.pack ("Error " <> show code <> ": ")  <> msg)

-- | List the bindings matching a filter. Mirrors
-- @Admin.describeAcls(AclBindingFilter)@.
describeAcls
  :: MonadIO m
  => AdminClient
  -> Acl.AclBindingFilter
  -> m (Either String [Acl.AclBinding])
describeAcls client (Acl.AclBindingFilter patFilter entryFilter) = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 29 1 3 1 $ \conn corrId apiVer -> do
        let req = DAReq.DescribeAclsRequest
              { DAReq.describeAclsRequestResourceTypeFilter =
                  resourceTypeCode (Resource.rpfResourceType patFilter)
              , DAReq.describeAclsRequestResourceNameFilter =
                  P.mkKafkaString
                    (maybe T.empty id (Resource.rpfName patFilter))
              , DAReq.describeAclsRequestPatternTypeFilter =
                  patternTypeCode (Resource.rpfPatternType patFilter)
              , DAReq.describeAclsRequestPrincipalFilter =
                  P.mkKafkaString
                    (maybe T.empty id (Acl.acefPrincipal entryFilter))
              , DAReq.describeAclsRequestHostFilter =
                  P.mkKafkaString
                    (maybe T.empty id (Acl.acefHost entryFilter))
              , DAReq.describeAclsRequestOperation =
                  aclOperationCode (Acl.acefOperation entryFilter)
              , DAReq.describeAclsRequestPermissionType =
                  aclPermissionCode (Acl.acefPermissionType entryFilter)
              }
            body = WC.runEncodeVer @DAReq.DescribeAclsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 29 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeAcls request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DAResp.DescribeAclsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse DescribeAclsResponse: " <> e))
              Right resp ->
                if DAResp.describeAclsResponseErrorCode resp /= 0
                  then pure $ Left $
                    "DescribeAcls: " <> T.unpack
                      (extractText (DAResp.describeAclsResponseErrorMessage resp))
                  else do
                    let resVec = case P.unKafkaArray (DAResp.describeAclsResponseResources resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                    pure $ Right $ concatMap flattenResource (V.toList resVec)
  where
    flattenResource r =
      let !rt   = resourceTypeFromCode (DAResp.describeAclsResourceResourceType r)
          !nm   = extractText (DAResp.describeAclsResourceResourceName r)
          !pt   = patternTypeFromCode (DAResp.describeAclsResourcePatternType r)
          !pat  = Resource.ResourcePattern
                    { Resource.rpResourceType = rt
                    , Resource.rpName         = nm
                    , Resource.rpPatternType  = pt
                    }
          aces = case P.unKafkaArray (DAResp.describeAclsResourceAcls r) of
                   P.Null      -> V.empty
                   P.NotNull v -> v
       in V.toList (V.map (\a -> Acl.AclBinding pat (flattenAce a)) aces)

    flattenAce a = Acl.AccessControlEntry
      { Acl.aceePrincipal      = extractText (DAResp.aclDescriptionPrincipal a)
      , Acl.aceeHost           = extractText (DAResp.aclDescriptionHost a)
      , Acl.aceeOperation      = aclOperationFromCode (DAResp.aclDescriptionOperation a)
      , Acl.aceePermissionType = aclPermissionFromCode (DAResp.aclDescriptionPermissionType a)
      }

-- | Delete the bindings matching the supplied filters. Mirrors
-- @Admin.deleteAcls(Collection<AclBindingFilter>)@.
deleteAcls
  :: MonadIO m
  => AdminClient
  -> [Acl.AclBindingFilter]
  -> m (Either String [AclDeletionResult])
deleteAcls client filters = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 31 1 3 1 $ \conn corrId apiVer -> do
        let !filterVec = V.fromList (map buildFilter filters)
            req = DelAReq.DeleteAclsRequest
              { DelAReq.deleteAclsRequestFilters = P.mkKafkaArray filterVec
              }
            body = WC.runEncodeVer @DelAReq.DeleteAclsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 31 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DeleteAcls request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DelAResp.DeleteAclsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse DeleteAclsResponse: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (DelAResp.deleteAclsResponseFilterResults resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                pure (Right (V.toList (V.map handleFilter rs)))
  where
    buildFilter (Acl.AclBindingFilter pat entry) = DelAReq.DeleteAclsFilter
      { DelAReq.deleteAclsFilterResourceTypeFilter =
          resourceTypeCode (Resource.rpfResourceType pat)
      , DelAReq.deleteAclsFilterResourceNameFilter =
          P.mkKafkaString (maybe T.empty id (Resource.rpfName pat))
      , DelAReq.deleteAclsFilterPatternTypeFilter =
          patternTypeCode (Resource.rpfPatternType pat)
      , DelAReq.deleteAclsFilterPrincipalFilter =
          P.mkKafkaString (maybe T.empty id (Acl.acefPrincipal entry))
      , DelAReq.deleteAclsFilterHostFilter =
          P.mkKafkaString (maybe T.empty id (Acl.acefHost entry))
      , DelAReq.deleteAclsFilterOperation =
          aclOperationCode (Acl.acefOperation entry)
      , DelAReq.deleteAclsFilterPermissionType =
          aclPermissionCode (Acl.acefPermissionType entry)
      }
    handleFilter f =
      let !code = DelAResp.deleteAclsFilterResultErrorCode f
          !msg  = extractText (DelAResp.deleteAclsFilterResultErrorMessage f)
          matches = case P.unKafkaArray (DelAResp.deleteAclsFilterResultMatchingAcls f) of
            P.Null      -> 0
            P.NotNull v -> V.length v
       in AclDeletionResult
            { adrDeletedCount = matches
            , adrError =
                if code == 0
                  then Nothing
                  else Just (T.pack ("Error " <> show code <> ": ") <> msg)
            }

----------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------

pickBroker :: AdminClient -> IO (Either String Conn.BrokerAddress)
pickBroker client = do
  let meta = Adm.adminMetadataOf client
  mbs <- atomically (Meta.getAllBrokers meta)
  case mbs of
    Nothing      -> pure (Left "No brokers available")
    Just []      -> pure (Left "No brokers available")
    Just (b : _) -> pure (Right (Meta.brokerMetaAddress b))

clientIdOf :: AdminClient -> P.KafkaString
clientIdOf client =
  P.mkKafkaString (Adm.adminClientId (Adm.adminConfigOf client))

----------------------------------------------------------------------
-- enum <-> wire-code conversions
----------------------------------------------------------------------

resourceTypeCode :: Resource.ResourceType -> Int8
resourceTypeCode = \case
  Resource.ResourceUnknown          -> 0
  Resource.ResourceAny              -> 1
  Resource.ResourceTopic            -> 2
  Resource.ResourceGroup            -> 3
  Resource.ResourceCluster          -> 4
  Resource.ResourceTransactionalId  -> 5
  Resource.ResourceDelegationToken  -> 6
  Resource.ResourceUser             -> 7

resourceTypeFromCode :: Int8 -> Resource.ResourceType
resourceTypeFromCode = \case
  1 -> Resource.ResourceAny
  2 -> Resource.ResourceTopic
  3 -> Resource.ResourceGroup
  4 -> Resource.ResourceCluster
  5 -> Resource.ResourceTransactionalId
  6 -> Resource.ResourceDelegationToken
  7 -> Resource.ResourceUser
  _ -> Resource.ResourceUnknown

patternTypeCode :: Resource.PatternType -> Int8
patternTypeCode = \case
  Resource.PatternUnknown  -> 0
  Resource.PatternAny      -> 1
  Resource.PatternMatch    -> 2
  Resource.PatternLiteral  -> 3
  Resource.PatternPrefixed -> 4

patternTypeFromCode :: Int8 -> Resource.PatternType
patternTypeFromCode = \case
  1 -> Resource.PatternAny
  2 -> Resource.PatternMatch
  3 -> Resource.PatternLiteral
  4 -> Resource.PatternPrefixed
  _ -> Resource.PatternUnknown

aclOperationCode :: Acl.AclOperation -> Int8
aclOperationCode = \case
  Acl.AclUnknownOp        -> 0
  Acl.AclAnyOp            -> 1
  Acl.AclAll              -> 2
  Acl.AclRead             -> 3
  Acl.AclWrite            -> 4
  Acl.AclCreate           -> 5
  Acl.AclDelete           -> 6
  Acl.AclAlter            -> 7
  Acl.AclDescribe         -> 8
  Acl.AclClusterAction    -> 9
  Acl.AclDescribeConfigs  -> 10
  Acl.AclAlterConfigs     -> 11
  Acl.AclIdempotentWrite  -> 12
  Acl.AclCreateTokens     -> 13
  Acl.AclDescribeTokens   -> 14
  Acl.AclTwoPhaseCommit   -> 15

aclOperationFromCode :: Int8 -> Acl.AclOperation
aclOperationFromCode = \case
  1  -> Acl.AclAnyOp
  2  -> Acl.AclAll
  3  -> Acl.AclRead
  4  -> Acl.AclWrite
  5  -> Acl.AclCreate
  6  -> Acl.AclDelete
  7  -> Acl.AclAlter
  8  -> Acl.AclDescribe
  9  -> Acl.AclClusterAction
  10 -> Acl.AclDescribeConfigs
  11 -> Acl.AclAlterConfigs
  12 -> Acl.AclIdempotentWrite
  13 -> Acl.AclCreateTokens
  14 -> Acl.AclDescribeTokens
  15 -> Acl.AclTwoPhaseCommit
  _  -> Acl.AclUnknownOp

aclPermissionCode :: Acl.AclPermissionType -> Int8
aclPermissionCode = \case
  Acl.AclUnknownPerm -> 0
  Acl.AclAnyPerm     -> 1
  Acl.AclDeny        -> 2
  Acl.AclAllow       -> 3

aclPermissionFromCode :: Int8 -> Acl.AclPermissionType
aclPermissionFromCode = \case
  1 -> Acl.AclAnyPerm
  2 -> Acl.AclDeny
  3 -> Acl.AclAllow
  _ -> Acl.AclUnknownPerm

groupStateText :: Common.GroupState -> Text
groupStateText = \case
  Common.GroupUnknownState -> "UNKNOWN"
  Common.GroupAssigning    -> "ASSIGNING"
  Common.GroupReconciling  -> "RECONCILING"
  Common.GroupStable       -> "STABLE"
  Common.GroupDead         -> "DEAD"
  Common.GroupEmpty        -> "EMPTY"

groupStateFromText :: Text -> Maybe Common.GroupState
groupStateFromText t = case T.toUpper t of
  "UNKNOWN"     -> Just Common.GroupUnknownState
  "ASSIGNING"   -> Just Common.GroupAssigning
  "RECONCILING" -> Just Common.GroupReconciling
  "STABLE"      -> Just Common.GroupStable
  "DEAD"        -> Just Common.GroupDead
  "EMPTY"       -> Just Common.GroupEmpty
  _             -> Nothing

groupTypeText :: Common.GroupType -> Text
groupTypeText = \case
  Common.ClassicGroup  -> "classic"
  Common.ConsumerGroup -> "consumer"
  Common.ShareGroup    -> "share"

groupTypeFromText :: Text -> Maybe Common.GroupType
groupTypeFromText t = case T.toLower t of
  "classic"  -> Just Common.ClassicGroup
  "consumer" -> Just Common.ConsumerGroup
  "share"    -> Just Common.ShareGroup
  _          -> Nothing

-- | Unwrap a wire 'P.KafkaBytes' to a strict 'ByteString'. The
-- protocol type carries a 'NotNull' tag; we collapse the 'Null'
-- case to an empty 'ByteString' since the broker only emits
-- 'NotNull' for the fields we read here.
fromKB :: P.KafkaBytes -> ByteString
fromKB (P.KafkaBytes (P.NotNull bs)) = bs
fromKB (P.KafkaBytes P.Null)         = mempty

----------------------------------------------------------------------
-- Partition reassignment (KIP-455)
----------------------------------------------------------------------

-- | A per-partition reassignment request. @prsTargetReplicas =
-- Nothing@ means "cancel any in-flight reassignment for this
-- partition".
data PartitionReassignmentSpec = PartitionReassignmentSpec
  { prsTopic          :: !Text
  , prsPartition      :: !Int32
  , prsTargetReplicas :: !(Maybe [Int32])
  }
  deriving stock (Eq, Show)

-- | An in-flight partition reassignment from
-- 'listPartitionReassignments'. Mirrors
-- @PartitionReassignment@ in the JVM SDK.
data OngoingPartitionReassignment = OngoingPartitionReassignment
  { oprTopic            :: !Text
  , oprPartition        :: !Int32
  , oprCurrentReplicas  :: ![Int32]
  , oprAddingReplicas   :: ![Int32]
  , oprRemovingReplicas :: ![Int32]
  }
  deriving stock (Eq, Show)

-- | Alter (or cancel) partition reassignments. Mirrors
-- @Admin.alterPartitionReassignments(Map<TopicPartition, Optional<NewPartitionReassignment>>)@.
alterPartitionReassignments
  :: MonadIO m
  => AdminClient
  -> [PartitionReassignmentSpec]
  -> m (Either String [(Text, Int32, Either String ())])
alterPartitionReassignments client specs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 45 0 0 0 $ \conn corrId apiVer -> do
        let grouped = groupByTopic specs
            !topics = V.fromList (map buildTopic grouped)
            req = APRReq.AlterPartitionReassignmentsRequest
              { APRReq.alterPartitionReassignmentsRequestTimeoutMs = 30000
              , APRReq.alterPartitionReassignmentsRequestTopics = P.mkKafkaArray topics
              }
            body = WC.runEncodeVer @APRReq.AlterPartitionReassignmentsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 45 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AlterPartitionReassignments request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @APRResp.AlterPartitionReassignmentsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if APRResp.alterPartitionReassignmentsResponseErrorCode resp /= 0
                  then pure $ Left $
                    "AlterPartitionReassignments: " <> T.unpack
                      (extractText (APRResp.alterPartitionReassignmentsResponseErrorMessage resp))
                  else do
                    let topicRs = case P.unKafkaArray (APRResp.alterPartitionReassignmentsResponseResponses resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                    pure $ Right $ concatMap flattenTopic (V.toList topicRs)
  where
    groupByTopic xs =
      let byT = foldr (\s acc -> Map.insertWith (++) (prsTopic s) [s] acc) Map.empty xs
       in Map.toList byT

    buildTopic (topic, rs) = APRReq.ReassignableTopic
      { APRReq.reassignableTopicName       = P.mkKafkaString topic
      , APRReq.reassignableTopicPartitions =
          P.mkKafkaArray (V.fromList (map buildPartition rs))
      }
    buildPartition s = APRReq.ReassignablePartition
      { APRReq.reassignablePartitionPartitionIndex = prsPartition s
      , APRReq.reassignablePartitionReplicas =
          case prsTargetReplicas s of
            Just rs -> P.mkKafkaArray (V.fromList rs)
            Nothing -> P.mkKafkaArray V.empty
      }
    flattenTopic t =
      let !nm = extractText (APRResp.reassignableTopicResponseName t)
          ps = case P.unKafkaArray (APRResp.reassignableTopicResponsePartitions t) of
            P.Null      -> V.empty
            P.NotNull v -> v
       in V.toList (V.map (decodePart nm) ps)
    decodePart nm p =
      let !pi_ = APRResp.reassignablePartitionResponsePartitionIndex p
          !code = APRResp.reassignablePartitionResponseErrorCode p
          !msg  = extractText (APRResp.reassignablePartitionResponseErrorMessage p)
       in if code == 0
            then (nm, pi_, Right ())
            else (nm, pi_, Left ("Error " <> show code <> ": " <> T.unpack msg))

-- | List in-flight partition reassignments. Passing 'Nothing'
-- asks for /every/ reassignment in the cluster; @'Just' tps@ scopes
-- to specific partitions.
listPartitionReassignments
  :: MonadIO m
  => AdminClient
  -> Maybe [(Text, [Int32])]              -- ^ topic + partition selector
  -> m (Either String [OngoingPartitionReassignment])
listPartitionReassignments client mScope = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 46 0 0 0 $ \conn corrId apiVer -> do
        let !topicsArr = case mScope of
              Nothing -> P.mkKafkaArray V.empty
              Just sc -> P.mkKafkaArray $ V.fromList
                [ LPRReq.ListPartitionReassignmentsTopics
                    { LPRReq.listPartitionReassignmentsTopicsName =
                        P.mkKafkaString t
                    , LPRReq.listPartitionReassignmentsTopicsPartitionIndexes =
                        P.mkKafkaArray (V.fromList ps)
                    }
                | (t, ps) <- sc
                ]
            req = LPRReq.ListPartitionReassignmentsRequest
              { LPRReq.listPartitionReassignmentsRequestTimeoutMs = 30000
              , LPRReq.listPartitionReassignmentsRequestTopics    = topicsArr
              }
            body = WC.runEncodeVer @LPRReq.ListPartitionReassignmentsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 46 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ListPartitionReassignments request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @LPRResp.ListPartitionReassignmentsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if LPRResp.listPartitionReassignmentsResponseErrorCode resp /= 0
                  then pure $ Left $
                    "ListPartitionReassignments: " <> T.unpack
                      (extractText (LPRResp.listPartitionReassignmentsResponseErrorMessage resp))
                  else do
                    let topicRs = case P.unKafkaArray (LPRResp.listPartitionReassignmentsResponseTopics resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                    pure $ Right $ concatMap flattenT (V.toList topicRs)
  where
    flattenT t =
      let !nm  = extractText (LPRResp.ongoingTopicReassignmentName t)
          ps = case P.unKafkaArray (LPRResp.ongoingTopicReassignmentPartitions t) of
            P.Null      -> V.empty
            P.NotNull v -> v
       in V.toList (V.map (decodeP nm) ps)
    decodeP nm p = OngoingPartitionReassignment
      { oprTopic            = nm
      , oprPartition        = LPRResp.ongoingPartitionReassignmentPartitionIndex p
      , oprCurrentReplicas  = unArr (LPRResp.ongoingPartitionReassignmentReplicas p)
      , oprAddingReplicas   = unArr (LPRResp.ongoingPartitionReassignmentAddingReplicas p)
      , oprRemovingReplicas = unArr (LPRResp.ongoingPartitionReassignmentRemovingReplicas p)
      }
    unArr arr = case P.unKafkaArray arr of
      P.Null      -> []
      P.NotNull v -> V.toList v

----------------------------------------------------------------------
-- Broker lifecycle
----------------------------------------------------------------------

-- | Unregister a broker. Mirrors @Admin.unregisterBroker(int)@.
-- Returns @Right ()@ on broker-side success; the @Left@ payload
-- carries the error code + message.
unregisterBroker
  :: MonadIO m
  => AdminClient
  -> Int32                                -- ^ broker id
  -> m (Either String ())
unregisterBroker client bid = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 64 0 0 0 $ \conn corrId apiVer -> do
        let req = UBReq.UnregisterBrokerRequest
              { UBReq.unregisterBrokerRequestBrokerId = bid
              }
            body = WC.runEncodeVer @UBReq.UnregisterBrokerRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 64 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("UnregisterBroker request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @UBResp.UnregisterBrokerResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if UBResp.unregisterBrokerResponseErrorCode resp == 0
                  then pure (Right ())
                  else pure $ Left $
                    "UnregisterBroker: error "
                      <> show (UBResp.unregisterBrokerResponseErrorCode resp)
                      <> ": "
                      <> T.unpack (extractText (UBResp.unregisterBrokerResponseErrorMessage resp))

----------------------------------------------------------------------
-- Client-quota admin (KIP-546)
----------------------------------------------------------------------

-- | A described quota entry — an entity together with the
-- per-name quota values configured for it. Mirrors
-- @ClientQuotaEntry@ in the JVM SDK.
data ClientQuotaEntry = ClientQuotaEntry
  { cqeEntity :: !Quota.ClientQuotaEntity
  , cqeValues :: !(Map Text Double)
  }
  deriving stock (Eq, Show)

-- | Describe quotas. Mirrors @Admin.describeClientQuotas@.
describeClientQuotas
  :: MonadIO m
  => AdminClient
  -> Quota.ClientQuotaFilter
  -> m (Either String [ClientQuotaEntry])
describeClientQuotas client (Quota.ClientQuotaFilter comps strict) = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 48 0 1 0 $ \conn corrId apiVer -> do
        let req = DCQReq.DescribeClientQuotasRequest
              { DCQReq.describeClientQuotasRequestComponents =
                  P.mkKafkaArray (V.fromList (map buildComp comps))
              , DCQReq.describeClientQuotasRequestStrict = strict
              }
            body = WC.runEncodeVer @DCQReq.DescribeClientQuotasRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 48 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeClientQuotas request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DCQResp.DescribeClientQuotasResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if DCQResp.describeClientQuotasResponseErrorCode resp /= 0
                  then pure $ Left $
                    "DescribeClientQuotas: " <> T.unpack
                      (extractText (DCQResp.describeClientQuotasResponseErrorMessage resp))
                  else do
                    let entries = case P.unKafkaArray (DCQResp.describeClientQuotasResponseEntries resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeEntry entries))
  where
    buildComp c =
      let (mt, mv) = case Quota.cqfcMatchType c of
            Quota.MatchExact nm -> (0 :: Int8, nm)
            Quota.MatchDefault  -> (1, T.empty)
            Quota.MatchAny      -> (2, T.empty)
       in DCQReq.ComponentData
            { DCQReq.componentDataEntityType = P.mkKafkaString (Quota.cqfcEntityType c)
            , DCQReq.componentDataMatchType  = mt
            , DCQReq.componentDataMatch      = P.mkKafkaString mv
            }
    decodeEntry e =
      let !ents = case P.unKafkaArray (DCQResp.entryDataEntity e) of
            P.Null      -> V.empty
            P.NotNull v -> v
          !vals = case P.unKafkaArray (DCQResp.entryDataValues e) of
            P.Null      -> V.empty
            P.NotNull v -> v
          !entMap = Map.fromList
            [ ( extractText (DCQResp.entityDataEntityType ed)
              , let n = extractText (DCQResp.entityDataEntityName ed)
                 in if T.null n then Nothing else Just n
              )
            | ed <- V.toList ents
            ]
          !valMap = Map.fromList
            [ ( extractText (DCQResp.valueDataKey vd)
              , DCQResp.valueDataValue vd
              )
            | vd <- V.toList vals
            ]
       in ClientQuotaEntry
            { cqeEntity = Quota.ClientQuotaEntity entMap
            , cqeValues = valMap
            }

-- | Alter quotas. Mirrors @Admin.alterClientQuotas@.
alterClientQuotas
  :: MonadIO m
  => AdminClient
  -> [Quota.ClientQuotaAlteration]
  -> Bool                                 -- ^ validateOnly
  -> m (Either String [(Quota.ClientQuotaEntity, Either String ())])
alterClientQuotas client alterations validate = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 49 0 1 0 $ \conn corrId apiVer -> do
        let req = ACQReq.AlterClientQuotasRequest
              { ACQReq.alterClientQuotasRequestEntries =
                  P.mkKafkaArray (V.fromList (map buildEntry alterations))
              , ACQReq.alterClientQuotasRequestValidateOnly = validate
              }
            body = WC.runEncodeVer @ACQReq.AlterClientQuotasRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 49 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AlterClientQuotas request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @ACQResp.AlterClientQuotasResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (ACQResp.alterClientQuotasResponseEntries resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decodeEntry rs))
  where
    buildEntry (Quota.ClientQuotaAlteration (Quota.ClientQuotaEntity entMap) ops) =
      ACQReq.EntryData
        { ACQReq.entryDataEntity =
            P.mkKafkaArray $ V.fromList
              [ ACQReq.EntityData
                  { ACQReq.entityDataEntityType = P.mkKafkaString k
                  , ACQReq.entityDataEntityName = P.mkKafkaString (maybe T.empty id v)
                  }
              | (k, v) <- Map.toList entMap
              ]
        , ACQReq.entryDataOps =
            P.mkKafkaArray $ V.fromList
              [ ACQReq.OpData
                  { ACQReq.opDataKey    = P.mkKafkaString (Quota.cqoKey op)
                  , ACQReq.opDataValue  = maybe 0 id (Quota.cqoValue op)
                  , ACQReq.opDataRemove = case Quota.cqoValue op of
                      Nothing -> True
                      Just _  -> False
                  }
              | op <- ops
              ]
        }
    decodeEntry e =
      let !ents = case P.unKafkaArray (ACQResp.entryDataEntity e) of
            P.Null      -> V.empty
            P.NotNull v -> v
          !entMap = Map.fromList
            [ ( extractText (ACQResp.entityDataEntityType ed)
              , let n = extractText (ACQResp.entityDataEntityName ed)
                 in if T.null n then Nothing else Just n
              )
            | ed <- V.toList ents
            ]
          !ent  = Quota.ClientQuotaEntity entMap
          !code = ACQResp.entryDataErrorCode e
          !msg  = extractText (ACQResp.entryDataErrorMessage e)
       in if code == 0
            then (ent, Right ())
            else (ent, Left ("Error " <> show code <> ": " <> T.unpack msg))

----------------------------------------------------------------------
-- Transaction admin (KIP-664)
----------------------------------------------------------------------

-- | A row from 'listTransactions'.
data TransactionListing = TransactionListing
  { tlTransactionalId :: !Text
  , tlProducerId      :: !Int64
  , tlState           :: !Text
    -- ^ One of: @\"Empty\"@, @\"Ongoing\"@, @\"PrepareCommit\"@,
    -- @\"PrepareAbort\"@, @\"CompleteCommit\"@,
    -- @\"CompleteAbort\"@, @\"Dead\"@, @\"PrepareEpochFence\"@,
    -- @\"Unknown\"@.
  }
  deriving stock (Eq, Show)

-- | The detailed state of a single transaction.
data TransactionDescription = TransactionDescription
  { tdTransactionalId   :: !Text
  , tdProducerId        :: !Int64
  , tdProducerEpoch     :: !Int16
  , tdTimeoutMs         :: !Int32
  , tdStartTimeMs       :: !Int64
  , tdState             :: !Text
  , tdTopicPartitions   :: ![TransactionTopicPartitions]
  }
  deriving stock (Eq, Show)

-- | The partitions of a single topic enrolled in a transaction.
data TransactionTopicPartitions = TransactionTopicPartitions
  { ttpTopic      :: !Text
  , ttpPartitions :: ![Int32]
  }
  deriving stock (Eq, Show)

-- | List active transactions on the cluster, optionally filtered
-- by transaction state and producer id. Mirrors
-- @Admin.listTransactions()@.
listTransactions
  :: MonadIO m
  => AdminClient
  -> [Text]                               -- ^ state filters
  -> [Int64]                              -- ^ producer-id filters
  -> Maybe Int64                          -- ^ min duration in ms
  -> m (Either String [TransactionListing])
listTransactions client stateFilters pidFilters durMs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 66 0 1 0 $ \conn corrId apiVer -> do
        let req = LTReq.ListTransactionsRequest
              { LTReq.listTransactionsRequestStateFilters =
                  P.mkKafkaArray (V.fromList (map P.mkKafkaString stateFilters))
              , LTReq.listTransactionsRequestProducerIdFilters =
                  P.mkKafkaArray (V.fromList pidFilters)
              , LTReq.listTransactionsRequestDurationFilter =
                  maybe (-1) id durMs
              }
            body = WC.runEncodeVer @LTReq.ListTransactionsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 66 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ListTransactions request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @LTResp.ListTransactionsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if LTResp.listTransactionsResponseErrorCode resp /= 0
                  then pure $ Left $
                    "ListTransactions: error code "
                      <> show (LTResp.listTransactionsResponseErrorCode resp)
                  else do
                    let ts = case P.unKafkaArray (LTResp.listTransactionsResponseTransactionStates resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decode_ ts))
  where
    decode_ t = TransactionListing
      { tlTransactionalId = extractText (LTResp.transactionStateTransactionalId t)
      , tlProducerId      = LTResp.transactionStateProducerId t
      , tlState           = extractText (LTResp.transactionStateTransactionState t)
      }

-- | Describe the supplied transactions. Mirrors
-- @Admin.describeTransactions(Collection<String>)@.
describeTransactions
  :: MonadIO m
  => AdminClient
  -> [Text]                               -- ^ transactional ids
  -> m (Either String [TransactionDescription])
describeTransactions client tids = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 65 0 0 0 $ \conn corrId apiVer -> do
        let req = DTReq.DescribeTransactionsRequest
              { DTReq.describeTransactionsRequestTransactionalIds =
                  P.mkKafkaArray (V.fromList (map P.mkKafkaString tids))
              }
            body = WC.runEncodeVer @DTReq.DescribeTransactionsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 65 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeTransactions request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DTResp.DescribeTransactionsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let ts = case P.unKafkaArray (DTResp.describeTransactionsResponseTransactionStates resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decode_ ts))
  where
    decode_ t =
      let !topics = case P.unKafkaArray (DTResp.transactionStateTopics t) of
            P.Null      -> V.empty
            P.NotNull v -> v
       in TransactionDescription
            { tdTransactionalId = extractText (DTResp.transactionStateTransactionalId t)
            , tdProducerId      = DTResp.transactionStateProducerId t
            , tdProducerEpoch   = DTResp.transactionStateProducerEpoch t
            , tdTimeoutMs       = DTResp.transactionStateTransactionTimeoutMs t
            , tdStartTimeMs     = DTResp.transactionStateTransactionStartTimeMs t
            , tdState           = extractText (DTResp.transactionStateTransactionState t)
            , tdTopicPartitions =
                V.toList (V.map decodeTopic topics)
            }
    decodeTopic tp =
      let !ps = case P.unKafkaArray (DTResp.topicDataPartitions tp) of
            P.Null      -> []
            P.NotNull v -> V.toList v
       in TransactionTopicPartitions
            { ttpTopic      = extractText (DTResp.topicDataTopic tp)
            , ttpPartitions = ps
            }

----------------------------------------------------------------------
-- SCRAM credential admin (KIP-554)
----------------------------------------------------------------------

-- | SCRAM mechanism identifier. Mirrors
-- @org.apache.kafka.clients.admin.ScramMechanism@. Codes
-- match the broker wire shape.
data ScramMechanism
  = ScramSha256
  | ScramSha512
  | ScramUnknown
  deriving stock (Eq, Show)

scramMechanismCode :: ScramMechanism -> Int8
scramMechanismCode = \case
  ScramUnknown -> 0
  ScramSha256  -> 1
  ScramSha512  -> 2

scramMechanismFromCode :: Int8 -> ScramMechanism
scramMechanismFromCode = \case
  1 -> ScramSha256
  2 -> ScramSha512
  _ -> ScramUnknown

-- | Per-user credential metadata returned by
-- 'describeUserScramCredentials'.
data ScramCredentialInfo = ScramCredentialInfo
  { sciMechanism  :: !ScramMechanism
  , sciIterations :: !Int32
  }
  deriving stock (Eq, Show)

-- | Add or update a SCRAM credential. The broker requires
-- caller-supplied salt + salted password (PBKDF2-applied).
data ScramCredentialUpsertion = ScramCredentialUpsertion
  { scuUser           :: !Text
  , scuMechanism      :: !ScramMechanism
  , scuIterations     :: !Int32
  , scuSalt           :: !ByteString
  , scuSaltedPassword :: !ByteString
  }
  deriving stock (Eq, Show)

-- | Delete a SCRAM credential for a user under a specific mechanism.
data ScramCredentialDeletion = ScramCredentialDeletion
  { scdUser      :: !Text
  , scdMechanism :: !ScramMechanism
  }
  deriving stock (Eq, Show)

-- | Describe SCRAM credentials for the supplied users. Passing
-- @[]@ asks for every user the requesting principal can see.
describeUserScramCredentials
  :: MonadIO m
  => AdminClient
  -> [Text]
  -> m (Either String [(Text, Either String [ScramCredentialInfo])])
describeUserScramCredentials client users = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 50 0 0 0 $ \conn corrId apiVer -> do
        let req = DSCReq.DescribeUserScramCredentialsRequest
              { DSCReq.describeUserScramCredentialsRequestUsers =
                  P.mkKafkaArray $ V.fromList
                    [ DSCReq.UserName { DSCReq.userNameName = P.mkKafkaString u } | u <- users ]
              }
            body = WC.runEncodeVer @DSCReq.DescribeUserScramCredentialsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 50 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeUserScramCredentials request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DSCResp.DescribeUserScramCredentialsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if DSCResp.describeUserScramCredentialsResponseErrorCode resp /= 0
                  then pure $ Left $
                    "DescribeUserScramCredentials: " <> T.unpack
                      (extractText (DSCResp.describeUserScramCredentialsResponseErrorMessage resp))
                  else do
                    let rs = case P.unKafkaArray (DSCResp.describeUserScramCredentialsResponseResults resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeUser rs))
  where
    decodeUser r =
      let !nm   = extractText (DSCResp.describeUserScramCredentialsResultUser r)
          !code = DSCResp.describeUserScramCredentialsResultErrorCode r
          !msg  = extractText (DSCResp.describeUserScramCredentialsResultErrorMessage r)
       in if code == 0
            then
              let cs = case P.unKafkaArray (DSCResp.describeUserScramCredentialsResultCredentialInfos r) of
                    P.Null      -> V.empty
                    P.NotNull v -> v
               in (nm, Right (V.toList (V.map decodeCI cs)))
            else (nm, Left ("Error " <> show code <> ": " <> T.unpack msg))
    decodeCI ci = ScramCredentialInfo
      { sciMechanism  = scramMechanismFromCode (DSCResp.credentialInfoMechanism ci)
      , sciIterations = DSCResp.credentialInfoIterations ci
      }

-- | Add and/or remove SCRAM credentials. Mirrors
-- @Admin.alterUserScramCredentials(List<UserScramCredentialAlteration>)@.
alterUserScramCredentials
  :: MonadIO m
  => AdminClient
  -> [ScramCredentialUpsertion]
  -> [ScramCredentialDeletion]
  -> m (Either String [(Text, Either String ())])
alterUserScramCredentials client upserts deletes = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 51 0 0 0 $ \conn corrId apiVer -> do
        let req = ASCReq.AlterUserScramCredentialsRequest
              { ASCReq.alterUserScramCredentialsRequestDeletions =
                  P.mkKafkaArray (V.fromList (map buildDel deletes))
              , ASCReq.alterUserScramCredentialsRequestUpsertions =
                  P.mkKafkaArray (V.fromList (map buildUps upserts))
              }
            body = WC.runEncodeVer @ASCReq.AlterUserScramCredentialsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 51 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AlterUserScramCredentials request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @ASCResp.AlterUserScramCredentialsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (ASCResp.alterUserScramCredentialsResponseResults resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decodeR rs))
  where
    buildDel d = ASCReq.ScramCredentialDeletion
      { ASCReq.scramCredentialDeletionName      = P.mkKafkaString (scdUser d)
      , ASCReq.scramCredentialDeletionMechanism = scramMechanismCode (scdMechanism d)
      }
    buildUps u = ASCReq.ScramCredentialUpsertion
      { ASCReq.scramCredentialUpsertionName           = P.mkKafkaString (scuUser u)
      , ASCReq.scramCredentialUpsertionMechanism      = scramMechanismCode (scuMechanism u)
      , ASCReq.scramCredentialUpsertionIterations     = scuIterations u
      , ASCReq.scramCredentialUpsertionSalt           = P.mkKafkaBytes (scuSalt u)
      , ASCReq.scramCredentialUpsertionSaltedPassword = P.mkKafkaBytes (scuSaltedPassword u)
      }
    decodeR r =
      let !nm   = extractText (ASCResp.alterUserScramCredentialsResultUser r)
          !code = ASCResp.alterUserScramCredentialsResultErrorCode r
          !msg  = extractText (ASCResp.alterUserScramCredentialsResultErrorMessage r)
       in if code == 0
            then (nm, Right ())
            else (nm, Left ("Error " <> show code <> ": " <> T.unpack msg))

----------------------------------------------------------------------
-- Producer-state admin (KIP-664)
----------------------------------------------------------------------

-- | Active-producer snapshot for a partition. Mirrors
-- @ProducerState@ in the JVM SDK.
data ProducerState = ProducerState
  { psProducerId             :: !Int64
  , psProducerEpoch          :: !Int32
  , psLastSequence           :: !Int32
  , psLastTimestamp          :: !Int64
  , psCoordinatorEpoch       :: !Int32
  , psCurrentTxnStartOffset  :: !Int64
  }
  deriving stock (Eq, Show)

-- | Describe the producer state for the supplied partitions.
describeProducers
  :: MonadIO m
  => AdminClient
  -> [(Text, [Int32])]                    -- ^ topic → partition list
  -> m (Either String [(Text, Int32, Either String [ProducerState])])
describeProducers client targets = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 61 0 0 0 $ \conn corrId apiVer -> do
        let !topicReqs = V.fromList
              [ DPReq.TopicRequest
                  { DPReq.topicRequestName            = P.mkKafkaString t
                  , DPReq.topicRequestPartitionIndexes = P.mkKafkaArray (V.fromList ps)
                  }
              | (t, ps) <- targets
              ]
            req = DPReq.DescribeProducersRequest
              { DPReq.describeProducersRequestTopics = P.mkKafkaArray topicReqs
              }
            body = WC.runEncodeVer @DPReq.DescribeProducersRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 61 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeProducers request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DPResp.DescribeProducersResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let topicRs = case P.unKafkaArray (DPResp.describeProducersResponseTopics resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                pure $ Right $ concatMap flattenT (V.toList topicRs)
  where
    flattenT t =
      let !nm = extractText (DPResp.topicResponseName t)
          ps = case P.unKafkaArray (DPResp.topicResponsePartitions t) of
            P.Null      -> V.empty
            P.NotNull v -> v
       in V.toList (V.map (decodeP nm) ps)
    decodeP nm p =
      let !pi_  = DPResp.partitionResponsePartitionIndex p
          !code = DPResp.partitionResponseErrorCode p
          !msg  = extractText (DPResp.partitionResponseErrorMessage p)
       in if code == 0
            then
              let ps_ = case P.unKafkaArray (DPResp.partitionResponseActiveProducers p) of
                    P.Null      -> V.empty
                    P.NotNull v -> v
               in (nm, pi_, Right (V.toList (V.map decodeS ps_)))
            else (nm, pi_, Left ("Error " <> show code <> ": " <> T.unpack msg))
    decodeS s = ProducerState
      { psProducerId            = DPResp.producerStateProducerId s
      , psProducerEpoch         = DPResp.producerStateProducerEpoch s
      , psLastSequence          = DPResp.producerStateLastSequence s
      , psLastTimestamp         = DPResp.producerStateLastTimestamp s
      , psCoordinatorEpoch      = DPResp.producerStateCoordinatorEpoch s
      , psCurrentTxnStartOffset = DPResp.producerStateCurrentTxnStartOffset s
      }

----------------------------------------------------------------------
-- Log directory admin
----------------------------------------------------------------------

-- | A single broker's report of a log directory.
data LogDirDescription = LogDirDescription
  { lddPath        :: !Text
  , lddErrorCode   :: !Int16
  , lddTotalBytes  :: !Int64
  , lddUsableBytes :: !Int64
  , lddTopics      :: ![TopicLogDirDescription]
  }
  deriving stock (Eq, Show)

data TopicLogDirDescription = TopicLogDirDescription
  { tlddName       :: !Text
  , tlddPartitions :: ![PartitionLogDirDescription]
  }
  deriving stock (Eq, Show)

data PartitionLogDirDescription = PartitionLogDirDescription
  { pldPartition    :: !Int32
  , pldPartitionSize :: !Int64
  , pldOffsetLag    :: !Int64
  , pldIsFutureKey  :: !Bool
  }
  deriving stock (Eq, Show)

-- | Describe the log directories on the supplied partitions.
-- Mirrors @Admin.describeLogDirs(Collection<Integer>)@ — the
-- JVM variant takes broker ids; this one piggy-backs on the
-- admin client's currently-connected broker and only reports
-- its log dirs.
describeLogDirs
  :: MonadIO m
  => AdminClient
  -> [(Text, [Int32])]                    -- ^ topics × partitions to query
  -> m (Either String [LogDirDescription])
describeLogDirs client targets = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 35 0 4 0 $ \conn corrId apiVer -> do
        let !ts = V.fromList
              [ DLDReq.DescribableLogDirTopic
                  { DLDReq.describableLogDirTopicTopic      = P.mkKafkaString t
                  , DLDReq.describableLogDirTopicPartitions = P.mkKafkaArray (V.fromList ps)
                  }
              | (t, ps) <- targets
              ]
            req = DLDReq.DescribeLogDirsRequest
              { DLDReq.describeLogDirsRequestTopics = P.mkKafkaArray ts
              }
            body = WC.runEncodeVer @DLDReq.DescribeLogDirsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 35 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeLogDirs request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DLDResp.DescribeLogDirsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (DLDResp.describeLogDirsResponseResults resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decodeR rs))
  where
    decodeR r =
      let !ts = case P.unKafkaArray (DLDResp.describeLogDirsResultTopics r) of
            P.Null      -> V.empty
            P.NotNull v -> v
       in LogDirDescription
            { lddPath        = extractText (DLDResp.describeLogDirsResultLogDir r)
            , lddErrorCode   = DLDResp.describeLogDirsResultErrorCode r
            , lddTotalBytes  = DLDResp.describeLogDirsResultTotalBytes r
            , lddUsableBytes = DLDResp.describeLogDirsResultUsableBytes r
            , lddTopics      = V.toList (V.map decodeT ts)
            }
    decodeT t =
      let !ps = case P.unKafkaArray (DLDResp.describeLogDirsTopicPartitions t) of
            P.Null      -> V.empty
            P.NotNull v -> v
       in TopicLogDirDescription
            { tlddName       = extractText (DLDResp.describeLogDirsTopicName t)
            , tlddPartitions = V.toList (V.map decodeP ps)
            }
    decodeP p = PartitionLogDirDescription
      { pldPartition    = DLDResp.describeLogDirsPartitionPartitionIndex p
      , pldPartitionSize = DLDResp.describeLogDirsPartitionPartitionSize p
      , pldOffsetLag    = DLDResp.describeLogDirsPartitionOffsetLag p
      , pldIsFutureKey  = DLDResp.describeLogDirsPartitionIsFutureKey p
      }

-- | Move replicas to specific log directories. Each entry says
-- "for these (topic, partition) pairs, put them on this path".
data ReplicaLogDirAssignment = ReplicaLogDirAssignment
  { rldaPath       :: !Text
  , rldaPartitions :: ![(Text, [Int32])]
  }
  deriving stock (Eq, Show)

-- | Reassign replicas to specific log directories. Mirrors
-- @Admin.alterReplicaLogDirs(Map<TopicPartitionReplica, String>)@
-- (we adopt the per-path shape because that's how the wire
-- carries the request — JVM users flip it client-side).
alterReplicaLogDirs
  :: MonadIO m
  => AdminClient
  -> [ReplicaLogDirAssignment]
  -> m (Either String [(Text, Int32, Either String ())])
alterReplicaLogDirs client assignments = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 34 0 2 0 $ \conn corrId apiVer -> do
        let !dirs = V.fromList
              [ ALDReq.AlterReplicaLogDir
                  { ALDReq.alterReplicaLogDirPath = P.mkKafkaString (rldaPath a)
                  , ALDReq.alterReplicaLogDirTopics = P.mkKafkaArray $ V.fromList
                      [ ALDReq.AlterReplicaLogDirTopic
                          { ALDReq.alterReplicaLogDirTopicName       = P.mkKafkaString t
                          , ALDReq.alterReplicaLogDirTopicPartitions = P.mkKafkaArray (V.fromList ps)
                          }
                      | (t, ps) <- rldaPartitions a
                      ]
                  }
              | a <- assignments
              ]
            req = ALDReq.AlterReplicaLogDirsRequest
              { ALDReq.alterReplicaLogDirsRequestDirs = P.mkKafkaArray dirs
              }
            body = WC.runEncodeVer @ALDReq.AlterReplicaLogDirsRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 34 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AlterReplicaLogDirs request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @ALDResp.AlterReplicaLogDirsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (ALDResp.alterReplicaLogDirsResponseResults resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                pure $ Right (concatMap flattenT (V.toList rs))
  where
    flattenT t =
      let !nm = extractText (ALDResp.alterReplicaLogDirTopicResultTopicName t)
          ps = case P.unKafkaArray (ALDResp.alterReplicaLogDirTopicResultPartitions t) of
            P.Null      -> V.empty
            P.NotNull v -> v
       in V.toList (V.map (decodeP nm) ps)
    decodeP nm p =
      let !pi_  = ALDResp.alterReplicaLogDirPartitionResultPartitionIndex p
          !code = ALDResp.alterReplicaLogDirPartitionResultErrorCode p
       in if code == 0
            then (nm, pi_, Right ())
            else (nm, pi_, Left ("Error " <> show code))

----------------------------------------------------------------------
-- Delegation tokens (KIP-48)
----------------------------------------------------------------------

-- | A described delegation token.
data DelegationToken = DelegationToken
  { dtTokenId        :: !Text
  , dtHmac           :: !ByteString
  , dtOwner          :: !(Text, Text)  -- principal type, principal name
  , dtTokenRequester :: !(Text, Text)
  , dtIssueTimestamp :: !Int64
  , dtExpiryTimestamp :: !Int64
  , dtMaxTimestamp   :: !Int64
  }
  deriving stock (Eq, Show)

-- | Create a delegation token. The optional renewers list
-- nominates additional principals allowed to renew/expire the
-- token; pass @[]@ to lock it down to the issuer.
createDelegationToken
  :: MonadIO m
  => AdminClient
  -> Maybe (Text, Text)                   -- ^ override owner principal (Nothing = use the issuer)
  -> [(Text, Text)]                       -- ^ renewers
  -> Int64                                -- ^ max lifetime ms (negative = broker default)
  -> m (Either String DelegationToken)
createDelegationToken client mOwner renewers maxLifeMs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 38 0 3 0 $ \conn corrId apiVer -> do
        let (ot, on) = case mOwner of
              Just (t, n) -> (t, n)
              Nothing     -> ("", "")
            !rens = V.fromList
              [ CDTReq.CreatableRenewers
                  { CDTReq.creatableRenewersPrincipalType = P.mkKafkaString t
                  , CDTReq.creatableRenewersPrincipalName = P.mkKafkaString n
                  }
              | (t, n) <- renewers
              ]
            req = CDTReq.CreateDelegationTokenRequest
              { CDTReq.createDelegationTokenRequestOwnerPrincipalType = P.mkKafkaString ot
              , CDTReq.createDelegationTokenRequestOwnerPrincipalName = P.mkKafkaString on
              , CDTReq.createDelegationTokenRequestRenewers           = P.mkKafkaArray rens
              , CDTReq.createDelegationTokenRequestMaxLifetimeMs      = maxLifeMs
              }
            body = WC.runEncodeVer @CDTReq.CreateDelegationTokenRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 38 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("CreateDelegationToken request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @CDTResp.CreateDelegationTokenResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if CDTResp.createDelegationTokenResponseErrorCode resp /= 0
                  then pure $ Left $ "CreateDelegationToken: error code "
                    <> show (CDTResp.createDelegationTokenResponseErrorCode resp)
                  else pure $ Right DelegationToken
                    { dtTokenId        =
                        extractText (CDTResp.createDelegationTokenResponseTokenId resp)
                    , dtHmac           =
                        fromKB (CDTResp.createDelegationTokenResponseHmac resp)
                    , dtOwner          =
                        ( extractText (CDTResp.createDelegationTokenResponsePrincipalType resp)
                        , extractText (CDTResp.createDelegationTokenResponsePrincipalName resp)
                        )
                    , dtTokenRequester =
                        ( extractText (CDTResp.createDelegationTokenResponseTokenRequesterPrincipalType resp)
                        , extractText (CDTResp.createDelegationTokenResponseTokenRequesterPrincipalName resp)
                        )
                    , dtIssueTimestamp = CDTResp.createDelegationTokenResponseIssueTimestampMs resp
                    , dtExpiryTimestamp = CDTResp.createDelegationTokenResponseExpiryTimestampMs resp
                    , dtMaxTimestamp   = CDTResp.createDelegationTokenResponseMaxTimestampMs resp
                    }

-- | Push the token's expiry deadline forward by @renewPeriodMs@.
-- Returns the new expiry timestamp on success.
renewDelegationToken
  :: MonadIO m
  => AdminClient
  -> ByteString                           -- ^ HMAC of the token to renew
  -> Int64                                -- ^ renew period ms
  -> m (Either String Int64)
renewDelegationToken client hmac periodMs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 39 0 2 0 $ \conn corrId apiVer -> do
        let req = RDTReq.RenewDelegationTokenRequest
              { RDTReq.renewDelegationTokenRequestHmac          = P.mkKafkaBytes hmac
              , RDTReq.renewDelegationTokenRequestRenewPeriodMs = periodMs
              }
            body = WC.runEncodeVer @RDTReq.RenewDelegationTokenRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 39 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("RenewDelegationToken request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @RDTResp.RenewDelegationTokenResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if RDTResp.renewDelegationTokenResponseErrorCode resp == 0
                  then pure $ Right (RDTResp.renewDelegationTokenResponseExpiryTimestampMs resp)
                  else pure $ Left $ "RenewDelegationToken: error code "
                    <> show (RDTResp.renewDelegationTokenResponseErrorCode resp)

-- | Set the token's expiry deadline to @now + expiryPeriodMs@.
-- Passing a negative period invalidates the token immediately.
-- Returns the new expiry timestamp.
expireDelegationToken
  :: MonadIO m
  => AdminClient
  -> ByteString
  -> Int64
  -> m (Either String Int64)
expireDelegationToken client hmac periodMs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 40 0 2 0 $ \conn corrId apiVer -> do
        let req = EDTReq.ExpireDelegationTokenRequest
              { EDTReq.expireDelegationTokenRequestHmac          = P.mkKafkaBytes hmac
              , EDTReq.expireDelegationTokenRequestExpiryTimePeriodMs = periodMs
              }
            body = WC.runEncodeVer @EDTReq.ExpireDelegationTokenRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 40 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ExpireDelegationToken request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @EDTResp.ExpireDelegationTokenResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if EDTResp.expireDelegationTokenResponseErrorCode resp == 0
                  then pure $ Right (EDTResp.expireDelegationTokenResponseExpiryTimestampMs resp)
                  else pure $ Left $ "ExpireDelegationToken: error code "
                    <> show (EDTResp.expireDelegationTokenResponseErrorCode resp)

-- | Describe issued delegation tokens. Pass @[]@ to ask for
-- /every/ token the requesting principal can see.
describeDelegationToken
  :: MonadIO m
  => AdminClient
  -> [(Text, Text)]                       -- ^ owner principals to filter on
  -> m (Either String [DelegationToken])
describeDelegationToken client owners = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 41 0 3 0 $ \conn corrId apiVer -> do
        let !os = V.fromList
              [ DDTReq.DescribeDelegationTokenOwner
                  { DDTReq.describeDelegationTokenOwnerPrincipalType = P.mkKafkaString t
                  , DDTReq.describeDelegationTokenOwnerPrincipalName = P.mkKafkaString n
                  }
              | (t, n) <- owners
              ]
            req = DDTReq.DescribeDelegationTokenRequest
              { DDTReq.describeDelegationTokenRequestOwners = P.mkKafkaArray os
              }
            body = WC.runEncodeVer @DDTReq.DescribeDelegationTokenRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 41 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeDelegationToken request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DDTResp.DescribeDelegationTokenResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if DDTResp.describeDelegationTokenResponseErrorCode resp /= 0
                  then pure $ Left $ "DescribeDelegationToken: error code "
                    <> show (DDTResp.describeDelegationTokenResponseErrorCode resp)
                  else do
                    let ts = case P.unKafkaArray (DDTResp.describeDelegationTokenResponseTokens resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeT ts))
  where
    decodeT t = DelegationToken
      { dtTokenId         = extractText (DDTResp.describedDelegationTokenTokenId t)
      , dtHmac            = fromKB (DDTResp.describedDelegationTokenHmac t)
      , dtOwner           =
          ( extractText (DDTResp.describedDelegationTokenPrincipalType t)
          , extractText (DDTResp.describedDelegationTokenPrincipalName t)
          )
      , dtTokenRequester  =
          ( extractText (DDTResp.describedDelegationTokenTokenRequesterPrincipalType t)
          , extractText (DDTResp.describedDelegationTokenTokenRequesterPrincipalName t)
          )
      , dtIssueTimestamp  = DDTResp.describedDelegationTokenIssueTimestamp t
      , dtExpiryTimestamp = DDTResp.describedDelegationTokenExpiryTimestamp t
      , dtMaxTimestamp    = DDTResp.describedDelegationTokenMaxTimestamp t
      }

----------------------------------------------------------------------
-- KRaft voter management (KIP-853)
----------------------------------------------------------------------

-- | A KRaft voter endpoint: a (listener-name, host, port)
-- triple. Mirrors @RaftVoterEndpoint@ in the JVM SDK.
data RaftVoterEndpoint = RaftVoterEndpoint
  { rveListenerName :: !Text
  , rveHost         :: !Text
  , rvePort         :: !Word16
  }
  deriving stock (Eq, Show)

-- | Add a voter node to the KRaft metadata quorum. Mirrors
-- @Admin.addRaftVoter(int, Uuid, Set<RaftVoterEndpoint>)@.
addRaftVoter
  :: MonadIO m
  => AdminClient
  -> Int32                                -- ^ voter id
  -> P.KafkaUuid                          -- ^ voter directory id
  -> [RaftVoterEndpoint]                  -- ^ endpoints
  -> m (Either String ())
addRaftVoter client vid vdid endpoints = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 80 0 0 0 $ \conn corrId apiVer -> do
        let !ls = V.fromList
              [ ARVReq.Listener
                  { ARVReq.listenerName = P.mkKafkaString (rveListenerName e)
                  , ARVReq.listenerHost = P.mkKafkaString (rveHost e)
                  , ARVReq.listenerPort = rvePort e
                  }
              | e <- endpoints
              ]
            req = ARVReq.AddRaftVoterRequest
              { ARVReq.addRaftVoterRequestClusterId =
                  P.mkKafkaString T.empty
              , ARVReq.addRaftVoterRequestTimeoutMs = 30000
              , ARVReq.addRaftVoterRequestVoterId = vid
              , ARVReq.addRaftVoterRequestVoterDirectoryId = vdid
              , ARVReq.addRaftVoterRequestListeners = P.mkKafkaArray ls
              }
            body = WC.runEncodeVer @ARVReq.AddRaftVoterRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 80 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AddRaftVoter request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @ARVResp.AddRaftVoterResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if ARVResp.addRaftVoterResponseErrorCode resp == 0
                  then pure (Right ())
                  else pure $ Left $ "AddRaftVoter: " <>
                    T.unpack (extractText (ARVResp.addRaftVoterResponseErrorMessage resp))

-- | Remove a voter node from the KRaft metadata quorum.
-- Mirrors @Admin.removeRaftVoter(int, Uuid)@.
removeRaftVoter
  :: MonadIO m
  => AdminClient
  -> Int32                                -- ^ voter id
  -> P.KafkaUuid                          -- ^ voter directory id
  -> m (Either String ())
removeRaftVoter client vid vdid = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 81 0 0 0 $ \conn corrId apiVer -> do
        let req = RRVReq.RemoveRaftVoterRequest
              { RRVReq.removeRaftVoterRequestClusterId = P.mkKafkaString T.empty
              , RRVReq.removeRaftVoterRequestVoterId = vid
              , RRVReq.removeRaftVoterRequestVoterDirectoryId = vdid
              }
            body = WC.runEncodeVer @RRVReq.RemoveRaftVoterRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 81 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("RemoveRaftVoter request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @RRVResp.RemoveRaftVoterResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if RRVResp.removeRaftVoterResponseErrorCode resp == 0
                  then pure (Right ())
                  else pure $ Left $ "RemoveRaftVoter: " <>
                    T.unpack (extractText (RRVResp.removeRaftVoterResponseErrorMessage resp))

----------------------------------------------------------------------
-- KRaft quorum description
----------------------------------------------------------------------

-- | A description of a KRaft replica's state.
data ReplicaState = ReplicaState
  { rsReplicaId             :: !Int32
  , rsLogEndOffset          :: !Int64
  , rsLastFetchTimestamp    :: !Int64
  , rsLastCaughtUpTimestamp :: !Int64
  }
  deriving stock (Eq, Show)

-- | A description of a single quorum partition.
data PartitionQuorumInfo = PartitionQuorumInfo
  { pqiPartition     :: !Int32
  , pqiLeaderId      :: !Int32
  , pqiLeaderEpoch   :: !Int32
  , pqiHighWatermark :: !Int64
  , pqiVoters        :: ![ReplicaState]
  , pqiObservers     :: ![ReplicaState]
  }
  deriving stock (Eq, Show)

-- | A snapshot of the KRaft metadata quorum.
data QuorumInfo = QuorumInfo
  { qiPartitions :: ![(Text, [PartitionQuorumInfo])]
  }
  deriving stock (Eq, Show)

-- | Describe the metadata quorum. Mirrors
-- @Admin.describeMetadataQuorum()@.
describeMetadataQuorum
  :: MonadIO m
  => AdminClient
  -> m (Either String QuorumInfo)
describeMetadataQuorum client = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 55 0 2 0 $ \conn corrId apiVer -> do
        -- Empty topics array asks for every topic the broker
        -- knows about; the JVM sends an explicit selector by
        -- default but the empty-list shape is the cheapest.
        let req = DQReq.DescribeQuorumRequest
              { DQReq.describeQuorumRequestTopics = P.mkKafkaArray V.empty
              }
            body = WC.runEncodeVer @DQReq.DescribeQuorumRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 55 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeQuorum request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DQResp.DescribeQuorumResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let ts = case P.unKafkaArray (DQResp.describeQuorumResponseTopics resp) of
                      P.Null      -> V.empty
                      P.NotNull v -> v
                pure $ Right (QuorumInfo (V.toList (V.map decodeT ts)))
  where
    decodeT t =
      let ps = case P.unKafkaArray (DQResp.topicDataPartitions t) of
            P.Null      -> V.empty
            P.NotNull v -> v
       in (extractText (DQResp.topicDataTopicName t), V.toList (V.map decodeP ps))
    decodeP p = PartitionQuorumInfo
      { pqiPartition     = DQResp.partitionDataPartitionIndex p
      , pqiLeaderId      = DQResp.partitionDataLeaderId p
      , pqiLeaderEpoch   = DQResp.partitionDataLeaderEpoch p
      , pqiHighWatermark = DQResp.partitionDataHighWatermark p
      , pqiVoters        = decodeReps (DQResp.partitionDataCurrentVoters p)
      , pqiObservers     = decodeReps (DQResp.partitionDataObservers p)
      }
    decodeReps arr = case P.unKafkaArray arr of
      P.Null      -> []
      P.NotNull v -> V.toList (V.map decodeR v)
    decodeR r = ReplicaState
      { rsReplicaId             = DQResp.replicaStateReplicaId r
      , rsLogEndOffset          = DQResp.replicaStateLogEndOffset r
      , rsLastFetchTimestamp    = DQResp.replicaStateLastFetchTimestamp r
      , rsLastCaughtUpTimestamp = DQResp.replicaStateLastCaughtUpTimestamp r
      }

----------------------------------------------------------------------
-- Consumer-group member removal (KIP-345)
----------------------------------------------------------------------

-- | A member to remove from a consumer group. The static
-- 'mtrGroupInstanceId' is the KIP-345 stable identifier;
-- 'mtrMemberId' is the dynamic id assigned by the broker on
-- join. At least one must be set.
data MemberToRemove = MemberToRemove
  { mtrMemberId        :: !(Maybe Text)
  , mtrGroupInstanceId :: !(Maybe Text)
  , mtrReason          :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | Force members out of a consumer group. Mirrors
-- @Admin.removeMembersFromConsumerGroup(String, RemoveMembersFromConsumerGroupOptions)@.
-- Routes through the LeaveGroup RPC; returns a per-member
-- result.
removeMembersFromConsumerGroup
  :: MonadIO m
  => AdminClient
  -> Text                                 -- ^ group id
  -> [MemberToRemove]
  -> m (Either String [(Text, Either String ())])
removeMembersFromConsumerGroup client groupId members = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 13 3 5 3 $ \conn corrId apiVer -> do
        let !ms = V.fromList
              [ LGRReq.MemberIdentity
                  { LGRReq.memberIdentityMemberId =
                      P.mkKafkaString (maybe T.empty id (mtrMemberId m))
                  , LGRReq.memberIdentityGroupInstanceId =
                      P.mkKafkaString (maybe T.empty id (mtrGroupInstanceId m))
                  , LGRReq.memberIdentityReason =
                      P.mkKafkaString (maybe T.empty id (mtrReason m))
                  }
              | m <- members
              ]
            req = LGRReq.LeaveGroupRequest
              { LGRReq.leaveGroupRequestGroupId = P.mkKafkaString groupId
              , LGRReq.leaveGroupRequestMemberId = P.mkKafkaString T.empty
              , LGRReq.leaveGroupRequestMembers = P.mkKafkaArray ms
              }
            body = WC.runEncodeVer @LGRReq.LeaveGroupRequest apiVer req
            cid  = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 13 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("LeaveGroup request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @LGRResp.LeaveGroupResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if LGRResp.leaveGroupResponseErrorCode resp /= 0
                  then pure $ Left $ "LeaveGroup: error code "
                    <> show (LGRResp.leaveGroupResponseErrorCode resp)
                  else do
                    let ms_ = case P.unKafkaArray (LGRResp.leaveGroupResponseMembers resp) of
                          P.Null      -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeM ms_))
  where
    decodeM m =
      let !mid  = extractText (LGRResp.memberResponseMemberId m)
          !code = LGRResp.memberResponseErrorCode m
       in if code == 0
            then (mid, Right ())
            else (mid, Left ("Error " <> show code))

