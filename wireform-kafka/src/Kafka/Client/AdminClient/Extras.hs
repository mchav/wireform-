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
  ) where

import Control.Concurrent.STM (atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int8, Int16, Int32)
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

