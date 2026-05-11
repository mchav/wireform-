{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.AdminTimeouts
Description : KIP-540 / KIP-918 / KIP-919 — AdminClient timeouts + KRaft routing

KIP-540 added a unified @timeout.ms@ to every AdminClient
operation (the JVM client previously honoured only
@request.timeout.ms@ + @retry.backoff.ms@ but had no overall
deadline). KIP-918 / KIP-919 added the ability to direct
operations explicitly at the KRaft controller quorum
(@controller.quorum.bootstrap.servers@) when the operation
modifies controller-only state (broker registration, KRaft
voter changes, dynamic broker config).

This module is the pure decision layer: it computes the
effective timeout / retry plan for an admin operation given
the global config + per-call override, and decides which broker
endpoint (random metadata broker vs. KRaft controller) the
operation should target.
-}
module Kafka.Client.AdminTimeouts
  ( -- * Per-operation timeout
    AdminCallTimeout (..)
  , effectiveDeadlineMs
    -- * Routing
  , AdminRouting (..)
  , routeOperation
  , AdminOperationKind (..)
  ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

-- | A per-call timeout request. The JVM client lets users
-- supply @TimeoutOption@ on every operation; the value here
-- mirrors that.
data AdminCallTimeout
  = -- | Use the AdminClient default (@default.api.timeout.ms@).
    AdminUseDefault
  | -- | Hard upper bound in ms.
    AdminTimeoutMs !Int
  | -- | No deadline (block forever; not recommended).
    AdminNoDeadline
  deriving stock (Eq, Show, Generic)

-- | Compute the wall-clock-ms deadline for a single AdminClient
-- call. Returns 'Nothing' for unbounded calls.
effectiveDeadlineMs
  :: Int64                -- ^ now (ms)
  -> Int                  -- ^ default.api.timeout.ms
  -> AdminCallTimeout
  -> Maybe Int64
effectiveDeadlineMs now defaultMs = \case
  AdminUseDefault   -> Just (now + fromIntegral defaultMs)
  AdminTimeoutMs n  -> Just (now + fromIntegral n)
  AdminNoDeadline   -> Nothing

-- | Where should an operation go? Mirrors the JVM client's
-- @AdminClient.RoutingTarget@ enum (KIP-918).
data AdminRouting
  = -- | Any cluster broker with metadata (the default for read
    --   operations).
    RouteAnyBroker
  | -- | The cluster controller broker (still on a regular broker
    --   port; needed for ACL / topic create when the broker hasn't
    --   migrated to KRaft yet).
    RouteControllerBroker
  | -- | The KRaft controller quorum
    --   (@controller.quorum.bootstrap.servers@). Used by
    --   broker-registration / quorum-management RPCs after
    --   KRaft cutover.
    RouteKRaftQuorum
  deriving stock (Eq, Show, Generic)

-- | The class of an Admin operation, for routing purposes.
data AdminOperationKind
  = AdminMetadataRead         -- ^ DescribeTopics, ListGroups, …
  | AdminTopicMutation        -- ^ CreateTopics, DeleteTopics, …
  | AdminConfigMutation       -- ^ AlterConfigs, IncrementalAlterConfigs
  | AdminAclMutation          -- ^ Create/DeleteAcls
  | AdminBrokerLifecycle      -- ^ BrokerHeartbeat, BrokerRegistration,
                              --   UnregisterBroker
  | AdminQuorumManagement     -- ^ AddRaftVoter, RemoveRaftVoter,
                              --   UpdateRaftVoter, DescribeQuorum
  deriving stock (Eq, Show, Generic)

-- | Pick the routing target for a given operation. Mirrors the
-- JVM client's @AdminClient.routeRequest@ heuristic.
routeOperation :: AdminOperationKind -> AdminRouting
routeOperation = \case
  AdminMetadataRead     -> RouteAnyBroker
  AdminTopicMutation    -> RouteControllerBroker
  AdminConfigMutation   -> RouteControllerBroker
  AdminAclMutation      -> RouteControllerBroker
  AdminBrokerLifecycle  -> RouteKRaftQuorum
  AdminQuorumManagement -> RouteKRaftQuorum
