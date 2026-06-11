{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Common
Description : Shared value types from @org.apache.kafka.common@

The Java SDK's @org.apache.kafka.common@ package is a grab-bag of
value types that show up in admin / consumer / metadata responses
(e.g. @Node@, @Cluster@, @TopicPartitionInfo@, @MetricName@,
@GroupState@). The Haskell client either re-derived them under
different module names or didn't carry a public version because
the corresponding admin RPC wasn't wrapped yet.

This module declares them in a single place so:

  1. Future admin-RPC wrappers (e.g. @describeCluster@) can return
     these typed records without inventing new shapes.
  2. Cross-package consumers (e.g. JVM-portability shims) can
     import them by exactly the name the Javadoc uses.

The types are intentionally declarative — they don't carry any
behaviour, just the same fields the JVM does.
-}
module Kafka.Common (
  -- * Node
  Node (..),

  -- * Endpoint
  Endpoint (..),

  -- * Cluster
  Cluster (..),
  emptyCluster,

  -- * Cluster resource
  ClusterResource (..),

  -- * Topic / partition
  PartitionInfo (..),
  TopicPartitionInfo (..),
  TopicPartitionReplica (..),
  TopicIdPartition (..),

  -- * Group state
  GroupState (..),
  ClassicGroupState (..),
  GroupType (..),

  -- * Metric name
  MetricName (..),
  MetricNameTemplate (..),

  -- * UUID / Topic id
  Uuid,
  uuidZero,
) where

import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Kafka.Client.TopicId qualified as TopicId


----------------------------------------------------------------------
-- Node
----------------------------------------------------------------------

{- | Information about a Kafka broker / KRaft voter node. Mirrors
@org.apache.kafka.common.Node@.
-}
data Node = Node
  { nodeId :: !Int
  , nodeHost :: !Text
  , nodePort :: !Int
  , nodeRack :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)


----------------------------------------------------------------------
-- Endpoint
----------------------------------------------------------------------

{- | Broker / controller endpoint. Mirrors
@org.apache.kafka.common.Endpoint@. Used by @addRaftVoter@ and
@describeCluster@-style admin RPCs.
-}
data Endpoint = Endpoint
  { endpointListenerName :: !Text
  , endpointHost :: !Text
  , endpointPort :: !Int
  , endpointSecurityProtocol :: !Text
  {- ^ One of @PLAINTEXT@ / @SSL@ / @SASL_PLAINTEXT@ /
  @SASL_SSL@. Kept as a 'Text' so this module doesn't need to
  duplicate the security-protocol enum.
  -}
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)


----------------------------------------------------------------------
-- Cluster
----------------------------------------------------------------------

{- | An immutable snapshot of a subset of nodes, topics, and
partitions in the cluster. Mirrors
@org.apache.kafka.common.Cluster@.

We use simple 'Map' / list shapes rather than the Java
@ClusterRef@ accessor pattern — the values are read-only.
-}
data Cluster = Cluster
  { clusterId :: !(Maybe Text)
  , clusterNodes :: ![Node]
  , clusterController :: !(Maybe Node)
  , clusterTopics :: !(Map Text [PartitionInfo])
  , clusterAuthorizedOperations :: ![Text]
  {- ^ Names of cluster-level operations the requesting principal
  is authorized for (e.g. @\"CREATE\"@, @\"ALTER\"@). When the
  broker didn't include this list the map is empty.
  -}
  }
  deriving stock (Eq, Show, Generic)


emptyCluster :: Cluster
emptyCluster =
  Cluster
    { clusterId = Nothing
    , clusterNodes = []
    , clusterController = Nothing
    , clusterTopics = Map.empty
    , clusterAuthorizedOperations = []
    }


----------------------------------------------------------------------
-- ClusterResource
----------------------------------------------------------------------

{- | Cluster-resource metadata. Mirrors
@org.apache.kafka.common.ClusterResource@. Only carries the
cluster id today; the Java type historically also carried the
bootstrap broker list but that's now part of 'Cluster'.
-}
newtype ClusterResource = ClusterResource
  { clusterResourceId :: Text
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)


----------------------------------------------------------------------
-- Partition info
----------------------------------------------------------------------

{- | Lightweight partition metadata. Mirrors
@org.apache.kafka.common.PartitionInfo@. Each replica is just the
broker id.
-}
data PartitionInfo = PartitionInfo
  { piTopic :: !Text
  , piPartition :: !Int32
  , piLeader :: !(Maybe Int)
  , piReplicas :: ![Int]
  , piIsr :: ![Int]
  , piOfflineReplicas :: ![Int]
  }
  deriving stock (Eq, Show, Generic)


{- | Richer partition description used by admin describe-topics.
Mirrors @org.apache.kafka.common.TopicPartitionInfo@.
-}
data TopicPartitionInfo = TopicPartitionInfo
  { tpiPartition :: !Int32
  , tpiLeader :: !(Maybe Node)
  , tpiReplicas :: ![Node]
  , tpiIsr :: ![Node]
  }
  deriving stock (Eq, Show, Generic)


{- | A topic / partition / broker triple. Mirrors
@org.apache.kafka.common.TopicPartitionReplica@. Used by
@describeReplicaLogDirs@.
-}
data TopicPartitionReplica = TopicPartitionReplica
  { tprTopic :: !Text
  , tprPartition :: !Int32
  , tprBrokerId :: !Int
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)


{- | A topic id paired with a partition number. Mirrors
@org.apache.kafka.common.TopicIdPartition@. The Java type also
carries the topic name; we make it optional because some uses
(e.g. ZooKeeper-era responses) don't include the name.
-}
data TopicIdPartition = TopicIdPartition
  { tipTopicId :: !Uuid
  , tipPartition :: !Int32
  , tipTopicName :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- Group state
----------------------------------------------------------------------

{- | The state of a consumer group. Mirrors
@org.apache.kafka.common.GroupState@ (KIP-848-era generic-group
vocabulary). The classic-group equivalent is 'ClassicGroupState'.
-}
data GroupState
  = GroupUnknownState
  | GroupAssigning
  | GroupReconciling
  | GroupStable
  | GroupDead
  | GroupEmpty
  deriving stock (Eq, Show, Generic)


{- | The state of a classic (pre-KIP-848) consumer group. Mirrors
@org.apache.kafka.common.ClassicGroupState@.
-}
data ClassicGroupState
  = CGUnknown
  | CGPreparingRebalance
  | CGCompletingRebalance
  | CGStable
  | CGDead
  | CGEmpty
  deriving stock (Eq, Show, Generic)


{- | The protocol family of a consumer group. Mirrors
@org.apache.kafka.common.GroupType@.
-}
data GroupType
  = ClassicGroup
  | ConsumerGroup
  | ShareGroup
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- Metric name
----------------------------------------------------------------------

{- | Metric identity: a name, a logical group, and a set of tags.
Mirrors @org.apache.kafka.common.MetricName@. The Haskell metrics
registry ('Kafka.Telemetry.Metrics') stores metrics under flat
string keys today; this richer shape is exposed for future
integration with Java-style reporters.
-}
data MetricName = MetricName
  { mnName :: !Text
  , mnGroup :: !Text
  , mnDescription :: !Text
  , mnTags :: ![(Text, Text)]
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Hashable)


{- | A MetricName template — same as 'MetricName' but the tag
values are unbound. Mirrors @MetricNameTemplate@.
-}
data MetricNameTemplate = MetricNameTemplate
  { mntName :: !Text
  , mntGroup :: !Text
  , mntDescription :: !Text
  , mntTagKeys :: ![Text]
  }
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- UUID / Topic id
----------------------------------------------------------------------

{- | Kafka's universally-unique identifier type — a 128-bit value
presented as an RFC 4122 UUID on the wire. Mirrors
@org.apache.kafka.common.Uuid@. We alias to the existing
'TopicId.TopicId' wrapper since the wire shape is identical.
-}
type Uuid = TopicId.TopicId


-- | The all-zero UUID Kafka uses as a sentinel.
uuidZero :: Uuid
uuidZero = TopicId.nullTopicId
