{- |
Module      : Kafka.Metadata
Description : Legacy metadata types for @hw-kafka-client@ migration.

This module ports the public metadata data types from @hw-kafka-client@
so imports continue to compile during migration. The old functions were
librdkafka metadata queries; in the native wireform stack callers should
use "Kafka.Client.AdminClient" for real metadata, group, topic, and
offset discovery.
-}
module Kafka.Metadata (
  KafkaMetadata (..),
  BrokerMetadata (..),
  TopicMetadata (..),
  PartitionMetadata (..),
  WatermarkOffsets (..),
  GroupMemberId (..),
  GroupMemberInfo (..),
  GroupProtocolType (..),
  GroupProtocol (..),
  GroupState (..),
  GroupInfo (..),
  allTopicsMetadata,
  topicMetadata,
  watermarkOffsets,
  watermarkOffsets',
  partitionWatermarkOffsets,
  offsetsForTime,
  offsetsForTime',
  topicOffsetsForTime,
  allConsumerGroupsInfo,
  consumerGroupInfo,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kafka.Consumer.Types (
  ConsumerGroupId (..),
  Offset (..),
  PartitionOffset (..),
  TopicPartition (..),
 )
import Kafka.Internal.Compat (HasKafka)
import Kafka.Types (
  BrokerId (..),
  ClientId (..),
  KafkaError (..),
  Millis (..),
  PartitionId (..),
  Timeout (..),
  TopicName (..),
 )


-- | Broker and topic metadata returned by the legacy API.
data KafkaMetadata = KafkaMetadata
  { kmBrokers :: [BrokerMetadata]
  , kmTopics :: [TopicMetadata]
  , kmOrigBroker :: !BrokerId
  }
  deriving (Show, Eq, Generic)


-- | Metadata for one broker.
data BrokerMetadata = BrokerMetadata
  { bmBrokerId :: !BrokerId
  , bmBrokerHost :: !Text
  , bmBrokerPort :: !Int
  }
  deriving (Show, Eq, Generic)


-- | Metadata for one topic partition.
data PartitionMetadata = PartitionMetadata
  { pmPartitionId :: !PartitionId
  , pmError :: Maybe KafkaError
  , pmLeader :: !BrokerId
  , pmReplicas :: [BrokerId]
  , pmInSyncReplicas :: [BrokerId]
  }
  deriving (Show, Eq, Generic)


-- | Metadata for one topic.
data TopicMetadata = TopicMetadata
  { tmTopicName :: !TopicName
  , tmPartitions :: [PartitionMetadata]
  , tmError :: Maybe KafkaError
  }
  deriving (Show, Eq, Generic)


-- | Low and high watermark offsets for a topic partition.
data WatermarkOffsets = WatermarkOffsets
  { woTopicName :: !TopicName
  , woPartitionId :: !PartitionId
  , woLowWatermark :: !Offset
  , woHighWatermark :: !Offset
  }
  deriving (Show, Eq, Generic)


-- | Consumer group member ID.
newtype GroupMemberId = GroupMemberId Text
  deriving (Show, Eq, Read, Ord)


-- | Legacy consumer group member description.
data GroupMemberInfo = GroupMemberInfo
  { gmiMemberId :: !GroupMemberId
  , gmiClientId :: !ClientId
  , gmiClientHost :: !Text
  , gmiMetadata :: !ByteString
  , gmiAssignment :: !ByteString
  }
  deriving (Show, Eq, Generic)


-- | Consumer group protocol type.
newtype GroupProtocolType = GroupProtocolType Text
  deriving (Show, Eq, Read, Ord, Generic)


-- | Consumer group protocol.
newtype GroupProtocol = GroupProtocol Text
  deriving (Show, Eq, Read, Ord, Generic)


-- | Legacy consumer group state.
data GroupState
  = GroupPreparingRebalance
  | GroupEmpty
  | GroupAwaitingSync
  | GroupStable
  | GroupDead
  deriving (Show, Eq, Read, Ord, Generic)


-- | Legacy consumer group description.
data GroupInfo = GroupInfo
  { giGroup :: !ConsumerGroupId
  , giError :: Maybe KafkaError
  , giState :: !GroupState
  , giProtocolType :: !GroupProtocolType
  , giProtocol :: !GroupProtocol
  , giMembers :: [GroupMemberInfo]
  }
  deriving (Show, Eq, Generic)


{- | Return metadata for all topics.

Compatibility stub: use "Kafka.Client.AdminClient" for native metadata.
-}
allTopicsMetadata :: (MonadIO m, HasKafka k) => k -> Timeout -> m (Either KafkaError KafkaMetadata)
allTopicsMetadata _ _ = pure (Left metadataUnsupported)


-- | Return metadata for one topic.
topicMetadata :: (MonadIO m, HasKafka k) => k -> Timeout -> TopicName -> m (Either KafkaError KafkaMetadata)
topicMetadata _ _ _ = pure (Left metadataUnsupported)


-- | Query low and high offsets for all partitions of a topic.
watermarkOffsets :: (MonadIO m, HasKafka k) => k -> Timeout -> TopicName -> m [Either KafkaError WatermarkOffsets]
watermarkOffsets _ _ _ = pure [Left metadataUnsupported]


-- | Query low and high offsets using existing topic metadata.
watermarkOffsets' :: (MonadIO m, HasKafka k) => k -> Timeout -> TopicMetadata -> m [Either KafkaError WatermarkOffsets]
watermarkOffsets' _ _ _ = pure [Left metadataUnsupported]


-- | Query low and high offsets for a single partition.
partitionWatermarkOffsets
  :: (MonadIO m, HasKafka k)
  => k
  -> Timeout
  -> TopicName
  -> PartitionId
  -> m (Either KafkaError WatermarkOffsets)
partitionWatermarkOffsets _ _ _ _ = pure (Left metadataUnsupported)


-- | Look up offsets for a topic by timestamp.
topicOffsetsForTime
  :: (MonadIO m, HasKafka k)
  => k
  -> Timeout
  -> Millis
  -> TopicName
  -> m (Either KafkaError [TopicPartition])
topicOffsetsForTime _ _ _ _ = pure (Left metadataUnsupported)


-- | Look up offsets for topic metadata by timestamp.
offsetsForTime'
  :: (MonadIO m, HasKafka k)
  => k
  -> Timeout
  -> Millis
  -> TopicMetadata
  -> m (Either KafkaError [TopicPartition])
offsetsForTime' _ _ _ _ = pure (Left metadataUnsupported)


-- | Look up offsets for topic partitions by timestamp.
offsetsForTime
  :: (MonadIO m, HasKafka k)
  => k
  -> Timeout
  -> Millis
  -> [(TopicName, PartitionId)]
  -> m (Either KafkaError [TopicPartition])
offsetsForTime _ _ _ _ = pure (Left metadataUnsupported)


-- | List and describe all consumer groups in the cluster.
allConsumerGroupsInfo :: (MonadIO m, HasKafka k) => k -> Timeout -> m (Either KafkaError [GroupInfo])
allConsumerGroupsInfo _ _ = pure (Left metadataUnsupported)


-- | Describe a specific consumer group.
consumerGroupInfo
  :: (MonadIO m, HasKafka k)
  => k
  -> Timeout
  -> ConsumerGroupId
  -> m (Either KafkaError [GroupInfo])
consumerGroupInfo _ _ _ = pure (Left metadataUnsupported)


metadataUnsupported :: KafkaError
metadataUnsupported =
  KafkaBadSpecification "Kafka.Metadata is present for source compatibility; use Kafka.Client.AdminClient for native wireform metadata"
