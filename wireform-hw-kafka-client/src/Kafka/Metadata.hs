module Kafka.Metadata
  ( KafkaMetadata (..)
  , BrokerMetadata (..)
  , TopicMetadata (..)
  , PartitionMetadata (..)
  , WatermarkOffsets (..)
  , GroupMemberId (..)
  , GroupMemberInfo (..)
  , GroupProtocolType (..)
  , GroupProtocol (..)
  , GroupState (..)
  , GroupInfo (..)
  , allTopicsMetadata
  , topicMetadata
  , watermarkOffsets
  , watermarkOffsets'
  , partitionWatermarkOffsets
  , offsetsForTime
  , offsetsForTime'
  , topicOffsetsForTime
  , allConsumerGroupsInfo
  , consumerGroupInfo
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kafka.Consumer.Types
  ( ConsumerGroupId (..)
  , Offset (..)
  , PartitionOffset (..)
  , TopicPartition (..)
  )
import Kafka.Internal.Compat (HasKafka)
import Kafka.Types
  ( BrokerId (..)
  , ClientId (..)
  , KafkaError (..)
  , Millis (..)
  , PartitionId (..)
  , Timeout (..)
  , TopicName (..)
  )

data KafkaMetadata = KafkaMetadata
  { kmBrokers :: [BrokerMetadata]
  , kmTopics :: [TopicMetadata]
  , kmOrigBroker :: !BrokerId
  } deriving (Show, Eq, Generic)

data BrokerMetadata = BrokerMetadata
  { bmBrokerId :: !BrokerId
  , bmBrokerHost :: !Text
  , bmBrokerPort :: !Int
  } deriving (Show, Eq, Generic)

data PartitionMetadata = PartitionMetadata
  { pmPartitionId :: !PartitionId
  , pmError :: Maybe KafkaError
  , pmLeader :: !BrokerId
  , pmReplicas :: [BrokerId]
  , pmInSyncReplicas :: [BrokerId]
  } deriving (Show, Eq, Generic)

data TopicMetadata = TopicMetadata
  { tmTopicName :: !TopicName
  , tmPartitions :: [PartitionMetadata]
  , tmError :: Maybe KafkaError
  } deriving (Show, Eq, Generic)

data WatermarkOffsets = WatermarkOffsets
  { woTopicName :: !TopicName
  , woPartitionId :: !PartitionId
  , woLowWatermark :: !Offset
  , woHighWatermark :: !Offset
  } deriving (Show, Eq, Generic)

newtype GroupMemberId = GroupMemberId Text
  deriving (Show, Eq, Read, Ord)

data GroupMemberInfo = GroupMemberInfo
  { gmiMemberId :: !GroupMemberId
  , gmiClientId :: !ClientId
  , gmiClientHost :: !Text
  , gmiMetadata :: !ByteString
  , gmiAssignment :: !ByteString
  } deriving (Show, Eq, Generic)

newtype GroupProtocolType = GroupProtocolType Text
  deriving (Show, Eq, Read, Ord, Generic)

newtype GroupProtocol = GroupProtocol Text
  deriving (Show, Eq, Read, Ord, Generic)

data GroupState
  = GroupPreparingRebalance
  | GroupEmpty
  | GroupAwaitingSync
  | GroupStable
  | GroupDead
  deriving (Show, Eq, Read, Ord, Generic)

data GroupInfo = GroupInfo
  { giGroup :: !ConsumerGroupId
  , giError :: Maybe KafkaError
  , giState :: !GroupState
  , giProtocolType :: !GroupProtocolType
  , giProtocol :: !GroupProtocol
  , giMembers :: [GroupMemberInfo]
  } deriving (Show, Eq, Generic)

allTopicsMetadata :: (MonadIO m, HasKafka k) => k -> Timeout -> m (Either KafkaError KafkaMetadata)
allTopicsMetadata _ _ = pure (Left metadataUnsupported)

topicMetadata :: (MonadIO m, HasKafka k) => k -> Timeout -> TopicName -> m (Either KafkaError KafkaMetadata)
topicMetadata _ _ _ = pure (Left metadataUnsupported)

watermarkOffsets :: (MonadIO m, HasKafka k) => k -> Timeout -> TopicName -> m [Either KafkaError WatermarkOffsets]
watermarkOffsets _ _ _ = pure [Left metadataUnsupported]

watermarkOffsets' :: (MonadIO m, HasKafka k) => k -> Timeout -> TopicMetadata -> m [Either KafkaError WatermarkOffsets]
watermarkOffsets' _ _ _ = pure [Left metadataUnsupported]

partitionWatermarkOffsets
  :: (MonadIO m, HasKafka k)
  => k
  -> Timeout
  -> TopicName
  -> PartitionId
  -> m (Either KafkaError WatermarkOffsets)
partitionWatermarkOffsets _ _ _ _ = pure (Left metadataUnsupported)

topicOffsetsForTime
  :: (MonadIO m, HasKafka k)
  => k
  -> Timeout
  -> Millis
  -> TopicName
  -> m (Either KafkaError [TopicPartition])
topicOffsetsForTime _ _ _ _ = pure (Left metadataUnsupported)

offsetsForTime'
  :: (MonadIO m, HasKafka k)
  => k
  -> Timeout
  -> Millis
  -> TopicMetadata
  -> m (Either KafkaError [TopicPartition])
offsetsForTime' _ _ _ _ = pure (Left metadataUnsupported)

offsetsForTime
  :: (MonadIO m, HasKafka k)
  => k
  -> Timeout
  -> Millis
  -> [(TopicName, PartitionId)]
  -> m (Either KafkaError [TopicPartition])
offsetsForTime _ _ _ _ = pure (Left metadataUnsupported)

allConsumerGroupsInfo :: (MonadIO m, HasKafka k) => k -> Timeout -> m (Either KafkaError [GroupInfo])
allConsumerGroupsInfo _ _ = pure (Left metadataUnsupported)

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
