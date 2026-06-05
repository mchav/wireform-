{-|
Module      : Kafka.Consumer.Types
Description : @hw-kafka-client@ consumer types backed by wireform handles.

This module mirrors the public consumer data types from
@hw-kafka-client@. It exists for transitional source compatibility;
new code should prefer the native "Kafka.Client.Consumer" records and
offset types.
-}
module Kafka.Consumer.Types
  ( KafkaConsumer (..)
  , ConsumerGroupId (..)
  , Offset (..)
  , OffsetReset (..)
  , RebalanceEvent (..)
  , PartitionOffset (..)
  , SubscribedPartitions (..)
  , Timestamp (..)
  , OffsetCommit (..)
  , OffsetStoreSync (..)
  , OffsetStoreMethod (..)
  , TopicPartition (..)
  , ConsumerRecord (..)
  , crMapKey
  , crMapValue
  , crMapKV
  , sequenceFirst
  , traverseFirst
  , traverseFirstM
  , traverseM
  , bitraverseM
  ) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..), bisequenceA)
import Data.Int (Int64)
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Kafka.Internal.Compat
  ( HasKafka (..)
  , HasKafkaConf (..)
  , Kafka (..)
  , KafkaConf (..)
  )
import Kafka.Types (Headers, Millis (..), PartitionId (..), TopicName (..))

-- | Main type for Kafka consumption.
--
-- Use 'Kafka.Consumer.newConsumer' to acquire this handle. The fields
-- wrap native wireform consumer state rather than librdkafka pointers.
data KafkaConsumer = KafkaConsumer
  { kcKafkaPtr :: !Kafka
    -- ^ Opaque compatibility handle for the underlying consumer.
  , kcKafkaConf :: !KafkaConf
    -- ^ Compatibility copy of Kafka-level properties.
  }

instance HasKafka KafkaConsumer where
  getKafka = kcKafkaPtr

instance HasKafkaConf KafkaConsumer where
  getKafkaConf = kcKafkaConf

-- | Consumer group ID.
--
-- Different consumers with the same group ID get assigned different
-- partitions of each subscribed topic.
newtype ConsumerGroupId = ConsumerGroupId
  { unConsumerGroupId :: Text
  } deriving (Show, Ord, Eq, IsString, Generic)

-- | A message offset in a partition.
newtype Offset = Offset { unOffset :: Int64 }
  deriving (Show, Eq, Ord, Read, Generic)

-- | Where to reset the offset when there is no initial offset in Kafka.
data OffsetReset
  = Earliest
  | Latest
  deriving (Show, Eq, Generic)

-- | Rebalancing lifecycle events from the legacy callback API.
data RebalanceEvent
  = RebalanceBeforeAssign [(TopicName, PartitionId)]
  | RebalanceAssign [(TopicName, PartitionId)]
  | RebalanceBeforeRevoke [(TopicName, PartitionId)]
  | RebalanceRevoke [(TopicName, PartitionId)]
  deriving (Eq, Show, Generic)

-- | Partition offset selector.
data PartitionOffset
  = PartitionOffsetBeginning
    -- ^ Start at the beginning of the partition.
  | PartitionOffsetEnd
    -- ^ Start at the end of the partition.
  | PartitionOffset Int64
    -- ^ Start at an explicit offset.
  | PartitionOffsetStored
    -- ^ Use the stored offset when available.
  | PartitionOffsetInvalid
    -- ^ Invalid or unknown offset.
  deriving (Eq, Show, Generic)

-- | Partitions subscribed by a consumer.
data SubscribedPartitions
  = SubscribedPartitions [PartitionId]
    -- ^ Subscribe only to these partitions.
  | SubscribedPartitionsAll
    -- ^ Subscribe to all partitions.
  deriving (Show, Eq, Generic)

-- | Consumer record timestamp.
data Timestamp
  = CreateTime !Millis
    -- ^ Timestamp set by the producer.
  | LogAppendTime !Millis
    -- ^ Timestamp set by the broker.
  | NoTimestamp
    -- ^ No timestamp is available.
  deriving (Show, Eq, Read, Generic)

-- | Offset commit mode.
data OffsetCommit
  = OffsetCommit
    -- ^ Block until broker offset commit is done.
  | OffsetCommitAsync
    -- ^ Commit offsets without blocking for the broker response.
  deriving (Show, Eq, Generic)

-- | Indicates how offsets are to be synced to disk in legacy code.
data OffsetStoreSync
  = OffsetSyncDisable
  | OffsetSyncImmediate
  | OffsetSyncInterval Int
  deriving (Show, Eq, Generic)

-- | Indicates the method of storing offsets in legacy code.
data OffsetStoreMethod
  = OffsetStoreBroker
  | OffsetStoreFile FilePath OffsetStoreSync
  deriving (Show, Eq, Generic)

-- | Kafka topic partition structure.
data TopicPartition = TopicPartition
  { tpTopicName :: TopicName
    -- ^ Topic name.
  , tpPartition :: PartitionId
    -- ^ Partition ID.
  , tpOffset :: PartitionOffset
    -- ^ Offset selector or concrete offset.
  } deriving (Show, Eq, Generic)

-- | Represents a /received/ message from Kafka.
data ConsumerRecord k v = ConsumerRecord
  { crTopic :: !TopicName
    -- ^ Kafka topic this message was received from.
  , crPartition :: !PartitionId
    -- ^ Kafka partition this message was received from.
  , crOffset :: !Offset
    -- ^ Offset within 'crPartition'.
  , crTimestamp :: !Timestamp
    -- ^ Message timestamp.
  , crHeaders :: !Headers
    -- ^ Message headers.
  , crKey :: !k
    -- ^ Message key.
  , crValue :: !v
    -- ^ Message value.
  } deriving (Eq, Show, Read, Typeable, Generic)

instance Bifunctor ConsumerRecord where
  bimap f g (ConsumerRecord t p o ts hds k v) =
    ConsumerRecord t p o ts hds (f k) (g v)

instance Functor (ConsumerRecord k) where
  fmap = second

instance Foldable (ConsumerRecord k) where
  foldMap f r = f (crValue r)

instance Traversable (ConsumerRecord k) where
  traverse f r = (\v -> crMapValue (const v) r) <$> f (crValue r)

instance Bifoldable ConsumerRecord where
  bifoldMap f g r = f (crKey r) <> g (crValue r)

instance Bitraversable ConsumerRecord where
  bitraverse f g r =
    (\k v -> bimap (const k) (const v) r) <$> f (crKey r) <*> g (crValue r)

{-# DEPRECATED crMapKey "Isn't concern of this library. Use 'first'" #-}
crMapKey :: (k -> k') -> ConsumerRecord k v -> ConsumerRecord k' v
crMapKey = first

{-# DEPRECATED crMapValue "Isn't concern of this library. Use 'second'" #-}
crMapValue :: (v -> v') -> ConsumerRecord k v -> ConsumerRecord k v'
crMapValue = second

{-# DEPRECATED crMapKV "Isn't concern of this library. Use 'bimap'" #-}
crMapKV :: (k -> k') -> (v -> v') -> ConsumerRecord k v -> ConsumerRecord k' v'
crMapKV = bimap

{-# DEPRECATED sequenceFirst "Isn't concern of this library. Use 'bitraverse' 'id' 'pure'" #-}
sequenceFirst :: (Bitraversable t, Applicative f) => t (f k) v -> f (t k v)
sequenceFirst = bitraverse id pure

{-# DEPRECATED traverseFirst "Isn't concern of this library. Use 'bitraverse' f 'pure'" #-}
traverseFirst
  :: (Bitraversable t, Applicative f)
  => (k -> f k')
  -> t k v
  -> f (t k' v)
traverseFirst f = bitraverse f pure

{-# DEPRECATED traverseFirstM "Isn't concern of this library. Use bitraverse directly" #-}
traverseFirstM
  :: (Bitraversable t, Applicative f, Monad m)
  => (k -> m (f k'))
  -> t k v
  -> m (f (t k' v))
traverseFirstM f r = bitraverse id pure <$> bitraverse f pure r

{-# DEPRECATED traverseM "Isn't concern of this library. Use sequenceA with traverse" #-}
traverseM
  :: (Traversable t, Applicative f, Monad m)
  => (v -> m (f v'))
  -> t v
  -> m (f (t v'))
traverseM f r = sequenceA <$> traverse f r

{-# DEPRECATED bitraverseM "Isn't concern of this library. Use bitraverse directly" #-}
bitraverseM
  :: (Bitraversable t, Applicative f, Monad m)
  => (k -> m (f k'))
  -> (v -> m (f v'))
  -> t k v
  -> m (f (t k' v'))
bitraverseM f g r = bisequenceA <$> bitraverse f g r
