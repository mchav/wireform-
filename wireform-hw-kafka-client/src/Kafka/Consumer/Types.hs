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
import Data.Bitraversable (Bitraversable (..))
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

data KafkaConsumer = KafkaConsumer
  { kcKafkaPtr :: !Kafka
  , kcKafkaConf :: !KafkaConf
  }

instance HasKafka KafkaConsumer where
  getKafka = kcKafkaPtr

instance HasKafkaConf KafkaConsumer where
  getKafkaConf = kcKafkaConf

newtype ConsumerGroupId = ConsumerGroupId
  { unConsumerGroupId :: Text
  } deriving (Show, Ord, Eq, IsString, Generic)

newtype Offset = Offset { unOffset :: Int64 }
  deriving (Show, Eq, Ord, Read, Generic)

data OffsetReset
  = Earliest
  | Latest
  deriving (Show, Eq, Generic)

data RebalanceEvent
  = RebalanceBeforeAssign [(TopicName, PartitionId)]
  | RebalanceAssign [(TopicName, PartitionId)]
  | RebalanceBeforeRevoke [(TopicName, PartitionId)]
  | RebalanceRevoke [(TopicName, PartitionId)]
  deriving (Eq, Show, Generic)

data PartitionOffset
  = PartitionOffsetBeginning
  | PartitionOffsetEnd
  | PartitionOffset Int64
  | PartitionOffsetStored
  | PartitionOffsetInvalid
  deriving (Eq, Show, Generic)

data SubscribedPartitions
  = SubscribedPartitions [PartitionId]
  | SubscribedPartitionsAll
  deriving (Show, Eq, Generic)

data Timestamp
  = CreateTime !Millis
  | LogAppendTime !Millis
  | NoTimestamp
  deriving (Show, Eq, Read, Generic)

data OffsetCommit
  = OffsetCommit
  | OffsetCommitAsync
  deriving (Show, Eq, Generic)

data OffsetStoreSync
  = OffsetSyncDisable
  | OffsetSyncImmediate
  | OffsetSyncInterval Int
  deriving (Show, Eq, Generic)

data OffsetStoreMethod
  = OffsetStoreBroker
  | OffsetStoreFile FilePath OffsetStoreSync
  deriving (Show, Eq, Generic)

data TopicPartition = TopicPartition
  { tpTopicName :: TopicName
  , tpPartition :: PartitionId
  , tpOffset :: PartitionOffset
  } deriving (Show, Eq, Generic)

data ConsumerRecord k v = ConsumerRecord
  { crTopic :: !TopicName
  , crPartition :: !PartitionId
  , crOffset :: !Offset
  , crTimestamp :: !Timestamp
  , crHeaders :: !Headers
  , crKey :: !k
  , crValue :: !v
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
