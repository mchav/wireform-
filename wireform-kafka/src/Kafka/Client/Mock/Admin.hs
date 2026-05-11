{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Client.Mock.Admin
-- Description : Mock admin-client view over a 'MockCluster'
--
-- Provides the high-level admin operations a Kafka admin client
-- exposes (createTopics / deleteTopics / describeTopic /
-- listConsumerGroups / etc.) so tests can exercise the
-- @rd_kafka_admin_*@ failure-mode equivalents (librdkafka 0138)
-- without touching the wire layer.
module Kafka.Client.Mock.Admin
  ( -- * Topic admin
    NewTopic (..)
  , defaultNewTopic
  , createTopicsAdmin
  , deleteTopicsAdmin
  , describeTopicAdmin
  , TopicDescription (..)
  , PartitionInfo (..)
    -- * Group admin
  , listConsumerGroupsAdmin
  , describeConsumerGroupAdmin
  , ConsumerGroupDescription (..)
    -- * Cluster admin
  , describeClusterAdmin
  , ClusterDescription (..)
    -- * Result types
  , AdminResult
  ) where

import Control.Monad (forM)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Kafka.Client.Mock.Cluster as C
import Kafka.Client.Mock.Cluster (BrokerId, MockCluster)
import Kafka.Client.Mock.Fault (MockError (..))

----------------------------------------------------------------------
-- AdminResult
----------------------------------------------------------------------

-- | A typed admin operation result. Mirrors the JVM client's
-- @KafkaFuture@ surface — Right on success, Left on the broker's
-- error response.
type AdminResult a = Either MockError a

----------------------------------------------------------------------
-- Topics
----------------------------------------------------------------------

data NewTopic = NewTopic
  { ntName        :: !Text
  , ntPartitions  :: !Int
  , ntReplication :: !Int
    -- ^ Replication factor. The mock doesn't actually replicate
    -- (it has a single in-process log) but rejects values larger
    -- than the broker count to mirror what a real broker does.
  }
  deriving stock (Eq, Show, Generic)

defaultNewTopic :: Text -> Int -> NewTopic
defaultNewTopic n p = NewTopic { ntName = n, ntPartitions = p, ntReplication = 1 }

-- | Create a batch of topics. Returns one result per topic in the
-- input order. Idempotent: if a topic already exists at the same
-- partition count, the result is 'Right ()'.
createTopicsAdmin
  :: MockCluster
  -> [NewTopic]
  -> IO [(Text, AdminResult ())]
createTopicsAdmin c nts = do
  brokers <- length <$> C.clusterBrokers c
  forM nts $ \nt -> do
    if ntReplication nt > brokers
      then pure (ntName nt, Left (ErrCustom "INVALID_REPLICATION_FACTOR"))
      else do
        existing <- C.partitionCount c (ntName nt)
        case existing of
          Just n
            | n == ntPartitions nt -> pure (ntName nt, Right ())
            | otherwise -> pure (ntName nt
                , Left (ErrCustom "TOPIC_ALREADY_EXISTS"))
          Nothing -> do
            C.createTopic c (ntName nt) (ntPartitions nt)
            pure (ntName nt, Right ())

-- | Delete a batch of topics; returns one result per topic.
-- Mirrors @AdminClient.deleteTopics@. Topics that didn't exist
-- come back as @Left UNKNOWN_TOPIC_OR_PARTITION@.
deleteTopicsAdmin
  :: MockCluster -> [Text] -> IO [(Text, AdminResult ())]
deleteTopicsAdmin c topics = forM topics $ \t -> do
  ok <- C.deleteTopic c t
  if ok
    then pure (t, Right ())
    else pure (t, Left ErrUnknownTopicOrPartition)

----------------------------------------------------------------------
-- Describe
----------------------------------------------------------------------

data PartitionInfo = PartitionInfo
  { piPartitionId :: !Int32
  , piLeader      :: !(Maybe BrokerId)
  , piHwm         :: !Int
  , piLso         :: !Int
  }
  deriving stock (Eq, Show, Generic)

data TopicDescription = TopicDescription
  { tdName       :: !Text
  , tdPartitions :: ![PartitionInfo]
  }
  deriving stock (Eq, Show, Generic)

describeTopicAdmin
  :: MockCluster -> Text -> IO (AdminResult TopicDescription)
describeTopicAdmin c topic = do
  mn <- C.partitionCount c topic
  case mn of
    Nothing -> pure (Left ErrUnknownTopicOrPartition)
    Just n  -> do
      parts <- forM [0 .. n - 1] $ \i -> do
        let pid = fromIntegral i :: Int32
        hwm <- C.partitionHWM c topic pid
        lso <- C.partitionLastStableOffset c topic pid
        pure PartitionInfo
          { piPartitionId = pid
          , piLeader      = Nothing
          , piHwm         = maybe 0 fromIntegral hwm
          , piLso         = maybe 0 fromIntegral lso
          }
      pure (Right TopicDescription
        { tdName       = topic
        , tdPartitions = parts
        })

----------------------------------------------------------------------
-- Groups
----------------------------------------------------------------------

data ConsumerGroupDescription = ConsumerGroupDescription
  { cgdGroupId :: !Text
  , cgdMembers :: ![Text]
  }
  deriving stock (Eq, Show, Generic)

listConsumerGroupsAdmin :: MockCluster -> IO (AdminResult [Text])
listConsumerGroupsAdmin c =
  Right . map (\(C.GroupId g) -> g) <$> C.knownGroups c

describeConsumerGroupAdmin
  :: MockCluster
  -> Text
  -> IO (AdminResult ConsumerGroupDescription)
describeConsumerGroupAdmin c gid = do
  members <- C.membersOf c (C.GroupId gid)
  pure $ Right ConsumerGroupDescription
    { cgdGroupId = gid
    , cgdMembers = map C.unMemberId members
    }

----------------------------------------------------------------------
-- Cluster
----------------------------------------------------------------------

data ClusterDescription = ClusterDescription
  { cdBrokerCount :: !Int
  , cdTopics      :: ![Text]
  , cdClusterId   :: !Text
  }
  deriving stock (Eq, Show, Generic)

-- | Mirrors @AdminClient.describeCluster@. The cluster id is
-- synthesised as @"mock-cluster"@ for tests; if you need a stable
-- value across runs pass it through your own helper.
describeClusterAdmin
  :: MockCluster -> IO (AdminResult ClusterDescription)
describeClusterAdmin c = do
  ts <- C.listTopics c
  bs <- C.clusterBrokers c
  pure $ Right ClusterDescription
    { cdBrokerCount = length bs
    , cdTopics      = ts
    , cdClusterId   = "mock-cluster"
    }

