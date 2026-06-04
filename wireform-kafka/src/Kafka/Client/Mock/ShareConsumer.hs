{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Client.Mock.ShareConsumer
-- Description : In-memory KIP-932 share-group consumer over a MockCluster
--
-- This module gives tests a deterministic share-group surface without
-- a Kafka 4.x broker. It models the queue-specific parts of KIP-932:
-- record locks, redelivery after lock expiry, and Accept / Release /
-- Reject acknowledgements.
module Kafka.Client.Mock.ShareConsumer
  ( MockShareConsumer
  , newMockShareConsumer
  , pollShareMC
  , acknowledgeShareMC
  , commitAcknowledgementsMC
  , pendingAcknowledgementsMC
  ) where

import Control.Concurrent.STM
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Int (Int32, Int64)
import Data.Text (Text)

import Kafka.Client.Mock.Cluster
  ( GroupId
  , MockCluster
  , StoredRecord
  , clusterClockNow
  , fetchSlice
  , partitionCount
  , partitionHWM
  )
import qualified Kafka.Client.Mock.Cluster as Cluster
import Kafka.Client.ShareConsumer
  ( Acknowledgement (..)
  , AcknowledgementType (..)
  , ShareConsumerConfig (..)
  , ShareRecord (..)
  )

data ShareKey = ShareKey !Text !Int32 !Int64 !Int64
  deriving stock (Eq, Ord, Show)

data ShareLock = ShareLock
  { slExpiresAtMs :: !Int64
  , slDeliveryCount :: !Int32
  }
  deriving stock (Eq, Show)

data MockShareConsumer = MockShareConsumer
  { mscCluster :: !MockCluster
  , mscGroupId :: !GroupId
  , mscConfig :: !ShareConsumerConfig
  , mscLocks :: !(TVar (Map ShareKey ShareLock))
  , mscAttempts :: !(TVar (Map ShareKey Int32))
  , mscCompleted :: !(TVar (Set ShareKey))
  , mscPendingAcks :: !(TVar [Acknowledgement])
  }

newMockShareConsumer
  :: MockCluster
  -> GroupId
  -> ShareConsumerConfig
  -> IO MockShareConsumer
newMockShareConsumer cluster groupId cfg = do
  locks <- newTVarIO Map.empty
  attempts <- newTVarIO Map.empty
  completed <- newTVarIO Set.empty
  pending <- newTVarIO []
  pure MockShareConsumer
    { mscCluster = cluster
    , mscGroupId = groupId
    , mscConfig = cfg
    , mscLocks = locks
    , mscAttempts = attempts
    , mscCompleted = completed
    , mscPendingAcks = pending
    }

pollShareMC :: MockShareConsumer -> Int -> IO [ShareRecord]
pollShareMC sc requestedMax = do
  now <- clusterClockNow (mscCluster sc)
  tps <- topicPartitions (mscCluster sc) (scTopics (mscConfig sc))
  locks <- readTVarIO (mscLocks sc)
  attempts <- readTVarIO (mscAttempts sc)
  completed <- readTVarIO (mscCompleted sc)
  candidates <- foldM
    (\acc tp -> do
        rs <- fetchEligiblePartition sc now locks attempts completed tp
        pure (acc <> rs))
    []
    tps
  let !limit = max 0 (min requestedMax (scMaxFetchRecords (mscConfig sc)))
      !selected = take limit candidates
  atomically $
    lockDelivered now sc selected
  pure selected

acknowledgeShareMC :: MockShareConsumer -> Acknowledgement -> IO ()
acknowledgeShareMC sc ack =
  atomically $ modifyTVar' (mscPendingAcks sc) (ack :)

commitAcknowledgementsMC :: MockShareConsumer -> IO [Acknowledgement]
commitAcknowledgementsMC sc = atomically $ do
  acks <- readTVar (mscPendingAcks sc)
  writeTVar (mscPendingAcks sc) []
  let !ordered = reverse acks
  mapM_ (applyAck sc) ordered
  pure ordered

pendingAcknowledgementsMC :: MockShareConsumer -> IO [Acknowledgement]
pendingAcknowledgementsMC sc = reverse <$> readTVarIO (mscPendingAcks sc)

topicPartitions :: MockCluster -> [Text] -> IO [(Text, Int32)]
topicPartitions cluster topics =
  foldM appendTopic [] topics
  where
    appendTopic acc topic = do
      countM <- partitionCount cluster topic
      case countM of
        Nothing -> pure acc
        Just count -> pure (acc <> map (\p -> (topic, fromIntegral p)) [0 .. count - 1])

fetchEligiblePartition
  :: MockShareConsumer
  -> Int64
  -> Map ShareKey ShareLock
  -> Map ShareKey Int32
  -> Set ShareKey
  -> (Text, Int32)
  -> IO [ShareRecord]
fetchEligiblePartition sc now locks attempts completed (topic, part) = do
  hwmM <- partitionHWM (mscCluster sc) topic part
  case hwmM of
    Nothing -> pure []
    Just hwm -> do
      fetched <- fetchSlice (mscCluster sc) topic part 0 (fromIntegral hwm) False
      case fetched of
        Left _ -> pure []
        Right (records, _) ->
          pure (foldr (eligibleRecord sc now locks attempts completed topic part) [] records)

eligibleRecord
  :: MockShareConsumer
  -> Int64
  -> Map ShareKey ShareLock
  -> Map ShareKey Int32
  -> Set ShareKey
  -> Text
  -> Int32
  -> StoredRecord
  -> [ShareRecord]
  -> [ShareRecord]
eligibleRecord sc now locks attempts completed topic part rec acc =
  let !key = shareKey topic part rec
      !priorDeliveries = Map.findWithDefault 0 key attempts
      !nextDelivery = priorDeliveries + 1
  in if Set.member key completed
       then acc
       else case Map.lookup key locks of
         Just lock
           | now < slExpiresAtMs lock -> acc
           | slDeliveryCount lock >= scMaxDeliveryCount (mscConfig sc) -> acc
           | otherwise -> shareRecord topic part rec (slDeliveryCount lock + 1) : acc
         Nothing
           | priorDeliveries >= scMaxDeliveryCount (mscConfig sc) -> acc
           | otherwise -> shareRecord topic part rec nextDelivery : acc

lockDelivered :: Int64 -> MockShareConsumer -> [ShareRecord] -> STM ()
lockDelivered now sc records = do
  modifyTVar' (mscLocks sc) (\locks -> foldr lockOne locks records)
  modifyTVar' (mscAttempts sc) (\attempts -> foldr noteAttempt attempts records)
  where
    !expiresAt = now + fromIntegral (scLockTimeoutMs (mscConfig sc))
    lockOne rec acc =
      let !key = ShareKey (srTopic rec) (srPartition rec) (srBaseOffset rec) (srLastOffset rec)
          !lock = ShareLock
            { slExpiresAtMs = expiresAt
            , slDeliveryCount = srDeliveryCount rec
            }
      in Map.insert key lock acc
    noteAttempt rec acc =
      let !key = ShareKey (srTopic rec) (srPartition rec) (srBaseOffset rec) (srLastOffset rec)
      in Map.insert key (srDeliveryCount rec) acc

applyAck :: MockShareConsumer -> Acknowledgement -> STM ()
applyAck sc ack = do
  let !key = ShareKey
        (ackTopic ack)
        (ackPartition ack)
        (ackBaseOffset ack)
        (ackLastOffset ack)
  case ackType ack of
    AckAccept -> complete key
    AckReject -> complete key
    AckRelease -> modifyTVar' (mscLocks sc) (Map.delete key)
  where
    complete key = do
      modifyTVar' (mscLocks sc) (Map.delete key)
      modifyTVar' (mscAttempts sc) (Map.delete key)
      modifyTVar' (mscCompleted sc) (Set.insert key)

shareKey :: Text -> Int32 -> StoredRecord -> ShareKey
shareKey topic part rec =
  ShareKey topic part (Cluster.srOffset rec) (Cluster.srOffset rec)

shareRecord :: Text -> Int32 -> StoredRecord -> Int32 -> ShareRecord
shareRecord topic part rec deliveryCount = ShareRecord
  { srTopic = topic
  , srPartition = part
  , srBaseOffset = Cluster.srOffset rec
  , srLastOffset = Cluster.srOffset rec
  , srKey = Cluster.srKey rec
  , srValue = Cluster.srValue rec
  , srHeaders = Cluster.srHeaders rec
  , srTimestamp = Cluster.srTimestamp rec
  , srDeliveryCount = deliveryCount
  }
