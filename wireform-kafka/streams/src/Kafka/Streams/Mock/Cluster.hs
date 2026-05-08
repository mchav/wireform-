{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Mock.Cluster
-- Description : In-process broker simulation modelled on librdkafka's
--               @rd_kafka_mock_cluster_t@
--
-- The 'MockCluster' is the source of truth for a tiny in-memory
-- Kafka emulation. It owns:
--
--   * a table of topics, each with N partitions;
--   * per-partition append-only logs of 'StoredRecord';
--   * per-partition high-water marks (HWM) and a 'last-stable-offset'
--     for read-committed (transactional) consumers;
--   * a set of brokers (id-ed; can be marked /down/);
--   * consumer-group state (member list, offsets);
--   * transaction state (open / committed / aborted markers).
--
-- Producers and consumers are thin views over the cluster; see
-- 'Kafka.Streams.Mock.Producer' and 'Kafka.Streams.Mock.Consumer'.
--
-- Determinism: every operation is single-threaded under STM; tests
-- pump the cluster from a single thread to avoid scheduler races.
-- The clock is /manual/: callers advance it via 'tickClock', so
-- tests can assert exact ordering without 'threadDelay'.
module Kafka.Streams.Mock.Cluster
  ( -- * Cluster
    MockCluster
  , newMockCluster
  , clusterClockNow
  , tickClock
    -- * Topology
  , createTopic
  , listTopics
  , partitionCount
    -- * Brokers
  , BrokerId (..)
  , addBroker
  , markBrokerDown
  , markBrokerUp
  , isBrokerUp
  , downedBrokers
    -- * Append + fetch
  , StoredRecord (..)
  , ProducerStamp (..)
  , appendToPartition
  , fetchSlice
  , partitionHWM
  , partitionLastStableOffset
    -- * Consumer-group offsets
  , GroupId (..)
  , commitGroupOffsets
  , groupOffsetsFor
    -- * Transaction markers
  , TxnId (..)
  , TxnState (..)
  , beginTxn
  , commitTxn
  , abortTxn
  , txnState
    -- * Inspection
  , dumpPartition
  , partitionLogSize
  ) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless, when)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (TopicName, unTopicName)

----------------------------------------------------------------------
-- Identifiers
----------------------------------------------------------------------

newtype BrokerId = BrokerId { unBrokerId :: Int }
  deriving stock (Eq, Ord, Show, Generic)

newtype GroupId = GroupId { unGroupId :: Text }
  deriving stock (Eq, Ord, Show, Generic)

newtype TxnId = TxnId { unTxnId :: Text }
  deriving stock (Eq, Ord, Show, Generic)

----------------------------------------------------------------------
-- Stored record + transaction state
----------------------------------------------------------------------

data StoredRecord = StoredRecord
  { srOffset    :: !Int64
  , srKey       :: !(Maybe ByteString)
  , srValue     :: !ByteString
  , srTimestamp :: !Timestamp
  , srProducer  :: !(Maybe ProducerStamp)
    -- ^ Set when the record was written inside an open transaction.
    -- Read-committed consumers skip records whose stamp belongs to
    -- a still-open or aborted transaction.
  }
  deriving stock (Eq, Show, Generic)

data ProducerStamp = ProducerStamp
  { psTxnId :: !TxnId
  , psEpoch :: !Int32
  }
  deriving stock (Eq, Ord, Show, Generic)

data TxnState
  = TxnOpen
  | TxnCommitted
  | TxnAborted
  deriving stock (Eq, Show, Generic)

----------------------------------------------------------------------
-- Partition + topic
----------------------------------------------------------------------

data MockPartition = MockPartition
  { mpLog               :: !(TVar (Seq StoredRecord))
  , mpHwm               :: !(TVar Int64)
  , mpLeader            :: !(TVar BrokerId)
  , mpReplicas          :: ![BrokerId]
  , mpLastStableOffset  :: !(TVar Int64)
  }

data MockTopic = MockTopic
  { mtName       :: !TopicName
  , mtPartitions :: !(Map Int32 MockPartition)
  }

----------------------------------------------------------------------
-- Cluster
----------------------------------------------------------------------

data MockCluster = MockCluster
  { mcTopics         :: !(TVar (Map TopicName MockTopic))
  , mcGroups         :: !(TVar (Map GroupId GroupOffsets))
  , mcTxns           :: !(TVar (Map TxnId TxnState))
  , mcBrokers        :: !(TVar [BrokerId])
  , mcDownBrokers    :: !(TVar (Set BrokerId))
  , mcClock          :: !(TVar Timestamp)
  }

type GroupOffsets = Map (TopicName, Int32) Int64

-- | Build a fresh cluster with @n@ brokers (ids 0..n-1) and the
-- clock at @t = 0@. Topics start empty; the caller adds them via
-- 'createTopic'.
newMockCluster :: Int -> IO MockCluster
newMockCluster n = do
  ts <- newTVarIO Map.empty
  gs <- newTVarIO Map.empty
  xs <- newTVarIO Map.empty
  bs <- newTVarIO [BrokerId i | i <- [0 .. n - 1]]
  ds <- newTVarIO Set.empty
  ck <- newTVarIO (Timestamp 0)
  pure MockCluster
    { mcTopics      = ts
    , mcGroups      = gs
    , mcTxns        = xs
    , mcBrokers     = bs
    , mcDownBrokers = ds
    , mcClock       = ck
    }

clusterClockNow :: MockCluster -> IO Timestamp
clusterClockNow = readTVarIO . mcClock

-- | Advance the cluster's logical clock by @n@ ms. Mirrors how
-- librdkafka's mock cluster lets tests advance time deterministically.
tickClock :: MockCluster -> Int64 -> IO ()
tickClock c d = atomically $
  modifyTVar' (mcClock c) (\(Timestamp t) -> Timestamp (t + d))

----------------------------------------------------------------------
-- Topology
----------------------------------------------------------------------

-- | Create a topic with @n@ partitions. Each partition's leader is
-- chosen round-robin from the current broker set (so the partition
-- count and broker count don't have to match). Idempotent if the
-- topic already exists with the same partition count; raises if the
-- caller asks for a different partition count.
createTopic :: MockCluster -> TopicName -> Int -> IO ()
createTopic c topic n = do
  brokers <- readTVarIO (mcBrokers c)
  when (null brokers) $
    error "createTopic: cluster has no brokers"
  parts <- mapM (mkPartition brokers) [0 .. fromIntegral (n - 1)]
  let !mt = MockTopic
        { mtName       = topic
        , mtPartitions = Map.fromList parts
        }
  atomically $ do
    existing <- readTVar (mcTopics c)
    case Map.lookup topic existing of
      Just t
        | Map.size (mtPartitions t) == n -> pure ()
        | otherwise -> error $
            "createTopic: " <> T.unpack (unTopicName topic)
            <> " already exists with " <> show (Map.size (mtPartitions t))
            <> " partitions; cannot resize to " <> show n
      Nothing ->
        writeTVar (mcTopics c) (Map.insert topic mt existing)
  where
    mkPartition brokers i = do
      log_ <- newTVarIO Seq.empty
      hwm  <- newTVarIO 0
      let !leader = brokers !! (fromIntegral i `mod` length brokers)
      ld   <- newTVarIO leader
      lso  <- newTVarIO 0
      pure (i, MockPartition
        { mpLog              = log_
        , mpHwm              = hwm
        , mpLeader           = ld
        , mpReplicas         = brokers
        , mpLastStableOffset = lso
        })

listTopics :: MockCluster -> IO [TopicName]
listTopics c = Map.keys <$> readTVarIO (mcTopics c)

partitionCount :: MockCluster -> TopicName -> IO (Maybe Int)
partitionCount c topic = do
  m <- readTVarIO (mcTopics c)
  pure (Map.size . mtPartitions <$> Map.lookup topic m)

----------------------------------------------------------------------
-- Brokers
----------------------------------------------------------------------

addBroker :: MockCluster -> IO BrokerId
addBroker c = atomically $ do
  bs <- readTVar (mcBrokers c)
  let !nextId = case bs of
        []                 -> BrokerId 0
        _                  -> BrokerId (1 + maximum (map unBrokerId bs))
  writeTVar (mcBrokers c) (bs ++ [nextId])
  pure nextId

markBrokerDown :: MockCluster -> BrokerId -> IO ()
markBrokerDown c b = atomically $
  modifyTVar' (mcDownBrokers c) (Set.insert b)

markBrokerUp :: MockCluster -> BrokerId -> IO ()
markBrokerUp c b = atomically $
  modifyTVar' (mcDownBrokers c) (Set.delete b)

isBrokerUp :: MockCluster -> BrokerId -> IO Bool
isBrokerUp c b = do
  ds <- readTVarIO (mcDownBrokers c)
  pure (not (Set.member b ds))

downedBrokers :: MockCluster -> IO [BrokerId]
downedBrokers c = Set.toList <$> readTVarIO (mcDownBrokers c)

----------------------------------------------------------------------
-- Append + fetch
----------------------------------------------------------------------

-- | Append one record to a (topic, partition) log. Returns the
-- assigned offset, or 'Left' if the partition doesn't exist.
appendToPartition
  :: MockCluster
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> Maybe ProducerStamp
  -> IO (Either String Int64)
appendToPartition c topic part mk v ts stamp = atomically $ do
  topics <- readTVar (mcTopics c)
  case Map.lookup topic topics >>= Map.lookup part . mtPartitions of
    Nothing -> pure (Left $
      "appendToPartition: no partition "
       <> show part <> " on topic "
       <> T.unpack (unTopicName topic))
    Just p  -> do
      hwm <- readTVar (mpHwm p)
      let !rec = StoredRecord
            { srOffset    = hwm
            , srKey       = mk
            , srValue     = v
            , srTimestamp = ts
            , srProducer  = stamp
            }
      modifyTVar' (mpLog p) (|> rec)
      writeTVar (mpHwm p) (hwm + 1)
      -- LSO advances on non-transactional writes; transactional
      -- ones bump it only when the txn commits/aborts.
      case stamp of
        Nothing -> writeTVar (mpLastStableOffset p) (hwm + 1)
        Just _  -> pure ()
      pure (Right hwm)

-- | Fetch up to @maxRecords@ records starting at @from@ on a
-- partition. Read-committed consumers should pass @stableOnly =
-- True@ so records belonging to still-open or aborted transactions
-- are filtered out. Returns the records + the next offset to fetch.
fetchSlice
  :: MockCluster
  -> TopicName
  -> Int32
  -> Int64       -- ^ from offset (inclusive)
  -> Int         -- ^ max records
  -> Bool        -- ^ stable-only (read-committed)
  -> IO (Either String ([StoredRecord], Int64))
fetchSlice c topic part from maxN stableOnly = atomically $ do
  topics <- readTVar (mcTopics c)
  case Map.lookup topic topics >>= Map.lookup part . mtPartitions of
    Nothing -> pure (Left "fetchSlice: no such partition")
    Just p  -> do
      log_ <- readTVar (mpLog p)
      hwm  <- readTVar (mpHwm p)
      lso  <- readTVar (mpLastStableOffset p)
      txns <- readTVar (mcTxns c)
      let !ceiling_ = if stableOnly then lso else hwm
          !slice   = takeAt from maxN (F.toList log_) ceiling_
          !visible = filter (visibleAt txns) slice
          !next    = if null slice
                       then from
                       else srOffset (last slice) + 1
      pure (Right (visible, next))
  where
    takeAt _from _n []   _ = []
    takeAt from_ n (r:rs) ceiling_
      | srOffset r < from_ = takeAt from_ n rs ceiling_
      | srOffset r >= ceiling_ = []
      | n <= 0 = []
      | otherwise = r : takeAt from_ (n - 1) rs ceiling_

    visibleAt txns r = case srProducer r of
      Nothing -> True
      Just (ProducerStamp tid _) -> case Map.lookup tid txns of
        Just TxnCommitted -> True
        Just TxnAborted   -> False
        Just TxnOpen      -> not stableOnly
        Nothing           -> not stableOnly

partitionHWM :: MockCluster -> TopicName -> Int32 -> IO (Maybe Int64)
partitionHWM c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= Map.lookup part . mtPartitions of
    Nothing -> pure Nothing
    Just p  -> Just <$> readTVarIO (mpHwm p)

partitionLastStableOffset
  :: MockCluster -> TopicName -> Int32 -> IO (Maybe Int64)
partitionLastStableOffset c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= Map.lookup part . mtPartitions of
    Nothing -> pure Nothing
    Just p  -> Just <$> readTVarIO (mpLastStableOffset p)

----------------------------------------------------------------------
-- Consumer group offsets
----------------------------------------------------------------------

commitGroupOffsets
  :: MockCluster
  -> GroupId
  -> [(TopicName, Int32, Int64)]
  -> IO ()
commitGroupOffsets c grp xs = atomically $
  modifyTVar' (mcGroups c) $ \m ->
    let cur = Map.findWithDefault Map.empty grp m
        upd = foldr
                (\(t, p, o) -> Map.insert (t, p) o)
                cur xs
     in Map.insert grp upd m

groupOffsetsFor
  :: MockCluster -> GroupId -> IO (Map (TopicName, Int32) Int64)
groupOffsetsFor c grp = do
  m <- readTVarIO (mcGroups c)
  pure (Map.findWithDefault Map.empty grp m)

----------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------

beginTxn :: MockCluster -> TxnId -> IO ()
beginTxn c tid = atomically $
  modifyTVar' (mcTxns c) (Map.insert tid TxnOpen)

-- | Commit a transaction. Bumps the LSO of every partition this
-- transaction touched up to its high-water mark. Idempotent: a
-- transaction not in 'TxnOpen' is silently ignored (mirrors what a
-- real broker would do on a duplicate EndTxn).
commitTxn :: MockCluster -> TxnId -> IO ()
commitTxn c tid = atomically $ do
  modifyTVar' (mcTxns c) (Map.adjust (const TxnCommitted) tid)
  -- Advance LSO on every partition that has at least one record
  -- belonging to this transaction.
  topics <- readTVar (mcTopics c)
  forM_ (Map.elems topics) $ \mt ->
    forM_ (Map.elems (mtPartitions mt)) $ \p -> do
      log_ <- readTVar (mpLog p)
      let touched = any (\r -> case srProducer r of
                                  Just (ProducerStamp t _) -> t == tid
                                  Nothing                  -> False)
                        log_
      when touched $ do
        hwm <- readTVar (mpHwm p)
        writeTVar (mpLastStableOffset p) hwm

abortTxn :: MockCluster -> TxnId -> IO ()
abortTxn c tid = atomically $ do
  modifyTVar' (mcTxns c) (Map.adjust (const TxnAborted) tid)
  -- LSO advances past aborted records so read-committed consumers
  -- skip them on the next fetch.
  topics <- readTVar (mcTopics c)
  forM_ (Map.elems topics) $ \mt ->
    forM_ (Map.elems (mtPartitions mt)) $ \p -> do
      log_ <- readTVar (mpLog p)
      let touched = any (\r -> case srProducer r of
                                  Just (ProducerStamp t _) -> t == tid
                                  Nothing                  -> False)
                        log_
      when touched $ do
        hwm <- readTVar (mpHwm p)
        writeTVar (mpLastStableOffset p) hwm

txnState :: MockCluster -> TxnId -> IO (Maybe TxnState)
txnState c tid = Map.lookup tid <$> readTVarIO (mcTxns c)

----------------------------------------------------------------------
-- Inspection helpers
----------------------------------------------------------------------

dumpPartition :: MockCluster -> TopicName -> Int32 -> IO [StoredRecord]
dumpPartition c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= Map.lookup part . mtPartitions of
    Nothing -> pure []
    Just p  -> F.toList <$> readTVarIO (mpLog p)

partitionLogSize :: MockCluster -> TopicName -> Int32 -> IO Int
partitionLogSize c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= Map.lookup part . mtPartitions of
    Nothing -> pure 0
    Just p  -> Seq.length <$> readTVarIO (mpLog p)
