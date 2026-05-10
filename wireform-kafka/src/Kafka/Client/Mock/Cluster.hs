{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Client.Mock.Cluster
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
-- 'Kafka.Client.Mock.Producer' and 'Kafka.Client.Mock.Consumer'.
--
-- Determinism: every operation is single-threaded under STM; tests
-- pump the cluster from a single thread to avoid scheduler races.
-- The clock is /manual/: callers advance it via 'tickClock', so
-- tests can assert exact ordering without 'threadDelay'.
module Kafka.Client.Mock.Cluster
  ( -- * Cluster
    MockCluster
  , newMockCluster
  , clusterClockNow
  , tickClock
    -- * Topology
  , createTopic
  , deleteTopic
  , listTopics
  , partitionCount
    -- * Auto-create
  , setAutoCreateTopics
  , getAutoCreateTopics
  , autoCreateDefaultPartitions
    -- * Brokers
  , BrokerId (..)
  , addBroker
  , markBrokerDown
  , markBrokerUp
  , isBrokerUp
  , downedBrokers
  , clusterBrokers
  , partitionLeader
  , reassignPartitionLeader
    -- * Cluster metadata
  , ClusterMetadata (..)
  , TopicMetadata (..)
  , PartitionMetadata (..)
  , describeClusterMetadata
    -- * Append + fetch
  , StoredRecord (..)
  , ProducerStamp (..)
  , appendToPartition
  , fetchSlice
  , partitionHWM
  , partitionLastStableOffset
    -- * Leader epoch (KIP-320)
  , currentLeaderEpoch
  , bumpLeaderEpoch
  , validateOffsetEpoch
    -- * Consumer-group offsets
  , GroupId (..)
  , commitGroupOffsets
  , commitGroupOffsetsWithMetadata
  , OffsetAndMetadata (..)
  , groupOffsetsFor
  , groupOffsetsWithMetadataFor
    -- * Transaction markers
  , TxnId (..)
  , TxnState (..)
  , beginTxn
  , commitTxn
  , abortTxn
  , txnState
  , currentTxnEpoch
  , sendOffsetsToTxn
  , pendingTxnOffsets
    -- * Group rebalance / assignment
  , MemberId (..)
  , unMemberId
  , joinGroup
  , leaveGroup
  , membersOf
  , knownGroups
  , assignmentFor
  , RebalanceDelta (..)
  , cooperativeRebalance
  , GenerationId (..)
  , currentGeneration
    -- * KRaft mode (KIP-500, librdkafka 0148)
  , KRaftRole (..)
  , setKRaftRole
  , kraftRole
  , controllerBroker
  , setControllerBroker
    -- * Re-authentication (KIP-368 / 0142)
  , setReauthDeadline
  , reauthDeadline
  , isReauthExpired
    -- * Inspection
  , dumpPartition
  , partitionLogSize
  ) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless, when)
import qualified Data.List as L
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
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
  , srTimestamp :: !Int64
  , srHeaders   :: ![(Text, ByteString)]
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
  , mpLeaderEpoch       :: !(TVar Int32)
    -- ^ Bumped by 'bumpLeaderEpoch' on a leader change. Consumers
    -- that committed an offset under an older epoch will be
    -- rejected via 'validateOffsetEpoch' — KIP-320.
  }

data MockTopic = MockTopic
  { mtName       :: !Text
  , mtPartitions :: !(IntMap MockPartition)
  }

----------------------------------------------------------------------
-- Cluster
----------------------------------------------------------------------

data MockCluster = MockCluster
  { mcTopics         :: !(TVar (Map Text MockTopic))
  , mcAutoCreate     :: !(TVar (Maybe Int))
    -- ^ When 'Just n', any append/fetch / subscribe to a topic
    -- that doesn't exist creates it with @n@ partitions on demand.
    -- 'Nothing' (the default) preserves strict-validation semantics:
    -- non-existent topics surface 'UnknownTopicOrPartition'.
  , mcGroups         :: !(TVar (Map GroupId GroupOffsets))
  , mcGroupsMeta     :: !(TVar (Map GroupId
                          (Map (Text, Int32) OffsetAndMetadata)))
  , mcGroupMembers   :: !(TVar (Map GroupId [(MemberId, Set Text)]))
  , mcTxns           :: !(TVar (Map TxnId TxnState))
  , mcTxnPendingOffs :: !(TVar (Map TxnId
                          (Map (GroupId, Text, Int32) OffsetAndMetadata)))
    -- ^ Per-txn pending offsets staged by 'sendOffsetsToTxn'. On
    -- 'commitTxn' these are atomically merged into the group
    -- offset store; on 'abortTxn' they're discarded.
    -- ^ Latest state per txn id. Re-used txn ids overwrite the
    -- previous state; for /per-snapshot/ visibility, see
    -- 'mcCommittedStamps' and 'mcAbortedStamps'.
  , mcTxnEpoch       :: !(TVar (Map TxnId Int32))
    -- ^ Current epoch for each transactional id. 'commitTxn' /
    -- 'abortTxn' bump it; producers stamped with a stale epoch
    -- are fenced on their next send.
  , mcCommittedStamps :: !(TVar (Set ProducerStamp))
  , mcAbortedStamps   :: !(TVar (Set ProducerStamp))
  , mcBrokers        :: !(TVar [BrokerId])
  , mcDownBrokers    :: !(TVar (Set BrokerId))
  , mcClock          :: !(TVar Int64)
  , mcGroupGen       :: !(TVar (Map GroupId GenerationId))
    -- ^ Per-group generation id. Bumped on every join / leave.
  , mcKRaftRole      :: !(TVar KRaftRole)
  , mcController     :: !(TVar (Maybe BrokerId))
  , mcReauthDl       :: !(TVar (Maybe Int64))
    -- ^ Wall-clock-derived deadline (in cluster-clock ms) past
    -- which re-auth is required. 'Nothing' disables the window.
  }

-- | Generation id tracking — bumped each time the group's
-- membership changes. Mirrors the @generation_id@ on
-- @JoinGroupResponse@.
newtype GenerationId = GenerationId { unGenerationId :: Int }
  deriving stock (Eq, Ord, Show, Generic)

-- | Mirrors KIP-500's broker / controller role split. Tests use
-- 'setKRaftRole' to flip the cluster between Zookeeper-style
-- (KRaftBroker) and KRaft (KRaftCombined / KRaftController).
data KRaftRole
  = KRaftBroker
  | KRaftController
  | KRaftCombined
  deriving stock (Eq, Show, Generic)

type GroupOffsets = Map (Text, Int32) Int64

-- | Offset + per-commit metadata. Mirrors Java's
-- @org.apache.kafka.clients.consumer.OffsetAndMetadata@.
data OffsetAndMetadata = OffsetAndMetadata
  { oamOffset      :: !Int64
  , oamMetadata    :: !(Maybe ByteString)
  , oamLeaderEpoch :: !(Maybe Int32)
  }
  deriving stock (Eq, Show, Generic)

-- | Identifier for a single consumer in a group. Mirrors the
-- @member.id@ Kafka assigns at JoinGroup time.
newtype MemberId = MemberId { unMemberId :: Text }
  deriving stock (Eq, Ord, Show, Generic)

-- | Build a fresh cluster with @n@ brokers (ids 0..n-1) and the
-- clock at @t = 0@. Topics start empty; the caller adds them via
-- 'createTopic'.
newMockCluster :: Int -> IO MockCluster
newMockCluster n = do
  ts <- newTVarIO Map.empty
  ac <- newTVarIO Nothing
  gs <- newTVarIO Map.empty
  gmd <- newTVarIO Map.empty
  gm <- newTVarIO Map.empty
  xs <- newTVarIO Map.empty
  xpo <- newTVarIO Map.empty
  ep <- newTVarIO Map.empty
  cs <- newTVarIO Set.empty
  as <- newTVarIO Set.empty
  bs <- newTVarIO [BrokerId i | i <- [0 .. n - 1]]
  ds <- newTVarIO Set.empty
  ck <- newTVarIO (0 :: Int64)
  gg <- newTVarIO Map.empty
  kr <- newTVarIO KRaftCombined
  ctl <- newTVarIO (case [BrokerId i | i <- [0 .. n - 1]] of
                     []      -> Nothing
                     (b : _) -> Just b)
  rd <- newTVarIO Nothing
  pure MockCluster
    { mcTopics          = ts
    , mcAutoCreate      = ac
    , mcGroups          = gs
    , mcGroupsMeta      = gmd
    , mcGroupMembers    = gm
    , mcTxns            = xs
    , mcTxnPendingOffs  = xpo
    , mcTxnEpoch        = ep
    , mcCommittedStamps = cs
    , mcAbortedStamps   = as
    , mcBrokers         = bs
    , mcDownBrokers     = ds
    , mcClock           = ck
    , mcGroupGen        = gg
    , mcKRaftRole       = kr
    , mcController      = ctl
    , mcReauthDl        = rd
    }

clusterClockNow :: MockCluster -> IO Int64
clusterClockNow = readTVarIO . mcClock

-- | Advance the cluster's logical clock by @n@ ms. Mirrors how
-- librdkafka's mock cluster lets tests advance time deterministically.
tickClock :: MockCluster -> Int64 -> IO ()
tickClock c d = atomically $
  modifyTVar' (mcClock c) (+ d)

----------------------------------------------------------------------
-- Topology
----------------------------------------------------------------------

-- | Create a topic with @n@ partitions. Each partition's leader is
-- chosen round-robin from the current broker set (so the partition
-- count and broker count don't have to match). Idempotent if the
-- topic already exists with the same partition count; raises if the
-- caller asks for a different partition count.
createTopic :: MockCluster -> Text -> Int -> IO ()
createTopic c topic n = do
  brokers <- readTVarIO (mcBrokers c)
  when (null brokers) $
    error "createTopic: cluster has no brokers"
  parts <- mapM (mkPartition brokers) [0 .. fromIntegral (n - 1)]
  let !mt = MockTopic
        { mtName       = topic
        , mtPartitions = IntMap.fromList
            [(fromIntegral k, v) | (k, v) <- parts]
        }
  atomically $ do
    existing <- readTVar (mcTopics c)
    case Map.lookup topic existing of
      Just t
        | IntMap.size (mtPartitions t) == n -> pure ()
        | otherwise -> error $
            "createTopic: " <> T.unpack (id topic)
            <> " already exists with " <> show (IntMap.size (mtPartitions t))
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
      lep  <- newTVarIO 0
      pure (i, MockPartition
        { mpLog              = log_
        , mpHwm              = hwm
        , mpLeader           = ld
        , mpReplicas         = brokers
        , mpLastStableOffset = lso
        , mpLeaderEpoch      = lep
        })

-- | Remove a topic and discard its partition logs. Group offset
-- entries pointing at the deleted topic remain (matches what a
-- real broker does — the consumer would discover the topic is
-- gone via metadata refresh and reset its position).
deleteTopic :: MockCluster -> Text -> IO Bool
deleteTopic c topic = atomically $ do
  m <- readTVar (mcTopics c)
  case Map.lookup topic m of
    Nothing -> pure False
    Just _  -> do
      writeTVar (mcTopics c) (Map.delete topic m)
      pure True

listTopics :: MockCluster -> IO [Text]
listTopics c = Map.keys <$> readTVarIO (mcTopics c)

-- | Enable / disable auto-create. 'Just n' configures the default
-- partition count for auto-created topics; 'Nothing' (the default)
-- disables auto-create.
setAutoCreateTopics :: MockCluster -> Maybe Int -> IO ()
setAutoCreateTopics c m = atomically $ writeTVar (mcAutoCreate c) m

getAutoCreateTopics :: MockCluster -> IO (Maybe Int)
getAutoCreateTopics = readTVarIO . mcAutoCreate

-- | The default partition count we'll use when 'setAutoCreateTopics'
-- has been called with 'Just _' but the caller wants the value back.
autoCreateDefaultPartitions :: Int
autoCreateDefaultPartitions = 1

partitionCount :: MockCluster -> Text -> IO (Maybe Int)
partitionCount c topic = do
  m <- readTVarIO (mcTopics c)
  pure (IntMap.size . mtPartitions <$> Map.lookup topic m)

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

-- | Snapshot the cluster's full broker registry (id-only). Useful
-- for the admin client mock and for tests that need to assert on
-- the broker count after 'addBroker'.
clusterBrokers :: MockCluster -> IO [BrokerId]
clusterBrokers = readTVarIO . mcBrokers

-- | Read the current leader for a (topic, partition).
partitionLeader
  :: MockCluster -> Text -> Int32 -> IO (Maybe BrokerId)
partitionLeader c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
    Nothing -> pure Nothing
    Just p  -> Just <$> readTVarIO (mpLeader p)

-- | Reassign a partition's leader and bump its leader epoch.
-- Tests use this to simulate a leader election after a broker
-- went down. Returns the new epoch.
reassignPartitionLeader
  :: MockCluster -> Text -> Int32 -> BrokerId -> IO (Maybe Int32)
reassignPartitionLeader c topic part newLeader = atomically $ do
  topics <- readTVar (mcTopics c)
  case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
    Nothing -> pure Nothing
    Just p  -> do
      writeTVar (mpLeader p) newLeader
      modifyTVar' (mpLeaderEpoch p) (+ 1)
      ep <- readTVar (mpLeaderEpoch p)
      pure (Just ep)

----------------------------------------------------------------------
-- Append + fetch
----------------------------------------------------------------------

-- | Append one record to a (topic, partition) log. Returns the
-- assigned offset, 'Left "fenced"' if the producer's epoch has been
-- superseded, or 'Left "no partition"' if the partition doesn't
-- exist. Carries headers (mirrors the JVM client's
-- @ProducerRecord.headers@).
appendToPartition
  :: MockCluster
  -> Text
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Int64
  -> [(Text, ByteString)]               -- ^ headers
  -> Maybe ProducerStamp
  -> IO (Either String Int64)
appendToPartition c topic part mk v ts hdrs stamp = do
  -- Auto-create outside the STM transaction (uses topology IO).
  ensureTopicForAppend c topic
  atomically $ do
    topics <- readTVar (mcTopics c)
    -- Fence check: stamp's epoch must match the cluster's current
    -- epoch for that txn id (or be Nothing).
    fenced <- case stamp of
      Nothing -> pure False
      Just (ProducerStamp tid ep) -> do
        epochs <- readTVar (mcTxnEpoch c)
        case Map.lookup tid epochs of
          Just cur | cur > ep -> pure True
          _                   -> pure False
    if fenced
      then pure (Left "fenced: producer epoch superseded")
      else case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
        Nothing -> pure (Left $
          "appendToPartition: no partition "
           <> show part <> " on topic "
           <> T.unpack topic)
        Just p  -> do
          -- Leader-down propagation: if the partition's current
          -- leader is in 'mcDownBrokers', surface a 'not_leader'
          -- error so the producer client can retry / refresh
          -- metadata.
          leader <- readTVar (mpLeader p)
          downs  <- readTVar (mcDownBrokers c)
          if Set.member leader downs
            then pure (Left "not_leader_for_partition")
            else do
              hwm <- readTVar (mpHwm p)
              let !rec = StoredRecord
                    { srOffset    = hwm
                    , srKey       = mk
                    , srValue     = v
                    , srTimestamp = ts
                    , srHeaders   = hdrs
                    , srProducer  = stamp
                    }
              modifyTVar' (mpLog p) (|> rec)
              writeTVar (mpHwm p) (hwm + 1)
              case stamp of
                Nothing -> writeTVar (mpLastStableOffset p) (hwm + 1)
                Just _  -> pure ()
              pure (Right hwm)

-- | Auto-create the topic if 'mcAutoCreate' is set and the topic
-- doesn't already exist. No-op otherwise.
ensureTopicForAppend :: MockCluster -> Text -> IO ()
ensureTopicForAppend c topic = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics of
    Just _  -> pure ()
    Nothing -> do
      mAuto <- readTVarIO (mcAutoCreate c)
      case mAuto of
        Nothing -> pure ()
        Just n  -> createTopic c topic n

-- | Fetch up to @maxRecords@ records starting at @from@ on a
-- partition. Read-committed consumers should pass @stableOnly =
-- True@ so records belonging to still-open or aborted transactions
-- are filtered out. Returns the records + the next offset to fetch.
fetchSlice
  :: MockCluster
  -> Text
  -> Int32
  -> Int64       -- ^ from offset (inclusive)
  -> Int         -- ^ max records
  -> Bool        -- ^ stable-only (read-committed)
  -> IO (Either String ([StoredRecord], Int64))
fetchSlice c topic part from maxN stableOnly = atomically $ do
  topics <- readTVar (mcTopics c)
  case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
    Nothing -> pure (Left "fetchSlice: no such partition")
    Just p  -> do
      log_ <- readTVar (mpLog p)
      hwm  <- readTVar (mpHwm p)
      lso  <- readTVar (mpLastStableOffset p)
      committed <- readTVar (mcCommittedStamps c)
      aborted   <- readTVar (mcAbortedStamps   c)
      let !ceiling_ = if stableOnly then lso else hwm
          !slice   = takeAt from maxN (F.toList log_) ceiling_
          !visible = filter (visibleAt committed aborted) slice
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

    visibleAt committed aborted r = case srProducer r of
      Nothing -> True
      Just stamp
        | Set.member stamp aborted   -> False
        | Set.member stamp committed -> True
        | otherwise                  -> not stableOnly

partitionHWM :: MockCluster -> Text -> Int32 -> IO (Maybe Int64)
partitionHWM c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
    Nothing -> pure Nothing
    Just p  -> Just <$> readTVarIO (mpHwm p)

partitionLastStableOffset
  :: MockCluster -> Text -> Int32 -> IO (Maybe Int64)
partitionLastStableOffset c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
    Nothing -> pure Nothing
    Just p  -> Just <$> readTVarIO (mpLastStableOffset p)

----------------------------------------------------------------------
-- Cluster metadata
----------------------------------------------------------------------

data PartitionMetadata = PartitionMetadata
  { pmId          :: !Int32
  , pmLeader      :: !(Maybe BrokerId)   -- 'Nothing' iff leader is down
  , pmReplicas    :: ![BrokerId]
  , pmLeaderEpoch :: !Int32
  }
  deriving stock (Eq, Show, Generic)

data TopicMetadata = TopicMetadata
  { tmName       :: !Text
  , tmPartitions :: ![PartitionMetadata]
  }
  deriving stock (Eq, Show, Generic)

data ClusterMetadata = ClusterMetadata
  { cmClusterId :: !Text
  , cmBrokers   :: ![BrokerId]
  , cmTopics    :: ![TopicMetadata]
  }
  deriving stock (Eq, Show, Generic)

-- | Snapshot of the cluster's full metadata. Mirrors what a
-- @MetadataRequest@ would surface to a client.
describeClusterMetadata :: MockCluster -> IO ClusterMetadata
describeClusterMetadata c = do
  bs <- readTVarIO (mcBrokers c)
  ds <- readTVarIO (mcDownBrokers c)
  topics <- readTVarIO (mcTopics c)
  tms <- mapM (mkTopicMeta ds) (Map.toAscList topics)
  pure ClusterMetadata
    { cmClusterId = "mock-cluster"
    , cmBrokers   = bs
    , cmTopics    = tms
    }
  where
    mkTopicMeta ds (name, mt) = do
      -- Convert the IntMap key back to Int32 to keep the
      -- downstream tuple shape consistent with the broker's
      -- partition-id type.
      pms <- mapM (mkPartMeta ds)
               [ (fromIntegral k, v)
               | (k, v) <- IntMap.toAscList (mtPartitions mt)
               ]
      pure TopicMetadata { tmName = name, tmPartitions = pms }

    mkPartMeta ds (pid, p) = do
      ld <- readTVarIO (mpLeader p)
      ep <- readTVarIO (mpLeaderEpoch p)
      pure PartitionMetadata
        { pmId          = pid
        , pmLeader      = if Set.member ld ds then Nothing else Just ld
        , pmReplicas    = mpReplicas p
        , pmLeaderEpoch = ep
        }

----------------------------------------------------------------------
-- Leader epoch (KIP-320)
----------------------------------------------------------------------

-- | Current leader epoch for a (topic, partition). Mirrors what
-- 'Metadata' would surface to a consumer.
currentLeaderEpoch
  :: MockCluster -> Text -> Int32 -> IO (Maybe Int32)
currentLeaderEpoch c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
    Nothing -> pure Nothing
    Just p  -> Just <$> readTVarIO (mpLeaderEpoch p)

-- | Bump the leader epoch on a partition. Tests use this to
-- simulate a leader election (e.g. after 'markBrokerDown' then
-- 'markBrokerUp'). After the bump, any consumer that tries to
-- 'validateOffsetEpoch' with the previous epoch is rejected.
bumpLeaderEpoch :: MockCluster -> Text -> Int32 -> IO Int32
bumpLeaderEpoch c topic part = atomically $ do
  topics <- readTVar (mcTopics c)
  case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
    Nothing -> pure (-1)
    Just p  -> do
      modifyTVar' (mpLeaderEpoch p) (+ 1)
      readTVar (mpLeaderEpoch p)

-- | Validate that an offset's last-known leader epoch matches the
-- partition's current epoch. Mirrors the
-- @OffsetFetch / Fetch / OffsetForLeaderEpoch@ KIP-320 dance:
--
--   * 'Right ()' — caller's epoch is current (or 'Nothing'); ok.
--   * 'Left "diverged"' — caller's epoch is below the partition's
--     current epoch; the consumer should reset.
validateOffsetEpoch
  :: MockCluster
  -> Text
  -> Int32
  -> Maybe Int32                       -- ^ caller's last-known epoch
  -> IO (Either String ())
validateOffsetEpoch _ _ _ Nothing = pure (Right ())
validateOffsetEpoch c topic part (Just ep) = do
  cur <- currentLeaderEpoch c topic part
  case cur of
    Nothing      -> pure (Left "no such partition")
    Just curEpoch
      | curEpoch == ep -> pure (Right ())
      | curEpoch >  ep -> pure (Left "diverged: leader epoch advanced")
      | otherwise      -> pure (Right ())  -- caller ahead is unusual
                                           -- but not a hard error

----------------------------------------------------------------------
-- Consumer group offsets
----------------------------------------------------------------------

commitGroupOffsets
  :: MockCluster
  -> GroupId
  -> [(Text, Int32, Int64)]
  -> IO ()
commitGroupOffsets c grp xs =
  commitGroupOffsetsWithMetadata c grp
    [ ((t, p), OffsetAndMetadata o Nothing Nothing) | (t, p, o) <- xs ]

-- | Commit offsets with full 'OffsetAndMetadata' (including
-- per-commit metadata bytes and KIP-320 leader-epoch). Mirrors
-- @KafkaConsumer.commitSync(Map<TopicPartition, OffsetAndMetadata>)@.
commitGroupOffsetsWithMetadata
  :: MockCluster
  -> GroupId
  -> [((Text, Int32), OffsetAndMetadata)]
  -> IO ()
commitGroupOffsetsWithMetadata c grp xs = atomically $ do
  modifyTVar' (mcGroups c) $ \m ->
    let cur = Map.findWithDefault Map.empty grp m
        upd = foldr
                (\((t, p), oam) -> Map.insert (t, p) (oamOffset oam))
                cur xs
     in Map.insert grp upd m
  modifyTVar' (mcGroupsMeta c) $ \m ->
    let cur = Map.findWithDefault Map.empty grp m
        upd = foldr
                (\(tp, oam) -> Map.insert tp oam)
                cur xs
     in Map.insert grp upd m

groupOffsetsFor
  :: MockCluster -> GroupId -> IO (Map (Text, Int32) Int64)
groupOffsetsFor c grp = do
  m <- readTVarIO (mcGroups c)
  pure (Map.findWithDefault Map.empty grp m)

-- | Fetch full per-(topic, partition) metadata, including the
-- KIP-320 leader-epoch the consumer last validated against.
groupOffsetsWithMetadataFor
  :: MockCluster
  -> GroupId
  -> IO (Map (Text, Int32) OffsetAndMetadata)
groupOffsetsWithMetadataFor c grp = do
  m <- readTVarIO (mcGroupsMeta c)
  pure (Map.findWithDefault Map.empty grp m)

----------------------------------------------------------------------
-- Transactions
----------------------------------------------------------------------

-- | Open a transaction. If the txn id has been used before, the
-- new attempt re-opens it at the current epoch (mirrors what the
-- broker does on @InitProducerId@).
beginTxn :: MockCluster -> TxnId -> IO ()
beginTxn c tid = atomically $ do
  modifyTVar' (mcTxns c) (Map.insert tid TxnOpen)
  -- Initialise the epoch to 0 if this is the first time.
  modifyTVar' (mcTxnEpoch c)
    (Map.alter (Just . maybe 0 id) tid)

-- | Read the current producer epoch for a txn id. Producers with a
-- stamp whose epoch is below this are fenced.
currentTxnEpoch :: MockCluster -> TxnId -> IO Int32
currentTxnEpoch c tid = do
  m <- readTVarIO (mcTxnEpoch c)
  pure (Map.findWithDefault 0 tid m)

-- | Commit a transaction. Bumps the LSO of every partition this
-- transaction touched up to its high-water mark. Idempotent: a
-- transaction not in 'TxnOpen' is silently ignored (mirrors what a
-- real broker would do on a duplicate EndTxn).
commitTxn :: MockCluster -> TxnId -> IO ()
commitTxn c tid = atomically $ do
  -- Drain pending offsets atomically with the txn marker.
  pending <- readTVar (mcTxnPendingOffs c)
  case Map.lookup tid pending of
    Nothing  -> pure ()
    Just per -> do
      modifyTVar' (mcGroups c) $ \m ->
        let go !acc ((g, t, p), oam) =
              Map.insertWith Map.union g
                (Map.singleton (t, p) (oamOffset oam)) acc
            !merged = foldl go m (Map.toList per)
         in merged
      modifyTVar' (mcGroupsMeta c) $ \m ->
        let go !acc ((g, t, p), oam) =
              Map.insertWith Map.union g
                (Map.singleton (t, p) oam) acc
            !merged = foldl go m (Map.toList per)
         in merged
      modifyTVar' (mcTxnPendingOffs c) (Map.delete tid)
  modifyTVar' (mcTxns c) (Map.insert tid TxnCommitted)
  -- Mark THIS specific snapshot (current epoch) as committed.
  ep <- readTVar (mcTxnEpoch c)
  let !curEp = Map.findWithDefault 0 tid ep
  modifyTVar' (mcCommittedStamps c)
    (Set.insert (ProducerStamp tid curEp))
  -- Bump the epoch so any in-flight producer with the old epoch
  -- gets fenced on its next send.
  modifyTVar' (mcTxnEpoch c) (Map.adjust (+ 1) tid)
  -- Advance LSO on every partition that has at least one record
  -- belonging to this transaction.
  topics <- readTVar (mcTopics c)
  forM_ (Map.elems topics) $ \mt ->
    forM_ (IntMap.elems (mtPartitions mt)) $ \p -> do
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
  -- Discard any pending offsets staged by 'sendOffsetsToTxn'.
  modifyTVar' (mcTxnPendingOffs c) (Map.delete tid)
  modifyTVar' (mcTxns c) (Map.insert tid TxnAborted)
  ep <- readTVar (mcTxnEpoch c)
  let !curEp = Map.findWithDefault 0 tid ep
  modifyTVar' (mcAbortedStamps c)
    (Set.insert (ProducerStamp tid curEp))
  modifyTVar' (mcTxnEpoch c) (Map.adjust (+ 1) tid)
  -- LSO advances past aborted records so read-committed consumers
  -- skip them on the next fetch.
  topics <- readTVar (mcTopics c)
  forM_ (Map.elems topics) $ \mt ->
    forM_ (IntMap.elems (mtPartitions mt)) $ \p -> do
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

-- | Stage consumer-group offsets to commit atomically when the txn
-- commits. Mirrors @KafkaProducer.sendOffsetsToTransaction@. The
-- offsets are visible to 'pendingTxnOffsets' until 'commitTxn'
-- (where they merge into the group's offset store) or 'abortTxn'
-- (where they're discarded).
sendOffsetsToTxn
  :: MockCluster
  -> TxnId
  -> GroupId
  -> [((Text, Int32), OffsetAndMetadata)]
  -> IO ()
sendOffsetsToTxn c tid grp xs = atomically $
  modifyTVar' (mcTxnPendingOffs c) $ \m ->
    let cur = Map.findWithDefault Map.empty tid m
        upd = foldr
                (\((t, p), oam) -> Map.insert (grp, t, p) oam)
                cur xs
     in Map.insert tid upd m

-- | Inspect the offsets staged by 'sendOffsetsToTxn' but not yet
-- committed or aborted. Empty after a successful commit / abort.
pendingTxnOffsets
  :: MockCluster
  -> TxnId
  -> IO (Map (GroupId, Text, Int32) OffsetAndMetadata)
pendingTxnOffsets c tid = do
  m <- readTVarIO (mcTxnPendingOffs c)
  pure (Map.findWithDefault Map.empty tid m)

----------------------------------------------------------------------
-- Group rebalance / assignment
----------------------------------------------------------------------

-- | Add a member to a consumer group, declaring the topics it
-- subscribes to. The cluster runs a round-robin assignment over
-- every (topic, partition) of the union of subscribed topics
-- across all current members; 'assignmentFor' returns each
-- member's slice.
joinGroup
  :: MockCluster
  -> GroupId
  -> MemberId
  -> [Text]
  -> IO ()
joinGroup c grp mid topics = atomically $ do
  modifyTVar' (mcGroupMembers c) $ \m ->
    let cur = Map.findWithDefault [] grp m
        !next = filter ((/= mid) . fst) cur
                ++ [(mid, Set.fromList topics)]
    in Map.insert grp next m
  bumpGen (mcGroupGen c) grp

leaveGroup :: MockCluster -> GroupId -> MemberId -> IO ()
leaveGroup c grp mid = atomically $ do
  modifyTVar' (mcGroupMembers c) $ \m ->
    Map.adjust (filter ((/= mid) . fst)) grp m
  bumpGen (mcGroupGen c) grp

-- | Read the group's current generation id (0 if the group has
-- no members yet).
currentGeneration :: MockCluster -> GroupId -> IO GenerationId
currentGeneration c grp = do
  m <- readTVarIO (mcGroupGen c)
  pure (Map.findWithDefault (GenerationId 0) grp m)

bumpGen
  :: TVar (Map GroupId GenerationId)
  -> GroupId
  -> STM ()
bumpGen ref grp = modifyTVar' ref $ \m ->
  let !cur = Map.findWithDefault (GenerationId 0) grp m
      !nxt = GenerationId (unGenerationId cur + 1)
   in Map.insert grp nxt m

membersOf :: MockCluster -> GroupId -> IO [MemberId]
membersOf c grp = do
  m <- readTVarIO (mcGroupMembers c)
  pure (map fst (Map.findWithDefault [] grp m))

-- | Every group id the cluster has heard about (via 'joinGroup'
-- or via offset commits). Mirrors @AdminClient.listConsumerGroups@.
knownGroups :: MockCluster -> IO [GroupId]
knownGroups c = do
  gs1 <- Map.keys <$> readTVarIO (mcGroupMembers c)
  gs2 <- Map.keys <$> readTVarIO (mcGroups c)
  pure (Set.toList (Set.fromList (gs1 ++ gs2)))

-- | Compute this member's currently-assigned partitions under a
-- deterministic round-robin assignor across the group's members.
-- The assignor is sticky enough for tests: members are sorted by
-- 'MemberId', topics by 'Text', and partitions are dealt out
-- in order.
assignmentFor
  :: MockCluster -> GroupId -> MemberId -> IO [(Text, Int32)]
assignmentFor c grp mid = do
  members <- readTVarIO (mcGroupMembers c)
  topics  <- readTVarIO (mcTopics c)
  let !grpMems  = Map.findWithDefault [] grp members
      !sorted   = L.sortBy
                    (\(a, _) (b, _) -> compare (unMemberId a) (unMemberId b))
                    grpMems
      -- Sorted list of all topics anyone in the group subscribed to.
      !allTopics = L.sort
                     (Set.toList (Set.unions (map snd sorted)))
  pure (concatMap (assignedForTopic topics sorted mid) allTopics)
  where
    -- Per-topic round-robin across the subset of members that
    -- subscribed to /this/ topic. Mirrors what Kafka's
    -- RangeAssignor / RoundRobinAssignor do for asymmetric
    -- subscriptions: a member only gets partitions of topics it
    -- declared.
    assignedForTopic
      :: Map Text MockTopic
      -> [(MemberId, Set Text)]
      -> MemberId
      -> Text
      -> [(Text, Int32)]
    assignedForTopic topicMap sorted self t =
      let !subscribers =
            [ m | (m, sub) <- sorted, Set.member t sub ]
       in case (Map.lookup t topicMap, lookupIdx self subscribers) of
            (Just mt, Just selfIdx) ->
              let !n  = IntMap.size (mtPartitions mt)
                  !ms = length subscribers
               in [ (t, fromIntegral i)
                  | i <- [0 .. n - 1]
                  , i `mod` ms == selfIdx
                  ]
            _ -> []

    lookupIdx :: MemberId -> [MemberId] -> Maybe Int
    lookupIdx m = go 0
      where
        go _  []     = Nothing
        go !i (x:xs)
          | x == m    = Just i
          | otherwise = go (i + 1) xs

-- | The delta a cooperative rebalance hands a member: which
-- partitions were revoked since the last assignment, which were
-- added, and the resulting full assignment. Mirrors KIP-429
-- cooperative-sticky rebalancing.
data RebalanceDelta = RebalanceDelta
  { rdRevoked :: ![(Text, Int32)]
  , rdAdded   :: ![(Text, Int32)]
  , rdAfter   :: ![(Text, Int32)]
  }
  deriving stock (Eq, Show, Generic)

-- | Compute the cooperative-rebalance delta for a member. Caller
-- supplies the member's current assignment (typically what
-- 'assignedPartitions' returned last); the function consults the
-- group state and computes what to revoke and what to add to
-- reach the new assignment.
cooperativeRebalance
  :: MockCluster
  -> GroupId
  -> MemberId
  -> [(Text, Int32)]                   -- ^ current assignment
  -> IO RebalanceDelta
cooperativeRebalance c grp mid current = do
  next <- assignmentFor c grp mid
  let !cs = Set.fromList current
      !ns = Set.fromList next
      !revoked = Set.toAscList (Set.difference cs ns)
      !added   = Set.toAscList (Set.difference ns cs)
  pure RebalanceDelta
    { rdRevoked = revoked
    , rdAdded   = added
    , rdAfter   = Set.toAscList ns
    }

----------------------------------------------------------------------
-- KRaft mode
----------------------------------------------------------------------

setKRaftRole :: MockCluster -> KRaftRole -> IO ()
setKRaftRole c r = atomically (writeTVar (mcKRaftRole c) r)

kraftRole :: MockCluster -> IO KRaftRole
kraftRole = readTVarIO . mcKRaftRole

-- | The current KRaft controller, or 'Nothing' if one hasn't been
-- elected.
controllerBroker :: MockCluster -> IO (Maybe BrokerId)
controllerBroker = readTVarIO . mcController

setControllerBroker :: MockCluster -> Maybe BrokerId -> IO ()
setControllerBroker c b = atomically (writeTVar (mcController c) b)

----------------------------------------------------------------------
-- Re-authentication
----------------------------------------------------------------------

-- | Set the wall-clock-aligned (cluster-clock) deadline at which
-- re-auth is required. 'Just t' arms the window; 'Nothing' clears
-- it.
setReauthDeadline :: MockCluster -> Maybe Int64 -> IO ()
setReauthDeadline c d = atomically (writeTVar (mcReauthDl c) d)

reauthDeadline :: MockCluster -> IO (Maybe Int64)
reauthDeadline = readTVarIO . mcReauthDl

-- | True iff a re-auth deadline is set and the cluster's clock
-- has passed it.
isReauthExpired :: MockCluster -> IO Bool
isReauthExpired c = do
  mDl <- readTVarIO (mcReauthDl c)
  case mDl of
    Nothing -> pure False
    Just dl -> do
      now <- readTVarIO (mcClock c)
      pure (now > dl)

----------------------------------------------------------------------
-- Inspection helpers
----------------------------------------------------------------------

dumpPartition :: MockCluster -> Text -> Int32 -> IO [StoredRecord]
dumpPartition c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
    Nothing -> pure []
    Just p  -> F.toList <$> readTVarIO (mpLog p)

partitionLogSize :: MockCluster -> Text -> Int32 -> IO Int
partitionLogSize c topic part = do
  topics <- readTVarIO (mcTopics c)
  case Map.lookup topic topics >>= IntMap.lookup (fromIntegral part) . mtPartitions of
    Nothing -> pure 0
    Just p  -> Seq.length <$> readTVarIO (mpLog p)
