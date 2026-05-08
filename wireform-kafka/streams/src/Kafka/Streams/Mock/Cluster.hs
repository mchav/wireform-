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
  , currentTxnEpoch
    -- * Group rebalance / assignment
  , MemberId (..)
  , joinGroup
  , leaveGroup
  , membersOf
  , assignmentFor
    -- * Inspection
  , dumpPartition
  , partitionLogSize
  ) where

import Control.Concurrent.STM
import Control.Monad (forM_, unless, when)
import qualified Data.List as L
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
  , mcGroupMembers   :: !(TVar (Map GroupId [(MemberId, Set TopicName)]))
  , mcTxns           :: !(TVar (Map TxnId TxnState))
  , mcTxnEpoch       :: !(TVar (Map TxnId Int32))
    -- ^ Current epoch for each transactional id. 'commitTxn' /
    -- 'abortTxn' bump it; producers stamped with a stale epoch
    -- are fenced on their next send.
  , mcBrokers        :: !(TVar [BrokerId])
  , mcDownBrokers    :: !(TVar (Set BrokerId))
  , mcClock          :: !(TVar Timestamp)
  }

type GroupOffsets = Map (TopicName, Int32) Int64

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
  gs <- newTVarIO Map.empty
  gm <- newTVarIO Map.empty
  xs <- newTVarIO Map.empty
  ep <- newTVarIO Map.empty
  bs <- newTVarIO [BrokerId i | i <- [0 .. n - 1]]
  ds <- newTVarIO Set.empty
  ck <- newTVarIO (Timestamp 0)
  pure MockCluster
    { mcTopics       = ts
    , mcGroups       = gs
    , mcGroupMembers = gm
    , mcTxns         = xs
    , mcTxnEpoch     = ep
    , mcBrokers      = bs
    , mcDownBrokers  = ds
    , mcClock        = ck
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
-- assigned offset, 'Left "fenced"' if the producer's epoch has been
-- superseded, or 'Left "no partition"' if the partition doesn't
-- exist. Carries headers (mirrors the JVM client's
-- @ProducerRecord.headers@).
appendToPartition
  :: MockCluster
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> [(Text, ByteString)]               -- ^ headers
  -> Maybe ProducerStamp
  -> IO (Either String Int64)
appendToPartition c topic part mk v ts hdrs stamp = atomically $ do
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
    else case Map.lookup topic topics >>= Map.lookup part . mtPartitions of
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
              , srHeaders   = hdrs
              , srProducer  = stamp
              }
        modifyTVar' (mpLog p) (|> rec)
        writeTVar (mpHwm p) (hwm + 1)
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
  modifyTVar' (mcTxns c) (Map.adjust (const TxnCommitted) tid)
  -- Bump the epoch so any in-flight producer with the old epoch
  -- gets fenced on its next send.
  modifyTVar' (mcTxnEpoch c) (Map.adjust (+ 1) tid)
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
  modifyTVar' (mcTxnEpoch c) (Map.adjust (+ 1) tid)
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
  -> [TopicName]
  -> IO ()
joinGroup c grp mid topics = atomically $
  modifyTVar' (mcGroupMembers c) $ \m ->
    let cur = Map.findWithDefault [] grp m
        !next = filter ((/= mid) . fst) cur
                ++ [(mid, Set.fromList topics)]
    in Map.insert grp next m

leaveGroup :: MockCluster -> GroupId -> MemberId -> IO ()
leaveGroup c grp mid = atomically $
  modifyTVar' (mcGroupMembers c) $ \m ->
    Map.adjust (filter ((/= mid) . fst)) grp m

membersOf :: MockCluster -> GroupId -> IO [MemberId]
membersOf c grp = do
  m <- readTVarIO (mcGroupMembers c)
  pure (map fst (Map.findWithDefault [] grp m))

-- | Compute this member's currently-assigned partitions under a
-- deterministic round-robin assignor across the group's members.
-- The assignor is sticky enough for tests: members are sorted by
-- 'MemberId', topics by 'TopicName', and partitions are dealt out
-- in order.
assignmentFor
  :: MockCluster -> GroupId -> MemberId -> IO [(TopicName, Int32)]
assignmentFor c grp mid = do
  members <- readTVarIO (mcGroupMembers c)
  topics  <- readTVarIO (mcTopics c)
  let !grpMems = Map.findWithDefault [] grp members
      !sorted  = L.sortBy (\(a, _) (b, _) -> compare (unMemberId a) (unMemberId b)) grpMems
  case lookupIdx mid sorted of
    Nothing  -> pure []
    Just self -> do
      let !subscribed = Set.unions (map snd sorted)
          !sortedTopics = L.sortBy
                            (\a b -> compare (unTopicName a) (unTopicName b))
                            (Set.toList subscribed)
          !allParts = expandParts topics sortedTopics
          !memberCount = length sorted
          !assigned = takeEvery memberCount self allParts
      pure assigned
  where
    lookupIdx :: MemberId -> [(MemberId, a)] -> Maybe Int
    lookupIdx m = go 0
      where
        go _  []                = Nothing
        go !i ((mm, _) : rest)
          | mm == m   = Just i
          | otherwise = go (i + 1) rest

    expandParts :: Map TopicName MockTopic -> [TopicName] -> [(TopicName, Int32)]
    expandParts topicMap = go []
      where
        go acc []     = reverse acc
        go acc (t:ts) = case Map.lookup t topicMap of
          Nothing -> go acc ts
          Just mt ->
            let !n = Map.size (mtPartitions mt)
                !parts = map (\i -> (t, fromIntegral i)) [0 .. n - 1]
             in go (reverse parts ++ acc) ts

    -- Round-robin: keep entries whose 0-based index modulo
    -- 'memberCount' equals 'self'.
    takeEvery :: Int -> Int -> [a] -> [a]
    takeEvery n self xs = go 0 xs
      where
        go _  []     = []
        go !i (y:ys)
          | i `mod` n == self = y : go (i + 1) ys
          | otherwise        = go (i + 1) ys

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
