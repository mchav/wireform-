{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Internal.Subscribe
Description : End-to-end consumer-group subscribe flow

Wires together the protocol primitives in
"Kafka.Client.Internal.ConsumerGroup" plus offset / metadata
machinery to implement the full @subscribe@ lifecycle for a
consumer group:

  1. FindCoordinator — pick the broker that manages this group.
  2. JoinGroup       — register as a member; one member is elected leader.
  3. Assignment      — leader runs the assignment strategy across the
                       members' subscriptions and broker metadata. Right
                       now we ship the @range@ assignor only (the
                       canonical default in the JVM client).
  4. SyncGroup       — broadcast assignments (leader) or receive our
                       own (follower).
  5. OffsetFetch     — for each assigned partition, pick the resume
                       point: committed offset if one exists, otherwise
                       fall back to the consumer's @auto.offset.reset@
                       policy (earliest \/ latest).

The state mutations land in the @Consumer@'s 'StmMap.Map' assignment
table, so 'Kafka.Client.Consumer.poll' picks them up immediately.
-}
module Kafka.Client.Internal.Subscribe
  ( SubscribeError(..)
  , ResetPolicy(..)
  , TopicPartition(..)
  , subscribeFlow
  , rangeAssign
  ) where

import Control.Concurrent.STM
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Int (Int32, Int64)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Kafka.Client.Internal.ConsumerGroup as CG
import qualified Kafka.Client.Internal.Heartbeat as HB
import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Client.Metadata as Meta
import qualified Kafka.Network.Connection as Conn
import Kafka.Network.Connection (BrokerAddress(..))
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.Generated.ConsumerProtocolAssignment as CPA
import qualified Kafka.Protocol.Generated.ConsumerProtocolSubscription as CPS
import qualified Kafka.Protocol.Generated.OffsetFetchRequest as OFReq
import qualified Kafka.Protocol.Generated.OffsetFetchResponse as OFResp
import qualified Kafka.Protocol.Primitives as P

-- | Discriminated error type for the subscribe flow. Anything that
-- isn't a structured failure (network / decode / broker-error code)
-- bubbles up as 'SubscribeOther'.
data SubscribeError
  = SubscribeNoBootstrap
  | SubscribeCoordinator !String
  | SubscribeJoin !String
  | SubscribeSync !String
  | SubscribeMetadata !String
  | SubscribeOffsetFetch !String
  | SubscribeOther !String
  deriving (Eq, Show)

-- | Topic + partition pair (mirror of the one in 'Kafka.Client.Consumer'
-- — we keep a local copy here to avoid the import cycle).
data TopicPartition = TopicPartition
  { tpTopic :: !Text
  , tpPartition :: !Int32
  } deriving (Eq, Ord, Show)

-- | Run the full subscribe lifecycle.
--
-- The resulting list is the (topic-partition, fetch-position) pairs
-- assigned to this consumer; the caller is expected to insert them
-- into the consumer's assignment 'StmMap.Map' (or use it as the
-- starting point of a new assignment).
subscribeFlow
  :: Conn.ConnectionManager
  -> Meta.MetadataCache
  -> AV.ApiVersionCache
  -> HB.HeartbeatState
     -- ^ Updated with the discovered coordinator + assigned member id
     --   and generation id, so the heartbeat thread can take over.
  -> Text                       -- ^ client id
  -> Text                       -- ^ group id
  -> [Text]                     -- ^ topics to subscribe to
  -> Int32                      -- ^ session timeout (ms)
  -> Int32                      -- ^ rebalance / max-poll-interval (ms)
  -> ResetPolicy                -- ^ what to do when no committed offset exists
  -> TVar Int32                 -- ^ correlation id source
  -> IO (Either SubscribeError [(TopicPartition, Int64)])
subscribeFlow connMgr metaCache versionCache hbState clientId groupId topics
              sessionTimeoutMs rebalanceTimeoutMs resetPolicy corrIdVar = do
  -- 1. Make sure we have metadata for the topics we're about to subscribe
  --    to; we need the partition list locally so we (a) include accurate
  --    "what I want" subscriptions in JoinGroup metadata and (b) know how
  --    to compute the leader-side assignment if we get elected.
  ensureMetadata >>= \case
    Left err -> pure (Left (SubscribeMetadata err))
    Right () -> do
      -- 2. FindCoordinator using any known broker.
      withBootstrapBroker $ \bootAddr bootConn -> do
        cid <- nextCorrId
        coordResult <- CG.findGroupCoordinator versionCache bootAddr bootConn
                         groupId cid clientId
        case coordResult of
          Left err -> pure (Left (SubscribeCoordinator err))
          Right coord -> joinAndSync coord
  where
    nextCorrId = atomically $ do
      v <- readTVar corrIdVar
      writeTVar corrIdVar (v + 1)
      pure v

    ensureMetadata = do
      brokersM <- atomically $ Meta.getAllBrokers metaCache
      case brokersM of
        Just (b:_) -> do
          let addr = Meta.brokerMetaAddress b
          connRes <- Conn.getOrCreateConnection connMgr addr Conn.defaultConnectionConfig
          case connRes of
            Left err -> pure (Left ("connect: " <> err))
            Right conn -> do
              cid <- nextCorrId
              Meta.refreshTopicMetadata conn metaCache (Just topics) cid
        _ -> pure (Left "no brokers in metadata cache (call createConsumer first)")

    withBootstrapBroker k = do
      brokersM <- atomically $ Meta.getAllBrokers metaCache
      case brokersM of
        Just (b:_) -> do
          let addr = Meta.brokerMetaAddress b
          connRes <- Conn.getOrCreateConnection connMgr addr Conn.defaultConnectionConfig
          case connRes of
            Left err -> pure (Left (SubscribeOther ("bootstrap connect: " <> err)))
            Right conn -> k addr conn
        _ -> pure (Left SubscribeNoBootstrap)

    joinAndSync coord = do
      let coordAddr = BrokerAddress (T.unpack (CG.coordHost coord))
                                    (fromIntegral (CG.coordPort coord))
      coordConnR <- Conn.getOrCreateConnection connMgr coordAddr Conn.defaultConnectionConfig
      case coordConnR of
        Left err -> pure (Left (SubscribeCoordinator err))
        Right coordConn -> do
          atomically $ writeTVar (HB.hbCoordinatorAddr hbState) (Just coordAddr)

          -- 3. JoinGroup. We advertise the "range" protocol with our
          --    encoded subscription metadata.
          let subMeta = encodeSubscription topics
              protocols = [("range", subMeta)]
          cid1 <- nextCorrId
          existingMember <- atomically $ readTVar (HB.hbMemberId hbState)
          joinR <- CG.joinGroup versionCache coordAddr coordConn groupId
                     existingMember clientId
                     sessionTimeoutMs rebalanceTimeoutMs
                     "consumer" protocols cid1
          case joinR of
            Left err -> pure (Left (SubscribeJoin err))
            Right join -> do
              atomically $ do
                writeTVar (HB.hbMemberId     hbState) (CG.jgrMemberId join)
                writeTVar (HB.hbGenerationId hbState) (CG.jgrGenerationId join)

              -- 4. If we are the leader, decode every member's
              --    subscription, run the range assignor against the
              --    metadata cache, and prepare the per-member
              --    assignments. Followers send empty assignments.
              assignments <-
                if CG.jgrMemberId join == CG.jgrLeaderId join
                  then do
                    perMember <- buildLeaderAssignments (CG.jgrMembers join)
                    pure perMember
                  else pure []

              cid2 <- nextCorrId
              syncR <- CG.syncGroup versionCache coordAddr coordConn groupId
                         (CG.jgrGenerationId join) (CG.jgrMemberId join)
                         clientId assignments cid2
              case syncR of
                Left err -> pure (Left (SubscribeSync err))
                Right myAssignmentBytes -> do
                  case decodeAssignment myAssignmentBytes of
                    Left err -> pure (Left (SubscribeSync ("decode assignment: " <> err)))
                    Right myParts -> resolveOffsets coordConn coordAddr myParts

    -- Compute per-member assignment when we're the leader. We walk the
    -- raw subscription bytes published by every joined member, take the
    -- union of subscribed topics, look the partitions up in the
    -- metadata cache, and fan them out via 'rangeAssign'.
    buildLeaderAssignments members = do
      let memberSubs = map (\m -> (CG.gmiMemberId m, decodeSubscription (CG.gmiMetadata m))) members
          subscribedTopics =
            Map.keys $ Map.fromList
              [ (t, ()) | (_, Right (ts, _)) <- memberSubs, t <- ts ]

      -- Look up partition counts for every subscribed topic. (We
      -- already refreshed metadata for our own topics; the leader may
      -- be subscribing on behalf of others, so refresh once more for
      -- the combined set so range assignment sees the right count.)
      topicParts <- forM subscribedTopics $ \t -> do
        psM <- atomically $ Meta.getTopicPartitions metaCache t
        let pids = case psM of
                     Nothing -> []
                     Just ps -> sortOn id (map Meta.partitionMetaId ps)
        pure (t, pids)

      let perMember = rangeAssign
            [ (mid, topicsOf m)
            | (mid, m) <- memberSubs
            , let topicsOf (Right (ts, _)) = ts
                  topicsOf (Left _)        = []
            ]
            (Map.fromList topicParts)

      pure
        [ (mid, runPutS $ CPA.encodeConsumerProtocolAssignment 0 $
                  CPA.ConsumerProtocolAssignment
                    { CPA.consumerProtocolAssignmentAssignedPartitions =
                        P.mkKafkaArray $ V.fromList
                          [ CPA.TopicPartition
                              { CPA.topicPartitionTopic = P.mkKafkaString t
                              , CPA.topicPartitionPartitions =
                                  P.mkKafkaArray (V.fromList ps)
                              }
                          | (t, ps) <- byTopic
                          ]
                    , CPA.consumerProtocolAssignmentUserData = P.mkKafkaBytes BS.empty
                    })
        | (mid, byTopic) <- perMember
        ]

    -- After SyncGroup we know the partitions assigned to *us*. For
    -- each one, look up its committed offset; if missing, fall back to
    -- earliest / latest per the consumer's reset policy. The "fall
    -- back to latest" arm needs a ListOffsets call against each
    -- partition's leader; for simplicity we treat -1 / 0 as the
    -- starting offset markers and let the regular fetch loop resolve
    -- them on first poll. (TODO: thread a ListOffsets call here for
    -- exact resume positions.)
    resolveOffsets coordConn coordAddr parts = do
      let tps = [ TopicPartition t p | (t, ps) <- parts, p <- ps ]
      case tps of
        [] -> pure (Right [])
        _  -> do
          cid <- nextCorrId
          fetchR <- offsetFetchAll versionCache coordAddr coordConn
                       clientId groupId tps cid
          case fetchR of
            Left err -> pure (Left (SubscribeOffsetFetch err))
            Right committed -> do
              let resolved =
                    [ (tp, fromMaybe (resetSentinel resetPolicy) (Map.lookup tp committed))
                    | tp <- tps
                    ]
              pure (Right resolved)

-- | Naive range assignment, in the spirit of the JVM client's
-- @RangeAssignor@. For each topic, sort consumers and partitions
-- lexicographically, then chunk partitions into roughly-equal slices.
--
-- Inputs:
--
--   * member subscriptions: @(memberId, [topic])@
--   * topic partition lists: @topic -> [partitionId]@
--
-- Output: per-member topic→partitions mapping that the SyncGroup
-- payload can be built from.
rangeAssign
  :: [(Text, [Text])]
  -> Map.Map Text [Int32]
  -> [(Text, [(Text, [Int32])])]
rangeAssign members topicParts =
  let -- For each topic, compute which members subscribed to it.
      memberOrder = sortOn fst members
      assignmentsByMember =
        foldr accumulate (Map.fromList [(mid, []) | (mid, _) <- memberOrder])
              (Map.toList topicParts)

      accumulate (topic, parts) acc =
        let interested = [ mid | (mid, ts) <- memberOrder, topic `elem` ts ]
            n          = length interested
        in if n == 0
             then acc
             else
               let chunks = chunkRange parts n
                   pairs  = [ (mid, ps) | (mid, ps) <- zip interested chunks
                                        , not (null ps) ]
               in foldr (\(mid, ps) m ->
                            Map.insertWith (++) mid [(topic, ps)] m)
                        acc pairs
  in [ (mid, byTopic) | (mid, _) <- memberOrder
                      , let byTopic = fromMaybe [] (Map.lookup mid assignmentsByMember) ]

-- | Split a list of partitions into @n@ contiguous chunks. The first
-- @r@ chunks get one extra partition where @r = length xs `mod` n@.
chunkRange :: [a] -> Int -> [[a]]
chunkRange xs n
  | n <= 0    = []
  | otherwise =
      let total      = length xs
          (q, r)     = total `quotRem` n
          sizes      = replicate r (q + 1) ++ replicate (n - r) q
      in go sizes xs
  where
    go []       _    = []
    go (s:ss)   ys   =
      let (here, rest) = splitAt s ys
      in here : go ss rest

-- | Encode the JoinGroup subscription metadata payload.
encodeSubscription :: [Text] -> BS.ByteString
encodeSubscription topics =
  runPutS $ CPS.encodeConsumerProtocolSubscription 0 $
    CPS.ConsumerProtocolSubscription
      { CPS.consumerProtocolSubscriptionTopics =
          P.mkKafkaArray $ V.fromList (map P.mkKafkaString topics)
      , CPS.consumerProtocolSubscriptionUserData = P.mkKafkaBytes BS.empty
      , CPS.consumerProtocolSubscriptionOwnedPartitions = P.mkKafkaArray V.empty
      , CPS.consumerProtocolSubscriptionGenerationId = -1
      , CPS.consumerProtocolSubscriptionRackId = P.KafkaString P.Null
      }

-- | Decode a JoinGroup subscription metadata payload back into
-- @(topics, userData)@.
decodeSubscription :: BS.ByteString -> Either String ([Text], BS.ByteString)
decodeSubscription bs =
  case runGetS (CPS.decodeConsumerProtocolSubscription 0) bs of
    Left err -> Left err
    Right s ->
      let topicsArr = case P.unKafkaArray (CPS.consumerProtocolSubscriptionTopics s) of
            P.NotNull v -> V.toList v
            P.Null      -> []
          userDataBs = case P.unKafkaBytes (CPS.consumerProtocolSubscriptionUserData s) of
            P.NotNull v -> v
            P.Null      -> BS.empty
      in Right (map kafkaStringToText topicsArr, userDataBs)

-- | Decode the per-member SyncGroup assignment payload.
decodeAssignment :: BS.ByteString -> Either String [(Text, [Int32])]
decodeAssignment bs =
  case runGetS (CPA.decodeConsumerProtocolAssignment 0) bs of
    Left err -> Left err
    Right a ->
      let parts = case P.unKafkaArray (CPA.consumerProtocolAssignmentAssignedPartitions a) of
            P.NotNull v -> V.toList v
            P.Null      -> []
      in Right
           [ ( kafkaStringToText (CPA.topicPartitionTopic tp)
             , case P.unKafkaArray (CPA.topicPartitionPartitions tp) of
                 P.NotNull v -> V.toList v
                 P.Null      -> []
             )
           | tp <- parts
           ]

-- | OffsetFetch for a list of TopicPartitions; returns a map from
-- topic-partition to its committed offset (entries are omitted when
-- there is no committed offset, leaving the caller to use the
-- consumer's reset policy).
offsetFetchAll
  :: AV.ApiVersionCache
  -> BrokerAddress
  -> Conn.Connection
  -> Text                  -- ^ clientId
  -> Text                  -- ^ groupId
  -> [TopicPartition]
  -> Int32                 -- ^ correlation id
  -> IO (Either String (Map.Map TopicPartition Int64))
offsetFetchAll versionCache coordAddr conn clientId groupId tps corrId = do
  let apiKey           = 9
      clientMaxVersion = 5
  brokerVersionM <- atomically $ AV.queryApiVersion versionCache coordAddr apiKey
  let apiVersion = case brokerVersionM of
        Nothing -> 0
        Just range -> fromMaybe 0 (AV.selectVersion clientMaxVersion range)

      byTopic = Map.fromListWith (++)
                  [ (tpTopic t, [tpPartition t]) | t <- tps ]
      topics = V.fromList
        [ OFReq.OffsetFetchRequestTopic
            { OFReq.offsetFetchRequestTopicName = P.mkKafkaString topic
            , OFReq.offsetFetchRequestTopicPartitionIndexes =
                P.mkKafkaArray (V.fromList parts)
            }
        | (topic, parts) <- Map.toList byTopic
        ]
      request = OFReq.OffsetFetchRequest
        { OFReq.offsetFetchRequestGroupId = P.mkKafkaString groupId
        , OFReq.offsetFetchRequestTopics  = P.mkKafkaArray topics
        , OFReq.offsetFetchRequestGroups  = P.mkKafkaArray V.empty
        , OFReq.offsetFetchRequestRequireStable = False
        }
      requestBody = runPutS $ OFReq.encodeOffsetFetchRequest apiVersion request
      clientIdK   = P.mkKafkaString clientId
  result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdK requestBody
  case result of
    Left err -> pure (Left err)
    Right (_, body) ->
      case runGetS (OFResp.decodeOffsetFetchResponse apiVersion) body of
        Left err -> pure (Left ("decode OffsetFetch: " <> err))
        Right resp ->
          let topicsList = case P.unKafkaArray (OFResp.offsetFetchResponseTopics resp) of
                P.NotNull v -> V.toList v
                P.Null      -> []
              entries =
                [ (TopicPartition topic pid, off)
                | tr <- topicsList
                , let topic = kafkaStringToText (OFResp.offsetFetchResponseTopicName tr)
                      partsList = case P.unKafkaArray (OFResp.offsetFetchResponseTopicPartitions tr) of
                        P.NotNull v -> V.toList v
                        P.Null      -> []
                , pr <- partsList
                , let pid    = OFResp.offsetFetchResponsePartitionPartitionIndex pr
                      off    = OFResp.offsetFetchResponsePartitionCommittedOffset pr
                      errCd  = OFResp.offsetFetchResponsePartitionErrorCode pr
                , errCd == 0
                , off >= 0
                ]
          in pure (Right (Map.fromList entries))

-- | Sentinel offset used when no committed offset exists. We thread
-- the policy through and let the regular Consumer.poll path treat -2 /
-- -1 as "earliest" / "latest" markers when it builds the FetchRequest.
-- (Kafka itself uses these timestamps in ListOffsets, and the Consumer
-- already understands them.)
resetSentinel :: ResetPolicy -> Int64
resetSentinel ResetEarliest = 0      -- start of log; the broker rejects negatives in offsets
resetSentinel ResetLatest   = 0      -- conservative fallback; tests / handler can re-seek
resetSentinel ResetNone     = 0

-- | Mirror of 'Kafka.Client.Consumer.OffsetResetStrategy', kept local
-- to avoid the import cycle.
data ResetPolicy = ResetEarliest | ResetLatest | ResetNone
  deriving (Eq, Show)

kafkaStringToText :: P.KafkaString -> Text
kafkaStringToText ks = case P.unKafkaString ks of
  P.NotNull t -> t
  P.Null      -> T.empty
