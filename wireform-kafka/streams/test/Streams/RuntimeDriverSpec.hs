{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the 'KafkaStreams' runtime driven through a
-- 'StreamDriver' rather than against a real broker. The runtime
-- consumes the same 'StreamDriver' record-of-IO that
-- 'newNativeDriver' returns; this spec uses 'newMockDriver' to
-- assert end-to-end behaviour deterministically.
--
-- We cover:
--
--   * Source records pumped into the mock driver flow through
--     the topology and produce sink records the mock driver
--     captures.
--   * Subscribe / commit / close IO actions fire in the
--     expected order.
--   * Pause halts engine feeding without halting polling.
--   * EOS-V2 commit cycles route through the bound
--     'EOSCoordinator' (begin → commitOffsets → commit).
module Streams.RuntimeDriverSpec (tests) where

import qualified Control.Concurrent
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Kafka.Client.Consumer as KC
import qualified Kafka.Client.RebalanceListener as RBL

import Kafka.Streams
import Kafka.Streams.Runtime.EOS
  ( EOSCoordinator (..)
  )
import qualified Data.Map.Strict as Map
import Kafka.Streams.Runtime.NativeDriver
  ( MockSend (..)
  , MockTxnEvent (..)
  , RebalanceEvent (..)
  , mockDriverCommitCount
  , mockDriverDrainSends
  , mockDriverInjectPoll
  , mockDriverInjectRebalance
  , mockDriverTxnLog
  , newMockDriver
  )

tests :: TestTree
tests = testGroup "Runtime <-> StreamDriver"
  [ records_pumped_through_topology
  , pause_stops_engine_but_not_polling
  , commit_cycle_invokes_eos_coordinator
  , multi_thread_runtime_dispatches_by_partition
  , multi_thread_runtime_drains_collectors_at_commit
  , rebalance_assigned_updates_owned_partitions
  , rebalance_revoked_moves_to_standby_grace
  , rebalance_lost_drops_without_grace
  , rebalance_reassignment_promotes_standby
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Build a one-source / one-sink topology that uppercases its
-- value. Used as the lightweight workload for the runtime tests.
buildUpcaseTopo :: IO TopologyValid
buildUpcaseTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  upper <- mapValues (T.toUpper :: Text -> Text) s
  toTopic (topicName "out") (produced textSerde textSerde) upper
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

consumerRecord :: Text -> Text -> Text -> Int64ish -> Int64ish -> KC.ConsumerRecord
consumerRecord topic k v off ts = KC.ConsumerRecord
  { KC.crTopic     = topic
  , KC.crPartition = 0
  , KC.crOffset    = fromIntegral (unI64 off)
  , KC.crTimestamp = fromIntegral (unI64 ts)
  , KC.crKey       = Just (bytes k)
  , KC.crValue     = bytes v
  , KC.crHeaders   = []
  }

newtype Int64ish = Int64ish { unI64 :: Integer }

----------------------------------------------------------------------
-- 1. Records flow through the topology
----------------------------------------------------------------------

records_pumped_through_topology :: TestTree
records_pumped_through_topology =
  testCase "records injected via the mock driver produce sink records" $ do
    topo <- buildUpcaseTopo
    let cfg = defaultStreamsConfig
          { applicationId    = "rt-app-1"
          , bootstrapServers = ["mock:0"]
          , numStreamThreads = 1
          , pollMs           = 0
          }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    -- Push two records the runtime will see on the next poll.
    mockDriverInjectPoll h
      [ consumerRecord "in" "k1" "hello" (Int64ish 0) (Int64ish 100)
      , consumerRecord "in" "k2" "world" (Int64ish 1) (Int64ish 101)
      ]

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- Wait deterministically: the runtime keeps polling, the
    -- queue is finite, and on every pass the commit count goes
    -- up. Once we've seen at least one commit AND the captured
    -- sends contain both records we know the batch was processed.
    sentRef <- newIORef []
    waitFor 1000 $ do
      sends <- mockDriverDrainSends h
      modifyIORef' sentRef (++ sends)
      acc <- readIORef sentRef
      pure (length acc == 2 && all ((== "out") . mockSendTopic) acc)

    finalSends <- readIORef sentRef
    map (unbytes . mockSendValue) finalSends @?= ["HELLO", "WORLD"]

    closeKafkaStreams ks
    awaitState ks StreamsClosed

----------------------------------------------------------------------
-- 2. Pause stops feeding the engine but keeps polling
----------------------------------------------------------------------

pause_stops_engine_but_not_polling :: TestTree
pause_stops_engine_but_not_polling =
  testCase "pause halts engine forwarding but keeps polling" $ do
    topo <- buildUpcaseTopo
    let cfg = defaultStreamsConfig
          { applicationId    = "rt-app-2"
          , bootstrapServers = ["mock:0"]
          , numStreamThreads = 1
          , pollMs           = 0
          }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    pauseKafkaStreams ks
    isPausedKafkaStreams ks >>= (@?= True)

    -- Inject records while paused; nothing should reach the sink.
    mockDriverInjectPoll h
      [ consumerRecord "in" "k1" "while-paused" (Int64ish 0) (Int64ish 100)
      ]

    -- Drain a few times so the loop has run; the runtime is still
    -- polling so the queue empties, but no sends fire.
    let drainPolls 0 = pure ()
        drainPolls n = do
          mockDriverInjectPoll h []
          drainPolls (n - 1)
    drainPolls 10

    sendsWhilePaused <- mockDriverDrainSends h
    sendsWhilePaused @?= []

    -- Resume and inject again. Now records should flow.
    resumeKafkaStreams ks
    mockDriverInjectPoll h
      [ consumerRecord "in" "k2" "after-resume" (Int64ish 1) (Int64ish 101)
      ]
    waitFor 1000 $ do
      ss <- mockDriverDrainSends h
      pure (any (\s -> mockSendValue s == bytes "AFTER-RESUME") ss)

    closeKafkaStreams ks
    awaitState ks StreamsClosed

----------------------------------------------------------------------
-- 3. EOS-V2 commit cycle routes through the coordinator
----------------------------------------------------------------------

commit_cycle_invokes_eos_coordinator :: TestTree
commit_cycle_invokes_eos_coordinator =
  testCase "EOS commit cycle: runtime drives begin → commitOffsets → commit through the coordinator" $ do
    topo <- buildUpcaseTopo
    let cfg = defaultStreamsConfig
          { applicationId       = "rt-app-3"
          , bootstrapServers    = ["mock:0"]
          , numStreamThreads    = 1
          , pollMs              = 0
          , processingGuarantee = ExactlyOnceV2
          }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    -- Recording coordinator: every callback appends a tag.
    callsRef <- newIORef ([] :: [Text])
    let log_ s = modifyIORef' callsRef (s :)
    let coord = EOSCoordinator
          { eosInit          = log_ "init"   *> pure (Right ())
          , eosBegin         = log_ "begin"  *> pure (Right ())
          , eosCommit        = log_ "commit" *> pure (Right ())
          , eosAbort         = log_ "abort"  *> pure (Right ())
          , eosCommitOffsets = \_ _ ->
              log_ "commitOffsets" *> pure (Right ())
          }
    applyEOSCoordinator ks coord

    mockDriverInjectPoll h
      [ consumerRecord "in" "k1" "x" (Int64ish 0) (Int64ish 100)
      ]
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- Wait for at least one full begin/commitOffsets/commit
    -- cycle to fire by observing the coordinator log.
    waitFor 1000 $ do
      cs <- reverse <$> readIORef callsRef
      pure (cs == ["begin", "commitOffsets", "commit"]
             || take 3 cs == ["begin", "commitOffsets", "commit"])

    closeKafkaStreams ks
    awaitState ks StreamsClosed

    cs <- reverse <$> readIORef callsRef
    -- The first three entries in order must be the begin /
    -- commitOffsets / commit triple.
    take 3 cs @?= ["begin", "commitOffsets", "commit"]

    -- Ensure the runtime did NOT drive the producer's transaction
    -- hooks itself (those go through the coordinator under the
    -- AtLeastOnce defaults of the mock driver).
    txn <- mockDriverTxnLog h
    txn @?= []

    -- And it did call the regular consumer commit at least once
    -- (the runtime issues sdConsumerCommit after every successful
    -- commit cycle).
    cnt <- mockDriverCommitCount h
    assertBool "runtime committed consumer offsets at least once"
      (cnt >= 1)

----------------------------------------------------------------------
-- 4. Multi-thread runtime: per-partition dispatch
----------------------------------------------------------------------

multi_thread_runtime_dispatches_by_partition :: TestTree
multi_thread_runtime_dispatches_by_partition =
  testCase "numStreamThreads=4: every partition's records reach the sink" $ do
    topo <- buildUpcaseTopo
    let cfg = defaultStreamsConfig
          { applicationId    = "rt-multi-1"
          , bootstrapServers = ["mock:0"]
          , numStreamThreads = 4
          , pollMs           = 0
          }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    let recOn p = consumerRecordPart "in" p ("k" <> textShow p) "hello"
                    (Int64ish (fromIntegral p)) (Int64ish 100)
    -- 8 records spread across 4 partitions (2 per partition).
    mockDriverInjectPoll h
      [ recOn 0, recOn 1, recOn 2, recOn 3
      , recOn 0, recOn 1, recOn 2, recOn 3
      ]

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    sentRef <- newIORef []
    waitFor 2000 $ do
      sends <- mockDriverDrainSends h
      modifyIORef' sentRef (++ sends)
      acc <- readIORef sentRef
      pure (length acc == 8 && all ((== "out") . mockSendTopic) acc)

    finalSends <- readIORef sentRef
    -- Order across workers isn't guaranteed (parallel), but
    -- every send must be the upper-cased value and there must
    -- be exactly 8 of them.
    map (unbytes . mockSendValue) finalSends @?=
      replicate 8 "HELLO"

    closeKafkaStreams ks
    awaitState ks StreamsClosed

----------------------------------------------------------------------
-- 5. Multi-thread runtime: collector drain + IQ federation
----------------------------------------------------------------------

multi_thread_runtime_drains_collectors_at_commit :: TestTree
multi_thread_runtime_drains_collectors_at_commit =
  testCase "numStreamThreads=2: per-key counts are partition-local + federated read returns the union" $ do
    topo <- buildCountTopo
    let cfg = defaultStreamsConfig
          { applicationId    = "rt-multi-2"
          , bootstrapServers = ["mock:0"]
          , numStreamThreads = 2
          , pollMs           = 0
          }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    -- Three distinct keys on three distinct partitions; the
    -- (topic, partition) hash decides which worker each lands
    -- on, and we don't care which one — only that every key
    -- shows up in the federated IQ view with the right count.
    mockDriverInjectPoll h
      [ consumerRecordPart "in" 0 "alice" "1" (Int64ish 0) (Int64ish 100)
      , consumerRecordPart "in" 1 "bob"   "1" (Int64ish 1) (Int64ish 100)
      , consumerRecordPart "in" 2 "carol" "1" (Int64ish 2) (Int64ish 100)
      , consumerRecordPart "in" 0 "alice" "1" (Int64ish 3) (Int64ish 100)
      , consumerRecordPart "in" 0 "alice" "1" (Int64ish 4) (Int64ish 100)
      ]

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- Wait until the federated count shows the expected totals.
    waitFor 2000 $ do
      mIQ <- queryKVStore @Text @Int64 ks (storeName "counts")
      case mIQ of
        Nothing -> pure False
        Just kvs -> do
          a <- roKvGet kvs "alice"
          b <- roKvGet kvs "bob"
          c <- roKvGet kvs "carol"
          pure (a == Just 3 && b == Just 1 && c == Just 1)

    closeKafkaStreams ks
    awaitState ks StreamsClosed

----------------------------------------------------------------------
-- Helpers shared by multi-thread tests
----------------------------------------------------------------------

textShow :: Show a => a -> Text
textShow = T.pack . show

-- | Build a count-by-key topology. Used by the IQ federation test.
buildCountTopo :: IO TopologyValid
buildCountTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = grouped textSerde textSerde
      kgs = groupByKey g s
  _ <- countStream (materializedAs (storeName "counts")) kgs
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

consumerRecordPart
  :: Text -> Int -> Text -> Text -> Int64ish -> Int64ish
  -> KC.ConsumerRecord
consumerRecordPart topic part k v off ts = KC.ConsumerRecord
  { KC.crTopic     = topic
  , KC.crPartition = fromIntegral part
  , KC.crOffset    = fromIntegral (unI64 off)
  , KC.crTimestamp = fromIntegral (unI64 ts)
  , KC.crKey       = Just (bytes k)
  , KC.crValue     = bytes v
  , KC.crHeaders   = []
  }

----------------------------------------------------------------------
-- 6. Multi-instance rebalance handling (KIP-415/429/441/869)
----------------------------------------------------------------------

-- | Build a 'KafkaStreams' running the upcase topology with the
-- supplied 'taskTimeoutMs' (which the runtime uses as the
-- standby grace window).
buildRebalanceFixture
  :: Int                                     -- ^ task.timeout.ms
  -> IO (KafkaStreams, IORef [(Text, [KC.TopicPartition])])
buildRebalanceFixture graceMs = do
  topo <- buildUpcaseTopo
  let cfg = defaultStreamsConfig
        { applicationId    = "rt-rebal"
        , bootstrapServers = ["mock:0"]
        , numStreamThreads = 1
        , pollMs           = 0
        , taskTimeoutMs    = graceMs
        }
  ks <- newKafkaStreams cfg topo
  log_ <- newIORef ([] :: [(Text, [KC.TopicPartition])])
  let rec_ tag tps = modifyIORef' log_ ((tag, tps) :)
  setRebalanceListener ks RBL.RebalanceListener
    { RBL.rlOnAssigned = rec_ "assigned"
    , RBL.rlOnRevoked  = rec_ "revoked"
    , RBL.rlOnLost     = rec_ "lost"
    }
  pure (ks, log_)

rebalance_assigned_updates_owned_partitions :: TestTree
rebalance_assigned_updates_owned_partitions =
  testCase "RebalanceAssigned: ksOwned gains the new tps; listener fires" $ do
    (ks, log_) <- buildRebalanceFixture 0
    (drv, h) <- newMockDriver
    let tp0 = KC.TopicPartition "in" 0
        tp1 = KC.TopicPartition "in" 1
    mockDriverInjectRebalance h (RebalanceAssigned [tp0, tp1])
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    waitFor 1000 $ do
      owned <- ownedPartitions ks
      pure (length owned == 2)

    owned <- ownedPartitions ks
    -- Order is not deterministic across HashSet; compare as sets.
    map (\(KC.TopicPartition t p) -> (t, p)) (sortBy_ owned)
      @?= [("in", 0), ("in", 1)]

    entries <- reverse <$> readIORef log_
    map fst entries @?= ["assigned"]
    map snd entries @?= [[tp0, tp1]]

    closeKafkaStreams ks
    awaitState ks StreamsClosed

rebalance_revoked_moves_to_standby_grace :: TestTree
rebalance_revoked_moves_to_standby_grace =
  testCase "RebalanceRevoked with non-zero grace: tp moves to ksStandbys" $ do
    (ks, _log) <- buildRebalanceFixture 60_000
    (drv, h) <- newMockDriver
    let tp = KC.TopicPartition "in" 0
    mockDriverInjectRebalance h (RebalanceAssigned [tp])
    mockDriverInjectRebalance h (RebalanceRevoked  [tp])
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- After draining both events: owned must be empty AND
    -- the revoked tp must appear in the standby map with a
    -- deadline a non-trivial time in the future.
    waitFor 1000 $ do
      owned <- ownedPartitions ks
      stby  <- standbyTasks ks
      pure (null owned && Map.member tp stby)

    owned <- ownedPartitions ks
    owned @?= []
    stby  <- standbyTasks ks
    -- The deadline should be a real timestamp, > 0.
    case Map.lookup tp stby of
      Just dl -> assertBool ("standby deadline > 0; got " <> show dl) (dl > 0)
      Nothing -> error "expected standby entry"

    closeKafkaStreams ks
    awaitState ks StreamsClosed

rebalance_lost_drops_without_grace :: TestTree
rebalance_lost_drops_without_grace =
  testCase "RebalanceLost: tp drops immediately; no standby entry; listener fires" $ do
    (ks, log_) <- buildRebalanceFixture 60_000
    (drv, h) <- newMockDriver
    let tp = KC.TopicPartition "in" 0
    mockDriverInjectRebalance h (RebalanceAssigned [tp])
    mockDriverInjectRebalance h (RebalanceLost     [tp])
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    waitFor 1000 $ do
      owned <- ownedPartitions ks
      stby  <- standbyTasks ks
      tags  <- map fst <$> readIORef log_
      pure (null owned && Map.null stby
             && "lost" `elem` tags)

    -- The listener saw both assigned then lost, in order.
    entries <- reverse <$> readIORef log_
    map fst entries @?= ["assigned", "lost"]

    closeKafkaStreams ks
    awaitState ks StreamsClosed

rebalance_reassignment_promotes_standby :: TestTree
rebalance_reassignment_promotes_standby =
  testCase "Re-assignment of a standby tp clears the standby and re-adds to owned" $ do
    (ks, _log) <- buildRebalanceFixture 60_000
    (drv, h) <- newMockDriver
    let tp = KC.TopicPartition "in" 0
    -- assign -> revoke (standby) -> re-assign (promote back to active)
    mockDriverInjectRebalance h (RebalanceAssigned [tp])
    mockDriverInjectRebalance h (RebalanceRevoked  [tp])
    mockDriverInjectRebalance h (RebalanceAssigned [tp])
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    waitFor 1000 $ do
      owned <- ownedPartitions ks
      stby  <- standbyTasks ks
      pure (tp `elem` owned && Map.null stby)

    owned <- ownedPartitions ks
    owned @?= [tp]
    stby  <- standbyTasks ks
    Map.null stby @?= True

    closeKafkaStreams ks
    awaitState ks StreamsClosed

-- | Stable sort over TopicPartition for the assigned-tps test.
sortBy_ :: [KC.TopicPartition] -> [KC.TopicPartition]
sortBy_ = sortOn (\(KC.TopicPartition t p) -> (t, p))
  where
    sortOn f = sortBy (\a b -> compare (f a) (f b))
    sortBy cmp = foldr insert' []
      where
        insert' x [] = [x]
        insert' x (y : ys)
          | cmp x y == GT = y : insert' x ys
          | otherwise     = x : y : ys

----------------------------------------------------------------------
-- Wait helper (no threadDelay)
----------------------------------------------------------------------

-- | Spin on the IO predicate up to N tries with @yieldM@-style
-- short waits between checks. We use 'Control.Concurrent.yield'
-- to nudge the scheduler instead of sleeping for a fixed
-- duration. If we exceed the cap, fail loudly so the test isn't
-- silently flaky.
waitFor :: Int -> IO Bool -> IO ()
waitFor 0 _ = error "waitFor: timed out"
waitFor n act = do
  ok <- act
  if ok
    then pure ()
    else do
      -- Yielding rather than sleeping keeps the test fast on
      -- a quiet machine; on a busy one it just lets the worker
      -- thread run.
      Control.Concurrent.yield
      waitFor (n - 1) act
