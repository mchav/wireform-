{-# LANGUAGE OverloadedStrings #-}

-- | Final-batch librdkafka mock-test ports:
-- telemetry counters (0150), consumer-group generation id (0147),
-- KRaft controller role (0148), reauthentication deadline (0142).
module Client.MockBrokerProtoSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Telemetry

tests :: TestTree
tests = testGroup "MockBrokerProto"
  [ -- Telemetry
    telemetry_starts_at_zero
  , telemetry_bumps_independently_per_op
  , telemetry_snapshot_is_consistent_under_increment
    -- Generation id
  , generation_starts_at_zero
  , generation_bumps_on_join
  , generation_bumps_on_leave
  , generation_per_group_independent
    -- KRaft
  , kraft_default_combined
  , kraft_role_round_trips
  , controller_initial_is_first_broker
  , controller_can_be_reassigned
    -- Reauth
  , reauth_deadline_default_unset
  , reauth_deadline_round_trips
  , reauth_expired_when_clock_advances_past_deadline
  ]

----------------------------------------------------------------------
-- Telemetry
----------------------------------------------------------------------

telemetry_starts_at_zero :: TestTree
telemetry_starts_at_zero =
  testCase "newTelemetryCounters: every counter starts at 0" $ do
    tc <- newTelemetryCounters
    s  <- snapshotCounters tc
    tsProduce s   @?= 0
    tsFetch s     @?= 0
    tsCommit s    @?= 0
    tsTxnBegin s  @?= 0
    tsTxnCommit s @?= 0
    tsTxnAbort s  @?= 0

telemetry_bumps_independently_per_op :: TestTree
telemetry_bumps_independently_per_op =
  testCase "each bump* function targets only its counter" $ do
    tc <- newTelemetryCounters
    bumpProduce tc
    bumpProduce tc
    bumpFetch   tc
    bumpCommit  tc
    bumpCommit  tc
    bumpCommit  tc
    bumpTxnBegin  tc
    bumpTxnCommit tc
    bumpTxnAbort  tc
    s <- snapshotCounters tc
    tsProduce s   @?= 2
    tsFetch s     @?= 1
    tsCommit s    @?= 3
    tsTxnBegin s  @?= 1
    tsTxnCommit s @?= 1
    tsTxnAbort s  @?= 1

telemetry_snapshot_is_consistent_under_increment :: TestTree
telemetry_snapshot_is_consistent_under_increment =
  testCase "snapshotCounters reads consistently after a sequence of bumps" $ do
    tc <- newTelemetryCounters
    mapM_ (\_ -> bumpProduce tc) [1 .. 1000 :: Int]
    s <- snapshotCounters tc
    tsProduce s @?= 1000

----------------------------------------------------------------------
-- Generation id
----------------------------------------------------------------------

generation_starts_at_zero :: TestTree
generation_starts_at_zero =
  testCase "currentGeneration on a never-joined group is 0" $ do
    c <- newMockCluster 1
    g <- currentGeneration c (GroupId "g")
    g @?= GenerationId 0

generation_bumps_on_join :: TestTree
generation_bumps_on_join =
  testCase "joinGroup bumps the generation id" $ do
    c <- newMockCluster 1
    let g = GroupId "g"
    joinGroup c g (MemberId "m1") []
    currentGeneration c g >>= (@?= GenerationId 1)
    joinGroup c g (MemberId "m2") []
    currentGeneration c g >>= (@?= GenerationId 2)

generation_bumps_on_leave :: TestTree
generation_bumps_on_leave =
  testCase "leaveGroup also bumps the generation id" $ do
    c <- newMockCluster 1
    let g = GroupId "g"
    joinGroup c g (MemberId "m1") []
    currentGeneration c g >>= (@?= GenerationId 1)
    leaveGroup c g (MemberId "m1")
    currentGeneration c g >>= (@?= GenerationId 2)

generation_per_group_independent :: TestTree
generation_per_group_independent =
  testCase "each group has its own generation counter" $ do
    c <- newMockCluster 1
    let g1 = GroupId "g1"
        g2 = GroupId "g2"
    joinGroup c g1 (MemberId "m1") []
    joinGroup c g1 (MemberId "m2") []
    joinGroup c g2 (MemberId "m3") []
    currentGeneration c g1 >>= (@?= GenerationId 2)
    currentGeneration c g2 >>= (@?= GenerationId 1)

----------------------------------------------------------------------
-- KRaft
----------------------------------------------------------------------

kraft_default_combined :: TestTree
kraft_default_combined =
  testCase "newMockCluster defaults to KRaftCombined" $ do
    c <- newMockCluster 1
    kraftRole c >>= (@?= KRaftCombined)

kraft_role_round_trips :: TestTree
kraft_role_round_trips =
  testCase "setKRaftRole / kraftRole round-trip every variant" $ do
    c <- newMockCluster 1
    setKRaftRole c KRaftBroker
    kraftRole c >>= (@?= KRaftBroker)
    setKRaftRole c KRaftController
    kraftRole c >>= (@?= KRaftController)
    setKRaftRole c KRaftCombined
    kraftRole c >>= (@?= KRaftCombined)

controller_initial_is_first_broker :: TestTree
controller_initial_is_first_broker =
  testCase "the cluster's initial controller is the first broker" $ do
    c <- newMockCluster 3
    controllerBroker c >>= (@?= Just (BrokerId 0))

controller_can_be_reassigned :: TestTree
controller_can_be_reassigned =
  testCase "setControllerBroker re-points the controller" $ do
    c <- newMockCluster 3
    setControllerBroker c (Just (BrokerId 2))
    controllerBroker c >>= (@?= Just (BrokerId 2))
    setControllerBroker c Nothing
    controllerBroker c >>= (@?= Nothing)

----------------------------------------------------------------------
-- Reauth
----------------------------------------------------------------------

reauth_deadline_default_unset :: TestTree
reauth_deadline_default_unset =
  testCase "fresh cluster has no reauth deadline" $ do
    c <- newMockCluster 1
    reauthDeadline c >>= (@?= Nothing)
    isReauthExpired c >>= (@?= False)

reauth_deadline_round_trips :: TestTree
reauth_deadline_round_trips =
  testCase "setReauthDeadline / reauthDeadline round-trips" $ do
    c <- newMockCluster 1
    setReauthDeadline c (Just 1000)
    reauthDeadline c >>= (@?= Just 1000)
    setReauthDeadline c Nothing
    reauthDeadline c >>= (@?= Nothing)

reauth_expired_when_clock_advances_past_deadline :: TestTree
reauth_expired_when_clock_advances_past_deadline =
  testCase "isReauthExpired flips True when tickClock crosses the deadline" $ do
    c <- newMockCluster 1
    setReauthDeadline c (Just 100)
    isReauthExpired c >>= (@?= False)
    tickClock c 50
    isReauthExpired c >>= (@?= False)
    tickClock c 60          -- now at 110 > 100
    isReauthExpired c >>= (@?= True)
