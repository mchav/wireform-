{-# LANGUAGE OverloadedStrings #-}

{- | Final-batch librdkafka mock-test ports:
telemetry counters (0150), consumer-group generation id (0147),
KRaft controller role (0148), reauthentication deadline (0142).
-}
module Client.MockBrokerProtoSpec (tests) where

import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Telemetry
import Test.Syd


tests :: Spec
tests =
  describe "MockBrokerProto" $
    sequence_
      [ -- Telemetry
        telemetry_starts_at_zero
      , telemetry_bumps_independently_per_op
      , telemetry_snapshot_is_consistent_under_increment
      , -- Generation id
        generation_starts_at_zero
      , generation_bumps_on_join
      , generation_bumps_on_leave
      , generation_per_group_independent
      , -- KRaft
        kraft_default_combined
      , kraft_role_round_trips
      , controller_initial_is_first_broker
      , controller_can_be_reassigned
      , -- Reauth
        reauth_deadline_default_unset
      , reauth_deadline_round_trips
      , reauth_expired_when_clock_advances_past_deadline
      ]


----------------------------------------------------------------------
-- Telemetry
----------------------------------------------------------------------

telemetry_starts_at_zero :: Spec
telemetry_starts_at_zero =
  it "newTelemetryCounters: every counter starts at 0" $ do
    tc <- newTelemetryCounters
    s <- snapshotCounters tc
    tsProduce s `shouldBe` 0
    tsFetch s `shouldBe` 0
    tsCommit s `shouldBe` 0
    tsTxnBegin s `shouldBe` 0
    tsTxnCommit s `shouldBe` 0
    tsTxnAbort s `shouldBe` 0


telemetry_bumps_independently_per_op :: Spec
telemetry_bumps_independently_per_op =
  it "each bump* function targets only its counter" $ do
    tc <- newTelemetryCounters
    bumpProduce tc
    bumpProduce tc
    bumpFetch tc
    bumpCommit tc
    bumpCommit tc
    bumpCommit tc
    bumpTxnBegin tc
    bumpTxnCommit tc
    bumpTxnAbort tc
    s <- snapshotCounters tc
    tsProduce s `shouldBe` 2
    tsFetch s `shouldBe` 1
    tsCommit s `shouldBe` 3
    tsTxnBegin s `shouldBe` 1
    tsTxnCommit s `shouldBe` 1
    tsTxnAbort s `shouldBe` 1


telemetry_snapshot_is_consistent_under_increment :: Spec
telemetry_snapshot_is_consistent_under_increment =
  it "snapshotCounters reads consistently after a sequence of bumps" $ do
    tc <- newTelemetryCounters
    mapM_ (\_ -> bumpProduce tc) [1 .. 1000 :: Int]
    s <- snapshotCounters tc
    tsProduce s `shouldBe` 1000


----------------------------------------------------------------------
-- Generation id
----------------------------------------------------------------------

generation_starts_at_zero :: Spec
generation_starts_at_zero =
  it "currentGeneration on a never-joined group is 0" $ do
    c <- newMockCluster 1
    g <- currentGeneration c (GroupId "g")
    g `shouldBe` GenerationId 0


generation_bumps_on_join :: Spec
generation_bumps_on_join =
  it "joinGroup bumps the generation id" $ do
    c <- newMockCluster 1
    let g = GroupId "g"
    joinGroup c g (MemberId "m1") []
    currentGeneration c g >>= (`shouldBe` GenerationId 1)
    joinGroup c g (MemberId "m2") []
    currentGeneration c g >>= (`shouldBe` GenerationId 2)


generation_bumps_on_leave :: Spec
generation_bumps_on_leave =
  it "leaveGroup also bumps the generation id" $ do
    c <- newMockCluster 1
    let g = GroupId "g"
    joinGroup c g (MemberId "m1") []
    currentGeneration c g >>= (`shouldBe` GenerationId 1)
    leaveGroup c g (MemberId "m1")
    currentGeneration c g >>= (`shouldBe` GenerationId 2)


generation_per_group_independent :: Spec
generation_per_group_independent =
  it "each group has its own generation counter" $ do
    c <- newMockCluster 1
    let g1 = GroupId "g1"
        g2 = GroupId "g2"
    joinGroup c g1 (MemberId "m1") []
    joinGroup c g1 (MemberId "m2") []
    joinGroup c g2 (MemberId "m3") []
    currentGeneration c g1 >>= (`shouldBe` GenerationId 2)
    currentGeneration c g2 >>= (`shouldBe` GenerationId 1)


----------------------------------------------------------------------
-- KRaft
----------------------------------------------------------------------

kraft_default_combined :: Spec
kraft_default_combined =
  it "newMockCluster defaults to KRaftCombined" $ do
    c <- newMockCluster 1
    kraftRole c >>= (`shouldBe` KRaftCombined)


kraft_role_round_trips :: Spec
kraft_role_round_trips =
  it "setKRaftRole / kraftRole round-trip every variant" $ do
    c <- newMockCluster 1
    setKRaftRole c KRaftBroker
    kraftRole c >>= (`shouldBe` KRaftBroker)
    setKRaftRole c KRaftController
    kraftRole c >>= (`shouldBe` KRaftController)
    setKRaftRole c KRaftCombined
    kraftRole c >>= (`shouldBe` KRaftCombined)


controller_initial_is_first_broker :: Spec
controller_initial_is_first_broker =
  it "the cluster's initial controller is the first broker" $ do
    c <- newMockCluster 3
    controllerBroker c >>= (`shouldBe` Just (BrokerId 0))


controller_can_be_reassigned :: Spec
controller_can_be_reassigned =
  it "setControllerBroker re-points the controller" $ do
    c <- newMockCluster 3
    setControllerBroker c (Just (BrokerId 2))
    controllerBroker c >>= (`shouldBe` Just (BrokerId 2))
    setControllerBroker c Nothing
    controllerBroker c >>= (`shouldBe` Nothing)


----------------------------------------------------------------------
-- Reauth
----------------------------------------------------------------------

reauth_deadline_default_unset :: Spec
reauth_deadline_default_unset =
  it "fresh cluster has no reauth deadline" $ do
    c <- newMockCluster 1
    reauthDeadline c >>= (`shouldBe` Nothing)
    isReauthExpired c >>= (`shouldBe` False)


reauth_deadline_round_trips :: Spec
reauth_deadline_round_trips =
  it "setReauthDeadline / reauthDeadline round-trips" $ do
    c <- newMockCluster 1
    setReauthDeadline c (Just 1000)
    reauthDeadline c >>= (`shouldBe` Just 1000)
    setReauthDeadline c Nothing
    reauthDeadline c >>= (`shouldBe` Nothing)


reauth_expired_when_clock_advances_past_deadline :: Spec
reauth_expired_when_clock_advances_past_deadline =
  it "isReauthExpired flips True when tickClock crosses the deadline" $ do
    c <- newMockCluster 1
    setReauthDeadline c (Just 100)
    isReauthExpired c >>= (`shouldBe` False)
    tickClock c 50
    isReauthExpired c >>= (`shouldBe` False)
    tickClock c 60 -- now at 110 > 100
    isReauthExpired c >>= (`shouldBe` True)
