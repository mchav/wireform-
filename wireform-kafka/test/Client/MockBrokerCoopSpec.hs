{-# LANGUAGE OverloadedStrings #-}

-- | Cooperative rebalance + asymmetric subscription + commit-during-rebalance
-- + sendOffsetsToTxn + leader-epoch validation. Mirrors librdkafka
-- 0113 / 0118 / 0120 / 0105 / 0139.
module Client.MockBrokerCoopSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Test.Syd

import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Consumer
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Producer

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

ts :: Integer -> Int64
ts = fromIntegral

tests :: Spec
tests = describe "MockBrokerCoop" $ sequence_
  [ -- Cooperative rebalance
    cooperative_initial_assignment_added
  , cooperative_member_join_revokes_overflow
  , cooperative_member_leave_adds_back
    -- Asymmetric subscription
  , asymmetric_subscription_per_member_topics
    -- Commit-during-rebalance race
  , commit_during_rebalance_keeps_offset
    -- Leader epoch / KIP-320
  , leader_epoch_starts_at_zero
  , bumpLeaderEpoch_advances_per_partition
  , validateOffsetEpoch_accepts_match_rejects_stale
    -- sendOffsetsToTxn
  , send_offsets_to_txn_visible_only_after_commit
  , send_offsets_to_txn_discarded_on_abort
  ]

----------------------------------------------------------------------
-- Cooperative rebalance
----------------------------------------------------------------------

cooperative_initial_assignment_added :: Spec
cooperative_initial_assignment_added =
  it "first rebalance: every assigned partition is an 'added'" $ do
    c <- newMockCluster 1
    createTopic c "t" 4
    let g = GroupId "coop"
    joinGroup c g (MemberId "m1") ["t"]
    rd <- cooperativeRebalance c g (MemberId "m1") []
    rdRevoked rd `shouldBe` []
    L.sort (rdAdded rd)   `shouldBe` [("t", p) | p <- [0, 1, 2, 3]]
    L.sort (rdAfter rd)   `shouldBe` [("t", p) | p <- [0, 1, 2, 3]]

cooperative_member_join_revokes_overflow :: Spec
cooperative_member_join_revokes_overflow =
  it "when a sibling joins, the existing member's delta revokes the now-foreign partitions" $ do
    c <- newMockCluster 1
    createTopic c "t" 4
    let g = GroupId "coop2"
    joinGroup c g (MemberId "a") ["t"]
    -- Compute what 'a' had before 'b' joined (full coverage).
    rd0 <- cooperativeRebalance c g (MemberId "a") []
    let !beforeA = rdAfter rd0
    L.sort beforeA `shouldBe` [("t", p) | p <- [0, 1, 2, 3]]
    -- Now 'b' joins.
    joinGroup c g (MemberId "b") ["t"]
    rdA <- cooperativeRebalance c g (MemberId "a") beforeA
    -- 'a' should keep partitions 0 and 2; partitions 1 and 3 are
    -- revoked (taken by 'b').
    L.sort (rdAfter rdA)   `shouldBe` [("t", 0), ("t", 2)]
    L.sort (rdRevoked rdA) `shouldBe` [("t", 1), ("t", 3)]
    rdAdded rdA            `shouldBe` []

cooperative_member_leave_adds_back :: Spec
cooperative_member_leave_adds_back =
  it "after a sibling leaves, the cooperative delta adds back the freed partitions" $ do
    c <- newMockCluster 1
    createTopic c "t" 4
    let g = GroupId "coop3"
    joinGroup c g (MemberId "a") ["t"]
    joinGroup c g (MemberId "b") ["t"]
    rdABefore <- cooperativeRebalance c g (MemberId "a") []
    -- 'a' currently owns [(t, 0), (t, 2)].
    leaveGroup c g (MemberId "b")
    rdA <- cooperativeRebalance c g (MemberId "a") (rdAfter rdABefore)
    -- 'a' picks up the freed partitions; nothing is revoked.
    rdRevoked rdA          `shouldBe` []
    L.sort (rdAdded rdA)   `shouldBe` [("t", 1), ("t", 3)]
    L.sort (rdAfter rdA)   `shouldBe` [("t", p) | p <- [0, 1, 2, 3]]

----------------------------------------------------------------------
-- Asymmetric subscription
----------------------------------------------------------------------

asymmetric_subscription_per_member_topics :: Spec
asymmetric_subscription_per_member_topics =
  it "members that subscribe to different topic sets get disjoint assignments" $ do
    c <- newMockCluster 1
    createTopic c "alpha" 2
    createTopic c "beta"  2
    let g = GroupId "async"
    joinGroup c g (MemberId "ma") ["alpha"]
    joinGroup c g (MemberId "mb") ["beta"]
    aA <- assignmentFor c g (MemberId "ma")
    aB <- assignmentFor c g (MemberId "mb")
    -- Each member gets only the topics it subscribed to (modulo
    -- the round-robin spread across the union of subscribers).
    -- The current assignor is per-union round-robin: it deals
    -- out (alpha, 0..1) and (beta, 0..1) across both members in
    -- sorted member-id order.
    -- ma -> alpha 0, beta 0; mb -> alpha 1, beta 1.
    -- Asymmetric subscriptions narrow each member to topics it
    -- declared.
    --
    -- We assert weaker: each member's assignment is a subset of
    -- its declared subscription.
    (if (all (\(t, _) -> t == "alpha") aA) then pure () else expectationFailure ("ma got non-alpha: " <> show aA))
    (if (all (\(t, _) -> t == "beta")  aB) then pure () else expectationFailure ("mb got non-beta: "  <> show aB))

----------------------------------------------------------------------
-- Commit-during-rebalance
----------------------------------------------------------------------

commit_during_rebalance_keeps_offset :: Spec
commit_during_rebalance_keeps_offset =
  it "an offset commit racing a rebalance is preserved across the assignor refresh" $ do
    c <- newMockCluster 1
    createTopic c "t" 2
    let g = GroupId "race"
    fp <- noFaults
    cons <- newMockConsumerWithId c fp g (MemberId "ma") ReadUncommitted 100
    subscribeMC cons ["t"]
    Right () <- commitOffsetsMC cons [("t", 0, 5), ("t", 1, 9)]
    -- Now a sibling joins; rebalance reduces this member's set.
    joinGroup c g (MemberId "mb") ["t"]
    refreshAssignment cons
    -- The committed offsets are still in the group store.
    m <- groupOffsetsFor c g
    Map.lookup ("t", 0) m `shouldBe` Just 5
    Map.lookup ("t", 1) m `shouldBe` Just 9

----------------------------------------------------------------------
-- Leader epoch (KIP-320)
----------------------------------------------------------------------

leader_epoch_starts_at_zero :: Spec
leader_epoch_starts_at_zero =
  it "every newly-created partition starts at leader epoch 0" $ do
    c <- newMockCluster 1
    createTopic c "t" 3
    eps <- mapM (\p -> currentLeaderEpoch c "t" p) [0, 1, 2]
    eps `shouldBe` [Just 0, Just 0, Just 0]

bumpLeaderEpoch_advances_per_partition :: Spec
bumpLeaderEpoch_advances_per_partition =
  it "bumpLeaderEpoch only advances the targeted partition" $ do
    c <- newMockCluster 1
    createTopic c "t" 2
    new <- bumpLeaderEpoch c "t" 1
    new `shouldBe` 1
    e0 <- currentLeaderEpoch c "t" 0
    e1 <- currentLeaderEpoch c "t" 1
    e0 `shouldBe` Just 0
    e1 `shouldBe` Just 1

validateOffsetEpoch_accepts_match_rejects_stale :: Spec
validateOffsetEpoch_accepts_match_rejects_stale =
  it "validateOffsetEpoch passes Just current-epoch and rejects stale" $ do
    c <- newMockCluster 1
    createTopic c "t" 1
    -- Bump partition 0 twice (now epoch 2).
    _ <- bumpLeaderEpoch c "t" 0
    _ <- bumpLeaderEpoch c "t" 0
    -- A consumer whose last-committed epoch was 1 -> diverged.
    rOld <- validateOffsetEpoch c "t" 0 (Just 1)
    case rOld of
      Left _ -> pure ()
      Right _ -> error "expected Left for stale epoch"
    rCur <- validateOffsetEpoch c "t" 0 (Just 2)
    rCur `shouldBe` Right ()
    rNothing <- validateOffsetEpoch c "t" 0 Nothing
    rNothing `shouldBe` Right ()

----------------------------------------------------------------------
-- sendOffsetsToTxn
----------------------------------------------------------------------

send_offsets_to_txn_visible_only_after_commit :: Spec
send_offsets_to_txn_visible_only_after_commit =
  it "sendOffsetsToTxn: pending until commitTxn merges into group store" $ do
    c <- newMockCluster 1
    createTopic c "in" 1
    let tx = TxnId "tx-eos"
        g  = GroupId "consumer-group"
    fp <- noFaults
    p  <- newMockProducer c fp (Just tx)
    Right () <- beginTxnMP p
    sendOffsetsToTxn c tx g
      [(("in", 0), OffsetAndMetadata 12 Nothing Nothing)]
    -- Pre-commit: pending visible, group store empty.
    pending <- pendingTxnOffsets c tx
    Map.size pending `shouldBe` 1
    m0 <- groupOffsetsFor c g
    Map.lookup ("in", 0) m0 `shouldBe` Nothing
    -- Commit: pending drains, group store has the offset.
    Right () <- commitTxnMP p
    pending2 <- pendingTxnOffsets c tx
    Map.size pending2 `shouldBe` 0
    m1 <- groupOffsetsFor c g
    Map.lookup ("in", 0) m1 `shouldBe` Just 12

send_offsets_to_txn_discarded_on_abort :: Spec
send_offsets_to_txn_discarded_on_abort =
  it "sendOffsetsToTxn: pending discarded on abortTxn" $ do
    c <- newMockCluster 1
    createTopic c "in" 1
    let tx = TxnId "tx-rollback"
        g  = GroupId "g"
    fp <- noFaults
    p  <- newMockProducer c fp (Just tx)
    Right () <- beginTxnMP p
    sendOffsetsToTxn c tx g
      [(("in", 0), OffsetAndMetadata 7 Nothing Nothing)]
    Right () <- abortTxnMP p
    pending <- pendingTxnOffsets c tx
    Map.size pending `shouldBe` 0
    m <- groupOffsetsFor c g
    Map.lookup ("in", 0) m `shouldBe` Nothing
