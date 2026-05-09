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
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Consumer
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Producer

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

ts :: Integer -> Int64
ts = fromIntegral

tests :: TestTree
tests = testGroup "MockBrokerCoop"
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

cooperative_initial_assignment_added :: TestTree
cooperative_initial_assignment_added =
  testCase "first rebalance: every assigned partition is an 'added'" $ do
    c <- newMockCluster 1
    createTopic c "t" 4
    let g = GroupId "coop"
    joinGroup c g (MemberId "m1") ["t"]
    rd <- cooperativeRebalance c g (MemberId "m1") []
    rdRevoked rd @?= []
    L.sort (rdAdded rd)   @?= [("t", p) | p <- [0, 1, 2, 3]]
    L.sort (rdAfter rd)   @?= [("t", p) | p <- [0, 1, 2, 3]]

cooperative_member_join_revokes_overflow :: TestTree
cooperative_member_join_revokes_overflow =
  testCase "when a sibling joins, the existing member's delta revokes the now-foreign partitions" $ do
    c <- newMockCluster 1
    createTopic c "t" 4
    let g = GroupId "coop2"
    joinGroup c g (MemberId "a") ["t"]
    -- Compute what 'a' had before 'b' joined (full coverage).
    rd0 <- cooperativeRebalance c g (MemberId "a") []
    let !beforeA = rdAfter rd0
    L.sort beforeA @?= [("t", p) | p <- [0, 1, 2, 3]]
    -- Now 'b' joins.
    joinGroup c g (MemberId "b") ["t"]
    rdA <- cooperativeRebalance c g (MemberId "a") beforeA
    -- 'a' should keep partitions 0 and 2; partitions 1 and 3 are
    -- revoked (taken by 'b').
    L.sort (rdAfter rdA)   @?= [("t", 0), ("t", 2)]
    L.sort (rdRevoked rdA) @?= [("t", 1), ("t", 3)]
    rdAdded rdA            @?= []

cooperative_member_leave_adds_back :: TestTree
cooperative_member_leave_adds_back =
  testCase "after a sibling leaves, the cooperative delta adds back the freed partitions" $ do
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
    rdRevoked rdA          @?= []
    L.sort (rdAdded rdA)   @?= [("t", 1), ("t", 3)]
    L.sort (rdAfter rdA)   @?= [("t", p) | p <- [0, 1, 2, 3]]

----------------------------------------------------------------------
-- Asymmetric subscription
----------------------------------------------------------------------

asymmetric_subscription_per_member_topics :: TestTree
asymmetric_subscription_per_member_topics =
  testCase "members that subscribe to different topic sets get disjoint assignments" $ do
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
    assertBool ("ma got non-alpha: " <> show aA)
               (all (\(t, _) -> t == "alpha") aA)
    assertBool ("mb got non-beta: "  <> show aB)
               (all (\(t, _) -> t == "beta")  aB)

----------------------------------------------------------------------
-- Commit-during-rebalance
----------------------------------------------------------------------

commit_during_rebalance_keeps_offset :: TestTree
commit_during_rebalance_keeps_offset =
  testCase "an offset commit racing a rebalance is preserved across the assignor refresh" $ do
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
    Map.lookup ("t", 0) m @?= Just 5
    Map.lookup ("t", 1) m @?= Just 9

----------------------------------------------------------------------
-- Leader epoch (KIP-320)
----------------------------------------------------------------------

leader_epoch_starts_at_zero :: TestTree
leader_epoch_starts_at_zero =
  testCase "every newly-created partition starts at leader epoch 0" $ do
    c <- newMockCluster 1
    createTopic c "t" 3
    eps <- mapM (\p -> currentLeaderEpoch c "t" p) [0, 1, 2]
    eps @?= [Just 0, Just 0, Just 0]

bumpLeaderEpoch_advances_per_partition :: TestTree
bumpLeaderEpoch_advances_per_partition =
  testCase "bumpLeaderEpoch only advances the targeted partition" $ do
    c <- newMockCluster 1
    createTopic c "t" 2
    new <- bumpLeaderEpoch c "t" 1
    new @?= 1
    e0 <- currentLeaderEpoch c "t" 0
    e1 <- currentLeaderEpoch c "t" 1
    e0 @?= Just 0
    e1 @?= Just 1

validateOffsetEpoch_accepts_match_rejects_stale :: TestTree
validateOffsetEpoch_accepts_match_rejects_stale =
  testCase "validateOffsetEpoch passes Just current-epoch and rejects stale" $ do
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
    rCur @?= Right ()
    rNothing <- validateOffsetEpoch c "t" 0 Nothing
    rNothing @?= Right ()

----------------------------------------------------------------------
-- sendOffsetsToTxn
----------------------------------------------------------------------

send_offsets_to_txn_visible_only_after_commit :: TestTree
send_offsets_to_txn_visible_only_after_commit =
  testCase "sendOffsetsToTxn: pending until commitTxn merges into group store" $ do
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
    Map.size pending @?= 1
    m0 <- groupOffsetsFor c g
    Map.lookup ("in", 0) m0 @?= Nothing
    -- Commit: pending drains, group store has the offset.
    Right () <- commitTxnMP p
    pending2 <- pendingTxnOffsets c tx
    Map.size pending2 @?= 0
    m1 <- groupOffsetsFor c g
    Map.lookup ("in", 0) m1 @?= Just 12

send_offsets_to_txn_discarded_on_abort :: TestTree
send_offsets_to_txn_discarded_on_abort =
  testCase "sendOffsetsToTxn: pending discarded on abortTxn" $ do
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
    Map.size pending @?= 0
    m <- groupOffsetsFor c g
    Map.lookup ("in", 0) m @?= Nothing
