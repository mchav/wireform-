{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streams.Properties.RebalanceBridgeSpec
-- Description : KIP-848 bridge tests
--
-- Validates that 'AssignmentDelta' values produced by
-- 'Kafka.Client.ConsumerGroupV2.planHeartbeat' translate into
-- the right 'RebalanceProtocol.Reconciliation' for the streams
-- runtime.
module Streams.Properties.RebalanceBridgeSpec (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Syd

import qualified Kafka.Client.ConsumerGroupV2 as CGV2
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Assignor (MemberId (..))
import qualified Kafka.Streams.Runtime.RebalanceProtocol as RP
import Kafka.Streams.Runtime.RebalanceBridge

tests :: Spec
tests = describe "Rebalance bridge (KIP-848 \xe2\x86\x92 streams reconciler)" $ sequence_
  [ unit_assigned_becomes_add
  , unit_revoked_becomes_remove
  , unit_lost_becomes_remove
  , unit_applyDelta_updates_gsOwned
  , unit_heartbeatPlan_drives_reconciliation
  ]

----------------------------------------------------------------------
-- Conversion shape
----------------------------------------------------------------------

mkDelta :: [(T.Text, Int)] -> [(T.Text, Int)] -> [(T.Text, Int)] -> CGV2.AssignmentDelta
mkDelta assigned revoked lost = CGV2.AssignmentDelta
  { CGV2.adAssigned = Set.fromList [(t, fromIntegral p) | (t, p) <- assigned]
  , CGV2.adRevoked  = Set.fromList [(t, fromIntegral p) | (t, p) <- revoked]
  , CGV2.adLost     = Set.fromList [(t, fromIntegral p) | (t, p) <- lost]
  }

unit_assigned_becomes_add :: Spec
unit_assigned_becomes_add =
  it "adAssigned partitions become rAdd TaskIds" $ do
    let delta = mkDelta [("orders", 0), ("orders", 1)] [] []
        r = deltaToReconciliation tpToTask 0 delta
    RP.rAdd r `shouldBe` Set.fromList [TaskId 0 0, TaskId 0 1]
    RP.rRemove r `shouldBe` Set.empty

unit_revoked_becomes_remove :: Spec
unit_revoked_becomes_remove =
  it "adRevoked partitions become rRemove TaskIds" $ do
    let delta = mkDelta [] [("orders", 2)] []
        r = deltaToReconciliation tpToTask 0 delta
    RP.rRemove r `shouldBe` Set.singleton (TaskId 0 2)

unit_lost_becomes_remove :: Spec
unit_lost_becomes_remove =
  it "adLost partitions also flow into rRemove" $ do
    let delta = mkDelta [] [] [("orders", 3)]
        r = deltaToReconciliation tpToTask 0 delta
    RP.rRemove r `shouldBe` Set.singleton (TaskId 0 3)

----------------------------------------------------------------------
-- Round-trip through GroupState
----------------------------------------------------------------------

unit_applyDelta_updates_gsOwned :: Spec
unit_applyDelta_updates_gsOwned =
  it "applyAssignmentDelta updates gsOwned for the local member" $ do
    let mid   = MemberId "m1"
        gs0   = RP.initialGroupState
        delta1 = mkDelta [("orders", 0), ("orders", 1)] [] []
        delta2 = mkDelta [("orders", 2)] [("orders", 0)] []
    let gs1 = applyAssignmentDelta tpToTask 0 mid delta1 gs0
    Map.findWithDefault Set.empty mid (RP.gsOwned gs1)
      `shouldBe` Set.fromList [TaskId 0 0, TaskId 0 1]
    let gs2 = applyAssignmentDelta tpToTask 0 mid delta2 gs1
    Map.findWithDefault Set.empty mid (RP.gsOwned gs2)
      `shouldBe` Set.fromList [TaskId 0 1, TaskId 0 2]

----------------------------------------------------------------------
-- End-to-end: planHeartbeat output drives the reconciliation
----------------------------------------------------------------------

unit_heartbeatPlan_drives_reconciliation :: Spec
unit_heartbeatPlan_drives_reconciliation =
  it "planHeartbeat -> AssignmentDelta -> Reconciliation matches manual diff" $ do
    let prev = Set.fromList [("orders", 0), ("orders", 1 :: Int)]
        new  = Set.fromList [("orders", 1), ("orders", 2)]
        prev32 = Set.fromList [(t, fromIntegral p) | (t, p) <- Set.toList prev]
        new32  = Set.fromList [(t, fromIntegral p) | (t, p) <- Set.toList new]
        plan = CGV2.planHeartbeat 0 1000 CGV2.MSStable prev32 new32 False
        delta = CGV2.hpDelta plan
        r = deltaToReconciliation tpToTask 0 delta
    -- The new assignment moved partition 0 -> 2 (keeping 1).
    RP.rAdd r    `shouldBe` Set.singleton (TaskId 0 2)
    RP.rRemove r `shouldBe` Set.singleton (TaskId 0 0)
