{-# LANGUAGE OverloadedStrings #-}

{- | Tests for KIP-389 / KIP-345 client-side reaction to a
coordinator dropping us from the group.

The actual heartbeat loop (network + threadDelay) is exercised by
the live integration suite; here we only test the pure
'applyHeartbeatOutcome' helper that the loop dispatches into when
a heartbeat response surfaces an error code. The split is
intentional: it lets us assert the *exact* state transition for
each error class without spinning up a fake broker.
-}
module Client.HeartbeatRejoinSpec (tests) where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Data.IORef (readIORef, writeIORef)
import Data.Text qualified as T
import Kafka.Client.Internal.Heartbeat qualified as HB
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import Test.Syd


tests :: Spec
tests =
  describe "Heartbeat rejoin reactions (KIP-389 / KIP-345)" $
    sequence_
      [ it
          "UNKNOWN_MEMBER_ID clears memberId and triggers rejoin"
          unit_unknownMemberClearsMemberId
      , it
          "FENCED_INSTANCE_ID clears memberId and triggers rejoin"
          unit_fencedInstanceClearsMemberId
      , it
          "ILLEGAL_GENERATION preserves memberId but triggers rejoin"
          unit_illegalGenerationKeepsMemberId
      , it
          "Other error triggers rejoin without clearing memberId"
          unit_otherErrorKeepsMemberId
      , it
          "Transport failure leaves state untouched"
          unit_transportFailureNoOp
      ]


------------------------------------------------------------------
-- helpers
------------------------------------------------------------------

freshHb :: IO HB.HeartbeatState
freshHb = do
  connMgr <- Conn.createConnectionManager
  versionCache <- AV.createVersionCache
  HB.createHeartbeatState
    "test-group"
    3000
    connMgr
    versionCache
    "test-client"


seedMember :: HB.HeartbeatState -> T.Text -> IO ()
seedMember st m = writeIORef (HB.hbMemberId st) m


resetRebalance :: HB.HeartbeatState -> IO ()
resetRebalance st = atomically $ writeTVar (HB.hbNeedsRebalance st) False


------------------------------------------------------------------
-- KIP-389 / KIP-345 reactions
------------------------------------------------------------------

unit_unknownMemberClearsMemberId :: IO ()
unit_unknownMemberClearsMemberId = do
  st <- freshHb
  seedMember st "consumer-1-uuid"
  HB.applyHeartbeatOutcome st HB.HeartbeatUnknownMember
  mid <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  mid `shouldBe` ""
  flag `shouldBe` True


unit_fencedInstanceClearsMemberId :: IO ()
unit_fencedInstanceClearsMemberId = do
  st <- freshHb
  seedMember st "static-member-7"
  HB.applyHeartbeatOutcome st HB.HeartbeatFencedInstance
  mid <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  mid `shouldBe` ""
  flag `shouldBe` True


unit_illegalGenerationKeepsMemberId :: IO ()
unit_illegalGenerationKeepsMemberId = do
  st <- freshHb
  seedMember st "consumer-9"
  HB.applyHeartbeatOutcome st HB.HeartbeatIllegalGeneration
  mid <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  -- ILLEGAL_GENERATION only invalidates the generation counter.
  -- The memberId stays so the rejoin can be a no-op.
  (if (not (T.null mid)) then pure () else expectationFailure ("memberId should be preserved, was " <> show mid))
  flag `shouldBe` True


unit_otherErrorKeepsMemberId :: IO ()
unit_otherErrorKeepsMemberId = do
  st <- freshHb
  seedMember st "consumer-x"
  HB.applyHeartbeatOutcome st (HB.HeartbeatOtherError 42 "x")
  mid <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  (not (T.null mid)) `shouldBe` True
  flag `shouldBe` True


unit_transportFailureNoOp :: IO ()
unit_transportFailureNoOp = do
  st <- freshHb
  seedMember st "consumer-z"
  resetRebalance st
  HB.applyHeartbeatOutcome st (HB.HeartbeatTransport "net")
  mid <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  (not (T.null mid)) `shouldBe` True
  flag `shouldBe` False
