{-# LANGUAGE OverloadedStrings #-}

-- | Tests for KIP-389 / KIP-345 client-side reaction to a
-- coordinator dropping us from the group.
--
-- The actual heartbeat loop (network + threadDelay) is exercised by
-- the live integration suite; here we only test the pure
-- 'applyHeartbeatOutcome' helper that the loop dispatches into when
-- a heartbeat response surfaces an error code. The split is
-- intentional: it lets us assert the *exact* state transition for
-- each error class without spinning up a fake broker.
module Client.HeartbeatRejoinSpec (tests) where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Data.IORef (readIORef, writeIORef)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Client.Internal.Heartbeat as HB

tests :: TestTree
tests = testGroup "Heartbeat rejoin reactions (KIP-389 / KIP-345)"
  [ testCase "UNKNOWN_MEMBER_ID clears memberId and triggers rejoin"
      unit_unknownMemberClearsMemberId
  , testCase "FENCED_INSTANCE_ID clears memberId and triggers rejoin"
      unit_fencedInstanceClearsMemberId
  , testCase "ILLEGAL_GENERATION preserves memberId but triggers rejoin"
      unit_illegalGenerationKeepsMemberId
  , testCase "Other error triggers rejoin without clearing memberId"
      unit_otherErrorKeepsMemberId
  , testCase "Transport failure leaves state untouched"
      unit_transportFailureNoOp
  ]

------------------------------------------------------------------
-- helpers
------------------------------------------------------------------

freshHb :: IO HB.HeartbeatState
freshHb = do
  connMgr      <- Conn.createConnectionManager
  versionCache <- AV.createVersionCache
  HB.createHeartbeatState
    "test-group" 3000 connMgr versionCache "test-client"

seedMember :: HB.HeartbeatState -> T.Text -> IO ()
seedMember st m = writeIORef (HB.hbMemberId st) m

resetRebalance :: HB.HeartbeatState -> IO ()
resetRebalance st = atomically $ writeTVar (HB.hbNeedsRebalance st) False

------------------------------------------------------------------
-- KIP-389 / KIP-345 reactions
------------------------------------------------------------------

unit_unknownMemberClearsMemberId :: Assertion
unit_unknownMemberClearsMemberId = do
  st <- freshHb
  seedMember st "consumer-1-uuid"
  HB.applyHeartbeatOutcome st HB.HeartbeatUnknownMember
  mid  <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  mid  @?= ""
  flag @?= True

unit_fencedInstanceClearsMemberId :: Assertion
unit_fencedInstanceClearsMemberId = do
  st <- freshHb
  seedMember st "static-member-7"
  HB.applyHeartbeatOutcome st HB.HeartbeatFencedInstance
  mid  <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  mid  @?= ""
  flag @?= True

unit_illegalGenerationKeepsMemberId :: Assertion
unit_illegalGenerationKeepsMemberId = do
  st <- freshHb
  seedMember st "consumer-9"
  HB.applyHeartbeatOutcome st HB.HeartbeatIllegalGeneration
  mid  <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  -- ILLEGAL_GENERATION only invalidates the generation counter.
  -- The memberId stays so the rejoin can be a no-op.
  assertBool ("memberId should be preserved, was " <> show mid)
    (not (T.null mid))
  flag @?= True

unit_otherErrorKeepsMemberId :: Assertion
unit_otherErrorKeepsMemberId = do
  st <- freshHb
  seedMember st "consumer-x"
  HB.applyHeartbeatOutcome st (HB.HeartbeatOtherError 42 "x")
  mid  <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  assertBool "memberId preserved" (not (T.null mid))
  flag @?= True

unit_transportFailureNoOp :: Assertion
unit_transportFailureNoOp = do
  st <- freshHb
  seedMember st "consumer-z"
  resetRebalance st
  HB.applyHeartbeatOutcome st (HB.HeartbeatTransport "net")
  mid  <- readIORef (HB.hbMemberId st)
  flag <- readTVarIO (HB.hbNeedsRebalance st)
  assertBool "memberId preserved" (not (T.null mid))
  flag @?= False
