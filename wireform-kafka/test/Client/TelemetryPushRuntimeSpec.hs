{-# LANGUAGE OverloadedStrings #-}

module Client.TelemetryPushRuntimeSpec (tests) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Telemetry.Push as Push
import Kafka.Telemetry.PushRuntime

tests :: TestTree
tests = testGroup "Telemetry PushRuntime"
  [ testCase "refresh stores subscription and broker client id" refresh_stores_subscription
  , testCase "push encodes and sends metrics after interval" push_sends_payload
  , testCase "empty payload advances push time without sending" empty_payload_skips_send
  , testCase "terminating sends final payload" terminating_sends_final_payload
  ]

mkSub :: Push.TelemetrySubscription
mkSub = Push.TelemetrySubscription
  { Push.tsClientInstanceId = "broker-client-1"
  , Push.tsSubscriptionId = 7
  , Push.tsRequestedMetrics = Set.singleton "producer."
  , Push.tsAcceptedFormats = [Push.OTLPProtobuf]
  , Push.tsPushIntervalMs = 100
  , Push.tsTelemetryMaxBytes = 1024
  , Push.tsDeltaTemporality = True
  }

refresh_stores_subscription :: IO ()
refresh_stores_subscription = do
  calls <- newTVarIO ([] :: [BS.ByteString])
  st <- newTelemetryRuntimeState
  r <- runTelemetryStep (runner calls "payload") st 10
  r @?= Right Push.TARefreshSubscription
  readBrokerClientInstanceId st >>= (@?= Just "broker-client-1")
  machine <- readTelemetryState st
  Push.tsmSubscription machine @?= Just mkSub

push_sends_payload :: IO ()
push_sends_payload = do
  calls <- newTVarIO []
  st <- primedState 0
  r <- runTelemetryStep (runner calls "payload") st 150
  r @?= Right (Push.TAPushNow "payload")
  readTVarIO calls >>= (@?= ["payload"])
  machine <- readTelemetryState st
  Push.tsmLastPushAtMs machine @?= 150

empty_payload_skips_send :: IO ()
empty_payload_skips_send = do
  calls <- newTVarIO []
  st <- primedState 0
  r <- runTelemetryStep (runner calls mempty) st 150
  r @?= Right (Push.TAPushNow mempty)
  readTVarIO calls >>= (@?= [])
  machine <- readTelemetryState st
  Push.tsmLastPushAtMs machine @?= 150

terminating_sends_final_payload :: IO ()
terminating_sends_final_payload = do
  calls <- newTVarIO []
  st <- primedState 0
  requestTelemetryStop st
  r <- runTelemetryStep (runner calls "final") st 10
  r @?= Right Push.TADone
  readTVarIO calls >>= (@?= ["final"])

primedState :: Int64 -> IO TelemetryRuntimeState
primedState now = do
  st <- newTelemetryRuntimeState
  r <- runTelemetryStep (runner (error "unused") "payload") st now
  r @?= Right Push.TARefreshSubscription
  pure st

runner :: TVar [BS.ByteString] -> BS.ByteString -> TelemetryRunner
runner calls payload = TelemetryRunner
  { trRefreshSubscription = pure (Right mkSub)
  , trEncodeMetrics = \_ -> pure payload
  , trPushMetrics = \_ bs _terminating -> do
      atomically $ modifyTVar' calls (bs :)
      pure (Right ())
  }
