{-# LANGUAGE OverloadedStrings #-}

module Client.TelemetryPushRuntimeSpec (tests) where

import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Set qualified as Set
import Kafka.Telemetry.Push qualified as Push
import Kafka.Telemetry.PushRuntime
import Test.Syd


tests :: Spec
tests =
  describe "Telemetry PushRuntime" $
    sequence_
      [ it "refresh stores subscription and broker client id" refresh_stores_subscription
      , it "push encodes and sends metrics after interval" push_sends_payload
      , it "empty payload advances push time without sending" empty_payload_skips_send
      , it "terminating sends final payload" terminating_sends_final_payload
      ]


mkSub :: Push.TelemetrySubscription
mkSub =
  Push.TelemetrySubscription
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
  r `shouldBe` Right Push.TARefreshSubscription
  readBrokerClientInstanceId st >>= (`shouldBe` Just "broker-client-1")
  machine <- readTelemetryState st
  Push.tsmSubscription machine `shouldBe` Just mkSub


push_sends_payload :: IO ()
push_sends_payload = do
  calls <- newTVarIO []
  st <- primedState 0
  r <- runTelemetryStep (runner calls "payload") st 150
  r `shouldBe` Right (Push.TAPushNow "payload")
  readTVarIO calls >>= (`shouldBe` ["payload"])
  machine <- readTelemetryState st
  Push.tsmLastPushAtMs machine `shouldBe` 150


empty_payload_skips_send :: IO ()
empty_payload_skips_send = do
  calls <- newTVarIO []
  st <- primedState 0
  r <- runTelemetryStep (runner calls mempty) st 150
  r `shouldBe` Right (Push.TAPushNow mempty)
  readTVarIO calls >>= (`shouldBe` [])
  machine <- readTelemetryState st
  Push.tsmLastPushAtMs machine `shouldBe` 150


terminating_sends_final_payload :: IO ()
terminating_sends_final_payload = do
  calls <- newTVarIO []
  st <- primedState 0
  requestTelemetryStop st
  r <- runTelemetryStep (runner calls "final") st 10
  r `shouldBe` Right Push.TADone
  readTVarIO calls >>= (`shouldBe` ["final"])


primedState :: Int64 -> IO TelemetryRuntimeState
primedState now = do
  st <- newTelemetryRuntimeState
  r <- runTelemetryStep (runner (error "unused") "payload") st now
  r `shouldBe` Right Push.TARefreshSubscription
  pure st


runner :: TVar [BS.ByteString] -> BS.ByteString -> TelemetryRunner
runner calls payload =
  TelemetryRunner
    { trRefreshSubscription = pure (Right mkSub)
    , trEncodeMetrics = \_ -> pure payload
    , trPushMetrics = \_ bs _terminating -> do
        atomically $ modifyTVar' calls (bs :)
        pure (Right ())
    }
