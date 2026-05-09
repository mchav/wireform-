{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-932 share group surface.
module Client.ShareConsumerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Client.ShareConsumer as SC

tests :: TestTree
tests = testGroup "ShareConsumer (KIP-932)"
  [ testCase "defaults match the Java client (lock=30s, max-deliveries=5)"
      defaults
  , testCase "acknowledgements buffered + drained in order"
      ack_buffer
  , testCase "shouldRedeliver requires expiry AND non-poison"
      redeliver_logic
  , testCase "lockExpiresAt is locked-at + lock-timeout"
      lock_expires
  ]

defaults :: IO ()
defaults = do
  let cfg = SC.defaultShareConsumerConfig "g" ["t"]
  SC.scLockTimeoutMs cfg     @?= 30_000
  SC.scMaxDeliveryCount cfg  @?= 5
  SC.scMaxFetchRecords cfg   @?= 500
  SC.scTopics cfg            @?= ["t"]

ack_buffer :: IO ()
ack_buffer = do
  c <- SC.createShareConsumer (SC.defaultShareConsumerConfig "g" ["t"])
  let mkAck o = SC.Acknowledgement "t" 0 o o SC.AckAccept
  SC.acknowledgeShareRecord c (mkAck 0)
  SC.acknowledgeShareRecord c (mkAck 1)
  SC.acknowledgeShareRecord c (mkAck 2)
  acks <- SC.commitAcknowledgements c
  -- Drained in the order they were enqueued.
  map SC.ackBaseOffset acks @?= [0, 1, 2]
  -- Subsequent commit returns empty.
  again <- SC.commitAcknowledgements c
  again @?= []

redeliver_logic :: IO ()
redeliver_logic = do
  let lock = SC.RecordLockState
        { SC.rlsLockedAtMs    = 0
        , SC.rlsLockTimeoutMs = 1000
        , SC.rlsDeliveryCount = 1
        }
  -- Within the lock window: don't redeliver yet.
  assertBool "still locked" (not (SC.shouldRedeliver 500 5 lock))
  -- Past the lock window with delivery count < max: redeliver.
  assertBool "expired -> redeliver" (SC.shouldRedeliver 5000 5 lock)
  -- Past the lock window but already at max-deliveries: poison
  -- pill, do not redeliver.
  let poison = lock { SC.rlsDeliveryCount = 5 }
  assertBool "poisoned -> no redeliver" (not (SC.shouldRedeliver 5000 5 poison))

lock_expires :: IO ()
lock_expires = do
  let lock = SC.RecordLockState 1000 5000 0
  SC.lockExpiresAt lock @?= 6000
