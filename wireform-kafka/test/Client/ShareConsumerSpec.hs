{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-932 share group surface.
module Client.ShareConsumerSpec (tests) where

import Test.Syd

import qualified Kafka.Client.ShareConsumer as SC

tests :: Spec
tests = describe "ShareConsumer (KIP-932)" $ sequence_
  [ it "defaults match the Java client (lock=30s, max-deliveries=5)"
      defaults
  , it "acknowledgements buffered + drained in order"
      ack_buffer
  , it "shouldRedeliver requires expiry AND non-poison"
      redeliver_logic
  , it "lockExpiresAt is locked-at + lock-timeout"
      lock_expires
  ]

defaults :: IO ()
defaults = do
  let cfg = SC.defaultShareConsumerConfig "g" ["t"]
  SC.scLockTimeoutMs cfg     `shouldBe` 30_000
  SC.scMaxDeliveryCount cfg  `shouldBe` 5
  SC.scMaxFetchRecords cfg   `shouldBe` 500
  SC.scTopics cfg            `shouldBe` ["t"]

ack_buffer :: IO ()
ack_buffer = do
  c <- SC.createShareConsumer (SC.defaultShareConsumerConfig "g" ["t"])
  let mkAck o = SC.Acknowledgement "t" 0 o o SC.AckAccept
  SC.acknowledgeShareRecord c (mkAck 0)
  SC.acknowledgeShareRecord c (mkAck 1)
  SC.acknowledgeShareRecord c (mkAck 2)
  acks <- SC.commitAcknowledgements c
  -- Drained in the order they were enqueued.
  map SC.ackBaseOffset acks `shouldBe` [0, 1, 2]
  -- Subsequent commit returns empty.
  again <- SC.commitAcknowledgements c
  again `shouldBe` []

redeliver_logic :: IO ()
redeliver_logic = do
  let lock = SC.RecordLockState
        { SC.rlsLockedAtMs    = 0
        , SC.rlsLockTimeoutMs = 1000
        , SC.rlsDeliveryCount = 1
        }
  -- Within the lock window: don't redeliver yet.
  (not (SC.shouldRedeliver 500 5 lock)) `shouldBe` True
  -- Past the lock window with delivery count < max: redeliver.
  (SC.shouldRedeliver 5000 5 lock) `shouldBe` True
  -- Past the lock window but already at max-deliveries: poison
  -- pill, do not redeliver.
  let poison = lock { SC.rlsDeliveryCount = 5 }
  (not (SC.shouldRedeliver 5000 5 poison)) `shouldBe` True

lock_expires :: IO ()
lock_expires = do
  let lock = SC.RecordLockState 1000 5000 0
  SC.lockExpiresAt lock `shouldBe` 6000
