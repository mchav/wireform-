{-# LANGUAGE OverloadedStrings #-}

module Client.MockShareConsumerSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd

import Kafka.Client.Mock.Cluster
  ( GroupId (..)
  , MockCluster
  , createTopic
  , newMockCluster
  , tickClock
  )
import Kafka.Client.Mock.Fault (noFaults)
import Kafka.Client.Mock.Producer
  ( MockProduceResult (..)
  , newMockProducer
  , sendMock
  )
import Kafka.Client.Mock.ShareConsumer
import Kafka.Client.ShareConsumer
  ( Acknowledgement (..)
  , AcknowledgementType (..)
  , ShareConsumerConfig (..)
  , ShareRecord (..)
  , acknowledgeShareRecord
  , commitAcknowledgements
  , createShareConsumerWithRunner
  , defaultShareConsumerConfig
  , pollShareRecords
  )

tests :: Spec
tests = describe "MockShareConsumer" $ sequence_
  [ it "AckAccept removes a delivered record from future polls" accept_removes
  , it "AckRelease redelivers a record with an incremented delivery count" release_redelivers
  , it "AckReject completes a record without redelivery" reject_completes
  , it "expired locks redeliver until max delivery count" lock_expiry_respects_max_delivery
  , it "public ShareConsumer runner delegates poll and commit" public_runner_delegates
  ]

accept_removes :: IO ()
accept_removes = do
  (_cluster, consumer) <- seededConsumer "accept"
  rec <- expectOne =<< pollShareMC consumer 10
  acknowledgeShareMC consumer (ackFor AckAccept rec)
  committed <- commitAcknowledgementsMC consumer
  committed `shouldBe` [ackFor AckAccept rec]
  again <- pollShareMC consumer 10
  again `shouldBe` []

release_redelivers :: IO ()
release_redelivers = do
  (_cluster, consumer) <- seededConsumer "release"
  rec1 <- expectOne =<< pollShareMC consumer 10
  srDeliveryCount rec1 `shouldBe` 1
  acknowledgeShareMC consumer (ackFor AckRelease rec1)
  _ <- commitAcknowledgementsMC consumer
  rec2 <- expectOne =<< pollShareMC consumer 10
  srBaseOffset rec2 `shouldBe` srBaseOffset rec1
  srDeliveryCount rec2 `shouldBe` 2

reject_completes :: IO ()
reject_completes = do
  (_cluster, consumer) <- seededConsumer "reject"
  rec <- expectOne =<< pollShareMC consumer 10
  acknowledgeShareMC consumer (ackFor AckReject rec)
  _ <- commitAcknowledgementsMC consumer
  again <- pollShareMC consumer 10
  again `shouldBe` []

lock_expiry_respects_max_delivery :: IO ()
lock_expiry_respects_max_delivery = do
  (cluster, consumer) <- seededConsumer "expiry"
  first <- expectOne =<< pollShareMC consumer 10
  srDeliveryCount first `shouldBe` 1
  tickClock cluster 11
  second <- expectOne =<< pollShareMC consumer 10
  srDeliveryCount second `shouldBe` 2
  tickClock cluster 11
  exhausted <- pollShareMC consumer 10
  exhausted `shouldBe` []

public_runner_delegates :: IO ()
public_runner_delegates = do
  (_cluster, mockConsumer) <- seededConsumer "runner"
  let cfg = defaultShareConsumerConfig "share-group-runner" ["share-topic"]
  public <- createShareConsumerWithRunner cfg (mockShareRunner mockConsumer)
  rec <- expectOne =<< pollShareRecords public 10
  acknowledgeShareRecord public (ackFor AckAccept rec)
  committed <- commitAcknowledgements public
  committed `shouldBe` [ackFor AckAccept rec]
  again <- pollShareRecords public 10
  again `shouldBe` []

seededConsumer :: Text -> IO (MockCluster, MockShareConsumer)
seededConsumer suffix = do
  cluster <- newMockCluster 1
  createTopic cluster "share-topic" 1
  faults <- noFaults
  producer <- newMockProducer cluster faults Nothing
  sent <- sendMock producer "share-topic" 0 Nothing (bytes ("value-" <> suffix)) (ts 0)
  sent `shouldBe` MPSent 0 0
  consumer <- newMockShareConsumer
    cluster
    (GroupId ("share-group-" <> suffix))
    (defaultShareConsumerConfig ("share-group-" <> suffix) ["share-topic"])
      { scLockTimeoutMs = 10
      , scMaxDeliveryCount = 2
      }
  pure (cluster, consumer)

ackFor :: AcknowledgementType -> ShareRecord -> Acknowledgement
ackFor ackType rec = Acknowledgement
  { ackTopic = srTopic rec
  , ackPartition = srPartition rec
  , ackBaseOffset = srBaseOffset rec
  , ackLastOffset = srLastOffset rec
  , ackType = ackType
  }

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

ts :: Integer -> Int64
ts = fromIntegral

expectOne :: [a] -> IO a
expectOne xs = case xs of
  [x] -> pure x
  _ -> expectationFailure ("expected exactly one record, got " <> show (length xs))
