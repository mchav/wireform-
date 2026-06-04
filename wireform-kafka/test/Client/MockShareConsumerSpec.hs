{-# LANGUAGE OverloadedStrings #-}

module Client.MockShareConsumerSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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
  , defaultShareConsumerConfig
  )

tests :: TestTree
tests = testGroup "MockShareConsumer"
  [ testCase "AckAccept removes a delivered record from future polls" accept_removes
  , testCase "AckRelease redelivers a record with an incremented delivery count" release_redelivers
  , testCase "AckReject completes a record without redelivery" reject_completes
  , testCase "expired locks redeliver until max delivery count" lock_expiry_respects_max_delivery
  ]

accept_removes :: IO ()
accept_removes = do
  (_cluster, consumer) <- seededConsumer "accept"
  rec <- expectOne =<< pollShareMC consumer 10
  acknowledgeShareMC consumer (ackFor AckAccept rec)
  committed <- commitAcknowledgementsMC consumer
  committed @?= [ackFor AckAccept rec]
  again <- pollShareMC consumer 10
  again @?= []

release_redelivers :: IO ()
release_redelivers = do
  (_cluster, consumer) <- seededConsumer "release"
  rec1 <- expectOne =<< pollShareMC consumer 10
  srDeliveryCount rec1 @?= 1
  acknowledgeShareMC consumer (ackFor AckRelease rec1)
  _ <- commitAcknowledgementsMC consumer
  rec2 <- expectOne =<< pollShareMC consumer 10
  srBaseOffset rec2 @?= srBaseOffset rec1
  srDeliveryCount rec2 @?= 2

reject_completes :: IO ()
reject_completes = do
  (_cluster, consumer) <- seededConsumer "reject"
  rec <- expectOne =<< pollShareMC consumer 10
  acknowledgeShareMC consumer (ackFor AckReject rec)
  _ <- commitAcknowledgementsMC consumer
  again <- pollShareMC consumer 10
  again @?= []

lock_expiry_respects_max_delivery :: IO ()
lock_expiry_respects_max_delivery = do
  (cluster, consumer) <- seededConsumer "expiry"
  first <- expectOne =<< pollShareMC consumer 10
  srDeliveryCount first @?= 1
  tickClock cluster 11
  second <- expectOne =<< pollShareMC consumer 10
  srDeliveryCount second @?= 2
  tickClock cluster 11
  exhausted <- pollShareMC consumer 10
  exhausted @?= []

seededConsumer :: Text -> IO (MockCluster, MockShareConsumer)
seededConsumer suffix = do
  cluster <- newMockCluster 1
  createTopic cluster "share-topic" 1
  faults <- noFaults
  producer <- newMockProducer cluster faults Nothing
  sent <- sendMock producer "share-topic" 0 Nothing (bytes ("value-" <> suffix)) (ts 0)
  sent @?= MPSent 0 0
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
  _ -> fail ("expected exactly one record, got " <> show (length xs))
