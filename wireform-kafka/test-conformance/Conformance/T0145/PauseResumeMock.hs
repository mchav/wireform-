{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conformance.T0145.PauseResumeMock
Description : librdkafka @tests\/0145-pause_resume_mock.c@

Port of the non-auth pause/resume mock-cluster behavior: paused
partitions are skipped by poll, then resumed partitions fetch from
their prior cursor.
-}
module Conformance.T0145.PauseResumeMock (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd

import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Consumer
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Producer

tests :: Spec
tests = describe "0145-pause_resume_mock" $ sequence_
  [ it "paused partition is skipped until resumed" $ do
      cluster <- newMockCluster 1
      createTopic cluster "paused" 1
      faults <- noFaults
      producer <- newMockProducer cluster faults Nothing
      sent <- sendMock producer "paused" 0 Nothing (bytes "v") (ts 0)
      sent `shouldBe` MPSent 0 0
      consumer <- newMockConsumer cluster faults (GroupId "group") ReadUncommitted 10
      subscribeMC consumer ["paused"]
      pausePartitions consumer [("paused", 0)]
      pausedPoll <- pollMC consumer
      prRecords pausedPoll `shouldBe` []
      resumePartitions consumer [("paused", 0)]
      resumedPoll <- pollMC consumer
      map (\(_, _, rec) -> srValue rec) (prRecords resumedPoll) `shouldBe` [bytes "v"]
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

ts :: Integer -> Int64
ts = fromIntegral
