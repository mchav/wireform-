{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Conformance.T0009.MockCluster
Description : librdkafka @tests\/0009-mock_cluster.c@

librdkafka's test validates that an in-process mock cluster can host
topics and serve produce/fetch traffic without an external broker. Our
non-auth analogue uses 'Kafka.Client.Mock.Cluster' plus the mock
producer/consumer views.
-}
module Conformance.T0009.MockCluster (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Consumer
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Producer
import Test.Syd


tests :: Spec
tests =
  describe "0009-mock_cluster" $
    sequence_
      [ it "mock cluster produces and consumes one record" $ do
          cluster <- newMockCluster 1
          createTopic cluster "events" 1
          faults <- noFaults
          producer <- newMockProducer cluster faults Nothing
          sent <- sendMock producer "events" 0 Nothing (bytes "value") (ts 0)
          sent `shouldBe` MPSent 0 0
          consumer <- newMockConsumer cluster faults (GroupId "group") ReadUncommitted 10
          subscribeMC consumer ["events"]
          polled <- pollMC consumer
          let values = map (\(_, _, rec) -> srValue rec) (prRecords polled)
          values `shouldBe` [bytes "value"]
      ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


ts :: Integer -> Int64
ts = fromIntegral
