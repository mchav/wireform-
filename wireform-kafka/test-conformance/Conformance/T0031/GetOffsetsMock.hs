{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conformance.T0031.GetOffsetsMock
Description : librdkafka @tests\/0031-get_offsets_mock.c@

The librdkafka test asks the mock broker for earliest/latest offsets.
The wireform mock cluster exposes high-water marks directly; this port
checks the non-auth offset bookkeeping behind ListOffsets-style tests.
-}
module Conformance.T0031.GetOffsetsMock (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Fault
import Kafka.Client.Mock.Producer

tests :: TestTree
tests = testGroup "0031-get_offsets_mock"
  [ testCase "partition high-water mark tracks produced records" $ do
      cluster <- newMockCluster 1
      createTopic cluster "offsets" 1
      faults <- noFaults
      producer <- newMockProducer cluster faults Nothing
      sendValues producer ["a", "b", "c"]
      partitionHWM cluster "offsets" 0 >>= (@?= Just 3)
      partitionLastStableOffset cluster "offsets" 0 >>= (@?= Just 3)
  ]

sendValues :: MockProducer -> [Text] -> IO ()
sendValues producer values =
  mapM_ sendOne (zip values [0 :: Int64 ..])
  where
    sendOne (value, timestamp) = do
      result <- sendMock producer "offsets" 0 Nothing (bytes value) timestamp
      case result of
        MPSent _ _ -> pure ()
        other -> assertFailure ("unexpected produce result: " <> show other)

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack
