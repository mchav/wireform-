{-# LANGUAGE OverloadedStrings #-}

module Client.ShareGroupExtrasSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.ShareConsumer as SC
import qualified Kafka.Client.ShareGroupExtras as SGE

tests :: TestTree
tests = testGroup "ShareGroupExtras (KIP-1119 / 1129)"
  [ testCase "pause + resume round-trip"
      pause_resume
  , testCase "decideDlq: under threshold -> Retry"
      dlq_retry
  , testCase "decideDlq: at threshold -> Deliver"
      dlq_deliver
  ]

pause_resume :: IO ()
pause_resume = do
  ps <- SGE.newPauseSet
  SGE.pausePartitionsSG ps [("t", 0), ("t", 1)]
  paused <- SGE.isPausedSG ps "t" 0
  paused @?= True
  SGE.resumePartitionsSG ps [("t", 0)]
  notP <- SGE.isPausedSG ps "t" 0
  notP @?= False
  stillP <- SGE.isPausedSG ps "t" 1
  stillP @?= True

mkRec :: Int -> SC.ShareRecord
mkRec n = SC.ShareRecord
  { SC.srTopic         = "t"
  , SC.srPartition     = 0
  , SC.srBaseOffset    = 0
  , SC.srLastOffset    = 0
  , SC.srKey           = Nothing
  , SC.srValue         = "v"
  , SC.srHeaders       = []
  , SC.srTimestamp     = 0
  , SC.srDeliveryCount = fromIntegral n
  }

dlq_retry :: IO ()
dlq_retry = SGE.decideDlq 5 (mkRec 2) (SGE.DlqRouteTo "dlq")
  @?= SGE.DlqDecisionRetry

dlq_deliver :: IO ()
dlq_deliver = SGE.decideDlq 5 (mkRec 5) (SGE.DlqRouteTo "dlq")
  @?= SGE.DlqDecisionDeliver (SGE.DlqRouteTo "dlq")
