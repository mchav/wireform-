{-# LANGUAGE OverloadedStrings #-}

module Client.ShareConsumerHelpersSpec (tests) where

import Kafka.Client.ShareConsumer qualified as SC
import Kafka.Client.ShareConsumer qualified as SGE
import Test.Syd


tests :: Spec
tests =
  describe "ShareConsumer helpers" $
    sequence_
      [ it
          "pause + resume round-trip"
          pause_resume
      , it
          "decideDlq: under threshold -> Retry"
          dlq_retry
      , it
          "decideDlq: at threshold -> Deliver"
          dlq_deliver
      ]


pause_resume :: IO ()
pause_resume = do
  ps <- SGE.newPauseSet
  SGE.pausePartitions ps [("t", 0), ("t", 1)]
  paused <- SGE.isPaused ps "t" 0
  paused `shouldBe` True
  SGE.resumePartitions ps [("t", 0)]
  notP <- SGE.isPaused ps "t" 0
  notP `shouldBe` False
  stillP <- SGE.isPaused ps "t" 1
  stillP `shouldBe` True


mkRec :: Int -> SC.ShareRecord
mkRec n =
  SC.ShareRecord
    { SC.srTopic = "t"
    , SC.srPartition = 0
    , SC.srBaseOffset = 0
    , SC.srLastOffset = 0
    , SC.srKey = Nothing
    , SC.srValue = "v"
    , SC.srHeaders = []
    , SC.srTimestamp = 0
    , SC.srDeliveryCount = fromIntegral n
    }


dlq_retry :: IO ()
dlq_retry =
  SGE.decideDlq 5 (mkRec 2) (SGE.DlqRouteTo "dlq")
    `shouldBe` SGE.DlqDecisionRetry


dlq_deliver :: IO ()
dlq_deliver =
  SGE.decideDlq 5 (mkRec 5) (SGE.DlqRouteTo "dlq")
    `shouldBe` SGE.DlqDecisionDeliver (SGE.DlqRouteTo "dlq")
