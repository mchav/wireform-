{-# LANGUAGE OverloadedStrings #-}

module Client.MetadataCacheControlSpec (tests) where

import Kafka.Client.MetadataCacheControl qualified as MCC
import Test.Syd


tests :: Spec
tests =
  describe "MetadataCacheControl (KIP-294 / 526)" $
    sequence_
      [ it
          "fresh age -> every topic needs refresh"
          fresh
      , it
          "after recordRefresh, topic stays fresh until threshold"
          after_refresh
      , it
          "topicsNeedingRefresh filters correctly"
          bulk
      , it
          "isStale matches the age check"
          stale_check
      ]


fresh :: IO ()
fresh = MCC.shouldRefreshTopic 1000 60_000 "t" MCC.emptyTopicMetadataAge `shouldBe` True


after_refresh :: IO ()
after_refresh = do
  let !age = MCC.recordRefresh 1000 "t" MCC.emptyTopicMetadataAge
  MCC.shouldRefreshTopic 30_000 60_000 "t" age `shouldBe` False
  -- After 60_001 ms it should be stale again.
  MCC.shouldRefreshTopic 61_001 60_000 "t" age `shouldBe` True


bulk :: IO ()
bulk = do
  let age = MCC.recordRefresh 1000 "t1" MCC.emptyTopicMetadataAge
  MCC.topicsNeedingRefresh 5000 60_000 ["t1", "t2"] age `shouldBe` ["t2"]


stale_check :: IO ()
stale_check = do
  MCC.isStale 5000 4000 500 `shouldBe` True
  MCC.isStale 5000 4500 1000 `shouldBe` False
