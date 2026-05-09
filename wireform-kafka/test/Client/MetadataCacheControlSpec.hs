{-# LANGUAGE OverloadedStrings #-}

module Client.MetadataCacheControlSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.MetadataCacheControl as MCC

tests :: TestTree
tests = testGroup "MetadataCacheControl (KIP-294 / 526)"
  [ testCase "fresh age -> every topic needs refresh"
      fresh
  , testCase "after recordRefresh, topic stays fresh until threshold"
      after_refresh
  , testCase "topicsNeedingRefresh filters correctly"
      bulk
  , testCase "isStale matches the age check"
      stale_check
  ]

fresh :: IO ()
fresh = MCC.shouldRefreshTopic 1000 60_000 "t" MCC.emptyTopicMetadataAge @?= True

after_refresh :: IO ()
after_refresh = do
  let !age = MCC.recordRefresh 1000 "t" MCC.emptyTopicMetadataAge
  MCC.shouldRefreshTopic 30_000 60_000 "t" age  @?= False
  -- After 60_001 ms it should be stale again.
  MCC.shouldRefreshTopic 61_001 60_000 "t" age  @?= True

bulk :: IO ()
bulk = do
  let age = MCC.recordRefresh 1000 "t1" MCC.emptyTopicMetadataAge
  MCC.topicsNeedingRefresh 5000 60_000 ["t1", "t2"] age @?= ["t2"]

stale_check :: IO ()
stale_check = do
  MCC.isStale 5000 4000 500  @?= True
  MCC.isStale 5000 4500 1000 @?= False
