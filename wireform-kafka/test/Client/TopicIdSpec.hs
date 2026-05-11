{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-516 topic-id resolution table.
module Client.TopicIdSpec (tests) where

import Control.Concurrent.STM (atomically)
import qualified Data.ByteString as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.TopicId as T

tests :: TestTree
tests = testGroup "TopicId resolution table (KIP-516)"
  [ testCase "nullTopicId is all-zeros"
      null_id
  , testCase "register + look up by name"
      lookup_by_name
  , testCase "register + look up by id"
      lookup_by_id
  , testCase "missing keys -> Nothing"
      missing
  ]

mkId :: Int -> T.TopicId
mkId n = T.TopicId (BS.replicate 16 (fromIntegral n))

null_id :: IO ()
null_id = do
  T.isNullTopicId T.nullTopicId @?= True
  T.isNullTopicId (mkId 1)      @?= False

lookup_by_name :: IO ()
lookup_by_name = do
  tab <- T.newTopicIdTable
  let tid = mkId 7
  atomically $ T.registerTopicId tab "events" tid
  r <- atomically $ T.topicIdFor tab "events"
  r @?= Just tid

lookup_by_id :: IO ()
lookup_by_id = do
  tab <- T.newTopicIdTable
  let tid = mkId 7
  atomically $ T.registerTopicId tab "events" tid
  r <- atomically $ T.topicNameFor tab tid
  r @?= Just "events"

missing :: IO ()
missing = do
  tab <- T.newTopicIdTable
  r1 <- atomically $ T.topicIdFor tab "absent"
  r1 @?= Nothing
  r2 <- atomically $ T.topicNameFor tab (mkId 99)
  r2 @?= Nothing
