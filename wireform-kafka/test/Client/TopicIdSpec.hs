{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-516 topic-id resolution table.
module Client.TopicIdSpec (tests) where

import Control.Concurrent.STM (atomically)
import Data.ByteString qualified as BS
import Kafka.Client.TopicId qualified as T
import Test.Syd


tests :: Spec
tests =
  describe "TopicId resolution table (KIP-516)" $
    sequence_
      [ it
          "nullTopicId is all-zeros"
          null_id
      , it
          "register + look up by name"
          lookup_by_name
      , it
          "register + look up by id"
          lookup_by_id
      , it
          "missing keys -> Nothing"
          missing
      ]


mkId :: Int -> T.TopicId
mkId n = T.TopicId (BS.replicate 16 (fromIntegral n))


null_id :: IO ()
null_id = do
  T.isNullTopicId T.nullTopicId `shouldBe` True
  T.isNullTopicId (mkId 1) `shouldBe` False


lookup_by_name :: IO ()
lookup_by_name = do
  tab <- T.newTopicIdTable
  let tid = mkId 7
  atomically $ T.registerTopicId tab "events" tid
  r <- atomically $ T.topicIdFor tab "events"
  r `shouldBe` Just tid


lookup_by_id :: IO ()
lookup_by_id = do
  tab <- T.newTopicIdTable
  let tid = mkId 7
  atomically $ T.registerTopicId tab "events" tid
  r <- atomically $ T.topicNameFor tab tid
  r `shouldBe` Just "events"


missing :: IO ()
missing = do
  tab <- T.newTopicIdTable
  r1 <- atomically $ T.topicIdFor tab "absent"
  r1 `shouldBe` Nothing
  r2 <- atomically $ T.topicNameFor tab (mkId 99)
  r2 `shouldBe` Nothing
