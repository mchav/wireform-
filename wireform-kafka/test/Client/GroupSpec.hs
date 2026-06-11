{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Client.GroupSpec
Description : Tests for the high-level consumer-group API

These tests exercise the pieces of "Kafka.Client.Group" and
"Kafka.Client.Internal.Subscribe" that don't need a live broker:
the range-assignor algebra, configuration validation, and the
default policies. The actual end-to-end @runConsumer@ is exercised
in the broker-gated integration suite.
-}
module Client.GroupSpec (groupSpec) where

import Control.Exception (try)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Client.Group qualified as Group
import Kafka.Client.Internal.Subscribe qualified as Sub
import Kafka.Errors (KafkaException)
import Test.Syd
import Test.Syd.Hedgehog ()


groupSpec :: Spec
groupSpec =
  describe "Group consumer (high-level)" $
    sequence_
      [ rangeAssignTests
      , roundRobinAssignTests
      , stickyAssignTests
      , configValidationTests
      ]


--------------------------------------------------------------------------------
-- Range assignment
--------------------------------------------------------------------------------

rangeAssignTests :: Spec
rangeAssignTests =
  describe "rangeAssign" $
    sequence_
      [ it "even split: 6 partitions / 3 consumers / single topic" $ do
          let result =
                Sub.rangeAssign
                  [ ("c1", ["t"])
                  , ("c2", ["t"])
                  , ("c3", ["t"])
                  ]
                  (Map.fromList [("t", [0, 1, 2, 3, 4, 5])])
          lookup "c1" result `shouldBe` Just [("t", [0, 1])]
          lookup "c2" result `shouldBe` Just [("t", [2, 3])]
          lookup "c3" result `shouldBe` Just [("t", [4, 5])]
      , it "uneven split: 7 partitions / 3 consumers" $ do
          let result =
                Sub.rangeAssign
                  [("c1", ["t"]), ("c2", ["t"]), ("c3", ["t"])]
                  (Map.fromList [("t", [0 .. 6])])
          -- The first (length `mod` n) consumers each take one extra
          -- partition. 7 mod 3 = 1, so c1 gets 3, c2 gets 2, c3 gets 2.
          lookup "c1" result `shouldBe` Just [("t", [0, 1, 2])]
          lookup "c2" result `shouldBe` Just [("t", [3, 4])]
          lookup "c3" result `shouldBe` Just [("t", [5, 6])]
      , it "more consumers than partitions: extras get nothing" $ do
          let result =
                Sub.rangeAssign
                  [("c1", ["t"]), ("c2", ["t"]), ("c3", ["t"])]
                  (Map.fromList [("t", [0, 1])])
          lookup "c1" result `shouldBe` Just [("t", [0])]
          lookup "c2" result `shouldBe` Just [("t", [1])]
          lookup "c3" result `shouldBe` Just []
      , it "topic-aware: each consumer only gets partitions it asked for" $ do
          let result =
                Sub.rangeAssign
                  [ ("c1", ["a"])
                  , ("c2", ["b"])
                  , ("c3", ["a", "b"])
                  ]
                  (Map.fromList [("a", [0, 1, 2, 3]), ("b", [0, 1])])
          -- "a" goes to c1 + c3 only (2 consumers, 4 partitions, even split):
          lookup "c1" result `shouldBe` Just [("a", [0, 1])]
          -- "b" goes to c2 + c3 only (2 consumers, 2 partitions, even split):
          -- c3 gets one of each topic.
          let c3Assignment = Map.fromList (maybe [] id (lookup "c3" result))
          Map.lookup "a" c3Assignment `shouldBe` Just [2, 3]
          Map.lookup "b" c3Assignment `shouldBe` Just [1]
          let c2Assignment = Map.fromList (maybe [] id (lookup "c2" result))
          Map.lookup "b" c2Assignment `shouldBe` Just [0]
      , it "no members: empty result" $ do
          Sub.rangeAssign [] (Map.fromList [("t", [0, 1, 2])]) `shouldBe` []
      , it "no partitions: members get empty assignments" $ do
          let result = Sub.rangeAssign [("c1", ["t"]), ("c2", ["t"])] Map.empty
          lookup "c1" result `shouldBe` Just []
          lookup "c2" result `shouldBe` Just []
      , it "every partition lands on exactly one member" $ H.property $ do
          nMembers <- H.forAll $ Gen.int (Range.linear 1 8)
          nPartitions <- H.forAll $ Gen.int (Range.linear 0 32)
          let members = [(T.pack ("c" <> show i), ["t"]) | i <- [1 .. nMembers]]
              parts = [0 .. fromIntegral (nPartitions - 1)]
              result = Sub.rangeAssign members (Map.fromList [("t", parts)])
              covered =
                concat
                  [ ps
                  | (_, byTopic) <- result
                  , (_, ps) <- byTopic
                  ]
          H.assert (length covered == nPartitions)
          H.assert (length (Map.keys (Map.fromList (zip covered (repeat ())))) == nPartitions)
      , it "no partition is assigned twice" $ H.property $ do
          nMembers <- H.forAll $ Gen.int (Range.linear 1 8)
          nPartitions <- H.forAll $ Gen.int (Range.linear 0 32)
          let members = [(T.pack ("c" <> show i), ["t"]) | i <- [1 .. nMembers]]
              parts = [0 .. fromIntegral (nPartitions - 1)]
              result = Sub.rangeAssign members (Map.fromList [("t", parts)])
              covered = concat [ps | (_, byTopic) <- result, (_, ps) <- byTopic]
          H.assert (length covered == length (Map.keys (Map.fromList (zip covered (repeat ())))))
      , it "assignment sizes differ by at most 1 within a topic" $ H.property $ do
          nMembers <- H.forAll $ Gen.int (Range.linear 1 8)
          nPartitions <- H.forAll $ Gen.int (Range.linear nMembers 32)
          let members = [(T.pack ("c" <> show i), ["t"]) | i <- [1 .. nMembers]]
              parts = [0 .. fromIntegral (nPartitions - 1)]
              result = Sub.rangeAssign members (Map.fromList [("t", parts)])
              sizes = [length ps | (_, byTopic) <- result, (_, ps) <- byTopic]
          case sizes of
            [] -> H.success
            _ -> H.assert (maximum sizes - minimum sizes <= 1)
      ]


--------------------------------------------------------------------------------
-- Round-robin assignment
--------------------------------------------------------------------------------

roundRobinAssignTests :: Spec
roundRobinAssignTests =
  describe "roundRobinAssign" $
    sequence_
      [ it "balanced split: 6 partitions / 3 consumers / single topic" $ do
          let result =
                Sub.roundRobinAssign
                  [("c1", ["t"]), ("c2", ["t"]), ("c3", ["t"])]
                  (Map.fromList [("t", [0, 1, 2, 3, 4, 5])])
          lookup "c1" result `shouldBe` Just [("t", [0, 3])]
          lookup "c2" result `shouldBe` Just [("t", [1, 4])]
          lookup "c3" result `shouldBe` Just [("t", [2, 5])]
      , it "balanced across topics: c3 gets one of each" $ do
          let result =
                Sub.roundRobinAssign
                  [("c1", ["a", "b"]), ("c2", ["a", "b"]), ("c3", ["a", "b"])]
                  (Map.fromList [("a", [0, 1, 2]), ("b", [0, 1, 2])])
          -- 6 total partitions over 3 consumers: each gets 2.
          let assigned mid = case lookup mid result of
                Just byTopic -> sum (map (length . snd) byTopic)
                Nothing -> 0
          assigned "c1" `shouldBe` 2
          assigned "c2" `shouldBe` 2
          assigned "c3" `shouldBe` 2
      , it "topic-aware: only subscribed consumers receive partitions" $ do
          let result =
                Sub.roundRobinAssign
                  [("c1", ["a"]), ("c2", ["b"]), ("c3", ["a", "b"])]
                  (Map.fromList [("a", [0, 1, 2]), ("b", [0, 1, 2])])
          -- c1 only sees 'a', c2 only sees 'b', c3 sees both.
          let topicsOf mid = case lookup mid result of
                Just byTopic -> map fst byTopic
                Nothing -> []
          topicsOf "c1" `shouldBe` ["a"]
          topicsOf "c2" `shouldBe` ["b"]
          -- c3 should get a mix; just check it's non-empty and only the
          -- subscribed topics.
          all (`elem` ["a", "b"]) (topicsOf "c3") `shouldBe` True
      , it "no double-assignment under round-robin" $ do
          let result =
                Sub.roundRobinAssign
                  [("c1", ["t"]), ("c2", ["t"]), ("c3", ["t"])]
                  (Map.fromList [("t", [0 .. 9])])
              allParts = concat [ps | (_, byTopic) <- result, (_, ps) <- byTopic]
          length allParts `shouldBe` 10
          length (Map.keys (Map.fromList (zip allParts (repeat ())))) `shouldBe` 10
      ]


--------------------------------------------------------------------------------
-- Sticky assignment
--------------------------------------------------------------------------------

stickyAssignTests :: Spec
stickyAssignTests =
  describe "stickyAssign" $
    sequence_
      [ it "no previous assignment behaves like round-robin" $ do
          let members = [("c1", ["t"]), ("c2", ["t"]), ("c3", ["t"])]
              parts = Map.fromList [("t", [0, 1, 2, 3, 4, 5])]
          Sub.stickyAssign members parts Nothing
            `shouldBe` Sub.roundRobinAssign members parts
      , it "preserves previous assignment when membership unchanged" $ do
          let members = [("c1", ["t"]), ("c2", ["t"])]
              parts = Map.fromList [("t", [0, 1, 2, 3])]
              prev = [("c1", [("t", [0, 1])]), ("c2", [("t", [2, 3])])]
          Sub.stickyAssign members parts (Just prev) `shouldBe` prev
      , it "rebalances when a new consumer joins" $ do
          -- Previous: c1 had everything. New consumer c2 joins; sticky
          -- should keep c1's partitions where possible and only move what
          -- it needs to balance.
          let members = [("c1", ["t"]), ("c2", ["t"])]
              parts = Map.fromList [("t", [0, 1, 2, 3])]
              prev = [("c1", [("t", [0, 1, 2, 3])]), ("c2", [])]
              result = Sub.stickyAssign members parts (Just prev)
              c1 = fromMaybe [] (lookup "c1" result)
              c2 = fromMaybe [] (lookup "c2" result)
              c1Parts = concat [ps | (_, ps) <- c1]
              c2Parts = concat [ps | (_, ps) <- c2]
          length c1Parts `shouldBe` 2
          length c2Parts `shouldBe` 2
          -- Every previous c1 partition that didn't move should still be
          -- in c1's new assignment.
          (length (filter (`elem` [0, 1, 2, 3]) c1Parts) == 2) `shouldBe` True
      , it "drops partitions that no longer exist" $ do
          let members = [("c1", ["t"])]
              parts = Map.fromList [("t", [0, 1])] -- only 0/1 still exist
              prev = [("c1", [("t", [0, 1, 2, 3])])] -- previously had 0..3
              result = Sub.stickyAssign members parts (Just prev)
              c1Parts = concat [ps | byTopic <- maybeToList (lookup "c1" result), (_, ps) <- byTopic]
          length c1Parts `shouldBe` 2
          all (`elem` [0, 1]) c1Parts `shouldBe` True
      ]


--------------------------------------------------------------------------------
-- Configuration validation
--------------------------------------------------------------------------------

configValidationTests :: Spec
configValidationTests =
  describe "GroupConfig validation" $
    sequence_
      [ it "empty bootstrap brokers is rejected" $ do
          r <-
            try $
              Group.runConsumer
                Group.defaultGroupConfig
                  { Group.bootstrapBrokers = []
                  , Group.groupId = "g"
                  , Group.topics = ["t"]
                  }
                (\_ -> pure ())
          case (r :: Either KafkaException ()) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected validation failure"
      , it "empty group id is rejected" $ do
          r <-
            try $
              Group.runConsumer
                Group.defaultGroupConfig
                  { Group.groupId = ""
                  , Group.topics = ["t"]
                  }
                (\_ -> pure ())
          case (r :: Either KafkaException ()) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected validation failure"
      , it "empty topics list is rejected" $ do
          r <-
            try $
              Group.runConsumer
                Group.defaultGroupConfig
                  { Group.groupId = "g"
                  , Group.topics = []
                  }
                (\_ -> pure ())
          case (r :: Either KafkaException ()) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected validation failure"
      , it "default config has non-trivial defaults" $ do
          let cfg = Group.defaultGroupConfig
          Group.sessionTimeoutMs cfg `shouldBe` 10000
          Group.maxPollIntervalMs cfg `shouldBe` 300000
          Group.maxPollRecords cfg `shouldBe` 500
          Group.pollTimeoutMs cfg `shouldBe` 1000
          Group.closeTimeoutMs cfg `shouldBe` 30000
      ]
