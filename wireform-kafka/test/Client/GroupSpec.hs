{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Client.GroupSpec
Description : Tests for the high-level consumer-group API

These tests exercise the pieces of "Kafka.Client.Group" and
"Kafka.Client.Internal.Subscribe" that don't need a live broker:
the range-assignor algebra, configuration validation, and the
default policies. The actual end-to-end @runConsumer@ is exercised
in the broker-gated integration suite.
-}
module Client.GroupSpec (groupSpec) where

import Control.Exception (try, IOException)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Kafka.Client.Group as Group
import qualified Kafka.Client.Internal.Subscribe as Sub

groupSpec :: TestTree
groupSpec = testGroup "Group consumer (high-level)"
  [ rangeAssignTests
  , configValidationTests
  ]

--------------------------------------------------------------------------------
-- Range assignment
--------------------------------------------------------------------------------

rangeAssignTests :: TestTree
rangeAssignTests = testGroup "rangeAssign"
  [ testCase "even split: 6 partitions / 3 consumers / single topic" $ do
      let result = Sub.rangeAssign
            [ ("c1", ["t"])
            , ("c2", ["t"])
            , ("c3", ["t"])
            ]
            (Map.fromList [("t", [0,1,2,3,4,5])])
      lookup "c1" result @?= Just [("t",[0,1])]
      lookup "c2" result @?= Just [("t",[2,3])]
      lookup "c3" result @?= Just [("t",[4,5])]

  , testCase "uneven split: 7 partitions / 3 consumers" $ do
      let result = Sub.rangeAssign
            [ ("c1", ["t"]), ("c2", ["t"]), ("c3", ["t"]) ]
            (Map.fromList [("t", [0..6])])
      -- The first (length `mod` n) consumers each take one extra
      -- partition. 7 mod 3 = 1, so c1 gets 3, c2 gets 2, c3 gets 2.
      lookup "c1" result @?= Just [("t",[0,1,2])]
      lookup "c2" result @?= Just [("t",[3,4])]
      lookup "c3" result @?= Just [("t",[5,6])]

  , testCase "more consumers than partitions: extras get nothing" $ do
      let result = Sub.rangeAssign
            [("c1", ["t"]), ("c2", ["t"]), ("c3", ["t"])]
            (Map.fromList [("t", [0,1])])
      lookup "c1" result @?= Just [("t",[0])]
      lookup "c2" result @?= Just [("t",[1])]
      lookup "c3" result @?= Just []

  , testCase "topic-aware: each consumer only gets partitions it asked for" $ do
      let result = Sub.rangeAssign
            [ ("c1", ["a"])
            , ("c2", ["b"])
            , ("c3", ["a","b"])
            ]
            (Map.fromList [("a", [0,1,2,3]), ("b", [0,1])])
      -- "a" goes to c1 + c3 only (2 consumers, 4 partitions, even split):
      lookup "c1" result @?= Just [("a",[0,1])]
      -- "b" goes to c2 + c3 only (2 consumers, 2 partitions, even split):
      -- c3 gets one of each topic.
      let c3Assignment = Map.fromList (maybe [] id (lookup "c3" result))
      Map.lookup "a" c3Assignment @?= Just [2,3]
      Map.lookup "b" c3Assignment @?= Just [1]
      let c2Assignment = Map.fromList (maybe [] id (lookup "c2" result))
      Map.lookup "b" c2Assignment @?= Just [0]

  , testCase "no members: empty result" $ do
      Sub.rangeAssign [] (Map.fromList [("t", [0,1,2])]) @?= []

  , testCase "no partitions: members get empty assignments" $ do
      let result = Sub.rangeAssign [("c1", ["t"]), ("c2", ["t"])] Map.empty
      lookup "c1" result @?= Just []
      lookup "c2" result @?= Just []

  , testProperty "every partition lands on exactly one member" $ H.property $ do
      nMembers    <- H.forAll $ Gen.int (Range.linear 1 8)
      nPartitions <- H.forAll $ Gen.int (Range.linear 0 32)
      let members = [ (T.pack ("c" <> show i), ["t"]) | i <- [1..nMembers] ]
          parts   = [0 .. fromIntegral (nPartitions - 1)]
          result  = Sub.rangeAssign members (Map.fromList [("t", parts)])
          covered = concat [ ps | (_, byTopic) <- result
                                , (_, ps)     <- byTopic ]
      H.assert (length covered == nPartitions)
      H.assert (length (Map.keys (Map.fromList (zip covered (repeat ())))) == nPartitions)

  , testProperty "no partition is assigned twice" $ H.property $ do
      nMembers    <- H.forAll $ Gen.int (Range.linear 1 8)
      nPartitions <- H.forAll $ Gen.int (Range.linear 0 32)
      let members = [ (T.pack ("c" <> show i), ["t"]) | i <- [1..nMembers] ]
          parts   = [0 .. fromIntegral (nPartitions - 1)]
          result  = Sub.rangeAssign members (Map.fromList [("t", parts)])
          covered = concat [ ps | (_, byTopic) <- result, (_, ps) <- byTopic ]
      H.assert (length covered == length (Map.keys (Map.fromList (zip covered (repeat ())))))

  , testProperty "assignment sizes differ by at most 1 within a topic" $ H.property $ do
      nMembers    <- H.forAll $ Gen.int (Range.linear 1 8)
      nPartitions <- H.forAll $ Gen.int (Range.linear nMembers 32)
      let members = [ (T.pack ("c" <> show i), ["t"]) | i <- [1..nMembers] ]
          parts   = [0 .. fromIntegral (nPartitions - 1)]
          result  = Sub.rangeAssign members (Map.fromList [("t", parts)])
          sizes   = [ length ps | (_, byTopic) <- result, (_, ps) <- byTopic ]
      case sizes of
        []      -> H.success
        _       -> H.assert (maximum sizes - minimum sizes <= 1)
  ]

--------------------------------------------------------------------------------
-- Configuration validation
--------------------------------------------------------------------------------

configValidationTests :: TestTree
configValidationTests = testGroup "GroupConfig validation"
  [ testCase "empty bootstrap brokers is rejected" $ do
      r <- try $ Group.runConsumer
              Group.defaultGroupConfig
                { Group.gcBootstrapBrokers = []
                , Group.gcGroupId          = "g"
                , Group.gcTopics           = ["t"]
                }
              (\_ -> pure ())
      case (r :: Either IOException ()) of
        Left _  -> pure ()
        Right _ -> assertFailure "expected validation failure"

  , testCase "empty group id is rejected" $ do
      r <- try $ Group.runConsumer
              Group.defaultGroupConfig
                { Group.gcGroupId = ""
                , Group.gcTopics  = ["t"]
                }
              (\_ -> pure ())
      case (r :: Either IOException ()) of
        Left _  -> pure ()
        Right _ -> assertFailure "expected validation failure"

  , testCase "empty topics list is rejected" $ do
      r <- try $ Group.runConsumer
              Group.defaultGroupConfig
                { Group.gcGroupId = "g"
                , Group.gcTopics  = []
                }
              (\_ -> pure ())
      case (r :: Either IOException ()) of
        Left _  -> pure ()
        Right _ -> assertFailure "expected validation failure"

  , testCase "default config has non-trivial defaults" $ do
      let cfg = Group.defaultGroupConfig
      Group.gcSessionTimeoutMs   cfg @?= 10000
      Group.gcMaxPollIntervalMs  cfg @?= 300000
      Group.gcMaxPollRecords     cfg @?= 500
      Group.gcPollTimeoutMs      cfg @?= 1000
      Group.gcCloseTimeoutMs     cfg @?= 30000
  ]
