{-# LANGUAGE OverloadedStrings #-}

module Streams.NamedSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams.Imperative

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

tests :: TestTree
tests = testGroup "Named operators"
  [ named_operator_appears_in_description
  , unnamed_operator_uses_generated_name
  , named_filter_runs_correctly
  ]

named_operator_appears_in_description :: TestTree
named_operator_appears_in_description =
  testCase "user-supplied operator name shows up in TopologyDescription" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    s' <- mapValuesNamed (named "MY-UPPER") T.toUpper s
    toTopicNamed (named "MY-SINK") (topicName "out")
                 (produced textSerde textSerde) s'
    topo <- buildTopology b
    let txt = pretty (describeTopology topo)
    assertBool ("MY-UPPER missing in:\n" <> T.unpack txt)
      ("MY-UPPER" `T.isInfixOf` txt)
    assertBool ("MY-SINK missing in:\n" <> T.unpack txt)
      ("MY-SINK" `T.isInfixOf` txt)

unnamed_operator_uses_generated_name :: TestTree
unnamed_operator_uses_generated_name =
  testCase "operator without an explicit Named uses the auto-generated prefix" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    s' <- mapValues T.toUpper s
    toTopic (topicName "out") (produced textSerde textSerde) s'
    topo <- buildTopology b
    let txt = pretty (describeTopology topo)
    assertBool ("KSTREAM-MAPVALUES- missing in:\n" <> T.unpack txt)
      ("KSTREAM-MAPVALUES-" `T.isInfixOf` txt)

named_filter_runs_correctly :: TestTree
named_filter_runs_correctly =
  testCase "filterStreamNamed actually filters records" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    s' <- filterStreamNamed (named "MY-FILTER")
            (\r -> recordValue r /= "skip") s
    toTopic (topicName "out") (produced textSerde textSerde) s'
    topo <- buildTopology b
    driver <- newDriver topo "n-app"
    pipeInput driver (topicName "in") Nothing (bytes "keep1") (Timestamp 0) 0
    pipeInput driver (topicName "in") Nothing (bytes "skip")  (Timestamp 0) 0
    pipeInput driver (topicName "in") Nothing (bytes "keep2") (Timestamp 0) 0
    out <- readOutput driver (topicName "out")
    length out @?= 2
    closeDriver driver
