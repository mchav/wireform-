{-# LANGUAGE OverloadedStrings #-}

module Streams.NamedSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


tests :: Spec
tests =
  describe "Named operators" $
    sequence_
      [ named_operator_appears_in_description
      , unnamed_operator_uses_generated_name
      , named_filter_runs_correctly
      ]


named_operator_appears_in_description :: Spec
named_operator_appears_in_description =
  it "user-supplied operator name shows up in TopologyDescription" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    s' <- mapValuesNamed (named "MY-UPPER") T.toUpper s
    toTopicNamed
      (named "MY-SINK")
      (topicName "out")
      (produced textSerde textSerde)
      s'
    topo <- buildTopology b
    let txt = pretty (describeTopology topo)
    (if ("MY-UPPER" `T.isInfixOf` txt) then pure () else expectationFailure ("MY-UPPER missing in:\n" <> T.unpack txt))
    (if ("MY-SINK" `T.isInfixOf` txt) then pure () else expectationFailure ("MY-SINK missing in:\n" <> T.unpack txt))


unnamed_operator_uses_generated_name :: Spec
unnamed_operator_uses_generated_name =
  it "operator without an explicit Named uses the auto-generated prefix" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    s' <- mapValues T.toUpper s
    toTopic (topicName "out") (produced textSerde textSerde) s'
    topo <- buildTopology b
    let txt = pretty (describeTopology topo)
    (if ("KSTREAM-MAPVALUES-" `T.isInfixOf` txt) then pure () else expectationFailure ("KSTREAM-MAPVALUES- missing in:\n" <> T.unpack txt))


named_filter_runs_correctly :: Spec
named_filter_runs_correctly =
  it "filterStreamNamed actually filters records" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    s' <-
      filterStreamNamed
        (named "MY-FILTER")
        (\r -> recordValue r /= "skip")
        s
    toTopic (topicName "out") (produced textSerde textSerde) s'
    topo <- buildTopology b
    driver <- newDriver topo "n-app"
    pipeInput driver (topicName "in") Nothing (bytes "keep1") (Timestamp 0) 0
    pipeInput driver (topicName "in") Nothing (bytes "skip") (Timestamp 0) 0
    pipeInput driver (topicName "in") Nothing (bytes "keep2") (Timestamp 0) 0
    out <- readOutput driver (topicName "out")
    length out `shouldBe` 2
    closeDriver driver
