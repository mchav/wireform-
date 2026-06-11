{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.TestUtilsSpec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


tests :: Spec
tests =
  describe "TestInputTopic / TestOutputTopic" $
    sequence_
      [ pipe_kv_round_trips
      , pipe_all_in_order
      , read_values_to_list
      , is_output_empty_after_drain
      ]


passthroughDriver :: IO TopologyTestDriver
passthroughDriver = do
  b <- newStreamsBuilder
  s <-
    streamFromTopic
      b
      (topicName "in")
      (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  newDriver topo "tu-app"


pipe_kv_round_trips :: Spec
pipe_kv_round_trips =
  it "pipeKV + readKV through TestInputTopic / TestOutputTopic" $ do
    d <- passthroughDriver
    let inT = createInputTopic d (topicName "in") textSerde textSerde
        outT = createOutputTopic d (topicName "out") textSerde textSerde
    pipeKV inT (Just "k") "hello"
    Just (Right (mk, v)) <- readKV outT
    mk `shouldBe` Just "k"
    v `shouldBe` "hello"
    closeDriver d


pipe_all_in_order :: Spec
pipe_all_in_order =
  it "pipeAll preserves submission order" $ do
    d <- passthroughDriver
    let inT = createInputTopic d (topicName "in") textSerde textSerde
        outT = createOutputTopic d (topicName "out") textSerde textSerde
    pipeAll
      inT
      [ (Just "a", "v1", Timestamp 0)
      , (Just "b", "v2", Timestamp 1)
      , (Just "c", "v3", Timestamp 2)
      ]
    vs <- readValuesToList outT
    vs `shouldBe` ["v1", "v2", "v3"]
    closeDriver d


read_values_to_list :: Spec
read_values_to_list =
  it "readValuesToList drains the entire topic" $ do
    d <- passthroughDriver
    let inT = createInputTopic d (topicName "in") textSerde textSerde
        outT = createOutputTopic d (topicName "out") textSerde textSerde
    mapM_ (\v -> pipeValue inT v) ["x", "y", "z"]
    readValuesToList outT >>= (`shouldBe` ["x", "y", "z"])
    -- Subsequent read returns nothing — readOutput drains.
    readValuesToList outT >>= (`shouldBe` [])
    closeDriver d


is_output_empty_after_drain :: Spec
is_output_empty_after_drain =
  it "isOutputEmpty: True before any input, False after, then True post-drain" $ do
    d <- passthroughDriver
    let inT = createInputTopic d (topicName "in") textSerde textSerde
        outT = createOutputTopic d (topicName "out") textSerde textSerde
    isOutputEmpty outT >>= (`shouldBe` True)
    pipeValue inT "x"
    isOutputEmpty outT >>= (`shouldBe` False)
    _ <- readValuesToList outT
    isOutputEmpty outT >>= (`shouldBe` True)
    closeDriver d
