{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.TestUtilsSpec (tests) where

import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Imperative

tests :: TestTree
tests = testGroup "TestInputTopic / TestOutputTopic"
  [ pipe_kv_round_trips
  , pipe_all_in_order
  , read_values_to_list
  , is_output_empty_after_drain
  ]

passthroughDriver :: IO TopologyTestDriver
passthroughDriver = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in")
         (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  newDriver topo "tu-app"

pipe_kv_round_trips :: TestTree
pipe_kv_round_trips =
  testCase "pipeKV + readKV through TestInputTopic / TestOutputTopic" $ do
    d <- passthroughDriver
    let inT  = createInputTopic  d (topicName "in")  textSerde textSerde
        outT = createOutputTopic d (topicName "out") textSerde textSerde
    pipeKV inT (Just "k") "hello"
    Just (Right (mk, v)) <- readKV outT
    mk @?= Just "k"
    v  @?= "hello"
    closeDriver d

pipe_all_in_order :: TestTree
pipe_all_in_order =
  testCase "pipeAll preserves submission order" $ do
    d <- passthroughDriver
    let inT  = createInputTopic  d (topicName "in")  textSerde textSerde
        outT = createOutputTopic d (topicName "out") textSerde textSerde
    pipeAll inT
      [ (Just "a", "v1", Timestamp 0)
      , (Just "b", "v2", Timestamp 1)
      , (Just "c", "v3", Timestamp 2)
      ]
    vs <- readValuesToList outT
    vs @?= ["v1", "v2", "v3"]
    closeDriver d

read_values_to_list :: TestTree
read_values_to_list =
  testCase "readValuesToList drains the entire topic" $ do
    d <- passthroughDriver
    let inT  = createInputTopic  d (topicName "in")  textSerde textSerde
        outT = createOutputTopic d (topicName "out") textSerde textSerde
    mapM_ (\v -> pipeValue inT v) ["x", "y", "z"]
    readValuesToList outT >>= (@?= ["x", "y", "z"])
    -- Subsequent read returns nothing — readOutput drains.
    readValuesToList outT >>= (@?= [])
    closeDriver d

is_output_empty_after_drain :: TestTree
is_output_empty_after_drain =
  testCase "isOutputEmpty: True before any input, False after, then True post-drain" $ do
    d <- passthroughDriver
    let inT  = createInputTopic  d (topicName "in")  textSerde textSerde
        outT = createOutputTopic d (topicName "out") textSerde textSerde
    isOutputEmpty outT >>= (@?= True)
    pipeValue inT "x"
    isOutputEmpty outT >>= (@?= False)
    _ <- readValuesToList outT
    isOutputEmpty outT >>= (@?= True)
    closeDriver d
