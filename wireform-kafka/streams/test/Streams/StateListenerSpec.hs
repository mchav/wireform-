{-# LANGUAGE OverloadedStrings #-}

module Streams.StateListenerSpec (tests) where

import Data.IORef
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Imperative

tests :: TestTree
tests = testGroup "StateListener"
  [ listener_observes_close_transition
  , listener_default_does_nothing
  , listener_can_be_replaced
  ]

mkHandle :: IO KafkaStreams
mkHandle = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left  err -> error (show err)
    Right v   -> newKafkaStreams defaultStreamsConfig
                   { applicationId = "sl-app"
                   , bootstrapServers = ["mock:0"]
                   } v

listener_observes_close_transition :: TestTree
listener_observes_close_transition =
  testCase "the listener observes the Closing -> Closed transition" $ do
    ks <- mkHandle
    log_ <- newIORef ([] :: [(StreamsStatus, StreamsStatus)])
    setStateListener ks (\old new -> modifyIORef' log_ ((old, new) :))
    closeKafkaStreams ks
    -- closeKafkaStreams transitions Created -> Closing -> Closed.
    seen <- reverse <$> readIORef log_
    seen @?= [ (StreamsCreated, StreamsClosing)
             , (StreamsClosing, StreamsClosed)
             ]

listener_default_does_nothing :: TestTree
listener_default_does_nothing =
  testCase "default listener does not raise on transitions" $ do
    ks <- mkHandle
    closeKafkaStreams ks
    -- If we reached here without exception, default works.
    streamsStatus ks >>= (@?= StreamsClosed)

listener_can_be_replaced :: TestTree
listener_can_be_replaced =
  testCase "setting a listener twice keeps the most recent" $ do
    ks <- mkHandle
    a <- newIORef (0 :: Int)
    b <- newIORef (0 :: Int)
    setStateListener ks (\_ _ -> modifyIORef' a (+ 1))
    setStateListener ks (\_ _ -> modifyIORef' b (+ 1))
    closeKafkaStreams ks
    aN <- readIORef a
    bN <- readIORef b
    aN @?= 0     -- replaced before any transition
    bN @?= 2     -- two transitions (Closing, Closed)
