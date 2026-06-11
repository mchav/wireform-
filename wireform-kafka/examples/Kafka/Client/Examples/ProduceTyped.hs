{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Client.Examples.ProduceTyped
Description : Typed publish through a 'Kafka.Topic.Topic'.

Saves the @encodeUtf8@ \/ JSON-encoder boilerplate by letting the
'Kafka.Topic.Topic' carry the key and value serdes. The same
producer is reused for every record.

> cabal run wireform-kafka-client-examples produce-typed
-}
module Kafka.Client.Examples.ProduceTyped (runDemo) where

import Data.Text (Text)
import Kafka qualified
import Kafka.Topic qualified as Topic


events :: Topic.Topic Text Text
events = Topic.textTopic "events"


runDemo :: IO ()
runDemo =
  Kafka.withProducer ["localhost:9092"] Kafka.defaultProducerConfig $ \p -> do
    md <- Kafka.publish p events (Just "k1") "hello"
    print md
    md' <- Kafka.publish p events (Just "k2") "world"
    print md'
