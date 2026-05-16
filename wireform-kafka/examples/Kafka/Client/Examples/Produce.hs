{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Client.Examples.Produce
-- Description : The smallest possible producer — open, send, close.
--
-- Mirrors the README's hello-world recipe. Open a producer with
-- 'withProducer', send a single record, the bracket flushes + closes
-- on exit. Run:
--
-- > cabal run wireform-kafka-client-examples produce
module Kafka.Client.Examples.Produce (runDemo) where

import qualified Kafka

runDemo :: IO ()
runDemo =
  Kafka.withProducer ["localhost:9092"] Kafka.defaultProducerConfig $ \p -> do
    md <- Kafka.sendMessage p "events" Nothing "hello"
    print md
