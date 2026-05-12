{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Main
-- Description : Dispatcher for the wireform-kafka client examples.
--
-- Run one of the demos by name:
--
-- > cabal run wireform-kafka-client-examples produce
-- > cabal run wireform-kafka-client-examples produce-typed
-- > cabal run wireform-kafka-client-examples consume
-- > cabal run wireform-kafka-client-examples group
-- > cabal run wireform-kafka-client-examples transaction
--
-- Every demo assumes a broker reachable at @localhost:9092@.
module Main (main) where

import           System.Environment                 (getArgs)
import           System.Exit                        (exitFailure)

import qualified Kafka.Client.Examples.Consume     as Consume
import qualified Kafka.Client.Examples.Group       as Group
import qualified Kafka.Client.Examples.Produce     as Produce
import qualified Kafka.Client.Examples.ProduceTyped as ProduceTyped
import qualified Kafka.Client.Examples.Transaction as Transaction

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["produce"]       -> Produce.runDemo
    ["produce-typed"] -> ProduceTyped.runDemo
    ["consume"]       -> Consume.runDemo
    ["group"]         -> Group.runDemo
    ["transaction"]   -> Transaction.runDemo
    _ -> do
      putStrLn "wireform-kafka client examples"
      putStrLn ""
      putStrLn "Run one of: produce | produce-typed | consume | group | transaction"
      exitFailure
