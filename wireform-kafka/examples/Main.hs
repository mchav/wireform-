{-# LANGUAGE LambdaCase #-}

{- |
Module      : Main
Description : Dispatcher for the wireform-kafka client examples.

Run one of the demos by name:

> cabal run wireform-kafka-client-examples produce
> cabal run wireform-kafka-client-examples produce-typed
> cabal run wireform-kafka-client-examples consume
> cabal run wireform-kafka-client-examples group
> cabal run wireform-kafka-client-examples transaction

Every demo assumes a broker reachable at @localhost:9092@.
-}
module Main (main) where

import Kafka.Client.Examples.Consume qualified as Consume
import Kafka.Client.Examples.Group qualified as Group
import Kafka.Client.Examples.Produce qualified as Produce
import Kafka.Client.Examples.ProduceTyped qualified as ProduceTyped
import Kafka.Client.Examples.Transaction qualified as Transaction
import System.Environment (getArgs)
import System.Exit (exitFailure)


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["produce"] -> Produce.runDemo
    ["produce-typed"] -> ProduceTyped.runDemo
    ["consume"] -> Consume.runDemo
    ["group"] -> Group.runDemo
    ["transaction"] -> Transaction.runDemo
    _ -> do
      putStrLn "wireform-kafka client examples"
      putStrLn ""
      putStrLn "Run one of: produce | produce-typed | consume | group | transaction"
      exitFailure
