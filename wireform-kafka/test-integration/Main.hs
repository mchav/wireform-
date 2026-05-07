{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Integration test suite entry point

Integration tests that require a running Kafka cluster.

Set the @WIREFORM_KAFKA_BROKER@ environment variable (e.g.
@WIREFORM_KAFKA_BROKER=localhost:9092@) to run against a live broker.
When unset the suite exits cleanly without running anything, so it
can be enabled by default in CI without forcing every developer to
stand up a broker locally.

Run via:

> WIREFORM_KAFKA_BROKER=localhost:9092 cabal test wireform-kafka:wireform-kafka-integration
-}
module Main (main) where

import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.IO as IO

import Test.Tasty
import qualified Integration.BasicSpec

main :: IO ()
main = do
  m <- Env.lookupEnv "WIREFORM_KAFKA_BROKER"
  case m of
    Just v | not (null v) ->
      defaultMain tests
    _ -> do
      IO.hPutStrLn IO.stderr
        "wireform-kafka-integration: WIREFORM_KAFKA_BROKER unset, skipping."
      Exit.exitSuccess

tests :: TestTree
tests = testGroup "Kafka Integration Tests"
  [ Integration.BasicSpec.tests
  ]
