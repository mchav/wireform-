{-# LANGUAGE OverloadedStrings #-}

-- | Live-broker integration tests for kafka-streams. Skipped at run
-- time unless @WIREFORM_KAFKA_BROKER=host:port@ is set, mirroring
-- the convention of the existing @wireform-kafka-integration@ suite.
module Main (main) where

import qualified Streams.Integration.RoundTrip as RT
import System.Environment (lookupEnv)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

main :: IO ()
main = do
  brokers <- lookupEnv "WIREFORM_KAFKA_BROKER"
  case brokers of
    Nothing -> defaultMain $ testGroup "kafka-streams-integration"
      [ testCase "skipped (set WIREFORM_KAFKA_BROKER to enable)" (pure ())
      ]
    Just bs -> defaultMain $ testGroup "kafka-streams-integration"
      [ RT.tests bs
      ]
