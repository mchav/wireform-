{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : TestUtil.BrokerGate
Description : Skip tests that need a live Kafka broker

These helpers gate tests that try to connect to a real Kafka broker
(default @localhost:9092@). When the @WIREFORM_KAFKA_BROKER@
environment variable is unset they short-circuit and report success
(skipped) instead of timing out on a failed connect.

To exercise the full suite against a broker:

@
WIREFORM_KAFKA_BROKER=localhost:9092 cabal test wireform-kafka
@
-}
module TestUtil.BrokerGate
  ( hasBroker
  , brokerCase
  , brokerProperty
  ) where

import qualified System.Environment as Env
import qualified Hedgehog as H
import Test.Syd
import Test.Syd.Hedgehog ()

hasBroker :: IO Bool
hasBroker = do
  m <- Env.lookupEnv "WIREFORM_KAFKA_BROKER"
  pure $ case m of
    Just v | not (null v) -> True
    _                     -> False

-- | A test case that requires a live broker. When no broker is configured
-- the body is skipped (the case still appears as a pass).
brokerCase :: String -> IO () -> Spec
brokerCase name body = it name $ do
  ok <- hasBroker
  if ok then body else pure ()

-- | A property test that requires a live broker. When no broker is
-- configured the property body is skipped via `H.discard`.
brokerProperty :: String -> H.Property -> Spec
brokerProperty name prop = it name $ H.withTests 1 $ H.property $ do
  ok <- H.evalIO hasBroker
  if ok
    then H.evalIO (H.check prop) >>= H.assert
    else H.success
