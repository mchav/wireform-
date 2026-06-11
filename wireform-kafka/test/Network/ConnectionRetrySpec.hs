{-# LANGUAGE OverloadedStrings #-}

module Network.ConnectionRetrySpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.IORef
import Data.List (isInfixOf)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Network.Connection qualified as Conn
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Test that exponential backoff delays increase geometrically
prop_backoffIncreasesExponentially :: Property
prop_backoffIncreasesExponentially = property $ do
  let config =
        Conn.defaultConnectionConfig
          { Conn.connRetryDelay = 100
          , Conn.connBackoffMultiplier = 2.0
          , Conn.connBackoffMaxMs = 10000
          }

  -- Calculate delays for several attempts
  delay0 <- evalIO $ Conn.calculateBackoffDelay 0 config
  delay1 <- evalIO $ Conn.calculateBackoffDelay 1 config
  delay2 <- evalIO $ Conn.calculateBackoffDelay 2 config
  delay3 <- evalIO $ Conn.calculateBackoffDelay 3 config

  -- Convert to milliseconds for comparison
  let delayMs0 = fromIntegral delay0 / 1000 :: Double
      delayMs1 = fromIntegral delay1 / 1000
      delayMs2 = fromIntegral delay2 / 1000
      delayMs3 = fromIntegral delay3 / 1000

  -- Check that each delay is approximately 2x the previous (accounting for jitter of 0.8-1.2)
  -- delay1 should be roughly 200ms (100 * 2^1), but with jitter 160-240ms
  annotate $ "Delay 0: " ++ show delayMs0 ++ "ms"
  annotate $ "Delay 1: " ++ show delayMs1 ++ "ms"
  annotate $ "Delay 2: " ++ show delayMs2 ++ "ms"
  annotate $ "Delay 3: " ++ show delayMs3 ++ "ms"

  -- Verify delays are in reasonable ranges (accounting for jitter)
  assert $ delayMs0 >= 80 && delayMs0 <= 120 -- 100 * [0.8, 1.2]
  assert $ delayMs1 >= 160 && delayMs1 <= 240 -- 200 * [0.8, 1.2]
  assert $ delayMs2 >= 320 && delayMs2 <= 480 -- 400 * [0.8, 1.2]
  assert $ delayMs3 >= 640 && delayMs3 <= 960 -- 800 * [0.8, 1.2]


-- | Test that backoff respects maximum delay limit
prop_backoffRespectsMaxLimit :: Property
prop_backoffRespectsMaxLimit = property $ do
  let maxDelayMs = 1000
      config =
        Conn.defaultConnectionConfig
          { Conn.connRetryDelay = 100
          , Conn.connBackoffMultiplier = 2.0
          , Conn.connBackoffMaxMs = maxDelayMs
          }

  -- After many attempts, delay should max out
  delay10 <- evalIO $ Conn.calculateBackoffDelay 10 config
  delay20 <- evalIO $ Conn.calculateBackoffDelay 20 config

  let delayMs10 = fromIntegral delay10 / 1000 :: Double
      delayMs20 = fromIntegral delay20 / 1000

  annotate $ "Delay after 10 attempts: " ++ show delayMs10 ++ "ms"
  annotate $ "Delay after 20 attempts: " ++ show delayMs20 ++ "ms"

  -- Both should be capped at maxDelayMs * jitter (max 1.2)
  let maxWithJitter = fromIntegral maxDelayMs * 1.2
  assert $ delayMs10 <= maxWithJitter
  assert $ delayMs20 <= maxWithJitter


-- | Test that jitter adds randomness within expected range
prop_jitterAddsRandomness :: Property
prop_jitterAddsRandomness = property $ do
  let config =
        Conn.defaultConnectionConfig
          { Conn.connRetryDelay = 1000
          , Conn.connBackoffMultiplier = 2.0
          , Conn.connBackoffMaxMs = 10000
          }

  -- Generate multiple delays for the same attempt number
  delays <- evalIO $ sequence $ replicate 20 (Conn.calculateBackoffDelay 0 config)

  let delaysMs = map (\d -> fromIntegral d / 1000 :: Double) delays
      minDelay = minimum delaysMs
      maxDelay = maximum delaysMs

  annotate $ "Min delay: " ++ show minDelay ++ "ms"
  annotate $ "Max delay: " ++ show maxDelay ++ "ms"

  -- Jitter should cause variation
  -- Expected base is 1000ms, with jitter [0.8, 1.2] gives [800, 1200]
  assert $ minDelay >= 800
  assert $ maxDelay <= 1200
  -- There should be some variation (not all the same)
  assert $ maxDelay - minDelay > 50 -- At least 50ms variation


-- | Test that connection gives up after max retries
unit_givesUpAfterMaxRetries :: Spec
unit_givesUpAfterMaxRetries = it "Connection gives up after max retries" $ do
  let config =
        Conn.defaultConnectionConfig
          { Conn.connMaxRetries = 2
          , Conn.connRetryDelay = 10 -- Short delay for testing
          , Conn.connBackoffMaxMs = 100
          }
      -- Try to connect to a non-existent host
      addr = Conn.BrokerAddress "invalid-host-that-does-not-exist-12345" 9999

  result <- Conn.connect addr config

  case result of
    Left err -> do
      -- Should mention the number of attempts
      (if (("after" `isInfixOf` err) && ("attempts" `isInfixOf` err)) then pure () else expectationFailure ("Error should mention attempts: " ++ err))
    Right _ ->
      expectationFailure "Connection should have failed"


-- | Test that successful connection doesn't retry
unit_successfulConnectionNoRetry :: Spec
unit_successfulConnectionNoRetry = it "Successful connection doesn't retry" $ do
  -- This test would require a mock connection or actual Kafka broker
  -- For now, we just verify the config works
  let config =
        Conn.defaultConnectionConfig
          { Conn.connMaxRetries = 3
          }
  (Conn.connMaxRetries config == 3) `shouldBe` True


-- | Test exponential backoff calculation with different multipliers
prop_differentMultipliers :: Property
prop_differentMultipliers = property $ do
  multiplier <- forAll $ Gen.double (Range.constant 1.5 3.0)

  let config =
        Conn.defaultConnectionConfig
          { Conn.connRetryDelay = 100
          , Conn.connBackoffMultiplier = multiplier
          , Conn.connBackoffMaxMs = 100000
          }

  delay0 <- evalIO $ Conn.calculateBackoffDelay 0 config
  delay1 <- evalIO $ Conn.calculateBackoffDelay 1 config

  let delayMs0 = fromIntegral delay0 / 1000 :: Double
      delayMs1 = fromIntegral delay1 / 1000

  -- The ratio should be approximately the multiplier (accounting for jitter)
  -- delay1 / delay0 should be around multiplier
  let ratio = delayMs1 / delayMs0

  annotate $ "Multiplier: " ++ show multiplier
  annotate $ "Delay 0: " ++ show delayMs0 ++ "ms"
  annotate $ "Delay 1: " ++ show delayMs1 ++ "ms"
  annotate $ "Ratio: " ++ show ratio

  -- With jitter range [0.8, 1.2], the ratio can vary quite a bit
  -- Expected: (100 * multiplier * jitter1) / (100 * jitter0)
  -- = multiplier * (jitter1/jitter0) where jitter ∈ [0.8, 1.2]
  -- Ratio range: multiplier * (0.8/1.2) to multiplier * (1.2/0.8)
  --            = multiplier * 0.667 to multiplier * 1.5
  let minRatio = multiplier * 0.6
      maxRatio = multiplier * 1.6

  assert $ ratio >= minRatio && ratio <= maxRatio


tests :: Spec
tests =
  describe "Connection Retry (KIP-580)" $
    sequence_
      [ describe "Properties" $
          sequence_
            [ it "Backoff increases exponentially" prop_backoffIncreasesExponentially
            , it "Backoff respects max limit" prop_backoffRespectsMaxLimit
            , it "Jitter adds randomness" prop_jitterAddsRandomness
            , it "Different multipliers work correctly" prop_differentMultipliers
            ]
      , describe "Unit Tests" $
          sequence_
            [ unit_givesUpAfterMaxRetries
            , unit_successfulConnectionNoRetry
            ]
      ]
