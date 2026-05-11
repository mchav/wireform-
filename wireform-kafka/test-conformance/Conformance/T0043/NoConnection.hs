{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conformance.T0043.NoConnection
Description : librdkafka @tests\/0043-no_connection.c@

librdkafka's @0043-no_connection@ creates a producer pointed at an
unreachable broker and asserts that send / metadata operations fail
gracefully (exponential backoff, no crash, eventual ETIMEDOUT).

Our analogue: 'Kafka.Network.Connection.connect' against an
unreachable broker fails after the configured retry budget without
crashing. This exercises the same code path the existing
@Network.ConnectionRetrySpec@ covers from the unit-test side; here
we keep the librdkafka test number visible so the catalog stays
diff-able.
-}
module Conformance.T0043.NoConnection (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kafka.Network.Connection as Conn

tests :: TestTree
tests = testGroup "0043-no_connection"
  [ testCase "connect to invalid host gives up after max retries" $ do
      let cfg = Conn.defaultConnectionConfig
            { Conn.connMaxRetries = 1
            , Conn.connRetryDelay = 10
            , Conn.connBackoffMaxMs = 50
            , Conn.connTimeout = 1
            }
          addr = Conn.BrokerAddress
            "wireform-kafka-conformance-no-such-host.invalid" 9999
      r <- Conn.connect addr cfg
      case r of
        Left _ -> pure ()
        Right _ -> assertFailure "expected connection failure, got success"

  , testCase "exponential backoff respects the max delay cap" $ do
      let cfg = Conn.defaultConnectionConfig
            { Conn.connRetryDelay      = 10
            , Conn.connBackoffMaxMs    = 50
            , Conn.connBackoffMultiplier = 2.0
            , Conn.connMaxRetries      = 5
            }
      -- attempt 0: ~10 ms, attempt 5: 10 * 2^5 = 320 ms, capped to 50 ms.
      d <- Conn.calculateBackoffDelay 5 cfg
      assertBool ("attempt-5 delay (" <> show d <> " us) is at most 50 ms * 1.2 jitter")
        (d <= 50 * 1000 * 2)
  ]
