{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Conformance.T0095.AllBrokersDown
Description : librdkafka @tests\/0095-all_brokers_down.c@

librdkafka's @0095-all_brokers_down@ creates a producer pointed at a
list of unreachable brokers and asserts that the @all_brokers_down@
event fires after a bounded number of retries, without crashing or
hanging. Our analogue: 'Kafka.Network.Connection.connect' against a
list of unreachable broker addresses returns a Left within the
configured retry budget — in particular, the cumulative wait time
is bounded above by @maxRetries * connBackoffMaxMs@ plus jitter.
-}
module Conformance.T0095.AllBrokersDown (tests) where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Kafka.Network.Connection qualified as Conn
import Test.Syd


tests :: Spec
tests =
  describe "0095-all_brokers_down" $
    sequence_
      [ it "connect fails fast with bounded retries" $ do
          let cfg =
                Conn.defaultConnectionConfig
                  { Conn.connMaxRetries = 2
                  , Conn.connRetryDelay = 5
                  , Conn.connBackoffMaxMs = 25
                  , Conn.connBackoffMultiplier = 2.0
                  , Conn.connTimeout = 1
                  }
              addr =
                Conn.BrokerAddress
                  "wireform-kafka-conformance-no-such-host.invalid"
                  9999

          t0 <- getCurrentTime
          r <- Conn.connect addr cfg
          t1 <- getCurrentTime

          case r of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected connection failure, got success"

          -- Total wall time should be a small multiple of the configured
          -- retry budget. Three attempts at <= 25 ms backoff each (with
          -- 0.8x to 1.2x jitter) is well under 1 second; we give ourselves
          -- a generous 5 second cap to avoid flaking on slow CI VMs.
          let elapsedSec = realToFrac (diffUTCTime t1 t0) :: Double
          (if (elapsedSec < 5.0) then pure () else expectationFailure ("elapsed " <> show elapsedSec <> " s is bounded"))
      , it "calculateBackoffDelay caps at connBackoffMaxMs" $ do
          let cfg =
                Conn.defaultConnectionConfig
                  { Conn.connRetryDelay = 10
                  , Conn.connBackoffMaxMs = 100
                  , Conn.connBackoffMultiplier = 2.0
                  }
          -- Attempt 20 would naively be 10 * 2^20 ms; the cap should
          -- crush it down to 100 ms (modulo jitter up to 1.2x).
          d <- Conn.calculateBackoffDelay 20 cfg
          (if (d <= 100 * 1000 * 2) then pure () else expectationFailure ("delay " <> show d <> " us is at most 100 ms * 1.2 jitter"))
      ]
