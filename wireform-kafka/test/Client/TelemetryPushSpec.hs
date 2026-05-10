{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-714 telemetry push state machine.
module Client.TelemetryPushSpec (tests) where

import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Telemetry.Push as P

tests :: TestTree
tests = testGroup "Telemetry push (KIP-714)"
  [ testCase "no subscription -> RefreshSubscription"
      no_sub
  , testCase "stale subscription -> Refresh"
      stale_sub
  , testCase "subscription valid + push interval elapsed -> Push"
      time_to_push
  , testCase "valid subscription, before interval -> Sleep"
      not_yet
  , testCase "terminating -> Done"
      terminating
  ]

mkSub :: P.TelemetrySubscription
mkSub = (P.noSubscription "client-1") { P.tsPushIntervalMs = 1000 }

no_sub :: IO ()
no_sub =
  P.planTelemetryStep 1000 P.initialState @?= P.TARefreshSubscription

stale_sub :: IO ()
stale_sub = do
  let st = P.initialState
        { P.tsmSubscription = Just mkSub
        , P.tsmLastSubAtMs  = 0
        }
  -- Refresh interval = 5 * pushIntervalMs = 5000
  -- now = 6000, elapsed = 6000 -> refresh.
  P.planTelemetryStep 6000 st @?= P.TARefreshSubscription

time_to_push :: IO ()
time_to_push = do
  let st = P.initialState
        { P.tsmSubscription = Just mkSub
        , P.tsmLastSubAtMs  = 1000
        , P.tsmLastPushAtMs = 1000
        }
  -- now = 2500, elapsed-since-push = 1500 >= 1000ms interval.
  P.planTelemetryStep 2500 st @?= P.TAPushNow mempty

not_yet :: IO ()
not_yet = do
  let st = P.initialState
        { P.tsmSubscription = Just mkSub
        , P.tsmLastSubAtMs  = 1000
        , P.tsmLastPushAtMs = 1000
        }
  -- now = 1500, elapsed-since-push = 500 < 1000ms interval.
  case P.planTelemetryStep 1500 st of
    P.TASleepUntilMs _ -> pure ()
    other              -> error ("expected sleep, got " <> show other)

terminating :: IO ()
terminating =
  let st = P.initialState { P.tsmTerminating = True }
  in P.planTelemetryStep 1000 st @?= P.TADone
