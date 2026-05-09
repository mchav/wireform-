{-# LANGUAGE OverloadedStrings #-}

module Client.ConnectionExtrasSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Client.ConnectionExtras as CX

tests :: TestTree
tests = testGroup "ConnectionExtras"
  [ testCase "shouldDelayConnect = Nothing when no throttle recorded"
      no_throttle
  , testCase "shouldDelayConnect returns remaining wait after recordThrottle"
      throttle_set
  , testCase "isIdle returns True for fresh + unrecorded keys"
      idle_unrecorded
  , testCase "recordActivity prevents idle until threshold passes"
      activity_then_idle
  , testCase "prioritise sorts critical first, low last"
      prioritise_order
  , testCase "defaultSaslTimeouts: 30s connect, 0 idle"
      sasl_defaults
  ]

no_throttle :: IO ()
no_throttle = do
  st <- CX.newConnectionQuotaState
  d <- CX.shouldDelayConnect st 1000
  d @?= Nothing

throttle_set :: IO ()
throttle_set = do
  st <- CX.newConnectionQuotaState
  CX.recordThrottle st 1000 5000
  d <- CX.shouldDelayConnect st 1500
  d @?= Just 4500

idle_unrecorded :: IO ()
idle_unrecorded = do
  t :: CX.IdleConnTracker String <- CX.newIdleConnTracker
  r <- CX.isIdle t "k" 1000 60_000
  r @?= True

activity_then_idle :: IO ()
activity_then_idle = do
  t :: CX.IdleConnTracker String <- CX.newIdleConnTracker
  CX.recordActivity t "k" 1000
  -- 30 s after activity: not idle (< 60 s threshold).
  notIdle <- CX.isIdle t "k" 31_000 60_000
  notIdle @?= False
  -- 70 s after activity: idle.
  idle    <- CX.isIdle t "k" 71_000 60_000
  idle    @?= True

prioritise_order :: IO ()
prioritise_order =
  map fst (CX.prioritise
    [ (CX.QosLow,      "a")
    , (CX.QosCritical, "b")
    , (CX.QosNormal,   "c")
    , (CX.QosHigh,     "d")
    ])
  @?= [CX.QosCritical, CX.QosHigh, CX.QosNormal, CX.QosLow]

sasl_defaults :: IO ()
sasl_defaults = do
  CX.saslConnectTimeoutMs CX.defaultSaslTimeouts @?= 30_000
  CX.saslMaxIdleMs        CX.defaultSaslTimeouts @?= 0
