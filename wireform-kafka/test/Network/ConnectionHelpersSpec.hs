{-# LANGUAGE OverloadedStrings #-}

module Network.ConnectionHelpersSpec (tests) where

import Test.Syd

import qualified Kafka.Network.Connection as CX

tests :: Spec
tests = describe "Connection helpers" $ sequence_
  [ it "shouldDelayConnect = Nothing when no throttle recorded"
      no_throttle
  , it "shouldDelayConnect returns remaining wait after recordThrottle"
      throttle_set
  , it "isIdle returns True for fresh + unrecorded keys"
      idle_unrecorded
  , it "recordActivity prevents idle until threshold passes"
      activity_then_idle
  , it "prioritise sorts critical first, low last"
      prioritise_order
  , it "defaultSaslTimeouts: 30s connect, 0 idle"
      sasl_defaults
  ]

no_throttle :: IO ()
no_throttle = do
  st <- CX.newConnectionQuotaState
  d <- CX.shouldDelayConnect st 1000
  d `shouldBe` Nothing

throttle_set :: IO ()
throttle_set = do
  st <- CX.newConnectionQuotaState
  CX.recordThrottle st 1000 5000
  d <- CX.shouldDelayConnect st 1500
  d `shouldBe` Just 4500

idle_unrecorded :: IO ()
idle_unrecorded = do
  t :: CX.IdleConnTracker String <- CX.newIdleConnTracker
  r <- CX.isIdle t "k" 1000 60_000
  r `shouldBe` True

activity_then_idle :: IO ()
activity_then_idle = do
  t :: CX.IdleConnTracker String <- CX.newIdleConnTracker
  CX.recordActivity t "k" 1000
  -- 30 s after activity: not idle (< 60 s threshold).
  notIdle <- CX.isIdle t "k" 31_000 60_000
  notIdle `shouldBe` False
  -- 70 s after activity: idle.
  idle    <- CX.isIdle t "k" 71_000 60_000
  idle    `shouldBe` True

prioritise_order :: IO ()
prioritise_order =
  map fst (CX.prioritise
    [ (CX.QosLow,      "a")
    , (CX.QosCritical, "b")
    , (CX.QosNormal,   "c")
    , (CX.QosHigh,     "d")
    ])
  `shouldBe` [CX.QosCritical, CX.QosHigh, CX.QosNormal, CX.QosLow]

sasl_defaults :: IO ()
sasl_defaults = do
  CX.saslConnectTimeoutMs CX.defaultSaslTimeouts `shouldBe` 30_000
  CX.saslMaxIdleMs        CX.defaultSaslTimeouts `shouldBe` 0
