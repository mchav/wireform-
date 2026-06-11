{-# LANGUAGE OverloadedStrings #-}

module Streams.ProbingRebalanceSpec (tests) where

import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.ProbingRebalance qualified as PR
import Test.Syd


tests :: Spec
tests =
  describe "Probing rebalance (KIP-441)" $
    sequence_
      [ it
          "classifyWarmups respects acceptable.recovery.lag"
          classify_threshold
      , it
          "readyWarmups filters by threshold"
          ready_filtered
      , it
          "shouldProbe = False when no warmups are ready"
          probe_no_ready
      , it
          "shouldProbe = False before the cadence elapses"
          probe_before_interval
      , it
          "shouldProbe = True after the cadence with a ready warmup"
          probe_after_interval
      , it
          "shouldProbe = False when interval is 0 (disabled)"
          probe_disabled
      ]


w :: Int -> Int -> PR.WarmupProgress
w sub p = PR.WarmupProgress (TaskId sub (fromIntegral p)) 0


ww :: Int -> Int -> Int -> PR.WarmupProgress
ww sub p lag = PR.WarmupProgress (TaskId sub (fromIntegral p)) (fromIntegral lag)


classify_threshold :: IO ()
classify_threshold =
  PR.classifyWarmups
    100
    [ ww 0 0 50
    , ww 0 1 100
    , ww 0 2 150
    ]
    `shouldBe` [ (ww 0 0 50, PR.WarmupReady)
               , (ww 0 1 100, PR.WarmupReady)
               , (ww 0 2 150, PR.WarmupCatchingUp)
               ]


ready_filtered :: IO ()
ready_filtered =
  PR.readyWarmups 10 [ww 0 0 5, ww 0 1 20, ww 0 2 0]
    `shouldBe` [ww 0 0 5, ww 0 2 0]


probe_no_ready :: IO ()
probe_no_ready =
  PR.shouldProbe 5_000_000 0 1_000 [ww 0 0 1_000_000] 100 `shouldBe` False


probe_before_interval :: IO ()
probe_before_interval =
  -- now=2000, lastProbe=1500, interval=1000 -> only 500ms elapsed.
  PR.shouldProbe 2000 1500 1000 [w 0 0] 100 `shouldBe` False


probe_after_interval :: IO ()
probe_after_interval =
  -- now=3000, lastProbe=1500, interval=1000 -> 1500ms elapsed.
  PR.shouldProbe 3000 1500 1000 [w 0 0] 100 `shouldBe` True


probe_disabled :: IO ()
probe_disabled =
  PR.shouldProbe 1_000_000 0 0 [w 0 0] 100 `shouldBe` False
