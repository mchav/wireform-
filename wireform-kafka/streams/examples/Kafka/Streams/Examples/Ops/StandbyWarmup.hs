{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.Ops.StandbyWarmup
Description : Warm a standby off the changelog and decide when it's ready

Models the KIP-441 warm-failover flow:

  1. The /active/ task writes through a 'loggedKeyValueStore'
     that mirrors every put to an in-memory changelog topic.
  2. A /standby/ task lives on a different node; we replay
     the changelog into its store via 'advanceStandby'.
  3. The runtime polls @lag = activeOffset - standbyOffset@
     periodically and uses
     'Kafka.Streams.Runtime.ProbingRebalance.classifyWarmups'
     to decide whether the standby is close enough to be
     promoted to active in the next probing rebalance.

The demo prints lag and classification at every step so you
can see the standby cross the acceptable-recovery-lag
threshold and become @WarmupReady@.
-}
module Kafka.Streams.Examples.Ops.StandbyWarmup (
  runDemo,
) where

import Control.Monad (forM_)
import Data.IORef (readIORef)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Examples.Ops.Helpers (bullet, section)
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.ProbingRebalance qualified as PR
import Kafka.Streams.Runtime.Standby
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)


runDemo :: IO ()
runDemo = do
  section "StandbyWarmupDemo"

  -- Active side: in-memory store wrapped with a changelog.
  changelog <- newInMemoryChangelogTopic
  activeUnder <- inMemoryKeyValueStore @Text @Text (storeName "session-store")
  active <-
    loggedKeyValueStore
      activeUnder
      changelog
      (storeName "session-store")
      textSerde
      textSerde

  -- Standby side: an independent store + a 'StandbyTask' that
  -- catches it up off the changelog.
  standbyStore <-
    inMemoryKeyValueStore @Text @Text
      (storeName "session-store-standby")
  sb <-
    newStandbyTask
      standbyStore
      changelog
      (storeName "session-store")
      textSerde
      textSerde

  -- KIP-441 knob: how far behind is "close enough"?
  let acceptableLag :: Int64
      acceptableLag = 2

  bullet
    ( "Active is now writing; acceptableRecoveryLag = "
        <> show acceptableLag
    )

  -- Three rounds: the active emits writes, then we advance the
  -- standby, then we check readiness.
  forM_
    [ (1 :: Int, mkBatch 1 5)
    , (2, mkBatch 6 5)
    , (3, mkBatch 11 2)
    ]
    $ \(round_, batch) -> do
      bullet ("Round " <> show round_ <> ": active writes " <> show (length batch))
      forM_ batch $ \(k, v) -> kvsPut active k v

      -- Standby pulls a batch off the changelog. The remaining
      -- lag is end-offset minus standby-offset.
      n <- advanceStandby sb
      headOff <- currentChangelogOffset changelog
      sbOff <- readIORef (sbOffset sb)
      let lag = headOff - sbOff
      bullet ("    standby replayed " <> show n <> " entries; lag = " <> show lag)

      let progress =
            PR.WarmupProgress
              { PR.task = TaskId 0 0
              , PR.lag = lag
              }
      case PR.classifyWarmups acceptableLag [progress] of
        [(_, PR.WarmupReady)] ->
          bullet "    classifier: WarmupReady -> eligible for next probing rebalance"
        [(_, PR.WarmupCatchingUp)] ->
          bullet "    classifier: WarmupCatchingUp -> keep as standby for now"
        _ -> bullet "    classifier: <unexpected shape>"
  where
    mkBatch :: Int -> Int -> [(Text, Text)]
    mkBatch start count =
      [ (T.pack ("k" <> show i), T.pack ("v" <> show i))
      | i <- [start .. start + count - 1]
      ]
