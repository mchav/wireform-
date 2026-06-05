{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | End-to-end test for the changelog poll-loop that drives
-- standby-task replay.
--
-- The driver's poll function is parameterised so we can feed
-- it deterministic batches; the test:
--
--   1. registers two StandbyTasks in a 'StandbyManager';
--   2. parks a 'StandbyDriver' against a stub poll fn that
--      hands out two batches across two ticks;
--   3. drives 'standbyDriverTick' twice;
--   4. asserts the local stores absorbed every record and
--      the lag report reached the warmup-lag map via the
--      runtime hookup.
module Streams.StandbyDriverSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Test.Syd

import Kafka.Streams.Imperative
import Kafka.Streams.Processor (TaskId (..))
import qualified Kafka.Streams.State.KeyValue.InMemory as KVInMem
import Kafka.Streams.Runtime.StandbyTask
import Kafka.Streams.Runtime.StandbyDriver

tests :: Spec
tests = describe "Standby driver (changelog poll-loop)" $ sequence_
  [ tick_dispatches_to_right_task
  , unknown_partition_is_dropped
  , lag_reports_make_it_through
  ]

----------------------------------------------------------------------
-- 1. Two tasks, one tick, each gets its own records
----------------------------------------------------------------------

tick_dispatches_to_right_task :: Spec
tick_dispatches_to_right_task =
  it "standbyDriverTick dispatches each batch to the matching task" $ do
    mgr <- newStandbyManager
    a <- newStandbyTask (TaskId 0 0) "cl" 0 (storeName "store-a")
    b <- newStandbyTask (TaskId 0 1) "cl" 1 (storeName "store-b")
    addStandbyTask mgr a
    addStandbyTask mgr b

    storeA <- KVInMem.inMemoryKeyValueStore @BSC.ByteString @(Maybe BSC.ByteString)
                (storeName "store-a")
    storeB <- KVInMem.inMemoryKeyValueStore @BSC.ByteString @(Maybe BSC.ByteString)
                (storeName "store-b")
    let lookupFn nm
          | nm == storeName "store-a" = pure (Just storeA)
          | nm == storeName "store-b" = pure (Just storeB)
          | otherwise                 = pure Nothing

    let pollFn _ms = pure
          [ (("cl", 0), [ChangelogRecord 0 "k0" (Just "v0") 1])
          , (("cl", 1), [ChangelogRecord 0 "k1" (Just "v1") 1])
          ]

    drv <- newStandbyDriver mgr pollFn lookupFn (\_ _ -> pure ()) 0
    standbyDriverTick drv

    kvsGet storeA "k0" >>= (`shouldBe` Just (Just "v0"))
    kvsGet storeB "k1" >>= (`shouldBe` Just (Just "v1"))

----------------------------------------------------------------------
-- 2. Records for an unknown (topic, partition) are silently dropped
----------------------------------------------------------------------

unknown_partition_is_dropped :: Spec
unknown_partition_is_dropped =
  it "standbyDriverTick: records for an unregistered partition don't crash" $ do
    mgr <- newStandbyManager
    t <- newStandbyTask (TaskId 0 0) "cl" 0 (storeName "store-a")
    addStandbyTask mgr t

    storeA <- KVInMem.inMemoryKeyValueStore @BSC.ByteString @(Maybe BSC.ByteString)
                (storeName "store-a")
    let lookupFn nm
          | nm == storeName "store-a" = pure (Just storeA)
          | otherwise                 = pure Nothing

    -- Partition 5 isn't a standby; the batch goes nowhere.
    let pollFn _ = pure
          [ (("cl", 5), [ChangelogRecord 0 "ignore" (Just "x") 1])
          , (("cl", 0), [ChangelogRecord 0 "k"      (Just "v") 1])
          ]
    drv <- newStandbyDriver mgr pollFn lookupFn (\_ _ -> pure ()) 0
    standbyDriverTick drv

    kvsGet storeA "k"      >>= (`shouldBe` Just (Just "v"))
    kvsGet storeA "ignore" >>= (`shouldBe` Nothing)

----------------------------------------------------------------------
-- 3. The driver fires the warmup-lag reporter end-to-end
----------------------------------------------------------------------

lag_reports_make_it_through :: Spec
lag_reports_make_it_through =
  it "standbyDriverTick: lag reports flow into the supplied callback" $ do
    mgr <- newStandbyManager
    t <- newStandbyTask (TaskId 0 0) "cl" 0 (storeName "store-a")
    addStandbyTask mgr t

    storeA <- KVInMem.inMemoryKeyValueStore @BSC.ByteString @(Maybe BSC.ByteString)
                (storeName "store-a")
    let lookupFn _ = pure (Just storeA)

    -- 3 records consumed, end-of-log = 10 -> lag = 7.
    let batch =
          [ ChangelogRecord 0 "a" (Just "1") 10
          , ChangelogRecord 1 "b" (Just "2") 10
          , ChangelogRecord 2 "c" (Just "3") 10
          ]
    let pollFn _ = pure [(("cl", 0), batch)]

    seenRef <- newIORef (Map.empty :: Map TaskId Int64)
    let reportFn tid lag =
          modifyIORef' seenRef (Map.insert tid lag)

    drv <- newStandbyDriver mgr pollFn lookupFn reportFn 0
    standbyDriverTick drv

    seen <- readIORef seenRef
    Map.lookup (TaskId 0 0) seen `shouldBe` Just 7
