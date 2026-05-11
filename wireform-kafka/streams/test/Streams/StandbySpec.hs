{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.StandbySpec (tests) where

import qualified Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Runtime.Standby
import Kafka.Streams.Serde (textSerde)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , storeName
  )

tests :: TestTree
tests = testGroup "Standby"
  [ standby_replays_basic_writes
  , standby_replays_tombstones
  , standby_advance_returns_count_of_applied
  , standby_only_applies_its_store
  , logged_store_passes_reads_through
  , restore_listener_fires_start_batch_end
  ]

standby_replays_basic_writes :: TestTree
standby_replays_basic_writes =
  testCase "active put -> changelog -> standby store" $ do
    topic <- newInMemoryChangelogTopic
    activeUnder <- inMemoryKeyValueStore @Text @Text (storeName "s")
    active <- loggedKeyValueStore activeUnder topic (storeName "s") textSerde textSerde

    standbyStore <- inMemoryKeyValueStore @Text @Text (storeName "s-standby")
    sb <- newStandbyTask standbyStore topic (storeName "s") textSerde textSerde

    kvsPut active "k1" "v1"
    kvsPut active "k2" "v2"
    kvsPut active "k1" "v1updated"

    n <- advanceStandby sb
    n @?= 3

    kvsGet (sbStore sb) "k1" >>= (@?= Just "v1updated")
    kvsGet (sbStore sb) "k2" >>= (@?= Just "v2")

standby_replays_tombstones :: TestTree
standby_replays_tombstones =
  testCase "active delete -> tombstone -> standby removes" $ do
    topic <- newInMemoryChangelogTopic
    activeUnder <- inMemoryKeyValueStore @Text @Text (storeName "s")
    active <- loggedKeyValueStore activeUnder topic (storeName "s") textSerde textSerde
    standbyStore <- inMemoryKeyValueStore @Text @Text (storeName "s-standby")
    sb <- newStandbyTask standbyStore topic (storeName "s") textSerde textSerde

    kvsPut active "k" "v"
    _ <- kvsDelete active "k"
    _ <- advanceStandby sb
    kvsGet (sbStore sb) "k" >>= (@?= Nothing)

standby_advance_returns_count_of_applied :: TestTree
standby_advance_returns_count_of_applied =
  testCase "advanceStandby returns the count of entries it applied" $ do
    topic <- newInMemoryChangelogTopic
    activeUnder <- inMemoryKeyValueStore @Text @Text (storeName "s")
    active <- loggedKeyValueStore activeUnder topic (storeName "s") textSerde textSerde
    standbyStore <- inMemoryKeyValueStore @Text @Text (storeName "s-standby")
    sb <- newStandbyTask standbyStore topic (storeName "s") textSerde textSerde

    kvsPut active "a" "1"
    n1 <- advanceStandby sb
    n1 @?= 1

    kvsPut active "b" "2"
    kvsPut active "c" "3"
    n2 <- advanceStandby sb
    n2 @?= 2

    -- Another advance with no new entries returns 0.
    n3 <- advanceStandby sb
    n3 @?= 0

standby_only_applies_its_store :: TestTree
standby_only_applies_its_store =
  testCase "standby ignores entries from other stores" $ do
    topic <- newInMemoryChangelogTopic
    -- Two active stores, one standby for the FIRST store only.
    a1Under <- inMemoryKeyValueStore @Text @Text (storeName "s1")
    a1 <- loggedKeyValueStore a1Under topic (storeName "s1") textSerde textSerde
    a2Under <- inMemoryKeyValueStore @Text @Text (storeName "s2")
    a2 <- loggedKeyValueStore a2Under topic (storeName "s2") textSerde textSerde

    sbStoreS1 <- inMemoryKeyValueStore @Text @Text (storeName "s1-sb")
    sb <- newStandbyTask sbStoreS1 topic (storeName "s1") textSerde textSerde

    kvsPut a1 "x" "from-s1"
    kvsPut a2 "x" "from-s2"
    n <- advanceStandby sb
    -- Only the s1 entry was applied, but advanceStandby's count is
    -- "applied to my store", not "in the topic". So n == 1.
    n @?= 1
    kvsGet (sbStore sb) "x" >>= (@?= Just "from-s1")

logged_store_passes_reads_through :: TestTree
logged_store_passes_reads_through =
  testCase "loggedKeyValueStore preserves get / approxEntries semantics" $ do
    topic <- newInMemoryChangelogTopic
    under <- inMemoryKeyValueStore @Text @Text (storeName "s")
    logged <- loggedKeyValueStore under topic (storeName "s") textSerde textSerde

    kvsPut logged "k" "v"
    kvsGet logged "k" >>= (@?= Just "v")
    kvsApproxEntries logged >>= (@?= 1)

    -- And the topic recorded the put.
    es <- readEntriesFrom topic 0
    length es @?= 1

restore_listener_fires_start_batch_end :: TestTree
restore_listener_fires_start_batch_end =
  testCase "RestoreListener gets onRestoreStart / Batch / End on advance" $ do
    topic <- newInMemoryChangelogTopic
    activeUnder <- inMemoryKeyValueStore @Text @Text (storeName "s")
    active <- loggedKeyValueStore activeUnder topic (storeName "s") textSerde textSerde
    standbyStore <- inMemoryKeyValueStore @Text @Text (storeName "s-sb")
    sb <- newStandbyTask standbyStore topic (storeName "s") textSerde textSerde

    starts  <- Data.IORef.newIORef ([] :: [Int])
    batches <- Data.IORef.newIORef ([] :: [Int])
    ends    <- Data.IORef.newIORef ([] :: [Int])
    setRestoreListener sb RestoreListener
      { onRestoreStart = \_ _ _ -> Data.IORef.modifyIORef' starts (1 :)
      , onBatchRestored = \_ _ n ->
          Data.IORef.modifyIORef' batches (n :)
      , onRestoreEnd = \_ n ->
          Data.IORef.modifyIORef' ends (fromIntegral n :)
      }

    kvsPut active "k1" "v1"
    kvsPut active "k2" "v2"
    _ <- advanceStandby sb
    Data.IORef.readIORef starts  >>= ((@?= 1) . length)
    Data.IORef.readIORef batches >>= (@?= [2])
    Data.IORef.readIORef ends    >>= (@?= [2])
