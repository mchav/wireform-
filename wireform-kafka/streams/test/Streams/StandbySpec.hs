{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.StandbySpec (tests) where

import Data.IORef qualified
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Runtime.Standby
import Kafka.Streams.Serde (textSerde)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  storeName,
 )
import Test.Syd


tests :: Spec
tests =
  describe "Standby" $
    sequence_
      [ standby_replays_basic_writes
      , standby_replays_tombstones
      , standby_advance_returns_count_of_applied
      , standby_only_applies_its_store
      , logged_store_passes_reads_through
      , restore_listener_fires_start_batch_end
      ]


standby_replays_basic_writes :: Spec
standby_replays_basic_writes =
  it "active put -> changelog -> standby store" $ do
    topic <- newInMemoryChangelogTopic
    activeUnder <- inMemoryKeyValueStore @Text @Text (storeName "s")
    active <- loggedKeyValueStore activeUnder topic (storeName "s") textSerde textSerde

    standbyStore <- inMemoryKeyValueStore @Text @Text (storeName "s-standby")
    sb <- newStandbyTask standbyStore topic (storeName "s") textSerde textSerde

    kvsPut active "k1" "v1"
    kvsPut active "k2" "v2"
    kvsPut active "k1" "v1updated"

    n <- advanceStandby sb
    n `shouldBe` 3

    kvsGet (sbStore sb) "k1" >>= (`shouldBe` Just "v1updated")
    kvsGet (sbStore sb) "k2" >>= (`shouldBe` Just "v2")


standby_replays_tombstones :: Spec
standby_replays_tombstones =
  it "active delete -> tombstone -> standby removes" $ do
    topic <- newInMemoryChangelogTopic
    activeUnder <- inMemoryKeyValueStore @Text @Text (storeName "s")
    active <- loggedKeyValueStore activeUnder topic (storeName "s") textSerde textSerde
    standbyStore <- inMemoryKeyValueStore @Text @Text (storeName "s-standby")
    sb <- newStandbyTask standbyStore topic (storeName "s") textSerde textSerde

    kvsPut active "k" "v"
    _ <- kvsDelete active "k"
    _ <- advanceStandby sb
    kvsGet (sbStore sb) "k" >>= (`shouldBe` Nothing)


standby_advance_returns_count_of_applied :: Spec
standby_advance_returns_count_of_applied =
  it "advanceStandby returns the count of entries it applied" $ do
    topic <- newInMemoryChangelogTopic
    activeUnder <- inMemoryKeyValueStore @Text @Text (storeName "s")
    active <- loggedKeyValueStore activeUnder topic (storeName "s") textSerde textSerde
    standbyStore <- inMemoryKeyValueStore @Text @Text (storeName "s-standby")
    sb <- newStandbyTask standbyStore topic (storeName "s") textSerde textSerde

    kvsPut active "a" "1"
    n1 <- advanceStandby sb
    n1 `shouldBe` 1

    kvsPut active "b" "2"
    kvsPut active "c" "3"
    n2 <- advanceStandby sb
    n2 `shouldBe` 2

    -- Another advance with no new entries returns 0.
    n3 <- advanceStandby sb
    n3 `shouldBe` 0


standby_only_applies_its_store :: Spec
standby_only_applies_its_store =
  it "standby ignores entries from other stores" $ do
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
    n `shouldBe` 1
    kvsGet (sbStore sb) "x" >>= (`shouldBe` Just "from-s1")


logged_store_passes_reads_through :: Spec
logged_store_passes_reads_through =
  it "loggedKeyValueStore preserves get / approxEntries semantics" $ do
    topic <- newInMemoryChangelogTopic
    under <- inMemoryKeyValueStore @Text @Text (storeName "s")
    logged <- loggedKeyValueStore under topic (storeName "s") textSerde textSerde

    kvsPut logged "k" "v"
    kvsGet logged "k" >>= (`shouldBe` Just "v")
    kvsApproxEntries logged >>= (`shouldBe` 1)

    -- And the topic recorded the put.
    es <- readEntriesFrom topic 0
    length es `shouldBe` 1


restore_listener_fires_start_batch_end :: Spec
restore_listener_fires_start_batch_end =
  it "RestoreListener gets onRestoreStart / Batch / End on advance" $ do
    topic <- newInMemoryChangelogTopic
    activeUnder <- inMemoryKeyValueStore @Text @Text (storeName "s")
    active <- loggedKeyValueStore activeUnder topic (storeName "s") textSerde textSerde
    standbyStore <- inMemoryKeyValueStore @Text @Text (storeName "s-sb")
    sb <- newStandbyTask standbyStore topic (storeName "s") textSerde textSerde

    starts <- Data.IORef.newIORef ([] :: [Int])
    batches <- Data.IORef.newIORef ([] :: [Int])
    ends <- Data.IORef.newIORef ([] :: [Int])
    setRestoreListener
      sb
      RestoreListener
        { onRestoreStart = \_ _ _ -> Data.IORef.modifyIORef' starts (1 :)
        , onBatchRestored = \_ _ n ->
            Data.IORef.modifyIORef' batches (n :)
        , onRestoreEnd = \_ n ->
            Data.IORef.modifyIORef' ends (fromIntegral n :)
        }

    kvsPut active "k1" "v1"
    kvsPut active "k2" "v2"
    _ <- advanceStandby sb
    Data.IORef.readIORef starts >>= ((`shouldBe` 1) . length)
    Data.IORef.readIORef batches >>= (`shouldBe` [2])
    Data.IORef.readIORef ends >>= (`shouldBe` [2])
