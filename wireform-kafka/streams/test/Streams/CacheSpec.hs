{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.CacheSpec (tests) where

import Data.IORef
import Test.Syd

import Kafka.Streams.State.KeyValue.Caching
  ( CachingConfig (..)
  , cachingKeyValueStore
  , defaultCachingConfig
  )
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , StateStore (..)
  , kvIteratorToList
  , storeName
  )

tests :: Spec
tests = describe "Cache" $ sequence_
  [ cache_dedups_per_flush
  , cache_read_your_writes
  , cache_eviction_on_budget
  , cache_tombstone_emits_nothing
  , cache_close_flushes
  , cache_range_merges
  ]

mkCachedStore
  :: IO ( IORef [(String, Maybe Int)]
        , KeyValueStore String Int
        , KeyValueStore String Int       -- underlying
        )
mkCachedStore = do
  emitted <- newIORef []
  under <- inMemoryKeyValueStore @String @Int (storeName "u")
  cached <- cachingKeyValueStore under (CachingConfig 100)
              (\k mv -> modifyIORef' emitted ((k, mv) :))
  pure (emitted, cached, under)

cache_dedups_per_flush :: Spec
cache_dedups_per_flush =
  it "5 puts on the same key result in 1 emit on flush" $ do
    (emitted, cached, _) <- mkCachedStore
    mapM_ (\v -> kvsPut cached "k" v) [1, 2, 3, 4, 5]
    -- Nothing should have flushed yet (budget = 100, only 1 dirty entry).
    emitsBeforeFlush <- readIORef emitted
    emitsBeforeFlush `shouldBe` []
    storeFlush (kvsBase cached)
    emits <- readIORef emitted
    emits `shouldBe` [("k", Just 5)]

cache_read_your_writes :: Spec
cache_read_your_writes =
  it "kvsGet sees the latest buffered write" $ do
    (_, cached, _) <- mkCachedStore
    kvsPut cached "k" 1
    kvsGet cached "k" >>= (`shouldBe` Just 1)
    kvsPut cached "k" 99
    kvsGet cached "k" >>= (`shouldBe` Just 99)

cache_eviction_on_budget :: Spec
cache_eviction_on_budget =
  it "filling the cache past budget evicts" $ do
    emitted <- newIORef []
    under <- inMemoryKeyValueStore @Int @Int (storeName "u")
    cached <- cachingKeyValueStore under (CachingConfig 3)
                (\k mv -> modifyIORef' emitted ((k, mv) :))

    -- 4th distinct key should trigger eviction of all 4.
    kvsPut cached 1 10
    kvsPut cached 2 20
    kvsPut cached 3 30
    kvsPut cached 4 40                -- triggers eviction (size > 3)

    emits <- readIORef emitted
    -- Eviction flushes the whole buffered map.
    -- Order: ascending key (Map.toAscList in flushAll, then prepended
    -- in the IORef so the captured list is reversed).
    reverse emits `shouldBe` [(1, Just 10), (2, Just 20), (3, Just 30), (4, Just 40)]
    -- Underlying store now has all four.
    mapM_ (\k -> kvsGet under k >>= (`shouldBe` Just (k * 10))) [1..4]

cache_tombstone_emits_nothing :: Spec
cache_tombstone_emits_nothing =
  it "delete buffers a tombstone that emits Nothing" $ do
    (emitted, cached, _) <- mkCachedStore
    kvsPut cached "k" 1
    _ <- kvsDelete cached "k"
    storeFlush (kvsBase cached)
    emits <- readIORef emitted
    emits `shouldBe` [("k", Nothing)]
    -- And subsequent get is Nothing.
    kvsGet cached "k" >>= (`shouldBe` Nothing)

cache_close_flushes :: Spec
cache_close_flushes =
  it "storeClose flushes all pending writes" $ do
    (emitted, cached, _) <- mkCachedStore
    kvsPut cached "a" 1
    kvsPut cached "b" 2
    storeClose (kvsBase cached)
    emits <- readIORef emitted
    -- Order: emit on close is in ascending key order; we prepend to
    -- the IORef, so the visible list is reversed.
    reverse emits `shouldBe` [("a", Just 1), ("b", Just 2)]

cache_range_merges :: Spec
cache_range_merges =
  it "kvsRange merges cache + underlying, dropping tombstoned keys" $ do
    (_, cached, under) <- mkCachedStore
    -- Pre-populate the underlying with c=30, d=40, e=50.
    kvsPut under "c" 30
    kvsPut under "d" 40
    kvsPut under "e" 50
    -- Buffer cache: a=1 (new), c=999 (override), e=tombstone.
    kvsPut cached "a" 1
    kvsPut cached "c" 999
    _ <- kvsDelete cached "e"

    it <- kvsRange cached "a" "z"
    xs <- kvIteratorToList it
    xs `shouldBe` [("a", 1), ("c", 999), ("d", 40)]
