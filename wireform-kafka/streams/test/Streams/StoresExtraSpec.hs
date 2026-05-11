{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the store-layer additions:
--   * Stores.lruMap (KIP: bounded in-memory LRU)
--   * KeyValueStore.kvsPutAll (bulk insert helper)
--   * VersionedKeyValueStore.vkvDelete (KIP-889)
module Streams.StoresExtraSpec (tests) where

import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams
import qualified Kafka.Streams.Stores as Stores
import Kafka.Streams.State.KeyValue.Versioned
  ( VersionedRecord (..)
  , defaultVersionedConfig
  , vkvDelete
  , vkvGetAsOf
  , vkvGetLatest
  , vkvPut
  )

tests :: TestTree
tests = testGroup "Stores extras"
  [ lru_evicts_oldest_when_full
  , put_all_inserts_in_one_call
  , versioned_delete_drops_at_or_after
  ]

----------------------------------------------------------------------
-- 1. LRU
----------------------------------------------------------------------

lru_evicts_oldest_when_full :: TestTree
lru_evicts_oldest_when_full =
  testCase "lruMap: oldest key is evicted when over capacity" $ do
    kvs <- Stores.lruMap @Text @Int (storeName "lru") 3
    kvsPut kvs "a" 1
    kvsPut kvs "b" 2
    kvsPut kvs "c" 3
    -- Touch "a" so "b" is now the LRU candidate.
    _ <- kvsGet kvs "a"
    kvsPut kvs "d" 4    -- evicts "b"
    kvsGet kvs "a" >>= (@?= Just 1)
    kvsGet kvs "b" >>= (@?= Nothing)
    kvsGet kvs "c" >>= (@?= Just 3)
    kvsGet kvs "d" >>= (@?= Just 4)

----------------------------------------------------------------------
-- 2. kvsPutAll
----------------------------------------------------------------------

put_all_inserts_in_one_call :: TestTree
put_all_inserts_in_one_call =
  testCase "kvsPutAll inserts a batch in one call" $ do
    kvs <- Stores.inMemoryKeyValueStore @Text @Int (storeName "batch")
    kvsPutAll kvs [("a", 1), ("b", 2), ("c", 3)]
    kvsGet kvs "a" >>= (@?= Just 1)
    kvsGet kvs "b" >>= (@?= Just 2)
    kvsGet kvs "c" >>= (@?= Just 3)

----------------------------------------------------------------------
-- 3. VersionedKeyValueStore.vkvDelete
----------------------------------------------------------------------

versioned_delete_drops_at_or_after :: TestTree
versioned_delete_drops_at_or_after =
  testCase "vkvDelete: versions >= ts disappear; older versions remain" $ do
    s <- Stores.versionedKeyValueStore @Text @Int
            (storeName "v") defaultVersionedConfig
    vkvPut s "k" 10 (Timestamp 100)
    vkvPut s "k" 20 (Timestamp 200)
    vkvPut s "k" 30 (Timestamp 300)
    -- delete at ts=250: drops the ts=300 entry. ts=100 and
    -- ts=200 remain queryable via vkvGetAsOf.
    vkvDelete s "k" (Timestamp 250)
    rl <- vkvGetLatest s "k"
    case rl of
      Just (VersionedRecord v ts) -> do
        v  @?= 20
        ts @?= Timestamp 200
      Nothing -> error "expected v=20 latest after delete"
    -- AsOf 100 still returns 10.
    r100 <- vkvGetAsOf s "k" (Timestamp 100)
    case r100 of
      Just (VersionedRecord v _) -> v @?= 10
      Nothing -> error "expected v=10 at ts=100"
    -- AsOf 300 (after delete) returns the latest pre-delete (20).
    r300 <- vkvGetAsOf s "k" (Timestamp 300)
    case r300 of
      Just (VersionedRecord v _) -> v @?= 20
      Nothing -> error "expected v=20 visible at ts=300 (delete applied)"
    -- Hush hlint about unused Text.
    _ <- pure (T.pack "ok")
    pure ()
