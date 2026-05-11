{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the RocksDB-backed KeyValueStore. Only compiled when
-- the @+rocksdb@ Cabal flag is enabled.
module Streams.RocksDBSpec (tests) where

import qualified System.IO.Temp as Temp
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.State.KeyValue.RocksDB
  ( defaultRocksDBConfig
  , rocksDBKeyValueStore
  )
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , StateStore (..)
  , kvIteratorToList
  , storeName
  )

tests :: TestTree
tests = testGroup "RocksDB"
  [ rocksdb_put_get
  , rocksdb_persists_across_reopen
  , rocksdb_range_iter
  ]

rocksdb_put_get :: TestTree
rocksdb_put_get = testCase "RocksDB put/get round-trip" $
  Temp.withSystemTempDirectory "rdbkv-test" $ \dir -> do
    kvs <- rocksDBKeyValueStore (storeName "s") (defaultRocksDBConfig dir)
    kvsPut kvs "k1" "v1"
    kvsPut kvs "k2" "v2"
    kvsGet kvs "k1" >>= (@?= Just "v1")
    kvsGet kvs "k2" >>= (@?= Just "v2")
    kvsGet kvs "k3" >>= (@?= Nothing)
    storeClose (kvsBase kvs)

rocksdb_persists_across_reopen :: TestTree
rocksdb_persists_across_reopen =
  testCase "RocksDB data survives close+reopen" $
    Temp.withSystemTempDirectory "rdbkv-test" $ \dir -> do
      let cfg = defaultRocksDBConfig dir
          nm  = storeName "p"
      kvs1 <- rocksDBKeyValueStore nm cfg
      kvsPut kvs1 "key" "value"
      storeClose (kvsBase kvs1)
      kvs2 <- rocksDBKeyValueStore nm cfg
      kvsGet kvs2 "key" >>= (@?= Just "value")
      storeClose (kvsBase kvs2)

rocksdb_range_iter :: TestTree
rocksdb_range_iter = testCase "RocksDB range iterator" $
  Temp.withSystemTempDirectory "rdbkv-test" $ \dir -> do
    kvs <- rocksDBKeyValueStore (storeName "s") (defaultRocksDBConfig dir)
    mapM_ (\(k, v) -> kvsPut kvs k v)
      [ ("a", "1"), ("b", "2"), ("c", "3"), ("d", "4")]
    it <- kvsRange kvs "b" "c"
    xs <- kvIteratorToList it
    xs @?= [("b", "2"), ("c", "3")]
    storeClose (kvsBase kvs)
