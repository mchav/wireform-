{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the RocksDB-backed KeyValueStore. Only compiled when
the @+rocksdb@ Cabal flag is enabled.
-}
module Streams.RocksDBSpec (tests) where

import Kafka.Streams.State.KeyValue.RocksDB (
  defaultRocksDBConfig,
  rocksDBKeyValueStore,
 )
import Kafka.Streams.State.Store (
  KeyValueStore (..),
  StateStore (..),
  kvIteratorToList,
  storeName,
 )
import System.IO.Temp qualified as Temp
import Test.Syd


tests :: Spec
tests =
  describe "RocksDB" $
    sequence_
      [ rocksdb_put_get
      , rocksdb_persists_across_reopen
      , rocksdb_range_iter
      ]


rocksdb_put_get :: Spec
rocksdb_put_get = it "RocksDB put/get round-trip" $
  Temp.withSystemTempDirectory "rdbkv-test" $ \dir -> do
    kvs <- rocksDBKeyValueStore (storeName "s") (defaultRocksDBConfig dir)
    kvsPut kvs "k1" "v1"
    kvsPut kvs "k2" "v2"
    kvsGet kvs "k1" >>= (`shouldBe` Just "v1")
    kvsGet kvs "k2" >>= (`shouldBe` Just "v2")
    kvsGet kvs "k3" >>= (`shouldBe` Nothing)
    storeClose (kvsBase kvs)


rocksdb_persists_across_reopen :: Spec
rocksdb_persists_across_reopen =
  it "RocksDB data survives close+reopen" $
    Temp.withSystemTempDirectory "rdbkv-test" $ \dir -> do
      let cfg = defaultRocksDBConfig dir
          nm = storeName "p"
      kvs1 <- rocksDBKeyValueStore nm cfg
      kvsPut kvs1 "key" "value"
      storeClose (kvsBase kvs1)
      kvs2 <- rocksDBKeyValueStore nm cfg
      kvsGet kvs2 "key" >>= (`shouldBe` Just "value")
      storeClose (kvsBase kvs2)


rocksdb_range_iter :: Spec
rocksdb_range_iter = it "RocksDB range iterator" $
  Temp.withSystemTempDirectory "rdbkv-test" $ \dir -> do
    kvs <- rocksDBKeyValueStore (storeName "s") (defaultRocksDBConfig dir)
    mapM_
      (\(k, v) -> kvsPut kvs k v)
      [("a", "1"), ("b", "2"), ("c", "3"), ("d", "4")]
    it <- kvsRange kvs "b" "c"
    xs <- kvIteratorToList it
    xs `shouldBe` [("b", "2"), ("c", "3")]
    storeClose (kvsBase kvs)
