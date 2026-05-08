{-# LANGUAGE OverloadedStrings #-}

module Streams.PersistentStoreSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified System.IO.Temp as Temp

import Kafka.Streams.State.KeyValue.Persistent
  ( defaultPersistentConfig
  , persistentKeyValueStore
  )
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , StateStore (..)
  , kvIteratorToList
  , storeName
  )

tests :: TestTree
tests = testGroup "Persistent KV store"
  [ basic_persist_and_recover
  , delete_persisted
  , range_iterator
  , two_writers_then_reopen
  ]

basic_persist_and_recover :: TestTree
basic_persist_and_recover =
  testCase "values written then snapshot survive a reopen" $
    Temp.withSystemTempDirectory "kstore-test" $ \dir -> do
      let cfg = defaultPersistentConfig dir
          nm  = storeName "p"
      kvs <- persistentKeyValueStore nm cfg
      kvsPut kvs "k1" "v1"
      kvsPut kvs "k2" "v2"
      kvsPut kvs "k3" "v3"
      -- Closing snapshots and truncates the WAL.
      storeClose (kvsBase kvs)
      -- Reopen.
      kvs2 <- persistentKeyValueStore nm cfg
      kvsGet kvs2 "k1" >>= (@?= Just "v1")
      kvsGet kvs2 "k2" >>= (@?= Just "v2")
      kvsGet kvs2 "k3" >>= (@?= Just "v3")
      storeClose (kvsBase kvs2)

two_writers_then_reopen :: TestTree
two_writers_then_reopen =
  testCase "two open/close cycles preserve cumulative state" $
    Temp.withSystemTempDirectory "kstore-test" $ \dir -> do
      let cfg = defaultPersistentConfig dir
          nm  = storeName "p"
      -- 1st cycle.
      kvs <- persistentKeyValueStore nm cfg
      kvsPut kvs "a" "1"
      kvsPut kvs "b" "2"
      storeClose (kvsBase kvs)
      -- 2nd cycle: edits compound on the snapshot.
      kvs2 <- persistentKeyValueStore nm cfg
      kvsPut kvs2 "c" "3"
      _ <- kvsDelete kvs2 "a"
      kvsPut kvs2 "b" "two"
      storeClose (kvsBase kvs2)
      -- 3rd cycle: verify cumulative state.
      kvs3 <- persistentKeyValueStore nm cfg
      kvsGet kvs3 "a" >>= (@?= Nothing)
      kvsGet kvs3 "b" >>= (@?= Just "two")
      kvsGet kvs3 "c" >>= (@?= Just "3")
      storeClose (kvsBase kvs3)

delete_persisted :: TestTree
delete_persisted = testCase "deletes persist across snapshot+reopen" $
  Temp.withSystemTempDirectory "kstore-test" $ \dir -> do
    let cfg = defaultPersistentConfig dir
        nm  = storeName "p"
    kvs <- persistentKeyValueStore nm cfg
    kvsPut kvs "a" "1"
    kvsPut kvs "b" "2"
    _ <- kvsDelete kvs "a"
    storeClose (kvsBase kvs)

    kvs2 <- persistentKeyValueStore nm cfg
    kvsGet kvs2 "a" >>= (@?= Nothing)
    kvsGet kvs2 "b" >>= (@?= Just "2")
    storeClose (kvsBase kvs2)

range_iterator :: TestTree
range_iterator = testCase "range iterator returns sorted slice" $
  Temp.withSystemTempDirectory "kstore-test" $ \dir -> do
    let cfg = defaultPersistentConfig dir
        nm  = storeName "p"
    kvs <- persistentKeyValueStore nm cfg
    mapM_
      (\(k, v) -> kvsPut kvs (BSC.pack k) (BSC.pack v))
      (zip ["k01","k02","k03","k04","k05","k06"]
           ["v01","v02","v03","v04","v05","v06"])
    it <- kvsRange kvs "k02" "k04"
    xs <- kvIteratorToList it
    xs @?= [("k02","v02"), ("k03","v03"), ("k04","v04")]
    storeClose (kvsBase kvs)
