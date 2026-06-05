{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the KIP-892 'TransactionalStore' overlay.
module Streams.TransactionalStoreSpec (tests) where

import qualified Data.Text as T
import Data.Text (Text)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import qualified Kafka.Streams.State.KeyValue.InMemory as Mem
import qualified Kafka.Streams.State.Store as Store
import qualified Kafka.Streams.State.Transactional as TX

tests :: Spec
tests = describe "TransactionalStore (KIP-892)" $ sequence_
  [ it "buffered put -> commit visible on underlying"
      put_commit_visible
  , it "buffered put -> abort discards"
      put_abort_invisible
  , it "read-your-writes within the open transaction"
      ryw
  , it "buffered delete on commit applies"
      delete_commit
  , it "putIfAbsent honours the underlying store"
      put_if_absent
  , it "abort is a no-op on the underlying store"
      prop_abort_noop
  ]

mkStore :: IO (Store.KeyValueStore Text Text)
mkStore = Mem.inMemoryKeyValueStore (Store.storeName "t")

put_commit_visible :: IO ()
put_commit_visible = do
  underlying <- mkStore
  ts <- TX.newTransactionalStore underlying
  let kvs = TX.txnStore ts
  Store.kvsPut kvs "k" "v"
  -- Pre-commit, underlying is untouched.
  pre <- Store.kvsGet underlying "k"
  pre `shouldBe` Nothing
  TX.txnCommit ts
  post <- Store.kvsGet underlying "k"
  post `shouldBe` Just "v"

put_abort_invisible :: IO ()
put_abort_invisible = do
  underlying <- mkStore
  ts <- TX.newTransactionalStore underlying
  let kvs = TX.txnStore ts
  Store.kvsPut kvs "k" "v"
  TX.txnAbort ts
  post <- Store.kvsGet underlying "k"
  post `shouldBe` Nothing

ryw :: IO ()
ryw = do
  underlying <- mkStore
  ts <- TX.newTransactionalStore underlying
  let kvs = TX.txnStore ts
  Store.kvsPut kvs "k" "v"
  -- Read-your-writes: the buffer wins.
  r <- Store.kvsGet kvs "k"
  r `shouldBe` Just "v"

delete_commit :: IO ()
delete_commit = do
  underlying <- mkStore
  Store.kvsPut underlying "k" "v"
  ts <- TX.newTransactionalStore underlying
  let kvs = TX.txnStore ts
  _ <- Store.kvsDelete kvs "k"
  -- Pre-commit underlying still has the value.
  pre <- Store.kvsGet underlying "k"
  pre `shouldBe` Just "v"
  TX.txnCommit ts
  post <- Store.kvsGet underlying "k"
  post `shouldBe` Nothing

put_if_absent :: IO ()
put_if_absent = do
  underlying <- mkStore
  Store.kvsPut underlying "k" "v"
  ts <- TX.newTransactionalStore underlying
  let kvs = TX.txnStore ts
  -- Should return the existing value and NOT buffer a new put.
  r <- Store.kvsPutIfAbsent kvs "k" "different"
  r `shouldBe` Just "v"
  TX.txnCommit ts
  post <- Store.kvsGet underlying "k"
  post `shouldBe` Just "v"

prop_abort_noop :: Property
prop_abort_noop = property $ do
  pairs <- forAll $ Gen.list (Range.linear 0 30) $ do
    k <- Gen.text (Range.linear 1 4) Gen.alphaNum
    v <- Gen.text (Range.linear 1 4) Gen.alphaNum
    pure (k, v)
  underlying <- evalIO mkStore
  ts <- evalIO (TX.newTransactionalStore underlying)
  let kvs = TX.txnStore ts
  evalIO $ mapM_ (\(k, v) -> Store.kvsPut kvs k v) pairs
  evalIO (TX.txnAbort ts)
  -- Underlying must still hold whatever it held at start (empty).
  remaining <- evalIO $ Store.kvsApproxEntries underlying
  remaining === 0
