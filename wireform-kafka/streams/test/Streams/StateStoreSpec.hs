{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.StateStoreSpec (tests) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Hedgehog
import Hedgehog ((===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Kafka.Streams.State.KeyValue.InMemory
  ( inMemoryKeyValueStore
  )
import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  , kvIteratorToList
  , storeName
  )

tests :: Spec
tests = describe "State store (in-memory KV)" $ sequence_
  [ it "put then get" $ do
      kvs <- inMemoryKeyValueStore @Int @Int (storeName "s")
      kvsPut kvs 1 100
      v <- kvsGet kvs 1
      v `shouldBe` Just 100

  , it "get missing returns Nothing" $ do
      kvs <- inMemoryKeyValueStore @Int @Int (storeName "s")
      v <- kvsGet kvs 99
      v `shouldBe` Nothing

  , it "putIfAbsent does not overwrite" $ do
      kvs <- inMemoryKeyValueStore @Int @Int (storeName "s")
      kvsPut kvs 1 100
      r <- kvsPutIfAbsent kvs 1 200
      r `shouldBe` Just 100
      v <- kvsGet kvs 1
      v `shouldBe` Just 100

  , it "putIfAbsent inserts when absent" $ do
      kvs <- inMemoryKeyValueStore @Int @Int (storeName "s")
      r <- kvsPutIfAbsent kvs 1 200
      r `shouldBe` Nothing
      v <- kvsGet kvs 1
      v `shouldBe` Just 200

  , it "delete removes" $ do
      kvs <- inMemoryKeyValueStore @Int @Int (storeName "s")
      kvsPut kvs 1 100
      r <- kvsDelete kvs 1
      r `shouldBe` Just 100
      v <- kvsGet kvs 1
      v `shouldBe` Nothing

  , it "delete on missing key is Nothing" $ do
      kvs <- inMemoryKeyValueStore @Int @Int (storeName "s")
      r <- kvsDelete kvs 99
      r `shouldBe` Nothing

  , it "range bounds inclusive" $ do
      kvs <- inMemoryKeyValueStore @Int @Int (storeName "s")
      mapM_ (\n -> kvsPut kvs n (n * 10)) [1..10]
      it <- kvsRange kvs 3 7
      xs <- kvIteratorToList it
      xs `shouldBe` [(3,30),(4,40),(5,50),(6,60),(7,70)]

  , it "all returns sorted" $ do
      kvs <- inMemoryKeyValueStore @Int @Int (storeName "s")
      mapM_ (\n -> kvsPut kvs n (n * 10)) [5,3,7,1,9]
      it <- kvsAll kvs
      xs <- kvIteratorToList it
      xs `shouldBe` [(1,10),(3,30),(5,50),(7,70),(9,90)]

  , it "approximateNumEntries tracks size" $ do
      kvs <- inMemoryKeyValueStore @Int @Int (storeName "s")
      mapM_ (\n -> kvsPut kvs n (n * 10)) [1..5]
      n <- kvsApproxEntries kvs
      n `shouldBe` 5
      _ <- kvsDelete kvs 3
      n2 <- kvsApproxEntries kvs
      n2 `shouldBe` 4

  , it "behaves like Data.Map" $ property $ do
      ops <- forAll (Gen.list (Range.linear 0 100) genOp)
      kvs <- liftIO (inMemoryKeyValueStore @Int @Int (storeName "s"))
      runOps kvs ops Map.empty
  ]
  where
    liftIO = Hedgehog.evalIO

    runOps kvs []           expected = do
      it <- liftIO (kvsAll kvs)
      xs <- liftIO (kvIteratorToList it)
      Map.toAscList expected === xs
    runOps kvs (op : ops_) expected = do
      let expected' = applyOp op expected
      liftIO (applyOpStore kvs op)
      runOps kvs ops_ expected'

    genOp = Gen.choice
      [ OpPut <$> Gen.int (Range.linearFrom 0 (-50) 50)
              <*> Gen.int (Range.linearFrom 0 (-100) 100)
      , OpDel <$> Gen.int (Range.linearFrom 0 (-50) 50)
      ]

data Op = OpPut !Int !Int | OpDel !Int
  deriving Show

applyOp :: Op -> Map Int Int -> Map Int Int
applyOp (OpPut k v) = Map.insert k v
applyOp (OpDel k)   = Map.delete k

applyOpStore :: KeyValueStore Int Int -> Op -> IO ()
applyOpStore kvs (OpPut k v) = kvsPut kvs k v
applyOpStore kvs (OpDel k)   = do
  _ <- kvsDelete kvs k
  pure ()
