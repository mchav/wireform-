{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.QuerySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Imperative
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)

tests :: TestTree
tests = testGroup "Query API"
  [ key_query
  , range_query
  , all_query
  , count_query
  , query_value_unwraps
  ]

mkStore :: IO (KeyValueStore Int Int)
mkStore = do
  s <- inMemoryKeyValueStore @Int @Int (storeName "q")
  mapM_ (\n -> kvsPut s n (n * 10)) [1, 2, 3, 4, 5]
  pure s

key_query :: TestTree
key_query = testCase "KeyQuery returns Just for a present key, Nothing otherwise" $ do
  s <- mkStore
  r1 <- execute s (KeyQuery 3 :: Query Int Int (Maybe Int))
  isSuccess r1 @?= True
  queryValue r1 @?= Just (Just 30)
  r2 <- execute s (KeyQuery 99 :: Query Int Int (Maybe Int))
  queryValue r2 @?= Just Nothing

range_query :: TestTree
range_query = testCase "RangeQuery returns the inclusive [lo, hi] slice in order" $ do
  s <- mkStore
  r <- execute s (RangeQuery 2 4 :: Query Int Int [(Int, Int)])
  queryValue r @?= Just [(2, 20), (3, 30), (4, 40)]

all_query :: TestTree
all_query = testCase "AllQuery returns every entry in ascending order" $ do
  s <- mkStore
  r <- execute s (AllQuery :: Query Int Int [(Int, Int)])
  queryValue r @?= Just [(1, 10), (2, 20), (3, 30), (4, 40), (5, 50)]

count_query :: TestTree
count_query = testCase "CountQuery returns the entry count" $ do
  s <- mkStore
  r <- execute s (CountQuery :: Query Int Int Int)
  queryValue r @?= Just 5

query_value_unwraps :: TestTree
query_value_unwraps = testCase "QueryFailure / queryValue returns Nothing on failure" $ do
  let qf = QueryFailure "boom" :: QueryResult Int
  isSuccess qf @?= False
  queryValue qf @?= Nothing
