module Streams.PersistentStoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "PersistentStoreSpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
