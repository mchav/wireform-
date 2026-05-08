module Streams.StateStoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "StateStoreSpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
