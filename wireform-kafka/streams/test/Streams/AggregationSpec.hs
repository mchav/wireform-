module Streams.AggregationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "AggregationSpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
