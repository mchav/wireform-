module Streams.TopologySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "TopologySpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
