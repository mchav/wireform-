module Streams.JoinSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "JoinSpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
