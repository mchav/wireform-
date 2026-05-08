module Streams.TimeSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "TimeSpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
