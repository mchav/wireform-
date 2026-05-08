module Streams.SerdeSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "SerdeSpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
