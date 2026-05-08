module Streams.WindowSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "WindowSpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
