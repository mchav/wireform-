module Streams.DriverSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "DriverSpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
