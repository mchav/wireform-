module Streams.PunctuatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "PunctuatorSpec"
  [ testCase "stub" ((1 :: Int) @?= 1) ]
