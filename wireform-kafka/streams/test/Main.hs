module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "kafka-streams"
  [ testCase "sanity" $ (1 :: Int) @?= 1
  ]


