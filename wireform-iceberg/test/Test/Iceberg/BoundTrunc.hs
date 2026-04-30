{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.BoundTrunc (tests) where

import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.BoundTrunc

tests :: TestTree
tests = testGroup "Iceberg.BoundTrunc"
  [ testCase "truncateLowerString takes the first N characters" $
      truncateLowerString 3 "iceberg" @?= "ice"

  , testCase "truncateUpperString bumps the last character" $
      truncateUpperString 3 "iceberg" @?= Just "icf"

  , testCase "truncateUpperString returns Nothing when prefix is all max" $
      truncateUpperString 2 "\1114111\1114111x" @?= Nothing

  , testCase "truncateUpperString preserves short strings" $
      truncateUpperString 5 "ice" @?= Just "ice"

  , testCase "truncateLowerBytes takes the first N bytes" $
      truncateLowerBytes 3 (BS.pack [1, 2, 3, 4, 5]) @?= BS.pack [1, 2, 3]

  , testCase "truncateUpperBytes carries through 0xFF" $
      truncateUpperBytes 3 (BS.pack [1, 0xFF, 0xFF, 0]) @?= Just (BS.pack [2, 0, 0])

  , testCase "truncateUpperBytes returns Nothing when prefix is all 0xFF" $
      truncateUpperBytes 2 (BS.pack [0xFF, 0xFF, 0]) @?= Nothing
  ]
