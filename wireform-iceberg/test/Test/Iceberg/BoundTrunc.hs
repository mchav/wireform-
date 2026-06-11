{-# LANGUAGE OverloadedStrings #-}

module Test.Iceberg.BoundTrunc (tests) where

import Data.ByteString qualified as BS
import Iceberg.BoundTrunc
import Test.Syd


tests :: Spec
tests =
  describe "Iceberg.BoundTrunc" $
    sequence_
      [ it "truncateLowerString takes the first N characters" $
          truncateLowerString 3 "iceberg" `shouldBe` "ice"
      , it "truncateUpperString bumps the last character" $
          truncateUpperString 3 "iceberg" `shouldBe` Just "icf"
      , it "truncateUpperString returns Nothing when prefix is all max" $
          truncateUpperString 2 "\1114111\1114111x" `shouldBe` Nothing
      , it "truncateUpperString preserves short strings" $
          truncateUpperString 5 "ice" `shouldBe` Just "ice"
      , it "truncateLowerBytes takes the first N bytes" $
          truncateLowerBytes 3 (BS.pack [1, 2, 3, 4, 5]) `shouldBe` BS.pack [1, 2, 3]
      , it "truncateUpperBytes carries through 0xFF" $
          truncateUpperBytes 3 (BS.pack [1, 0xFF, 0xFF, 0]) `shouldBe` Just (BS.pack [2, 0, 0])
      , it "truncateUpperBytes returns Nothing when prefix is all 0xFF" $
          truncateUpperBytes 2 (BS.pack [0xFF, 0xFF, 0]) `shouldBe` Nothing
      ]
