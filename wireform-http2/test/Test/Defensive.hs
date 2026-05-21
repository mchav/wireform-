module Test.Defensive (tests) where

import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP2.RateLimit

tests :: TestTree
tests = testGroup "Defensive"
  [ testGroup "RateCounter"
      [ testCase "tickRate counts up within a window" $ do
          rc <- newRateCounter
          n1 <- tickRate rc
          n2 <- tickRate rc
          n3 <- tickRate rc
          n1 @?= 1
          n2 @?= 2
          n3 @?= 3
      , testCase "tickRateWith resets after simulated 1s window" $ do
          rc <- newRateCounter
          -- First tick anchors the window to a known monotonic time
          -- by jumping well past the initial window
          n1 <- tickRateWith 1e9 rc
          n1 @?= 1
          n2 <- tickRateWith (1e9 + 0.5) rc
          n2 @?= 2
          -- Jump forward past the 1s window boundary
          n3 <- tickRateWith (1e9 + 1.1) rc
          n3 @?= 1
      , testCase "tickRateWith at exact boundary does not reset" $ do
          rc <- newRateCounter
          _  <- tickRateWith 1e9 rc
          _  <- tickRateWith (1e9 + 0.5) rc
          -- Exactly 1.0s is NOT > 1.0, so still same window
          n  <- tickRateWith (1e9 + 1.0) rc
          n @?= 3
      ]
  , testGroup "headerListSize"
      [ testCase "RFC 7541 4.1: 32 + name + value per field" $ do
          let hs = [("a", "bc"), ("d", "ef")]
          headerListSize hs @?= 70
      , testCase "empty list is zero" $
          headerListSize [] @?= 0
      ]
  ]

headerListSize :: [(BS.ByteString, BS.ByteString)] -> Int
headerListSize = foldr (\(n, v) acc -> acc + 32 + BS.length n + BS.length v) 0
