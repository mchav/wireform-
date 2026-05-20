module Test.Defensive (tests) where

import Control.Concurrent (threadDelay)
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
      , testCase "tickRate resets after a 1s window" $ do
          rc <- newRateCounter
          _  <- tickRate rc
          _  <- tickRate rc
          threadDelay 1_100_000  -- 1.1s, past the 1s window
          n  <- tickRate rc
          n @?= 1
      ]
  , testGroup "headerListSize"
      [ testCase "RFC 7541 4.1: 32 + name + value per field" $ do
          -- Two headers: ("a", "bc") and ("d", "ef") =>
          -- (32+1+2) + (32+1+2) = 70
          let hs = [("a", "bc"), ("d", "ef")]
          headerListSize hs @?= 70
      , testCase "empty list is zero" $
          headerListSize [] @?= 0
      ]
  ]

-- Re-export local copy of headerListSize for testing; the function in
-- Network.HTTP2.Server is module-local (not exported), so we mirror
-- the RFC formula here.
headerListSize :: [(BS.ByteString, BS.ByteString)] -> Int
headerListSize = foldr (\(n, v) acc -> acc + 32 + BS.length n + BS.length v) 0
