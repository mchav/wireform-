module Test.Defensive (tests) where

import qualified Data.ByteString as BS

import Test.Syd

import Network.HTTP2.RateLimit

tests :: Spec
tests = describe "Defensive" $ sequence_
  [ describe "RateCounter" $ sequence_
      [ it "tickRate counts up within a window" $ do
          rc <- newRateCounter
          n1 <- tickRate rc
          n2 <- tickRate rc
          n3 <- tickRate rc
          n1 `shouldBe` 1
          n2 `shouldBe` 2
          n3 `shouldBe` 3
      , it "tickRateWith resets after simulated 1s window" $ do
          rc <- newRateCounter
          -- First tick anchors the window to a known monotonic time
          -- by jumping well past the initial window
          n1 <- tickRateWith 1e9 rc
          n1 `shouldBe` 1
          n2 <- tickRateWith (1e9 + 0.5) rc
          n2 `shouldBe` 2
          -- Jump forward past the 1s window boundary
          n3 <- tickRateWith (1e9 + 1.1) rc
          n3 `shouldBe` 1
      , it "tickRateWith at exact boundary does not reset" $ do
          rc <- newRateCounter
          _  <- tickRateWith 1e9 rc
          _  <- tickRateWith (1e9 + 0.5) rc
          -- Exactly 1.0s is NOT > 1.0, so still same window
          n  <- tickRateWith (1e9 + 1.0) rc
          n `shouldBe` 3
      ]
  , describe "headerListSize" $ sequence_
      [ it "RFC 7541 4.1: 32 + name + value per field" $ do
          let hs = [("a", "bc"), ("d", "ef")]
          headerListSize hs `shouldBe` 70
      , it "empty list is zero" $
          headerListSize [] `shouldBe` 0
      ]
  ]

headerListSize :: [(BS.ByteString, BS.ByteString)] -> Int
headerListSize = foldr (\(n, v) acc -> acc + 32 + BS.length n + BS.length v) 0
