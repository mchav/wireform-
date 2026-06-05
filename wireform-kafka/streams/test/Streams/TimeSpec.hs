module Streams.TimeSpec (tests) where

import Hedgehog ((===), forAll, property, assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Kafka.Streams.Time

tests :: Spec
tests = describe "Time" $ sequence_
  [ it "noTimestamp is unknown" $
      isKnownTimestamp noTimestamp `shouldBe` False
  , it "Timestamp 0 is known" $
      isKnownTimestamp (Timestamp 0) `shouldBe` True
  , it "millis clamps negative" $
      durationMillis (millis (-5)) `shouldBe` 0
  , it "addMillis is inverse of diffMillis" $ property $ do
      ts <- forAll (Timestamp <$> Gen.int64 (Range.linearFrom 0 0 1_000_000))
      n  <- forAll (Gen.int64 (Range.linearFrom 0 (-100_000) 100_000))
      diffMillis (addMillis ts n) ts === n
  , it "advanceStreamTime is monotone" $ property $ do
      a <- forAll (Timestamp <$> Gen.int64 (Range.linearFrom 0 0 1_000_000))
      b <- forAll (Timestamp <$> Gen.int64 (Range.linearFrom 0 0 1_000_000))
      let StreamTime t1 = advanceStreamTime a (advanceStreamTime b initialStreamTime)
          StreamTime t2 = advanceStreamTime b (advanceStreamTime a initialStreamTime)
      t1 === t2
  , it "advanceStreamTime never regresses" $ property $ do
      ts <- forAll
              (mapM (\_ -> Gen.int64 (Range.linearFrom 0 0 100_000))
                    [1..(20 :: Int)])
      let st = foldr (advanceStreamTime . Timestamp) initialStreamTime ts
          StreamTime (Timestamp v) = st
      assert (v >= maximum (0 : map id ts))
  , it "tumbling-window-style addDuration" $
      let t  = Timestamp 1000
          d  = millis 500
       in addDuration t d `shouldBe` Timestamp 1500
  , it "subDuration" $
      subDuration (Timestamp 1000) (millis 250) `shouldBe` Timestamp 750
  ]
