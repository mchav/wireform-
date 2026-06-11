{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Streams.Properties.WindowMathSpec
Description : Property tests for window-assignment math

The properties below capture invariants of the four shipped
window policies (tumbling, hopping, sliding, unlimited) +
session merges. They run a few hundred randomised cases per
run.
-}
module Streams.Properties.WindowMathSpec (tests) where

import Data.Int (Int64)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Time (
  Duration,
  Timestamp (..),
  addMillis,
  millis,
 )
import Kafka.Streams.Window
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------

genTimestamp :: H.Gen Timestamp
genTimestamp =
  Timestamp <$> Gen.int64 (Range.linearFrom 0 0 1_000_000_000)


genWindowSize :: H.Gen Duration
genWindowSize = millis <$> Gen.int64 (Range.linear 1 60_000)


genAdvanceSize :: H.Gen Duration
genAdvanceSize = millis <$> Gen.int64 (Range.linear 1 60_000)


----------------------------------------------------------------------
-- Tumbling
----------------------------------------------------------------------

tumblingTests :: Spec
tumblingTests =
  describe "tumblingWindows" $
    sequence_
      [ it "produces exactly one window per timestamp" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t <- H.forAll genTimestamp
            let ws = windowsAssign (tumblingWindows sz) t
            length ws H.=== 1
      , it "the assigned window contains the timestamp" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t <- H.forAll genTimestamp
            case windowsAssign (tumblingWindows sz) t of
              [w] -> H.assert (windowContains w t)
              xs -> H.annotate ("got " <> show xs) >> H.failure
      , it "window size equals the configured size" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t <- H.forAll genTimestamp
            case windowsAssign (tumblingWindows sz) t of
              [w] -> windowSize w H.=== windowsSize (tumblingWindows sz)
              _ -> H.failure
      , it "timestamps at distance sz fall in disjoint windows" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t <- H.forAll genTimestamp
            let cfg = tumblingWindows sz
                t' = addMillis t (windowsSize cfg)
            case (windowsAssign cfg t, windowsAssign cfg t') of
              ([w1], [w2]) -> do
                H.annotate (show (w1, w2))
                H.assert (not (windowOverlaps w1 w2))
              _ -> H.failure
      , it "two records in the same slot land in identical windows" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t@(Timestamp tv) <- H.forAll genTimestamp
            let cfg = tumblingWindows sz
                szMs = windowsSize cfg
                -- The slot containing @t@ is @[start, start + szMs)@.
                -- Generate a second timestamp /inside the same slot/
                -- so the property holds by construction.
                slack = max 0 (szMs - (tv `mod` szMs) - 1)
            delta <- H.forAll (Gen.int64 (Range.linear 0 slack))
            let t' = addMillis t delta
            case (windowsAssign cfg t, windowsAssign cfg t') of
              ([w1], [w2]) -> w1 H.=== w2
              _ -> H.failure
      ]


----------------------------------------------------------------------
-- Hopping
----------------------------------------------------------------------

hoppingTests :: Spec
hoppingTests =
  describe "hoppingWindows" $
    sequence_
      [ it "every assigned window contains the timestamp" $
          H.property $ do
            sz <- H.forAll genWindowSize
            ad <- H.forAll genAdvanceSize
            t <- H.forAll genTimestamp
            let cfg = hoppingWindows sz ad
                ws = windowsAssign cfg t
            mapM_ (\w -> H.assert (windowContains w t)) ws
      , it "every assigned window has the configured size" $
          H.property $ do
            sz <- H.forAll genWindowSize
            ad <- H.forAll genAdvanceSize
            t <- H.forAll genTimestamp
            let cfg = hoppingWindows sz ad
                ws = windowsAssign cfg t
            mapM_ (\w -> windowSize w H.=== windowsSize cfg) ws
      , it "window starts are advance-aligned" $
          H.property $ do
            sz <- H.forAll genWindowSize
            ad <- H.forAll genAdvanceSize
            t <- H.forAll genTimestamp
            let cfg = hoppingWindows sz ad
                ws = windowsAssign cfg t
                a = windowsAdvance cfg
            mapM_
              (\(Window (Timestamp s) _) -> (s `mod` a) H.=== 0)
              ws
      , it "advance == size collapses to tumbling cardinality" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t <- H.forAll genTimestamp
            let cfg = hoppingWindows sz sz
                ws = windowsAssign cfg t
            length ws H.=== 1
      ]


----------------------------------------------------------------------
-- Sliding
----------------------------------------------------------------------

slidingTests :: Spec
slidingTests =
  describe "slidingWindows" $
    sequence_
      [ it "produces exactly one window per timestamp" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t <- H.forAll genTimestamp
            length (windowsAssign (slidingWindows sz) t) H.=== 1
      , it "window right edge is t + 1" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t@(Timestamp tv) <- H.forAll genTimestamp
            case windowsAssign (slidingWindows sz) t of
              [Window _ (Timestamp e)] -> e H.=== (tv + 1)
              _ -> H.failure
      , it "window contains the assigning timestamp" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t <- H.forAll genTimestamp
            case windowsAssign (slidingWindows sz) t of
              [w] -> H.assert (windowContains w t)
              _ -> H.failure
      , it "window size equals the configured size" $
          H.property $ do
            sz <- H.forAll genWindowSize
            t <- H.forAll genTimestamp
            case windowsAssign (slidingWindows sz) t of
              [w] -> windowSize w H.=== windowsSize (slidingWindows sz)
              _ -> H.failure
      ]


----------------------------------------------------------------------
-- Unlimited
----------------------------------------------------------------------

unlimitedTests :: Spec
unlimitedTests =
  describe "unlimitedWindows" $
    sequence_
      [ it "produces exactly one window per timestamp" $
          H.property $ do
            t <- H.forAll genTimestamp
            length (windowsAssign unlimitedWindows t) H.=== 1
      , it "window starts at the timestamp" $
          H.property $ do
            t@(Timestamp tv) <- H.forAll genTimestamp
            case windowsAssign unlimitedWindows t of
              [Window (Timestamp s) _] -> s H.=== tv
              _ -> H.failure
      ]


----------------------------------------------------------------------
-- Sessions
----------------------------------------------------------------------

sessionTests :: Spec
sessionTests =
  describe "sessionWindows" $
    sequence_
      [ it "abutting sessions merge" $
          H.property $ do
            gapMs <- H.forAll (Gen.int64 (Range.linear 1 1000))
            let sw = sessionWindows (millis gapMs)
            Timestamp a <- H.forAll genTimestamp
            len1 <- H.forAll (Gen.int64 (Range.linear 1 10000))
            let win1 = Window (Timestamp a) (Timestamp (a + len1))
                -- Touch within the inactivity gap exactly.
                startB = a + len1 + gapMs
            len2 <- H.forAll (Gen.int64 (Range.linear 1 10000))
            let win2 = Window (Timestamp startB) (Timestamp (startB + len2))
            case mergeSession sw win1 win2 of
              Just (Window s e) -> do
                (s, e) H.=== (Timestamp a, Timestamp (startB + len2))
              Nothing -> H.failure
      , it "non-touching sessions don't merge" $
          H.property $ do
            gapMs <- H.forAll (Gen.int64 (Range.linear 1 1000))
            let sw = sessionWindows (millis gapMs)
            Timestamp a <- H.forAll genTimestamp
            len1 <- H.forAll (Gen.int64 (Range.linear 1 10000))
            let win1 = Window (Timestamp a) (Timestamp (a + len1))
            offsetBeyondGap <-
              H.forAll
                (Gen.int64 (Range.linear (gapMs + 1) (gapMs + 10000)))
            let startB = a + len1 + offsetBeyondGap
            len2 <- H.forAll (Gen.int64 (Range.linear 1 10000))
            let win2 = Window (Timestamp startB) (Timestamp (startB + len2))
            case mergeSession sw win1 win2 of
              Nothing -> pure ()
              Just merged ->
                H.annotate ("unexpected merge: " <> show merged) >> H.failure
      ]


----------------------------------------------------------------------
-- Aggregate
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Window math properties" $
    sequence_
      [ tumblingTests
      , hoppingTests
      , slidingTests
      , unlimitedTests
      , sessionTests
      ]
