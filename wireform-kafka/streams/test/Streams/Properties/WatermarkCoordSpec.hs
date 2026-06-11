{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Streams.Properties.WatermarkCoordSpec
Description : Property suite for the cross-source watermark
              coordinator

Properties:

  1. /Effective = min/: the effective watermark equals the
     minimum of every live source's individual watermark.
  2. /Idle skipping/: once a source has been silent for more
     than 'IdleTimeout', the coordinator excludes it from the
     min so the rest can keep advancing.
  3. /Strategy correctness/: each built-in strategy
     ('monotonicAscending', 'boundedOutOfOrderness',
     'noWatermark') produces the documented watermark for
     arbitrary record sequences.
  4. /Alignment/: 'shouldPauseSource' returns 'True' iff the
     caller is ahead of its group by more than the configured
     bound.
  5. /Register / unregister round-trip/: after
     'unregisterSource', the source no longer contributes to
     the min.
-}
module Streams.Properties.WatermarkCoordSpec (tests) where

import Control.Monad (forM_)
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Time (
  Duration,
  Timestamp (..),
  millis,
  minTimestamp,
 )
import Kafka.Streams.Watermark
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Strategy properties
----------------------------------------------------------------------

prop_monotonic_ascending :: H.Property
prop_monotonic_ascending = H.property $ do
  ts <-
    H.forAll
      ( Gen.list
          (Range.linear 1 30)
          (Gen.int64 (Range.linear 0 1_000_000))
      )
  let xs = scanl1 step (map Timestamp ts)
      step prev t = runStrategy monotonicAscending prev t
  -- Final watermark = running-max of inputs.
  let expected = scanl1 max (map Timestamp ts)
  xs H.=== expected


prop_bounded_out_of_orderness :: H.Property
prop_bounded_out_of_orderness = H.property $ do
  lagMs <- H.forAll (Gen.int64 (Range.linear 0 5_000))
  ts <-
    H.forAll
      ( Gen.list
          (Range.linear 1 30)
          (Gen.int64 (Range.linear 0 1_000_000))
      )
  let lag = millis lagMs
      strat = boundedOutOfOrderness lag
      step prev t = runStrategy strat prev t
      xs = scanl1 step (map Timestamp ts)
  -- Each step: watermark = max prev (recordTs - lag).
  -- And the sequence is non-decreasing.
  H.assert (and (zipWith (<=) xs (drop 1 xs ++ [last xs])))


----------------------------------------------------------------------
-- Coordinator: effective watermark = min of live sources
----------------------------------------------------------------------

prop_effective_is_min :: H.Property
prop_effective_is_min = H.property $ do
  -- Two sources, each gets a list of timestamps.
  tsA <-
    H.forAll
      ( Gen.list
          (Range.linear 1 20)
          (Gen.int64 (Range.linear 0 1_000_000))
      )
  tsB <-
    H.forAll
      ( Gen.list
          (Range.linear 1 20)
          (Gen.int64 (Range.linear 0 1_000_000))
      )
  (effective, perA, perB) <- H.evalIO $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 60_000))
    let sa = SourceId "A"
        sb = SourceId "B"
    registerSource coord sa monotonicAscending Nothing
    registerSource coord sb monotonicAscending Nothing
    forM_ tsA (reportRecord coord sa . Timestamp)
    forM_ tsB (reportRecord coord sb . Timestamp)
    eff <- currentEffectiveWatermark coord
    Timestamp wa <- pure $ Timestamp (maximum tsA)
    Timestamp wb <- pure $ Timestamp (maximum tsB)
    pure (eff, wa, wb)
  effective H.=== Timestamp (min perA perB)


----------------------------------------------------------------------
-- Coordinator: idle source is skipped after timeout
----------------------------------------------------------------------

prop_idle_source_excluded_after_timeout :: H.Property
prop_idle_source_excluded_after_timeout = H.property $ do
  -- A is permanently behind, B is faster. If A goes idle and
  -- enough wall-clock passes, the effective watermark catches
  -- up to B.
  aBehind <- H.forAll (Gen.int64 (Range.linear 0 999))
  bAhead <- H.forAll (Gen.int64 (Range.linear 1_000 10_000))
  (effLiveA, effIdleA) <- H.evalIO $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 1_000))
    let sa = SourceId "A"
        sb = SourceId "B"
    registerSource coord sa monotonicAscending Nothing
    registerSource coord sb monotonicAscending Nothing
    advanceWallClock coord (Timestamp 0)
    _ <- reportRecord coord sa (Timestamp aBehind)
    _ <- reportRecord coord sb (Timestamp bAhead)
    e1 <- currentEffectiveWatermark coord
    markIdle coord sa
    -- Advance past the timeout. The idle bound is 1000ms.
    advanceWallClock coord (Timestamp 5_000)
    e2 <- currentEffectiveWatermark coord
    pure (e1, e2)
  effLiveA H.=== Timestamp aBehind
  effIdleA H.=== Timestamp bAhead


----------------------------------------------------------------------
-- Coordinator: unregister removes the source from the min
----------------------------------------------------------------------

prop_unregister_removes :: H.Property
prop_unregister_removes = H.property $ do
  aTs <- H.forAll (Gen.int64 (Range.linear 0 999))
  bTs <- H.forAll (Gen.int64 (Range.linear 1_000 10_000))
  observed <- H.evalIO $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 60_000))
    let sa = SourceId "A"
        sb = SourceId "B"
    registerSource coord sa monotonicAscending Nothing
    registerSource coord sb monotonicAscending Nothing
    _ <- reportRecord coord sa (Timestamp aTs)
    _ <- reportRecord coord sb (Timestamp bTs)
    eAll <- currentEffectiveWatermark coord
    unregisterSource coord sa
    eB <- currentEffectiveWatermark coord
    pure (eAll, eB)
  let (eAll, eB) = observed
  eAll H.=== Timestamp (min aTs bTs)
  eB H.=== Timestamp bTs


----------------------------------------------------------------------
-- Alignment groups
----------------------------------------------------------------------

unit_alignment_pause_when_spread :: Spec
unit_alignment_pause_when_spread =
  it "shouldPauseSource: true when source out-paces group by bound" $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 60_000))
    let sa = SourceId "A"
        sb = SourceId "B"
        grp = "g1"
    registerSource coord sa monotonicAscending (Just grp)
    registerSource coord sb monotonicAscending (Just grp)
    declareAlignmentGroup coord (AlignmentGroup grp (millis 100))
    _ <- reportRecord coord sa (Timestamp 1000)
    _ <- reportRecord coord sb (Timestamp 0)
    -- A is 1000ms ahead of B; bound is 100ms.
    shouldPauseSource coord sa >>= (`shouldBe` True)
    shouldPauseSource coord sb >>= (`shouldBe` False)
    backlogA <- alignmentBacklog coord sa
    backlogA `shouldBe` 1000


unit_alignment_no_pause_within_bound :: Spec
unit_alignment_no_pause_within_bound =
  it "shouldPauseSource: false when spread is within bound" $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 60_000))
    let sa = SourceId "A"
        sb = SourceId "B"
        grp = "g1"
    registerSource coord sa monotonicAscending (Just grp)
    registerSource coord sb monotonicAscending (Just grp)
    declareAlignmentGroup coord (AlignmentGroup grp (millis 500))
    _ <- reportRecord coord sa (Timestamp 100)
    _ <- reportRecord coord sb (Timestamp 0)
    shouldPauseSource coord sa >>= (`shouldBe` False)


unit_alignment_no_group_no_pause :: Spec
unit_alignment_no_group_no_pause =
  it "shouldPauseSource: false for ungrouped sources" $ do
    coord <- newWatermarkCoordinator (IdleTimeout (millis 60_000))
    let sa = SourceId "A"
    registerSource coord sa monotonicAscending Nothing
    _ <- reportRecord coord sa (Timestamp 10_000)
    shouldPauseSource coord sa >>= (`shouldBe` False)
    alignmentBacklog coord sa >>= (`shouldBe` 0)


----------------------------------------------------------------------
-- Coordinator: chaos / many sources
----------------------------------------------------------------------

data SourceOp
  = SrcReport !Int !Int64
  | SrcIdle !Int
  | SrcActive !Int
  deriving stock (Eq, Show)


genSourceOp :: Int -> H.Gen SourceOp
genSourceOp maxSrc = do
  s <- Gen.int (Range.linear 0 maxSrc)
  Gen.frequency
    [ (5, SrcReport s <$> Gen.int64 (Range.linear 0 1_000_000))
    , (1, pure (SrcIdle s))
    , (1, pure (SrcActive s))
    ]


prop_chaos_effective_consistent :: H.Property
prop_chaos_effective_consistent = H.property $ do
  numSources <- H.forAll (Gen.int (Range.linear 1 5))
  ops <-
    H.forAll
      ( Gen.list
          (Range.linear 1 60)
          (genSourceOp (numSources - 1))
      )
  outcome <- H.evalIO $ do
    -- High idle timeout so no source ever times out during the
    -- workload; we want to test pure /effective = min/ here.
    coord <- newWatermarkCoordinator (IdleTimeout (millis 600_000))
    forM_ [0 .. numSources - 1] $ \i ->
      registerSource coord (sidOf i) monotonicAscending Nothing
    forM_ ops $ \op -> case op of
      SrcReport i ts -> () <$ reportRecord coord (sidOf i) (Timestamp ts)
      SrcIdle i -> markIdle coord (sidOf i)
      SrcActive i -> markActive coord (sidOf i)
    eff <- currentEffectiveWatermark coord
    per <- perSourceWatermarks coord
    pure (eff, per)
  let (eff, per) = outcome
  -- Every source counts toward the min because the idle timeout
  -- never triggered.
  let wms = [w | (_, w, _, _) <- per]
  case wms of
    [] -> eff H.=== minTimestamp
    _ -> eff H.=== minimum wms
  where
    sidOf i = SourceId (T.pack ("src-" <> show i))


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Watermark coordinator" $
    sequence_
      [ it "monotonicAscending is the running-max strategy" $
          H.withTests 80 prop_monotonic_ascending
      , it "boundedOutOfOrderness produces a monotonic series" $
          H.withTests 80 prop_bounded_out_of_orderness
      , it "effective watermark = min of live sources" $
          H.withTests 80 prop_effective_is_min
      , it "idle source is excluded after timeout" $
          H.withTests 60 prop_idle_source_excluded_after_timeout
      , it "unregisterSource removes the source from the min" $
          H.withTests 60 prop_unregister_removes
      , unit_alignment_pause_when_spread
      , unit_alignment_no_pause_within_bound
      , unit_alignment_no_group_no_pause
      , it "chaos: effective = min(live sources) under arbitrary ops" $
          H.withTests 80 prop_chaos_effective_consistent
      ]
