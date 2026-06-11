{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Streams.Properties.WatermarkSpec
Description : Stream-time / watermark monotonicity properties

The engine maintains a per-task /stream time/ defined as the max
of every record timestamp the topology has consumed so far
(`Kafka.Streams.Time.advanceStreamTime`). This is the watermark
Kafka Streams drives windows, suppress, and stream-time
punctuators off, so any regression is silently catastrophic for
downstream operators.

This module enforces the invariant under chaotic delivery
schedules:

  1. Monotonicity: 'currentStreamTime' never decreases after a
     pipe, regardless of input ordering.
  2. Tight max: after a sequence of valid (non-sentinel)
     timestamps, 'currentStreamTime' equals the maximum of
     those timestamps.
  3. 'advanceDriverStreamTime' is idempotent and respects
     monotonicity: feeding a timestamp older than 'currentStreamTime'
     cannot pull the watermark back.
  4. Interleaved record + manual advances compose correctly:
     stream time is max(all manual advances, all extracted
     record timestamps).
  5. Out-of-order records do not regress the watermark, and the
     final stream time matches what we'd get from feeding the
     same records sorted ascending.
-}
module Streams.Properties.WatermarkSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Imperative (
  Timestamp (..),
  advanceDriverStreamTime,
  buildTopology,
  closeDriver,
  consumed,
  currentStreamTime,
  newDriver,
  newStreamsBuilder,
  pipeInput,
  streamFromTopic,
  textSerde,
 )
import Test.Syd
import Test.Syd.Hedgehog ()


----------------------------------------------------------------------
-- Test topology
----------------------------------------------------------------------

{- | Passthrough topology. The source-side extractor on @"in"@
defaults to 'recordTimestampExtractor', so every timestamp we
pipe is exactly what gets fed into 'advanceStreamTime'.
-}
buildPassthrough :: IO (IO (), Timestamp -> Int -> IO (), IO Timestamp, IO ())
buildPassthrough = do
  b <- newStreamsBuilder
  _ <- streamFromTopic b "in" (consumed textSerde textSerde)
  topo <- buildTopology b
  driver <- newDriver topo "watermark-prop"
  let bytes = BSC.pack . T.unpack :: Text -> BSC.ByteString
      pipe ts part =
        pipeInput driver "in" Nothing (bytes "v") ts part
      readSt = currentStreamTime driver
      close = closeDriver driver
      advanceTo = advanceDriverStreamTime driver
      build = pure ()
  -- "build" is unused — caller has the driver already constructed.
  _ <- pure (advanceTo, advanceTo, build)
  pure (build, pipe, readSt, close)


----------------------------------------------------------------------
-- Property 1: monotonicity under arbitrary order
----------------------------------------------------------------------

prop_monotonic_under_chaos :: H.Property
prop_monotonic_under_chaos = H.property $ do
  -- Positive timestamps only here; the noTimestamp sentinel is
  -- tested separately so the assertion remains crisp.
  tss <-
    H.forAll $
      Gen.list (Range.linear 1 60) (Gen.int64 (Range.linear 0 1_000_000))
  trace <- H.evalIO $ do
    (_, pipe, readSt, close) <- buildPassthrough
    let go !acc [] = pure (reverse acc)
        go !acc (ms : rest) = do
          pipe (Timestamp ms) 0
          Timestamp now <- readSt
          go (now : acc) rest
    rs <- go [] tss
    close
    pure rs
  -- Every prefix's reported stream-time must equal the running max
  -- of the supplied timestamps.
  let expected = drop 1 (scanl max minBound tss)
  trace H.=== expected
  -- And the sequence is non-decreasing.
  H.assert (and (zipWith (<=) trace (drop 1 trace ++ [last trace])))


----------------------------------------------------------------------
-- Property 2: sorted vs unsorted yield identical final watermark
----------------------------------------------------------------------

prop_order_irrelevant_for_final :: H.Property
prop_order_irrelevant_for_final = H.property $ do
  tss <-
    H.forAll $
      Gen.list (Range.linear 1 60) (Gen.int64 (Range.linear 0 1_000_000))
  (finalUnsorted, finalSorted) <- H.evalIO $ do
    let run xs = do
          (_, pipe, readSt, close) <- buildPassthrough
          mapM_ (\ms -> pipe (Timestamp ms) 0) xs
          Timestamp t <- readSt
          close
          pure t
    a <- run tss
    b <- run (List.sort tss)
    pure (a, b)
  finalUnsorted H.=== finalSorted


----------------------------------------------------------------------
-- Property 3: 'advanceDriverStreamTime' is monotonic and idempotent
----------------------------------------------------------------------

prop_advance_is_monotonic :: H.Property
prop_advance_is_monotonic = H.property $ do
  steps <-
    H.forAll $
      Gen.list (Range.linear 1 60) (Gen.int64 (Range.linear 0 1_000_000))
  (observed, expected) <- H.evalIO $ do
    (_, _, readSt, close) <- buildPassthrough
    -- We need an advance hook on the same driver. Reuse via a
    -- second builder is overkill — close the helper and reopen.
    close
    b <- newStreamsBuilder
    _ <- streamFromTopic b "in" (consumed textSerde textSerde)
    topo <- buildTopology b
    driver <- newDriver topo "watermark-adv"
    let go !acc [] = pure (reverse acc)
        go !acc (ms : rest) = do
          advanceDriverStreamTime driver (Timestamp ms)
          Timestamp st <- currentStreamTime driver
          go (st : acc) rest
    rs <- go [] steps
    closeDriver driver
    _ <- pure readSt -- silence unused-name warning
    let exp_ = drop 1 (scanl max minBound steps)
    pure (rs, exp_)
  observed H.=== expected


----------------------------------------------------------------------
-- Property 4: interleaved record + manual advances compose
----------------------------------------------------------------------

data Step
  = Pipe !Int64
  | Manual !Int64
  deriving stock (Eq, Show)


genStep :: H.Gen Step
genStep =
  Gen.choice
    [ Pipe <$> Gen.int64 (Range.linear 0 1_000_000)
    , Manual <$> Gen.int64 (Range.linear 0 1_000_000)
    ]


prop_record_and_advance_compose :: H.Property
prop_record_and_advance_compose = H.property $ do
  steps <- H.forAll (Gen.list (Range.linear 1 60) genStep)
  (observed, expectedFinal) <- H.evalIO $ do
    b <- newStreamsBuilder
    _ <- streamFromTopic b "in" (consumed textSerde textSerde)
    topo <- buildTopology b
    driver <- newDriver topo "watermark-mixed"
    let bytes = BSC.pack . T.unpack :: Text -> BSC.ByteString
    let go !acc [] = pure (reverse acc)
        go !acc (s : rest) = do
          case s of
            Pipe ms ->
              pipeInput driver "in" Nothing (bytes "v") (Timestamp ms) 0
            Manual ms ->
              advanceDriverStreamTime driver (Timestamp ms)
          Timestamp st <- currentStreamTime driver
          go (st : acc) rest
    rs <- go [] steps
    Timestamp final <- currentStreamTime driver
    closeDriver driver
    let allTs = [ms | Pipe ms <- steps] ++ [ms | Manual ms <- steps]
        exp_ = case allTs of
          [] -> minBound
          xs -> maximum xs
    pure (rs, (final, exp_))
  let (final, exp_) = expectedFinal
  -- Pointwise: trace is non-decreasing.
  H.assert (List.sort observed == observed)
  -- The end-state matches the algebraic spec
  -- (stream time = max of every step's timestamp).
  final H.=== exp_


----------------------------------------------------------------------
-- Property 5: advance backwards is a no-op
----------------------------------------------------------------------

prop_backward_advance_is_noop :: H.Property
prop_backward_advance_is_noop = H.property $ do
  high <- H.forAll (Gen.int64 (Range.linear 1 1_000_000))
  low <- H.forAll (Gen.int64 (Range.linear 0 (high - 1)))
  (after, before) <- H.evalIO $ do
    b <- newStreamsBuilder
    _ <- streamFromTopic b "in" (consumed textSerde textSerde)
    topo <- buildTopology b
    driver <- newDriver topo "watermark-back"
    advanceDriverStreamTime driver (Timestamp high)
    Timestamp t1 <- currentStreamTime driver
    advanceDriverStreamTime driver (Timestamp low)
    Timestamp t2 <- currentStreamTime driver
    closeDriver driver
    pure (t2, t1)
  before H.=== high
  after H.=== high


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Watermark monotonicity" $
    sequence_
      [ it "stream-time = running max of piped timestamps" $
          H.withTests 80 prop_monotonic_under_chaos
      , it "order of inputs does not affect final watermark" $
          H.withTests 60 prop_order_irrelevant_for_final
      , it "advanceDriverStreamTime is monotonic" $
          H.withTests 60 prop_advance_is_monotonic
      , it "interleaved pipe + advance compose to max" $
          H.withTests 80 prop_record_and_advance_compose
      , it "backwards advance is a no-op" $
          H.withTests 50 prop_backward_advance_is_noop
      ]
