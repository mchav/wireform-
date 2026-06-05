{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.ReplaySpec
-- Description : Tests for offline replay & backfill
module Streams.ReplaySpec (tests) where

import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Control.Category ((>>>))
import Data.Void (Void)

import Kafka.Streams
import qualified Kafka.Streams.Materialized as Mat
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Topology.Free as F
import Kafka.Streams.Driver (OutputRecord (..), decodeOutput)
import Kafka.Streams.Time (Timestamp (..))

import Kafka.Streams.Replay

----------------------------------------------------------------------
-- Topology under test: classic word count
----------------------------------------------------------------------

wordCount :: F.Topology Void ()
wordCount =
  F.source @Text @Text "lines"
    >>> F.concatMapValues (T.words . T.toLower :: Text -> [Text])
    >>> F.groupBy (\r -> recordValue r)
    >>> F.count countMat
    >>> F.toStream
    >>> F.sink "counts"
  where
    countMat :: Materialized Text Int64
    countMat =
      Mat.withValueSerde int64Serde
        $ Mat.withKeySerde textSerde
        $ Mat.materializedAs (storeName "counts-store")

validTopology :: IO Topo.TopologyValid
validTopology = do
  topo <- F.buildTopologyFrom wordCount
  case Topo.validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

line :: Text -> Int64 -> ReplayRecord
line v ts = replayRecord textSerde textSerde (topicName "lines") Nothing v (Timestamp ts)

tests :: TestTree
tests = testGroup "Replay"
  [ select_window_and_shift
  , replay_reprocesses_through_topology
  , replay_window_skips_out_of_range
  , backfill_builds_state
  , capture_roundtrips
  ]

----------------------------------------------------------------------

select_window_and_shift :: TestTree
select_window_and_shift =
  testCase "selectForReplay filters [from,to) then shifts timestamps" $ do
    let recs = [ line "a" 10, line "b" 20, line "c" 30, line "d" 40 ]
        plan = defaultReplayPlan
                 { replayFrom = Just (Timestamp 20)
                 , replayTo   = Just (Timestamp 40)
                 , replayTimeShiftMs = 1000
                 }
        out = selectForReplay plan recs
    map rrValue out @?= [valueBytes "b", valueBytes "c"]
    map rrTimestamp out @?= [Timestamp 1020, Timestamp 1030]
  where
    valueBytes v = rrValue (line v 0)

replay_reprocesses_through_topology :: TestTree
replay_reprocesses_through_topology =
  testCase "runReplay feeds records and produces count output" $ do
    topo <- validTopology
    let recs =
          [ line "the quick brown fox" 1
          , line "the lazy dog" 2
          , line "the the the" 3
          ]
    res <- runReplay topo "replay-test" defaultReplayPlan recs
    replayConsumed res @?= 3
    replaySkipped res  @?= 0
    -- The "counts" sink should carry the running count for "the".
    let counts = decodedCounts res
    lookup "the" counts @?= Just 5   -- 1 + 1 + 3
    lookup "fox" counts @?= Just 1

replay_window_skips_out_of_range :: TestTree
replay_window_skips_out_of_range =
  testCase "runReplay honours the plan window" $ do
    topo <- validTopology
    let recs = [ line "alpha" 5, line "beta" 15, line "gamma" 25 ]
        plan = defaultReplayPlan { replayFrom = Just (Timestamp 10)
                                 , replayTo   = Just (Timestamp 20) }
    res <- runReplay topo "replay-win" plan recs
    replayConsumed res @?= 1
    replaySkipped res  @?= 2
    replayMinTimestamp res @?= Just (Timestamp 15)

backfill_builds_state :: TestTree
backfill_builds_state =
  testCase "withReplayDriver builds queryable state from history" $ do
    topo <- validTopology
    let recs = [ line "x y" 1, line "x x" 2 ]
    total <- withReplayDriver topo "backfill" defaultReplayPlan recs $ \d _res -> do
      entries <- dumpKeyValueStore @Text @Int64 d (storeName "counts-store")
      pure (lookup "x" entries)
    total @?= Just 3

capture_roundtrips :: TestTree
capture_roundtrips =
  testCase "encodeReplayLog / decodeReplayLog round-trips" $ do
    let recs =
          [ replayRecordBytes (topicName "t") (Just "k1") "v1" (Timestamp 7) 0
          , replayRecordBytes (topicName "t") Nothing "\x00\xff\x10" (Timestamp 9) 2
          ]
    case decodeReplayLog (encodeReplayLog recs) of
      Left err  -> assertBool ("decode failed: " <> err) False
      Right out -> out @?= recs

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | The "counts" sink carries a changelog (each key re-emitted with
-- its latest count); fold to the last value seen per key.
decodedCounts :: ReplayResult -> [(Text, Int64)]
decodedCounts res =
  foldl latest [] (concatMap decode crs)
  where
    crs = maybe [] id (lookup (topicName "counts") (replayOutputs res))
    decode cr = case decodeOutput textSerde int64Serde cr of
      Right o | Just k <- orKey o -> [(k, orValue o)]
      _                           -> []
    latest acc (k, v) = (k, v) : Prelude.filter ((/= k) . fst) acc
