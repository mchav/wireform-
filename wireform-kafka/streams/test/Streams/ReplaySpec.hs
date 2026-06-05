{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.ReplaySpec
-- Description : Tests for offline replay & backfill
module Streams.ReplaySpec (tests) where

import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)

import Data.IORef (modifyIORef', newIORef, readIORef)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)

import Control.Category ((>>>))
import Data.Void (Void)

import Kafka.Streams
import qualified Kafka.Streams.Materialized as Mat
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Topology.Free as F
import Kafka.Streams.Driver (OutputRecord (..), decodeOutput)
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (Header (..), headersFromList)

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

-- | A header-preserving passthrough: source -> sink.
passthrough :: F.Topology Void ()
passthrough = F.source @Text @Text "in" >>> F.sink "out"

validPassthrough :: IO Topo.TopologyValid
validPassthrough = do
  topo <- F.buildTopologyFrom passthrough
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
  , headers_survive_replay
  , offset_window_selects
  , pacer_sees_gaps
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

headers_survive_replay :: TestTree
headers_survive_replay =
  testCase "record headers flow through replay to the sink" $ do
    topo <- validPassthrough
    let hs = headersFromList [Header "trace-id" "abc", Header "bin" "\x01\x02"]
        rec = replayWithHeaders hs
                (replayRecord textSerde textSerde (topicName "in")
                              (Just "k") "v" (Timestamp 1))
    res <- runReplay topo "hdr" defaultReplayPlan [rec]
    case lookup (topicName "out") (replayOutputs res) of
      Just [cr] -> case decodeOutput textSerde textSerde cr of
        Right o -> orHeaders o @?= hs
        Left e  -> assertFailure ("decode failed: " <> show e)
      other -> assertFailure ("expected one output record, got "
                                <> show (fmap length other))

offset_window_selects :: TestTree
offset_window_selects =
  testCase "selectForReplay honours the offset window" $ do
    let recs =
          [ replayWithOffset 100 (line "a" 1)
          , replayWithOffset 150 (line "b" 2)
          , replayWithOffset 200 (line "c" 3)
          , line "d" 4               -- no captured offset
          ]
        plan = defaultReplayPlan
                 { replayFromOffset = Just 120, replayToOffset = Just 200 }
        out = selectForReplay plan recs
    map rrValue out @?= [rrValue (line "b" 0)]

pacer_sees_gaps :: TestTree
pacer_sees_gaps =
  testCase "runReplayPaced calls the pacer with inter-record gaps" $ do
    topo <- validTopology
    ref <- newIORef []
    let pacer gap = modifyIORef' ref (gap :)
        recs = [ line "a" 10, line "b" 35, line "c" 40 ]
    _ <- runReplayPaced topo "pace" defaultReplayPlan pacer recs
    gaps <- readIORef ref
    reverse gaps @?= [25, 5]

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
