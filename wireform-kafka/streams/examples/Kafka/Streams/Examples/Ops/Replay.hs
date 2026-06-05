{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.Ops.Replay
-- Description : Demo of offline replay & backfill
--
-- Shows the @Kafka.Streams.Replay@ utilities:
--
--   1. full replay of a captured log through the word-count topology;
--   2. windowed replay (event-time selection);
--   3. backfill — replay history into state, then read the store back;
--   4. capture round-trip through the newline-delimited JSON format.
module Kafka.Streams.Examples.Ops.Replay
  ( runDemo
  ) where

import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Control.Category ((>>>))

import Kafka.Streams
import qualified Kafka.Streams.Materialized as Mat
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Topology.Free as F
import Kafka.Streams.Time (Timestamp (..))

import Kafka.Streams.Examples.Ops.Helpers (bullet, section)

import Kafka.Streams.Replay

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

buildValid :: IO Topo.TopologyValid
buildValid = do
  topo <- F.buildTopologyFrom wordCount
  case Topo.validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

logLine :: Text -> Int64 -> ReplayRecord
logLine v ts =
  replayRecord textSerde textSerde (topicName "lines") Nothing v (Timestamp ts)

capturedLog :: [ReplayRecord]
capturedLog =
  [ logLine "the quick brown fox" 100
  , logLine "the lazy dog"        200
  , logLine "the the the"         300
  , logLine "quick fox jumps"     400
  ]

runDemo :: IO ()
runDemo = do
  section "ReplayDemo"
  topo <- buildValid

  -- (1) Full replay --------------------------------------------------
  full <- runReplay topo "replay-demo" defaultReplayPlan capturedLog
  bullet "Full replay:"
  bullet ("    " <> T.unpack (renderReplayResult full))

  -- (2) Windowed replay ---------------------------------------------
  let windowPlan = defaultReplayPlan
        { replayFrom = Just (Timestamp 150)
        , replayTo   = Just (Timestamp 350)
        }
  windowed <- runReplay topo "replay-demo" windowPlan capturedLog
  bullet "Windowed replay [150,350):"
  bullet ("    " <> T.unpack (renderReplayResult windowed))

  -- (3) Backfill: replay history, then read state back --------------
  bullet "Backfill — state built from the captured log:"
  withReplayDriver topo "backfill-demo" defaultReplayPlan capturedLog $ \d _ -> do
    entries <- dumpKeyValueStore @Text @Int64 d (storeName "counts-store")
    mapM_ (\(k, v) -> bullet ("    " <> T.unpack k <> " = " <> show v))
          (sortByKey entries)

  -- (4) Capture round-trip ------------------------------------------
  let encoded = encodeReplayLog capturedLog
  bullet ("Capture format (newline-delimited JSON), "
            <> show (length capturedLog) <> " records; first line:")
  TIO.putStrLn ("    " <> firstLine encoded)
  case decodeReplayLog encoded of
    Left err -> bullet ("    decode error: " <> err)
    Right rs -> bullet ("    decoded " <> show (length rs)
                          <> " records, round-trip "
                          <> (if rs == capturedLog then "OK" else "MISMATCH"))

sortByKey :: [(Text, Int64)] -> [(Text, Int64)]
sortByKey = foldr insertSorted []
  where
    insertSorted x [] = [x]
    insertSorted x (y : ys)
      | fst x <= fst y = x : y : ys
      | otherwise      = y : insertSorted x ys

firstLine :: BLC.ByteString -> Text
firstLine = T.pack . takeWhile (/= '\n') . BLC.unpack
