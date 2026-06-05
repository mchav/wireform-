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

import Data.IORef (modifyIORef', newIORef, readIORef)

import Kafka.Streams.Examples.Ops.Helpers (bullet, section)
import Kafka.Streams.Driver (OutputRecord (..), decodeOutput)
import qualified Kafka.Streams.Types as KT

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

-- | Header-preserving passthrough used to show headers surviving replay.
passthrough :: F.Topology Void ()
passthrough = F.source @Text @Text "in" >>> F.sink "out"

buildValid :: IO Topo.TopologyValid
buildValid = do
  topo <- F.buildTopologyFrom wordCount
  case Topo.validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

buildPassthrough :: IO Topo.TopologyValid
buildPassthrough = do
  topo <- F.buildTopologyFrom passthrough
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

  -- (4) Headers survive replay --------------------------------------
  ptopo <- buildPassthrough
  let hs = KT.headersFromList
             [KT.Header "trace-id" "abc123", KT.Header "source" "demo"]
      hdrRec = replayWithHeaders hs
                 (replayRecord textSerde textSerde (topicName "in")
                               (Just "k") "v" (Timestamp 1))
  hdrRes <- runReplay ptopo "replay-hdr" defaultReplayPlan [hdrRec]
  let outHeaderKeys =
        concatMap (\(_, crs) -> concatMap decodeHeaderKeys crs)
                  (replayOutputs hdrRes)
  bullet "Headers survive replay (passthrough in -> out):"
  bullet ("    output header keys = " <> show outHeaderKeys)

  -- (5) Offset-window replay ----------------------------------------
  let offsetLog = zipWith replayWithOffset [0 ..] capturedLog
      offPlan = defaultReplayPlan
        { replayFromOffset = Just 1, replayToOffset = Just 3 }
  offRes <- runReplay topo "replay-off" offPlan offsetLog
  bullet "Offset-window replay [1,3):"
  bullet ("    " <> T.unpack (renderReplayResult offRes))

  -- (6) Rate-controlled (paced) replay ------------------------------
  waited <- newIORef (0 :: Int)
  let pacer gap = modifyIORef' waited (+ fromIntegral gap)
  _ <- runReplayPaced topo "replay-paced" defaultReplayPlan pacer capturedLog
  totalWait <- readIORef waited
  bullet ("Paced replay would wait " <> show totalWait
            <> "ms total at 1x (sum of inter-record gaps).")

  -- (7) Capture round-trip ------------------------------------------
  let encoded =
        encodeReplayLog (replayWithHeaders hs (head capturedLog) : tail capturedLog)
  bullet ("Capture format (newline-delimited JSON), "
            <> show (length capturedLog) <> " records; first line:")
  TIO.putStrLn ("    " <> firstLine encoded)
  case decodeReplayLog encoded of
    Left err -> bullet ("    decode error: " <> err)
    Right rs -> bullet ("    decoded " <> show (length rs)
                          <> " records (headers + offsets preserved)")

-- | Decode a collected record's header keys for display.
decodeHeaderKeys cr =
  case decodeOutput textSerde textSerde cr of
    Right o -> map KT.headerKey (KT.headersToList (orHeaders o))
    Left _  -> []

sortByKey :: [(Text, Int64)] -> [(Text, Int64)]
sortByKey = foldr insertSorted []
  where
    insertSorted x [] = [x]
    insertSorted x (y : ys)
      | fst x <= fst y = x : y : ys
      | otherwise      = y : insertSorted x ys

firstLine :: BLC.ByteString -> Text
firstLine = T.pack . takeWhile (/= '\n') . BLC.unpack
