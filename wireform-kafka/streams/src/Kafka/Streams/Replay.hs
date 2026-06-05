{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Replay
-- Description : Offline replay & backfill over the topology driver
--
-- Operational utilities for /reprocessing/ a recorded stream of input
-- records through a topology without a broker, built on the
-- 'Kafka.Streams.Driver.TopologyTestDriver'. The two headline use
-- cases:
--
--   * __Replay / reprocessing__ — re-run historical records through a
--     (possibly fixed or changed) topology and capture the outputs and
--     resulting state. Pair with a captured log
--     ('encodeReplayLog' \/ 'decodeReplayLog') to snapshot production
--     traffic and replay it offline.
--
--   * __Backfill__ — populate a topology's state stores from a window
--     of historical data before going live, optionally /time-shifting/
--     the records so windowed aggregations land in the intended
--     buckets. 'withReplayDriver' hands back the live driver so the
--     freshly-built state can be inspected or snapshotted
--     ('dumpKeyValueStore').
--
-- A 'ReplayPlan' selects an event-time window @[from, to)@ and applies
-- an optional time shift; records are fed in their original order
-- (faithful reprocessing), and stream time is advanced to the last
-- timestamp at the end so windowed / suppressed emits flush.
--
-- This is the offline (driver) layer. It is distinct from
-- /changelog replay/ ('Kafka.Streams.Runtime.Standby') and
-- /snapshot restore/ ('Kafka.Streams.State.KeyValue.Snapshot'), which
-- rebuild a single store's bytes rather than reprocessing input
-- through the whole topology.
module Kafka.Streams.Replay
  ( -- * Records
    ReplayRecord (..)
  , replayRecord
  , replayRecordBytes

    -- * Plan
  , ReplayPlan (..)
  , defaultReplayPlan
  , selectForReplay

    -- * Result
  , ReplayResult (..)
  , renderReplayResult

    -- * Running
  , runReplay
  , runReplayWith
  , withReplayDriver

    -- * State inspection (backfill)
  , dumpKeyValueStore

    -- * Capture format (newline-delimited JSON)
  , encodeReplayLog
  , decodeReplayLog
  ) where

import Control.Exception (bracket)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Int (Int64)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Wireform.Base64 (decodeBase64, encodeBase64)

import Kafka.Streams.Driver
  ( CollectedRecord
  , TopologyTestDriver
  , advanceDriverStreamTime
  , closeDriver
  , getKeyValueStore
  , newDriverWith
  , pipeInput
  , readOutputAll
  )
import Kafka.Streams.Errors (logAndContinue)
import Kafka.Streams.Serde (Serde, serialize)
import Kafka.Streams.State.Store
  ( StoreName
  , kvIteratorToList
  , kvsAll
  )
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (TopicName, topicName, unTopicName)

----------------------------------------------------------------------
-- Records
----------------------------------------------------------------------

-- | A single record to feed back through a topology. Mirrors the
-- arguments of 'Kafka.Streams.Driver.pipeInput' (the driver does not
-- carry input headers, so they are intentionally absent).
data ReplayRecord = ReplayRecord
  { rrTopic     :: !TopicName
  , rrKey       :: !(Maybe ByteString)
  , rrValue     :: !ByteString
  , rrTimestamp :: !Timestamp
  , rrPartition :: !Int
  } deriving stock (Eq, Show)

-- | Build a 'ReplayRecord' from typed key / value via serdes
-- (partition defaults to 0).
replayRecord
  :: Serde k -> Serde v
  -> TopicName -> Maybe k -> v -> Timestamp
  -> ReplayRecord
replayRecord ks vs topic mk v ts = ReplayRecord
  { rrTopic     = topic
  , rrKey       = fmap (serialize ks) mk
  , rrValue     = serialize vs v
  , rrTimestamp = ts
  , rrPartition = 0
  }

-- | Build a 'ReplayRecord' straight from key / value bytes.
replayRecordBytes
  :: TopicName -> Maybe ByteString -> ByteString -> Timestamp -> Int
  -> ReplayRecord
replayRecordBytes = ReplayRecord

----------------------------------------------------------------------
-- Plan
----------------------------------------------------------------------

-- | How to select and rewrite records before replaying them.
data ReplayPlan = ReplayPlan
  { replayFrom :: !(Maybe Timestamp)
    -- ^ Inclusive lower bound on the /original/ event time. 'Nothing'
    -- means no lower bound.
  , replayTo :: !(Maybe Timestamp)
    -- ^ Exclusive upper bound on the /original/ event time. 'Nothing'
    -- means no upper bound.
  , replayTimeShiftMs :: !Int64
    -- ^ Milliseconds added to every selected record's timestamp
    -- /after/ window selection. Use to backfill a historical window
    -- into a different target window. Default: 0.
  , replayAdvanceStreamTimeAtEnd :: !Bool
    -- ^ After feeding all records, advance stream time to the last
    -- (shifted) timestamp so windowed / suppressed operators flush
    -- their pending emits. Default: 'True'.
  } deriving stock (Eq, Show)

defaultReplayPlan :: ReplayPlan
defaultReplayPlan = ReplayPlan
  { replayFrom                   = Nothing
  , replayTo                     = Nothing
  , replayTimeShiftMs            = 0
  , replayAdvanceStreamTimeAtEnd = True
  }

-- | Apply a plan to a record list: keep records whose original
-- timestamp falls in @[from, to)@, then add the time shift. Input
-- order is preserved (faithful reprocessing).
selectForReplay :: ReplayPlan -> [ReplayRecord] -> [ReplayRecord]
selectForReplay plan = map shift . filter inWindow
  where
    inWindow r = aboveFrom (rrTimestamp r) && belowTo (rrTimestamp r)
    aboveFrom ts = maybe True (ts >=) (replayFrom plan)
    belowTo ts = maybe True (ts <) (replayTo plan)
    shift r
      | replayTimeShiftMs plan == 0 = r
      | otherwise = r { rrTimestamp = bump (rrTimestamp r) }
    bump (Timestamp t) = Timestamp (t + replayTimeShiftMs plan)

----------------------------------------------------------------------
-- Result
----------------------------------------------------------------------

-- | Outcome of a replay run.
data ReplayResult = ReplayResult
  { replayConsumed     :: !Int
    -- ^ Records actually fed through the topology.
  , replaySkipped      :: !Int
    -- ^ Records dropped by the plan's window.
  , replayOutputs      :: ![(TopicName, [CollectedRecord])]
    -- ^ Output records drained per sink topic.
  , replayMinTimestamp :: !(Maybe Timestamp)
  , replayMaxTimestamp :: !(Maybe Timestamp)
  }

-- | A short human-readable summary line.
renderReplayResult :: ReplayResult -> Text
renderReplayResult r =
  T.intercalate " "
    [ "consumed=" <> T.pack (show (replayConsumed r))
    , "skipped=" <> T.pack (show (replaySkipped r))
    , "outputTopics=" <> T.pack (show (length (replayOutputs r)))
    , "outputRecords=" <> T.pack (show outRecs)
    , "ts=" <> tsRange
    ]
  where
    outRecs = List.foldl' (\acc (_, rs) -> acc + length rs) 0 (replayOutputs r)
    tsRange = case (replayMinTimestamp r, replayMaxTimestamp r) of
      (Just (Timestamp lo), Just (Timestamp hi)) ->
        "[" <> T.pack (show lo) <> "," <> T.pack (show hi) <> "]"
      _ -> "[]"

----------------------------------------------------------------------
-- Running
----------------------------------------------------------------------

-- | Replay a record log through a fresh driver for the given
-- validated topology, returning the result. The driver is created and
-- closed internally.
runReplay
  :: Topo.TopologyValid -> Text -> ReplayPlan -> [ReplayRecord]
  -> IO ReplayResult
runReplay topo appId plan records =
  bracket
    (newDriverWith topo appId logAndContinue)
    closeDriver
    (\d -> runReplayWith d plan records)

-- | Replay a record log through an /existing/ driver. Lets callers
-- chain multiple replays against the same accumulating state (e.g.
-- incremental backfill) before inspecting it.
runReplayWith
  :: TopologyTestDriver -> ReplayPlan -> [ReplayRecord] -> IO ReplayResult
runReplayWith d plan records = do
  let selected = selectForReplay plan records
  mapM_ feed selected
  case (replayAdvanceStreamTimeAtEnd plan, maxTs selected) of
    (True, Just mx) -> advanceDriverStreamTime d mx
    _               -> pure ()
  outs <- readOutputAll d
  pure ReplayResult
    { replayConsumed     = length selected
    , replaySkipped      = length records - length selected
    , replayOutputs      = outs
    , replayMinTimestamp = minTs selected
    , replayMaxTimestamp = maxTs selected
    }
  where
    feed r = pipeInput d (rrTopic r) (rrKey r) (rrValue r)
                         (rrTimestamp r) (rrPartition r)

-- | Replay into a fresh driver, then hand the live driver and the
-- result to a continuation so the freshly-built state can be
-- inspected or snapshotted. The driver is closed afterwards (even on
-- exception). This is the backfill entry point.
withReplayDriver
  :: Topo.TopologyValid -> Text -> ReplayPlan -> [ReplayRecord]
  -> (TopologyTestDriver -> ReplayResult -> IO a)
  -> IO a
withReplayDriver topo appId plan records k =
  bracket
    (newDriverWith topo appId logAndContinue)
    closeDriver
    (\d -> do
        res <- runReplayWith d plan records
        k d res)

minTs :: [ReplayRecord] -> Maybe Timestamp
minTs [] = Nothing
minTs rs = Just (List.foldl1' min (map rrTimestamp rs))

maxTs :: [ReplayRecord] -> Maybe Timestamp
maxTs [] = Nothing
maxTs rs = Just (List.foldl1' max (map rrTimestamp rs))

----------------------------------------------------------------------
-- State inspection (backfill)
----------------------------------------------------------------------

-- | Read every entry of a named key-value store as a list. The
-- key / value types are the caller's responsibility (same contract as
-- 'getKeyValueStore'). Returns @[]@ if the store is absent.
dumpKeyValueStore
  :: TopologyTestDriver -> StoreName -> IO [(k, v)]
dumpKeyValueStore d sn = do
  m <- getKeyValueStore d sn
  case m of
    Nothing  -> pure []
    Just kvs -> kvsAll kvs >>= kvIteratorToList

----------------------------------------------------------------------
-- Capture format (newline-delimited JSON)
----------------------------------------------------------------------

-- | Encode a record log as newline-delimited JSON. Key / value bytes
-- are base64-encoded. Suitable for capturing production traffic to a
-- file and replaying it offline.
encodeReplayLog :: [ReplayRecord] -> BL.ByteString
encodeReplayLog =
  BLC.intercalate "\n" . map (A.encode . toJson)

-- | Decode a newline-delimited JSON record log. Blank lines are
-- ignored. Returns the first parse error, if any.
decodeReplayLog :: BL.ByteString -> Either String [ReplayRecord]
decodeReplayLog =
  traverse decodeLine . filter (not . BL.null) . BLC.lines
  where
    decodeLine ln = do
      v <- A.eitherDecode ln
      A.parseEither parseJson v

toJson :: ReplayRecord -> A.Value
toJson r = A.object
  [ "topic"     .= unTopicName (rrTopic r)
  , "key"       .= fmap b64 (rrKey r)
  , "value"     .= b64 (rrValue r)
  , "timestamp" .= unTimestamp (rrTimestamp r)
  , "partition" .= rrPartition r
  ]
  where
    b64 = TE.decodeUtf8 . encodeBase64

parseJson :: A.Value -> A.Parser ReplayRecord
parseJson = A.withObject "ReplayRecord" $ \o -> do
  topic <- o .: "topic"
  mKeyT <- o .:? "key"
  valT  <- o .: "value"
  ts    <- o .: "timestamp"
  part  <- o .: "partition"
  key   <- traverse decodeField mKeyT
  val   <- decodeField valT
  pure ReplayRecord
    { rrTopic     = topicName topic
    , rrKey       = key
    , rrValue     = val
    , rrTimestamp = Timestamp ts
    , rrPartition = part
    }
  where
    decodeField t =
      case decodeBase64 (TE.encodeUtf8 t) of
        Just bs -> pure bs
        Nothing -> fail "ReplayRecord: invalid base64 field"
