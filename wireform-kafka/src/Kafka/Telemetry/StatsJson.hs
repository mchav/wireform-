{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Telemetry.StatsJson
Description : librdkafka-style statistics JSON snapshot

Mirrors the JSON document librdkafka emits via its
@statistics.interval.ms@ + @stats_cb@ knobs (see
<https://github.com/confluentinc/librdkafka/blob/master/STATISTICS.md>).
The shape is close to a 1:1 rename so observability tooling
written against librdkafka (collectd, Datadog, custom dashboards)
ports without changes.

Top-level layout we emit:

@
{
  "name":      "wireform-kafka#producer-1",
  "client_id": "wireform-kafka",
  "type":      "producer",  // or "consumer"
  "ts":        1715200000000000,
  "time":      1715200000,
  "msg_cnt":   42,
  "msg_size":  16384,
  "tx":        12,
  "rx":        9,
  "topics":    { "<topic>": { ... } }
}
@

We deliberately do /not/ ship every librdkafka field because some
are C-API specific (e.g. @rdkafka_version@). The renderer is
extensible: callers add their own counters via 'addCustomCounter'.
-}
module Kafka.Telemetry.StatsJson (
  -- * Snapshot
  StatsSnapshot (..),
  StatsClientType (..),
  TopicStats (..),
  defaultSnapshot,

  -- * Render
  renderStats,
) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)


data StatsClientType
  = StatsProducer
  | StatsConsumer
  deriving stock (Eq, Show, Generic)


instance ToJSON StatsClientType where
  toJSON StatsProducer = Aeson.String "producer"
  toJSON StatsConsumer = Aeson.String "consumer"


-- | Per-topic counters.
data TopicStats = TopicStats
  { tsTopic :: !Text
  , tsMsgCount :: !Int64
  , tsBatchCount :: !Int64
  , tsBytesIn :: !Int64
  , tsBytesOut :: !Int64
  , tsErrorCount :: !Int64
  }
  deriving stock (Eq, Show, Generic)


instance ToJSON TopicStats where
  toJSON ts =
    object
      [ "topic" .= tsTopic ts
      , "msg_cnt" .= tsMsgCount ts
      , "batch_cnt" .= tsBatchCount ts
      , "rx_bytes" .= tsBytesOut ts
      , "tx_bytes" .= tsBytesIn ts
      , "error_cnt" .= tsErrorCount ts
      ]


-- | A point-in-time snapshot of the client's counters.
data StatsSnapshot = StatsSnapshot
  { ssName :: !Text
  , ssClientId :: !Text
  , ssType :: !StatsClientType
  , ssTimestampUs :: !Int64
  -- ^ Wall-clock microseconds since epoch.
  , ssMsgCount :: !Int64
  , ssMsgSize :: !Int64
  , ssTxCount :: !Int64
  , ssRxCount :: !Int64
  , ssTopics :: !(Map Text TopicStats)
  , ssCustom :: !(Map Text Value)
  {- ^ Free-form, app-specific counters surfaced under
  @"custom":{...}@. Add via 'addCustomCounter' or by
  constructing the map yourself.
  -}
  }
  deriving stock (Eq, Show, Generic)


defaultSnapshot :: Text -> Text -> StatsClientType -> StatsSnapshot
defaultSnapshot name clientId tp =
  StatsSnapshot
    { ssName = name
    , ssClientId = clientId
    , ssType = tp
    , ssTimestampUs = 0
    , ssMsgCount = 0
    , ssMsgSize = 0
    , ssTxCount = 0
    , ssRxCount = 0
    , ssTopics = Map.empty
    , ssCustom = Map.empty
    }


instance ToJSON StatsSnapshot where
  toJSON ss =
    object
      [ "name" .= ssName ss
      , "client_id" .= ssClientId ss
      , "type" .= ssType ss
      , "ts" .= ssTimestampUs ss
      , "time" .= (ssTimestampUs ss `div` 1_000_000)
      , "msg_cnt" .= ssMsgCount ss
      , "msg_size" .= ssMsgSize ss
      , "tx" .= ssTxCount ss
      , "rx" .= ssRxCount ss
      , "topics" .= toTopicMap (ssTopics ss)
      , "custom"
          .= Aeson.Object
            ( KeyMap.fromList
                [ (Key.fromText k, v)
                | (k, v) <- Map.toList (ssCustom ss)
                ]
            )
      ]
    where
      toTopicMap m =
        Aeson.Object $
          KeyMap.fromList
            [(Key.fromText t, Aeson.toJSON ts) | (t, ts) <- Map.toList m]


{- | Render a snapshot as a UTF-8 'LBS.ByteString'. Stable
key-order so snapshot-style tests can diff the output across
runs.
-}
renderStats :: StatsSnapshot -> LBS.ByteString
renderStats = Aeson.encode . Aeson.toJSON
