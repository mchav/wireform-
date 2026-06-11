{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the librdkafka-style statistics JSON renderer
(`Kafka.Telemetry.StatsJson`).
-}
module Client.StatsJsonSpec (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Kafka.Telemetry.StatsJson qualified as Stats
import Test.Syd


tests :: Spec
tests =
  describe "Telemetry: librdkafka stats JSON" $
    sequence_
      [ it
          "default snapshot renders the canonical top-level keys"
          defaults_have_canonical_keys
      , it
          "type field renders as 'producer' / 'consumer'"
          type_renders_correctly
      , it
          "topic counters round-trip through the JSON"
          topic_counters
      , it
          "renderStats produces stable JSON"
          stable_output
      ]


defaults_have_canonical_keys :: IO ()
defaults_have_canonical_keys = do
  let snap = Stats.defaultSnapshot "wfkafka" "client-1" Stats.StatsProducer
      bs = LBS.toStrict (Stats.renderStats snap)
  -- Must contain every documented librdkafka key we mirror.
  let needs =
        [ "name"
        , "client_id"
        , "type"
        , "ts"
        , "time"
        , "msg_cnt"
        , "msg_size"
        , "tx"
        , "rx"
        , "topics"
        , "custom"
        ]
  mapM_
    ( \k ->
        (if (BS.isInfixOf k bs) then pure () else expectationFailure ("missing key: " <> show k))
    )
    needs


type_renders_correctly :: IO ()
type_renders_correctly = do
  Aeson.toJSON Stats.StatsProducer `shouldBe` Aeson.String "producer"
  Aeson.toJSON Stats.StatsConsumer `shouldBe` Aeson.String "consumer"


topic_counters :: IO ()
topic_counters = do
  let snap =
        (Stats.defaultSnapshot "wfkafka" "c-1" Stats.StatsProducer)
          { Stats.ssTopics =
              Map.fromList
                [
                  ( "events"
                  , Stats.TopicStats "events" 100 5 1024 0 1
                  )
                ]
          }
      val = Aeson.toJSON snap
  case val of
    Aeson.Object km -> do
      case KeyMap.lookup (Key.fromText "topics") km of
        Just (Aeson.Object topicsKm) ->
          case KeyMap.lookup (Key.fromText "events") topicsKm of
            Just _ -> pure ()
            Nothing -> error "topics.events missing"
        _ -> error "topics not an object"
    _ -> error "snapshot not an object"


stable_output :: IO ()
stable_output = do
  let snap = Stats.defaultSnapshot "wfkafka" "stable" Stats.StatsProducer
      a = Stats.renderStats snap
      b = Stats.renderStats snap
  a `shouldBe` b
