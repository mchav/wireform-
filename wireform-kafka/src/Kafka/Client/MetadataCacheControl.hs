{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Client.MetadataCacheControl
Description : KIP-294 / KIP-526 — reduce consumer + producer metadata lookups

KIP-294 lets consumers reuse the metadata cache the producer
populates rather than each component refreshing independently.
KIP-526 added age-aware refreshing on the producer (only re-poll
metadata for topics that haven't been refreshed within
@metadata.max.age.ms@).

This module is the pure decision layer: 'shouldRefreshTopic'
tells the consumer / producer "yes, the metadata for this topic
is too old", and 'TopicMetadataAge' is the bookkeeping the
caller carries.
-}
module Kafka.Client.MetadataCacheControl (
  -- * Bookkeeping
  TopicMetadataAge (..),
  emptyTopicMetadataAge,
  recordRefresh,

  -- * Decisions
  shouldRefreshTopic,
  topicsNeedingRefresh,

  -- * Window helpers
  isStale,
) where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)


newtype TopicMetadataAge = TopicMetadataAge
  { unTopicMetadataAge :: Map Text Int64
  -- ^ Last-refresh epoch-ms per topic.
  }
  deriving stock (Eq, Show, Generic)


emptyTopicMetadataAge :: TopicMetadataAge
emptyTopicMetadataAge = TopicMetadataAge Map.empty


{- | Record that the metadata for the given topic was just
refreshed.
-}
recordRefresh
  :: Int64
  -- ^ now (ms)
  -> Text
  -> TopicMetadataAge
  -> TopicMetadataAge
recordRefresh now topic (TopicMetadataAge m) =
  TopicMetadataAge (Map.insert topic now m)


{- | Pure check: should we refresh this topic now? Returns 'True'
when the topic isn't tracked yet or its last refresh is older
than @metadata.max.age.ms@.
-}
shouldRefreshTopic
  :: Int64
  -- ^ now (ms)
  -> Int
  -- ^ metadata.max.age.ms
  -> Text
  -> TopicMetadataAge
  -> Bool
shouldRefreshTopic now maxAgeMs topic (TopicMetadataAge m) =
  case Map.lookup topic m of
    Nothing -> True
    Just ts -> now - ts >= fromIntegral maxAgeMs


{- | Bulk version: which of the supplied topics should be
refreshed?
-}
topicsNeedingRefresh
  :: Int64
  -> Int
  -> [Text]
  -> TopicMetadataAge
  -> [Text]
topicsNeedingRefresh now maxAgeMs topics age =
  [t | t <- topics, shouldRefreshTopic now maxAgeMs t age]


{- | Generic age check usable against any (epoch-ms, threshold)
pair.
-}
isStale
  :: Int64
  -- ^ now (ms)
  -> Int64
  -- ^ last-touched (ms)
  -> Int
  -- ^ threshold (ms)
  -> Bool
isStale now ts maxAgeMs = now - ts >= fromIntegral maxAgeMs
