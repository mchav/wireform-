{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.TopicId
Description : KIP-516 topic identifiers (UUID) on producer + consumer

KIP-516 introduced /topic IDs/: a stable UUID assigned by the
controller when a topic is created. They survive
delete-and-recreate cycles (a brand-new topic with the same
name gets a new id) so producers / consumers / replicas can
distinguish "the same topic" from "a topic that came back".

Wire-level encoding has shipped for every API that ever
references a topic (Fetch, Produce, Metadata, OffsetCommit,
DeleteTopics, …) — the generated types carry both
@topicName@ and @topicId@. This module exposes the high-level
TopicId value + a topic-id ↔ topic-name resolution table so
the Producer / Consumer can route by id when the broker
prefers (the broker may close a connection if a producer keeps
addressing a deleted-and-recreated topic by name).
-}
module Kafka.Client.TopicId
  ( -- * UUID type
    TopicId (..)
  , nullTopicId
  , isNullTopicId
    -- * Resolution table
  , TopicIdTable
  , newTopicIdTable
  , registerTopicId
  , topicIdFor
  , topicNameFor
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Kafka topic id — a 128-bit UUID encoded as a 16-byte
-- ByteString to mirror the wire shape ('Kafka.Protocol.Primitives.UUID').
-- Use 'nullTopicId' for "no id known".
newtype TopicId = TopicId { unTopicId :: ByteString }
  deriving stock (Eq, Ord, Show, Generic)

nullTopicId :: TopicId
nullTopicId = TopicId (BS.replicate 16 0)

isNullTopicId :: TopicId -> Bool
isNullTopicId (TopicId bs) = bs == BS.replicate 16 0

-- | Bidirectional lookup table.
data TopicIdTable = TopicIdTable
  { titByName :: !(TVar (Map Text TopicId))
  , titById   :: !(TVar (Map TopicId Text))
  }

newTopicIdTable :: IO TopicIdTable
newTopicIdTable = do
  byName <- newTVarIO Map.empty
  byId   <- newTVarIO Map.empty
  pure TopicIdTable { titByName = byName, titById = byId }

-- | Insert or replace a (name, id) pair. The producer / consumer
-- update this whenever a MetadataResponse fills in the topic
-- id; subsequent requests use the recorded id.
registerTopicId
  :: TopicIdTable -> Text -> TopicId -> STM ()
registerTopicId t name tid = do
  modifyTVar' (titByName t) (Map.insert name tid)
  modifyTVar' (titById   t) (Map.insert tid name)

topicIdFor :: TopicIdTable -> Text -> STM (Maybe TopicId)
topicIdFor t name = do
  m <- readTVar (titByName t)
  pure (Map.lookup name m)

topicNameFor :: TopicIdTable -> TopicId -> STM (Maybe Text)
topicNameFor t tid = do
  m <- readTVar (titById t)
  pure (Map.lookup tid m)
