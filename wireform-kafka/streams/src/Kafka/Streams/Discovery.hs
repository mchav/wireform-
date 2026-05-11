{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Discovery
-- Description : KIP-535 cross-instance interactive-query discovery
--
-- Mirrors @org.apache.kafka.streams.StreamsMetadata@ and
-- @KeyQueryMetadata@:
--
--   * 'StreamsMetadata' — what every instance in the group
--     looks like to its peers: host:port, owned partitions per
--     store, standby partitions per store.
--   * 'KeyQueryMetadata' — for a specific @(store, key)@ which
--     instance owns the active and which owns the standby.
--
-- A running 'KafkaStreams' instance:
--
--   1. publishes its own metadata into the consumer-group
--      'JoinGroup' subscription metadata (the JVM uses
--      @application.server@ for the host:port);
--   2. learns peers' metadata from 'SyncGroup' assignment
--      responses;
--   3. exposes lookup helpers so user code can route a query
--      to the right host before issuing IQ.
--
-- The streams runtime currently exposes the local-side surface
-- ('localStreamsMetadata' + 'setApplicationServer'); the
-- cross-instance assignment-metadata exchange lands on the
-- live consumer-group path. The data types are the same.
module Kafka.Streams.Discovery
  ( -- * Metadata
    StreamsMetadata (..)
  , HostInfo (..)
  , parseHostInfo
    -- * KeyQueryMetadata
  , KeyQueryMetadata (..)
  , makeKeyQueryMetadata
  ) where

import Data.Int (Int32)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import GHC.Generics (Generic)

import qualified Kafka.Client.Consumer as KC

-- | host:port pair an instance advertises in JoinGroup's
-- subscription metadata. Mirrors Java's
-- @org.apache.kafka.streams.state.HostInfo@.
data HostInfo = HostInfo
  { hostInfoHost :: !Text
  , hostInfoPort :: !Int
  }
  deriving stock (Eq, Ord, Show, Generic)

-- | Parse a @"host:port"@ string into a 'HostInfo'. Returns
-- 'Left' with a reason if the format is wrong.
parseHostInfo :: Text -> Either String HostInfo
parseHostInfo t = case T.splitOn ":" t of
  [h, pTxt] -> case T.decimal pTxt of
    Right (p, rest) | T.null rest -> Right (HostInfo h p)
    _ -> Left ("application.server: bad port: " <> T.unpack pTxt)
  _ -> Left ("application.server: expected host:port, got " <> T.unpack t)

-- | What one instance of the streams app looks like to its
-- peers. Mirrors Java's 'StreamsMetadata'.
data StreamsMetadata = StreamsMetadata
  { smHost           :: !HostInfo
  , smStateStores    :: !(Set Text)
    -- ^ Every state-store name this instance has materialised.
  , smTopicPartitions :: !(Set KC.TopicPartition)
    -- ^ Active partitions this instance owns.
  , smStandbyTopicPartitions :: !(Set KC.TopicPartition)
    -- ^ Partitions held in standby.
  }
  deriving stock (Eq, Show, Generic)

-- | KIP-535 routing record: for a given @(store, key)@ which
-- instance owns the active and which (if any) owns the
-- standby. Used by external IQ proxies to decide where to
-- send the user's query.
data KeyQueryMetadata = KeyQueryMetadata
  { kqmActiveHost   :: !HostInfo
  , kqmStandbyHosts :: ![HostInfo]
  , kqmPartition    :: !Int32
  }
  deriving stock (Eq, Show, Generic)

-- | Build a 'KeyQueryMetadata' from the metadata of every peer
-- in the group and the partition that owns @key@.
makeKeyQueryMetadata
  :: [StreamsMetadata]            -- ^ all peers' metadata
  -> Text                         -- ^ topic
  -> Int32                        -- ^ partition the key lands on
  -> Maybe KeyQueryMetadata
makeKeyQueryMetadata peers topic part =
  let tp = KC.TopicPartition topic part
      activeOf m = Set.member tp (smTopicPartitions m)
      standbyOf m = Set.member tp (smStandbyTopicPartitions m)
      activePeers  = filter activeOf  peers
      standbyPeers = filter standbyOf peers
  in case activePeers of
       (a : _) -> Just KeyQueryMetadata
         { kqmActiveHost   = smHost a
         , kqmStandbyHosts = map smHost standbyPeers
         , kqmPartition    = part
         }
       [] -> Nothing
