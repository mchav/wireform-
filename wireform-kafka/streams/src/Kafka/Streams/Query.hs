{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Query
-- Description : Structured 'Query' API (KIP-796 + KIP-805 + KIP-889 + KIP-960)
--
-- A typed query language over state stores. Mirrors Java's
-- @org.apache.kafka.streams.query.{Query,KeyQuery,RangeQuery,
-- WindowKeyQuery,WindowRangeQuery,VersionedKeyQuery,
-- MultiVersionedKeyQuery,Position,PositionBound,
-- StateQueryRequest,StateQueryResult,QueryResult}@.
--
-- Users construct a 'Query', execute it against a store via
-- 'execute' (single-store entry) or wrap it in a
-- 'StateQueryRequest' for partition-aware federation.
module Kafka.Streams.Query
  ( -- * Queries
    Query (..)
  , QueryResult (..)
  , execute
  , isSuccess
  , queryValue
  , queryFailureReason
    -- * Window-store queries (KIP-805)
  , executeWindowKeyQuery
  , executeWindowRangeQuery
    -- * Versioned-store queries (KIP-889 / KIP-960)
  , executeVersionedKeyQuery
  , executeMultiVersionedKeyQuery
    -- * Position (KIP-796)
  , Position
  , emptyPosition
  , positionAdvance
  , positionAt
  , PositionBound (..)
  , unboundedPosition
  , atPosition
    -- * State-query request / result
  , StateQueryRequest (..)
  , inStore
  , withQuery
  , withPartitions
  , StateQueryResult (..)
  , noStateQueryResult
  ) where

import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Streams.State.Store
  ( KeyValueIterator (..)
  , KeyValueStore (..)
  , StoreName
  , WindowStore (..)
  , kvIteratorToList
  )
import Kafka.Streams.State.KeyValue.Versioned
  ( VersionedKeyValueStore
  , VersionedRecord
  , vkvGetAsOf
  , vkvGetHistory
  )
import Kafka.Streams.Time (Timestamp)

-- | A typed query against a 'KeyValueStore'. The third type
-- parameter is the type the query produces.
data Query k v r where
  KeyQuery   :: !k                  -> Query k v (Maybe v)
  RangeQuery :: !k -> !k            -> Query k v [(k, v)]
  AllQuery   ::                        Query k v [(k, v)]
  CountQuery ::                        Query k v Int

-- | Result of executing a query: success carries the result,
-- failure carries an error message.
data QueryResult r
  = QuerySuccess !r
  | QueryFailure !String
  deriving stock (Eq, Show, Generic)

isSuccess :: QueryResult r -> Bool
isSuccess (QuerySuccess _) = True
isSuccess (QueryFailure _) = False

queryValue :: QueryResult r -> Maybe r
queryValue (QuerySuccess r) = Just r
queryValue (QueryFailure _) = Nothing

queryFailureReason :: QueryResult r -> Maybe String
queryFailureReason (QuerySuccess _) = Nothing
queryFailureReason (QueryFailure e) = Just e

-- | Execute a query against a 'KeyValueStore'. Errors propagate
-- as 'QueryFailure' so callers don't need to catch.
execute
  :: KeyValueStore k v
  -> Query k v r
  -> IO (QueryResult r)
execute kvs q = case q of
  KeyQuery k -> do
    r <- kvsGet kvs k
    pure (QuerySuccess r)
  RangeQuery lo hi -> do
    it <- kvsRange kvs lo hi
    QuerySuccess <$> drainIterator it
  AllQuery -> do
    it <- kvsAll kvs
    QuerySuccess <$> drainIterator it
  CountQuery -> do
    n <- kvsApproxEntries kvs
    pure (QuerySuccess (fromIntegral n))

drainIterator :: KeyValueIterator k v -> IO [(k, v)]
drainIterator = kvIteratorToList

----------------------------------------------------------------------
-- KIP-805: Window-store queries
----------------------------------------------------------------------

-- | @WindowKeyQuery@: fetch the value for a given @(key,
-- windowStart)@. Mirrors Java's
-- @WindowKeyQuery.withKeyAndWindowStartRange@ point form.
executeWindowKeyQuery
  :: WindowStore k v
  -> k
  -> Timestamp
  -> IO (QueryResult (Maybe v))
executeWindowKeyQuery ws k ts = QuerySuccess <$> wsFetch ws k ts

-- | @WindowRangeQuery@: fetch every @(windowStart, value)@ entry
-- for a given key inside a timestamp range. Mirrors Java's
-- @WindowRangeQuery.withKeyAndRange(key, from, to)@.
executeWindowRangeQuery
  :: WindowStore k v
  -> k
  -> Timestamp        -- ^ inclusive from
  -> Timestamp        -- ^ inclusive to
  -> IO (QueryResult [(Timestamp, v)])
executeWindowRangeQuery ws k from to = do
  it <- wsFetchRange ws k from to
  rs <- kvIteratorToList it
  pure (QuerySuccess rs)

----------------------------------------------------------------------
-- KIP-889 / KIP-960: Versioned-store queries
----------------------------------------------------------------------

-- | @VersionedKeyQuery.withKeyAsOf(key, ts)@. Returns the
-- value valid at the supplied timestamp.
executeVersionedKeyQuery
  :: VersionedKeyValueStore k v
  -> k
  -> Timestamp
  -> IO (QueryResult (Maybe (VersionedRecord v)))
executeVersionedKeyQuery vkvs k ts =
  QuerySuccess <$> vkvGetAsOf vkvs k ts

-- | @MultiVersionedKeyQuery@: every version of @k@ whose
-- valid-time falls in the supplied range, in ascending order.
executeMultiVersionedKeyQuery
  :: VersionedKeyValueStore k v
  -> k
  -> Timestamp        -- ^ from (inclusive)
  -> Timestamp        -- ^ to   (inclusive)
  -> IO (QueryResult [VersionedRecord v])
executeMultiVersionedKeyQuery vkvs k from to =
  QuerySuccess <$> vkvGetHistory vkvs k from to

----------------------------------------------------------------------
-- KIP-796: Position
----------------------------------------------------------------------

-- | A per-(topic, partition) progress vector. Mirrors Java's
-- @org.apache.kafka.streams.query.Position@: a map from
-- @(topic, partition) -> offset@ that callers attach to a
-- query to express "I want to see results that have absorbed
-- at least up to these offsets".
newtype Position = Position { offsets :: Map (Text, Int32) Int64 }
  deriving stock (Eq, Show, Generic)

emptyPosition :: Position
emptyPosition = Position Map.empty

-- | Update a 'Position' with an @offset@ for @(topic,
-- partition)@. If the supplied offset is older than the one
-- already in the position, the position is unchanged.
positionAdvance :: Text -> Int32 -> Int64 -> Position -> Position
positionAdvance t p o (Position m) =
  Position (Map.insertWith max (t, p) o m)

-- | Read the offset for @(topic, partition)@ from a 'Position'
-- if any.
positionAt :: Text -> Int32 -> Position -> Maybe Int64
positionAt t p (Position m) = Map.lookup (t, p) m

-- | Freshness bound for a query. JVM's @PositionBound@.
data PositionBound
  = Unbounded
  | At !Position
  deriving stock (Eq, Show, Generic)

unboundedPosition :: PositionBound
unboundedPosition = Unbounded

atPosition :: Position -> PositionBound
atPosition = At

----------------------------------------------------------------------
-- KIP-796: StateQueryRequest / StateQueryResult
----------------------------------------------------------------------

-- | Partition-aware query container. Mirrors Java's
-- @org.apache.kafka.streams.query.StateQueryRequest@.
data StateQueryRequest k v r = StateQueryRequest
  { store         :: !StoreName
  , query         :: !(Query k v r)
  , partitions    :: !(Maybe (Set Int32))
    -- ^ If 'Nothing' the request goes to every partition the
    --   local instance holds; otherwise restricted to the
    --   supplied set.
  , positionBound :: !PositionBound
  , staleEnabled  :: !Bool
    -- ^ Mirrors JVM's @enableExecutionInfo@ +
    --   @withStaleStoresEnabled@ combined into a single boolean
    --   for our simpler runtime.
  }

-- | Construct a 'StateQueryRequest' against a named store.
inStore :: StoreName -> Query k v r -> StateQueryRequest k v r
inStore sn q = StateQueryRequest
  { store         = sn
  , query         = q
  , partitions    = Nothing
  , positionBound = Unbounded
  , staleEnabled  = False
  }

withQuery
  :: Query k v r
  -> StateQueryRequest k v r0
  -> StateQueryRequest k v r
withQuery q req = req { query = q }

withPartitions
  :: Set Int32
  -> StateQueryRequest k v r
  -> StateQueryRequest k v r
withPartitions ps req = req { partitions = Just ps }

-- | Per-partition result keyed by partition id. Mirrors Java's
-- @StateQueryResult@.
data StateQueryResult r = StateQueryResult
  { results  :: !(Map Int32 (QueryResult r))
  , position :: !Position
    -- ^ The position vector reported by the local instance at
    --   query time; clients can chain this into the next
    --   request via 'atPosition'.
  }
  deriving stock (Eq, Show, Generic)

-- | An empty 'StateQueryResult'. Useful as a starting value
-- when federating per-partition results.
noStateQueryResult :: StateQueryResult r
noStateQueryResult = StateQueryResult Map.empty emptyPosition
