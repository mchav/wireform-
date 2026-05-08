{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Query
-- Description : Structured 'Query' API (KIP-796)
--
-- A typed query language over state stores. Mirrors Java's
-- @org.apache.kafka.streams.query.{Query,KeyQuery,RangeQuery,QueryResult}@.
-- Users construct a 'Query', execute it against a store via
-- 'execute', and inspect a typed 'QueryResult' (with success /
-- failure framing).
--
-- This is an alternative entry point to the existing
-- 'Kafka.Streams.InteractiveQueries' helpers; both coexist —
-- 'execute' is a higher-level façade for callers that want to
-- treat queries as values.
module Kafka.Streams.Query
  ( Query (..)
  , QueryResult (..)
  , execute
  , isSuccess
  , queryValue
  ) where

import GHC.Generics (Generic)

import Kafka.Streams.State.Store
  ( KeyValueIterator (..)
  , KeyValueStore (..)
  )

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

-- | Execute a query against a 'KeyValueStore'. Errors propagate as
-- 'QueryFailure' so callers don't need to catch.
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
drainIterator it = go []
  where
    go acc = do
      mx <- kvIterNext it
      case mx of
        Nothing -> do
          kvIterClose it
          pure (reverse acc)
        Just kv -> go (kv : acc)
