{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.Filter
Description : KIP-906 client-side record filter

KIP-906 lets a consumer attach a /predicate/ that runs locally
on every record returned by 'poll' so the application sees only
records it cares about. Useful for sparse keys: avoids paying
for the user-callback dispatch on irrelevant records.

The predicate runs after the deserializer; if it returns
'False', the record is silently skipped (commit offsets still
advance past it the same as if the consumer had handled it).

Tracks Java's @ConsumerRecordFilter@ in shape; we provide both
the typeclass-style 'RecordFilter' record + a few common
constructors for tag-based filtering.
-}
module Kafka.Client.Filter
  ( -- * Record filter
    RecordFilter (..)
  , identityFilter
  , noopFilter
    -- * Constructors
  , byKeyEquals
  , byHeaderEquals
  , byTopicIn
  , byPredicate
  , combine
  , (<&&>)
  , (<||>)
  , negateFilter
    -- * Application
  , applyFilter
  ) where

import Data.ByteString (ByteString)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Client.Consumer (ConsumerRecord (..))

-- | A record predicate. Returns 'True' to keep the record,
-- 'False' to drop it.
newtype RecordFilter = RecordFilter
  { runRecordFilter :: ConsumerRecord -> Bool
  }
  deriving stock Generic

-- | Keep every record (the default).
identityFilter :: RecordFilter
identityFilter = RecordFilter (\_ -> True)

-- | Synonym for 'identityFilter'.
noopFilter :: RecordFilter
noopFilter = identityFilter

byKeyEquals :: ByteString -> RecordFilter
byKeyEquals expected = RecordFilter $ \r ->
  case crKey r of
    Just k  -> k == expected
    Nothing -> False

byHeaderEquals :: Text -> ByteString -> RecordFilter
byHeaderEquals name expected = RecordFilter $ \r ->
  case lookup name (crHeaders r) of
    Just v  -> v == expected
    Nothing -> False

-- | Pass records whose topic is in the supplied set.
--
-- Uses 'HashSet Text' for O(1) average membership; for sparse
-- consumers with large allow-lists the 'Data.Set'-based variant
-- this used to be was a measurable hit on the 'poll' path.
byTopicIn :: HashSet Text -> RecordFilter
byTopicIn topics = RecordFilter $ \r ->
  HashSet.member (crTopic r) topics

byPredicate :: (ConsumerRecord -> Bool) -> RecordFilter
byPredicate = RecordFilter

combine :: (Bool -> Bool -> Bool) -> RecordFilter -> RecordFilter -> RecordFilter
combine op (RecordFilter a) (RecordFilter b) = RecordFilter $ \r ->
  a r `op` b r

infixl 3 <&&>
(<&&>) :: RecordFilter -> RecordFilter -> RecordFilter
(<&&>) = combine (&&)

infixl 2 <||>
(<||>) :: RecordFilter -> RecordFilter -> RecordFilter
(<||>) = combine (||)

negateFilter :: RecordFilter -> RecordFilter
negateFilter (RecordFilter p) = RecordFilter (not . p)

-- | Drop records that fail the filter from a list.
applyFilter :: RecordFilter -> [ConsumerRecord] -> [ConsumerRecord]
applyFilter f = filter (runRecordFilter f)
