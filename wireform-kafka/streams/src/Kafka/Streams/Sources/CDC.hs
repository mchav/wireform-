{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Sources.CDC
-- Description : Change-data-capture source primitive (Riffle §5)
--
-- Most upstream Kafka Streams topologies consume from a Kafka
-- topic. Riffle adds a /source primitive/ for change-data-capture
-- (CDC) feeds, where each record is structured as
-- @Insert / Update / Delete@ with a before-image and an
-- after-image of the row. CDC sources are how Debezium, AWS DMS,
-- and direct database tail-readers publish to Kafka in practice.
--
-- This module provides:
--
--   * 'CDCEvent' — the canonical event ADT.
--   * 'CDCSource' — a record producing 'CDCEvent's that the
--     runtime can poll. The contract is similar in shape to the
--     mock consumer but is independent of the Kafka wire protocol
--     so non-Kafka transports (binlog tail, WAL stream, etc.) can
--     plug in directly.
--   * 'cdcToKTableProcessor' — wires a 'CDCSource' into a
--     materialised KTable, applying the standard CDC-to-KTable
--     mapping (Insert/Update → @put@, Delete → @delete@).
--   * 'inMemoryCDCSource' — deterministic in-process test
--     source. The chaos suite drives this.
module Kafka.Streams.Sources.CDC
  ( -- * Events
    CDCEvent (..)
  , CDCOp (..)
  , cdcKey
  , cdcAfter
  , cdcBefore
    -- * Source contract
  , CDCSource (..)
    -- * In-memory test source
  , inMemoryCDCSource
  , pushCDC
    -- * KTable wiring
  , cdcToKTableStep
  , applyCDCToKVStore
  ) where

import Control.Concurrent.STM
  ( TQueue
  , atomically
  , flushTQueue
  , newTQueueIO
  , writeTQueue
  )
import Data.Int (Int64)
import GHC.Generics (Generic)

import Kafka.Streams.State.Store
  ( KeyValueStore (..)
  )
import Kafka.Streams.Time (Timestamp)

----------------------------------------------------------------------
-- Events
----------------------------------------------------------------------

-- | Kind of change captured.
data CDCOp = CDCInsert | CDCUpdate | CDCDelete
  deriving stock (Eq, Show, Generic)

-- | One CDC event. The @before@ image is present on 'CDCUpdate'
-- and 'CDCDelete'; the @after@ image is present on 'CDCInsert'
-- and 'CDCUpdate'.
--
-- We don't fold these into separate constructors because every
-- downstream processor wants the same set of fields and the
-- 'CDCOp' tag is enough to discriminate. The 'Maybe v' fields
-- match the Debezium / DMS wire schema 1:1.
data CDCEvent k v = CDCEvent
  { cdcOp        :: !CDCOp
  , cdcKey'      :: !k
  , cdcBefore'   :: !(Maybe v)
  , cdcAfter'    :: !(Maybe v)
  , cdcSrcOffset :: !Int64
    -- ^ Source-system offset / LSN / SCN. Monotonically
    -- increasing within one source partition; the runtime uses
    -- this for commit-offset bookkeeping.
  , cdcTs        :: !Timestamp
    -- ^ Event-time timestamp. Used as the record's
    -- 'recordTimestamp' downstream.
  }
  deriving stock (Eq, Show, Generic)

-- Field accessors avoid the awkward primes when callers don't
-- want to enable @DuplicateRecordFields@.
cdcKey :: CDCEvent k v -> k
cdcKey = cdcKey'

cdcBefore :: CDCEvent k v -> Maybe v
cdcBefore = cdcBefore'

cdcAfter :: CDCEvent k v -> Maybe v
cdcAfter = cdcAfter'

----------------------------------------------------------------------
-- Source contract
----------------------------------------------------------------------

-- | A 'CDCSource' is a non-Kafka source the runtime can poll. It
-- is structurally similar to 'Kafka.Streams.Mock.Consumer' but
-- typed at the CDC event ADT.
--
-- The runtime calls 'cdcPoll' on each tick. Empty list means "no
-- new events"; the runtime backs off as it would on a Kafka
-- consumer. 'cdcCommitTo' is called after a successful commit
-- cycle; the source uses this to advance its read cursor on the
-- upstream system (e.g. update a tail-reader checkpoint).
--
-- 'cdcClose' is invoked on shutdown; the source should release
-- any underlying connection / file handle.
data CDCSource k v = CDCSource
  { cdcSourceName :: !String
  , cdcPoll       :: !(IO [CDCEvent k v])
  , cdcCommitTo   :: !(Int64 -> IO ())
  , cdcClose      :: !(IO ())
  }

----------------------------------------------------------------------
-- In-memory reference source
----------------------------------------------------------------------

-- | Deterministic in-process CDC source. Tests push events with
-- 'pushCDC'; the runtime polls them in FIFO order via 'cdcPoll'.
inMemoryCDCSource :: String -> IO (CDCSource k v, TQueue (CDCEvent k v))
inMemoryCDCSource nm = do
  q <- newTQueueIO
  pure
    ( CDCSource
        { cdcSourceName = nm
        , cdcPoll       = atomically (flushTQueue q)
        , cdcCommitTo   = \_ -> pure ()
        , cdcClose      = pure ()
        }
    , q
    )

-- | Push events into an in-memory source's queue. Intended for
-- tests; the production source plug-in fills the queue from its
-- transport thread.
pushCDC :: TQueue (CDCEvent k v) -> CDCEvent k v -> IO ()
pushCDC q e = atomically (writeTQueue q e)

----------------------------------------------------------------------
-- KTable wiring
----------------------------------------------------------------------

-- | Apply a single CDC event to a 'KeyValueStore' the way a
-- canonical CDC-to-KTable bridge would: Insert / Update become
-- 'kvsPut'; Delete becomes 'kvsDelete'. An Insert / Update with a
-- 'Nothing' after-image is treated as a logical delete (matches
-- the Debezium tombstone convention).
applyCDCToKVStore
  :: KeyValueStore k v
  -> CDCEvent k v
  -> IO ()
applyCDCToKVStore kvs e = case cdcOp e of
  CDCInsert -> case cdcAfter e of
    Just v  -> kvsPut kvs (cdcKey e) v
    Nothing -> () <$ kvsDelete kvs (cdcKey e)
  CDCUpdate -> case cdcAfter e of
    Just v  -> kvsPut kvs (cdcKey e) v
    Nothing -> () <$ kvsDelete kvs (cdcKey e)
  CDCDelete -> () <$ kvsDelete kvs (cdcKey e)

-- | Drain one batch of events from a 'CDCSource' onto a
-- 'KeyValueStore'. Returns the count of events applied. The
-- runtime would call this from the topology's source-processor;
-- this helper makes the loop easy to test in isolation.
cdcToKTableStep
  :: CDCSource k v
  -> KeyValueStore k v
  -> IO Int
cdcToKTableStep src kvs = do
  evs <- cdcPoll src
  mapM_ (applyCDCToKVStore kvs) evs
  pure (length evs)
