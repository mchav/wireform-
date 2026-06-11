{-# LANGUAGE BangPatterns #-}

{- |
Module      : Kafka.Streams.State.Window.Timestamped
Description : Window store that also stores per-entry timestamps

Mirrors Java's
@org.apache.kafka.streams.state.TimestampedWindowStore@:
every entry carries the record timestamp alongside the value,
so downstream code (typically a punctuator) can ask
@ReadOnlyTimestampedWindowStore.fetch(key, ts)@ and get both
the value and the record-timestamp without consulting the
record metadata separately.

The in-memory implementation builds on top of an underlying
'WindowStore' whose value type is 'ValueAndTimestamp v' (the
same 'ValueAndTimestamp' as 'TimestampedKeyValueStore'), so
downstream code that reads via @kvIterator@ sees the
timestamp through 'vatTimestamp'.
-}
module Kafka.Streams.State.Window.Timestamped (
  TimestampedWindowStore (..),
  inMemoryTimestampedWindowStore,
  twsPut,
  twsFetch,
  twsFetchRange,
) where

import Data.Int (Int64)
import Kafka.Streams.State.KeyValue.Timestamped (
  ValueAndTimestamp (..),
 )
import Kafka.Streams.State.Store (
  KeyValueIterator,
  StoreName,
  WindowStore (..),
 )
import Kafka.Streams.State.Window.InMemory (
  inMemoryWindowStore,
 )
import Kafka.Streams.Time (Timestamp)


{- | A 'TimestampedWindowStore' is a 'WindowStore' parameterised
by 'ValueAndTimestamp v'. We keep the field name + helper
functions distinct from the underlying @wsPut@ / @wsFetch@
so the call-site reads naturally.
-}
newtype TimestampedWindowStore k v = TimestampedWindowStore
  { unTimestampedWindowStore :: WindowStore k (ValueAndTimestamp v)
  }


{- | Build an in-memory timestamped window store. The
underlying retention is the same as a plain
'WindowStore'; the only difference is that the values are
wrapped in 'ValueAndTimestamp'.
-}
inMemoryTimestampedWindowStore
  :: Ord k
  => StoreName -> Int64 -> Int64 -> IO (TimestampedWindowStore k v)
inMemoryTimestampedWindowStore nm winSize retention = do
  ws <- inMemoryWindowStore nm winSize retention
  pure (TimestampedWindowStore ws)


{- | Put a value with an explicit record timestamp into the
@(key, windowStart)@ slot. Mirrors
@TimestampedWindowStore.put(key, value, ts, windowStart)@
on the JVM (modulo parameter order).
-}
twsPut
  :: TimestampedWindowStore k v
  -> k
  -> v
  -> Timestamp -- value + its record timestamp
  -> Timestamp -- windowStart
  -> IO ()
twsPut (TimestampedWindowStore ws) k v vts winStart =
  wsPut ws k (ValueAndTimestamp v vts) winStart


twsFetch
  :: TimestampedWindowStore k v
  -> k
  -> Timestamp
  -> IO (Maybe (ValueAndTimestamp v))
twsFetch (TimestampedWindowStore ws) = wsFetch ws


twsFetchRange
  :: TimestampedWindowStore k v
  -> k
  -> Timestamp
  -> Timestamp
  -> IO (KeyValueIterator Timestamp (ValueAndTimestamp v))
twsFetchRange (TimestampedWindowStore ws) = wsFetchRange ws
