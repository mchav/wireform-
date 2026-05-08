{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Driver
-- Description : Synchronous, broker-less topology driver
--
-- @
-- TopologyTestDriver
-- @
--
-- is the in-process driver Kafka Streams ships for unit-testing; we
-- mirror its surface here. Records pushed via 'pipeInput' are routed
-- through the topology synchronously; records produced by sinks
-- accumulate in the in-memory collector and are read out via
-- 'readOutput'.
--
-- == Differences from the JVM
--
--   * 'pipeInput' is synchronous and exception-passing; there is no
--     producer thread or batching layer.
--   * Wall-clock time is /entirely/ user-controlled — call
--     'advanceWallClockTime' to step it.
--   * Stream time is advanced automatically by the timestamp
--     extractor on every 'pipeInput'.
--   * State stores are realised eagerly when the driver starts; they
--     stay open until 'closeDriver'.
--
-- == Common usage
--
-- @
-- driver <- newDriver topology "test-app"
-- pipeInput driver "input-topic" (Just "k") "v" (Timestamp 0) 0 0
-- out <- readOutput driver "output-topic"
-- closeDriver driver
-- @
module Kafka.Streams.Driver
  ( TopologyTestDriver
  , newDriver
  , newDriverWith
  , pipeInput
  , pipeInputs
  , readOutput
  , readOutputAll
  , advanceWallClockTime
  , advanceDriverStreamTime
  , currentStreamTime
  , currentWallClockTime
  , commitDriver
  , closeDriver
  , driverEngine
  , getKeyValueStore
  , getWindowStore
  , getSessionStore
    -- * Output decoding helpers
  , OutputRecord (..)
  , decodeOutput
    -- * Re-export of the raw collected record shape
  , CollectedRecord (..)
  ) where

import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.IORef
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.Errors (DeserializationHandler, logAndContinue)
import Kafka.Streams.Internal.Engine
  ( Engine
  , advanceStreamTimeTo
  , advanceWallClock
  , buildEngine
  , closeEngine
  , commitEngine
  , engineCollector
  , feedSource
  , storeByName
  , storeEntryAny
  , streamTimeOfEngine
  , wallClockTimeOfEngine
  )
import Kafka.Streams.Internal.RecordCollector
  ( CollectedRecord (..)
  , RecordCollector
  , collectorPeek
  , collectorTake
  , inMemoryCollector
  )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Serde (Serde, deserialize)
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueStore
  , SessionStore
  , StoreName
  , WindowStore
  )
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types
  ( Headers
  , TopicName
  )

----------------------------------------------------------------------
-- Driver
----------------------------------------------------------------------

data TopologyTestDriver = TopologyTestDriver
  { driverEngine :: !Engine
  , driverNextOff :: !(IORef Int64)
  }

-- | Build a driver with default settings:
--
--   * 'TaskId' = @TaskId 0 0@
--   * Deserialisation handler = 'logAndContinue'
--   * Wall-clock time starts at the current system time
newDriver :: Topo.Topology -> Text -> IO TopologyTestDriver
newDriver topo appId =
  case Topo.validateTopology topo of
    Left err -> error $ "TopologyTestDriver: invalid topology: " <> show err
    Right v  -> newDriverWith v appId logAndContinue

-- | Like 'newDriver' but takes an explicit handler and a validated
-- topology.
newDriverWith
  :: Topo.TopologyValid
  -> Text
  -> DeserializationHandler
  -> IO TopologyTestDriver
newDriverWith validated appId deserHandler = do
  collector <- inMemoryCollector
  engine <- buildEngine validated (TaskId 0 0) appId collector deserHandler
  off    <- newIORef 0
  pure TopologyTestDriver
    { driverEngine  = engine
    , driverNextOff = off
    }

-- | Pipe one record through the topology, blocking until all
-- downstream effects (state-store writes, sink emissions, stream-time
-- punctuators) complete.
pipeInput
  :: TopologyTestDriver
  -> TopicName
  -> Maybe ByteString          -- ^ key bytes (or 'Nothing' for a tombstone)
  -> ByteString                -- ^ value bytes
  -> Timestamp
  -> Int                       -- ^ partition (advisory)
  -> IO ()
pipeInput d topic key value ts part = do
  off <- atomicModifyIORef' (driverNextOff d) (\n -> (n + 1, n))
  feedSource (driverEngine d) topic key value ts part off

pipeInputs
  :: TopologyTestDriver
  -> [(TopicName, Maybe ByteString, ByteString, Timestamp, Int)]
  -> IO ()
pipeInputs d = mapM_ go
  where
    go (t, k, v, ts, p) = pipeInput d t k v ts p

-- | Read the next batch of records produced for the given topic. The
-- records are returned in the order they were emitted; the queue is
-- /drained/ — subsequent calls return only newly produced records.
readOutput
  :: TopologyTestDriver
  -> TopicName
  -> IO [CollectedRecord]
readOutput d topic = do
  let collector = engineCollectorOf d
  collectorTake collector topic

-- | Read every output record currently buffered, grouped by topic.
readOutputAll
  :: TopologyTestDriver
  -> IO [(TopicName, [CollectedRecord])]
readOutputAll d = do
  m <- engineCollectorPeekOf d
  let tops = Map.keys m
  forM tops $ \t -> do
    rs <- collectorTake (engineCollectorOf d) t
    pure (t, rs)

-- | Step wall-clock time forward by @deltaMs@ and fire any due
-- wall-clock punctuators.
advanceWallClockTime
  :: TopologyTestDriver
  -> Int64                                  -- ^ delta in milliseconds
  -> IO ()
advanceWallClockTime d delta = advanceWallClock (driverEngine d) delta

-- | Advance stream-time to the supplied timestamp (no record is fed
-- in). Triggers stream-time punctuators that come due.
advanceDriverStreamTime
  :: TopologyTestDriver
  -> Timestamp
  -> IO ()
advanceDriverStreamTime d ts = advanceStreamTimeTo (driverEngine d) ts

currentStreamTime :: TopologyTestDriver -> IO Timestamp
currentStreamTime = streamTimeOfEngine . driverEngine

currentWallClockTime :: TopologyTestDriver -> IO Timestamp
currentWallClockTime = wallClockTimeOfEngine . driverEngine

-- | Flush every store, drain the collector. After this returns the
-- driver is in the same state it would be after a successful commit
-- on a real runtime.
commitDriver :: TopologyTestDriver -> IO ()
commitDriver = commitEngine . driverEngine

closeDriver :: TopologyTestDriver -> IO ()
closeDriver = closeEngine . driverEngine

----------------------------------------------------------------------
-- Store helpers
----------------------------------------------------------------------

-- | Look up a typed key-value store. The user is responsible for the
-- key/value types matching what the topology declared (the same way
-- the JVM driver requires the right @Class@ at @getKeyValueStore@).
getKeyValueStore
  :: TopologyTestDriver
  -> StoreName
  -> IO (Maybe (KeyValueStore k v))
getKeyValueStore d sn = do
  m <- storeByName (driverEngine d) sn
  pure $ case m of
    Just se -> case storeEntryAny se of
      AnyKeyValueStore kvs -> Just (Unsafe.unsafeCoerce kvs)
      _                    -> Nothing
    Nothing -> Nothing

getWindowStore
  :: TopologyTestDriver
  -> StoreName
  -> IO (Maybe (WindowStore k v))
getWindowStore d sn = do
  m <- storeByName (driverEngine d) sn
  pure $ case m of
    Just se -> case storeEntryAny se of
      AnyWindowStore ws -> Just (Unsafe.unsafeCoerce ws)
      _                 -> Nothing
    Nothing -> Nothing

getSessionStore
  :: TopologyTestDriver
  -> StoreName
  -> IO (Maybe (SessionStore k v))
getSessionStore d sn = do
  m <- storeByName (driverEngine d) sn
  pure $ case m of
    Just se -> case storeEntryAny se of
      AnySessionStore ss -> Just (Unsafe.unsafeCoerce ss)
      _                  -> Nothing
    Nothing -> Nothing

----------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------

engineCollectorOf :: TopologyTestDriver -> RecordCollector
engineCollectorOf d = engineCollector (driverEngine d)

engineCollectorPeekOf
  :: TopologyTestDriver
  -> IO (Map TopicName (Seq.Seq CollectedRecord))
engineCollectorPeekOf d =
  collectorPeek (engineCollector (driverEngine d))

----------------------------------------------------------------------
-- Output decoding
----------------------------------------------------------------------

-- | An output record decoded back through the user-supplied serdes.
data OutputRecord k v = OutputRecord
  { orKey       :: !(Maybe k)
  , orValue     :: !v
  , orTimestamp :: !Timestamp
  , orHeaders   :: !Headers
  }

-- | Decode a 'CollectedRecord' through a typed key/value 'Serde' pair.
decodeOutput
  :: Serde k -> Serde v
  -> CollectedRecord
  -> Either String (OutputRecord k v)
decodeOutput ks vs cr = do
  k <- maybe (Right Nothing) (fmap Just . deserialize ks) (crKey cr)
  v <- deserialize vs (crValue cr)
  pure OutputRecord
    { orKey       = k
    , orValue     = v
    , orTimestamp = crTimestamp cr
    , orHeaders   = crHeaders cr
    }
