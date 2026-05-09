{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.StatsEmitter
Description : @statistics.interval.ms@ scheduled snapshot driver

librdkafka exposes a @stats_cb@ callback that fires every
@statistics.interval.ms@ with the JSON document built by the
client. This module provides the equivalent for wireform-kafka:

  * 'StatsEmitterConfig' — interval + sink callback.
  * 'startStatsEmitter' — fork a background thread that scrapes
    'Kafka.Telemetry.Metrics.MetricsRegistry' on each tick,
    converts to a 'Kafka.Telemetry.StatsJson.StatsSnapshot',
    and hands the result to the sink.
  * 'stopStatsEmitter' — graceful shutdown.

Producer + consumer wiring is left to the caller — invoke
'startStatsEmitter' once you've constructed the registry +
client, save the returned handle, and call 'stopStatsEmitter'
on the way out.
-}
module Kafka.Client.StatsEmitter
  ( StatsEmitterConfig (..)
  , defaultStatsEmitterConfig
  , StatsEmitter
  , startStatsEmitter
  , stopStatsEmitter
  , triggerSnapshotNow
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time.Clock.POSIX as Time
import GHC.Generics (Generic)

import qualified Kafka.Telemetry.Metrics as M
import qualified Kafka.Telemetry.StatsJson as Stats

data StatsEmitterConfig = StatsEmitterConfig
  { secIntervalMs :: !Int
    -- ^ @statistics.interval.ms@. 0 disables emission.
  , secName       :: !Text
  , secClientId   :: !Text
  , secClientType :: !Stats.StatsClientType
  , secSink       :: !(LBS.ByteString -> IO ())
    -- ^ Where to send the rendered JSON. Default
    --   ('defaultStatsEmitterConfig') is a no-op so the emitter
    --   doesn't accidentally spam stderr.
  }

defaultStatsEmitterConfig
  :: Text                         -- ^ name
  -> Text                         -- ^ client id
  -> Stats.StatsClientType
  -> StatsEmitterConfig
defaultStatsEmitterConfig nm cid tp = StatsEmitterConfig
  { secIntervalMs = 0
  , secName       = nm
  , secClientId   = cid
  , secClientType = tp
  , secSink       = \_ -> pure ()
  }

data StatsEmitter = StatsEmitter
  { seThread   :: !ThreadId
  , seRunning  :: !(TVar Bool)
  , seConfig   :: !StatsEmitterConfig
  , seRegistry :: !M.MetricsRegistry
  }

-- | Start a background stats-emitter thread. The thread sleeps
-- for @secIntervalMs@ between snapshots; if the interval is 0
-- it runs once and then idles (the snapshot can still be
-- triggered manually via 'triggerSnapshotNow').
startStatsEmitter
  :: StatsEmitterConfig
  -> M.MetricsRegistry
  -> IO StatsEmitter
startStatsEmitter cfg reg = do
  running <- newTVarIO True
  tid <- forkIO (loop running)
  pure StatsEmitter
    { seThread   = tid
    , seRunning  = running
    , seConfig   = cfg
    , seRegistry = reg
    }
  where
    loop running = do
      keepGoing <- readTVarIO running
      if not keepGoing
        then pure ()
        else do
          _ <- try (emitOnce cfg reg) :: IO (Either SomeException ())
          let !iv = secIntervalMs cfg
          if iv <= 0
            then do
              -- Idle: wake periodically to recheck the running flag.
              threadDelay 1_000_000
              loop running
            else do
              threadDelay (iv * 1000)
              loop running

emitOnce :: StatsEmitterConfig -> M.MetricsRegistry -> IO ()
emitOnce StatsEmitterConfig{..} reg = do
  snap <- M.snapshotMetrics reg
  nowMicro <- round . (* 1_000_000) <$> Time.getPOSIXTime :: IO Int
  let !st = (Stats.defaultSnapshot secName secClientId secClientType)
        { Stats.ssTimestampUs = fromIntegral nowMicro
        , Stats.ssMsgCount    = lookupCounter snap "kafka.producer.record.send.total"
        , Stats.ssMsgSize     = lookupCounter snap "kafka.producer.record.size.bytes"
        , Stats.ssTxCount     = lookupCounter snap "kafka.producer.request.total"
        , Stats.ssRxCount     = lookupCounter snap "kafka.consumer.fetch.request.total"
        }
  secSink (Stats.renderStats st)

-- | Force an immediate snapshot.
triggerSnapshotNow :: StatsEmitter -> IO ()
triggerSnapshotNow se = emitOnce (seConfig se) (seRegistry se)

-- | Stop the background thread. Idempotent.
stopStatsEmitter :: StatsEmitter -> IO ()
stopStatsEmitter se = do
  atomically (writeTVar (seRunning se) False)
  killThread (seThread se)

lookupCounter :: M.MetricSnapshot -> Text -> Int64
lookupCounter s name =
  let !raw = sumWithName name (M.snapshotCounters s)
  in round raw
  where
    sumWithName n =
      Map.foldlWithKey'
        (\acc (k, _) v -> if k == n then acc + v else acc) 0
