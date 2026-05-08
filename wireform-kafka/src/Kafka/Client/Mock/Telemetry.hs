{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Client.Mock.Telemetry
-- Description : KIP-714 telemetry counters layered on the mock cluster
--
-- Tracks per-(producer or consumer) operation counts so tests can
-- assert on throughput shapes the same way librdkafka 0150
-- (telemetry mock) does for the real client.
module Kafka.Client.Mock.Telemetry
  ( TelemetryCounters
  , newTelemetryCounters
  , bumpProduce
  , bumpFetch
  , bumpCommit
  , bumpTxnBegin
  , bumpTxnCommit
  , bumpTxnAbort
  , snapshotCounters
  , TelemetrySnapshot (..)
  ) where

import Control.Concurrent.STM
import Data.Int (Int64)
import GHC.Generics (Generic)

data TelemetryCounters = TelemetryCounters
  { tcProduce   :: !(TVar Int64)
  , tcFetch     :: !(TVar Int64)
  , tcCommit    :: !(TVar Int64)
  , tcTxnBegin  :: !(TVar Int64)
  , tcTxnCommit :: !(TVar Int64)
  , tcTxnAbort  :: !(TVar Int64)
  }

data TelemetrySnapshot = TelemetrySnapshot
  { tsProduce   :: !Int64
  , tsFetch     :: !Int64
  , tsCommit    :: !Int64
  , tsTxnBegin  :: !Int64
  , tsTxnCommit :: !Int64
  , tsTxnAbort  :: !Int64
  }
  deriving stock (Eq, Show, Generic)

newTelemetryCounters :: IO TelemetryCounters
newTelemetryCounters = atomically $ do
  p <- newTVar 0
  f <- newTVar 0
  c <- newTVar 0
  b <- newTVar 0
  k <- newTVar 0
  a <- newTVar 0
  pure TelemetryCounters
    { tcProduce   = p
    , tcFetch     = f
    , tcCommit    = c
    , tcTxnBegin  = b
    , tcTxnCommit = k
    , tcTxnAbort  = a
    }

bumpProduce, bumpFetch, bumpCommit
  , bumpTxnBegin, bumpTxnCommit, bumpTxnAbort
  :: TelemetryCounters -> IO ()
bumpProduce   tc = atomically (modifyTVar' (tcProduce   tc) (+ 1))
bumpFetch     tc = atomically (modifyTVar' (tcFetch     tc) (+ 1))
bumpCommit    tc = atomically (modifyTVar' (tcCommit    tc) (+ 1))
bumpTxnBegin  tc = atomically (modifyTVar' (tcTxnBegin  tc) (+ 1))
bumpTxnCommit tc = atomically (modifyTVar' (tcTxnCommit tc) (+ 1))
bumpTxnAbort  tc = atomically (modifyTVar' (tcTxnAbort  tc) (+ 1))

snapshotCounters :: TelemetryCounters -> IO TelemetrySnapshot
snapshotCounters tc = atomically $ do
  p <- readTVar (tcProduce   tc)
  f <- readTVar (tcFetch     tc)
  c <- readTVar (tcCommit    tc)
  b <- readTVar (tcTxnBegin  tc)
  k <- readTVar (tcTxnCommit tc)
  a <- readTVar (tcTxnAbort  tc)
  pure TelemetrySnapshot
    { tsProduce   = p
    , tsFetch     = f
    , tsCommit    = c
    , tsTxnBegin  = b
    , tsTxnCommit = k
    , tsTxnAbort  = a
    }
