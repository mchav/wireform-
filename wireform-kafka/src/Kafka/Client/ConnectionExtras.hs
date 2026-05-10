{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.ConnectionExtras
Description : KIP-612 / 974 / 1142 / 1182 / 1191 — connection ergonomics

  * KIP-612: client-side awareness of broker connection-creation-rate
    quotas; the client honours the broker's
    @connection.creation.rate@ throttle response by backing off
    before opening another connection.
  * KIP-974: per-connection idle expiry — cap the time the
    client keeps an idle connection open.
  * KIP-1142: SASL connect-timeout config knob.
  * KIP-1182: Quality-of-Service framework (priority class +
    weight per request type).
  * KIP-1191: configurable @max.idle.time.ms@ per SASL session.
-}
module Kafka.Client.ConnectionExtras
  ( -- * Connection quota throttling (KIP-612)
    ConnectionQuotaState
  , newConnectionQuotaState
  , recordThrottle
  , shouldDelayConnect
    -- * Idle expiry (KIP-974)
  , IdleConnTracker
  , newIdleConnTracker
  , recordActivity
  , isIdle
    -- * SASL connect-timeout (KIP-1142 / KIP-1191)
  , SaslTimeouts (..)
  , defaultSaslTimeouts
    -- * QoS (KIP-1182)
  , QosClass (..)
  , QosWeight (..)
  , prioritise
  ) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Int (Int64)
import qualified Data.List as L
import GHC.Generics (Generic)

----------------------------------------------------------------------
-- KIP-612 connection quota
----------------------------------------------------------------------

newtype ConnectionQuotaState = ConnectionQuotaState
  { cqsThrottleUntilMs :: TVar Int64 }

newConnectionQuotaState :: IO ConnectionQuotaState
newConnectionQuotaState = ConnectionQuotaState <$> newTVarIO 0

-- | Record a broker-imposed throttle (the broker's
-- @connection.creation.rate.throttle.ms@ response). The client
-- waits until @nowMs >= cqsThrottleUntilMs@ before opening
-- another connection.
recordThrottle :: ConnectionQuotaState -> Int64 -> Int -> IO ()
recordThrottle st now throttleMs = atomically $
  writeTVar (cqsThrottleUntilMs st) (now + fromIntegral throttleMs)

shouldDelayConnect :: ConnectionQuotaState -> Int64 -> IO (Maybe Int)
shouldDelayConnect st now = do
  until_ <- readTVarIO (cqsThrottleUntilMs st)
  let !wait = until_ - now
  pure $ if wait > 0 then Just (fromIntegral wait) else Nothing

----------------------------------------------------------------------
-- KIP-974 idle expiry
----------------------------------------------------------------------

newtype IdleConnTracker key = IdleConnTracker
  { ictLastActivity :: TVar (Map key Int64) }

newIdleConnTracker :: Ord key => IO (IdleConnTracker key)
newIdleConnTracker = IdleConnTracker <$> newTVarIO Map.empty

recordActivity :: Ord key => IdleConnTracker key -> key -> Int64 -> IO ()
recordActivity (IdleConnTracker v) k now = atomically $
  modifyTVar' v (Map.insert k now)

isIdle
  :: Ord key
  => IdleConnTracker key
  -> key
  -> Int64        -- ^ now (ms)
  -> Int          -- ^ idle threshold (ms)
  -> IO Bool
isIdle (IdleConnTracker v) k now thresholdMs = do
  m <- readTVarIO v
  pure $ case Map.lookup k m of
    Nothing  -> True
    Just ts  -> now - ts >= fromIntegral thresholdMs

----------------------------------------------------------------------
-- KIP-1142 / KIP-1191 SASL timeouts
----------------------------------------------------------------------

data SaslTimeouts = SaslTimeouts
  { saslConnectTimeoutMs :: !Int
    -- ^ KIP-1142 — bound on the initial SASL handshake.
  , saslMaxIdleMs        :: !Int
    -- ^ KIP-1191 — per-session idle window before forced
    --   re-auth. Default 0 = unbounded.
  }
  deriving stock (Eq, Show, Generic)

defaultSaslTimeouts :: SaslTimeouts
defaultSaslTimeouts = SaslTimeouts
  { saslConnectTimeoutMs = 30_000
  , saslMaxIdleMs        = 0
  }

----------------------------------------------------------------------
-- KIP-1182 quality of service
----------------------------------------------------------------------

data QosClass
  = QosCritical
  | QosHigh
  | QosNormal
  | QosLow
  deriving stock (Eq, Ord, Show, Generic)

newtype QosWeight = QosWeight { unQosWeight :: Int }
  deriving stock (Eq, Show, Generic)

-- | Sort a queue of (class, payload) entries with critical
-- requests first, low last. Within the same class the relative
-- order is preserved (stable sort).
prioritise :: [(QosClass, a)] -> [(QosClass, a)]
prioritise = L.sortOn (\(c, _) -> classWeight c)
  where
    classWeight QosCritical = 0 :: Int
    classWeight QosHigh     = 1
    classWeight QosNormal   = 2
    classWeight QosLow      = 3
