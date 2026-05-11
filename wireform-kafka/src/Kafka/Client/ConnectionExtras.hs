{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.ConnectionExtras
Description : Connection-layer ergonomics: quota throttling,
              idle-expiry tracking, SASL timeouts, request QoS

Helpers that complement the core 'Kafka.Network.Connection':

  * Quota throttling — honour the broker's
    @connection.creation.rate@ throttle response and back off
    before opening another connection.
  * Idle-expiry tracking — cap the time the client keeps an
    idle connection open.
  * SASL connect timeouts — bound the time a SASL handshake
    can block startup.
  * Request QoS — priority class + weight per request type
    for callers that want fair-share scheduling.
-}
module Kafka.Client.ConnectionExtras
  ( -- * Connection-quota throttling
    ConnectionQuotaState
  , newConnectionQuotaState
  , recordThrottle
  , shouldDelayConnect
    -- * Idle expiry
  , IdleConnTracker
  , newIdleConnTracker
  , recordActivity
  , isIdle
    -- * SASL connect timeouts
  , SaslTimeouts (..)
  , defaultSaslTimeouts
    -- * Request QoS
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
-- Connection quota
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
-- Idle expiry
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
-- SASL timeouts
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
-- Quality of service
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
