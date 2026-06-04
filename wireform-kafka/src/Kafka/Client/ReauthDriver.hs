{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Client.ReauthDriver
Description : KIP-368 mid-session SASL re-authentication driver

Wraps the pure deadline math in 'Kafka.Network.Auth.SASL'
('effectiveReauthDeadlineMs' / 'reauthRequiredAtMs') with the
runtime machinery the producer / consumer pipeline needs:

  * a per-broker 'ReauthState' that tracks the next deadline,
    whether the connection is currently in the middle of a
    handshake, and a /quiesce/ TVar callers block on while the
    handshake is being run;
  * 'startReauthThread' forks a background driver that wakes
    on the deadline, asks the supplied 'ReauthRunner' to do the
    fresh @SaslHandshake@ + @SaslAuthenticate@ round, and
    re-arms the deadline from the broker's
    @session.lifetime.ms@ on success;
  * 'awaitReauthQuiet' is what the pipeline send-loop calls
    just before queueing a new request — it blocks (in STM)
    until the driver has finished any in-flight handshake. In
    the steady state it returns immediately.

The actual @SaslHandshake@ + @SaslAuthenticate@ network
exchange lives in 'Kafka.Network.Auth.SASL.authenticate'; this
module is just the schedule + drain orchestration.

Tests: 'Network.ReauthDriverSpec' exercises the deadline →
quiesce → unquiesce ladder using a stub 'ReauthRunner' so the
spec doesn't open a real socket.
-}
module Kafka.Client.ReauthDriver
  ( -- * State
    ReauthState
  , ReauthRunner (..)
  , createReauthState
    -- * Lifecycle
  , startReauthThread
  , stopReauthThread
    -- * Pipeline interaction
  , awaitReauthQuiet
  , forceReauthNow
    -- * Inspection
  , currentDeadlineMs
  , reauthInProgress
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.Int (Int64)
import GHC.Generics (Generic)

import qualified Kafka.Network.Auth.SASL as SASL
import qualified Kafka.Time as KafkaTime

-- | Pluggable re-auth runner. The pipeline supplies one whose
-- 'rrAuthenticate' opens a fresh handshake on the connection
-- (or, if the implementation prefers, on a sibling channel)
-- and returns the broker-advertised @session.lifetime.ms@ for
-- the next deadline computation.
--
-- Returning 'Left err' marks the connection unusable; the
-- driver writes the error into 'reauthLastError' so the
-- pipeline can tear the connection down.
data ReauthRunner = ReauthRunner
  { authenticate :: IO (Either SASL.AuthError SASL.AuthSuccess)
    -- ^ 'Right AuthSuccess' on success; carries the broker's
    --   @session.lifetime.ms@.
  , logger :: SASL.AuthError -> IO ()
    -- ^ Optional callback for failure observability. The driver
    --   already records the error in 'lastError'; this
    --   gives the producer a chance to bump a metric / log.
  }

-- | Per-connection re-auth state.
data ReauthState = ReauthState
  { deadline               :: !(TVar (Maybe Int64))
    -- ^ Next epoch-ms past which a fresh handshake must
    --   complete. 'Nothing' disables re-auth (e.g. plain TCP
    --   without SASL).
  , inFlight               :: !(TVar Bool)
    -- ^ 'True' between the moment the driver kicks off a
    --   handshake and the moment the handshake completes
    --   (success or fail). The pipeline's send loop blocks on
    --   this via 'awaitReauthQuiet'.
  , running                :: !(TVar Bool)
  , lastError              :: !(TVar (Maybe SASL.AuthError))
  , clientMaxMs            :: !Int
    -- ^ The configured @connections.max.reauth.ms@ value.
  , lastBrokerLifetimeMs   :: !(TVar Int)
    -- ^ The broker's most recent @session.lifetime.ms@.
  , thread                 :: !(TVar (Maybe ThreadId))
  }

-- | Build a fresh state. The deadline is computed lazily on
-- the first authentication callback, so the initial value is
-- 'Nothing' and the driver waits for someone to call
-- 'forceReauthNow' or for the broker's lifetime to be recorded.
createReauthState
  :: Int                  -- ^ client @connections.max.reauth.ms@
  -> IO ReauthState
createReauthState maxMs = do
  d   <- newTVarIO Nothing
  inf <- newTVarIO False
  run <- newTVarIO True
  err <- newTVarIO Nothing
  bro <- newTVarIO 0
  thd <- newTVarIO Nothing
  pure ReauthState
    { deadline             = d
    , inFlight             = inf
    , running              = run
    , lastError            = err
    , clientMaxMs          = maxMs
    , lastBrokerLifetimeMs = bro
    , thread               = thd
    }

-- | Start the background thread. Idempotent; calling twice on
-- the same state has no effect.
startReauthThread :: ReauthState -> ReauthRunner -> IO ()
startReauthThread st runner = do
  existing <- readTVarIO st.thread
  case existing of
    Just _ -> pure ()
    Nothing -> do
      tid <- forkIO (driverLoop st runner)
      atomically $ writeTVar st.thread (Just tid)

stopReauthThread :: ReauthState -> IO ()
stopReauthThread st = do
  atomically (writeTVar st.running False)
  m <- readTVarIO st.thread
  case m of
    Nothing  -> pure ()
    Just tid -> killThread tid

----------------------------------------------------------------------
-- Driver loop
----------------------------------------------------------------------

driverLoop :: ReauthState -> ReauthRunner -> IO ()
driverLoop st runner = loop
  where
    loop = do
      keepGoing <- readTVarIO st.running
      if not keepGoing
        then pure ()
        else do
          mDeadline <- readTVarIO st.deadline
          now <- nowMs
          let !nowI = fromIntegral now :: Int
          case mDeadline of
            Nothing -> do
              -- No deadline set yet (e.g. the SASL handshake
              -- hasn't been run on this connection yet). Sleep
              -- and check again — once the producer / pipeline
              -- bootstraps the connection it'll record the
              -- broker lifetime via 'recordBrokerLifetime' (see
              -- the helper below).
              threadDelay 250_000
              loop
            Just d -> do
              let !dI = fromIntegral d :: Int
              if SASL.reauthRequiredAtMs nowI (Just dI)
                then doHandshake st runner >> loop
                else do
                  let !sleepUs = min 1_000_000
                                  ((dI - nowI) * 1000)
                  threadDelay (max 1000 sleepUs)
                  loop

doHandshake :: ReauthState -> ReauthRunner -> IO ()
doHandshake st runner = do
  atomically $ writeTVar st.inFlight True
  r <- try runner.authenticate :: IO (Either SomeException (Either SASL.AuthError SASL.AuthSuccess))
  case r of
    Left e -> do
      let !err = SASL.AuthTransport ("reauth: " <> show e)
      atomically $ do
        writeTVar st.lastError (Just err)
        writeTVar st.inFlight False
      runner.logger err
    Right (Left err) -> do
      atomically $ do
        writeTVar st.lastError (Just err)
        writeTVar st.inFlight False
      runner.logger err
    Right (Right success) -> do
      now <- nowMs
      let !brokerLifetimeMs = SASL.authSessionLifetimeMs success
      atomically $ do
        writeTVar st.lastBrokerLifetimeMs brokerLifetimeMs
        let !d = SASL.effectiveReauthDeadlineMs
                   (fromIntegral now)
                   brokerLifetimeMs
                   st.clientMaxMs
        writeTVar st.deadline (fmap fromIntegral d)
        writeTVar st.inFlight False
        writeTVar st.lastError Nothing

----------------------------------------------------------------------
-- Pipeline interaction
----------------------------------------------------------------------

-- | Block until no handshake is in flight. The producer's send
-- loop calls this just before queueing a new request — in the
-- steady state it returns immediately; during a handshake it
-- pauses until the driver finishes.
awaitReauthQuiet :: ReauthState -> IO ()
awaitReauthQuiet st = atomically $ do
  inf <- readTVar st.inFlight
  check (not inf)

-- | Trigger a handshake right now (e.g. on first connection
-- bootstrap or when an application code path knows the credentials
-- have changed). Returns immediately; the actual handshake fires
-- on the driver thread.
forceReauthNow :: ReauthState -> IO ()
forceReauthNow st = atomically $
  -- Setting the deadline to "now - 1" forces the loop to fire
  -- on its next wakeup (within ~250 ms of idle wait).
  writeTVar st.deadline (Just 0)

-- | Inspect the current deadline. Useful for tests / metrics.
currentDeadlineMs :: ReauthState -> IO (Maybe Int64)
currentDeadlineMs st = readTVarIO st.deadline

-- | Inspect whether a handshake is in flight right now.
reauthInProgress :: ReauthState -> IO Bool
reauthInProgress st = readTVarIO st.inFlight

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

nowMs :: IO Int64
nowMs = KafkaTime.currentTimeMillis
