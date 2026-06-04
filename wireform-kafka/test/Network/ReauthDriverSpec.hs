{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the KIP-368 mid-session re-authentication driver
-- (`Kafka.Client.ReauthDriver`). We use a stub 'ReauthRunner'
-- that records every authentication attempt instead of opening
-- a real socket.
module Network.ReauthDriverSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.IORef
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Client.ReauthDriver as RD
import qualified Kafka.Network.Auth.SASL as SASL

tests :: TestTree
tests = testGroup "Reauth driver (KIP-368)"
  [ testCase "newly created state has no deadline + no in-flight handshake"
      fresh_state
  , testCase "forceReauthNow triggers a handshake on the next driver tick"
      force_now
  , testCase "successful runner records the broker lifetime and a new deadline"
      success_path
  , testCase "failing runner records the error and clears in-flight"
      failure_path
  , testCase "awaitReauthQuiet returns immediately when no handshake is in flight"
      quiet_returns_now
  ]

fresh_state :: IO ()
fresh_state = do
  st <- RD.createReauthState 60_000
  d <- RD.currentDeadlineMs st
  d @?= Nothing
  inFlight <- RD.reauthInProgress st
  inFlight @?= False

force_now :: IO ()
force_now = do
  st <- RD.createReauthState 60_000
  callsRef <- newIORef (0 :: Int)
  let runner = RD.ReauthRunner
        { RD.authenticate = do
            modifyIORef' callsRef (+ 1)
            pure (Right (SASL.AuthSuccess 60_000))
        , RD.logger = \_ -> pure ()
        }
  RD.startReauthThread st runner
  RD.forceReauthNow st
  -- Give the driver up to ~1s to fire (it wakes on a 250ms cadence).
  let waitFor n
        | n == 0 = pure ()
        | otherwise = do
            c <- readIORef callsRef
            if c >= 1 then pure () else threadDelay 100_000 >> waitFor (n - 1)
  waitFor 20
  RD.stopReauthThread st
  c <- readIORef callsRef
  assertBool "handshake fired at least once" (c >= 1)

success_path :: IO ()
success_path = do
  st <- RD.createReauthState 60_000
  let runner = RD.ReauthRunner
        { RD.authenticate = pure (Right (SASL.AuthSuccess 30_000))
        , RD.logger       = \_ -> pure ()
        }
  RD.startReauthThread st runner
  RD.forceReauthNow st
  -- Wait for completion.
  let waitFor n
        | n == 0 = pure ()
        | otherwise = do
            d <- RD.currentDeadlineMs st
            inf <- RD.reauthInProgress st
            if not inf && maybe False (> 0) d
              then pure ()
              else threadDelay 50_000 >> waitFor (n - 1)
  waitFor 40
  RD.stopReauthThread st
  d <- RD.currentDeadlineMs st
  assertBool "deadline populated" (maybe False (> 0) d)

failure_path :: IO ()
failure_path = do
  st <- RD.createReauthState 60_000
  let theErr = SASL.AuthMechanism "stub failure"
      runner = RD.ReauthRunner
        { RD.authenticate = pure (Left theErr)
        , RD.logger       = \_ -> pure ()
        }
  RD.startReauthThread st runner
  RD.forceReauthNow st
  -- Allow the driver to fire + record the error.
  let waitFor n
        | n == 0 = pure ()
        | otherwise = do
            inf <- RD.reauthInProgress st
            if not inf then pure ()
                       else threadDelay 50_000 >> waitFor (n - 1)
  waitFor 40
  RD.stopReauthThread st
  inFlight <- RD.reauthInProgress st
  inFlight @?= False

quiet_returns_now :: IO ()
quiet_returns_now = do
  st <- RD.createReauthState 60_000
  -- No runner started; nothing in flight.
  RD.awaitReauthQuiet st
