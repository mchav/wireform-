{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Telemetry.PushRuntime
Description : KIP-714 telemetry push runtime shell

A small IO layer around "Kafka.Telemetry.Push". The production
client can call 'runTelemetryStep' from its own loop; tests can
inject a 'TelemetryRunner' and assert exact effects without a
broker.
-}
module Kafka.Telemetry.PushRuntime (
  TelemetryRunner (..),
  TelemetryRuntimeState,
  newTelemetryRuntimeState,
  runTelemetryStep,
  requestTelemetryStop,
  readTelemetryState,
  readBrokerClientInstanceId,
) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text.Encoding qualified as TE
import Kafka.Telemetry.Push (
  TelemetryAction (..),
  TelemetryStateMachine,
  TelemetrySubscription (..),
  applyTelemetryPush,
  applyTelemetryRefresh,
  initialState,
  markTelemetryTerminating,
  planTelemetryStep,
  tsmSubscription,
 )


data TelemetryRunner = TelemetryRunner
  { trRefreshSubscription :: IO (Either String TelemetrySubscription)
  , trEncodeMetrics :: TelemetrySubscription -> IO ByteString
  , trPushMetrics :: TelemetrySubscription -> ByteString -> Bool -> IO (Either String ())
  }


data TelemetryRuntimeState = TelemetryRuntimeState
  { trsMachine :: !(TVar TelemetryStateMachine)
  , trsBrokerClientInstanceId :: !(TVar (Maybe ByteString))
  }


newTelemetryRuntimeState :: IO TelemetryRuntimeState
newTelemetryRuntimeState = do
  machine <- newTVarIO initialState
  brokerId <- newTVarIO Nothing
  pure
    TelemetryRuntimeState
      { trsMachine = machine
      , trsBrokerClientInstanceId = brokerId
      }


readTelemetryState :: TelemetryRuntimeState -> IO TelemetryStateMachine
readTelemetryState = readTVarIO . trsMachine


readBrokerClientInstanceId :: TelemetryRuntimeState -> IO (Maybe ByteString)
readBrokerClientInstanceId = readTVarIO . trsBrokerClientInstanceId


requestTelemetryStop :: TelemetryRuntimeState -> IO ()
requestTelemetryStop st =
  atomically $ modifyTVar' (trsMachine st) markTelemetryTerminating


runTelemetryStep
  :: TelemetryRunner
  -> TelemetryRuntimeState
  -> Int64
  -> IO (Either String TelemetryAction)
runTelemetryStep runner st now = do
  machine <- readTVarIO (trsMachine st)
  case planTelemetryStep now machine of
    TARefreshSubscription -> do
      refreshed <- trRefreshSubscription runner
      case refreshed of
        Left err -> pure (Left err)
        Right sub -> do
          atomically $ do
            modifyTVar' (trsMachine st) (applyTelemetryRefresh now sub)
            writeTVar
              (trsBrokerClientInstanceId st)
              (Just (TE.encodeUtf8 (tsClientInstanceId sub)))
          pure (Right TARefreshSubscription)
    TAPushNow _ -> case tsmSubscriptionOrError machine of
      Left err -> pure (Left err)
      Right sub -> do
        payload <- trEncodeMetrics runner sub
        pushed <-
          if payload == mempty
            then pure (Right ())
            else trPushMetrics runner sub payload False
        case pushed of
          Left err -> pure (Left err)
          Right () -> do
            atomically $
              modifyTVar' (trsMachine st) (applyTelemetryPush now)
            pure (Right (TAPushNow payload))
    sleep@(TASleepUntilMs _) -> pure (Right sleep)
    TADone -> case tsmSubscriptionOrError machine of
      Left _ -> pure (Right TADone)
      Right sub -> do
        payload <- trEncodeMetrics runner sub
        pushed <-
          if payload == mempty
            then pure (Right ())
            else trPushMetrics runner sub payload True
        case pushed of
          Left err -> pure (Left err)
          Right () -> pure (Right TADone)
  where
    tsmSubscriptionOrError machine =
      case tsmSubscription machine of
        Nothing -> Left "telemetry runtime: no active subscription"
        Just sub -> Right sub
