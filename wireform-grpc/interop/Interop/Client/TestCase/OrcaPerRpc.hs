{-# LANGUAGE OverloadedStrings #-}

module Interop.Client.TestCase.OrcaPerRpc (runTest) where

import Data.Map.Strict qualified as Map
import Data.Proxy

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec (OrcaLoadReport)

import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions
import Interop.Util.Messages

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#orca_per_rpc>
--
-- Verifies that per-query ORCA load reports are attached to the response
-- trailers via the @endpoint-load-metrics-bin@ header, including map fields
-- (@request_cost@ and @utilization@).
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn ->
      withRPC conn def (Proxy @UnaryCall) $ \call -> do
        sendFinalInput call request

        elem1 <- recvOutputWithMeta call
        case elem1 of
          FinalElem (_meta, _resp) trailers ->
            verifyOrcaReport trailers
          StreamElem (_meta, _resp) -> do
            elem2 <- recvOutputWithMeta call
            case elem2 of
              NoMoreElems trailers -> verifyOrcaReport trailers
              _                    -> assertFailure "Expected trailers"
          NoMoreElems _ ->
            assertFailure "Expected a response"
  where
    expectedReport :: TestOrcaReport
    expectedReport =
        (mempty :: TestOrcaReport)
          & #cpuUtilization    .~ 0.8210
          & #memoryUtilization .~ 0.5847
          & #requestCost       .~ Map.fromList [("db", 3.2)]
          & #utilization       .~ Map.fromList [("cpu", 0.8), ("mem", 0.5)]

    request :: Proto SimpleRequest
    request =
        (mempty)
          & #responseSize       .~ 1
          & #orcaPerQueryReport .~ Just expectedReport

    expectedOrcaLoadReport :: OrcaLoadReport
    expectedOrcaLoadReport = testOrcaToLoadReport expectedReport

    verifyOrcaReport :: ProperTrailers' -> IO ()
    verifyOrcaReport trailers =
        case properTrailersOrcaLoadReport trailers of
          Right (Just report) ->
            assertEqual expectedOrcaLoadReport report
          Right Nothing ->
            assertFailure "Expected ORCA load report in trailers"
          Left err ->
            assertFailure $ "Failed to parse ORCA report: " ++ show err
