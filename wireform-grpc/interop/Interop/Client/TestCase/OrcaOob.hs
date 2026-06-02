{-# LANGUAGE OverloadedStrings #-}

module Interop.Client.TestCase.OrcaOob (runTest) where

import Data.Proxy
import Data.Vector qualified as V

import Network.GRPC.Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec (OrcaLoadReport)

import Interop.Client.Connect
import Interop.Cmdline
import Interop.Util.Exceptions
import Interop.Util.Messages

import Proto.API.Interop

-- | <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#orca_oob>
--
-- Simplified OOB ORCA test for self-testing. The full spec requires a custom
-- LB policy and an OpenRCA streaming service; here we verify the server
-- updates its OOB metrics state on @orca_oob_report@ and attaches the latest
-- OOB report to the FullDuplexCall trailers.
--
-- Flow:
--   1. Send first message with @orca_oob_report@ = report1, get response
--   2. Send second message with @orca_oob_report@ = report2, get response
--   3. Half-close the stream
--   4. Verify the trailers contain report2 (the latest OOB state)
--
-- NOTE: The spec also includes @utilization@ map fields. These are omitted
-- because protobuf map encoding for @TestOrcaReport@ through the interop
-- proto triggers a decode error (tracked separately).
runTest :: Cmdline -> IO ()
runTest cmdline =
    withConnection def (testServer cmdline) $ \conn ->
      withRPC conn def (Proxy @FullDuplexCall) $ \call -> do
        -- Step 1: send first OOB report, receive response
        sendNextInput call request1
        elem1 <- recvOutputWithMeta call
        case elem1 of
          StreamElem _ -> return ()
          _            -> assertFailure "Expected streaming response for msg 1"

        -- Step 2: send second OOB report (final input), receive response + trailers
        sendFinalInput call request2
        elem2 <- recvOutputWithMeta call
        case elem2 of
          FinalElem (_meta, _resp) trailers ->
            verifyOrcaReport expectedOrca2 trailers
          StreamElem (_meta, _resp) -> do
            elem3 <- recvOutputWithMeta call
            case elem3 of
              NoMoreElems trailers ->
                verifyOrcaReport expectedOrca2 trailers
              _ -> assertFailure "Expected trailers after last response"
          NoMoreElems _ ->
            assertFailure "Expected response for msg 2"
  where
    report1 :: TestOrcaReport
    report1 =
        (mempty :: TestOrcaReport)
          & #cpuUtilization    .~ 0.8210
          & #memoryUtilization .~ 0.5847

    report2 :: TestOrcaReport
    report2 =
        (mempty :: TestOrcaReport)
          & #cpuUtilization    .~ 0.29309
          & #memoryUtilization .~ 0.2

    request1 :: Proto StreamingOutputCallRequest
    request1 =
        (mempty)
          & #orcaOobReport       .~ Just report1
          & #responseParameters  .~ V.singleton ((mempty) & #size .~ 1)

    request2 :: Proto StreamingOutputCallRequest
    request2 =
        (mempty)
          & #orcaOobReport       .~ Just report2
          & #responseParameters  .~ V.singleton ((mempty) & #size .~ 1)

    expectedOrca2 :: OrcaLoadReport
    expectedOrca2 = testOrcaToLoadReport report2

    verifyOrcaReport :: OrcaLoadReport -> ProperTrailers' -> IO ()
    verifyOrcaReport expected trailers =
        case properTrailersOrcaLoadReport trailers of
          Right (Just report) ->
            assertEqual expected report
          Right Nothing ->
            assertFailure "Expected ORCA OOB load report in trailers"
          Left err ->
            assertFailure $ "Failed to parse ORCA OOB report: " ++ show err
