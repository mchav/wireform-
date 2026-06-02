{-# LANGUAGE OverloadedStrings #-}

module Interop.Server.TestService.UnaryCall (handle) where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.StreamElem qualified as StreamElem
import Network.GRPC.Server
import Network.GRPC.Spec (OrcaLoadReport)

import Interop.Server.Common
import Interop.Util.Messages

import Proto.API.Interop

-- | Handle @TestService.UnaryCall@
--
-- This is not implemented using the Protobuf communication patterns because
-- we need to deal with metadata.
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#unarycall>
handle :: Call UnaryCall -> IO ()
handle call = do
    -- Send initial metadata, hold on the trailers
    trailers <- constructResponseMetadata call

    -- Wait for the request
    (inboundMeta, request) <- do
      streamElem <- recvInputWithMeta call
      case StreamElem.value streamElem of
        Nothing -> fail "Expected element"
        Just x  -> return x

    -- Check compression of the input
    -- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#client_compressed_unary>
    let expectCompressed :: Bool
        expectCompressed = maybe False (^. #value) (request ^. #expectCompressed)
    checkInboundCompression expectCompressed inboundMeta

    -- Send response
    payload <- payloadOfType (request ^. #responseType) (request ^. #responseSize)

    let outboundMeta :: OutboundMeta
        outboundMeta = def {
              outboundEnableCompression =
                maybe False (^. #value) (request ^. #responseCompressed)
            }

        response :: Proto SimpleResponse
        response = (mempty) & #payload .~ Just (getProto payload)

    sendOutputWithMeta call $ StreamElem (outboundMeta, response)

    -- Send status and trailers
    echoStatus (request ^. #responseStatus)

    -- If the request includes ORCA per-query metrics, attach them to the
    -- trailers via the endpoint-load-metrics-bin header.
    let mOrcaReport :: Maybe OrcaLoadReport
        mOrcaReport = testOrcaToLoadReport <$> (request ^. #orcaPerQueryReport)

    sendTrailersWithOrca call trailers mOrcaReport
