module Interop.Server.TestService.FullDuplexCall (handle) where

import Control.Concurrent.STM

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server
import Network.GRPC.Spec (OrcaLoadReport)

import Interop.Server.Common
import Interop.Server.TestService.StreamingOutputCall qualified as StreamingOutputCall
import Interop.Util.Messages

import Proto.API.Interop

-- | Handle @TestService.FullDuplexCall@
--
-- <https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md#fullduplexcall>
handle :: TVar (Maybe OrcaLoadReport) -> Call FullDuplexCall -> IO ()
handle oobState call = do
    trailers <- constructResponseMetadata call

    let handleRequest :: Proto StreamingOutputCallRequest -> IO ()
        handleRequest request = do
            -- If orca_oob_report is set, update the shared OOB state
            case request ^. #orcaOobReport of
              Nothing     -> return ()
              Just report ->
                atomically $ writeTVar oobState (Just (testOrcaToLoadReport report))

            StreamingOutputCall.handleRequest call request
            echoStatus (request ^. #responseStatus)

        loop :: IO ()
        loop = do
            streamElem <- recvInput call
            case streamElem of
              StreamElem  r   -> handleRequest r >> loop
              FinalElem   r _ -> handleRequest r
              NoMoreElems   _ -> return ()

    loop

    -- Include the current OOB report in the stream trailers
    mOob <- atomically $ readTVar oobState
    sendTrailersWithOrca call trailers mOob
