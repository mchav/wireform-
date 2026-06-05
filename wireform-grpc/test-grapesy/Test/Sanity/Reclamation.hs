module Test.Sanity.Reclamation (tests) where

import Control.Exception
import Control.Monad
import Test.Syd

import Network.GRPC.Client qualified as Client
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Server qualified as Server

import Test.Driver.ClientServer

import Proto.API.Ping

tests :: Spec
tests = describe "Test.Sanity.Reclamation" $ sequence_ [
      it "serverException1" serverException1
    , it "serverException2" serverException2
    ]

{-------------------------------------------------------------------------------
  Server-side exception

  Test for <https://github.com/well-typed/grapesy/issues/257>.
-------------------------------------------------------------------------------}

-- | Handler that throws immediately
brokenHandler :: Server.Call Ping -> IO ()
brokenHandler _call = throwIO $ DeliberateServerException 1

serverException1 :: IO ()
serverException1 = testClientServer $ ClientServerTest {
      config = def { isExpectedServerException = isDeliberateException }
    , server = [Server.someRpcHandler $ Server.mkRpcHandler brokenHandler]
    , client = \params testServer delimitTestScope -> delimitTestScope $
        replicateM_ 1000 $ do
          Client.withConnection params testServer $ \conn ->
            Client.withRPC conn def (Proxy @Ping) $ \call -> do
              resp <- try $ Client.recvFinalOutput call
              case resp of
                Left GrpcException{} -> return ()
                Right _ -> expectationFailure "Unexpected response"
    }

serverException2 :: IO ()
serverException2 = testClientServer $ ClientServerTest {
      config = def { isExpectedServerException = isDeliberateException }
    , server = [Server.someRpcHandler $ Server.mkRpcHandler brokenHandler]
    , client = \params testServer delimitTestScope -> delimitTestScope $
        replicateM_ 1000 $
          Client.withConnection params testServer $ \conn ->
            Client.withRPC conn def (Proxy @Ping) $ \call -> do
              resp <- try $ do
                -- The only difference between 'serverException1' is this call
                -- to 'sendFinalInput'. We will probably get the exception when
                -- we try to /receive/ a message from the server, but we
                -- sometimes already get it when we /send/.
                Client.sendFinalInput call (mempty)
                Client.recvFinalOutput call
              case resp of
                Left GrpcException{} -> return ()
                Right _ -> expectationFailure "Unexpected response"
    }
