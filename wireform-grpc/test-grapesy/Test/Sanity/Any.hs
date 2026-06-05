{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Sanity.Any (tests) where

import Control.Exception
import Data.Vector qualified as V
import Test.Syd

import Network.GRPC.Client (rpc)
import Network.GRPC.Client.StreamType.IO qualified as Client
import Network.GRPC.Common
import Test.Util.Exception
import Network.GRPC.Common.Protobuf
import Network.GRPC.Common.Protobuf.Any (Any)
import Network.GRPC.Common.Protobuf.Any qualified as Any
import Network.GRPC.Server.StreamType qualified as Server

import Test.Driver.ClientServer

import Proto.API.Ping
import Proto.API.TestAny

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

tests :: Spec
tests = describe "Test.Sanity.Any" $ sequence_ [
      it "Any"    testAny
    , it "Status" testStatus
    ]

{-------------------------------------------------------------------------------
  Using the 'Any' wrapper
-------------------------------------------------------------------------------}

testAny :: IO ()
testAny = testClientServer $ ClientServerTest {
      config = def
    , server = [Server.fromMethod @Reverse $ Server.mkNonStreaming handler]
    , client = simpleTestClient $ \conn -> do
        let req, expected :: Proto TestAnyMsg
            req      = withDetails [Any.pack detail1, Any.pack detail2]
            expected = withDetails [Any.pack detail2, Any.pack detail1]

        resp <- Client.nonStreaming conn (rpc @Reverse) req
        resp `shouldBe` expected
    }
  where
    handler :: Proto TestAnyMsg -> IO (Proto TestAnyMsg)
    handler (Proto msg) = return $ Proto msg { testAnyMsgDetails = V.reverse (testAnyMsgDetails msg) }

    detail1 :: A
    detail1 = (getProto mempty) { aA = 1 }

    detail2 :: B
    detail2 = (getProto mempty) { bB = 1 }

    withDetails :: [Any] -> Proto TestAnyMsg
    withDetails details =
        Proto (getProto mempty) { testAnyMsgMessage = "foo", testAnyMsgDetails = V.fromList details }

{-------------------------------------------------------------------------------
  Protobuf-specific error details (which relies on 'Any')
-------------------------------------------------------------------------------}

testStatus :: IO ()
testStatus = testClientServer $ ClientServerTest {
      config = def {
          isExpectedServerException = \(WrapExactException e) ->
            case fromException e of
              Just err'  -> grpcError err' == GrpcNotFound
              _otherwise -> False
        }
    , server = [
          Server.fromMethod @Ping $ Server.mkNonStreaming $ \_req ->
            throwProtobufErrorHom protobufError
        ]
    , client = simpleTestClient $ \conn -> do
        mResp :: Either GrpcException (Proto PongMessage) <-
          try $ Client.nonStreaming conn (rpc @Ping) (mempty)
        case mResp of
          Right _   -> expectationFailure "Expected exception"
          Left  err ->
            toProtobufErrorHom err `shouldBe` (Right protobufError)
    }
  where
    protobufError :: ProtobufError A
    protobufError = ProtobufError {
          protobufErrorCode    = GrpcNotFound
        , protobufErrorMessage = Just "Not found"
        , protobufErrorDetails = [detail1, detail2]
        }

    detail1, detail2 :: A
    detail1 = getProto $ (mempty :: Proto A) & #a .~ 1
    detail2 = getProto $ (mempty :: Proto A) & #a .~ 2
