{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Payments.Client
Description : Minimal gRPC client that fires one 'CreatePayment' call.

A convenience driver for poking the running server: it opens an insecure
connection, sends a single sample 'PaymentRequest', and prints the
'PaymentResponse'. Useful for smoke-testing the full broker-backed path.
-}
module Payments.Client (
  runClient,
) where

import Network.GRPC.Client
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Proto.API.Payments
import Proto.Google.Protobuf.Duration (defaultDuration, durationSeconds)


-- | Connect to @host:port@ and create one demo payment.
runClient :: String -> Int -> IO ()
runClient host port = do
  let server = ServerInsecure (Address host (fromIntegral port) Nothing)
  withConnection def server $ \conn -> do
    let req :: Proto PaymentRequest
        req =
          Proto $
            defaultPaymentRequest
              { paymentRequestIdempotencyKey = "demo-idem-1"
              , paymentRequestPayerAccount = "acct-alice"
              , paymentRequestPayeeAccount = "acct-merchant"
              , paymentRequestAmountMinor = 150_000
              , paymentRequestCurrency = "USD"
              , paymentRequestType = TransactionType'TransactionTypePayment
              , paymentRequestDescription = "demo payment"
              , paymentRequestAuthorizationWindow = Just (defaultDuration {durationSeconds = 600})
              }
    resp <- nonStreaming conn (rpc @CreatePayment) req
    putStrLn "CreatePayment response:"
    print (getProto resp)
