{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Payments.Demo
-- Description : Run the whole pipeline in-process, no broker required.
--
-- This drives 'paymentsTopology' through the in-process
-- 'Kafka.Streams.Driver.TopologyTestDriver'. It synthesises a handful of
-- 'TransactionEvent's (as if a gRPC 'CreatePayment' call had appended them to
-- the log), pipes their protobuf bytes into the @transactions@ source topic,
-- then drains and pretty-prints the two derived views the topology produced.
--
-- It is the fastest way to /see/ the event-sourcing fan-out work end to end
-- without standing up Kafka.
module Payments.Demo
  ( runDemo
  , sampleEvents
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Kafka.Streams
  ( CollectedRecord
  , Timestamp (..)
  , closeDriver
  , crValue
  , newDriver
  , pipeInput
  , readOutput
  , topicName
  )
import Kafka.Serde.Proto (decodeProto, encodeProto)

import Proto.Lens ((&), (.~), (^.))
import Proto.Payments
import Proto.Google.Protobuf.Duration (durationSeconds, defaultDuration)
import Proto.Google.Protobuf.Timestamp (timestampSeconds)
import Payments.Domain (transactionEventFromRequest)
import Payments.Serdes (bookkeepingTopic, riskFeaturesTopic, transactionsTopic)
import Payments.Streams (buildPaymentsTopology)

runDemo :: IO ()
runDemo = do
  putStrLn "=== Payments pipeline — in-memory (TopologyTestDriver) ==="
  putStrLn ""
  topo <- buildPaymentsTopology
  driver <- newDriver topo "payments-demo-app"

  putStrLn ("Appending " <> show (length sampleEvents) <> " transaction events to "
            <> T.unpack transactionsTopic <> ":")
  mapM_ (\ev -> putStrLn ("  + " <> describeEvent ev)) sampleEvents
  mapM_ (feed driver) (zip [0 ..] sampleEvents)

  putStrLn ""
  putStrLn ("Risk features on " <> T.unpack riskFeaturesTopic <> ":")
  riskOut <- readOutput driver (topicName riskFeaturesTopic)
  mapM_ printRisk riskOut

  putStrLn ""
  putStrLn ("Bookkeeping entries on " <> T.unpack bookkeepingTopic <> ":")
  bookOut <- readOutput driver (topicName bookkeepingTopic)
  mapM_ printEntry bookOut

  closeDriver driver
  where
    feed driver (i, ev) =
      pipeInput
        driver
        (topicName transactionsTopic)
        (Just (TE.encodeUtf8 (ev ^. #payerAccount)))
        (encodeProto ev)
        (Timestamp (baseTs + i))
        0

-- | A small, hand-rolled batch of events exercising payments, a refund, and
-- the high-value threshold.
sampleEvents :: [TransactionEvent]
sampleEvents =
  [ event "txn-1001" "acct-alice" "acct-merchant" 4_999 paymentT 0 "coffee subscription"
  , event "txn-1002" "acct-bob"   "acct-merchant" 250_000 paymentT 1 "laptop"
  , event "txn-1003" "acct-alice" "acct-merchant" 120_000 paymentT 2 "annual membership"
  , event "txn-1004" "acct-merchant" "acct-bob"   250_000 refundT 3 "laptop refund"
  ]
  where
    paymentT = TransactionType'TransactionTypePayment
    refundT = TransactionType'TransactionTypeRefund
    event txnId payer payee amount ty offset desc =
      transactionEventFromRequest txnId (baseTs + offset) $
        mempty
          & #idempotencyKey .~ (txnId <> "-idem")
          & #payerAccount .~ payer
          & #payeeAccount .~ payee
          & #amountMinor .~ amount
          & #currency .~ "USD"
          & #type .~ ty
          & #description .~ desc
          -- A google.protobuf.Duration: a 10-minute authorization window.
          & #authorizationWindow .~ Just (defaultDuration {durationSeconds = 600})

baseTs :: Int64
baseTs = 1_700_000_000_000

----------------------------------------------------------------------
-- Pretty printers
----------------------------------------------------------------------

describeEvent :: TransactionEvent -> String
describeEvent ev =
  T.unpack (ev ^. #transactionId)
    <> ": " <> T.unpack (ev ^. #payerAccount)
    <> " -> " <> T.unpack (ev ^. #payeeAccount)
    <> " " <> show (ev ^. #amountMinor) <> " " <> T.unpack (ev ^. #currency)
    <> " (" <> showType (ev ^. #type) <> ")"
    <> ", auth-window=" <> showWindow (ev ^. #authorizationWindow)
    <> ", received@" <> showStamp (ev ^. #receivedAt)
  where
    showWindow = maybe "none" (\d -> show (durationSeconds d) <> "s")
    showStamp = maybe "?" (\t -> show (timestampSeconds t) <> "s")

showType :: TransactionType -> String
showType = \case
  TransactionType'TransactionTypePayment -> "payment"
  TransactionType'TransactionTypeRefund -> "refund"
  _ -> "unspecified"

printRisk :: CollectedRecord -> IO ()
printRisk cr = case decodeProto (crValue cr) :: Either String RiskFeature of
  Left err -> putStrLn ("  <undecodable risk feature: " <> err <> ">")
  Right rf ->
    putStrLn $
      "  " <> T.unpack (rf ^. #account)
        <> " | " <> T.unpack (rf ^. #transactionId)
        <> " | $" <> show (rf ^. #amountMajor)
        <> (if rf ^. #isHighValue then " | HIGH-VALUE" else "")
        <> (if rf ^. #isOutbound then " | outbound" else " | inbound")

printEntry :: CollectedRecord -> IO ()
printEntry cr = case decodeProto (crValue cr) :: Either String BookkeepingEntry of
  Left err -> putStrLn ("  <undecodable bookkeeping entry: " <> err <> ">")
  Right be ->
    putStrLn $
      "  " <> T.unpack (be ^. #entryId)
        <> " | debit " <> T.unpack (be ^. #debitAccount)
        <> " | credit " <> T.unpack (be ^. #creditAccount)
        <> " | " <> show (be ^. #amountMinor) <> " " <> T.unpack (be ^. #currency)
        <> " | " <> T.unpack (be ^. #memo)
