{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Payments.Domain
Description : Pure projections between the wire contracts.

This is where the actual business logic lives, and deliberately so: it is
all /pure/ functions over the generated protobuf records. The gRPC server
and the Kafka Streams topology are thin shells that move bytes around and
call into here.

  * 'transactionEventFromRequest' — the gRPC front door turns an inbound
    'PaymentRequest' into the canonical 'TransactionEvent' that gets
    appended to the event log.
  * 'eventToRiskFeature' — the risk-engine projection.
  * 'eventToBookkeepingEntry' — the bookkeeping-product projection.

Both projections are total functions of a single 'TransactionEvent', which
is what makes the event log replayable: rebuild either view from scratch by
folding the stream.
-}
module Payments.Domain (
  -- * Tunables
  highValueThresholdMinor,
  minorUnitsPerMajor,

  -- * Projections
  transactionEventFromRequest,
  eventToRiskFeature,
  eventToBookkeepingEntry,

  -- * Keys
  riskKeyForEvent,
) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Proto.Google.Protobuf.Timestamp (Timestamp (..), defaultTimestamp)
import Proto.Payments


{- | Transactions at or above this amount (in minor units) are flagged as
high-value by the risk projection. 100_000 minor units == 1,000.00 major.
-}
highValueThresholdMinor :: Int64
highValueThresholdMinor = 100_000


{- | Minor units per major unit (cents per dollar). Used to render the
risk feature's @amount_major@.
-}
minorUnitsPerMajor :: Double
minorUnitsPerMajor = 100


{- | Build the canonical event from an inbound request. The caller supplies
the freshly-minted transaction id and the event timestamp (both effects
live in the server, keeping this function pure and testable).
-}
transactionEventFromRequest :: Text -> Int64 -> PaymentRequest -> TransactionEvent
transactionEventFromRequest transactionId occurredAtMillis req =
  defaultTransactionEvent
    { transactionEventTransactionId = transactionId
    , transactionEventIdempotencyKey = paymentRequestIdempotencyKey req
    , transactionEventPayerAccount = paymentRequestPayerAccount req
    , transactionEventPayeeAccount = paymentRequestPayeeAccount req
    , transactionEventAmountMinor = paymentRequestAmountMinor req
    , transactionEventCurrency = paymentRequestCurrency req
    , transactionEventOccurredAtMillis = occurredAtMillis
    , transactionEventType = paymentRequestType req
    , transactionEventDescription = paymentRequestDescription req
    , -- Carry the Duration through untouched, and stamp the event with a
      -- Timestamp derived from the same epoch-millis the rest of the demo uses.
      transactionEventAuthorizationWindow = paymentRequestAuthorizationWindow req
    , transactionEventReceivedAt = Just (millisToTimestamp occurredAtMillis)
    }


-- | Convert epoch-millis into a @google.protobuf.Timestamp@.
millisToTimestamp :: Int64 -> Timestamp
millisToTimestamp millis =
  defaultTimestamp
    { timestampSeconds = millis `div` 1000
    , timestampNanos = fromIntegral ((millis `mod` 1000) * 1_000_000)
    }


{- | The account the risk engine assesses: the account losing money. For a
payment that is the payer; for a refund it is the payee.
-}
riskKeyForEvent :: TransactionEvent -> Text
riskKeyForEvent ev =
  case transactionEventType ev of
    TransactionType'TransactionTypeRefund -> transactionEventPayeeAccount ev
    _ -> transactionEventPayerAccount ev


{- | Flatten an event into a numeric risk feature keyed by the assessed
account.
-}
eventToRiskFeature :: TransactionEvent -> RiskFeature
eventToRiskFeature ev =
  defaultRiskFeature
    { riskFeatureAccount = riskKeyForEvent ev
    , riskFeatureTransactionId = transactionEventTransactionId ev
    , riskFeatureAmountMajor = fromIntegral (transactionEventAmountMinor ev) / minorUnitsPerMajor
    , riskFeatureCurrency = transactionEventCurrency ev
    , riskFeatureIsHighValue = transactionEventAmountMinor ev >= highValueThresholdMinor
    , riskFeatureIsOutbound = isOutbound
    , riskFeatureObservedAtMillis = transactionEventOccurredAtMillis ev
    }
  where
    isOutbound = case transactionEventType ev of
      TransactionType'TransactionTypeRefund -> False
      _ -> True


{- | Turn an event into a single ledger posting for the bookkeeping product.
A payment debits the payee and credits the payer; a refund reverses the
two. (The exact debit/credit convention does not matter for the demo —
the point is that this is a /different, domain-owned shape/ derived from
the same event.)
-}
eventToBookkeepingEntry :: TransactionEvent -> BookkeepingEntry
eventToBookkeepingEntry ev =
  defaultBookkeepingEntry
    { bookkeepingEntryEntryId = transactionEventTransactionId ev <> "-entry"
    , bookkeepingEntryTransactionId = transactionEventTransactionId ev
    , bookkeepingEntryDebitAccount = debit
    , bookkeepingEntryCreditAccount = credit
    , bookkeepingEntryAmountMinor = transactionEventAmountMinor ev
    , bookkeepingEntryCurrency = transactionEventCurrency ev
    , bookkeepingEntryPostedAtMillis = transactionEventOccurredAtMillis ev
    , bookkeepingEntryMemo = memo
    }
  where
    (debit, credit) = case transactionEventType ev of
      TransactionType'TransactionTypeRefund ->
        (transactionEventPayerAccount ev, transactionEventPayeeAccount ev)
      _ ->
        (transactionEventPayeeAccount ev, transactionEventPayerAccount ev)
    memo =
      let d = transactionEventDescription ev
      in if T.null d then "payment " <> transactionEventTransactionId ev else d
