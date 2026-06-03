{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Payments.Serdes
-- Description : 'HasSerde' instances + topic names for the payments demo.
--
-- The Kafka Streams free-arrow DSL resolves a 'Serde' at every type-changing
-- operator through the 'HasSerde' class. We give each generated protobuf
-- message a 'HasSerde' instance backed by 'protoSerde' (the wireform-proto
-- codec) so the topology reads cleanly without threading explicit serdes
-- through every node. These are orphan instances, which is fine for an
-- example app that owns both the types and the topology.
module Payments.Serdes
  ( -- * Topic names
    transactionsTopic
  , riskFeaturesTopic
  , bookkeepingTopic
  ) where

import Data.Text (Text)

import Kafka.Serde (HasSerde (..))
import Kafka.Serde.Proto (protoSerde)

import Proto.Payments

instance HasSerde TransactionEvent where
  serde = protoSerde

instance HasSerde RiskFeature where
  serde = protoSerde

instance HasSerde BookkeepingEntry where
  serde = protoSerde

-- | The event log: every accepted payment is appended here as a
-- 'TransactionEvent'.
transactionsTopic :: Text
transactionsTopic = "payments.transactions"

-- | Risk-engine projection output.
riskFeaturesTopic :: Text
riskFeaturesTopic = "payments.risk-features"

-- | Bookkeeping projection output.
bookkeepingTopic :: Text
bookkeepingTopic = "payments.bookkeeping-entries"
