{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Payments.Streams
-- Description : The Kafka Streams topology: event log -> two derived views.
--
-- This is the event-sourcing fan-out. A single source — the
-- 'transactionsTopic' carrying 'TransactionEvent's — is forked into two
-- independent projections:
--
-- @
--                                 ┌──► mapValues eventToRiskFeature
--                                 │      ► re-key by assessed account
--                                 │      ► sink  payments.risk-features
--   source payments.transactions ─┤
--                                 │
--                                 └──► mapValues eventToBookkeepingEntry
--                                        ► re-key by transaction id
--                                        ► sink  payments.bookkeeping-entries
-- @
--
-- The fork is the free-arrow @(&&&)@: it feeds the same upstream stream into
-- both halves. Each half ends in a sink, so the combined output is @((),())@,
-- which we collapse to @()@ with @arr (const ())@. The whole thing is a pure
-- 'F.Topology' value — inspectable and optimisable before it is compiled into
-- a runnable graph.
module Payments.Streams
  ( paymentsTopology
  , buildPaymentsTopology
  , riskBranch
  , bookkeepingBranch
  ) where

import Control.Arrow (arr, (&&&))
import Control.Category ((>>>))
import Data.Text (Text)
import Data.Void (Void)

import Kafka.Streams (KStream, recordValue)
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Topology.Free as F

import Proto.Payments
import Payments.Domain (eventToBookkeepingEntry, eventToRiskFeature)
import Payments.Serdes (bookkeepingTopic, riskFeaturesTopic, transactionsTopic)
import Payments.Serdes ()

-- | The full topology, rooted at the transactions event log.
paymentsTopology :: F.Topology Void ()
paymentsTopology =
  F.source @Text @TransactionEvent transactionsTopic
    >>> (riskBranch &&& bookkeepingBranch)
    >>> arr (const ())

-- | Risk-engine branch: project each event to a 'RiskFeature', re-key by the
-- assessed account so the risk store is partitioned per account, and sink.
riskBranch :: F.Topology (KStream Text TransactionEvent) ()
riskBranch =
  F.mapValues eventToRiskFeature
    >>> F.selectKey (\r -> riskFeatureAccount (recordValue r))
    >>> F.sink riskFeaturesTopic

-- | Bookkeeping branch: project each event to a 'BookkeepingEntry', re-key by
-- transaction id, and sink.
bookkeepingBranch :: F.Topology (KStream Text TransactionEvent) ()
bookkeepingBranch =
  F.mapValues eventToBookkeepingEntry
    >>> F.selectKey (\r -> bookkeepingEntryTransactionId (recordValue r))
    >>> F.sink bookkeepingTopic

-- | Compile the AST into a runnable 'Topo.Topology' graph.
buildPaymentsTopology :: IO Topo.Topology
buildPaymentsTopology = F.buildTopologyFrom paymentsTopology
