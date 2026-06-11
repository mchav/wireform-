{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Mock.Fault
Description : Streams-side wrappers for 'Kafka.Client.Mock.Fault'

Re-exports the core fault-injection API with thin 'TopicName'
adapters so existing streams call sites keep working.
-}
module Kafka.Streams.Mock.Fault (
  -- * Errors
  F.MockError (..),
  F.isRetriable,
  F.isFatal,
  F.kafkaErrorText,

  -- * Fault policy
  F.FaultPolicy,
  F.noFaults,
  queueProduceErrors,
  queueFetchErrors,
  F.queueCommitErrors,
  F.queueTxnErrors,
  F.queueTxnBeginErrors,
  F.queueTxnCommitErrors,
  F.queueTxnAbortErrors,
  addProduceFault,
  addFetchFault,
  F.addCommitFault,
  F.addTxnFault,
  F.addTxnBeginFault,
  F.addTxnCommitFault,
  F.addTxnAbortFault,
  F.TxnOp (..),
  F.clearFaults,

  -- * Querying / popping
  takeProduceFault,
  takeFetchFault,
  F.takeCommitFault,
  F.takeTxnFault,
  F.takeTxnFaultFor,

  -- * Permanent (sticky) faults
  setStickyProduce,
  setStickyFetch,
  clearStickyProduce,
  clearStickyFetch,
) where

import Data.Int (Int32)
import Kafka.Client.Mock.Fault qualified as F
import Kafka.Streams.Types (TopicName, unTopicName)


queueProduceErrors
  :: F.FaultPolicy -> TopicName -> Int32 -> [F.MockError] -> IO ()
queueProduceErrors fp t p = F.queueProduceErrors fp (unTopicName t) p


queueFetchErrors
  :: F.FaultPolicy -> TopicName -> Int32 -> [F.MockError] -> IO ()
queueFetchErrors fp t p = F.queueFetchErrors fp (unTopicName t) p


addProduceFault :: F.FaultPolicy -> TopicName -> Int32 -> F.MockError -> IO ()
addProduceFault fp t p = F.addProduceFault fp (unTopicName t) p


addFetchFault :: F.FaultPolicy -> TopicName -> Int32 -> F.MockError -> IO ()
addFetchFault fp t p = F.addFetchFault fp (unTopicName t) p


takeProduceFault
  :: F.FaultPolicy -> TopicName -> Int32 -> IO (Maybe F.MockError)
takeProduceFault fp t p = F.takeProduceFault fp (unTopicName t) p


takeFetchFault
  :: F.FaultPolicy -> TopicName -> Int32 -> IO (Maybe F.MockError)
takeFetchFault fp t p = F.takeFetchFault fp (unTopicName t) p


setStickyProduce
  :: F.FaultPolicy -> TopicName -> Int32 -> F.MockError -> IO ()
setStickyProduce fp t p = F.setStickyProduce fp (unTopicName t) p


setStickyFetch
  :: F.FaultPolicy -> TopicName -> Int32 -> F.MockError -> IO ()
setStickyFetch fp t p = F.setStickyFetch fp (unTopicName t) p


clearStickyProduce :: F.FaultPolicy -> TopicName -> Int32 -> IO ()
clearStickyProduce fp t p = F.clearStickyProduce fp (unTopicName t) p


clearStickyFetch :: F.FaultPolicy -> TopicName -> Int32 -> IO ()
clearStickyFetch fp t p = F.clearStickyFetch fp (unTopicName t) p
