{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.ProducerExtras
Description : Producer ergonomics: transactional-id helpers,
              txn-error classification, bounded txn deadlines,
              enhanced ack callbacks

Pure helpers and small typed knobs that round out the core
'Kafka.Client.Producer'. Each entry is either a configuration
type or a decision helper that user code can consume without
touching the producer's mutable state:

  * 'transactionalIdOptional' — pick a @transactional.id@ from
    an optional override + an application prefix + a per-process
    suffix. Useful for rolling deployments where the
    transactional id is generated lazily.
  * 'TxnErrorRecovery' / 'classifyTxnError' — classify a
    broker error code into abort-the-txn, retry, or fatal. Lets
    callers do recovery without a hand-written switch.
  * 'TxnDeadline' / 'effectiveTxnDeadlineMs' — bounded
    commit/abort deadlines so a stuck coordinator can't pin the
    producer open during shutdown.
  * 'EnhancedCallback' — consistent ack-callback record
    (success + failure paths), with a dispatcher and a no-op
    constructor.
-}
module Kafka.Client.ProducerExtras
  ( -- * Optional transactional id
    transactionalIdOptional
    -- * Transactional-error classification
  , TxnErrorRecovery (..)
  , classifyTxnError
    -- * Bounded transactional-op deadlines
  , TxnDeadline (..)
  , effectiveTxnDeadlineMs
    -- * Enhanced ack callbacks
  , EnhancedCallback (..)
  , noopEnhancedCallback
  , dispatchEnhanced
  ) where

import Control.Exception (SomeException, try)
import Data.Int (Int16, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Kafka.Client.Producer as KP
import qualified Kafka.Client.RecordMetadata as RM
import qualified Kafka.Client.RetryClassifier as RC

----------------------------------------------------------------------
-- Optional transactional id
----------------------------------------------------------------------

-- | Optionally derive a @transactional.id@ from a base prefix +
-- a per-process suffix. Keeps the user-facing API symmetric
-- with the JVM client, where @transactional.id@ may be left
-- blank and the client picks one based on host/process ids.
transactionalIdOptional
  :: Maybe Text          -- ^ explicit override (matches the JVM client's @transactional.id@ property)
  -> Text                -- ^ application prefix
  -> Text                -- ^ per-process suffix (e.g. host\@pid)
  -> Text
transactionalIdOptional (Just t) _ _      = t
transactionalIdOptional Nothing prefix sf = prefix <> "-" <> sf

----------------------------------------------------------------------
-- Txn-error recovery
----------------------------------------------------------------------

data TxnErrorRecovery
  = TxnRecoverByAbort
    -- ^ Abort the current transaction and let the producer
    --   continue with the next.
  | TxnRecoverByRetry
    -- ^ Re-issue the same operation after a short backoff.
  | TxnRecoverFatal
    -- ^ Producer must close.
  deriving stock (Eq, Show, Generic)

classifyTxnError :: Int16 -> TxnErrorRecovery
classifyTxnError code = case RC.classify code of
  RC.ECNoError    -> TxnRecoverByRetry
  RC.ECRetriable  -> TxnRecoverByRetry
  RC.ECAbortable  -> TxnRecoverByAbort
  RC.ECFatal      -> TxnRecoverFatal

----------------------------------------------------------------------
-- Bounded txn-op deadlines
----------------------------------------------------------------------

-- | A deadline supplied to @commitTransaction@ /
-- @abortTransaction@. Callers can bound the wait so a
-- misbehaving coordinator can't pin the producer open during
-- shutdown.
data TxnDeadline
  = TxnUseProducerDefault       -- ^ Fall back to the producer's @transaction.timeout.ms@.
  | TxnDeadlineMs !Int          -- ^ Hard upper bound in ms.
  deriving stock (Eq, Show, Generic)

effectiveTxnDeadlineMs
  :: Int64                      -- ^ now (ms)
  -> Int                        -- ^ producer's @transaction.timeout.ms@
  -> TxnDeadline
  -> Int64
effectiveTxnDeadlineMs now defaultMs = \case
  TxnUseProducerDefault -> now + fromIntegral defaultMs
  TxnDeadlineMs ms      -> now + fromIntegral ms

----------------------------------------------------------------------
-- Enhanced ack callbacks
----------------------------------------------------------------------

-- | The enhanced producer-callback shape. Each JVM 3.x producer
-- hook receives the same @Either ProducerError RecordMetadata@
-- outcome at every stage of the send pipeline (enqueue, send,
-- ack, retry, delivered).
data EnhancedCallback = EnhancedCallback
  { ecOnEnqueue   :: !(KP.ProducerRecord -> IO ())
  , ecOnSend      :: !(KP.ProducerRecord -> IO ())
  , ecOnAck       :: !(KP.ProducerRecord
                       -> Either RM.ProducerError KP.RecordMetadata
                       -> IO ())
  , ecOnRetry     :: !(KP.ProducerRecord -> Int -> IO ())
  , ecOnDelivered :: !(KP.RecordMetadata -> IO ())
  }

noopEnhancedCallback :: EnhancedCallback
noopEnhancedCallback = EnhancedCallback
  { ecOnEnqueue   = \_   -> pure ()
  , ecOnSend      = \_   -> pure ()
  , ecOnAck       = \_ _ -> pure ()
  , ecOnRetry     = \_ _ -> pure ()
  , ecOnDelivered = \_   -> pure ()
  }

-- | Dispatch a single ack outcome through the enhanced
-- callback, swallowing any exception so the sender thread
-- can't be torn down by a buggy hook.
dispatchEnhanced
  :: EnhancedCallback
  -> KP.ProducerRecord
  -> Either RM.ProducerError KP.RecordMetadata
  -> IO ()
dispatchEnhanced ec rec_ outcome = do
  r <- try (ecOnAck ec rec_ outcome) :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left _   -> pure ()
