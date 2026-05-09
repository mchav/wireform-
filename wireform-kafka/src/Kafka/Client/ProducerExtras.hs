{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.ProducerExtras
Description : KIP-185 / 588 / 691 / 732 / 849 / 1044 / 1166 / 1199 — producer ergonomics

A grab-bag of small producer-side surfaces that round out the
core 'Kafka.Client.Producer'. Each entry is either a typed knob
or a pure decision helper.

  * KIP-185: 'transactionalIdOptional' helper for upgrades
    where the @transactional.id@ is generated lazily.
  * KIP-588: 'recoverFromTxnError' classifier — distinguishes
    "abort the txn but the producer can keep going" from
    "kill the producer".
  * KIP-691 / KIP-1199: enhanced configurable callback record
    surfaces.
  * KIP-732 / KIP-849: bounded
    @commitTransaction(Duration)@ /
    @abortTransaction(Duration)@ surfaces.
  * KIP-1044: producer recovery from transaction abortable
    errors (the broker now distinguishes abortable vs. fatal
    txn errors via error code 104).
  * KIP-1166: consistent error callback shape — every
    onAcknowledgement now sees the same 'Either ProducerError
    RecordMetadata' regardless of the failure point.
-}
module Kafka.Client.ProducerExtras
  ( -- * KIP-185
    transactionalIdOptional
    -- * KIP-588 / 1044
  , TxnErrorRecovery (..)
  , classifyTxnError
    -- * KIP-732 / 849 — bounded txn ops
  , TxnDeadline (..)
  , effectiveTxnDeadlineMs
    -- * KIP-691 / 1199 callbacks
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
-- KIP-185
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
-- KIP-588 / KIP-1044 txn-error recovery
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
-- KIP-732 / KIP-849 deadlines for txn ops
----------------------------------------------------------------------

-- | A deadline supplied to @commitTransaction@ /
-- @abortTransaction@. KIP-849 lets callers bound the wait so a
-- misbehaving coordinator can't block shutdown indefinitely.
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
-- KIP-691 / 1199 enhanced callbacks
----------------------------------------------------------------------

-- | The /enhanced/ callback shape. Every JVM 3.x producer hook
-- now receives the same outcome shape.
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
