{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- |
-- Module      : Kafka.Streams.Errors
-- Description : Exception hierarchy for Kafka Streams
--
-- Mirrors the exception family in @org.apache.kafka.streams.errors@:
--
--   * 'StreamsException'           — top-level
--   * 'TopologyException'          — invalid topology configuration
--   * 'StreamsNotStartedException' — operation requires a running runtime
--   * 'TaskMigratedException'      — task fenced off; retry/rebalance
--   * 'ProcessorStateException'    — state store is in an unusable state
--   * 'InvalidStateStoreException' — store missing or wrong type
--   * 'DeserializationException'   — serde failed at the source
--   * 'ProducerFencedException'    — EOS producer fenced
--
-- The 'DeserializationHandler' / 'ProductionHandler' types let user
-- code decide whether to fail-fast, log-and-continue, or send to a
-- dead-letter topic.
module Kafka.Streams.Errors
  ( StreamsException (..)
  , TopologyException (..)
  , ProcessorStateException (..)
  , InvalidStateStoreException (..)
  , DeserializationException (..)
  , ProductionException (..)
  , ProcessingException (..)
  , TaskMigratedException (..)
  , StreamsNotStartedException (..)
  , ProducerFencedException (..)
    -- * Deserialization handler (KIP-161)
  , DeserializationHandler (..)
  , DeserializationResponse (..)
  , logAndContinue
  , logAndFail
    -- * Production handler (KIP-280)
  , ProductionHandler (..)
  , ProductionResponse (..)
  , logAndContinueProduction
  , logAndFailProduction
    -- * Processing handler (KIP-1033)
  , ProcessingExceptionHandler (..)
  , ProcessingResponse (..)
  , logAndContinueProcessing
  , logAndFailProcessing
    -- * Uncaught exception handler (KIP-671)
  , StreamsUncaughtExceptionHandler (..)
  , UncaughtExceptionResponse (..)
  , replaceThreadOnException
  , shutdownClientOnException
  , shutdownApplicationOnException
  ) where

import Control.Exception (Exception, SomeException)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Top-level streams exception.
data StreamsException = StreamsException
  { streamsErrorMessage :: !Text
  , streamsErrorContext :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | Bad topology — cycles, dangling source, missing store, etc.
newtype TopologyException = TopologyException Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | State store is unusable (corrupt, closed mid-operation).
newtype ProcessorStateException = ProcessorStateException Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | Store missing or wrong type.
newtype InvalidStateStoreException = InvalidStateStoreException Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | Source-level deserialisation failed.
data DeserializationException = DeserializationException
  { topic     :: !Text
  , partition :: !Int32
  , offset    :: !Int
  , key       :: !(Maybe ByteString)
  , value     :: !ByteString
  , reason    :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | Sink-level production failed.
data ProductionException = ProductionException
  { topic  :: !Text
  , reason :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | Task migrated — caller should rebalance.
newtype TaskMigratedException = TaskMigratedException Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | Operation needs the runtime running.
data StreamsNotStartedException = StreamsNotStartedException
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | EOS producer was fenced (newer epoch claimed the txn id).
newtype ProducerFencedException = ProducerFencedException Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | What to do after a deserialisation failure.
data DeserializationResponse
  = DeserContinueProcessing
  | DeserFailFast
  deriving stock (Eq, Show, Generic)

-- | User-supplied deserialisation handler.
newtype DeserializationHandler = DeserializationHandler
  { runDeserializationHandler :: DeserializationException -> IO DeserializationResponse
  }

-- | What to do after a production failure.
data ProductionResponse
  = ProdContinueProcessing
  | ProdFailFast
  deriving stock (Eq, Show, Generic)

-- | User-supplied production handler.
newtype ProductionHandler = ProductionHandler
  { runProductionHandler :: ProductionException -> IO ProductionResponse
  }

-- | Continue on deserialisation failure (matches Kafka's @LogAndContinueExceptionHandler@).
logAndContinue :: DeserializationHandler
logAndContinue = DeserializationHandler $ \_ -> pure DeserContinueProcessing

-- | Fail-fast on deserialisation failure (matches Kafka's @LogAndFailExceptionHandler@).
logAndFail :: DeserializationHandler
logAndFail = DeserializationHandler $ \_ -> pure DeserFailFast

-- | Continue on production failure (KIP-280
-- @DefaultProductionExceptionHandler@).
logAndContinueProduction :: ProductionHandler
logAndContinueProduction =
  ProductionHandler $ \_ -> pure ProdContinueProcessing

-- | Fail-fast on production failure.
logAndFailProduction :: ProductionHandler
logAndFailProduction =
  ProductionHandler $ \_ -> pure ProdFailFast

----------------------------------------------------------------------
-- KIP-1033: ProcessingExceptionHandler
----------------------------------------------------------------------

-- | An exception raised inside a 'Processor' during normal record
-- processing (not source-side deserialisation, not sink-side
-- production). Carries enough context for a handler to log /
-- route to a DLQ / decide whether to fail the stream thread.
data ProcessingException = ProcessingException
  { topic     :: !Text
  , partition :: !Int32
  , offset    :: !Int
  , node      :: !Text
    -- ^ Topology node-name where the exception was raised.
  , reason    :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | Whether to continue running after a processing exception or
-- propagate it to the uncaught-exception handler.
data ProcessingResponse
  = ProcessingContinue
  | ProcessingFail
  deriving stock (Eq, Show, Generic)

-- | User-supplied processing handler (KIP-1033).
newtype ProcessingExceptionHandler = ProcessingExceptionHandler
  { runProcessingExceptionHandler
      :: ProcessingException -> IO ProcessingResponse
  }

-- | Continue on processing failure. Matches Kafka's
-- @LogAndContinueProcessingExceptionHandler@.
logAndContinueProcessing :: ProcessingExceptionHandler
logAndContinueProcessing =
  ProcessingExceptionHandler $ \_ -> pure ProcessingContinue

-- | Fail-fast on processing failure. Matches Kafka's
-- @LogAndFailProcessingExceptionHandler@.
logAndFailProcessing :: ProcessingExceptionHandler
logAndFailProcessing =
  ProcessingExceptionHandler $ \_ -> pure ProcessingFail

----------------------------------------------------------------------
-- KIP-671: StreamsUncaughtExceptionHandler
----------------------------------------------------------------------

-- | What to do when a stream-thread's event-loop dies with an
-- exception the per-record handlers didn't catch. Mirrors
-- Java's @StreamThreadExceptionResponse@.
data UncaughtExceptionResponse
  = ReplaceThread
    -- ^ Spawn a fresh stream-thread to take over the
    --   assignment; the application keeps running.
  | ShutdownClient
    -- ^ Tear this @KafkaStreams@ instance down cleanly. The
    --   rest of the cluster keeps going.
  | ShutdownApplication
    -- ^ Tear every instance of the application down (this one
    --   plus the rest of the consumer group). Used when the
    --   error is a global invariant violation.
  deriving stock (Eq, Show, Generic)

-- | User-supplied handler called when a stream-thread dies.
newtype StreamsUncaughtExceptionHandler = StreamsUncaughtExceptionHandler
  { runStreamsUncaughtExceptionHandler
      :: SomeException -> IO UncaughtExceptionResponse
  }

-- | Default: respawn the thread on failure. Matches Java's
-- @StreamThreadExceptionResponse.REPLACE_THREAD@.
replaceThreadOnException :: StreamsUncaughtExceptionHandler
replaceThreadOnException =
  StreamsUncaughtExceptionHandler $ \_ -> pure ReplaceThread

-- | Tear this instance down on failure.
shutdownClientOnException :: StreamsUncaughtExceptionHandler
shutdownClientOnException =
  StreamsUncaughtExceptionHandler $ \_ -> pure ShutdownClient

-- | Tear the whole application down on failure.
shutdownApplicationOnException :: StreamsUncaughtExceptionHandler
shutdownApplicationOnException =
  StreamsUncaughtExceptionHandler $ \_ -> pure ShutdownApplication
