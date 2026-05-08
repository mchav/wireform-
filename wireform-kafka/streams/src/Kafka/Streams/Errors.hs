{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
  , TaskMigratedException (..)
  , StreamsNotStartedException (..)
  , ProducerFencedException (..)
    -- * Handlers
  , DeserializationHandler (..)
  , DeserializationResponse (..)
  , ProductionHandler (..)
  , ProductionResponse (..)
  , logAndContinue
  , logAndFail
  ) where

import Control.Exception (Exception)
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
  { deserTopic     :: !Text
  , deserPartition :: !Int32
  , deserOffset    :: !Int
  , deserKey       :: !(Maybe ByteString)
  , deserValue     :: !ByteString
  , deserReason    :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)

-- | Sink-level production failed.
data ProductionException = ProductionException
  { prodTopic  :: !Text
  , prodReason :: !Text
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
