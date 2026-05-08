{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Kafka.Streams.Processor
-- Description : Processor + ProcessorContext + Punctuator
--
-- The processor API is intentionally close to
-- @org.apache.kafka.streams.processor.api@:
--
--   * 'ProcessorContext' is the dependency-injection seam every
--     processor sees (forward, schedule, recordMetadata, getStore).
--   * 'Processor' is a record of three callbacks:
--     'procInit' / 'procProcess' / 'procClose'. We pass parents
--     /typed/ so users do not have to type-cast at process time.
--   * 'Punctuator' lets a processor schedule periodic callbacks driven
--     either by 'StreamTimePunctuation' (advances with stream time,
--     i.e. record timestamps) or 'WallClockTimePunctuation' (advances
--     with the system clock).
module Kafka.Streams.Processor
  ( -- * Processor
    Processor (..)
  , noopProcessor
  , statelessProcessor
    -- * Typed handles to processor identity
  , ProcessorName (..)
  , processorName
    -- * Context
  , ProcessorContext (..)
  , currentRecordMetadata
  , forwardRecord
  , forwardTo
  , schedule
  , taskId
  , applicationIdC
  , streamTimeC
  , wallClockTimeC
  , getStateStore
  , SinkEmit (..)
    -- * Punctuators
  , Punctuator (..)
  , PunctuationType (..)
  , Cancellable (..)
  , cancelled
  , forwardingPunctuator
    -- * Task identifiers
  , TaskId (..)
  , taskIdText
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Kafka.Streams.State.Store (AnyStateStore, StoreName)
import Kafka.Streams.Time (Timestamp)
import qualified Kafka.Streams.Types
import Kafka.Streams.Types (NodeName, Record, RecordMetadata)

-- | Logical processor name. Two processors with the same name belong
-- to the same node in the topology — no two distinct processor
-- instances may share a name.
newtype ProcessorName = ProcessorName { unProcessorName :: Text }
  deriving stock (Eq, Ord, Show, Generic)

processorName :: Text -> ProcessorName
processorName = ProcessorName

-- | Subtopology task identifier (matches Java's
-- @TaskId(subtopologyId, partition)@).
data TaskId = TaskId
  { taskSubtopology :: !Int
  , taskPartition   :: !Int32
  }
  deriving stock (Eq, Ord, Show, Generic)

taskIdText :: TaskId -> Text
taskIdText (TaskId sub p) =
  T.pack (show sub) <> "_" <> T.pack (show p)

-- | When does the punctuator fire?
data PunctuationType
  = StreamTimePunctuation     -- ^ Stream-time (Java: @PunctuationType.STREAM_TIME@)
  | WallClockTimePunctuation  -- ^ Wall-clock (Java: @PunctuationType.WALL_CLOCK_TIME@)
  deriving stock (Eq, Show, Generic)

-- | A user-supplied callback fired by 'schedule'.
newtype Punctuator = Punctuator
  { runPunctuator :: Timestamp -> IO ()
  }

-- | Token returned by 'schedule' for cancelling the punctuator.
newtype Cancellable = Cancellable
  { cancel :: IO ()
  }

-- | A no-op cancel token.
cancelled :: Cancellable
cancelled = Cancellable (pure ())

-- | Build a 'Punctuator' that forwards a record on every fire by
-- calling the supplied builder with the fire timestamp. Returning
-- 'Nothing' from the builder skips that fire (no record forwarded).
forwardingPunctuator
  :: ProcessorContext
  -> (Timestamp -> IO (Maybe (Record k v)))
  -> Punctuator
forwardingPunctuator ctx f = Punctuator $ \ts -> do
  m <- f ts
  case m of
    Nothing -> pure ()
    Just r  -> ctxForward ctx r

-- | The full processor surface. Generic over the input @(k, v)@.
--
-- 'procInit' is called once with the typed 'ProcessorContext' and is
-- the place to register stores / punctuators / read configuration.
-- 'procProcess' is the per-record callback. 'procClose' is invoked on
-- task shutdown (after the last commit).
data Processor k v = Processor
  { procName    :: !ProcessorName
  , procInit    :: !(ProcessorContext -> IO ())
  , procProcess :: !(Record k v -> IO ())
  , procClose   :: !(IO ())
  }

-- | A processor that does nothing.
noopProcessor :: ProcessorName -> Processor k v
noopProcessor n = Processor
  { procName    = n
  , procInit    = \_ -> pure ()
  , procProcess = \_ -> pure ()
  , procClose   = pure ()
  }

-- | Convenience constructor for stateless transformers that don't
-- need 'procInit'.
statelessProcessor
  :: ProcessorName
  -> (Record k v -> IO ())
  -> Processor k v
statelessProcessor n p = Processor
  { procName    = n
  , procInit    = \_ -> pure ()
  , procProcess = p
  , procClose   = pure ()
  }

-- | Dependency-injection record handed to every processor.
--
-- We pin the downstream forward type as 'forwardAny' (existentially
-- typed). The DSL erases the value's static type when crossing
-- processor boundaries; the runtime preserves type-safety at the
-- /edges/ (sources / sinks) and trusts the topology builder to wire
-- compatible pairs in between. This matches the Java erasure.
data ProcessorContext = ProcessorContext
  { ctxApplicationId   :: !Text
  , ctxTaskId          :: !TaskId
  , ctxRecordMetadata  :: !(IO (Maybe RecordMetadata))
  , ctxStreamTime      :: !(IO Timestamp)
  , ctxWallClockTime   :: !(IO Timestamp)
  , ctxForward         :: !(forall k v. Record k v -> IO ())
    -- ^ Forward to all downstream nodes. The runtime tags every
    -- forward with the current (sourceNode, recordMetadata) so sink
    -- processors can attribute it correctly.
  , ctxForwardTo       :: !(forall k v. NodeName -> Record k v -> IO ())
    -- ^ Forward to a single named downstream node. Used by 'branch'.
  , ctxSchedule        :: !(Int -> PunctuationType -> Punctuator -> IO Cancellable)
    -- ^ @schedule intervalMs type pun@ — register a punctuator.
  , ctxGetStore        :: !(StoreName -> IO (Maybe AnyStateStore))
    -- ^ Look up an attached store by name. Returns 'Nothing' if no
    -- such store was declared on this processor.
  , ctxEmitToTopic     :: !(SinkEmit -> IO ())
    -- ^ Low-level: emit a record directly to the runtime's
    -- collector under an explicitly chosen topic. Used by
    -- 'TopicNameExtractor' sinks; user code generally goes through
    -- 'forwardRecord' instead.
  , ctxRecordHeaders   :: !(IO (Maybe Kafka.Streams.Types.Headers))
    -- ^ Read the headers attached to the record currently being
    -- processed. Returns 'Nothing' when called from outside a
    -- record-processing context (e.g. from a punctuator).
  , ctxAddHeader       :: !(Kafka.Streams.Types.Header -> IO ())
    -- ^ Append a header to the in-flight record. Subsequent
    -- 'forwardRecord' calls (in the same 'procProcess' invocation)
    -- see the updated headers.
  }

-- | Bytes-already-serialised emission record. Defined here (rather
-- than in 'Kafka.Streams.Internal.RecordCollector') to avoid an
-- import cycle.
data SinkEmit = SinkEmit
  { seTopic     :: !Text
  , seKey       :: !(Maybe ByteString)
  , seValue     :: !ByteString
  , seTimestamp :: !Timestamp
  }
  deriving stock Generic

-- | Convenience access to the lookup function.
getStateStore :: ProcessorContext -> StoreName -> IO (Maybe AnyStateStore)
getStateStore = ctxGetStore

currentRecordMetadata :: ProcessorContext -> IO (Maybe RecordMetadata)
currentRecordMetadata = ctxRecordMetadata

forwardRecord :: ProcessorContext -> Record k v -> IO ()
forwardRecord ctx r = ctxForward ctx r

forwardTo :: ProcessorContext -> NodeName -> Record k v -> IO ()
forwardTo ctx nm r = ctxForwardTo ctx nm r

schedule
  :: ProcessorContext
  -> Int
  -> PunctuationType
  -> Punctuator
  -> IO Cancellable
schedule = ctxSchedule

taskId :: ProcessorContext -> TaskId
taskId = ctxTaskId

applicationIdC :: ProcessorContext -> Text
applicationIdC = ctxApplicationId

streamTimeC :: ProcessorContext -> IO Timestamp
streamTimeC = ctxStreamTime

wallClockTimeC :: ProcessorContext -> IO Timestamp
wallClockTimeC = ctxWallClockTime
