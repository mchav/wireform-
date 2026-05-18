{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Processor.Mock
-- Description : Mock 'ProcessorContext' for unit-testing user 'Processor'
--               implementations
--
-- Mirror of the JVM
-- @org.apache.kafka.streams.processor.api.MockProcessorContext@. Used
-- to exercise a 'Processor' / 'FixedKeyProcessor' in isolation without
-- spinning up a 'TopologyTestDriver' or the runtime.
--
-- The mock captures everything a real context dispatches:
--
--   * Forwarded records — both anonymous ('Forward') and named
--     ('ForwardTo'); 'capturedForwards' yields them in the order they
--     were forwarded.
--   * Scheduled punctuators — 'capturedPunctuators'. Tests can invoke
--     them at known times to assert their behaviour.
--   * Commit requests — 'commitRequested'.
--   * Headers appended to the in-flight record — 'capturedHeaders'.
--
-- @
-- ctx <- newMockProcessorContext \"app\" (TaskId 0 0)
-- let p = statelessProcessor (processorName \"upper\")
--           (\\r -> forwardRecord ctx (mapValue T.toUpper r))
-- procInit p (mockContext ctx)
-- procProcess p (mkRecord (Just \"k\") \"hello\" (Timestamp 0))
-- forwarded \<- capturedForwards ctx
-- length forwarded \`shouldBe\` 1
-- @
module Kafka.Streams.Processor.Mock
  ( -- * Builder
    MockProcessorContext
  , newMockProcessorContext
  , mockContext
    -- * Captured side-effects
  , CapturedForward (..)
  , CapturedPunctuator (..)
  , capturedForwards
  , capturedPunctuators
  , clearCapturedForwards
  , clearCapturedPunctuators
  , commitRequested
  , readCommitRequested
    -- * Time / metadata controls
  , setStreamTime
  , setWallClockTime
  , setRecordMetadata
    -- * Store registration
  , registerStateStore
  ) where

import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Unsafe.Coerce (unsafeCoerce)

import Kafka.Streams.Processor
  ( Cancellable (..)
  , ProcessorContext (..)
  , PunctuationType (..)
  , Punctuator (..)
  , SinkEmit (..)
  , TaskId
  )
import Kafka.Streams.State.Store (AnyStateStore, StoreName)
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types
  ( Headers
  , NodeName
  , Record
  , RecordMetadata
  , addHeader
  , emptyHeaders
  )

----------------------------------------------------------------------
-- Captured side-effects
----------------------------------------------------------------------

-- | A forwarded record. The key/value type is erased on purpose —
-- the 'ProcessorContext' itself erases them too, so the mock can't
-- carry typed evidence here without further plumbing. Use
-- 'unsafeForwardedRecord' to recover a typed view at the test site
-- when you know the downstream type.
data CapturedForward where
  Forward   :: forall k v. !(Record k v) -> CapturedForward
  ForwardTo :: forall k v. !NodeName -> !(Record k v) -> CapturedForward
  EmitTopic :: !SinkEmit -> CapturedForward

-- | A scheduled punctuator. The mock doesn't fire them on its own;
-- tests use 'runPunctuator' on the captured value to invoke them
-- at known times.
data CapturedPunctuator = CapturedPunctuator
  { cpIntervalMs :: !Int
  , cpType       :: !PunctuationType
  , cpPunctuator :: !Punctuator
  }

----------------------------------------------------------------------
-- Mock context
----------------------------------------------------------------------

-- | The mock. Carries the underlying 'ProcessorContext' plus
-- accessors for the captured side-effects.
data MockProcessorContext = MockProcessorContext
  { mpcContext        :: !ProcessorContext
  , mpcForwards       :: !(IORef (Seq CapturedForward))
  , mpcPunctuators    :: !(IORef (Seq CapturedPunctuator))
  , mpcCommitFlag     :: !(IORef Bool)
  , mpcHeaders        :: !(IORef Headers)
  , mpcStreamTime     :: !(IORef Timestamp)
  , mpcWallClockTime  :: !(IORef Timestamp)
  , mpcRecordMetadata :: !(IORef (Maybe RecordMetadata))
  , mpcStores         :: !(IORef (Map StoreName AnyStateStore))
  }

-- | Build a new mock context. Stream-time and wall-clock-time start
-- at 0; the record metadata is 'Nothing'.
newMockProcessorContext
  :: Text                                 -- ^ application id
  -> TaskId
  -> IO MockProcessorContext
newMockProcessorContext appId tid = do
  fwdRef     <- newIORef Seq.empty
  punctRef   <- newIORef Seq.empty
  commitRef  <- newIORef False
  hdrsRef    <- newIORef emptyHeaders
  stRef      <- newIORef (Timestamp 0)
  wcRef      <- newIORef (Timestamp 0)
  metaRef    <- newIORef Nothing
  storeRef   <- newIORef Map.empty
  let ctx = ProcessorContext
        { ctxApplicationId  = appId
        , ctxTaskId         = tid
        , ctxRecordMetadata = readIORef metaRef
        , ctxStreamTime     = readIORef stRef
        , ctxWallClockTime  = readIORef wcRef
        , ctxForward        = \r ->
            atomicModifyIORef' fwdRef
              (\s -> (s |> Forward (unsafeCoerce r), ()))
        , ctxForwardTo      = \nm r ->
            atomicModifyIORef' fwdRef
              (\s -> (s |> ForwardTo nm (unsafeCoerce r), ()))
        , ctxSchedule       = \intervalMs ptype pun -> do
            atomicModifyIORef' punctRef $ \s ->
              (s |> CapturedPunctuator intervalMs ptype pun, ())
            pure Cancellable { cancel = pure () }
        , ctxGetStore       = \nm -> do
            m <- readIORef storeRef
            pure (Map.lookup nm m)
        , ctxEmitToTopic    = \se ->
            atomicModifyIORef' fwdRef
              (\s -> (s |> EmitTopic se, ()))
        , ctxRecordHeaders  = Just <$> readIORef hdrsRef
        , ctxAddHeader      = \h ->
            atomicModifyIORef' hdrsRef (\hs -> (addHeader h hs, ()))
        , ctxRequestCommit  = writeIORef commitRef True
        , ctxRegisterPreCommitDrain = \_ -> pure ()
        , ctxCoordinatedWatermark = pure Nothing
          -- Mock context: pre-commit drains aren't exercised in
          -- unit tests of individual processors. The registry is
          -- a no-op so the mock has the same record shape as
          -- the engine-built 'ProcessorContext'.
        }
  pure MockProcessorContext
    { mpcContext        = ctx
    , mpcForwards       = fwdRef
    , mpcPunctuators    = punctRef
    , mpcCommitFlag     = commitRef
    , mpcHeaders        = hdrsRef
    , mpcStreamTime     = stRef
    , mpcWallClockTime  = wcRef
    , mpcRecordMetadata = metaRef
    , mpcStores         = storeRef
    }

-- | Yield the underlying 'ProcessorContext' to pass into a
-- 'Processor's 'procInit'.
mockContext :: MockProcessorContext -> ProcessorContext
mockContext = mpcContext

----------------------------------------------------------------------
-- Accessors
----------------------------------------------------------------------

-- | Every record the processor has forwarded since the last
-- 'clearCapturedForwards', in the order they were forwarded.
capturedForwards :: MockProcessorContext -> IO [CapturedForward]
capturedForwards m = do
  s <- readIORef (mpcForwards m)
  pure (foldr (:) [] s)

-- | Drop every captured forward. Useful between phases of a test.
clearCapturedForwards :: MockProcessorContext -> IO ()
clearCapturedForwards m = writeIORef (mpcForwards m) Seq.empty

-- | Every punctuator the processor has scheduled since the last
-- 'clearCapturedPunctuators'.
capturedPunctuators :: MockProcessorContext -> IO [CapturedPunctuator]
capturedPunctuators m = do
  s <- readIORef (mpcPunctuators m)
  pure (foldr (:) [] s)

-- | Drop every captured punctuator.
clearCapturedPunctuators :: MockProcessorContext -> IO ()
clearCapturedPunctuators m = writeIORef (mpcPunctuators m) Seq.empty

-- | @True@ iff 'ctxRequestCommit' has been called since the mock
-- was created or 'readCommitRequested' last cleared it.
commitRequested :: MockProcessorContext -> IO Bool
commitRequested m = readIORef (mpcCommitFlag m)

-- | Read and clear the commit-requested flag.
readCommitRequested :: MockProcessorContext -> IO Bool
readCommitRequested m =
  atomicModifyIORef' (mpcCommitFlag m) (\b -> (False, b))

----------------------------------------------------------------------
-- Time / metadata controls
----------------------------------------------------------------------

setStreamTime :: MockProcessorContext -> Timestamp -> IO ()
setStreamTime m t = writeIORef (mpcStreamTime m) t

setWallClockTime :: MockProcessorContext -> Timestamp -> IO ()
setWallClockTime m t = writeIORef (mpcWallClockTime m) t

setRecordMetadata
  :: MockProcessorContext -> Maybe RecordMetadata -> IO ()
setRecordMetadata m r = writeIORef (mpcRecordMetadata m) r

----------------------------------------------------------------------
-- Store registration
----------------------------------------------------------------------

-- | Pre-register a state store so 'getStateStore' on the mock
-- context returns it. Tests typically build an in-memory store and
-- register it here before exercising the processor.
registerStateStore
  :: MockProcessorContext -> StoreName -> AnyStateStore -> IO ()
registerStateStore m nm st =
  atomicModifyIORef' (mpcStores m) (\mp -> (Map.insert nm st mp, ()))
