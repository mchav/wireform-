{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streams.Properties.OperatorWatermarkSpec
-- Description : Operators (e.g. suppress) read the coordinated
--               watermark, not just per-task stream-time.
module Streams.Properties.OperatorWatermarkSpec (tests) where

import qualified Data.IORef as IORef
import qualified Data.Text as T
import Test.Syd

import Kafka.Streams (Timestamp (..))
import qualified Kafka.Streams.Processor as Processor
import Kafka.Streams.Processor (effectiveTime)
import Kafka.Streams.Processor.Mock (newMockProcessorContext, mockContext)
import qualified Kafka.Streams.Processor.Mock as Mock
import Kafka.Streams.Processor (TaskId (..))

tests :: Spec
tests = describe "Operator watermark plumbing" $ sequence_
  [ effective_time_falls_back_to_stream_time
  , effective_time_prefers_coordinator_when_set
  ]

effective_time_falls_back_to_stream_time :: Spec
effective_time_falls_back_to_stream_time =
  it "effectiveTime returns ctxStreamTime when no coordinator wired" $ do
    mctx <- newMockProcessorContext "test-app" (TaskId 0 0)
    Mock.setStreamTime mctx (Timestamp 42)
    let ctx = mockContext mctx
    t <- effectiveTime ctx
    t `shouldBe` Timestamp 42

effective_time_prefers_coordinator_when_set :: Spec
effective_time_prefers_coordinator_when_set =
  it "effectiveTime returns coordinated wm when ctxCoordinatedWatermark is Just" $ do
    -- We can't trivially wire a coordinator into the mock
    -- context (it would require a parallel mock; for the
    -- engine path see WatermarkWiringSpec). Instead, we
    -- override the field directly via a record-update, which
    -- is the same trick the runtime uses.
    mctx <- newMockProcessorContext "test-app" (TaskId 0 0)
    Mock.setStreamTime mctx (Timestamp 100)
    let baseCtx = mockContext mctx
        wmRef = Timestamp 50
        ctx = baseCtx
          { Processor.ctxCoordinatedWatermark = pure (Just wmRef) }
    t <- effectiveTime ctx
    t `shouldBe` wmRef
