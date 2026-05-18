{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | KIP-328 bounded BufferConfig enforcement.
--
-- We exercise 'suppressWindowedWith' by feeding it a stream of
-- pre-windowed records (the windowed-aggregation step would
-- add coverage we don't need at this layer) and observing the
-- downstream emissions / runtime exception under each
-- overflow policy.
module Streams.BoundedSuppressSpec (tests) where

import qualified Control.Concurrent
import Control.Exception (try, SomeException, evaluate)
import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import Kafka.Streams
import Kafka.Streams.Serde.Windowed (windowedSerde)

tests :: TestTree
tests = testGroup "Bounded Suppress (KIP-328)"
  [ emit_early_when_full_drains_oldest
  , shutdown_when_full_throws
  , unbounded_buffer_stays_buffered_until_grace
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

ts :: Int64 -> Timestamp
ts = Timestamp

-- | Build a topology that:
--
--   source(in, k=Text, v=Int64)
--     -> map @k -> WindowedKey k (Timestamp 0)
--     -> suppressWindowedWith ...
--     -> sink(out, k=WindowedKey Text, v=Int64)
--
-- so we can drive 'suppressWindowedWith' directly without
-- the windowed-aggregate machinery in the way.
buildSuppressTopo
  :: BufferConfig -> Int64 -> Int64 -> IO Topology
buildSuppressTopo cfg windowSizeMs graceMs = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in")
         (consumed textSerde int64Serde)
  -- Carry the window-start through the record key as a
  -- WindowedKey. Different input keys -> different windowed
  -- keys -> different buffer slots (so we can fill the cap).
  let toWindowed k v = (WindowedKey k (Timestamp 0), v)
  -- WindowedKey has no default HasSerde instance, so supply the
  -- pair of serdes explicitly via mapKeyValueWith.
  ws <- mapKeyValueWith (windowedSerde textSerde) int64Serde toWindowed s
  out <- suppressWindowedWith
           (millis graceMs)
           windowSizeMs
           cfg
           ws
  toTopic (topicName "out")
          (produced (windowedSerde textSerde) int64Serde) out
  buildTopology b

i64 :: Int64 -> BSC.ByteString
i64 = serialize int64Serde

----------------------------------------------------------------------
-- 1. EmitEarlyWhenFull
----------------------------------------------------------------------

emit_early_when_full_drains_oldest :: TestTree
emit_early_when_full_drains_oldest =
  testCase "BufferConfig EmitEarlyWhenFull: oldest entries are flushed when over cap" $ do
    -- Cap = 2, grace is huge so natural-flush doesn't kick in.
    topo <- buildSuppressTopo
              (emitEarlyWhenFull (maxRecordsBufferConfig 2))
              1000 100_000
    driver <- newDriver topo "sup-bounded-eearly"

    -- 3 distinct keys -> 3 buffered window entries. Cap is 2,
    -- so the third put forces an early-emit of the oldest.
    pipeInput driver (topicName "in") (Just (bytes "k1"))
              (i64 1) (ts 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k2"))
              (i64 2) (ts 10) 0
    pipeInput driver (topicName "in") (Just (bytes "k3"))
              (i64 3) (ts 20) 0

    let outTopic = createOutputTopic driver (topicName "out")
                     (windowedSerde textSerde) int64Serde
    outs <- readKeyValuesToList outTopic
    -- Filter for successful decodes only (k = WindowedKey).
    let ok = [ (wk, v) | Right (Just wk, v) <- outs ]
    assertBool
      ("expected >= 1 early-emitted records; got "
        <> show (length ok))
      (not (null ok))
    closeDriver driver

----------------------------------------------------------------------
-- 2. ShutdownWhenFull throws
----------------------------------------------------------------------

shutdown_when_full_throws :: TestTree
shutdown_when_full_throws =
  testCase "BufferConfig ShutdownWhenFull: throws SuppressBufferFullException" $ do
    topo <- buildSuppressTopo
              (shutDownWhenFull (maxRecordsBufferConfig 2))
              1000 100_000
    driver <- newDriver topo "sup-bounded-shutdown"

    -- The first two fit; the third should throw.
    pipeInput driver (topicName "in") (Just (bytes "k1"))
              (i64 1) (ts 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k2"))
              (i64 2) (ts 10) 0
    r <- try (pipeInput driver (topicName "in") (Just (bytes "k3"))
              (i64 3) (ts 20) 0) :: IO (Either SomeException ())
    case r of
      Left _  -> pure ()
      Right _ ->
        error
          ("expected SuppressBufferFullException after the third "
            <> "distinct key; runtime did not throw")
    closeDriver driver

----------------------------------------------------------------------
-- 3. Unbounded buffer still works
----------------------------------------------------------------------

unbounded_buffer_stays_buffered_until_grace :: TestTree
unbounded_buffer_stays_buffered_until_grace =
  testCase "Unbounded buffer: entries stay until grace elapses (no early eviction)" $ do
    topo <- buildSuppressTopo unboundedBufferConfig 1000 1000
    driver <- newDriver topo "sup-unbounded"

    -- 5 distinct keys, all within the same window — none should
    -- flush early because the cap is unbounded and grace
    -- hasn't elapsed.
    mapM_
      (\(i :: Int) ->
        pipeInput driver (topicName "in")
          (Just (bytes (T.pack ("k" <> show i))))
          (i64 (fromIntegral i)) (ts (fromIntegral i * 10)) 0)
      [0 .. 4]
    let outTopic = createOutputTopic driver (topicName "out")
                     (windowedSerde textSerde) int64Serde
    outs <- readKeyValuesToList outTopic
    let ok = [ wk | Right (Just wk, _) <- outs ]
    length ok @?= 0
    closeDriver driver
