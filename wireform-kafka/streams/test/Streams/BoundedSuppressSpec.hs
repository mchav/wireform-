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
tests = testGroup "Bounded Suppress (KIP-328 + Riffle)"
  [ emit_early_when_full_drains_oldest
  , shutdown_when_full_throws
  , unbounded_buffer_stays_buffered_until_grace
  , drop_oldest_silently_evicts_without_emitting
  , shed_routes_overflow_to_dead_letter_topic
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

----------------------------------------------------------------------
-- 4. DropOldestSilently
--
-- The Riffle "drop" policy: same eviction trigger as
-- 'EmitEarlyWhenFull', but the evicted records are silently
-- discarded — they MUST NOT appear downstream of the suppress
-- operator. Downstream sees only the windows that remain in the
-- buffer and eventually flush via grace.
----------------------------------------------------------------------

drop_oldest_silently_evicts_without_emitting :: TestTree
drop_oldest_silently_evicts_without_emitting =
  testCase "BufferConfig DropOldestSilently: oldest entries are evicted, not emitted" $ do
    topo <- buildSuppressTopo
              (dropOldestSilently (maxRecordsBufferConfig 2))
              1000 100_000
    driver <- newDriver topo "sup-bounded-drop"

    -- 3 distinct keys; cap is 2. The third put evicts k1 from
    -- the buffer. Downstream sees NOTHING until grace elapses
    -- (which it doesn't in this run, since grace = 100s).
    pipeInput driver (topicName "in") (Just (bytes "k1"))
              (i64 1) (ts 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k2"))
              (i64 2) (ts 10) 0
    pipeInput driver (topicName "in") (Just (bytes "k3"))
              (i64 3) (ts 20) 0

    let outTopic = createOutputTopic driver (topicName "out")
                     (windowedSerde textSerde) int64Serde
    outs <- readKeyValuesToList outTopic
    let ok = [ wk | Right (Just wk, _) <- outs ]
    -- Drop policy means downstream sees zero records until
    -- grace flushes — and grace hasn't elapsed.
    length ok @?= 0
    closeDriver driver

----------------------------------------------------------------------
-- 5. Shed-to-DLQ (suppressWindowedShed)
--
-- The Riffle "shed" policy: oldest entries over the cap are
-- routed to a dead-letter topic via the engine's record
-- collector, and downstream of the suppress operator sees
-- nothing for them. Downstream only sees records that flushed
-- naturally via grace.
----------------------------------------------------------------------

shed_routes_overflow_to_dead_letter_topic :: TestTree
shed_routes_overflow_to_dead_letter_topic =
  testCase "DeadLetterShelf: overflow entries land on the side topic" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in")
           (consumed textSerde int64Serde)
    let toWindowed k v = (WindowedKey k (Timestamp 0), v)
    ws <- mapKeyValueWith
            (windowedSerde textSerde) int64Serde toWindowed s
    let shelf = DeadLetterShelf
          { dlsTopic       = topicName "dlq"
          , dlsKeySerde    = windowedSerde textSerde
          , dlsValueSerde  = int64Serde
          , dlsRecordCap   = 2
          }
    out <- suppressWindowedShed (millis 100_000) 1000 shelf ws
    toTopic (topicName "out")
            (produced (windowedSerde textSerde) int64Serde) out
    topo <- buildTopology b
    driver <- newDriver topo "sup-bounded-shed"

    -- 3 distinct keys; cap is 2. The third put pushes k1
    -- (oldest) onto the DLQ topic. Downstream of suppress
    -- ("out") still has nothing — grace hasn't elapsed.
    pipeInput driver (topicName "in") (Just (bytes "k1"))
              (i64 1) (ts 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k2"))
              (i64 2) (ts 10) 0
    pipeInput driver (topicName "in") (Just (bytes "k3"))
              (i64 3) (ts 20) 0

    let outTopic = createOutputTopic driver (topicName "out")
                     (windowedSerde textSerde) int64Serde
        dlqTopic = createOutputTopic driver (topicName "dlq")
                     (windowedSerde textSerde) int64Serde

    outsMain <- readKeyValuesToList outTopic
    let okMain = [ wk | Right (Just wk, _) <- outsMain ]
    length okMain @?= 0

    outsDlq <- readKeyValuesToList dlqTopic
    let okDlq = [ wk | Right (Just wk, _) <- outsDlq ]
    -- At least one shed record reached the DLQ (the oldest by
    -- window-start was k1; the exact count depends on per-
    -- write enforcement, but it MUST be > 0).
    assertBool
      ("expected >= 1 shed record on the DLQ topic; got "
        <> show (length okDlq))
      (not (null okDlq))
    closeDriver driver
