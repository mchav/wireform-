{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.SideEffects
-- Description : Every flavour of side effect the streams DSL supports
--
-- The DSL has four seams for running 'IO' inside a topology:
--
--   * 'peekStream'    — observe records non-destructively
--                       (mirrors @KStream.peek@)
--   * 'mapValuesM'    — IO-flavoured @KStream.mapValues@; the JVM
--                       version is nominally pure but in practice
--                       people sneak side effects into it. Here it
--                       is explicit.
--   * 'foreachStream' — terminal IO sink, no downstream node
--                       (mirrors @KStream.foreach@)
--   * The Processor API + a 'Punctuator' scheduled by
--     'schedule', for wall-clock or stream-time triggered work
--     (mirrors @ProcessorContext.schedule@).
--
-- This demo wires all four into a single topology that emulates a
-- mini order-processing pipeline:
--
--   1. Trace every incoming event with 'peekStream' (logger seam).
--   2. Enrich each order via a simulated /external/ customer
--      lookup with 'mapValuesM' (DB / HTTP seam).
--   3. Tap a metrics counter with 'foreachStream' (metrics seam).
--   4. A Processor + stream-time 'Punctuator' summarises the
--      batch every 30 seconds (background-flush seam).
--
-- In a real deployment you would replace the IORefs with whatever
-- side-effecting clients your service uses.
--
-- == Caveats vs. the JVM
--
-- * Effects in 'peek' / 'foreach' / 'mapValuesM' / a 'Punctuator'
--   are NOT part of an EOS-V2 transaction. A topology rewind on
--   rebalance will replay them. If exactly-once is required for
--   the side effect, gate it on an idempotency-token state store
--   the way the JVM advice does.
-- * On a single task, effects fire in topology order. With
--   @numStreamThreads > 1@ different keys may be processed
--   concurrently across tasks; if your effect needs a global
--   ordering, route through @repartition@ to one partition or
--   sink to a topic and consume separately.
module Kafka.Streams.Examples.SideEffects
  ( runDemo
  , buildSideEffectsTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams
import qualified Kafka.Streams.Topology as Topo

-- | A bag of "external systems" the topology talks to. We pass
-- it in so the runDemo function can also inspect the resulting
-- effect log after running. In a production app these would be
-- handles to a logger, a DB connection pool, a metrics client,
-- and an outbox.
data Externals = Externals
  { extLog       :: IORef [Text]
    -- ^ "log lines" emitted by 'peekStream'.
  , extMetrics   :: IORef (Map.Map Text Int64)
    -- ^ counters bumped from 'foreachStream'.
  , extLookups   :: IORef Int64
    -- ^ how many times 'mapValuesM' issued an external lookup.
  , extProfileDB :: IORef (Map.Map Text Text)
    -- ^ a fake "customers" table the mapValuesM call hits.
  , extBatchOut  :: IORef [(Timestamp, Text)]
    -- ^ records the Punctuator pushed out as a batch summary.
  }

newExternals :: IO Externals
newExternals = do
  l   <- newIORef []
  m   <- newIORef Map.empty
  lu  <- newIORef 0
  db  <- newIORef Map.empty
  bs  <- newIORef []
  pure (Externals l m lu db bs)

----------------------------------------------------------------------
-- Topology
----------------------------------------------------------------------

-- | Topology nodes, in order:
--
--     orders [src]
--       -> peek (log)
--       -> mapValuesM (enrich via fake DB)
--       -> peek (metrics tap)
--       -> sink "enriched-orders"
--       -> processor "BatchSummary" (with stream-time Punctuator)
buildSideEffectsTopology :: Externals -> IO Topology
buildSideEffectsTopology ext = do
  b <- newStreamsBuilder
  src <- streamFromTopic b
            (topicName "orders")
            (consumed textSerde textSerde)

  -- (1) Logger seam: peek without altering the stream.
  logged <- peekStream
              (\r -> modifyIORef' (extLog ext)
                       (\xs -> xs ++ [trace r]))
              src

  -- (2) DB seam: look up the customer profile via mapValuesM.
  --     The JVM API would be 'mapValues', leaving the IO implicit;
  --     here it is typed so the topology shows where the IO
  --     happens. Each call bumps a counter the test can inspect.
  enriched <- mapValuesM (lookupProfile ext) logged

  -- (3) Metrics seam: foreachStream is terminal but in practice
  --     you usually want both a sink topic AND a metrics tap.
  --     We use peek here so the stream continues; foreachStream
  --     is the same shape but consumes the stream.
  tapped <- peekStream (bumpMetrics ext) enriched

  -- Sink the enriched record to a topic.
  toTopic
    (topicName "enriched-orders")
    (produced textSerde textSerde)
    tapped

  -- (4) Background-flush seam: a low-level Processor that
  --     schedules a stream-time Punctuator every 30 seconds.
  --     The Punctuator drains a buffered count into the
  --     extBatchOut log. Demonstrates the 'schedule' API and
  --     state-store-backed buffering.
  let bufNm = storeName "batch-buffer"
  -- The processor reads from the same source; it doesn't need
  -- the enriched stream — its job is to summarise raw event
  -- counts every 30s for an out-of-band "status" channel.
  _ <- processValuesStream
         "BatchSummary"
         [bufNm]
         (batchSummaryProc ext bufNm)
         textSerde
         src
  let kvBuilder = inMemoryKeyValueStoreBuilder bufNm
                    :: StoreBuilderKV Text Int64
  -- Find the BatchSummary processor by recovering the most
  -- recently named node and attaching the store to it. We do
  -- it here rather than through 'processStream' because the
  -- store-attach requires the processor's actual node name.
  -- (Same dance as Kafka.Streams.Examples.ProcessorAPI.)
  withTopology_ b $ \t -> do
    -- The summary processor's node name is whatever the next
    -- 'freshNodeName b "BatchSummary"' returned inside
    -- 'processValuesStream'. We retrieve it from the topology
    -- via the last-added processor with that prefix.
    let !names = [ n | (Topo.NodeName n) <- Map.keys (Topo.topoProcessors t)
                     , T.isPrefixOf "BatchSummary-" n
                     ]
        !owner = case names of
                   (n : _) -> Topo.NodeName n
                   []      -> error "BatchSummary processor not found"
    Topo.addStateStoreKV kvBuilder [owner] t

  buildTopology b
  where
    trace r =
      "trace key="
        <> maybe "<no-key>" (T.pack . show) (recordKey r)
        <> " value=" <> recordValue r

----------------------------------------------------------------------
-- Side-effect handlers
----------------------------------------------------------------------

-- | Simulated external lookup. Increments a counter per call so
-- the demo can show "we hit the DB N times".
lookupProfile :: Externals -> Text -> IO Text
lookupProfile ext order = do
  modifyIORef' (extLookups ext) (+ 1)
  db <- readIORef (extProfileDB ext)
  let !customerId = T.takeWhile (/= '|') order
      !rest       = T.drop 1 (T.dropWhile (/= '|') order)
      !customer   = Map.findWithDefault "<unknown>" customerId db
  pure (customer <> "|" <> rest)

bumpMetrics :: Externals -> Record Text Text -> IO ()
bumpMetrics ext r = do
  modifyIORef' (extMetrics ext)
    (Map.insertWith (+) "events.observed" 1)
  -- "Errors" tracked by a simple value-substring sentinel.
  let v = recordValue r
  if "<unknown>" `T.isInfixOf` v
    then modifyIORef' (extMetrics ext)
           (Map.insertWith (+) "events.unknown_customer" 1)
    else pure ()

----------------------------------------------------------------------
-- Background-flush processor
----------------------------------------------------------------------

batchSummaryProc
  :: Externals
  -> StoreName
  -> IO (Processor Text Text)
batchSummaryProc ext bufNm = do
  ctxRef   <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (KeyValueStore Text Int64))
  pure Processor
    { procName = processorName "BatchSummary"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx bufNm >>= \case
          Just (AnyKeyValueStore kvs) ->
            writeIORef storeRef
              (Just (Unsafe.unsafeCoerce kvs))
          _ -> error "batchSummaryProc: store missing"
        -- Schedule a stream-time Punctuator at 30-second cadence.
        -- Wall-clock would be 'WallClockTimePunctuation'; here we
        -- key off stream time so the demo is deterministic
        -- against the test driver's pipeInput timestamps.
        _ <- schedule ctx
                30_000
                StreamTimePunctuation
                (Punctuator (\t -> flushBatch ext storeRef t))
        pure ()
    , procClose   = pure ()
    , procProcess = \r -> do
        mst <- readIORef storeRef
        case (mst, recordKey r) of
          (Just kvs, Just k) -> do
            cur <- kvsGet kvs k
            kvsPut kvs k (maybe 1 (+ 1) cur)
          _ -> pure ()
    }

flushBatch
  :: Externals
  -> IORef (Maybe (KeyValueStore Text Int64))
  -> Timestamp
  -> IO ()
flushBatch ext storeRef now = do
  mst <- readIORef storeRef
  case mst of
    Nothing  -> pure ()
    Just kvs -> do
      it <- kvsAll kvs
      pairs <- kvIteratorToList it
      mapM_
        (\(k, n) -> do
           modifyIORef' (extBatchOut ext)
             (\xs -> xs ++ [(now, k <> ":" <> T.pack (show n))])
           -- Reset the per-key count; a real implementation
           -- might also write to a downstream sink via
           -- 'forwardRecord' (for which we'd need ctx).
           kvsPut kvs k 0)
        pairs

----------------------------------------------------------------------
-- Demo
----------------------------------------------------------------------

runDemo :: IO ()
runDemo = do
  putStrLn "=== SideEffectsDemo ==="
  ext <- newExternals
  -- Pre-seed the "customers" table.
  writeIORef (extProfileDB ext) $ Map.fromList
    [ ("c1", "alice"), ("c2", "bob"), ("c3", "carol") ]

  topo   <- buildSideEffectsTopology ext
  driver <- newDriver topo "side-effects-app"

  let order ts c v =
        pipeInput driver (topicName "orders")
          (Just (BSC.pack (T.unpack c)))
          (BSC.pack (T.unpack (c <> "|" <> v)))
          (Timestamp ts)
          0
      sec :: Int64 -> Int64
      sec n = n * 1000

  -- Burst over 70 seconds so the 30s stream-time punctuator
  -- fires twice (at t=30 and t=60).
  order (sec 0)  "c1" "o-100|199.95"
  order (sec 5)  "c2" "o-101|49.50"
  order (sec 12) "c1" "o-102|14.99"
  order (sec 25) "c4" "o-103|9.00"   -- unknown customer
  -- Push stream time past 30s -> first Punctuator fires.
  order (sec 35) "c3" "o-104|79.00"
  order (sec 60) "c1" "o-105|10.00"
  -- Push past 60s -> second Punctuator fires.
  order (sec 75) "c2" "o-106|22.50"

  -- Let the test driver flush stream time so the final
  -- punctuator fires before we read the externals.
  advanceDriverStreamTime driver (Timestamp (sec 90))

  enriched <- readOutput driver (topicName "enriched-orders")
  putStrLn ("Enriched orders (sink): " <> show (length enriched))
  mapM_ (\cr -> putStrLn ("  -> " <> BSC.unpack (crValue cr))) enriched

  putStrLn ""
  logLines <- readIORef (extLog ext)
  putStrLn ("peek log lines: " <> show (length logLines))
  mapM_ (\l -> putStrLn ("  " <> T.unpack l)) (take 3 logLines)
  putStrLn (if length logLines > 3
              then "  ... " <> show (length logLines - 3) <> " more"
              else "")

  putStrLn ""
  metrics <- readIORef (extMetrics ext)
  putStrLn "metrics counters (foreach tap):"
  mapM_
    (\(k, v) -> putStrLn ("  " <> T.unpack k <> " = " <> show v))
    (Map.toAscList metrics)

  putStrLn ""
  lookups <- readIORef (extLookups ext)
  putStrLn ("mapValuesM external lookups: " <> show lookups)

  putStrLn ""
  batches <- readIORef (extBatchOut ext)
  putStrLn ("batch summaries flushed by Punctuator: "
            <> show (length batches))
  mapM_
    (\(Timestamp t, line) ->
       putStrLn ("  t=" <> show t <> " " <> T.unpack line))
    batches

  closeDriver driver
