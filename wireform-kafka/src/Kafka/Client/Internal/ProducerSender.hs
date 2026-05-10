{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Kafka.Client.Internal.ProducerSender
Description : Producer background sender thread with retry logic
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the background sender thread for the Kafka producer.

The sender thread:
- Continuously drains ready batches from the accumulator
- Groups batches by broker
- Sends ProduceRequests to each broker
- Handles retries with exponential backoff
- Tracks in-flight requests
- Updates batch states and notifies waiting threads

Design patterns:
- Dedicated sender thread per producer (similar to Java Kafka client)
- In-flight request tracking with correlation IDs
- Exponential backoff for retries
- Graceful shutdown support
-}
module Kafka.Client.Internal.ProducerSender
  ( -- * Sender Thread
    SenderState(..)
  , createSenderState
  , startSenderThread
  , stopSenderThread
    -- * Batch Sending
  , sendBatches
  , retryBatch
  , bumpBatchAttempts
  , batchBackoffMs
  , shouldRetry
    -- * Pipelined sender (per-broker in-flight tracking)
  , senderTotalInFlight
    -- * Pure batch construction (exposed for testing)
  , buildRecordBatch
    -- * Timeout Checking (KIP-91)
  , isBatchTimedOut
    -- * Configuration
  , RetryConfig(..)
  , defaultRetryConfig
  , nextRetryBackoffMs
    -- * Structured logger
  , LogLevel(..)
  , Logger
  , defaultLogger
  , silentLogger
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, void, when, forM, forM_)
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int
import qualified Data.List
import Data.List (groupBy, sortBy, partition)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO (hPutStrLn, stderr)
import qualified Kafka.Time as KafkaTime

import qualified Kafka.Client.Internal.BatchAccumulator as BA
import qualified Kafka.Client.Internal.Request as Req
import qualified Kafka.Client.Metadata as Meta
import Kafka.Compression.Types (CompressionCodec (NoCompression))
import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV
import qualified Kafka.Protocol.VersionNegotiation as VN
import qualified Kafka.Protocol.Generated.ProduceRequest as PR
import qualified Kafka.Protocol.Generated.ProduceResponse as PResp
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.RecordBatch as RB
import qualified Kafka.Protocol.RecordBatchWire as RBW
import qualified Kafka.Protocol.Wire.Codec as WC

-- | Retry configuration for failed sends. Mirrors librdkafka's
-- @retries@ + @retry.backoff.ms@ + @retry.backoff.max.ms@ +
-- @retry.backoff.multiplier@ knobs. The producer threads a
-- 'RetryConfig' through 'sendMessage' / 'sendMessageAsync' so a
-- per-batch retry loop can compute its own next-backoff via
-- 'nextRetryBackoffMs'.
data RetryConfig = RetryConfig
  { retryMaxAttempts       :: !Int
    -- ^ Maximum number of retry attempts (default: 2147483647).
  , retryBackoffMs         :: !Int
    -- ^ Initial backoff in ms (default: 100).
  , retryBackoffMaxMs      :: !Int
    -- ^ Ceiling for the exponential progression in ms (default: 1000).
  , retryBackoffMultiplier :: !Double
    -- ^ Multiplier between consecutive backoffs (default: 2.0).
  , retryBackoffJitter     :: !Double
    -- ^ Jitter band in [0.0, 1.0]; the actual backoff is
    --   @backoff * (1 ± jitter)@ randomised uniformly. Default 0.2.
  } deriving (Eq, Show)

-- | Default retry configuration. Matches the producer-side
-- @defaultProducerConfig@.
defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
  { retryMaxAttempts       = 2147483647
  , retryBackoffMs         = 100
  , retryBackoffMaxMs      = 1000
  , retryBackoffMultiplier = 2.0
  , retryBackoffJitter     = 0.2
  }

-- | Severity bucket for the structured 'Logger'.
data LogLevel
  = LogDebug
  | LogInfo
  | LogWarn
  | LogError
  deriving (Eq, Ord, Show)

-- | Structured logger callback. Sender threads call this on every
-- retriable produce error / batch send / timeout, so callers can
-- route the events to whatever observability stack they prefer.
-- 'defaultLogger' writes to stderr; 'silentLogger' is a no-op
-- suitable for tests.
type Logger = LogLevel -> Text -> IO ()

defaultLogger :: Logger
defaultLogger lvl msg =
  -- Use 'putStrLn' on stderr so library logs don't pollute stdout
  -- — the producer's record callbacks are the public success
  -- channel.
  hPutStrLn stderr (renderLogLevel lvl <> " " <> T.unpack msg)

renderLogLevel :: LogLevel -> String
renderLogLevel = \case
  LogDebug -> "[debug]"
  LogInfo  -> "[info]"
  LogWarn  -> "[warn]"
  LogError -> "[error]"

silentLogger :: Logger
silentLogger _ _ = pure ()

-- | Compute the next backoff for the given attempt number
-- (0-indexed: attempt 0 returns @retryBackoffMs@). The curve is
-- @backoff_n = min(retryBackoffMaxMs, retryBackoffMs * retryBackoffMultiplier^n)@
-- with deterministic jitter applied as a function of @n@ so two
-- runs of the same test produce identical numbers.
nextRetryBackoffMs :: RetryConfig -> Int -> Int
nextRetryBackoffMs RetryConfig{..} attempt =
  let !raw     = fromIntegral retryBackoffMs
                   * (retryBackoffMultiplier ^ max 0 attempt)
      !capped  = min (fromIntegral retryBackoffMaxMs) raw :: Double
      -- Sin-based jitter: deterministic per attempt, no PRNG, so
      -- tests can reproduce the curve exactly.
      !jit     = sin (fromIntegral attempt) * retryBackoffJitter
   in max 0 (round (capped * (1 + jit)))

-- | Per-broker pipelined-send state.
--
-- The sender holds one 'BrokerPipe' per (broker, connection)
-- pair. Each pipe owns:
--
--   * A dedicated 'Connection' (separate from
--     'producerConnManager' so the pipelined writer doesn't race
--     against the on-demand metadata refresh path that uses the
--     shared cache).
--
--   * A bounded outbox 'TBQueue OutboundProduce' that the sender
--     main loop pushes encoded ProduceRequests into; the bound
--     equals 'producerMaxInFlight' so the producer-side back-
--     pressures naturally without an extra semaphore.
--
--   * An unbounded in-flight FIFO 'TBQueue PendingProduce'
--     coordinating writer → reader hand-off. Kafka guarantees
--     responses come back in request order on a given
--     connection, so a FIFO is sufficient (no correlation-ID
--     map needed).
--
--   * A single writer 'Async' that pulls outbox items, frames
--     and writes the bytes, and pushes the matching pending
--     entry onto the in-flight FIFO.
--
--   * A single reader 'Async' that reads framed responses,
--     pulls the head of the in-flight FIFO, parses the body, and
--     dispatches per-batch callbacks via 'processProduceResponse'.
--
-- Together the writer + reader unlock pipelining: the producer
-- can stream up to 'senderMaxInFlight' ProduceRequests onto the
-- wire before any of their responses come back. Without
-- pipelining the sender pays a full broker round-trip per
-- ProduceRequest; with pipelining it pays one round-trip per
-- 'senderMaxInFlight' requests, which is what librdkafka and
-- the JVM client do via @max.in.flight.requests.per.connection@.
data BrokerPipe = BrokerPipe
  { bpAddr     :: !Conn.BrokerAddress
  , bpConn     :: !Conn.Connection
  , bpOutbox   :: !(TBQueue OutboundProduce)
  , bpInFlight :: !(TBQueue PendingProduce)
  , bpWriter   :: !(Async ())
  , bpReader   :: !(Async ())
  , bpInFlightCount :: !(TVar Int)
    -- ^ Mirrors @length(bpInFlight) + (in-write items)@ for the
    --   'flushProducer' / 'closeProducer' drain check; kept on
    --   the writer side so the main sender loop can poll it
    --   without taking the FIFO out of order.
  }

-- | Outbound work item written to a 'BrokerPipe' outbox by the
-- main sender loop. Carries the unencoded inputs; the per-broker
-- writer thread builds the ProduceRequest, runs the wire
-- encoder, and writes to the socket. Pushing the encoding off
-- the main sender loop frees the loop to fan more rounds out to
-- the writer in parallel.
data OutboundProduce = OutboundProduce
  { opCorrId          :: !Int32
  , opApiKey          :: !Int16
  , opApiVersion      :: !Int16
  , opClientId        :: !P.KafkaString
  , opTransactionalId :: !(Maybe Text)
  , opAcks            :: !Int16
  , opTimeoutMs       :: !Int32
  , opMetadata        :: !Meta.MetadataCache
  , opBatches         :: ![BA.ProducerBatch]
  , opPending         :: !PendingProduce
  }

-- | In-flight pending entry handed off from writer to reader.
-- Holds the per-batch context the response handler needs to
-- dispatch callbacks.
data PendingProduce = PendingProduce
  { ppCorrId     :: !Int32
  , ppApiVersion :: !Int16
  , ppBatches    :: ![BA.ProducerBatch]
  , ppMetaCache  :: !Meta.MetadataCache
  }

-- | State for the sender thread
data SenderState = SenderState
  { senderAccumulator :: !BA.BatchAccumulator
    -- ^ Batch accumulator to drain from
  , senderMetadata :: !Meta.MetadataCache
    -- ^ Metadata cache for broker lookup
  , senderConnManager :: !Conn.ConnectionManager
    -- ^ Connection manager for getting broker connections
  , senderRetryConfig :: !RetryConfig
    -- ^ Retry configuration
  , senderRunning :: !(TVar Bool)
    -- ^ Whether the sender thread should keep running
  , senderBusy :: !(IORef Bool)
    -- ^ True while the sender loop is mid-iteration on a drained
    --   batch (i.e. has called 'drainReadyBatches' but the broker
    --   reply for those records has not yet landed).
    --
    --   Required for correctness of 'flushProducer' /
    --   'closeProducer'\'s drain check: polling
    --   'BA.hasReadyBatches' alone races the in-flight window in
    --   which the sender has /pulled/ the batches off the
    --   accumulator but is still waiting for a 'ProduceResponse'.
    --   Without this flag a producer that 'flushProducer' +
    --   'closeProducer' immediately can lose every record that
    --   was on the wire at the time of the flush. Plain 'IORef'
    --   is sufficient because the sender thread is the only
    --   writer.
  , senderMaxInFlight :: !Int
    -- ^ Maximum concurrent ProduceRequests in flight per broker
    --   pipe. Mirrors @max.in.flight.requests.per.connection@.
  , senderBrokerPipes :: !(IORef (HashMap Conn.BrokerAddress BrokerPipe))
    -- ^ Per-broker pipelined-send state. Lazily populated on
    --   first send to a broker. Each pipe owns its own
    --   'Connection' (so the writer / reader threads don't race
    --   against the on-demand metadata refresh path that uses
    --   the shared 'producerConnManager').
  , senderAcks :: !Int16
    -- ^ Required acks: 0 = none, 1 = leader, -1 = all ISRs
  , senderTimeoutMs :: !Int32
    -- ^ Request timeout in milliseconds (used in ProduceRequest)
  , senderDeliveryTimeoutMs :: !Int32
    -- ^ Delivery timeout in milliseconds (KIP-91: total time including retries)
  , senderCompression :: !CompressionCodec
    -- ^ Compression codec to use
  , senderClientId :: !Text
    -- ^ Client ID for requests
  , senderCorrelationId :: !(IORef Int32)
    -- ^ Next correlation ID to use. Single sender thread per
    --   producer is the sole writer; SPSC counter served by
    --   'IORef' + 'atomicModifyIORef\'' instead of an STM
    --   transaction on every Produce request.
  , senderLogger :: !Logger
    -- ^ Structured logger callback; invoked on retriable / fatal
    --   produce errors. Defaults to 'defaultLogger'.
  , senderVersionCache :: !AV.ApiVersionCache
    -- ^ Per-broker negotiated API version cache. Populated on
    --   first contact with each leader broker via
    --   'VN.ensureVersionsNegotiated' so the Produce-version
    --   selection ('VN.pickApiVersion') matches what the broker
    --   actually accepts (rather than the hardcoded v3 the
    --   sender used to emit unconditionally).
  , senderTransactionalId :: !(IORef (Maybe Text))
    -- ^ Transactional id the producer is bound to.
    --
    --   Tier 3 of the STM-replacement work: single-writer (the
    --   producer's @bindTransaction@ path) / single-reader (the
    --   sender thread on every produce); 'IORef' is sufficient.
    --
    -- For transactional sends ('attrIsTransactional == True' on
    -- the batch) the broker requires the @transactionalId@
    -- field on the ProduceRequest envelope to match the txn id
    -- the producer's records claim. Sending @Null@ here makes
    -- the broker reject the produce with
    -- @TRANSACTIONAL_ID_AUTHORIZATION_FAILED@ (53) — the
    -- broker's authorization gate runs before the no-auth
    -- shortcut, and a null transactionalId on a transactional
    -- ProduceRequest is, definitionally, unauthorised.
    --
    -- 'Nothing' for non-transactional / idempotent-only
    -- producers; written by 'Producer.bindTransaction' when a
    -- 'Transaction' is bound. The sender reads this TVar each
    -- send and folds it into the request, so a producer that
    -- /unbinds/ a transaction mid-flight stops attaching the
    -- id from the next send onwards.
  }

-- | Create a new sender state
createSenderState
  :: BA.BatchAccumulator
  -> Meta.MetadataCache
  -> Conn.ConnectionManager
  -> RetryConfig
  -> Int16        -- ^ Required acks
  -> Int           -- ^ Delivery timeout (ms) - KIP-91
  -> CompressionCodec
  -> Text         -- ^ Client ID
  -> AV.ApiVersionCache
                 -- ^ Per-broker negotiated API-version cache (shared
                 --   with the producer's bootstrap handshake)
  -> Int          -- ^ Max in-flight ProduceRequests per broker pipe
                 --   ('producerMaxInFlight'). The pipelined sender
                 --   keeps up to this many ProduceRequests on the
                 --   wire before blocking the main sender loop on
                 --   the per-broker outbox bound.
  -> IO SenderState
createSenderState accumulator metadata connManager retryConfig acks deliveryTimeoutMs compression clientId versionCache maxInFlight = do
  running <- newTVarIO True
  busy <- newIORef False
  correlationId <- newIORef 0
  txnIdVar <- newIORef Nothing
  brokerPipes <- newIORef HashMap.empty
  -- Use the smaller of delivery timeout and 30 seconds for individual request timeout
  -- The delivery timeout covers all retries, while request timeout is per-request
  let requestTimeoutMs = min 30000 (fromIntegral deliveryTimeoutMs)
  return SenderState
    { senderAccumulator = accumulator
    , senderMetadata = metadata
    , senderConnManager = connManager
    , senderRetryConfig = retryConfig
    , senderRunning = running
    , senderBusy = busy
    , senderMaxInFlight = max 1 maxInFlight
    , senderBrokerPipes = brokerPipes
    , senderAcks = acks
    , senderTimeoutMs = requestTimeoutMs
    , senderDeliveryTimeoutMs = fromIntegral deliveryTimeoutMs
    , senderCompression = compression
    , senderClientId = clientId
    , senderCorrelationId = correlationId
    , senderLogger = defaultLogger
    , senderVersionCache = versionCache
    , senderTransactionalId = txnIdVar
    }

-- | Start the sender background thread
startSenderThread :: SenderState -> IO (Async ())
startSenderThread state = async $ senderLoop state

-- | Stop the sender thread gracefully and tear down every
-- per-broker pipelined send pipe (cancel writer + reader,
-- disconnect the dedicated socket).
stopSenderThread :: SenderState -> Async () -> IO ()
stopSenderThread state thread = do
  atomically $ writeTVar (senderRunning state) False
  cancel thread
  pipes <- readIORef (senderBrokerPipes state)
  forM_ (HashMap.elems pipes) $ \BrokerPipe{..} -> do
    cancel bpWriter
    cancel bpReader
    void (try (Conn.disconnect bpConn) :: IO (Either SomeException ()))
  atomicModifyIORef' (senderBrokerPipes state) $ \_ -> (HashMap.empty, ())

-- | Main sender loop
senderLoop :: SenderState -> IO ()
senderLoop state@SenderState{..} = do
  -- Check if we should continue running
  shouldRun <- atomically $ readTVar senderRunning
  
  if not shouldRun
    then return ()  -- Exit loop
    else do
      -- Check for ready batches
      hasReady <- BA.hasReadyBatches senderAccumulator
      
      if hasReady
        then do
          -- Mark the sender busy /before/ draining; the drain
          -- removes batches from the accumulator queue but the
          -- 'ProduceResponse' for them only lands inside
          -- 'sendBatches' below. 'flushProducer' /
          -- 'closeProducer' wait on (not hasReady) AND (not
          -- senderBusy) so they don't return mid-flight.
          writeIORef senderBusy True
          batches <- BA.drainReadyBatches senderAccumulator
          result <- try $ sendBatches state batches
          writeIORef senderBusy False
          case result of
            Left (e :: SomeException) ->
              senderLogger
                LogError
                (T.pack ("Sender loop error: " <> show e))
            Right () -> return ()
          senderLoop state
        else do
          threadDelay 10000  -- 10ms
          senderLoop state

-- | Send a collection of batches to their respective brokers via
-- the per-broker pipelined send pipe (see 'BrokerPipe').
--
-- Batches are first grouped by broker (the partition leader) and
-- then split into rounds where every (topic, partition) appears
-- at most once per round; each round becomes one ProduceRequest
-- (the Kafka wire spec requires one 'PartitionProduceData' per
-- partition per request).
--
-- For each round, the sender:
--   1. Builds the ProduceRequest record + serialised body.
--   2. Pushes it to the broker pipe's outbox (blocks if the
--      pipe is at 'senderMaxInFlight').
--   3. Returns immediately — the writer thread takes over and
--      hands the pending entry to the reader thread which
--      dispatches per-batch callbacks once the broker replies.
--
-- Net effect: the sender main loop can drain and queue many
-- ProduceRequests without paying a full broker round-trip per
-- request, which is the librdkafka /
-- @max.in.flight.requests.per.connection@ model.
sendBatches :: SenderState -> [BA.ProducerBatch] -> IO ()
sendBatches state@SenderState{..} batches = do
  batchesByBroker <- groupBatchesByBroker senderMetadata batches
  forM_ (Map.toList batchesByBroker) $ \(broker, brokerBatches) -> do
    pipeR <- ensureBrokerPipe state broker
    case pipeR of
      Left err ->
        forM_ brokerBatches $ \batch ->
          forM_ (BA.batchCallbacks batch) $ \cb ->
            cb (Left (T.pack ("Failed to open broker pipe: " <> err)))
      Right pipe -> do
        forM_ (roundsPerPartition brokerBatches) $ \roundBatches -> do
          result <- try $ enqueueRound state pipe broker roundBatches
          case result of
            Left (e :: SomeException) ->
              forM_ roundBatches $ \batch ->
                forM_ (BA.batchCallbacks batch) $ \cb ->
                  cb (Left ("Exception enqueueing batch: " <> T.pack (show e)))
            Right () -> pure ()
  where
    -- Split batches into rounds such that every (topic, partition)
    -- appears at most once per round. The k-th round contains the
    -- k-th batch (in arrival order) for every partition that had
    -- at least k queued.
    roundsPerPartition :: [BA.ProducerBatch] -> [[BA.ProducerBatch]]
    roundsPerPartition bs =
      let groups :: [[BA.ProducerBatch]]
          groups = groupBy (\a b -> tpKey a == tpKey b)
                 $ sortBy (comparing tpKey) bs
          tpKey :: BA.ProducerBatch -> BA.TopicPartition
          tpKey = BA.batchTopicPartition
      in transposeNonEmpty groups

    transposeNonEmpty :: [[a]] -> [[a]]
    transposeNonEmpty xss =
      case [ (h, t) | (h:t) <- xss ] of
        []   -> []
        hts  -> map fst hts : transposeNonEmpty (map snd hts)

-- | Build the ProduceRequest for one round of batches, encode it,
-- and push it to the broker's outbox. Blocks on
-- 'senderMaxInFlight' backpressure (the outbox is a bounded
-- 'TBQueue').
enqueueRound
  :: SenderState
  -> BrokerPipe
  -> Meta.BrokerMetadata
  -> [BA.ProducerBatch]
  -> IO ()
enqueueRound state@SenderState{..} pipe _broker batches = do
  -- Drop any batches that have exceeded the per-batch delivery
  -- timeout (KIP-91) before we hit the wire.
  currentTime <- KafkaTime.currentTimeMillis
  let (timedOut, valid) = partition (isBatchTimedOut currentTime senderDeliveryTimeoutMs) batches
  forM_ timedOut $ \batch -> do
    let createTime = BA.batchCreateTime batch
        elapsed    = currentTime - createTime
        msg = "Delivery timeout exceeded: batch created "
                <> T.pack (show elapsed)
                <> "ms ago, timeout is "
                <> T.pack (show senderDeliveryTimeoutMs) <> "ms"
    forM_ (BA.batchCallbacks batch) $ \cb -> cb (Left msg)
  when (not (null valid)) $ do
    -- The ApiVersions handshake was already run in
    -- 'ensureBrokerPipe' before the writer / reader threads
    -- started.
    txnIdM <- readIORef senderTransactionalId
    verR <- VN.pickApiVersionForRange @PR.ProduceRequest
              3 13 senderVersionCache (bpAddr pipe) 3
    let !apiVersion = case verR of
          Right v -> v
          Left  _ -> 3
        !apiKey      = 0
        !clientId    = P.mkKafkaString senderClientId
    cid <- atomicModifyIORef' senderCorrelationId $ \k -> (k + 1, k)
    let !pending = PendingProduce
          { ppCorrId     = cid
          , ppApiVersion = apiVersion
          , ppBatches    = valid
          , ppMetaCache  = senderMetadata
          }
        !out = OutboundProduce
          { opCorrId          = cid
          , opApiKey          = apiKey
          , opApiVersion      = apiVersion
          , opClientId        = clientId
          , opTransactionalId = txnIdM
          , opAcks            = senderAcks
          , opTimeoutMs       = senderTimeoutMs
          , opMetadata        = senderMetadata
          , opBatches         = valid
          , opPending         = pending
          }
    -- Bound on outbox depth = senderMaxInFlight provides
    -- producer-side backpressure: the writeTBQueue blocks once
    -- the pipe has 'senderMaxInFlight' requests queued or in
    -- flight, matching the librdkafka /
    -- 'max.in.flight.requests.per.connection' semantics.
    atomically $ do
      modifyTVar' (bpInFlightCount pipe) (+ 1)
      writeTBQueue (bpOutbox pipe) out

-- | Look up (or lazily open) the pipelined-send pipe for a
-- broker. Pipe construction opens its own dedicated TCP
-- connection and starts the writer + reader async threads.
ensureBrokerPipe
  :: SenderState
  -> Meta.BrokerMetadata
  -> IO (Either String BrokerPipe)
ensureBrokerPipe state@SenderState{..} brokerMeta = do
  let !addr = Meta.brokerMetaAddress brokerMeta
  existing <- HashMap.lookup addr <$> readIORef senderBrokerPipes
  case existing of
    Just pipe -> pure (Right pipe)
    Nothing -> do
      connR <- Conn.connect addr Conn.defaultConnectionConfig
      case connR of
        Left err -> pure (Left err)
        Right conn -> do
          -- Run the ApiVersions handshake on this dedicated
          -- connection /before/ the writer + reader threads
          -- start. Doing it here keeps the in-flight FIFO empty
          -- when the handshake's send + receive run, so we
          -- don't race against the pipelined produces.
          let nextCid = atomicModifyIORef' senderCorrelationId $ \cid -> (cid + 1, cid)
          _ <- VN.ensureVersionsNegotiated conn addr senderVersionCache nextCid
          outbox    <- newTBQueueIO (fromIntegral senderMaxInFlight)
          inflight  <- newTBQueueIO 65536  -- effectively unbounded
          counter   <- newTVarIO 0
          let !pipeStub = PipeBootstrap
                { pbsAddr     = addr
                , pbsConn     = conn
                , pbsOutbox   = outbox
                , pbsInFlight = inflight
                , pbsCount    = counter
                , pbsState    = state
                }
          writerA <- async (brokerWriterLoop pipeStub)
          readerA <- async (brokerReaderLoop pipeStub)
          let !pipe = BrokerPipe
                { bpAddr          = addr
                , bpConn          = conn
                , bpOutbox        = outbox
                , bpInFlight      = inflight
                , bpWriter        = writerA
                , bpReader        = readerA
                , bpInFlightCount = counter
                }
          -- CAS into the map; if a racing inserter beat us we
          -- discard our pipe (close its threads + connection)
          -- and use theirs.
          inserted <- atomicModifyIORef' senderBrokerPipes $ \m ->
            case HashMap.lookup addr m of
              Just existingPipe -> (m, Right existingPipe)
              Nothing -> (HashMap.insert addr pipe m, Left pipe)
          case inserted of
            Right p -> do
              cancel writerA
              cancel readerA
              _ <- try (Conn.disconnect conn) :: IO (Either SomeException ())
              pure (Right p)
            Left p -> pure (Right p)

-- | Bootstrap helper for the writer / reader threads.
data PipeBootstrap = PipeBootstrap
  { pbsAddr     :: !Conn.BrokerAddress
  , pbsConn     :: !Conn.Connection
  , pbsOutbox   :: !(TBQueue OutboundProduce)
  , pbsInFlight :: !(TBQueue PendingProduce)
  , pbsCount    :: !(TVar Int)
  , pbsState    :: !SenderState
  }

-- | Per-broker writer loop. Pulls outbox items, frames + writes
-- the request bytes to the wire, and pushes the matching pending
-- entry onto the in-flight FIFO so the reader knows what
-- batches the next response corresponds to.
brokerWriterLoop :: PipeBootstrap -> IO ()
brokerWriterLoop PipeBootstrap{..} = forever $ do
  out <- atomically $ readTBQueue pbsOutbox
  -- Encoding lives here (in the writer thread) rather than in
  -- the main sender loop so the main loop can fan more rounds
  -- out to this thread in parallel with us hitting the wire.
  request <- buildProduceRequest
               (opMetadata out)
               (opTransactionalId out)
               (opAcks out)
               (opTimeoutMs out)
               (opBatches out)
  let !requestBody = WC.runEncodeVer @PR.ProduceRequest (opApiVersion out) request
      !framed      = Req.frameRequest
                       (opApiKey out)
                       (opApiVersion out)
                       (opCorrId out)
                       (opClientId out)
                       requestBody
  result <- try (Req.sendRawRequest pbsConn framed)
              :: IO (Either SomeException ())
  case result of
    Left e -> do
      -- Wire write failed; surface to all the round's batches
      -- and don't enqueue a pending entry (no response will
      -- ever come). The reader thread keeps going against
      -- whatever is already in flight; a subsequent request
      -- on this pipe will hit the broken socket and surface.
      let !errMsg = "Wire write failed: " <> T.pack (show (e :: SomeException))
      forM_ (ppBatches (opPending out)) $ \batch ->
        forM_ (BA.batchCallbacks batch) $ \cb -> cb (Left errMsg)
      atomically $ modifyTVar' pbsCount (subtract 1)
    Right () ->
      atomically $ writeTBQueue pbsInFlight (opPending out)

-- | Per-broker reader loop. Reads framed responses off the wire,
-- pulls the head of the in-flight FIFO (Kafka guarantees
-- per-connection in-order responses), parses the body, and
-- dispatches per-batch callbacks via 'processProduceResponse'.
brokerReaderLoop :: PipeBootstrap -> IO ()
brokerReaderLoop pbs@PipeBootstrap{..} = do
  rawR <- try (Req.receiveRawResponse pbsConn) :: IO (Either SomeException BS.ByteString)
  case rawR of
    Left e -> do
      -- Drain in-flight: surface a transport error to whatever
      -- is left waiting. Then exit the loop (the broken
      -- connection means the writer's next write will also
      -- fail and the user will see an error from the next
      -- send).
      drained <- atomically $ flushTBQueueAll pbsInFlight
      forM_ drained $ \pp ->
        forM_ (ppBatches pp) $ \batch ->
          forM_ (BA.batchCallbacks batch) $ \cb ->
            cb (Left ("Wire read failed: " <> T.pack (show (e :: SomeException))))
      atomically $ writeTVar pbsCount 0
      void (try (Conn.disconnect pbsConn) :: IO (Either SomeException ()))
    Right raw -> do
      pp <- atomically $ readTBQueue pbsInFlight
      let parsed = Req.parseResponseFrame 0 (ppApiVersion pp) raw
      case parsed of
        Left err ->
          forM_ (ppBatches pp) $ \batch ->
            forM_ (BA.batchCallbacks batch) $ \cb ->
              cb (Left ("Response decode error: " <> T.pack err))
        Right (responseCorrId, body)
          | responseCorrId /= ppCorrId pp ->
              forM_ (ppBatches pp) $ \batch ->
                forM_ (BA.batchCallbacks batch) $ \cb ->
                  cb (Left "Correlation ID mismatch")
          | otherwise ->
              case WC.runDecodeVer @PResp.ProduceResponse (ppApiVersion pp) body of
                Left err ->
                  forM_ (ppBatches pp) $ \batch ->
                    forM_ (BA.batchCallbacks batch) $ \cb ->
                      cb (Left ("Failed to parse response: " <> T.pack err))
                Right resp ->
                  processProduceResponse (Just (ppMetaCache pp)) (ppBatches pp) resp
      atomically $ modifyTVar' pbsCount (subtract 1)
      brokerReaderLoop pbs

-- | Atomically drain a 'TBQueue' to a list (returning items in
-- FIFO order). Used by the broker reader's error path.
flushTBQueueAll :: TBQueue a -> STM [a]
flushTBQueueAll q = do
  empty <- isEmptyTBQueue q
  if empty
    then pure []
    else do
      x <- readTBQueue q
      xs <- flushTBQueueAll q
      pure (x : xs)

-- | Total number of ProduceRequests currently queued or in-flight
-- across every broker pipe. The producer's
-- 'Kafka.Client.Producer.flushProducer' /
-- 'Kafka.Client.Producer.closeProducer' wait paths consult this
-- to know when every record has been acked by the broker (the
-- pipelined writer / reader hand-off makes 'BA.hasReadyBatches'
-- false the moment the sender pulls batches off the accumulator,
-- so we need a separate signal that the broker has finished
-- replying for them).
senderTotalInFlight :: SenderState -> IO Int
senderTotalInFlight SenderState{..} = do
  pipes <- readIORef senderBrokerPipes
  -- One STM transaction per pipe: cheap; the producer's drain
  -- loop polls this every 100ms.
  fmap sum $ forM (HashMap.elems pipes) $ \BrokerPipe{..} ->
    atomically (readTVar bpInFlightCount)

-- | Group batches by their target broker
groupBatchesByBroker
  :: Meta.MetadataCache
  -> [BA.ProducerBatch]
  -> IO (Map Meta.BrokerMetadata [BA.ProducerBatch])
groupBatchesByBroker metadata batches = do
  -- Look up leader for each batch's topic-partition
  batchesWithBroker <- forM batches $ \batch -> do
    let tp = BA.batchTopicPartition batch
        topic = BA.tpTopic tp
        partition = BA.tpPartition tp
    
    brokerM <- atomically $ Meta.getPartitionLeader metadata topic partition
    return (brokerM, batch)
  
  -- Handle batches with unknown leaders by invoking error callbacks
  let unknownLeaderBatches = [batch | (Nothing, batch) <- batchesWithBroker]
  forM_ unknownLeaderBatches $ \batch -> do
    let tp = BA.batchTopicPartition batch
        errorMsg = "Unknown leader for topic " <> BA.tpTopic tp <> 
                   " partition " <> T.pack (show (BA.tpPartition tp))
        callbacks = BA.batchCallbacks batch
    forM_ callbacks $ \callback -> callback (Left errorMsg)
  
  -- Group by broker, filtering out batches with unknown leaders
  let validBatches = [(broker, batch) | (Just broker, batch) <- batchesWithBroker]
      grouped = groupBy (\(b1, _) (b2, _) -> b1 == b2)
              $ sortBy (comparing fst) validBatches
  
  return $ Map.fromList $ map (\g -> (fst (head g), map snd g)) grouped

-- | Check if a batch has exceeded the delivery timeout (KIP-91)
isBatchTimedOut :: Int64 -> Int32 -> BA.ProducerBatch -> Bool
isBatchTimedOut currentTime deliveryTimeoutMs batch =
  let createTime = BA.batchCreateTime batch
      elapsed = currentTime - createTime
  in elapsed > fromIntegral deliveryTimeoutMs

-- | Build a ProduceRequest from a list of batches.
-- Compression happens here, so this is an IO operation.
-- The metadata cache is consulted per-topic to populate the
-- KIP-516 TopicId field; on pre-v10 brokers (or topics not yet
-- in the cache) the field stays 'P.nullUuid' which is fine for
-- v0-v12 (where the encoder ignores it). v13+ requires a real
-- topic id.
--
-- The @transactionalId@ is plumbed in from the caller (read off
-- the 'SenderState' TVar populated by @bindTransaction@). When
-- 'Just' it goes onto the request envelope verbatim; @Nothing@
-- sends @KafkaString Null@. The broker requires this to match
-- the txn id any transactional records in the request claim or
-- it rejects with @TRANSACTIONAL_ID_AUTHORIZATION_FAILED@ (53).
buildProduceRequest
  :: Meta.MetadataCache
  -> Maybe Text
  -> Int16
  -> Int32
  -> [BA.ProducerBatch]
  -> IO PR.ProduceRequest
buildProduceRequest metaCache transactionalIdM acks timeoutMs batches = do
  -- Group batches by topic
  let batchesByTopic = groupBy (\b1 b2 -> BA.tpTopic (BA.batchTopicPartition b1) == BA.tpTopic (BA.batchTopicPartition b2))
                     $ sortBy (comparing (BA.tpTopic . BA.batchTopicPartition)) batches

  -- Build TopicProduceData for each topic (with compression, so IO)
  topicData <- mapM (buildTopicProduceData metaCache) batchesByTopic

  return $ PR.ProduceRequest
    { PR.produceRequestTransactionalId = case transactionalIdM of
        Nothing  -> P.KafkaString P.Null
        Just tid -> P.mkKafkaString tid
    , PR.produceRequestAcks = acks
    , PR.produceRequestTimeoutMs = timeoutMs
    , PR.produceRequestTopicData = P.mkKafkaArray (V.fromList topicData)
    }

-- | Build TopicProduceData for a group of batches from the same topic.
-- Compression happens here, so this is an IO operation. Populates
-- the KIP-516 'topicProduceDataTopicId' from the metadata cache
-- when known; falls back to 'P.nullUuid' otherwise (which is
-- what every Produce version through v12 expects in the field).
buildTopicProduceData
  :: Meta.MetadataCache -> [BA.ProducerBatch] -> IO PR.TopicProduceData
buildTopicProduceData metaCache batches = do
  let topic = BA.tpTopic $ BA.batchTopicPartition $ head batches
  topicIdM <- atomically (Meta.getTopicId metaCache topic)
  -- 'sendBatches' upstream guarantees that each (topic,
  -- partition) appears at most once in this batch list, so a
  -- straight per-batch mapM produces one PartitionProduceData per
  -- partition.
  partitionData <- mapM buildPartitionProduceData batches
  return $ PR.TopicProduceData
    { PR.topicProduceDataName = P.mkKafkaString topic
    , PR.topicProduceDataTopicId =
        -- 'getTopicId' returns 'Nothing' before the cache is
        -- populated; that path keeps the field at nullUuid
        -- which is what v0-v12 expect anyway.
        case topicIdM of
          Just tid -> tid
          Nothing  -> P.nullUuid
    , PR.topicProduceDataPartitionData = P.mkKafkaArray (V.fromList partitionData)
    }

-- | Build PartitionProduceData for a single batch.
-- Applies compression using the batch's compression codec and level.
--
-- 'sendBatches' upstream guarantees that each round contains at
-- most one batch per (topic, partition), so the pre-fix shape of
-- "one PartitionProduceData per ProducerBatch" is now safe — the
-- protocol's "one PartitionProduceData per partition per request"
-- invariant is upheld by the round splitter rather than by
-- coalescing here.
buildPartitionProduceData :: BA.ProducerBatch -> IO PR.PartitionProduceData
buildPartitionProduceData batch = do
  let partition = BA.tpPartition $ BA.batchTopicPartition batch
      recordBatch = buildRecordBatch batch
      compressionLevel = BA.batchCompressionLevel batch
      codec = RB.attrCompressionType (RB.batchAttributes recordBatch)
  if codec == NoCompression
    then pure $ PR.PartitionProduceData
      { PR.partitionProduceDataIndex   = partition
      , PR.partitionProduceDataRecords = P.mkKafkaBytes (RBW.encodeRecordBatchWire recordBatch)
      }
    else do
      encodeResult <- RBW.encodeRecordBatchWireCompressedWithLevel recordBatch compressionLevel
      case encodeResult of
        Left err -> do
          hPutStrLn stderr
            ("[warn] producer: compression failed for batch, "
              <> "sending uncompressed: " <> err)
          pure $ PR.PartitionProduceData
            { PR.partitionProduceDataIndex   = partition
            , PR.partitionProduceDataRecords = P.mkKafkaBytes (RBW.encodeRecordBatchWire recordBatch)
            }
        Right recordBytes ->
          pure $ PR.PartitionProduceData
            { PR.partitionProduceDataIndex   = partition
            , PR.partitionProduceDataRecords = P.mkKafkaBytes recordBytes
            }

-- | Build a RecordBatch from a ProducerBatch. Idempotent /
-- transactional state is read off the batch itself
-- ('batchProducerId', 'batchProducerEpoch', 'batchBaseSequence',
-- 'batchIsTransactional'); the producer-side 'sendMessage' /
-- 'sendBatch' path stamps these fields when @producerIdempotent@
-- or @producerTransactional@ is enabled, via 'BA.appendRecordStamped'.
buildRecordBatch :: BA.ProducerBatch -> RB.RecordBatch
buildRecordBatch batch =
  let !recSeq = BA.batchRecords batch
      !nRec = Seq.length recSeq
      -- The producer-side append path leaves 'recordOffsetDelta'
      -- at 0 because the offset within the batch is only known
      -- once the batch is sealed (the same record could in
      -- principle live in different positions across retries).
      -- Stamp the per-record delta here, immediately before
      -- handing the records to the wire encoder; without it the
      -- broker sees N records all claiming delta 0 and silently
      -- collapses them, which is the data-loss bug the
      -- 'PartitionProduceData' consolidation in
      -- 'buildTopicProduceData' was supposed to expose.
      !records = V.generate nRec $ \i ->
        let !r = Seq.index recSeq i
        in r { RB.recordOffsetDelta = fromIntegral i }
      attrs = RB.Attributes
        { RB.attrCompressionType = BA.batchCompression batch
        , RB.attrTimestampType = RB.CreateTime
        , RB.attrIsTransactional = BA.batchIsTransactional batch
        , RB.attrIsControl = False
        , RB.attrHasDeleteHorizon = False
        }
      baseTimestamp = BA.batchBaseTimestamp batch
      -- Single fold instead of (V.toList + map + maximum +
      -- intermediate list). 'V.foldl'' walks the vector in one
      -- pass and computes the max delta directly.
      !maxTimestamp =
        if nRec == 0
          then baseTimestamp
          else baseTimestamp
                 + V.foldl' (\acc r -> max acc (RB.recordTimestampDelta r))
                            0 records
  in
    RB.RecordBatch
      { RB.batchBaseOffset = 0  -- Broker will assign
      , RB.batchPartitionLeaderEpoch = RB.noPartitionLeaderEpoch
      , RB.batchAttributes = attrs
      , RB.batchLastOffsetDelta = fromIntegral (V.length records) - 1
      , RB.batchBaseTimestamp = baseTimestamp
      , RB.batchMaxTimestamp = maxTimestamp
      , RB.batchProducerId = BA.batchProducerId batch
      , RB.batchProducerEpoch = BA.batchProducerEpoch batch
      , RB.batchBaseSequence = BA.batchBaseSequence batch
      , RB.batchRecords = records
      }

-- | Determine if a batch should be retried. Compares the batch's
-- accrued 'BA.batchAttempts' against the sender's configured
-- 'retryMaxAttempts'. The sender increments 'batchAttempts' via
-- 'bumpBatchAttempts' before re-enqueuing the batch.
shouldRetry :: SenderState -> BA.ProducerBatch -> Bool
shouldRetry SenderState{..} batch =
  BA.batchAttempts batch < retryMaxAttempts senderRetryConfig

-- | Compute the next backoff for /this specific/ batch's accrued
-- attempt count.
batchBackoffMs :: SenderState -> BA.ProducerBatch -> Int
batchBackoffMs SenderState{..} batch =
  nextRetryBackoffMs senderRetryConfig (BA.batchAttempts batch)

-- | Increment the batch's attempt counter, returning the updated
-- batch. The sender calls this just before re-enqueueing the
-- batch onto the partition queue.
bumpBatchAttempts :: BA.ProducerBatch -> BA.ProducerBatch
bumpBatchAttempts batch =
  batch { BA.batchAttempts = BA.batchAttempts batch + 1 }

-- | Retry a failed batch: wait for this batch's exponential
-- backoff, then bump its attempt counter. The caller is
-- responsible for actually re-enqueueing the returned batch.
retryBatch :: SenderState -> BA.ProducerBatch -> IO BA.ProducerBatch
retryBatch state batch = do
  let backoffMs = batchBackoffMs state batch
  threadDelay (backoffMs * 1000)
  pure (bumpBatchAttempts batch)

-- | Process a ProduceResponse and update batch states. Honours
-- the broker's @ThrottleTimeMs@ (KIP-219): if the broker has
-- requested back-pressure, the sender thread sleeps for that
-- duration /before/ returning to its loop, so the next produce
-- request waits the broker-requested interval. This is the same
-- behaviour the JVM client / librdkafka implement.
processProduceResponse
  :: Maybe Meta.MetadataCache  -- ^ Optional metadata cache for KIP-466 leader-cache patches
  -> [BA.ProducerBatch]
  -> PResp.ProduceResponse
  -> IO ()
processProduceResponse metaCacheM batches response = do
  -- KIP-219 throttle. Negative / zero values are no-ops.
  let throttleMs = PResp.produceResponseThrottleTimeMs response
  when (throttleMs > 0) $
    threadDelay (fromIntegral throttleMs * 1000)

  -- Extract topic responses
  let topics = case P.unKafkaArray (PResp.produceResponseResponses response) of
        P.Null -> V.empty
        P.NotNull vec -> vec

  -- Pre-index outbound batches by their (topic, partition) so
  -- the per-response lookup is O(1).
  let batchIndex :: HashMap BA.TopicPartition [BA.ProducerBatch]
      !batchIndex = HashMap.fromListWith (++)
        [ (BA.batchTopicPartition b, [b]) | b <- batches ]

  -- Process each topic response
  V.forM_ topics $ \topicResp -> do
    let topicName = extractText $ PResp.topicProduceResponseName topicResp
        partitions = case P.unKafkaArray (PResp.topicProduceResponsePartitionResponses topicResp) of
          P.Null -> V.empty
          P.NotNull vec -> vec

    -- Process each partition response
    V.forM_ partitions $ \partResp -> do
      let partitionId = PResp.partitionProduceResponseIndex partResp
          errorCode = PResp.partitionProduceResponseErrorCode partResp
          baseOffset = PResp.partitionProduceResponseBaseOffset partResp
          curLeader   = PResp.partitionProduceResponseCurrentLeader partResp
          curLeaderId = PResp.leaderIdAndEpochLeaderId curLeader

      -- KIP-466: if the broker reports a CurrentLeader for this
      -- partition (>= 0), patch the metadata cache so the next
      -- produce goes to that broker without an extra
      -- MetadataRequest round-trip.
      case metaCacheM of
        Just cache | curLeaderId >= 0 ->
          atomically $
            Meta.updatePartitionLeader cache topicName partitionId curLeaderId
        _ -> pure ()

      let !key = BA.TopicPartition topicName partitionId
          matchingBatches = HashMap.lookupDefault [] key batchIndex

      -- Update batch state based on error code
      forM_ matchingBatches $ \batch -> do
        if errorCode == 0
          then do
            -- Success: complete each record callback with its
            -- assigned offset. We deliberately don't log on every
            -- success — application code observes deliveries via
            -- the per-record callbacks, which are the public
            -- success channel.
            let callbacks = BA.batchCallbacks batch
            timestamp <- KafkaTime.currentTimeMillis
            -- (Per-record timestamps aren't surfaced in
            -- ProduceResponse; the per-record
            -- 'PartitionProduceResponse.LogAppendTimeMs' is
            -- only set when the topic is in @LogAppendTime@ mode,
            -- and we can't tell here, so we use the wall-clock
            -- timestamp as a reasonable fallback.)
            -- 'callbacks' is a 'Seq', so we walk the sequence
            -- with its index to assign each record's offset.
            forM_ (Seq.mapWithIndex (,) callbacks) $ \(idx, callback) -> do
              let recordOffset = baseOffset + fromIntegral (idx :: Int)
                  metadata = (topicName, partitionId, recordOffset, timestamp)
              callback (Right metadata)
          else do
            hPutStrLn stderr
              ("[error] producer: error sending batch to "
                <> T.unpack topicName <> " partition "
                <> show partitionId
                <> ": error code " <> show errorCode)
            let callbacks = BA.batchCallbacks batch
                errorMsg = "Kafka error: " <> T.pack (show errorCode)
            forM_ callbacks $ \callback -> do
              callback (Left errorMsg)

-- | Extract text from a KafkaString
extractText :: P.KafkaString -> Text
extractText (P.KafkaString P.Null) = ""
extractText (P.KafkaString (P.NotNull t)) = t

