{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Client.Pipeline
Description : Request pipelining for Kafka client
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements request pipelining: multiple in-flight requests
to the same broker connection, correlation-id routing of responses,
and configurable backpressure.

== Threads

A live 'Pipeline' owns three async threads:

  * the /send loop/ drains the request queue, writes wire bytes with
    a 4-byte big-endian length prefix, and flushes the connection;
  * the /receive loop/ reads length-prefixed responses, peels the
    correlation id off the head, and routes the body to the matching
    pending request;
  * the /timeout loop/ wakes once a second to fail any pending
    request whose elapsed time has exceeded 'pipelineTimeout'.

== Backpressure

'sendRequest' blocks (in STM) when 'pipelineMaxInFlight' has been
reached, and / or when the send queue has accumulated
'pipelineMaxQueueSize' items. The block is released as the receive
or timeout loops drain pending entries.

== Thread safety

Every state field is a 'TVar' / 'TQueue' / 'TMVar', and all
transitions are atomic. Multiple producer threads can call
'sendRequest' concurrently; correlation ids are assigned under the
same STM transaction that registers the pending request.

== Sample

@
pipe <- createPipeline conn defaultPipelineConfig
respE <- sendAndWait pipe requestBytes
case respE of
  Right body -> ...
  Left  err  -> ...
closePipeline pipe
@
-}
module Kafka.Client.Pipeline
  ( -- * Pipeline Types
    Pipeline
  , pipelineConnection
  , PipelineConfig(..)
  , RequestId
  , ResponseSlot
    -- * Pipeline Creation
  , createPipeline
  , closePipeline
    -- * Request/Response Operations
  , sendRequest
  , waitResponse
  , sendAndWait
    -- * Pipeline Statistics
  , PipelineStats(..)
  , getPipelineStats
    -- * Default Configuration
  , defaultPipelineConfig
    -- * KIP-368 mid-session re-authentication
  , withPausedPipeline
  , pausePipeline
  , resumePipeline
  , isPipelinePaused
  , awaitPipelineDrained
  , attachReauthDriver
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, when)
import qualified Data.Binary.Put as BP
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Int
import GHC.Generics (Generic)
import qualified Kafka.Time as KafkaTime
import qualified Kafka.Client.ReauthDriver
import Kafka.Network.Connection
  ( Connection
  , connectionPut
  , connectionClose
  )
import qualified Kafka.Network.Connection.Internal as Conn (connDuplex)

import qualified Kafka.Network.FrameParser as FrameParser
import qualified Wireform.Network as WN
import Wireform.Parser.Driver (LoopControl (..))
import Wireform.Parser.Error (ParseError (..))
import qualified Wireform.Transport.Config as WC
import Wireform.Transport.Config (profileConfig, Profile (..))

-- | Unique identifier for a pipelined request.
type RequestId = Int32

-- | Pipeline configuration.
data PipelineConfig = PipelineConfig
  { pipelineMaxInFlight :: !Int
    -- ^ Maximum number of in-flight requests (default: 100)
  , pipelineMaxQueueSize :: !Int
    -- ^ Maximum size of request queue (default: 1000)
  , pipelineTimeout :: !Int
    -- ^ Request timeout in seconds (default: 30)
  , pipelineRingSize :: !Int
    -- ^ Magic-ring receive buffer size, in bytes (default: 16 MiB).
    --   MUST be at least as large as the largest single Kafka
    --   response the broker will return (e.g. the configured
    --   @fetch.max.bytes@ + protocol overhead), otherwise the
    --   streaming frame parser deadlocks waiting for bytes the
    --   ring cannot hold.  The magic-ring constructor rounds this
    --   up to the next power-of-two page-aligned multiple, and
    --   the underlying mmap is virtual address space only —
    --   physical memory is only paged in for the bytes actually
    --   touched, so over-provisioning is cheap on Linux.
  } deriving (Eq, Show, Generic)

-- | Default pipeline configuration.
defaultPipelineConfig :: PipelineConfig
defaultPipelineConfig = PipelineConfig
  { pipelineMaxInFlight = 100
  , pipelineMaxQueueSize = 1000
  , pipelineTimeout = 30
  , pipelineRingSize = 16 * 1024 * 1024
  }

-- | Pending request awaiting response. The body 'TMVar' resolves
-- to either the bytes the broker returned or a textual error
-- (timeout, pipeline closed, send failure, decode failure).
data PendingRequest = PendingRequest
  { pendingRequestId :: !RequestId
  , pendingResponse  :: !(TMVar (Either String ByteString))
  , pendingTimestamp :: !Int
    -- ^ Unix-time seconds at which the request was queued; the
    --   timeout loop uses this as the start of the timeout window.
  }

-- | One queued request: its correlation id stamped into the wire
-- bytes plus the response slot to fill on completion.
data SendItem = SendItem
  { siCorrelationId :: !RequestId
  , siBytes         :: !ByteString
  }

-- | Request pipeline state.
data Pipeline = Pipeline
  { pipelineConnection :: !Connection
  , pipelineConfig     :: !PipelineConfig
  , pipelineNextId     :: !(IORef RequestId)
    -- ^ Monotonic correlation-id source. Pre-Tier-1 this lived in
    --   STM so 'allocateAndQueue' could allocate the next id and
    --   register the pending entry under one transaction. Allocation
    --   moved to IO ('atomicModifyIORef\'') with a snapshot-based
    --   collision check inside 'sendRequest'; the 32-bit id space
    --   against a bounded inflight count makes the residual race
    --   negligible (and 'sendRequest' retries on the lookup miss).
  , pipelinePending    :: !(TVar (IntMap PendingRequest))
    -- ^ Outstanding requests keyed on correlation id; every
    -- response from the broker triggers a 'lookup' + 'delete'.
  , pipelineSendQueue  :: !(TQueue SendItem)
  , pipelineStats      :: !(TVar PipelineStats)
  , pipelineClosed     :: !(TVar Bool)
  , pipelineThreads    :: !(TVar [Async ()])
  , pipelinePaused     :: !(TVar Bool)
    -- ^ When 'True' the send loop blocks instead of draining
    --   'pipelineSendQueue'. Used by 'withPausedPipeline' to
    --   serialise the KIP-368 mid-session re-authentication
    --   handshake against in-flight requests.
  }

-- | Pipeline statistics for monitoring.
data PipelineStats = PipelineStats
  { statsRequestsSent      :: !Int
  , statsResponsesReceived :: !Int
  , statsRequestsTimedOut  :: !Int
  , statsCurrentInFlight   :: !Int
  , statsQueueSize         :: !Int
  } deriving (Eq, Show, Generic)

----------------------------------------------------------------------
-- Lifecycle
----------------------------------------------------------------------

-- | Create a new request pipeline. Spawns the send / receive /
-- timeout threads; they tear down cleanly when 'closePipeline' is
-- called.
createPipeline
  :: Connection
  -> PipelineConfig
  -> IO Pipeline
createPipeline conn config = do
  nextId    <- newIORef 0
  pending   <- newTVarIO IntMap.empty
  sendQueue <- newTQueueIO
  stats     <- newTVarIO emptyStats
  closed    <- newTVarIO False
  threads   <- newTVarIO []
  paused    <- newTVarIO False
  let pipe = Pipeline
        { pipelineConnection = conn
        , pipelineConfig     = config
        , pipelineNextId     = nextId
        , pipelinePending    = pending
        , pipelineSendQueue  = sendQueue
        , pipelineStats      = stats
        , pipelineClosed     = closed
        , pipelineThreads    = threads
        , pipelinePaused     = paused
        }
  sa <- async (sendLoop pipe)
  ra <- async (receiveLoop pipe)
  ta <- async (timeoutLoop pipe)
  atomically $ writeTVar (pipelineThreads pipe) [sa, ra, ta]
  pure pipe

emptyStats :: PipelineStats
emptyStats = PipelineStats 0 0 0 0 0

-- | Close a pipeline. Stops the send/receive/timeout threads,
-- fails every still-pending request with @"pipeline closed"@, and
-- closes the underlying 'Connection'. Idempotent.
closePipeline :: Pipeline -> IO ()
closePipeline Pipeline{..} = do
  alreadyClosed <- atomically $ do
    c <- readTVar pipelineClosed
    if c then pure True else writeTVar pipelineClosed True $> False
  unless alreadyClosed $ do
    -- Wake any pending request with a closed-pipeline error.
    atomically $ do
      m <- readTVar pipelinePending
      mapM_
        (\PendingRequest{..} ->
            tryPutTMVar pendingResponse (Left "pipeline closed"))
        (IntMap.elems m)
      writeTVar pipelinePending IntMap.empty
    -- Cancel background threads.
    ts <- readTVarIO pipelineThreads
    mapM_ cancel ts
    -- Close the underlying connection (best-effort; the threads
    -- may have already noticed the EOF).
    _ <- try (connectionClose pipelineConnection) :: IO (Either SomeException ())
    pure ()
  where
    ($>) :: STM a -> b -> STM b
    m $> b = m >> pure b

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

-- | A response slot returned by 'sendRequest'. Either the
-- response bytes the broker sent, or a textual error string
-- describing why the slot will never be filled (timeout,
-- pipeline closed, etc.). 'waitResponse' blocks on it.
type ResponseSlot = TMVar (Either String ByteString)

-- | Queue a request for sending. The caller supplies a /builder/
-- that takes the pipeline-allocated correlation id and produces
-- the bytes that go on the wire — Kafka requests embed the
-- correlation id in their header, so the pipeline has to thread
-- the assigned id back to the caller before serialising. The
-- returned 'ResponseSlot' is an empty 'TMVar' that will be filled
-- by the receive (or timeout) thread; pass it to 'waitResponse'
-- to block on it. Returns the assigned id alongside the slot.
sendRequest
  :: Pipeline
  -> (RequestId -> ByteString)
  -> IO (Either String (RequestId, ResponseSlot))
sendRequest pipe@Pipeline{..} builder = do
  closed <- readTVarIO pipelineClosed
  if closed
    then pure (Left "pipeline closed")
    else do
      now <- nowSeconds
      -- Allocate the correlation id outside STM. The collision
      -- check that 'nextFreeCorrelationId' used to do inside the
      -- transaction has moved to a snapshot read of 'pipelinePending'
      -- here; in the (practically unreachable) event a concurrent
      -- request inserted the same id between the snapshot and the
      -- STM commit, 'allocateAndQueue' aborts with
      -- 'CidCollision' and we retry from the top.
      sendRequestRetry pipe builder now (32 :: Int)

sendRequestRetry
  :: Pipeline
  -> (RequestId -> ByteString)
  -> Int
  -> Int
  -> IO (Either String (RequestId, ResponseSlot))
sendRequestRetry _    _       _   0 = pure (Left "no free correlation ids")
sendRequestRetry pipe@Pipeline{..} builder now n = do
  pendingSnap <- readTVarIO pipelinePending
  cid <- nextFreeCorrelationIdIO pipelineNextId pendingSnap
  res <- atomically (allocateAndQueue pipe cid builder now)
  case res of
    Left "cid collision" -> sendRequestRetry pipe builder now (n - 1)
    other                -> pure other

allocateAndQueue
  :: Pipeline
  -> RequestId
  -> (RequestId -> ByteString)
  -> Int
  -> STM (Either String (RequestId, ResponseSlot))
allocateAndQueue Pipeline{..} cid builder now = do
  c <- readTVar pipelineClosed
  if c
    then pure (Left "pipeline closed")
    else do
      -- Backpressure: block until both inflight + queue have headroom.
      qSize <- queueSize pipelineSendQueue
      pendingMap <- readTVar pipelinePending
      let !inFlight = IntMap.size pendingMap
      check (qSize <  pipelineMaxQueueSize pipelineConfig)
      check (inFlight < pipelineMaxInFlight pipelineConfig)
      if IntMap.member (fromIntegral cid) pendingMap
        then pure (Left "cid collision")
        else do
          slot <- newEmptyTMVar
          let !req = PendingRequest
                { pendingRequestId = cid
                , pendingResponse  = slot
                , pendingTimestamp = now
                }
          writeTVar pipelinePending
            (IntMap.insert (fromIntegral cid) req pendingMap)
          let !bytes = builder cid
          writeTQueue pipelineSendQueue (SendItem cid bytes)
          modifyTVar' pipelineStats $ \s -> s
            { statsRequestsSent  = statsRequestsSent s + 1
            , statsCurrentInFlight = inFlight + 1
            , statsQueueSize     = qSize + 1
            }
          pure (Right (cid, slot))

queueSize :: TQueue a -> STM Int
queueSize q = go 0 []
  where
    -- Drain into a list, count, then put back. STM is atomic so
    -- the sender thread sees a consistent count.
    go !n acc = do
      mh <- tryReadTQueue q
      case mh of
        Nothing -> do
          mapM_ (writeTQueue q) (reverse acc)
          pure n
        Just h -> go (n + 1) (h : acc)

-- | IO sibling of the (now-removed) STM 'nextFreeCorrelationId'.
-- Walks the counter forward via 'atomicModifyIORef\'' until either
-- (a) it lands on an id not currently in the supplied pending
-- snapshot or (b) it has skipped 32 ids without finding one
-- (practically impossible: 32-bit space against a bounded
-- 'pipelineMaxInFlight'). The lookup is against a snapshot, not
-- the live STM 'pipelinePending'; 'allocateAndQueue' rechecks the
-- live map under STM and reports 'CidCollision' so 'sendRequest'
-- can retry, keeping the externally observable behaviour
-- equivalent to the pre-Tier-1 STM allocator.
nextFreeCorrelationIdIO
  :: IORef RequestId -> IntMap a -> IO RequestId
nextFreeCorrelationIdIO ref m = go (32 :: Int)
  where
    go 0 = pure 0
    go k = do
      cid <- atomicModifyIORef' ref $ \c ->
        let !next = if c == maxBound then 0 else c + 1
        in (next, c)
      if IntMap.member (fromIntegral cid) m
        then go (k - 1)
        else pure cid

-- | Wait for a response on the given slot. Blocks until the
-- receive loop fills it or the timeout thread fails it.
waitResponse
  :: ResponseSlot
  -> IO (Either String ByteString)
waitResponse slot = atomically (takeTMVar slot)

-- | Send + wait helper. The 'builder' argument matches
-- 'sendRequest': it receives the pipeline-allocated correlation
-- id so the caller can stamp it into the request header.
sendAndWait
  :: Pipeline
  -> (RequestId -> ByteString)
  -> IO (Either String ByteString)
sendAndWait pipe builder = do
  sendResult <- sendRequest pipe builder
  case sendResult of
    Left  err           -> pure (Left err)
    Right (_cid, slot)  -> waitResponse slot

-- | Snapshot the current statistics.
getPipelineStats :: Pipeline -> IO PipelineStats
getPipelineStats Pipeline{..} = readTVarIO pipelineStats

----------------------------------------------------------------------
-- KIP-368 mid-session re-authentication
----------------------------------------------------------------------

-- | Pause the pipeline's send loop. New 'sendRequest' calls
-- still accept work and queue it, but no bytes hit the wire
-- until 'resumePipeline'. Used by KIP-368 mid-session
-- re-authentication to gate the wire while a fresh SASL
-- handshake runs.
pausePipeline :: Pipeline -> IO ()
pausePipeline Pipeline{..} = atomically (writeTVar pipelinePaused True)

-- | Unpause the pipeline. The send thread wakes up and drains
-- the accumulated 'pipelineSendQueue' immediately.
resumePipeline :: Pipeline -> IO ()
resumePipeline Pipeline{..} = atomically (writeTVar pipelinePaused False)

isPipelinePaused :: Pipeline -> IO Bool
isPipelinePaused Pipeline{..} = readTVarIO pipelinePaused

-- | Block until 'pipelinePending' is empty (i.e. every
-- previously-sent request has either resolved or timed out).
-- Used by 'withPausedPipeline' as the second half of the
-- drain barrier: pausing alone gates new sends, this blocks
-- the caller until in-flight requests retire.
awaitPipelineDrained :: Pipeline -> IO ()
awaitPipelineDrained Pipeline{..} = atomically $ do
  m <- readTVar pipelinePending
  when (not (IntMap.null m)) retry

-- | Run an action while the pipeline is paused and all
-- previously-issued requests have completed. Mirrors the JVM
-- client's KIP-368 contract: the caller pauses new sends,
-- waits for the in-flight set to drain, runs the fresh
-- @SaslHandshake@ + @SaslAuthenticate@ exchange directly on
-- 'pipelineConnection', and resumes the pipeline so queued
-- requests catch up.
--
-- Pausing is best-effort: if the pipeline is already closed
-- when this is called, the action runs anyway against the
-- (now-defunct) connection — callers that want to gate on
-- closure should check 'isPipelinePaused' / pipeline status
-- before invoking. We always 'resumePipeline' in the bracket
-- finaliser so a thrown exception inside the action doesn't
-- leave the pipeline permanently parked.
withPausedPipeline
  :: forall a
   . Pipeline
  -> (Connection -> IO a)
  -> IO a
withPausedPipeline pipe action = do
  pausePipeline pipe
  awaitPipelineDrained pipe
  (result :: Either SomeException a)
    <- try (action (pipelineConnection pipe))
  resumePipeline pipe
  case result of
    Left e  -> throwIO e
    Right a -> pure a

-- | KIP-368: attach a 'ReauthState'-driven background thread to
-- this pipeline. The pipeline's 'pipelinePaused' flag is
-- toggled around every 'authenticate' call so the SASL
-- handshake doesn't race against in-flight requests:
--
-- @
-- attachReauthDriver pipe reauthState reauthRunner
-- @
--
-- runs the user's 'authenticate' /inside/
-- 'withPausedPipeline', which:
--
--   1. flips 'pipelinePaused' to 'True', preventing the send
--      loop from queueing new bytes;
--   2. blocks until 'awaitPipelineDrained' (no pending
--      requests left in flight);
--   3. invokes the user-supplied authenticator on the
--      pipeline's connection;
--   4. flips 'pipelinePaused' back to 'False' (or rethrows on
--      failure, again flipping back).
--
-- After the wrap, 'startReauthThread' is invoked so the
-- background driver wakes when the broker-advertised
-- @session.lifetime.ms@ deadline is reached.
attachReauthDriver
  :: Pipeline
  -> Kafka.Client.ReauthDriver.ReauthState
  -> Kafka.Client.ReauthDriver.ReauthRunner
  -> IO ()
attachReauthDriver pipe st runner = do
  let !wrapped = runner
        { Kafka.Client.ReauthDriver.authenticate =
            withPausedPipeline pipe $ \_conn ->
              runner.authenticate
        }
  Kafka.Client.ReauthDriver.startReauthThread st wrapped

----------------------------------------------------------------------
-- Background loops
----------------------------------------------------------------------

-- | Drain the send queue, write each request to the wire framed
-- with a 4-byte big-endian length prefix, and flush. Note: we
-- assume the caller has /already/ stamped the correlation id into
-- the request bytes (the wire format places the correlation id in
-- the request header). The pipeline's own 'allocateAndQueue' is
-- responsible for that — we trust 'requestBytes' as-is.
sendLoop :: Pipeline -> IO ()
sendLoop p@Pipeline{..} = loop
  where
    loop = do
      mItem <- atomically $ do
        c <- readTVar pipelineClosed
        if c
          then pure Nothing
          else do
            -- Pause gate: while the caller has parked the
            -- pipeline (e.g. running a mid-session SASL
            -- re-auth), block here instead of draining the
            -- send queue. Wakes up automatically when
            -- 'pipelinePaused' flips back to 'False'.
            p_ <- readTVar pipelinePaused
            when p_ retry
            Just <$> readTQueue pipelineSendQueue
      case mItem of
        Nothing -> pure ()
        Just (SendItem _cid bytes) -> do
          let !framed = frameMessage bytes
          r <- try (connectionPut pipelineConnection framed)
                 :: IO (Either SomeException ())
          case r of
            Left e -> failPipeline p ("send failed: " <> show e)
            Right () -> do
              atomically $ modifyTVar' pipelineStats $ \s -> s
                { statsQueueSize = max 0 (statsQueueSize s - 1) }
              loop

-- | Wire-format framing. Kafka requests are sent as
-- @[Int32 length][bytes]@; we mirror the Kafka client by
-- prepending the length here so callers don't have to.
frameMessage :: ByteString -> ByteString
frameMessage payload =
  let !len = fromIntegral (BS.length payload) :: Int32
      !hdr = BL.toStrict (BP.runPut (BP.putInt32be len))
   in BS.append hdr payload

-- | Read frames off the connection and route each one to its
-- pending request.
--
-- Bytes flow from the broker socket through the wireform magic-ring
-- transport ('Kafka.Network.RingTransport.withConnectionTransport')
-- and the streaming Kafka frame parser
-- ('Kafka.Network.FrameParser.kafkaFrameParser') feeds one
-- @(correlationId, body)@ to the loop handler per frame.  No
-- per-chunk @connectionGetExact@ allocation and no per-frame
-- 'runGet' invocation; the framing walk is one bounds-check + two
-- 32-bit loads + a slice.
--
-- The body 'ByteString' the parser hands us is a zero-copy slice
-- of the ring's backing memory.  We immediately @BS.copy@ it before
-- storing in the response slot — 'pendingResponse' may outlive the
-- transport scope (the application thread reads it long after the
-- ring tail has advanced past those bytes).
receiveLoop :: Pipeline -> IO ()
receiveLoop p@Pipeline{..} = do
  closed0 <- readTVarIO pipelineClosed
  unless closed0 $ do
    -- The Pipeline's Connection already owns a magic-ring
    -- DuplexTransport; pull the receive side out directly and feed
    -- it to the streaming Kafka frame parser.  (Pre-rewrite this
    -- went through Kafka.Network.RingTransport.withConnectionTransport,
    -- which bridged a crypton-connection 'Network.Connection' onto a
    -- fresh magic ring; the new connection's ring IS the recv ring,
    -- no bridging needed.)
    let rx = WN.duplexReceive (Conn.connDuplex pipelineConnection)
    eLoop <- try @SomeException $
      FrameParser.runKafkaFrameLoop rx (handleFrame p)
    case eLoop of
      Left e -> failPipeline p ("recv failed: " <> show e)
      Right (Right ()) -> pure ()  -- clean EOF / 'Stop'
      Right (Left perr) ->
        failPipeline p ("recv decode: " <> renderFrameError perr)

handleFrame :: Pipeline -> (RequestId, ByteString) -> IO LoopControl
handleFrame p@Pipeline{..} (cid, bodySlice) = do
  closed <- readTVarIO pipelineClosed
  if closed
    then pure Stop
    else do
      let !body = BS.copy bodySlice
      mReq <- atomically $ do
        m <- readTVar pipelinePending
        case IntMap.lookup (fromIntegral cid) m of
          Nothing -> pure Nothing
          Just req -> do
            writeTVar pipelinePending
              (IntMap.delete (fromIntegral cid) m)
            modifyTVar' pipelineStats $ \s -> s
              { statsResponsesReceived = statsResponsesReceived s + 1
              , statsCurrentInFlight   = max 0 (statsCurrentInFlight s - 1)
              }
            pure (Just req)
      case mReq of
        Nothing  -> pure ()  -- correlation id we don't recognise; drop
        Just req -> atomically $
          tryPutTMVar (pendingResponse req) (Right body)
            >> pure ()
      pure Continue

renderFrameError :: ParseError FrameParser.FrameError -> String
renderFrameError = \case
  ParseFail pos             -> "parse failed at " <> show pos
  ParseErr  pos e           -> "parse error at " <> show pos <> ": " <> show e
  ParseUnexpectedEof pos n  -> "unexpected EOF at " <> show pos
                             <> " (needed " <> show n <> " more bytes)"
  ParseTransportError exc   -> "transport error: " <> show exc

-- | Wake once a second; for every pending request whose elapsed
-- time exceeds 'pipelineTimeout' seconds, fail it with
-- @"timed out"@ and remove it from the pending map.
timeoutLoop :: Pipeline -> IO ()
timeoutLoop p@Pipeline{..} = loop
  where
    loop = do
      threadDelay 1_000_000  -- 1s
      closed <- readTVarIO pipelineClosed
      unless closed $ do
        now <- nowSeconds
        let cutoff = now - pipelineTimeout pipelineConfig
        timedOut <- atomically $ do
          m <- readTVar pipelinePending
          let !(alive, expiredMap) =
                IntMap.partition
                  (\PendingRequest{..} -> pendingTimestamp > cutoff)
                  m
              !expiredList = IntMap.elems expiredMap
              !nExpired    = IntMap.size  expiredMap
          writeTVar pipelinePending alive
          modifyTVar' pipelineStats $ \s -> s
            { statsRequestsTimedOut =
                statsRequestsTimedOut s + nExpired
            , statsCurrentInFlight =
                max 0 (statsCurrentInFlight s - nExpired)
            }
          pure expiredList
        mapM_ (\PendingRequest{..} ->
                  atomically $
                    tryPutTMVar pendingResponse (Left "timed out") >> pure ())
              timedOut
        loop

-- | Wake every pending request with the given error message and
-- mark the pipeline closed. Called by send/receive loops on
-- non-recoverable I/O errors.
failPipeline :: Pipeline -> String -> IO ()
failPipeline Pipeline{..} reason = do
  atomically $ do
    writeTVar pipelineClosed True
    m <- readTVar pipelinePending
    mapM_
      (\PendingRequest{..} ->
          tryPutTMVar pendingResponse (Left reason))
      (IntMap.elems m)
    writeTVar pipelinePending IntMap.empty

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Wall-clock seconds since the POSIX epoch via the fast
-- vDSO-coarse clock; used by the timeout loop to age out
-- pending requests. Sub-second jitter is fine here — the
-- timeout loop ticks once a second.
nowSeconds :: IO Int
nowSeconds = (`div` 1000) . fromIntegral <$> KafkaTime.currentTimeMillis

