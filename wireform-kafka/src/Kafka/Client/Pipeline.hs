{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever, unless, when)
import Data.Binary.Get (getInt32be, runGet)
import qualified Data.Binary.Put as BP
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Int
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import GHC.Generics (Generic)
import qualified Kafka.Time as KafkaTime
import Network.Connection
  ( Connection
  , connectionGetExact
  , connectionPut
  , connectionClose
  )

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
  } deriving (Eq, Show, Generic)

-- | Default pipeline configuration.
defaultPipelineConfig :: PipelineConfig
defaultPipelineConfig = PipelineConfig
  { pipelineMaxInFlight = 100
  , pipelineMaxQueueSize = 1000
  , pipelineTimeout = 30
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
  , pipelineNextId     :: !(TVar RequestId)
  , pipelinePending    :: !(TVar (IntMap PendingRequest))
    -- ^ 'IntMap' keyed on the 'RequestId' (correlation id) is
    -- the hottest access on the pipeline: every response from
    -- the broker triggers a 'lookup' + 'delete'. 'IntMap' wins
    -- over both 'Data.Map.Strict.Map' (which would do
    -- lexicographic 'Int' compares per branch) and
    -- 'Data.HashMap.Strict.HashMap' (which would compute a
    -- per-call 'hashInt32' and walk a collision chain). For
    -- the typical 5-100 in-flight count the Patricia trie is
    -- 1-7 nodes deep, comparing branching bits directly.
  , pipelineSendQueue  :: !(TQueue SendItem)
  , pipelineStats      :: !(TVar PipelineStats)
  , pipelineClosed     :: !(TVar Bool)
  , pipelineThreads    :: !(TVar [Async ()])
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
  nextId    <- newTVarIO 0
  pending   <- newTVarIO IntMap.empty
  sendQueue <- newTQueueIO
  stats     <- newTVarIO emptyStats
  closed    <- newTVarIO False
  threads   <- newTVarIO []
  let pipe = Pipeline
        { pipelineConnection = conn
        , pipelineConfig     = config
        , pipelineNextId     = nextId
        , pipelinePending    = pending
        , pipelineSendQueue  = sendQueue
        , pipelineStats      = stats
        , pipelineClosed     = closed
        , pipelineThreads    = threads
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
      atomically (allocateAndQueue pipe builder now)

allocateAndQueue
  :: Pipeline
  -> (RequestId -> ByteString)
  -> Int
  -> STM (Either String (RequestId, ResponseSlot))
allocateAndQueue Pipeline{..} builder now = do
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
      -- Allocate a fresh correlation id. We loop until we find
      -- one that isn't already pending — practically unreachable
      -- (correlation ids are 32-bit) but defensive.
      cid <- nextFreeCorrelationId pipelineNextId pendingMap
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

nextFreeCorrelationId
  :: TVar RequestId -> IntMap a -> STM RequestId
nextFreeCorrelationId ref m = go (32 :: Int)
  where
    go 0 = pure 0  -- gave up (every id pending — practically impossible)
    go n = do
      cid <- readTVar ref
      let !next = if cid == maxBound then 0 else cid + 1
      writeTVar ref next
      if IntMap.member (fromIntegral cid) m
        then go (n - 1)
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
          else Just <$> readTQueue pipelineSendQueue
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
-- pending request. The Kafka response framing is
-- @[Int32 length][Int32 correlationId][body]@; we strip the first
-- four bytes (length) and the next four (correlation id), then
-- deliver the rest to the waiting 'TMVar'.
receiveLoop :: Pipeline -> IO ()
receiveLoop p@Pipeline{..} = loop
  where
    loop = do
      closed <- readTVarIO pipelineClosed
      unless closed $ do
        eFrame <- try (readFrame pipelineConnection)
                    :: IO (Either SomeException (Either String (RequestId, ByteString)))
        case eFrame of
          Left e ->
            failPipeline p ("recv failed: " <> show e)
          Right (Left err) ->
            failPipeline p ("recv decode: " <> err)
          Right (Right (cid, body)) -> do
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
              Nothing -> pure ()  -- correlation id we don't recognise; drop
              Just req -> atomically $
                tryPutTMVar (pendingResponse req) (Right body)
                  >> pure ()
            loop

-- | Read one length-prefixed Kafka response off the wire and
-- split it into @(correlationId, body)@.
readFrame
  :: Connection
  -> IO (Either String (RequestId, ByteString))
readFrame conn = do
  lenBytes <- connectionGetExact conn 4
  if BS.length lenBytes < 4
    then pure (Left "short read on frame length")
    else do
      let !len = fromIntegral
                   (runGet getInt32be (BL.fromStrict lenBytes)) :: Int
      if len < 4
        then pure (Left "frame too short to contain a correlation id")
        else do
          payload <- connectionGetExact conn len
          if BS.length payload < len
            then pure (Left "short read on frame body")
            else
              let !cidBs = BS.take 4 payload
                  !body  = BS.drop 4 payload
                  !cid   = fromIntegral
                             (runGet getInt32be (BL.fromStrict cidBs))
               in pure (Right (cid, body))

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
          -- 'IntMap' has 'partition' built in: one Patricia-trie
          -- walk splits the still-alive entries from the expired
          -- ones in O(n + m).
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

-- 'IntSet' kept imported for future use (not currently required
-- but the timeout loop's cancel-set is a likely follow-up).
_keepIntSet :: IntSet
_keepIntSet = IntSet.empty
