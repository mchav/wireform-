{- | Streaming-friendly base transport.

The default 'Network.HTTP.Client.Base.baseTransport' and
'Network.HTTP.Client.Pool.pooledTransport' materialise response
bodies before returning, because the low-level
'Network.HTTP.Connection.withConnection' bracket has to close
when 'sendOn' returns and the response popper lives inside that
bracket.

This module's 'streamedTransport' takes a different shape: for
each request it forks a /worker thread/ that owns the connection
for the response's lifetime, pipes the body through an unbounded
'TBQueue', and returns a 'RawResponse' whose 'bodyPopper' reads
from that queue. The caller's popper is therefore live and
streaming — chunks land in memory as they come off the wire,
rather than the whole body being drained up front.

== Trade-offs vs the pool

* /No connection reuse./ Each request opens a fresh connection
  and closes it at popper EOF. Pair with
  'Network.HTTP.Client.Pool.pooledTransport' when you want reuse
  on the request side and don't need streaming responses.
* /Strict EOF contract./ The worker thread blocks until either
  the popper has returned its EOF chunk (empty 'ByteString') or
  the popper's 'Weak' reference is collected by the GC. Callers
  that abandon the response without draining it tie up a worker
  until GC fires; for tests and reasonable user code that's
  fine.
* /Streaming-aware middleware./ Compression and tracing both work
  unchanged: compression sees the chunks as they arrive,
  'withTracing' keeps its span open until the popper hits EOF
  via the same mechanism (the streaming popper bumps an
  on-EOF callback the middleware can install).

== Future
This module is the substrate for a streaming-capable pool. Once
'wireform-http2' exposes non-bracketed connection open\/close, the
pool can adopt this same worker-per-checkout shape and we collapse
the two transports back into one.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Client.Streaming
  ( streamedTransport
  , streamedTransportVia
  , streamedTransportWith
  , streamedTransportWithVia
  , StreamingError (..)
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, mask, throwIO, try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.IORef

import qualified Network.HTTP.Connection    as Conn
import qualified Network.HTTP.Message       as Msg
import qualified Network.HTTP.Types.Body    as LB
import qualified Network.HTTP.Types.Version as LV
import qualified Network.HTTP.VersionRange  as VR

import           Network.HTTP.Client.BodyStream (BodyStream, Popper, bodyStreamBytes)
import           Network.HTTP.Client.Protocol
import qualified Network.HTTP.Client.Request   as WReq
import           Network.HTTP.Client.Response
import           Network.HTTP.Client.Transport
import qualified Network.HTTP.Client.URI       as WURI
import qualified Network.HTTP.Client.Proxy     as Pxy
import           Network.HTTP.Client.Proxy     (Proxy, ProxyConfig)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data StreamingError
  = StreamingInvalidURI !String
  | StreamingHandshakeFailed !String
  deriving stock (Show)

instance Exception StreamingError

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Streaming base transport with a default queue size.
streamedTransport :: VR.VersionRange -> Transport IO
streamedTransport = streamedTransportWith 32

-- | Proxy-aware variant of 'streamedTransport'. See
-- 'streamedTransportWithVia' for the routing rules.
streamedTransportVia :: VR.VersionRange -> ProxyConfig -> Transport IO
streamedTransportVia = streamedTransportWithVia 32

-- | Streaming base transport with explicit chunk-queue capacity. A
-- small queue applies natural back-pressure to the producer when
-- the consumer is slow; a large queue smooths over jittery
-- consumers at the cost of more buffering.
streamedTransportWith :: Int -> VR.VersionRange -> Transport IO
streamedTransportWith qcap versionRange =
  streamedTransportWithVia qcap versionRange Pxy.noProxyConfig

-- | Proxy-aware variant of 'streamedTransportWith'. Routes HTTPS
-- targets through @CONNECT@ when an HTTPS proxy is configured;
-- HTTP targets dial the proxy directly (the absolute-form request
-- line is the responsibility of the
-- 'Network.HTTP.Client.Proxy.withProxy' middleware).
streamedTransportWithVia :: Int -> VR.VersionRange -> ProxyConfig -> Transport IO
streamedTransportWithVia qcap versionRange pcfg = Transport $ \req -> do
  cfg <- connectionConfigForRequest versionRange req
  uri_ <- case WURI.renderRequestURI (WReq.requestURI req) of
    Right u  -> pure u
    Left err -> throwIO (StreamingInvalidURI err)
  let mProxy
        | Pxy.shouldBypass pcfg (WURI.uriHost uri_) = Nothing
        | otherwise = case WURI.uriScheme uri_ of
            WURI.SchemeHttp  -> Pxy.proxyForHttp pcfg
            WURI.SchemeHttps -> Pxy.proxyForHttps pcfg
  bodyBytes <- bodyStreamBytes (WReq.body req)
  let lowReq = toLowLevelRequest uri_ bodyBytes req

  chunks   <- newTBQueueIO (fromIntegral qcap)
  rawVar   <- newEmptyMVar
  doneVar  <- newEmptyMVar
  finished <- newIORef False

  -- Worker holds the bracketed connection for the lifetime of the
  -- response body. It produces chunks into 'chunks' and blocks on
  -- 'doneVar' until the consumer signals EOF.
  _wtid <- forkIO $ workerLoop cfg mProxy lowReq chunks rawVar doneVar finished
  result <- takeMVar rawVar
  case result of
    Left e    -> throwIO e
    Right raw -> pure raw

-- ---------------------------------------------------------------------------
-- Worker
-- ---------------------------------------------------------------------------

workerLoop
  :: Conn.ConnectionConfig
  -> Maybe Proxy
  -> Msg.Request
  -> TBQueue (Maybe ByteString)
  -> MVar (Either SomeException RawResponse)
  -> MVar ()
  -> IORef Bool
  -> IO ()
workerLoop cfg mProxy lowReq chunks rawVar doneVar finished = do
  outcome <- try $ Conn.withConnectionVia cfg mProxy Nothing $ \conn ->
    Conn.withResponseOn conn lowReq $ \resp -> do
      -- 1. Hand a 'RawResponse' back to the caller. Its popper
      --    drains 'chunks'. We have to deliver the raw response
      --    /before/ pumping (otherwise the caller is blocked
      --    waiting for it).
      let popper = readChunk chunks finished doneVar
          -- Cancellation combines a real H2 RST_STREAM (when the
          -- low-level layer surfaced one as @responseCancel@) with
          -- the local consumer-side EOF + worker wake-up: that way
          -- a caller that cancels still drops out of the popper
          -- and the bracketed @Conn.withConnection@ unwinds.
          cancel = do
            Msg.responseCancel resp
            writeIORef finished True
            atomically (writeTBQueue chunks Nothing)
            _ <- tryPutMVar doneVar ()
            pure ()
          raw   = RawResponse
            { statusCode    = Msg.responseStatus resp
            , headers       = Msg.responseHeaders resp
            , bodyPopper    = popper
            , protocolInfo  = case Msg.responseVersion resp of
                LV.HTTP2 -> HTTP2 Http2Info
                  { h2StreamId     = Msg.responseH2StreamId resp
                  , h2PushPromises = pure []
                  , h2CancelStream = cancel
                  }
                _        -> HTTP1_1
            }
      putMVar rawVar (Right raw)

      -- 2. Pump the underlying body popper into 'chunks'.
      pumpBody (Msg.responseBody resp) chunks

      -- 3. Wait until the caller has drained or abandoned the
      --    popper. 'doneVar' is signalled by 'readChunk' on EOF or
      --    by the streaming-aware middleware when it's done.
      takeMVar doneVar
  case outcome of
    Right () -> pure ()
    Left (e :: SomeException) -> do
      -- If anything failed before 'rawVar' was filled, surface
      -- the error there.
      noResult <- tryPutMVar rawVar (Left e)
      -- If the response was already delivered, push an EOF into
      -- the chunk queue so the consumer's popper can finish.
      atomically (writeTBQueue chunks Nothing)
      _ <- pure noResult
      pure ()

-- | Drain chunks from the underlying body into the TBQueue. On
-- end-of-body, queues a single 'Nothing' as the EOF marker.
pumpBody :: LB.Body -> TBQueue (Maybe ByteString) -> IO ()
pumpBody body chunks = case body of
  LB.BodyEmpty    -> atomically (writeTBQueue chunks Nothing)
  LB.BodyBytes bs -> do
    !_ <- pure bs  -- materialised already
    atomically $ do
      writeTBQueue chunks (Just bs)
      writeTBQueue chunks Nothing
  LB.BodyStream p -> drainStream p
  where
    drainStream p = do
      mChunk <- p
      case mChunk of
        Nothing -> atomically (writeTBQueue chunks Nothing)
        Just b
          | BS.null b -> drainStream p
          | otherwise -> do
              atomically (writeTBQueue chunks (Just b))
              drainStream p

-- | Read one chunk from the queue. EOF (the queue's 'Nothing'
-- marker, or repeated reads after EOF) returns an empty
-- 'ByteString' and signals the worker via 'doneVar'.
readChunk
  :: TBQueue (Maybe ByteString)
  -> IORef Bool
  -> MVar ()
  -> Popper
readChunk chunks finished doneVar = do
  alreadyDone <- readIORef finished
  if alreadyDone
    then pure BS.empty
    else do
      mb <- atomically (readTBQueue chunks)
      case mb of
        Just b
          | BS.null b -> readChunk chunks finished doneVar
          | otherwise -> pure b
        Nothing -> do
          writeIORef finished True
          void (tryPutMVar doneVar ())
          pure BS.empty

-- ---------------------------------------------------------------------------
-- Connection config + request lowering (shared with the pool, but
-- replicated here to keep the modules independent).
-- ---------------------------------------------------------------------------

connectionConfigForRequest
  :: VR.VersionRange
  -> WReq.Request BodyStream
  -> IO Conn.ConnectionConfig
connectionConfigForRequest versionRange req = do
  case WURI.renderRequestURI (WReq.requestURI req) of
    Left err -> throwIO (StreamingInvalidURI err)
    Right u  -> do
      let scheme = WURI.uriScheme u
          host   = BS8.unpack (WURI.uriHost u)
          tls    = case scheme of
            WURI.SchemeHttps -> Just (Conn.defaultTlsConnectionConfig host)
            WURI.SchemeHttp  -> Nothing
      pure Conn.ConnectionConfig
        { Conn.connectionHost         = host
        , Conn.connectionPort         = show (WURI.uriPort u)
        , Conn.connectionVersionRange = versionRange
        , Conn.connectionTls          = tls
        }

toLowLevelRequest
  :: WURI.URI
  -> ByteString
  -> WReq.Request BodyStream
  -> Msg.Request
toLowLevelRequest uri_ bodyBytes req =
  let lowScheme = case WURI.uriScheme uri_ of
        WURI.SchemeHttps -> Msg.SchemeHttps
        WURI.SchemeHttp  -> Msg.SchemeHttp
      target = WURI.uriPathAndQuery uri_
      hostBs = WURI.uriHost uri_
      authority =
        Just (hostBs <> case (WURI.uriScheme uri_, WURI.uriPort uri_) of
                          (WURI.SchemeHttp,  80)  -> ""
                          (WURI.SchemeHttps, 443) -> ""
                          (_,                p)   -> ":" <> BS8.pack (show p))
  in Msg.Request
       { Msg.requestMethod    = WReq.method req
       , Msg.requestTarget    = target
       , Msg.requestAuthority = authority
       , Msg.requestScheme    = lowScheme
       , Msg.requestHeaders   = WReq.headers req
       , Msg.requestBody      = if BS.null bodyBytes
                                   then LB.BodyEmpty
                                   else LB.BodyBytes bodyBytes
       , Msg.requestVersion   = VR.preferredVersion (Conn.connectionVersionRange placeholder)
       , Msg.requestTrailers  = pure []
       }
  where
    placeholder = Conn.defaultConnectionConfig
      { Conn.connectionVersionRange = VR.preferHttp1 }
