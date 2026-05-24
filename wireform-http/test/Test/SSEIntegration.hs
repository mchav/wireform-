{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | End-to-end SSE tests: a real HTTP\/1.1 server using
-- 'sseResponseBody' + 'SseChannel' on one side, the high-level
-- 'withSSE' \/ 'withSSEFrames' client over
-- 'streamedTransport' on the other.
--
-- These exist because the unit + property tests in "Test.SSE" only
-- exercise the parser and the in-process mocks. The whole point of
-- the server-side push API is that one popper call ends up as one
-- chunked-transfer chunk on the wire, gets flushed promptly, and
-- the client's parser dispatches it as a single event — none of
-- which is exercised without a real server, a real socket, and the
-- real HTTP\/1.1 chunked encoder \/ decoder in between.
module Test.SSEIntegration (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
  (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.IORef
import Control.Monad (forM_)
import qualified Network.Socket as NS

import Test.Tasty
import Test.Tasty.HUnit

import qualified Network.HTTP.Types.Header  as H
import qualified Network.HTTP.Types.Status  as S
import qualified Network.HTTP.Types.Version as V
import qualified Network.HTTP.Message       as Msg
import qualified Network.HTTP.Types.Body    as TB
import Network.HTTP.Server
import Network.HTTP.VersionRange (VersionRange, http1Only)

import Network.HTTP.Client

tests :: TestTree
tests = testGroup "SSE end-to-end (HTTP/1.1)"
  [ basicRoundTrip
  , heartbeatsAndEvents
  , manyEventsOnOneConnection
  , largeDataPayload
  ]

-- | Additional regression: a single event whose data payload is large.
-- Exercises the 'Wireform.Builder' accumulator in 'spDataBuf' across
-- many @data:@ field appends, and the chunked-body reader at the
-- per-chunk 16 KiB read boundary.
largeDataPayload :: TestTree
largeDataPayload = testCase "single event with 64 KiB data round-trips" $ do
  let bigLines = map (\i -> BS8.pack ("line-" <> show (i :: Int) <> "-"
                                  <> replicate 56 'x'))
                     [0 .. 999]
      payload  = BS.intercalate "\n" bigLines
      ev       = defaultSseEvent { sseData = payload }
  withSseServer (pushAllAndClose [ev]) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    got @?= [ev]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

basicRoundTrip :: TestTree
basicRoundTrip = testCase "events server -> client round-trip" $ do
  let evs =
        [ defaultSseEvent { sseEventType = Just "tick", sseEventId = Just "1", sseData = "first"  }
        , defaultSseEvent { sseEventType = Just "tick", sseEventId = Just "2", sseData = "second" }
        , defaultSseEvent { sseEventType = Just "done", sseEventId = Just "3", sseData = "bye"    }
        ]
  withSseServer (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    got @?= evs

heartbeatsAndEvents :: TestTree
heartbeatsAndEvents = testCase "withSSEFrames sees comments + retry + events" $ do
  let frames =
        [ SseComment " keepalive"
        , SseRetry 2500
        , SseDispatch (defaultSseEvent { sseData = "a" })
        , SseComment " still-alive"
        , SseDispatch (defaultSseEvent { sseData = "b" })
        ]
  -- Server must use the frame-aware popper, otherwise
  -- 'awaitSseEvent' would silently drop comments + retry on the
  -- way out.
  withSseFrameServer (pushFramesAndClose frames) $ \port -> do
    received <- newIORef ([] :: [SseFrame])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSEFrames transport req $ \nextFrame ->
      drainFrames nextFrame received
    got <- reverse <$> readIORef received
    got @?= frames

manyEventsOnOneConnection :: TestTree
manyEventsOnOneConnection =
  testCase "1000 events stay in order across the wire" $ do
  -- 1000 is comfortably above every internal queue cap we thread
  -- through (streamedTransport: 32; SseChannel: 64; defaultChunkLineCap
  -- after enough small chunks pile up: 4 KiB). The third one was a real
  -- bug — see the commit fixing 'readChunkSizeLineFrom' to
  -- compare the cap against the size-line length itself rather
  -- than against ring occupancy. Pre-fix this test truncated
  -- non-deterministically around the 25–200 event mark.
  let evs = map mkEvent [0 .. 999 :: Int]
      mkEvent i = defaultSseEvent
        { sseEventId = Just (BS8.pack (show i))
        , sseData    = BS8.pack ("payload-" <> show i)
        }
  withSseServer (pushAllAndClose evs) $ \port -> do
    received <- newIORef ([] :: [ServerSentEvent])
    let req       = get (compileURL port "/events")
        transport = streamedTransport http1Only
    withSSE transport req $ \nextEvent ->
      drainEvents nextEvent received
    got <- reverse <$> readIORef received
    length got @?= length evs
    got @?= evs

-- ---------------------------------------------------------------------------
-- Server-side producers
-- ---------------------------------------------------------------------------

pushAllAndClose :: [ServerSentEvent] -> SseChannel -> IO ()
pushAllAndClose evs ch = do
  forM_ evs (sendSseEvent ch)
  closeSseChannel ch

pushFramesAndClose :: [SseFrame] -> SseChannel -> IO ()
pushFramesAndClose fs ch = do
  forM_ fs (sendSseFrame ch)
  closeSseChannel ch

-- ---------------------------------------------------------------------------
-- Consumer drains
-- ---------------------------------------------------------------------------

drainEvents :: IO (Maybe ServerSentEvent) -> IORef [ServerSentEvent] -> IO ()
drainEvents nextEvent ref = loop
  where
    loop = nextEvent >>= \case
      Nothing -> pure ()
      Just ev -> do
        modifyIORef' ref (ev :)
        loop

drainFrames :: IO (Maybe SseFrame) -> IORef [SseFrame] -> IO ()
drainFrames nextFrame ref = loop
  where
    loop = nextFrame >>= \case
      Nothing -> pure ()
      Just f  -> do
        modifyIORef' ref (f :)
        loop

-- ---------------------------------------------------------------------------
-- Server plumbing: spawn a localhost HTTP/1.1 server whose handler
-- spins up an 'SseChannel', forks the caller-supplied producer onto
-- it, and returns an SSE response body that drains the channel.
-- ---------------------------------------------------------------------------

withSseServer
  :: (SseChannel -> IO ())
  -> (String -> IO a)
  -> IO a
withSseServer = withSseServerOn (sseResponseBody . awaitSseEvent)

withSseFrameServer
  :: (SseChannel -> IO ())
  -> (String -> IO a)
  -> IO a
withSseFrameServer = withSseServerOn (sseResponseBodyFrames . awaitSseFrame)

withSseServerOn
  :: (SseChannel -> TB.Body)
  -> (SseChannel -> IO ())
  -> (String -> IO a)
  -> IO a
withSseServerOn mkBody producer = withTestServer http1Only handler
  where
    handler _req = do
      ch <- newSseChannel 64
      _  <- forkIO (producer ch)
      pure Msg.Response
        { Msg.responseStatus     = S.status200
        , Msg.responseVersion    = V.HTTP1_1
        , Msg.responseHeaders    =
            [ (H.hContentType,  "text/event-stream")
            , (H.hCacheControl, "no-store")
            ]
        , Msg.responseBody       = mkBody ch
        , Msg.responseTrailers   = pure []
        , Msg.responseH2StreamId = 0
        , Msg.responseCancel     = pure ()
        }

withTestServer
  :: VersionRange
  -> Handler
  -> (String -> IO a)
  -> IO a
withTestServer range handler action = do
  readyVar <- newEmptyMVar
  let hints = NS.defaultHints
        { NS.addrFlags      = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> assertFailure "no addr available for test bind" >> error "unreachable"
    (addr : _) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \listenSock -> do
        NS.setSocketOption listenSock NS.ReuseAddr 1
        NS.bind listenSock (NS.addrAddress addr)
        NS.listen listenSock 128
        bound <- NS.getSocketName listenSock
        let portStr = case bound of
              NS.SockAddrInet p _ -> show (fromIntegral p :: Int)
              _                   -> "0"
            cfg = defaultServerConfig
              { serverHost         = "127.0.0.1"
              , serverPort         = portStr
              , serverVersionRange = range
              , serverHandler      = handler
              }
        tid <- forkIO $ do
          putMVar readyVar ()
          runServerOnListener cfg listenSock
        takeMVar readyVar
        action portStr `finally` killThread tid

compileURL :: String -> ByteString -> UriTemplate
compileURL port path =
  let bs = "http://127.0.0.1:" <> BS8.pack port <> path
  in case parseTemplate (BS8.unpack bs) of
       Right t  -> t
       Left err -> error ("compileURL: " <> show err)
