{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for "Network.HTTP.Client.SSE".
--
-- The bulk of the assertions go through the incremental parser: feed
-- a known chunk, check the surfaced frames; feed the same wire one
-- byte at a time, check the result is identical; then a Hedgehog
-- property says render-then-parse round-trips through a sane subset
-- of events.
--
-- A handful of end-to-end tests drive @withSSE@ through a mock
-- transport so the headers it injects (Accept, Cache-Control) and
-- the status \/ Content-Type assertions are covered.
module Test.SSE (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  (newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (try, SomeException, fromException)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe (mapMaybe)

import qualified Network.HTTP.Types.Body as TB

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Syd
import Test.Syd.Hedgehog ()

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Client
import Network.HTTP.Client.Response (RawResponse (headers))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Run the parser to exhaustion on a single bytestring and collect
-- all surfaced frames.
parseAll :: ByteString -> [SseFrame]
parseAll = parseEventStream

-- | Same as 'parseAll' but split the input into one chunk per byte.
-- Lets us catch off-by-one carry / CRLF folding bugs that don't
-- show up on whole-buffer input.
parseByByte :: ByteString -> [SseFrame]
parseByByte bs = drainParser newSseParser (oneByteChunks bs)
  where
    oneByteChunks b
      | BS.null b = []
      | otherwise = BS.take 1 b : oneByteChunks (BS.drop 1 b)
    drainParser _ []           = []
    drainParser p (c : rest)   =
      let (p', fs) = feedSseParser p c
      in fs <> drainParser p' rest

-- | Just dispatched events.
events :: [SseFrame] -> [ServerSentEvent]
events = mapMaybe pick
  where
    pick (SseDispatch ev) = Just ev
    pick _                = Nothing

-- | Drain a popper into a list of frames or events.
drainFramePopper :: IO (Maybe SseFrame) -> IO [SseFrame]
drainFramePopper p = go []
  where
    go acc = p >>= \case
      Nothing -> pure (reverse acc)
      Just f  -> go (f : acc)

drainEventPopper :: IO (Maybe ServerSentEvent) -> IO [ServerSentEvent]
drainEventPopper p = go []
  where
    go acc = p >>= \case
      Nothing -> pure (reverse acc)
      Just ev -> go (ev : acc)

-- | Build a transport that hands back a single response built from
-- the given status, headers, and a list of body chunks. The popper
-- yields each chunk in order, then EOF.
chunkTransport :: S.Status -> [H.Header] -> [ByteString] -> Transport IO
chunkTransport status hdrs chunks = unsafeMkTransport $ \_ -> do
  remaining <- newIORef (filter (not . BS.null) chunks)
  let popper = atomicModifyIORef' remaining $ \case
        []     -> ([], BS.empty)
        (c:cs) -> (cs, c)
  pure RawResponse
    { statusCode   = status
    , headers      = hdrs
    , bodyPopper   = popper
    , protocolInfo = HTTP1_1
    }

dummyReq :: Request ()
dummyReq = get (compileTemplate "/events")

compileTemplate :: String -> UriTemplate
compileTemplate s = case parseTemplate s of
  Right t  -> t
  Left err -> error ("compileTemplate: " <> show err)

-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests = describe "Network.HTTP.Client.SSE" $ sequence_
  [ parserBasicTests
  , parserLineEndingTests
  , parserFieldTests
  , parserBomTests
  , chunkBoundaryTests
  , popperTests
  , withSSETests
  , renderTests
  , mediaTypeTests
  , serverBodyTests
  , channelTests
  , roundTripProperties
  ]

-- ---------------------------------------------------------------------------
-- Basic parser shapes
-- ---------------------------------------------------------------------------

parserBasicTests :: Spec
parserBasicTests = describe "parser/basic" $ sequence_
  [ it "empty body yields no frames" $
      parseAll "" `shouldBe` []

  , it "single data line dispatches one event" $ do
      let frames = parseAll "data: hello\n\n"
      events frames `shouldBe` [defaultSseEvent { sseData = "hello" }]

  , it "multi-line data joins with LF and strips the trailer" $ do
      let frames = parseAll "data: line1\ndata: line2\ndata: line3\n\n"
      events frames `shouldBe`
        [defaultSseEvent { sseData = "line1\nline2\nline3" }]

  , it "event field carries through to dispatch" $ do
      let frames = parseAll "event: ping\ndata: x\n\n"
      events frames `shouldBe`
        [defaultSseEvent { sseEventType = Just "ping", sseData = "x" }]

  , it "id is preserved across events" $ do
      let frames = parseAll "id: 42\ndata: a\n\ndata: b\n\n"
      events frames `shouldBe`
        [ defaultSseEvent { sseEventId = Just "42", sseData = "a" }
        , defaultSseEvent { sseEventId = Just "42", sseData = "b" }
        ]

  , it "id with NUL is ignored entirely" $ do
      let frames = parseAll (BS.concat ["id: 1\nid: a", BS.pack [0x00], "b\ndata: x\n\n"])
      events frames `shouldBe`
        [defaultSseEvent { sseEventId = Just "1", sseData = "x" }]

  , it "consecutive events in one chunk" $ do
      let frames = parseAll "data: a\n\ndata: b\n\ndata: c\n\n"
      events frames `shouldBe`
        [ defaultSseEvent { sseData = "a" }
        , defaultSseEvent { sseData = "b" }
        , defaultSseEvent { sseData = "c" }
        ]

  , it "pending event without a terminating blank line is dropped" $ do
      let frames = parseAll "data: lonely\n"
      events frames `shouldBe` []

  , it "blank line with no data does not dispatch" $ do
      let frames = parseAll "event: ignored\n\n"
      events frames `shouldBe` []

  , it "event-type buffer resets after suppressed dispatch" $ do
      -- "event: foo\n\n" (suppressed, but spec resets event-type)
      -- followed by "data: x\n\n" should dispatch as the default
      -- type, not "foo".
      let frames = parseAll "event: foo\n\ndata: x\n\n"
      events frames `shouldBe` [defaultSseEvent { sseData = "x" }]
  ]

-- ---------------------------------------------------------------------------
-- Line endings
-- ---------------------------------------------------------------------------

parserLineEndingTests :: Spec
parserLineEndingTests = describe "parser/line endings" $ sequence_
  [ it "CRLF terminators" $ do
      let frames = parseAll "data: hello\r\n\r\n"
      events frames `shouldBe` [defaultSseEvent { sseData = "hello" }]

  , it "bare CR terminators" $ do
      let frames = parseAll "data: hello\r\r"
      events frames `shouldBe` [defaultSseEvent { sseData = "hello" }]

  , it "mixed CR / LF / CRLF" $ do
      let frames = parseAll "data: a\rdata: b\ndata: c\r\n\r\n"
      events frames `shouldBe` [defaultSseEvent { sseData = "a\nb\nc" }]

  , it "CRLF folded across the chunk boundary" $ do
      -- Feed "data: hi\r" and then "\n\n" — the LF after the
      -- carried CR must be swallowed, not interpreted as a
      -- second empty line.
      let (p1, fs1) = feedSseParser newSseParser "data: hi\r"
          (_,  fs2) = feedSseParser p1 "\n\r\n"
      events (fs1 <> fs2) `shouldBe` [defaultSseEvent { sseData = "hi" }]
  ]

-- ---------------------------------------------------------------------------
-- Field syntax
-- ---------------------------------------------------------------------------

parserFieldTests :: Spec
parserFieldTests = describe "parser/fields" $ sequence_
  [ it "comment line is surfaced as SseComment" $ do
      let frames = parseAll ": heartbeat\n\n"
      frames `shouldBe` [SseComment " heartbeat"]

  , it "comment doesn't dispatch a pending event" $ do
      let frames = parseAll "data: x\n: log\ndata: y\n\n"
      events frames `shouldBe` [defaultSseEvent { sseData = "x\ny" }]

  , it "line with no colon: field name only, empty value" $ do
      -- "data" with no colon and no value still appends LF
      -- to the data buffer.
      let frames = parseAll "data\ndata\n\n"
      events frames `shouldBe` [defaultSseEvent { sseData = "\n" }]

  , it "exactly one leading space stripped from value" $ do
      events (parseAll "data:nospace\n\n")
        `shouldBe` [defaultSseEvent { sseData = "nospace" }]
      events (parseAll "data: one\n\n")
        `shouldBe` [defaultSseEvent { sseData = "one" }]
      events (parseAll "data:  two\n\n")
        `shouldBe` [defaultSseEvent { sseData = " two" }]

  , it "retry surfaces SseRetry, parses base-10" $ do
      parseAll "retry: 5000\n\n" `shouldBe` [SseRetry 5000]

  , it "retry with non-digit ignored" $ do
      parseAll "retry: 5s\n\n" `shouldBe` []
      parseAll "retry: \n\n"   `shouldBe` []

  , it "unknown field is silently ignored" $ do
      events (parseAll "fancy: nope\ndata: x\n\n")
        `shouldBe` [defaultSseEvent { sseData = "x" }]

  , it "value with embedded colon is not split twice" $ do
      events (parseAll "data: a:b:c\n\n")
        `shouldBe` [defaultSseEvent { sseData = "a:b:c" }]
  ]

-- ---------------------------------------------------------------------------
-- BOM
-- ---------------------------------------------------------------------------

parserBomTests :: Spec
parserBomTests = describe "parser/BOM" $ sequence_
  [ it "UTF-8 BOM consumed at start" $ do
      let body = BS.pack [0xEF, 0xBB, 0xBF] <> "data: x\n\n"
      events (parseAll body) `shouldBe` [defaultSseEvent { sseData = "x" }]

  , it "BOM split across chunks" $ do
      let part1 = BS.pack [0xEF]
          part2 = BS.pack [0xBB, 0xBF] <> "data: y\n"
          part3 = "\n"
          (p1, fs1) = feedSseParser newSseParser part1
          (p2, fs2) = feedSseParser p1 part2
          (_,  fs3) = feedSseParser p2 part3
      events (fs1 <> fs2 <> fs3) `shouldBe`
        [defaultSseEvent { sseData = "y" }]

  , it "non-BOM leading bytes are not stripped" $ do
      -- 0xEF 0xBB but third byte is 'X' (0x58), not 0xBF: not a BOM.
      -- The bytes should pass through as field content.
      let body = BS.concat [BS.pack [0xEF, 0xBB], "Xdata: q\n\n"]
      -- The whole first line is "\xEF\xBBXdata: q" — that's the
      -- field name "\xEF\xBBXdata" and value "q". Unknown field,
      -- no data dispatch.
      events (parseAll body) `shouldBe` []
  ]

-- ---------------------------------------------------------------------------
-- Chunk boundaries
-- ---------------------------------------------------------------------------

chunkBoundaryTests :: Spec
chunkBoundaryTests = describe "parser/chunk boundaries" $ sequence_
  [ it "byte-at-a-time matches one-shot, simple event" $ do
      let body = "data: hello\n\n"
      events (parseByByte body) `shouldBe` events (parseAll body)

  , it "byte-at-a-time matches one-shot, complex stream" $ do
      let body = BS.concat
            [ ": keep-alive\n\n"
            , "event: foo\nid: 1\ndata: a\ndata: b\n\n"
            , "retry: 250\n\n"
            , "data: c\n\n"
            ]
      parseByByte body `shouldBe` parseAll body

  , it "byte-at-a-time matches one-shot, CRLF stream" $ do
      let body = "event: x\r\nid: 1\r\ndata: a\r\n\r\ndata: b\r\n\r\n"
      parseByByte body `shouldBe` parseAll body

  , it "all single split points produce identical frames" $ do
      let body = "event: t\ndata: one\ndata: two\n\nid: 9\ndata: three\n\n"
          oneShot = parseAll body
      mapM_ (\i -> assertSplitEq body i oneShot) [0 .. BS.length body]
  ]

assertSplitEq :: ByteString -> Int -> [SseFrame] -> IO ()
assertSplitEq body i expected = do
  let (a, b) = BS.splitAt i body
      (p1, fs1) = feedSseParser newSseParser a
      (_,  fs2) = feedSseParser p1 b
      actual = fs1 <> fs2
  actual `shouldBe` expected

-- ---------------------------------------------------------------------------
-- Popper integration
-- ---------------------------------------------------------------------------

popperTests :: Spec
popperTests = describe "popper" $ sequence_
  [ it "sseFramePopper dispatches across chunk boundaries" $ do
      popper <- popperFromList
        [ "event: ev\n"
        , "data: hello"
        , " world\n"
        , "\n"
        ]
      framePopper <- sseFramePopper popper
      fs <- drainFramePopper framePopper
      fs `shouldBe`
        [ SseDispatch ServerSentEvent
            { sseEventType = Just "ev"
            , sseEventId   = Nothing
            , sseData      = "hello world"
            }
        ]

  , it "sseEventPopper drops comments and retry" $ do
      popper <- popperFromStrict $ BS.concat
        [ ": heartbeat\n\n"
        , "retry: 250\n\n"
        , "data: a\n\n"
        , "data: b\n\n"
        ]
      ep <- sseEventPopper popper
      evs <- drainEventPopper ep
      evs `shouldBe`
        [ defaultSseEvent { sseData = "a" }
        , defaultSseEvent { sseData = "b" }
        ]

  , it "popper returns Nothing once EOF, stays Nothing" $ do
      popper <- popperFromStrict "data: x\n\n"
      ep <- sseEventPopper popper
      Just _ <- ep
      r1 <- ep
      r2 <- ep
      r1 `shouldBe` Nothing
      r2 `shouldBe` Nothing
  ]

-- ---------------------------------------------------------------------------
-- withSSE end-to-end
-- ---------------------------------------------------------------------------

withSSETests :: Spec
withSSETests = describe "withSSE" $ sequence_
  [ it "drains events and sets Accept + Cache-Control headers" $ do
      let sseHdrs = [(H.hContentType, "text/event-stream")]
          base    = chunkTransport S.status200 sseHdrs
                      [ "data: one\n\n"
                      , "data: two\n\n"
                      ]
      (logged, log_) <- withRequestLog base
      got <- newMVar []
      withSSE logged dummyReq $ \nextEvent -> do
        let loop = nextEvent >>= \case
              Nothing -> pure ()
              Just ev -> do
                xs <- takeMVar got
                putMVar got (ev : xs)
                loop
        loop
      collected <- reverse <$> takeMVar got
      collected `shouldBe`
        [ defaultSseEvent { sseData = "one" }
        , defaultSseEvent { sseData = "two" }
        ]
      assertLog log_ (anyRequest (hasHeaderEq H.hAccept "text/event-stream"))
      assertLog log_ (anyRequest (hasHeaderEq H.hCacheControl "no-store"))

  , it "non-2xx status raises SseUnexpectedStatus" $ do
      let base = chunkTransport S.status500
                   [(H.hContentType, "text/event-stream")] []
      result <- try (withSSE base dummyReq $ \_ -> pure ())
              :: IO (Either SomeException ())
      case result of
        Left e -> case fromException e of
          Just (SseUnexpectedStatus s) ->
            S.statusCode s `shouldBe` 500
          _ -> expectationFailure ("wrong exception: " <> show e)
        Right _ -> expectationFailure "expected SseUnexpectedStatus"

  , it "wrong Content-Type raises SseUnexpectedContentType" $ do
      let base = chunkTransport S.status200
                   [(H.hContentType, "text/plain")] []
      result <- try (withSSE base dummyReq $ \_ -> pure ())
              :: IO (Either SomeException ())
      case result of
        Left e -> case fromException e of
          Just (SseUnexpectedContentType mt) -> do
            mtType mt    `shouldBe` "text"
            mtSubType mt `shouldBe` "plain"
          _ -> expectationFailure ("wrong exception: " <> show e)
        Right _ -> expectationFailure "expected SseUnexpectedContentType"

  , it "missing Content-Type also raises SseUnexpectedContentType" $ do
      -- Per RFC 9110 the default is application/octet-stream, which
      -- is /not/ text/event-stream, so we still reject.
      let base = chunkTransport S.status200 [] []
      result <- try (withSSE base dummyReq $ \_ -> pure ())
              :: IO (Either SomeException ())
      case result of
        Left e -> case fromException e of
          Just (SseUnexpectedContentType _) -> pure ()
          _ -> expectationFailure ("wrong exception: " <> show e)
        Right _ -> expectationFailure "expected SseUnexpectedContentType"

  , it "withSSEFrames surfaces comments and retry" $ do
      let body = BS.concat
            [ ": keepalive\n\n"
            , "retry: 1000\n\n"
            , "data: msg\n\n"
            ]
          base = chunkTransport S.status200
                   [(H.hContentType, "text/event-stream")] [body]
      collected <- newIORef []
      withSSEFrames base dummyReq $ \nextFrame -> do
        let loop = nextFrame >>= \case
              Nothing -> pure ()
              Just f  -> do
                modifyIORef' collected (f :)
                loop
        loop
      fs <- reverse <$> readIORef collected
      fs `shouldBe`
        [ SseComment " keepalive"
        , SseRetry 1000
        , SseDispatch (defaultSseEvent { sseData = "msg" })
        ]
  ]

-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderTests :: Spec
renderTests = describe "render" $ sequence_
  [ it "single-line data" $
      renderServerSentEvent (defaultSseEvent { sseData = "hello" })
        `shouldBe` "data: hello\n\n"

  , it "multi-line data emits one data: per line" $
      renderServerSentEvent (defaultSseEvent { sseData = "a\nb\nc" })
        `shouldBe` "data: a\ndata: b\ndata: c\n\n"

  , it "event + id + data" $
      renderServerSentEvent ServerSentEvent
        { sseEventType = Just "ping"
        , sseEventId   = Just "42"
        , sseData      = "payload"
        }
        `shouldBe` "event: ping\nid: 42\ndata: payload\n\n"

  , it "comment frame" $
      renderSseFrame (SseComment " hi") `shouldBe` ": hi\n\n"

  , it "retry frame" $
      renderSseFrame (SseRetry 2500) `shouldBe` "retry: 2500\n\n"

  , it "empty data renders an event with no data lines" $
      -- The reverse direction (parsing back) would suppress
      -- dispatch, but the renderer is honest about what the
      -- caller asked for.
      renderServerSentEvent defaultSseEvent `shouldBe` "\n"
  ]

-- ---------------------------------------------------------------------------
-- MediaType integration
-- ---------------------------------------------------------------------------

mediaTypeTests :: Spec
mediaTypeTests = describe "MediaType integration" $ sequence_
  [ it "EventStream tag produces the right Content-Type" $
      mediaType @EventStream `shouldBe`
        MediaType { mtType = "text"
                  , mtSubType = "event-stream"
                  , mtParameters = []
                  }

  , it "as @EventStream decodes a fixture body" $ do
      let body = BS.concat
            [ "event: tick\n"
            , "data: 1\n\n"
            , "event: tick\n"
            , "data: 2\n\n"
            ]
          transport = stubBytes S.status200
            [(H.hContentType, "text/event-stream")] body
      Response { responseBody = evs } <-
        sendIO transport dummyReq (as @EventStream @[ServerSentEvent])
      evs `shouldBe`
        [ defaultSseEvent { sseEventType = Just "tick", sseData = "1" }
        , defaultSseEvent { sseEventType = Just "tick", sseData = "2" }
        ]
  ]

-- ---------------------------------------------------------------------------
-- Server-side body (sseBodyPopper / sseResponseBody)
-- ---------------------------------------------------------------------------

serverBodyTests :: Spec
serverBodyTests = describe "server body" $ sequence_
  [ it "sseBodyPopper renders one event per call" $ do
      ref <- newIORef
        [ defaultSseEvent { sseData = "first"  }
        , defaultSseEvent { sseData = "second" }
        ]
      let source = atomicModifyIORef' ref $ \case
            []       -> ([], Nothing)
            (x : xs) -> (xs, Just x)
          popper = sseBodyPopper source
      c1 <- popper
      c2 <- popper
      c3 <- popper
      c1 `shouldBe` Just "data: first\n\n"
      c2 `shouldBe` Just "data: second\n\n"
      c3 `shouldBe` Nothing

  , it "sseBodyPopperFrames preserves retry + comment" $ do
      ref <- newIORef
        [ SseComment " keepalive"
        , SseRetry 1000
        , SseDispatch (defaultSseEvent { sseData = "x" })
        ]
      let source = atomicModifyIORef' ref $ \case
            []       -> ([], Nothing)
            (x : xs) -> (xs, Just x)
          popper = sseBodyPopperFrames source
      chunks <- drainBytePopper popper
      BS.concat chunks `shouldBe`
        ": keepalive\n\nretry: 1000\n\ndata: x\n\n"

  , it "sseResponseBody returns a BodyStream" $ do
      let body = sseResponseBody (pure Nothing)
      case body of
        TB.BodyStream _ -> pure ()
        TB.BodyEmpty    -> expectationFailure "expected BodyStream, got BodyEmpty"
        TB.BodyBytes _  -> expectationFailure "expected BodyStream, got BodyBytes"

  , it "round-trip: events sent via popper parse back identically" $ do
      let evs =
            [ defaultSseEvent { sseEventType = Just "tick", sseData = "1" }
            , defaultSseEvent { sseEventType = Just "tick", sseData = "2" }
            , defaultSseEvent { sseEventType = Just "done", sseData = "ok" }
            ]
      ref <- newIORef evs
      let source = atomicModifyIORef' ref $ \case
            []       -> ([], Nothing)
            (x : xs) -> (xs, Just x)
          popper = sseBodyPopper source
      wire <- BS.concat <$> drainBytePopper popper
      parseEventStreamEvents wire `shouldBe` evs
  ]

drainBytePopper :: IO (Maybe ByteString) -> IO [ByteString]
drainBytePopper p = go []
  where
    go acc = p >>= \case
      Nothing -> pure (reverse acc)
      Just bs -> go (bs : acc)

-- ---------------------------------------------------------------------------
-- SseChannel
-- ---------------------------------------------------------------------------

channelTests :: Spec
channelTests = describe "SseChannel" $ sequence_
  [ it "FIFO: closed channel drained then signals end" $ do
      ch <- newSseChannel 4
      sendSseEvent ch (defaultSseEvent { sseData = "x" })
      sendSseEvent ch (defaultSseEvent { sseData = "y" })
      closeSseChannel ch
      e1 <- awaitSseEvent ch
      e2 <- awaitSseEvent ch
      e3 <- awaitSseEvent ch
      e1 `shouldBe` Just (defaultSseEvent { sseData = "x" })
      e2 `shouldBe` Just (defaultSseEvent { sseData = "y" })
      e3 `shouldBe` Nothing

  , it "close before send: producer drops silently" $ do
      ch <- newSseChannel 1
      closeSseChannel ch
      sendSseEvent ch (defaultSseEvent { sseData = "ignored" })
      sendSseEvent ch (defaultSseEvent { sseData = "also ignored" })
      e <- awaitSseEvent ch
      e `shouldBe` Nothing

  , it "isSseChannelClosed reflects state transitions" $ do
      ch <- newSseChannel 1
      before <- isSseChannelClosed ch
      before `shouldBe` False
      closeSseChannel ch
      after_ <- isSseChannelClosed ch
      after_ `shouldBe` True

  , it "awaitSseEvent skips retry + comment frames" $ do
      ch <- newSseChannel 4
      sendSseComment ch " heartbeat"
      sendSseRetry   ch 2500
      sendSseEvent   ch (defaultSseEvent { sseData = "real" })
      closeSseChannel ch
      ev  <- awaitSseEvent ch
      end <- awaitSseEvent ch
      ev  `shouldBe` Just (defaultSseEvent { sseData = "real" })
      end `shouldBe` Nothing

  , it "awaitSseFrame surfaces every frame in order" $ do
      ch <- newSseChannel 4
      sendSseComment ch "c"
      sendSseRetry   ch 100
      sendSseEvent   ch (defaultSseEvent { sseData = "d" })
      closeSseChannel ch
      fs <- drainFramesViaChannel ch
      fs `shouldBe`
        [ SseComment "c"
        , SseRetry 100
        , SseDispatch (defaultSseEvent { sseData = "d" })
        ]

  , it "producer in another thread, bounded backpressure" $ do
      -- Cap of 2 means the producer will block until the consumer
      -- drains. We use forkIO + an MVar to coordinate without
      -- threadDelay; the producer just keeps pushing and the
      -- bounded queue applies the actual back-pressure.
      ch <- newSseChannel 2
      let evs = map (\i -> defaultSseEvent { sseData = BS8.pack ("e" <> show i) })
                    [0 .. 19 :: Int]
      doneSending <- newEmptyMVar
      _ <- forkIO $ do
        forM_ evs (sendSseEvent ch)
        closeSseChannel ch
        putMVar doneSending ()
      collected <- drainEventsViaChannel ch
      takeMVar doneSending
      collected `shouldBe` evs

  , it "channel feeds sseBodyPopper end-to-end" $ do
      ch <- newSseChannel 4
      let evs =
            [ defaultSseEvent { sseEventType = Just "a", sseData = "1" }
            , defaultSseEvent { sseEventType = Just "b", sseData = "2" }
            ]
      doneSending <- newEmptyMVar
      _ <- forkIO $ do
        forM_ evs (sendSseEvent ch)
        closeSseChannel ch
        putMVar doneSending ()
      let popper = sseBodyPopper (awaitSseEvent ch)
      wire <- BS.concat <$> drainBytePopper popper
      takeMVar doneSending
      parseEventStreamEvents wire `shouldBe` evs
  ]

drainFramesViaChannel :: SseChannel -> IO [SseFrame]
drainFramesViaChannel ch = go []
  where
    go acc = awaitSseFrame ch >>= \case
      Nothing -> pure (reverse acc)
      Just f  -> go (f : acc)

drainEventsViaChannel :: SseChannel -> IO [ServerSentEvent]
drainEventsViaChannel ch = go []
  where
    go acc = awaitSseEvent ch >>= \case
      Nothing -> pure (reverse acc)
      Just ev -> go (ev : acc)

-- ---------------------------------------------------------------------------
-- Round-trip property
-- ---------------------------------------------------------------------------

roundTripProperties :: Spec
roundTripProperties = describe "properties" $ sequence_
  [ it "render . parse round-trips dispatched events" $
      Hedgehog.property $ do
        evs <- Hedgehog.forAll genEvents
        let wire   = BS.concat (map renderServerSentEvent evs)
            parsed = parseEventStreamEvents wire
        parsed Hedgehog.=== evs

  , it "render . parse round-trips frame sequences" $
      Hedgehog.property $ do
        frames <- Hedgehog.forAll genFrames
        let wire   = BS.concat (map renderSseFrame frames)
            parsed = parseEventStream wire
        parsed Hedgehog.=== frames

  , it "byte-at-a-time feeding equals one-shot feeding" $
      Hedgehog.property $ do
        frames <- Hedgehog.forAll genFrames
        let wire = BS.concat (map renderSseFrame frames)
        parseByByte wire Hedgehog.=== parseAll wire
  ]

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | A token suitable for an event-type or id field: ASCII printable
-- characters minus CR \/ LF \/ NUL \/ ':' so a render-then-parse
-- round trip is faithful. (The spec only requires absence of CR \/
-- LF; we also exclude ':' so a value never collides with the field
-- separator, and NUL because IDs containing NUL are ignored.)
genToken :: Hedgehog.Gen ByteString
genToken = fmap (BS.map sanitize) (Gen.bytes (Range.linear 1 16))
  where
    sanitize b
      | b == 0x0A || b == 0x0D || b == 0x00 || b == 0x3A = 0x78
      | b < 0x20  = 0x78
      | b >= 0x7F = 0x78
      | otherwise = b

-- | A non-empty data payload. Lines are joined with single LFs; we
-- explicitly forbid CR so the renderer's LF-only split is faithful,
-- and forbid empty result so the parser doesn't drop the event.
genData :: Hedgehog.Gen ByteString
genData = do
  ls <- Gen.list (Range.linear 1 6) genLine
  let joined = BS.intercalate "\n" ls
  if BS.null joined then pure "x" else pure joined
  where
    genLine = fmap (BS.map sanitize) (Gen.bytes (Range.linear 0 32))
    sanitize b
      | b == 0x0A || b == 0x0D = 0x78
      | b < 0x20  = 0x78
      | b >= 0x7F = 0x78
      | otherwise = b

genEvent :: Hedgehog.Gen ServerSentEvent
genEvent = do
  et <- Gen.maybe genToken
  ei <- Gen.maybe genToken
  d  <- genData
  pure ServerSentEvent
    { sseEventType = et
    , sseEventId   = ei
    , sseData      = d
    }

-- | Events with the carry-forward of @id@ baked in. The parser
-- preserves the last seen id across subsequent events; for the
-- round-trip property we therefore generate a sequence where
-- @id@ inherits the previous event's value when the current event
-- doesn't set one, so the in-memory representation matches what
-- comes back out of the parser.
genEvents :: Hedgehog.Gen [ServerSentEvent]
genEvents = do
  raw <- Gen.list (Range.linear 0 8) genEvent
  pure (rewriteIds Nothing raw)
  where
    rewriteIds _    []        = []
    rewriteIds prev (ev:rest) =
      let i = case sseEventId ev of
                Just _  -> sseEventId ev
                Nothing -> prev
          ev' = ev { sseEventId = i }
      in ev' : rewriteIds i rest

-- | Comments survive a round trip as long as they have no CR/LF.
genComment :: Hedgehog.Gen ByteString
genComment = fmap (BS.map sanitize) (Gen.bytes (Range.linear 0 24))
  where
    sanitize b
      | b == 0x0A || b == 0x0D = 0x78
      | otherwise              = b

-- | Frame sequences: a mix of dispatches, retries, and comments.
-- For each event, optionally interleave 0..2 side frames (retries
-- or comments) before it.
genFrames :: Hedgehog.Gen [SseFrame]
genFrames = do
  evs      <- genEvents
  preludes <- Gen.list (Range.singleton (length evs)) genPrelude
  pure (interleave preludes evs)
  where
    interleave []     xs       = map SseDispatch xs
    interleave ps     []       = concat ps
    interleave (p:ps) (x:xs)   = p <> (SseDispatch x : interleave ps xs)
    genPrelude = Gen.list (Range.linear 0 2) genSideFrame
    genSideFrame = Gen.choice
      [ fmap SseComment genComment
      , fmap SseRetry   (Gen.int (Range.linear 0 10000))
      ]
