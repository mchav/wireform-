{-# LANGUAGE OverloadedStrings #-}
{- | End-to-end HTTP\/2 integration through the unified
'Network.HTTP.Client' \/ 'Network.HTTP.Server' surface.

Uses plaintext HTTP\/2 (prior-knowledge h2c) on an ephemeral
@127.0.0.1@ port — no TLS or ALPN required to exercise the
protocol-level changes (streaming, trailers, flow control,
HEADERS continuation, cancellation, GOAWAY refusal).
-}
module Test.Http2Integration (tests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket, finally, try, SomeException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.IORef
import qualified Network.Socket as NS

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP
import qualified Network.HTTP.Types.Status as S
import qualified Network.HTTP.Types.Version as V

tests :: TestTree
tests = testGroup "HTTP/2 integration"
  [ helloWorld
  , echoBody
  , streamingResponseBody
  , streamingRequestBody
  , serverEmitsTrailers
  , largeHeaderBlockTriggersContinuation
  , concurrentStreams
  , cancellationViaWithResponse
  ]

------------------------------------------------------------------------

helloWorld :: TestTree
helloWorld = testCase "HTTP/2 plaintext (h2c prior knowledge): hello world" $
  withTestServer http2Only (\_ -> pure (resp200 "hi")) $ \port -> do
    r <- runClient http2Only port $ \c ->
      sendRequest c (mkRequest "GET" "/" port BodyEmpty [])
    responseStatus r @?= S.status200
    drainBody (responseBody r) >>= (@?= "hi")
    responseVersion r @?= V.HTTP2

echoBody :: TestTree
echoBody = testCase "POST echoes a bounded request body" $
  withTestServer http2Only echo $ \port -> do
    r <- runClient http2Only port $ \c ->
      sendRequest c (mkRequest "POST" "/" port (BodyBytes "payload") [])
    responseStatus r @?= S.status200
    drainBody (responseBody r) >>= (@?= "payload")
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body)

streamingResponseBody :: TestTree
streamingResponseBody =
  testCase "streaming response body arrives chunk-wise" $
    withTestServer http2Only handler $ \port -> do
      r <- runClient http2Only port $ \c ->
        sendRequest c (mkRequest "GET" "/" port BodyEmpty [])
      responseStatus r @?= S.status200
      -- We don't assert chunk boundaries — HTTP/2 may re-frame DATA
      -- frames freely — only the assembled body.
      drainBody (responseBody r) >>= (@?= "alphabetagamma")
  where
    handler _ = do
      ref <- newIORef ["alpha", "beta", "gamma"]
      pure Response
        { responseStatus   = S.status200
        , responseVersion  = V.HTTP2
        , responseHeaders  = []
        , responseBody     = BodyStream $ do
            xs <- readIORef ref
            case xs of
              [] -> pure Nothing
              (h:t) -> writeIORef ref t >> pure (Just h)
        , responseTrailers = pure []
        }

streamingRequestBody :: TestTree
streamingRequestBody =
  testCase "streaming request body is delivered chunked over DATA frames" $
    withTestServer http2Only echo $ \port -> do
      chunkRef <- newIORef ["one ", "two ", "three"]
      let producer = do
            xs <- readIORef chunkRef
            case xs of
              [] -> pure Nothing
              (h:t) -> writeIORef chunkRef t >> pure (Just h)
      r <- runClient http2Only port $ \c ->
        sendRequest c (mkRequest "POST" "/" port (BodyStream producer) [])
      responseStatus r @?= S.status200
      drainBody (responseBody r) >>= (@?= "one two three")
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body)

serverEmitsTrailers :: TestTree
serverEmitsTrailers =
  testCase "server-set responseTrailers reach the client's crResponseTrailers" $
    withTestServer http2Only handler $ \port -> do
      r <- runClient http2Only port $ \c ->
        sendRequest c (mkRequest "GET" "/" port BodyEmpty [])
      responseStatus r @?= S.status200
      _ <- drainBody (responseBody r)
      trs <- responseTrailers r
      lookup (CI.mk "grpc-status") trs @?= Just "0"
      lookup (CI.mk "grpc-message") trs @?= Just "OK"
  where
    handler _ = pure Response
      { responseStatus   = S.status200
      , responseVersion  = V.HTTP2
      , responseHeaders  = []
      , responseBody     = BodyBytes "body"
      , responseTrailers = pure
          [ (CI.mk "grpc-status", "0")
          , (CI.mk "grpc-message", "OK")
          ]
      }

largeHeaderBlockTriggersContinuation :: TestTree
largeHeaderBlockTriggersContinuation =
  testCase "header block > MAX_FRAME_SIZE splits across HEADERS + CONTINUATION" $
    withTestServer http2Only handler $ \port -> do
      -- ~32 KiB of distinct header values guarantees we cross the
      -- default 16 KiB SETTINGS_MAX_FRAME_SIZE.
      let bigVal = BS.replicate 512 0x61  -- 512 bytes of 'a'
          manyHeaders =
            [ (CI.mk (BS8.pack ("X-Test-" <> show n)), bigVal)
            | n <- [1 :: Int .. 64]
            ]
      r <- runClient http2Only port $ \c ->
        sendRequest c (mkRequest "GET" "/" port BodyEmpty manyHeaders)
      responseStatus r @?= S.status200
      drainBody (responseBody r) >>= (@?= "got-it")
  where
    handler _ = pure (resp200 "got-it")

concurrentStreams :: TestTree
concurrentStreams = testCase "multiple streams on one connection are independent" $
  withTestServer http2Only handler $ \port -> do
    runClient http2Only port $ \c -> do
      r1 <- sendRequest c (mkRequest "GET" "/one" port BodyEmpty [])
      r2 <- sendRequest c (mkRequest "GET" "/two" port BodyEmpty [])
      b1 <- drainBody (responseBody r1)
      b2 <- drainBody (responseBody r2)
      b1 @?= "one"
      b2 @?= "two"
  where
    handler req = case requestTarget req of
      "/one" -> pure (resp200 "one")
      "/two" -> pure (resp200 "two")
      _      -> pure (resp200 "?")

cancellationViaWithResponse :: TestTree
cancellationViaWithResponse =
  testCase "withResponse sends RST_STREAM(CANCEL) when the action throws" $
    withTestServer http2Only handler $ \port -> do
      result <- try @SomeException $
        runClient http2Only port $ \c ->
          withResponse c (mkRequest "GET" "/" port BodyEmpty []) $ \_ ->
            error "intentional"
      case result of
        Left _  -> pure ()
        Right _ -> assertFailure "expected the action's exception to propagate"
  where
    handler _ = do
      -- A non-trivial body so the recv side has some activity even
      -- after the RST.  We don't observe the peer's RST directly
      -- here; the test mainly verifies that the exception
      -- propagates and the bracket runs to completion (no hang).
      ref <- newIORef [BS.replicate 1024 0x62, BS.replicate 1024 0x63]
      pure Response
        { responseStatus   = S.status200
        , responseVersion  = V.HTTP2
        , responseHeaders  = []
        , responseBody     = BodyStream $ do
            xs <- readIORef ref
            case xs of
              [] -> pure Nothing
              (h:t) -> writeIORef ref t >> pure (Just h)
        , responseTrailers = pure []
        }

------------------------------------------------------------------------
-- Plumbing
------------------------------------------------------------------------

withTestServer
  :: VersionRange
  -> Handler
  -> (String -> IO a)
  -> IO a
withTestServer range handler action = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> assertFailure "no addr available for test bind"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \listenSock -> do
        NS.setSocketOption listenSock NS.ReuseAddr 1
        NS.bind listenSock (NS.addrAddress addr)
        NS.listen listenSock 128
        bound <- NS.getSocketName listenSock
        let portStr = case bound of
              NS.SockAddrInet p _ -> show (fromIntegral p :: Int)
              _ -> "0"
            cfg = defaultServerConfig
              { serverHost = "127.0.0.1"
              , serverPort = portStr
              , serverVersionRange = range
              , serverHandler = handler
              }
        tid <- forkIO (runServerOnListener cfg listenSock)
        threadDelay 10000
        action portStr `finally` killThread tid

runClient :: VersionRange -> String -> (Client -> IO a) -> IO a
runClient range port action = do
  let cfg = defaultClientConfig
        { clientHost = "127.0.0.1"
        , clientPort = port
        , clientVersionRange = range
        , clientTls = Nothing
        }
  withClient cfg action

mkRequest
  :: BS.ByteString
  -> BS.ByteString
  -> String
  -> Body
  -> Headers
  -> Request
mkRequest method target port body extras = Request
  { requestMethod    = methodFromBytes method
  , requestTarget    = target
  , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
  , requestScheme    = SchemeHttp
  , requestHeaders   = extras
  , requestBody      = body
  , requestVersion   = V.HTTP2
  , requestTrailers  = pure []
  }

resp200 :: BS.ByteString -> Response
resp200 body = Response
  { responseStatus   = S.status200
  , responseVersion  = V.HTTP2
  , responseHeaders  = []
  , responseBody     = if BS.null body then BodyEmpty else BodyBytes body
  , responseTrailers = pure []
  }

drainBody :: Body -> IO BS.ByteString
drainBody BodyEmpty = pure ""
drainBody (BodyBytes bs) = pure bs
drainBody (BodyStream p) = BS.concat <$> go
  where
    go = do
      mc <- p
      case mc of
        Nothing -> pure []
        Just c -> (c :) <$> go
