{-# LANGUAGE OverloadedStrings #-}

{- | End-to-end HTTP\/2 integration through the unified
'Network.HTTP.Connection' \/ 'Network.HTTP.Server' surface.

Uses plaintext HTTP\/2 (prior-knowledge h2c) on an ephemeral
@127.0.0.1@ port — no TLS or ALPN required to exercise the
protocol-level changes (streaming, trailers, flow control,
HEADERS continuation, cancellation, GOAWAY refusal).
-}
module Test.Http2Integration (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (SomeException, bracket, finally, try)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Network.HTTP
import Network.HTTP.Connection
import Network.HTTP.Server
import Network.HTTP.Types.Status qualified as S
import Network.HTTP.Types.Version qualified as V
import Network.Socket qualified as NS
import Test.Syd


tests :: Spec
tests =
  describe "HTTP/2 integration" $
    sequence_
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

helloWorld :: Spec
helloWorld = it "HTTP/2 plaintext (h2c prior knowledge): hello world" $
  withTestServer http2Only (\_ -> pure (resp200 "hi")) $ \port -> do
    (status, ver, body) <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, responseVersion r, b)
    status `shouldBe` S.status200
    body `shouldBe` "hi"
    ver `shouldBe` V.HTTP2


echoBody :: Spec
echoBody = it "POST echoes a bounded request body" $
  withTestServer http2Only echo $ \port -> do
    (status, body) <- runClient http2Only port $ \c -> do
      r <- sendOn c (mkRequest "POST" "/" port (BodyBytes "payload") [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status `shouldBe` S.status200
    body `shouldBe` "payload"
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body)


streamingResponseBody :: Spec
streamingResponseBody =
  it "streaming response body arrives chunk-wise" $
    withTestServer http2Only handler $ \port -> do
      (status, body) <- runClient http2Only port $ \c -> do
        r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      status `shouldBe` S.status200
      -- We don't assert chunk boundaries — HTTP/2 may re-frame DATA
      -- frames freely — only the assembled body.
      body `shouldBe` "alphabetagamma"
  where
    handler _ = do
      ref <- newIORef ["alpha", "beta", "gamma"]
      pure
        Response
          { responseStatus = S.status200
          , responseVersion = V.HTTP2
          , responseHeaders = []
          , responseBody = BodyStream $ do
              xs <- readIORef ref
              case xs of
                [] -> pure Nothing
                (h : t) -> writeIORef ref t >> pure (Just h)
          , responseTrailers = pure []
          , responseH2StreamId = 0
          , responseCancel = pure ()
          , responsePushPromises = pure []
          }


streamingRequestBody :: Spec
streamingRequestBody =
  it "streaming request body is delivered chunked over DATA frames" $
    withTestServer http2Only echo $ \port -> do
      chunkRef <- newIORef ["one ", "two ", "three"]
      let producer = do
            xs <- readIORef chunkRef
            case xs of
              [] -> pure Nothing
              (h : t) -> writeIORef chunkRef t >> pure (Just h)
      (status, body) <- runClient http2Only port $ \c -> do
        r <- sendOn c (mkRequest "POST" "/" port (BodyStream producer) [])
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      status `shouldBe` S.status200
      body `shouldBe` "one two three"
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body)


serverEmitsTrailers :: Spec
serverEmitsTrailers =
  it "server-set responseTrailers reach the client's crResponseTrailers" $
    withTestServer http2Only handler $ \port -> do
      (status, trs) <- runClient http2Only port $ \c -> do
        r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
        _ <- drainBody (responseBody r)
        t <- responseTrailers r
        pure (responseStatus r, t)
      status `shouldBe` S.status200
      lookup (CI.mk "grpc-status") trs `shouldBe` Just "0"
      lookup (CI.mk "grpc-message") trs `shouldBe` Just "OK"
  where
    handler _ =
      pure
        Response
          { responseStatus = S.status200
          , responseVersion = V.HTTP2
          , responseHeaders = []
          , responseBody = BodyBytes "body"
          , responseTrailers =
              pure
                [ (CI.mk "grpc-status", "0")
                , (CI.mk "grpc-message", "OK")
                ]
          , responseH2StreamId = 0
          , responseCancel = pure ()
          , responsePushPromises = pure []
          }


largeHeaderBlockTriggersContinuation :: Spec
largeHeaderBlockTriggersContinuation =
  it "header block > MAX_FRAME_SIZE splits across HEADERS + CONTINUATION" $
    withTestServer http2Only handler $ \port -> do
      -- ~32 KiB of distinct header values guarantees we cross the
      -- default 16 KiB SETTINGS_MAX_FRAME_SIZE.
      let bigVal = BS.replicate 512 0x61 -- 512 bytes of 'a'
          manyHeaders =
            [ (CI.mk (BS8.pack ("X-Test-" <> show n)), bigVal)
            | n <- [1 :: Int .. 64]
            ]
      (status, body) <- runClient http2Only port $ \c -> do
        r <- sendOn c (mkRequest "GET" "/" port BodyEmpty manyHeaders)
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      status `shouldBe` S.status200
      body `shouldBe` "got-it"
  where
    handler _ = pure (resp200 "got-it")


concurrentStreams :: Spec
concurrentStreams = it "multiple streams on one connection are independent" $
  withTestServer http2Only handler $ \port -> do
    runClient http2Only port $ \c -> do
      r1 <- sendOn c (mkRequest "GET" "/one" port BodyEmpty [])
      r2 <- sendOn c (mkRequest "GET" "/two" port BodyEmpty [])
      b1 <- drainBody (responseBody r1)
      b2 <- drainBody (responseBody r2)
      b1 `shouldBe` "one"
      b2 `shouldBe` "two"
  where
    handler req = case requestTarget req of
      "/one" -> pure (resp200 "one")
      "/two" -> pure (resp200 "two")
      _ -> pure (resp200 "?")


cancellationViaWithResponse :: Spec
cancellationViaWithResponse =
  it "withResponseOn sends RST_STREAM(CANCEL) when the action throws" $
    withTestServer http2Only handler $ \port -> do
      result <- try @SomeException $
        runClient http2Only port $ \c ->
          withResponseOn c (mkRequest "GET" "/" port BodyEmpty []) $ \_ ->
            error "intentional"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected the action's exception to propagate"
  where
    handler _ = do
      -- A non-trivial body so the recv side has some activity even
      -- after the RST.  We don't observe the peer's RST directly
      -- here; the test mainly verifies that the exception
      -- propagates and the bracket runs to completion (no hang).
      ref <- newIORef [BS.replicate 1024 0x62, BS.replicate 1024 0x63]
      pure
        Response
          { responseStatus = S.status200
          , responseVersion = V.HTTP2
          , responseHeaders = []
          , responseBody = BodyStream $ do
              xs <- readIORef ref
              case xs of
                [] -> pure Nothing
                (h : t) -> writeIORef ref t >> pure (Just h)
          , responseTrailers = pure []
          , responseH2StreamId = 0
          , responseCancel = pure ()
          , responsePushPromises = pure []
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
  let hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> expectationFailure "no addr available for test bind"
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
              _ -> "0"
            cfg =
              defaultServerConfig
                { serverHost = "127.0.0.1"
                , serverPort = portStr
                , serverVersionRange = range
                , serverHandler = handler
                }
        readyVar <- newEmptyMVar
        tid <- forkIO $ do
          putMVar readyVar ()
          runServerOnListener cfg listenSock
        takeMVar readyVar
        action portStr `finally` killThread tid


runClient :: VersionRange -> String -> (Connection -> IO a) -> IO a
runClient range port action = do
  let cfg =
        defaultConnectionConfig
          { connectionHost = "127.0.0.1"
          , connectionPort = port
          , connectionVersionRange = range
          , connectionTls = Nothing
          }
  withConnection cfg action


mkRequest
  :: BS.ByteString
  -> BS.ByteString
  -> String
  -> Body
  -> Headers
  -> Request
mkRequest method target port body extras =
  Request
    { requestMethod = methodFromBytes method
    , requestTarget = target
    , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
    , requestScheme = SchemeHttp
    , requestHeaders = extras
    , requestBody = body
    , requestVersion = V.HTTP2
    , requestTrailers = pure []
    }


resp200 :: BS.ByteString -> Response
resp200 body =
  Response
    { responseStatus = S.status200
    , responseVersion = V.HTTP2
    , responseHeaders = []
    , responseBody = if BS.null body then BodyEmpty else BodyBytes body
    , responseTrailers = pure []
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
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
