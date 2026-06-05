{-# LANGUAGE OverloadedStrings #-}
{- | Concurrency stress tests for the HTTP client/server stack.

These tests exercise shared mutable state under concurrent load:
- HPACK encoder exclusion (parallel H2 response encoding)
- Flow control pressure (many concurrent streams writing data)
- HTTP/1 keep-alive under rapid sequential requests
- HTTP/2 concurrent request/response multiplexing
- Connection close races
-}
module Test.ConcurrencyStress (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally, try, SomeException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.IORef
import qualified Network.Socket as NS

import Test.Syd

import Network.HTTP
import Network.HTTP.Connection
import Network.HTTP.Server
import qualified Network.HTTP.Types.Status as S
import qualified Network.HTTP.Types.Version as V

tests :: Spec
tests = describe "Concurrency stress" $ sequence_
  [ h2ParallelStreams
  , h2ParallelStreamsWithBodies
  , h2ParallelStreamsWithCustomHeaders
  , h2ManySequentialRequestsSameConn
  , h1RapidSequentialRequests
  , h1ParallelConnections
  , h2FlowControlPressure
  , h2ConcurrentSendAndRecv
  ]

------------------------------------------------------------------------
-- HTTP/2 concurrency
------------------------------------------------------------------------

h2ParallelStreams :: Spec
h2ParallelStreams =
  it "H2: 20 parallel streams on one connection" $
    withTestServer http2Only echoTarget $ \port -> do
      runClient http2Only port $ \c -> do
        responses <- mapM (\i ->
          sendOn c (mkH2Request "GET" (BS8.pack ("/path-" <> show i)) port BodyEmpty [])
          ) [1 :: Int .. 20]
        bodies <- mapM (drainBody . responseBody) responses
        length bodies `shouldBe` 20
        let statuses = map responseStatus responses
        all (== S.status200) statuses `shouldBe` True
  where
    echoTarget req = pure (resp200 V.HTTP2 (requestTarget req))

h2ParallelStreamsWithBodies :: Spec
h2ParallelStreamsWithBodies =
  it "H2: 10 parallel POST streams with 4 KiB bodies" $
    withTestServer http2Only echo $ \port -> do
      let payload i = BS.replicate 4096 (fromIntegral i)
      runClient http2Only port $ \c -> do
        responses <- mapM (\i ->
          sendOn c (mkH2Request "POST" "/" port (BodyBytes (payload i)) [])
          ) [1 :: Int .. 10]
        bodies <- mapM (drainBody . responseBody) responses
        mapM_ (\(i, body) ->
          body `shouldBe` payload i
          ) (zip [1 :: Int .. 10] bodies)
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 V.HTTP2 body)

h2ParallelStreamsWithCustomHeaders :: Spec
h2ParallelStreamsWithCustomHeaders =
  it "H2: 15 parallel streams with distinct custom headers (HPACK encoder stress)" $
    withTestServer http2Only headerEcho $ \port -> do
      runClient http2Only port $ \c -> do
        responses <- mapM (\i -> do
          let hdrs = [ (CI.mk (BS8.pack ("x-stream-" <> show i)), BS8.pack ("value-" <> show i))
                     , (CI.mk "x-request-id", BS8.pack (show i))
                     ]
          sendOn c (mkH2Request "GET" "/" port BodyEmpty hdrs)
          ) [1 :: Int .. 15]
        bodies <- mapM (drainBody . responseBody) responses
        length bodies `shouldBe` 15
        mapM_ (\body ->
          (not (BS.null body)) `shouldBe` True
          ) bodies
  where
    headerEcho req = do
      let reqId = case lookup (CI.mk "x-request-id") (requestHeaders req) of
            Just v  -> v
            Nothing -> "unknown"
      pure (resp200 V.HTTP2 reqId)

h2ManySequentialRequestsSameConn :: Spec
h2ManySequentialRequestsSameConn =
  it "H2: 50 sequential requests on one connection" $
    withTestServer http2Only (\_ -> pure (resp200 V.HTTP2 "ok")) $ \port -> do
      runClient http2Only port $ \c -> do
        mapM_ (\i -> do
          r <- sendOn c (mkH2Request "GET" (BS8.pack ("/" <> show i)) port BodyEmpty [])
          body <- drainBody (responseBody r)
          responseStatus r `shouldBe` S.status200
          body `shouldBe` "ok"
          ) [1 :: Int .. 50]

h2FlowControlPressure :: Spec
h2FlowControlPressure =
  it "H2: 5 parallel streams each sending 16 KiB (flow control pressure)" $
    withTestServer http2Only echo $ \port -> do
      let payload = BS.replicate (16 * 1024) 0x41
      runClient http2Only port $ \c -> do
        responses <- mapM (\_ ->
          sendOn c (mkH2Request "POST" "/" port (BodyBytes payload) [])
          ) [1 :: Int .. 5]
        bodies <- mapM (drainBody . responseBody) responses
        mapM_ (\body ->
          BS.length body `shouldBe` 16 * 1024
          ) bodies
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 V.HTTP2 body)

h2ConcurrentSendAndRecv :: Spec
h2ConcurrentSendAndRecv =
  it "H2: interleaved send and recv across streams" $
    withTestServer http2Only handler $ \port -> do
      runClient http2Only port $ \c -> do
        r1 <- sendOn c (mkH2Request "GET" "/slow" port BodyEmpty [])
        r2 <- sendOn c (mkH2Request "GET" "/fast" port BodyEmpty [])
        b2 <- drainBody (responseBody r2)
        b1 <- drainBody (responseBody r1)
        responseStatus r1 `shouldBe` S.status200
        responseStatus r2 `shouldBe` S.status200
        b1 `shouldBe` "slow-response"
        b2 `shouldBe` "fast-response"
  where
    handler req = case requestTarget req of
      "/slow" -> pure (resp200 V.HTTP2 "slow-response")
      "/fast" -> pure (resp200 V.HTTP2 "fast-response")
      _       -> pure (resp200 V.HTTP2 "?")

------------------------------------------------------------------------
-- HTTP/1 concurrency
------------------------------------------------------------------------

h1RapidSequentialRequests :: Spec
h1RapidSequentialRequests =
  it "H1: 30 rapid sequential requests on one keep-alive connection" $
    withTestServer http1Only counter $ \port -> do
      runClient http1Only port $ \c -> do
        mapM_ (\i -> do
          r <- sendOn c (mkH1Request "GET" (BS8.pack ("/" <> show i)) port BodyEmpty [])
          _ <- drainBody (responseBody r)
          responseStatus r `shouldBe` S.status200
          ) [1 :: Int .. 30]
  where
    counter _ = pure (resp200 V.HTTP1_1 "ok")

h1ParallelConnections :: Spec
h1ParallelConnections =
  it "H1: 10 parallel connections, 3 requests each" $
    withTestServer http1Only (\_ -> pure (resp200 V.HTTP1_1 "parallel-ok")) $ \port -> do
      doneVar <- newEmptyMVar
      errRef <- newIORef (Nothing :: Maybe SomeException)
      let worker = do
            result <- try @SomeException $
              runClient http1Only port $ \c -> do
                mapM_ (\i -> do
                  r <- sendOn c (mkH1Request "GET" (BS8.pack ("/" <> show i)) port BodyEmpty [])
                  body <- drainBody (responseBody r)
                  responseStatus r `shouldBe` S.status200
                  body `shouldBe` "parallel-ok"
                  ) [1 :: Int .. 3]
            case result of
              Left e -> atomicModifyIORef' errRef (\_ -> (Just e, ()))
              Right () -> pure ()
            putMVar doneVar ()
      mapM_ (\_ -> forkIO worker) [1 :: Int .. 10]
      mapM_ (\_ -> takeMVar doneVar) [1 :: Int .. 10]
      merr <- readIORef errRef
      case merr of
        Just e  -> expectationFailure ("worker failed: " <> show e)
        Nothing -> pure ()

------------------------------------------------------------------------
-- Plumbing
------------------------------------------------------------------------

withTestServer
  :: VersionRange
  -> Handler
  -> (String -> IO a)
  -> IO a
withTestServer range handler action = do
  readyVar <- newEmptyMVar
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> expectationFailure "no addr available for test bind"
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
        tid <- forkIO $ do
          putMVar readyVar ()
          runServerOnListener cfg listenSock
        takeMVar readyVar
        action portStr `finally` killThread tid

runClient :: VersionRange -> String -> (Connection -> IO a) -> IO a
runClient range port action = do
  let cfg = defaultConnectionConfig
        { connectionHost = "127.0.0.1"
        , connectionPort = port
        , connectionVersionRange = range
        , connectionTls = Nothing
        }
  withConnection cfg action

mkH2Request
  :: BS.ByteString -> BS.ByteString -> String -> Body -> Headers -> Request
mkH2Request method target port body extras = Request
  { requestMethod    = methodFromBytes method
  , requestTarget    = target
  , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
  , requestScheme    = SchemeHttp
  , requestHeaders   = extras
  , requestBody      = body
  , requestVersion   = V.HTTP2
  , requestTrailers  = pure []
  }

mkH1Request
  :: BS.ByteString -> BS.ByteString -> String -> Body -> Headers -> Request
mkH1Request method target port body extras = Request
  { requestMethod    = methodFromBytes method
  , requestTarget    = target
  , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
  , requestScheme    = SchemeHttp
  , requestHeaders   = extras
  , requestBody      = body
  , requestVersion   = V.HTTP1_1
  , requestTrailers  = pure []
  }

resp200 :: V.Version -> BS.ByteString -> Response
resp200 ver body = Response
  { responseStatus   = S.status200
  , responseVersion  = ver
  , responseHeaders  = []
  , responseBody     = if BS.null body then BodyEmpty else BodyBytes body
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
