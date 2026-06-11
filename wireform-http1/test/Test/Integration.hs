{-# LANGUAGE OverloadedStrings #-}

{- | End-to-end integration tests: actually open a socket pair, run our
server on one side and our client on the other, and verify request /
response semantics on the wire.

These exercise the full pipeline (recv buffer -> parser -> handler ->
encoder -> send buffer) and let us catch desync bugs that pure-code
tests would miss.
-}
module Test.Integration (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally)
import Data.ByteString qualified as BS
import Data.IORef
import Network.HTTP1.Client
import Network.HTTP1.Encode qualified as Enc
import Network.HTTP1.Method
import Network.HTTP1.Server
import Network.HTTP1.Status
import Network.HTTP1.Types
import Network.HTTP1.Version
import Network.Socket qualified as NS
import System.Directory (removeFile)
import Test.Syd


tests :: Spec
tests =
  describe "Integration" $
    sequence_
      [ helloWorldTest
      , echoBodyTest
      , chunkedRequestTest
      , chunkedResponseTest
      , keepAlivePipelineTest
      , preEncodedGetTest
      , preEncodedHeadTest
      , sendFileGetTest
      , sendFileHeadTest
      , sendFileCachedFdTest
      ]


------------------------------------------------------------------------

helloWorldTest :: Spec
helloWorldTest = it "GET hello world" $
  withServer (\_ -> pure $ resp200 "Hello, world!\n") $ \port -> do
    let req = mkReq GET "/" port BodyEmpty []
    Right r <- sendRequest (clientCfg port) req
    responseStatus r `shouldBe` OK
    bodyOf r >>= (`shouldBe` "Hello, world!\n")


echoBodyTest :: Spec
echoBodyTest = it "POST echo body" $
  withServer echo $ \port -> do
    let req = mkReq POST "/echo" port (BodyBytes "round trip me") []
    Right r <- sendRequest (clientCfg port) req
    responseStatus r `shouldBe` OK
    bodyOf r >>= (`shouldBe` "round trip me")
  where
    echo req = do
      body <- drainAll (requestBody req)
      pure $
        Response
          OK
          HTTP_1_1
          [("Content-Type", "text/plain")]
          (BodyBytes body)
          (pure [])


chunkedRequestTest :: Spec
chunkedRequestTest = it "POST chunked request body" $
  withServer echo $ \port -> do
    chunkRef <- newIORef ["one", "two", "three!"]
    let producer = do
          xs <- readIORef chunkRef
          case xs of
            [] -> pure Nothing
            (h : t) -> do writeIORef chunkRef t; pure (Just h)
        req = mkReq POST "/" port (BodyStream producer) []
    Right r <- sendRequest (clientCfg port) req
    responseStatus r `shouldBe` OK
    bodyOf r >>= (`shouldBe` "onetwothree!")
  where
    echo req = do
      body <- drainAll (requestBody req)
      pure $ Response OK HTTP_1_1 [] (BodyBytes body) (pure [])


chunkedResponseTest :: Spec
chunkedResponseTest = it "streaming chunked response" $
  withServer streaming $ \port -> do
    let req = mkReq GET "/stream" port BodyEmpty []
    Right r <- sendRequest (clientCfg port) req
    responseStatus r `shouldBe` OK
    bodyOf r >>= (`shouldBe` "alphabetagamma")
  where
    streaming _ = do
      chunkRef <- newIORef ["alpha", "beta", "gamma"]
      pure $ Response OK HTTP_1_1 [] (BodyStream (next chunkRef)) (pure [])
    next ref = do
      xs <- readIORef ref
      case xs of
        [] -> pure Nothing
        (h : t) -> do writeIORef ref t; pure (Just h)


keepAlivePipelineTest :: Spec
keepAlivePipelineTest = it "keep-alive: two requests on one connection" $
  withServer (\_ -> pure (resp200 "ok")) $ \port -> do
    withClientConnection (clientCfg port) $ \conn -> do
      Right r1 <- sendRequestOn conn (mkReq GET "/a" port BodyEmpty [])
      _ <- bodyOf r1
      Right r2 <- sendRequestOn conn (mkReq GET "/b" port BodyEmpty [])
      _ <- bodyOf r2
      responseStatus r1 `shouldBe` OK
      responseStatus r2 `shouldBe` OK


------------------------------------------------------------------------
-- Pre-encoded responses
------------------------------------------------------------------------

-- A precomputed static response — identical shape to what
-- bench-server/WireformServer.hs ships.
staticOk :: Response
staticOk =
  Enc.precomputeResponse $
    Response
      { responseStatus = OK
      , responseVersion = HTTP_1_1
      , responseHeaders = [("Content-Type", "text/plain"), ("Server", "test")]
      , responseBody = BodyBytes "Hello, world!\n"
      , responseTrailers = pure []
      }


preEncodedGetTest :: Spec
preEncodedGetTest = it "pre-encoded response (GET) is byte-identical to encoder output" $
  withServer (\_ -> pure staticOk) $ \port -> do
    Right r <- sendRequest (clientCfg port) (mkReq GET "/" port BodyEmpty [])
    responseStatus r `shouldBe` OK
    body <- bodyOf r
    body `shouldBe` "Hello, world!\n"
    -- Verify the parsed headers match what the encoder would emit.
    BS.length body `shouldBe` 14


preEncodedHeadTest :: Spec
preEncodedHeadTest = it "pre-encoded response served as HEAD drops body but keeps Content-Length" $
  withServer (\_ -> pure staticOk) $ \port -> do
    -- HEAD on the same precomputed response. The server slices
    -- peBytes to peHeadLen so metadata survives but the body is gone.
    Right r <- sendRequest (clientCfg port) (mkReq HEAD "/" port BodyEmpty [])
    responseStatus r `shouldBe` OK
    -- HEAD response framing is special: the parser sees
    -- Content-Length: 14 in the headers but knows HEAD MUST NOT carry
    -- a body, so it frames as NoBody.
    body <- bodyOf r
    body `shouldBe` ""


------------------------------------------------------------------------
-- sendfile(2)
------------------------------------------------------------------------

{- | Bracket-style helper that writes a temp file with given contents,
runs an action against it, then removes the file.
-}
withTempFile :: FilePath -> BS.ByteString -> (FilePath -> IO a) -> IO a
withTempFile path contents action = do
  BS.writeFile path contents
  action path `finally` (removeFile path)


{- | Sendfile bodies can be larger than the client's recv buffer, so
we drain inside 'withClientConnection' to keep the connection live
while we read.
-}
sendFileGetTest :: Spec
sendFileGetTest = it "sendfile body: GET delivers file bytes verbatim" $
  withTempFile "/tmp/wireform-http1-sendfile-test.txt" payload $ \path -> do
    let handler _ = do
          fb <- wholeFileBody path
          pure $
            Response
              OK
              HTTP_1_1
              [("Content-Type", "text/plain"), ("Server", "test")]
              (BodyFile fb)
              (pure [])
    withServer handler $ \port -> do
      withClientConnection (clientCfg port) $ \conn -> do
        Right r <- sendRequestOn conn (mkReq GET "/" port BodyEmpty [])
        responseStatus r `shouldBe` OK
        body <- bodyOf r
        body `shouldBe` payload
  where
    payload = BS.replicate 4096 0x61 -- 4 KiB of 'a'


sendFileHeadTest :: Spec
sendFileHeadTest = it "sendfile body: HEAD emits headers, no body" $
  withTempFile "/tmp/wireform-http1-sendfile-head-test.txt" payload $ \path -> do
    let handler _ = do
          fb <- wholeFileBody path
          pure $
            Response
              OK
              HTTP_1_1
              [("Content-Type", "text/plain"), ("Server", "test")]
              (BodyFile fb)
              (pure [])
    withServer handler $ \port -> do
      withClientConnection (clientCfg port) $ \conn -> do
        Right r <- sendRequestOn conn (mkReq HEAD "/" port BodyEmpty [])
        responseStatus r `shouldBe` OK
        body <- bodyOf r
        body `shouldBe` ""
  where
    payload = BS.replicate 256 0x62


sendFileCachedFdTest :: Spec
sendFileCachedFdTest = it "sendfile body: cached fd path (no per-request open/close)" $
  withTempFile "/tmp/wireform-http1-sendfile-cached-test.txt" payload $ \path -> do
    -- Open the fd once and reuse it for multiple requests on the
    -- same keep-alive connection. This is the static-file fast path.
    fb <- wholeFileBodyFd path
    let handler _ =
          pure $
            Response
              OK
              HTTP_1_1
              [("Content-Type", "text/plain")]
              (BodyFile fb)
              (pure [])
    withServer handler $ \port -> do
      withClientConnection (clientCfg port) $ \conn -> do
        -- Two requests on the same connection — exercises the fd
        -- being reused without an intervening close.
        Right r1 <- sendRequestOn conn (mkReq GET "/a" port BodyEmpty [])
        body1 <- bodyOf r1
        body1 `shouldBe` payload
        Right r2 <- sendRequestOn conn (mkReq GET "/b" port BodyEmpty [])
        body2 <- bodyOf r2
        body2 `shouldBe` payload
  where
    payload = BS.replicate 1024 0x63 -- 1 KiB of 'c'


------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{- | Spin up the server on a free port, run the test, then tear down.
Synchronises on the server signalling that it has @listen()@ed.
-}
withServer :: Handler -> (String -> IO ()) -> IO ()
withServer handler action = do
  readyMV <- newEmptyMVar
  let cfg =
        defaultServerConfig
          { serverHost = "127.0.0.1"
          , serverPort = "0" -- placeholder; we'll bind manually
          , serverHandler = handler
          }
  -- Bind ourselves to know the port, then hand the listening socket
  -- to runServerOnSocket via accept loop.
  let hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> expectationFailure "no addr"
    (addr : _) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \listenSock -> do
        NS.setSocketOption listenSock NS.ReuseAddr 1
        NS.bind listenSock (NS.addrAddress addr)
        NS.listen listenSock 128
        boundAddr <- NS.getSocketName listenSock
        let portInt = case boundAddr of
              NS.SockAddrInet p _ -> fromIntegral p :: Int
              _ -> 0
        tid <- forkIO $ acceptForever cfg listenSock readyMV
        putMVar readyMV ()
        action (show portInt) `finally` killThread tid


acceptForever :: ServerConfig -> NS.Socket -> MVar () -> IO ()
acceptForever cfg listenSock _ready = loop
  where
    loop = do
      (s, _) <- NS.accept listenSock
      _ <- forkIO (runServerOnSocket cfg s)
      loop


clientCfg :: String -> ClientConfig
clientCfg p = defaultClientConfig {clientHost = "127.0.0.1", clientPort = p}


mkReq :: Method -> BS.ByteString -> String -> Body -> Headers -> Request
mkReq m t port body extras =
  Request
    { requestMethod = m
    , requestTarget = t
    , requestVersion = HTTP_1_1
    , requestHeaders = [("Host", BS.pack (map (fromIntegral . fromEnum) ("127.0.0.1:" <> port)))] <> extras
    , requestBody = body
    , requestTrailers = pure []
    }


resp200 :: BS.ByteString -> Response
resp200 b = Response OK HTTP_1_1 [("Content-Type", "text/plain")] (BodyBytes b) (pure [])


bodyOf :: Response -> IO BS.ByteString
bodyOf r = drainAll (responseBody r)


drainAll :: Body -> IO BS.ByteString
drainAll BodyEmpty = pure ""
drainAll (BodyBytes bs) = pure bs
drainAll (BodyStream prod) = BS.concat <$> go
  where
    go = do
      mc <- prod
      case mc of
        Nothing -> pure []
        Just c -> (c :) <$> go
