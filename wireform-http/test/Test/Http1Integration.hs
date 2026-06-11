{-# LANGUAGE OverloadedStrings #-}

{- | End-to-end HTTP\/1.x integration through the unified
'Network.HTTP.Connection' \/ 'Network.HTTP.Server' surface.

Each test runs a real TCP listener on @127.0.0.1@ with an
ephemeral port (@bind 0@) and drives the unified accept loop via
'runServerOnListener', so the test exercises the whole
client → server roundtrip including the conversion layer.
TLS is covered separately because it needs cert fixtures.
-}
module Test.Http1Integration (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally)
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
  describe "HTTP/1.x integration" $
    sequence_
      [ helloWorld
      , echoBody
      , streamingResponse
      , chunkedRequestWithEmptyTrailers
      , expectContinue
      ]


------------------------------------------------------------------------

helloWorld :: Spec
helloWorld = it "GET hello world (HTTP/1.1)" $
  withTestServer http1Only (\_ -> pure (resp200 "hi")) $ \port -> do
    (status, ver, body) <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/" port BodyEmpty [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, responseVersion r, b)
    status `shouldBe` S.status200
    body `shouldBe` "hi"
    ver `shouldBe` V.HTTP1_1


echoBody :: Spec
echoBody = it "POST echoes Content-Length body" $
  withTestServer http1Only echo $ \port -> do
    (status, body) <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "POST" "/" port (BodyBytes "rountrip") [])
      b <- drainBody (responseBody r)
      pure (responseStatus r, b)
    status `shouldBe` S.status200
    body `shouldBe` "rountrip"
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body)


streamingResponse :: Spec
streamingResponse = it "streaming response body arrives chunk-wise" $
  withTestServer http1Only stream $ \port -> do
    body <- runClient http1Only port $ \c -> do
      r <- sendOn c (mkRequest "GET" "/stream" port BodyEmpty [])
      drainBody (responseBody r)
    body `shouldBe` "alphabetagamma"
  where
    stream _ = do
      ref <- newIORef ["alpha", "beta", "gamma"]
      pure
        Response
          { responseStatus = S.status200
          , responseVersion = V.HTTP1_1
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


chunkedRequestWithEmptyTrailers :: Spec
chunkedRequestWithEmptyTrailers =
  it "chunked request body: handler sees [] trailers when none sent" $
    withTestServer http1Only handler $ \port -> do
      chunkRef <- newIORef ["one ", "two ", "three!"]
      let producer = do
            xs <- readIORef chunkRef
            case xs of
              [] -> pure Nothing
              (h : t) -> writeIORef chunkRef t >> pure (Just h)
      (status, body) <- runClient http1Only port $ \c -> do
        r <- sendOn c (mkRequest "POST" "/" port (BodyStream producer) [])
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      status `shouldBe` S.status200
      body `shouldBe` "one two three!"
  where
    handler req = do
      body <- drainBody (requestBody req)
      trs <- requestTrailers req
      trs `shouldBe` []
      pure (resp200 body)


expectContinue :: Spec
expectContinue =
  it "Expect: 100-continue: server emits 100, client absorbs it" $
    withTestServer http1Only echo $ \port -> do
      (status, body) <- runClient http1Only port $ \c -> do
        r <-
          sendOn
            c
            ( mkRequest
                "POST"
                "/"
                port
                (BodyBytes "payload")
                [(CI.mk "Expect", "100-continue")]
            )
        b <- drainBody (responseBody r)
        pure (responseStatus r, b)
      -- The final response we observe is the handler's 200, not the
      -- interim 100; the underlying HTTP/1 client transparently
      -- skips informational responses.
      status `shouldBe` S.status200
      body `shouldBe` "payload"
  where
    echo req = do
      body <- drainBody (requestBody req)
      pure (resp200 body)


------------------------------------------------------------------------
-- Plumbing
------------------------------------------------------------------------

{- | Bind an ephemeral TCP port, spawn the unified accept loop on
it, and pass the resolved port string to the action.  The
listener is torn down on exit.
-}
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
    , requestVersion = V.HTTP1_1
    , requestTrailers = pure []
    }


resp200 :: BS.ByteString -> Response
resp200 body =
  Response
    { responseStatus = S.status200
    , responseVersion = V.HTTP1_1
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
