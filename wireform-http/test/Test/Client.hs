{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for the high-level wireform HTTP client (@Network.HTTP.Client.*@).

These exercise the pieces that are testable without spinning up a
live server: media-type matching, request encoding, the assertion
library against a mock transport, VCR record/replay, and the
middleware combinators.
-}
module Test.Client (tests) where

import Codec.Compression.Brotli qualified as Brotli
import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zlib qualified as Zlib
import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.Request (Request (headers))
import Network.HTTP.Client.Response (RawResponse (headers))
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Status qualified as S
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd


-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

data User = User
  { userId :: !Int
  , userName :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)


tests :: Spec
tests =
  describe "Network.HTTP.Client" $
    sequence_
      [ mediaTypeTests
      , requestBuildingTests
      , sendTests
      , middlewareTests
      , vcrTests
      , mockAPITests
      , resourceTests
      , expectationTests
      , stateMachineTests
      , decoderTests
      , faultTests
      , tracingTests
      , compressionTests
      , poolTests
      , streamingVcrTests
      , decoderAltTests
      , pathMatcherTests
      ]


-- ---------------------------------------------------------------------------
-- Media type parsing + matching
-- ---------------------------------------------------------------------------

mediaTypeTests :: Spec
mediaTypeTests =
  describe "MediaType" $
    sequence_
      [ it "parses type/subtype" $ do
          case parseMediaType "application/json" of
            Right m -> do
              mtType m `shouldBe` "application"
              mtSubType m `shouldBe` "json"
            Left err -> expectationFailure err
      , it "parses parameters and lowercases the name" $ do
          case parseMediaType "Application/JSON; charset=utf-8" of
            Right m -> do
              mtType m `shouldBe` "application"
              mtSubType m `shouldBe` "json"
              lookup "charset" (mtParameters m) `shouldBe` Just "utf-8"
            Left err -> expectationFailure err
      , it "wildcard matches" $ do
          matches "application/json" "*/*" `shouldBe` True
          matches "application/json" "application/*" `shouldBe` True
          matches "application/json" "text/*" `shouldBe` False
          matches "text/plain" "text/plain" `shouldBe` True
      , it "Accept header rendering omits q=1" $ do
          acceptHeaderValue
            [ ("application/json", maxQuality)
            , ("text/plain", Quality 0.5)
            ]
            `shouldBe` "application/json, text/plain; q=0.5"
      ]


-- ---------------------------------------------------------------------------
-- Request building / bindVar
-- ---------------------------------------------------------------------------

requestBuildingTests :: Spec
requestBuildingTests =
  describe "Request building" $
    sequence_
      [ it "bindVar substitutes a path variable" $ do
          tpl <- case parseTemplate "/users/{userId}" of
            Right t -> pure t
            Left err -> expectationFailure (show err) >> error "unreachable"
          let req = bindVar' "userId" (42 :: Int) (request M.mGet (templateURI tpl) ())
          requestURIToText (requestURI req) `shouldBe` "/users/42"
      , it "withBody @JSON sets Content-Type" $ do
          let req :: Request BS.ByteString
              req = withBody @JSON (User 1 "alice") (post (compileTemplate "/users"))
              Request {headers = hs} = req
          H.lookupHeader H.hContentType hs `shouldBe` Just "application/json; charset=utf-8"
      , it "setHeader replaces previous header values" $ do
          let req :: Request ()
              req =
                setHeader H.hContentType "text/plain"
                  . setHeader H.hContentType "application/xml"
                  $ get (compileTemplate "/x")
              Request {headers = hs} = req
          H.lookupHeader H.hContentType hs `shouldBe` Just "text/plain"
      ]


compileTemplate :: String -> UriTemplate
compileTemplate s = case parseTemplate s of
  Right t -> t
  Left err -> error ("compileTemplate: " <> show err)


-- Helpers ----------------------------------------------------------------

bindVar' :: Text -> Int -> Request a -> Request a
bindVar' n v r = r {requestURI = bindVar n v (requestURI r)}


-- ---------------------------------------------------------------------------
-- send + mock transport
-- ---------------------------------------------------------------------------

sendTests :: Spec
sendTests =
  describe "send / mocks" $
    sequence_
      [ it "stubJSON decodes a response" $ do
          let transport = stubJSON S.status200 (User 7 "alice")
          Response {responseBody = u} <-
            sendIO transport (get (compileTemplate "/users/7")) (as @JSON @User)
          u `shouldBe` User 7 "alice"
      , it "request log captures method + uri" $ do
          let inner = stubJSON S.status200 (User 1 "bob")
          (t, log_) <- withRequestLog inner
          _ <- sendIO t (get (compileTemplate "/users/1")) (as @JSON @User)
          assertLog log_ (requestCount 1)
          assertLog log_ (anyRequest (hasMethod M.mGet <> hasURI "/users/1"))
          assertLog log_ (anyRequest (hasURIPath "/users/1"))
      , it "decode failure throws DecodeFailure" $ do
          let transport = stub S.status200 "not json at all"
          result <-
            try (sendIO transport (get (compileTemplate "/x")) (as @JSON @User))
              :: IO (Either SomeException (Response User))
          case result of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected DecodeFailure"
      ]


-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

middlewareTests :: Spec
middlewareTests =
  describe "middleware" $
    sequence_
      [ it "withAuth adds Authorization header" $ do
          -- Place the log innermost so it observes the request that the
          -- base transport sees (i.e. after the auth middleware ran).
          (logged, log_) <- withRequestLog (stubStatus S.status200)
          let transport = withAuth (Bearer "tok123") logged
          _ <-
            try (sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text))
              :: IO (Either SomeException (Response Text))
          assertLog log_ (anyRequest (hasHeaderEq H.hAuthorization "Bearer tok123"))
      , it "failFirstN retries to success" $ do
          let canned = errorResp
          retried <- failFirstN 2 canned (stubJSON S.status200 (User 9 "ok"))
          let withRet = withRetry defaultRetryPolicy retried
          Response {responseBody = u} <-
            sendIO withRet (get (compileTemplate "/x")) (as @JSON @User)
          u `shouldBe` User 9 "ok"
      ]


errorResp :: RawResponse
errorResp =
  RawResponse
    { statusCode = S.status503
    , headers = []
    , bodyPopper = pure BS.empty
    , protocolInfo = HTTP1_1
    }


-- ---------------------------------------------------------------------------
-- VCR
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- MockAPI declarative routing
-- ---------------------------------------------------------------------------

mockAPITests :: Spec
mockAPITests =
  describe "MockAPI" $
    sequence_
      [ it "first matching route wins" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mGet <> hasURIPath "/health")
                            (\_ _ -> ok200 "ok")
                        , on
                            (hasMethod M.mGet <> hasURIPathPrefix "/users/")
                            (\_ _ -> json200 (User 1 "alice"))
                        ]
                    , fallback = throwUnexpected
                    }
          Response {responseBody = u} <-
            sendIO api (get (compileTemplate "/users/1")) (as @JSON @User)
          u `shouldBe` User 1 "alice"
      , it "fallback runs when nothing matches" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mGet <> hasURIPath "/health")
                            (\_ _ -> ok200 "ok")
                        ]
                    , fallback = \_ _ -> rawResponse S.status418 [] "i'm a teapot"
                    }
          raw <- sendRaw api =<< prep (get (compileTemplate "/unknown"))
          statusCode raw `shouldBe` S.status418
      , it "hasQueryParam matches" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            ( hasMethod M.mGet
                                <> hasURIPath "/search"
                                <> hasQueryParam "q" "hello"
                            )
                            (\_ _ -> ok200 "found")
                        ]
                    , fallback = \_ _ -> rawResponse S.status404 [] ""
                    }
          raw <- sendRaw api =<< prep (get (compileTemplate "/search?q=hello"))
          statusCode raw `shouldBe` S.status200
      , it "hasJSONBody matches on shape" $ do
          let target = User 1 "alice"
              api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mPost <> hasJSONBody target)
                            (\_ _ -> ok200 "match")
                        ]
                    , fallback = \_ _ -> rawResponse S.status404 [] ""
                    }
          raw <-
            sendRaw api
              =<< prep
                (withBody @JSON target (post (compileTemplate "/users")))
          statusCode raw `shouldBe` S.status200
      ]


-- ---------------------------------------------------------------------------
-- Resource mocks
-- ---------------------------------------------------------------------------

resourceTests :: Spec
resourceTests =
  describe "resource" $
    sequence_
      [ it "POST then GET, then DELETE then GET" $ do
          idVar <- newIORef (0 :: Int)
          let nextId = atomicModifyIORef' idVar (\n -> (n + 1, T.pack (show (n + 1))))
          userRoutes <-
            resource
              ResourceConfig
                { basePath = "/users"
                , idField = \(User i _) -> T.pack (show i)
                , generateId = nextId
                }
          let api = mockAPI MockAPI {routes = userRoutes, fallback = throwUnexpected}

          created <-
            sendIO
              api
              (withBody @JSON (User 0 "alice") (post (compileTemplate "/users")))
              (as @JSON @User)
          responseStatus created `shouldBe` S.status201

          Response {responseBody = u} <-
            sendIO
              api
              (get (compileTemplate "/users/0"))
              (as @JSON @User)
          userName u `shouldBe` "alice"

          delRaw <- sendRaw api =<< prep (delete (compileTemplate "/users/0"))
          statusCode delRaw `shouldBe` S.status204

          notFound <- sendRaw api =<< prep (get (compileTemplate "/users/0"))
          statusCode notFound `shouldBe` S.status404
      ]


-- ---------------------------------------------------------------------------
-- Expectations
-- ---------------------------------------------------------------------------

expectationTests :: Spec
expectationTests =
  describe "withExpectations" $
    sequence_
      [ it "expected counts pass" $ do
          withExpectations
            [ expect_
                (hasMethod M.mGet <> hasURIPath "/users")
                (Exactly 1)
                (json200 (User 1 "alice"))
            ]
            $ \t -> do
              _ <- sendIO t (get (compileTemplate "/users")) (as @JSON @User)
              pure ()
      , it "violated count throws ExpectationNotMet" $ do
          result <- try
            $ withExpectations
              [ expect_
                  (hasMethod M.mGet <> hasURIPath "/users")
                  (Exactly 2)
                  (json200 (User 1 "alice"))
              ]
            $ \t -> do
              _ <- sendIO t (get (compileTemplate "/users")) (as @JSON @User)
              pure ()
          case (result :: Either ExpectationNotMet ()) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected ExpectationNotMet"
      , it "unexpected request throws UnexpectedRequest" $ do
          result <- try
            $ withExpectations
              [ expect_
                  (hasMethod M.mGet <> hasURIPath "/users")
                  AnyTimes
                  (json200 (User 1 "alice"))
              ]
            $ \t -> do
              _ <- sendIO t (get (compileTemplate "/admin")) (as @JSON @User)
              pure ()
          case (result :: Either SomeException ()) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected an exception"
      ]


-- ---------------------------------------------------------------------------
-- State machines
-- ---------------------------------------------------------------------------

stateMachineTests :: Spec
stateMachineTests =
  describe "stateMachine" $
    sequence_
      [ it "polling: pending -> pending -> complete" $ do
          transport <-
            stateMachine
              StateMachine
                { initialState = 0 :: Int
                , transition = \n _ _ ->
                    let body_ =
                          if n < 2
                            then "{\"status\":\"pending\"}"
                            else "{\"status\":\"complete\"}"
                    in (,) (n + 1)
                         <$> rawResponse
                           S.status200
                           [(H.hContentType, "application/json")]
                           body_
                }
          let req = get (compileTemplate "/jobs/1")
          r1 <- sendRaw transport =<< prep req
          r1Body <- rawResponseBytes r1
          BS.isInfixOf "pending" r1Body `shouldBe` True
          r2 <- sendRaw transport =<< prep req
          r2Body <- rawResponseBytes r2
          BS.isInfixOf "pending" r2Body `shouldBe` True
          r3 <- sendRaw transport =<< prep req
          r3Body <- rawResponseBytes r3
          BS.isInfixOf "complete" r3Body `shouldBe` True
      ]


-- ---------------------------------------------------------------------------
-- Decoders
-- ---------------------------------------------------------------------------

decoderTests :: Spec
decoderTests =
  describe "Decoder" $
    sequence_
      [ it "asEither @JSON @JSON returns Right on 2xx" $ do
          let transport = stubJSON S.status200 (User 1 "alice")
          Response {responseBody = r} <-
            sendIO
              transport
              (get (compileTemplate "/users/1"))
              (asEither @JSON @ErrorPayload @JSON @User)
          r `shouldBe` Right (User 1 "alice")
      , it "asEither returns Left on non-2xx" $ do
          let transport = stubJSON S.status404 (ErrorPayload "not found")
          Response {responseBody = r} <-
            sendIO
              transport
              (get (compileTemplate "/users/999"))
              (asEither @JSON @ErrorPayload @JSON @User)
          r `shouldBe` Left (ErrorPayload "not found")
      ]


data ErrorPayload = ErrorPayload {errMsg :: !Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)


-- ---------------------------------------------------------------------------
-- Fault injection
-- ---------------------------------------------------------------------------

faultTests :: Spec
faultTests =
  describe "Faults" $
    sequence_
      [ it "withTruncation cuts the body" $ do
          let inner = stub S.status200 "abcdefghij"
              transport = withTruncation 4 inner
          raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
          bs <- rawResponseBytes raw
          bs `shouldBe` "abcd"
      ]


-- ---------------------------------------------------------------------------
-- Tracing (smoke test: middleware composes without error)
-- ---------------------------------------------------------------------------

tracingTests :: Spec
tracingTests =
  describe "withTracing" $
    sequence_
      [ it "no-op tracing middleware passes the response through" $ do
          let inner = stubJSON S.status200 (User 1 "alice")
              transport = withTracing defaultTracingConfig inner
          Response {responseBody = u} <-
            sendIO transport (get (compileTemplate "/users/1")) (as @JSON @User)
          u `shouldBe` User 1 "alice"
      , it "TracingDisabled short-circuits to the inner transport" $ do
          let inner = stubJSON S.status200 (User 2 "bob")
              transport = withTracing TracingDisabled inner
          Response {responseBody = u} <-
            sendIO transport (get (compileTemplate "/users/2")) (as @JSON @User)
          u `shouldBe` User 2 "bob"
      , it "span lifetime extends until the popper hits EOF" $ do
          -- Smoke test: the popper should still produce its chunk
          -- /after/ sendRaw returns, since the span (and the popper
          -- wrapping it) live past that boundary.
          let inner = stub S.status200 "streamed body"
              transport = withTracing defaultTracingConfig inner
          raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
          first <- bodyPopper raw
          first `shouldBe` "streamed body"
          eof <- bodyPopper raw
          eof `shouldBe` BS.empty
      ]


-- Build a Request BodyStream from any Body-bearing Request for use
-- with sendRaw.
prep :: Body body => Request body -> IO (Request BodyStream)
prep r = prepareRequest [] r


-- ---------------------------------------------------------------------------
-- Streaming VCR + Alt + path matcher
-- ---------------------------------------------------------------------------

streamingVcrTests :: Spec
streamingVcrTests =
  describe "Streaming VCR" $
    sequence_
      [ it "withChunkedRecording preserves chunk boundaries on replay" $ do
          let chunks = ["alpha", "beta", "gamma"] :: [BS.ByteString]
          innerRaw <- do
            p <- popperFromList chunks
            pure $ mockTransport $ \_ ->
              pure
                RawResponse
                  { statusCode = S.status200
                  , Network.HTTP.Client.Response.headers = []
                  , bodyPopper = p
                  , protocolInfo = HTTP1_1
                  }
          withSystemTempDirectory "wire-vcr-stream" $ \dir -> do
            let path = dir <> "/stream.yaml"
            _ <- recordSessionChunked innerRaw path $ \t -> do
              raw <- sendRaw t =<< prep (get (compileTemplate "/x"))
              -- materialise to ensure recording runs
              _ <- rawResponseBytes raw
              pure ()
            cassette <- loadCassette path
            replay <- replayTransport cassette byMethodAndURI
            raw <- sendRaw replay =<< prep (get (compileTemplate "/x"))
            replayed <- pullChunks (bodyPopper raw)
            replayed `shouldBe` chunks
      ]


pullChunks :: IO BS.ByteString -> IO [BS.ByteString]
pullChunks p = go []
  where
    go acc = do
      c <- p
      if BS.null c then pure (reverse acc) else go (c : acc)


decoderAltTests :: Spec
decoderAltTests =
  describe "ResponseDecoder Alt" $
    sequence_
      [ it "<!> defers to the second decoder when the first can't decode" $ do
          let transport =
                stubBytes
                  S.status200
                  [(H.hContentType, "text/plain; charset=utf-8")]
                  "plain bytes"
              decoder :: ResponseDecoder Text
              decoder = as @JSON @Text <!> as @PlainText @Text
          Response {responseBody = t} <-
            sendIO transport (get (compileTemplate "/x")) decoder
          t `shouldBe` "plain bytes"
      ]


pathMatcherTests :: Spec
pathMatcherTests =
  describe "hasPathMatches" $
    sequence_
      [ it "captures simple :id segment" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mGet <> hasPathMatches "/users/:id")
                            (\_ _ -> ok200 "match")
                        ]
                    , fallback = \_ _ -> rawResponse S.status404 [] ""
                    }
          raw <- sendRaw api =<< prep (get (compileTemplate "/users/42"))
          statusCode raw `shouldBe` S.status200
      , it "rejects extra segments" $ do
          let api =
                mockAPI
                  MockAPI
                    { routes =
                        [ on
                            (hasMethod M.mGet <> hasPathMatches "/users/:id")
                            (\_ _ -> ok200 "match")
                        ]
                    , fallback = \_ _ -> rawResponse S.status404 [] ""
                    }
          raw <- sendRaw api =<< prep (get (compileTemplate "/users/42/posts"))
          statusCode raw `shouldBe` S.status404
      ]


-- ---------------------------------------------------------------------------
-- Compression
-- ---------------------------------------------------------------------------

poolTests :: Spec
poolTests =
  describe "ConnectionPool" $
    sequence_
      [ it "newPool / closePool round-trip is clean" $ do
          pool <- newPool defaultPoolConfig
          closePool pool
      , it "withPool runs the action and tears down" $ do
          ran <- newIORef False
          withPool defaultPoolConfig $ \_ -> writeIORef ran True
          readIORef ran >>= (`shouldBe` True)
      , it "ClientConfig.ccPoolConfig defaults to Just" $
          case ccPoolConfig defaultClientConfig of
            Just _ -> pure ()
            Nothing -> expectationFailure "expected a default PoolConfig"
      ]


compressionTests :: Spec
compressionTests =
  describe "withDecompression" $
    sequence_
      [ it "decodes gzip-encoded response" $ do
          let payload = "the quick brown fox jumps over the lazy dog" :: BS.ByteString
              compressed = BSL.toStrict $ GZip.compress (BSL.fromStrict payload)
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [(H.hContentType, "text/plain"), ("Content-Encoding", "gzip")]
                  compressed
              transport = withDecompression inner
          Response {responseBody = t} <-
            sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
          TE.encodeUtf8 t `shouldBe` payload
      , it "decodes deflate (zlib) response" $ do
          let payload = "deflate payload bytes here" :: BS.ByteString
              compressed = BSL.toStrict $ Zlib.compress (BSL.fromStrict payload)
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [(H.hContentType, "text/plain"), ("Content-Encoding", "deflate")]
                  compressed
              transport = withDecompression inner
          Response {responseBody = t} <-
            sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
          TE.encodeUtf8 t `shouldBe` payload
      , it "decodes brotli response" $ do
          let payload =
                "brotli payload that is reasonably long for the encoder"
                  :: BS.ByteString
              compressed = BSL.toStrict $ Brotli.compress (BSL.fromStrict payload)
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [(H.hContentType, "text/plain"), ("Content-Encoding", "br")]
                  compressed
              transport = withDecompression inner
          Response {responseBody = t} <-
            sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
          TE.encodeUtf8 t `shouldBe` payload
      , it "adds Accept-Encoding when absent" $ do
          (logged, log_) <- withRequestLog (stubStatus S.status200)
          let transport = withDecompression logged
          _ <- sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
          assertLog log_ (anyRequest (hasHeaderEq H.hAcceptEncoding "br, gzip, deflate"))
      , it "leaves unknown Content-Encoding alone" $ do
          let payload = "raw bytes" :: BS.ByteString
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [("Content-Encoding", "x-magic")]
                  payload
              transport = withDecompression inner
          raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
          bs <- rawResponseBytes raw
          bs `shouldBe` payload
      , it "strips Content-Encoding + Content-Length after decompression" $ do
          let payload = "hello" :: BS.ByteString
              compressed = BSL.toStrict $ GZip.compress (BSL.fromStrict payload)
              inner = mockTransport $ \_ ->
                rawResponse
                  S.status200
                  [ ("Content-Encoding", "gzip")
                  ,
                    ( H.hContentLength
                    , BS8.pack (show (BS.length compressed))
                    )
                  ]
                  compressed
              transport = withDecompression inner
          raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
          let RawResponse {headers = respHdrs} = raw
          H.lookupHeader "Content-Encoding" respHdrs `shouldBe` Nothing
          H.lookupHeader H.hContentLength respHdrs `shouldBe` Nothing
      ]


vcrTests :: Spec
vcrTests =
  describe "VCR" $
    sequence_
      [ it "record then replay reproduces the response" $ do
          withSystemTempDirectory "wire-vcr" $ \dir -> do
            let cassettePath = dir <> "/login.yaml"
                real = stubJSON S.status201 (User 1 "alice")
                postUsers = post (compileTemplate "/users")
            Response {responseBody = u} <-
              recordSession real cassettePath $ \t ->
                sendIO t postUsers (as @JSON @User)
            u `shouldBe` User 1 "alice"

            cassette <- loadCassette cassettePath
            transport <- replayTransport cassette byMethodAndURI
            Response {responseBody = u2} <-
              sendIO transport postUsers (as @JSON @User)
            u2 `shouldBe` User 1 "alice"
      ]
