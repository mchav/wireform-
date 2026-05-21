{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the high-level wireform HTTP client (@Network.HTTP.Client.*@).
--
-- These exercise the pieces that are testable without spinning up a
-- live server: media-type matching, request encoding, the assertion
-- library against a mock transport, VCR record/replay, and the
-- middleware combinators.
module Test.Client (tests) where

import qualified Codec.Compression.Brotli as Brotli
import qualified Codec.Compression.GZip   as GZip
import qualified Codec.Compression.Zlib   as Zlib
import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.IO.Temp (withSystemTempDirectory)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Client

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

data User = User
  { userId   :: !Int
  , userName :: !Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

tests :: TestTree
tests = testGroup "Network.HTTP.Client"
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

mediaTypeTests :: TestTree
mediaTypeTests = testGroup "MediaType"
  [ testCase "parses type/subtype" $ do
      case parseMediaType "application/json" of
        Right m -> do
          mtType m    @?= "application"
          mtSubType m @?= "json"
        Left err -> assertFailure err

  , testCase "parses parameters and lowercases the name" $ do
      case parseMediaType "Application/JSON; charset=utf-8" of
        Right m -> do
          mtType m    @?= "application"
          mtSubType m @?= "json"
          lookup "charset" (mtParameters m) @?= Just "utf-8"
        Left err -> assertFailure err

  , testCase "wildcard matches" $ do
      matches "application/json" "*/*"          @?= True
      matches "application/json" "application/*" @?= True
      matches "application/json" "text/*"        @?= False
      matches "text/plain"       "text/plain"    @?= True

  , testCase "Accept header rendering omits q=1" $ do
      acceptHeaderValue
        [ ("application/json", maxQuality)
        , ("text/plain", Quality 0.5)
        ] @?= "application/json, text/plain; q=0.5"
  ]

-- ---------------------------------------------------------------------------
-- Request building / bindVar
-- ---------------------------------------------------------------------------

requestBuildingTests :: TestTree
requestBuildingTests = testGroup "Request building"
  [ testCase "bindVar substitutes a path variable" $ do
      tpl <- case parseTemplate "/users/{userId}" of
        Right t  -> pure t
        Left err -> assertFailure (show err) >> error "unreachable"
      let req = bindVar' "userId" (42 :: Int) (request M.mGet (templateURI tpl) ())
      requestURIToText (requestURI req) @?= "/users/42"

  , testCase "withBody @JSON sets Content-Type" $ do
      let req :: Request BS.ByteString
          req = withBody @JSON (User 1 "alice") (post (compileTemplate "/users"))
          Request { headers = hs } = req
      H.lookupHeader H.hContentType hs @?= Just "application/json; charset=utf-8"

  , testCase "setHeader replaces previous header values" $ do
      let req :: Request ()
          req = setHeader H.hContentType "text/plain"
              . setHeader H.hContentType "application/xml"
              $ get (compileTemplate "/x")
          Request { headers = hs } = req
      H.lookupHeader H.hContentType hs @?= Just "text/plain"
  ]

compileTemplate :: String -> UriTemplate
compileTemplate s = case parseTemplate s of
  Right t  -> t
  Left err -> error ("compileTemplate: " <> show err)

-- Helpers ----------------------------------------------------------------

bindVar' :: Text -> Int -> Request a -> Request a
bindVar' n v r = r { requestURI = bindVar n v (requestURI r) }

-- ---------------------------------------------------------------------------
-- send + mock transport
-- ---------------------------------------------------------------------------

sendTests :: TestTree
sendTests = testGroup "send / mocks"
  [ testCase "stubJSON decodes a response" $ do
      let transport = stubJSON S.status200 (User 7 "alice")
      Response { responseBody = u } <-
        sendIO transport (get (compileTemplate "/users/7")) (as @JSON @User)
      u @?= User 7 "alice"

  , testCase "request log captures method + uri" $ do
      let inner = stubJSON S.status200 (User 1 "bob")
      (t, log_) <- withRequestLog inner
      _ <- sendIO t (get (compileTemplate "/users/1")) (as @JSON @User)
      assertLog log_ (requestCount 1)
      assertLog log_ (anyRequest (hasMethod M.mGet <> hasURI "/users/1"))
      assertLog log_ (anyRequest (hasURIPath "/users/1"))

  , testCase "decode failure throws DecodeFailure" $ do
      let transport = stub S.status200 "not json at all"
      result <- try (sendIO transport (get (compileTemplate "/x")) (as @JSON @User))
              :: IO (Either SomeException (Response User))
      case result of
        Left _  -> pure ()
        Right _ -> assertFailure "expected DecodeFailure"
  ]

-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

middlewareTests :: TestTree
middlewareTests = testGroup "middleware"
  [ testCase "withAuth adds Authorization header" $ do
      -- Place the log innermost so it observes the request that the
      -- base transport sees (i.e. after the auth middleware ran).
      (logged, log_) <- withRequestLog (stubStatus S.status200)
      let transport = withAuth (Bearer "tok123") logged
      _ <- try (sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text))
             :: IO (Either SomeException (Response Text))
      assertLog log_ (anyRequest (hasHeaderEq H.hAuthorization "Bearer tok123"))

  , testCase "failFirstN retries to success" $ do
      let canned = errorResp
      retried <- failFirstN 2 canned (stubJSON S.status200 (User 9 "ok"))
      let withRet = withRetry defaultRetryPolicy retried
      Response { responseBody = u } <-
        sendIO withRet (get (compileTemplate "/x")) (as @JSON @User)
      u @?= User 9 "ok"
  ]

errorResp :: RawResponse
errorResp = RawResponse
  { statusCode   = S.status503
  , headers      = []
  , bodyPopper   = pure BS.empty
  , protocolInfo = HTTP1_1
  }

-- ---------------------------------------------------------------------------
-- VCR
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- MockAPI declarative routing
-- ---------------------------------------------------------------------------

mockAPITests :: TestTree
mockAPITests = testGroup "MockAPI"
  [ testCase "first matching route wins" $ do
      let api = mockAPI MockAPI
            { routes =
                [ on (hasMethod M.mGet <> hasURIPath "/health")
                    (\_ _ -> ok200 "ok")
                , on (hasMethod M.mGet <> hasURIPathPrefix "/users/")
                    (\_ _ -> json200 (User 1 "alice"))
                ]
            , fallback = throwUnexpected
            }
      Response { responseBody = u } <-
        sendIO api (get (compileTemplate "/users/1")) (as @JSON @User)
      u @?= User 1 "alice"

  , testCase "fallback runs when nothing matches" $ do
      let api = mockAPI MockAPI
            { routes   = [on (hasMethod M.mGet <> hasURIPath "/health")
                              (\_ _ -> ok200 "ok")]
            , fallback = \_ _ -> rawResponse S.status418 [] "i'm a teapot"
            }
      raw <- sendRaw api =<< prep (get (compileTemplate "/unknown"))
      statusCode raw @?= S.status418

  , testCase "hasQueryParam matches" $ do
      let api = mockAPI MockAPI
            { routes =
                [ on (hasMethod M.mGet
                       <> hasURIPath "/search"
                       <> hasQueryParam "q" "hello")
                    (\_ _ -> ok200 "found")
                ]
            , fallback = \_ _ -> rawResponse S.status404 [] ""
            }
      raw <- sendRaw api =<< prep (get (compileTemplate "/search?q=hello"))
      statusCode raw @?= S.status200

  , testCase "hasJSONBody matches on shape" $ do
      let target = User 1 "alice"
          api = mockAPI MockAPI
            { routes =
                [ on (hasMethod M.mPost <> hasJSONBody target)
                    (\_ _ -> ok200 "match")
                ]
            , fallback = \_ _ -> rawResponse S.status404 [] ""
            }
      raw <- sendRaw api =<< prep
        (withBody @JSON target (post (compileTemplate "/users")))
      statusCode raw @?= S.status200
  ]

-- ---------------------------------------------------------------------------
-- Resource mocks
-- ---------------------------------------------------------------------------

resourceTests :: TestTree
resourceTests = testGroup "resource"
  [ testCase "POST then GET, then DELETE then GET" $ do
      idVar <- newIORef (0 :: Int)
      let nextId = atomicModifyIORef' idVar (\n -> (n + 1, T.pack (show (n + 1))))
      userRoutes <- resource ResourceConfig
        { basePath   = "/users"
        , idField    = \(User i _) -> T.pack (show i)
        , generateId = nextId
        }
      let api = mockAPI MockAPI { routes = userRoutes, fallback = throwUnexpected }

      created <- sendIO api
        (withBody @JSON (User 0 "alice") (post (compileTemplate "/users")))
        (as @JSON @User)
      responseStatus created @?= S.status201

      Response { responseBody = u } <- sendIO api
        (get (compileTemplate "/users/0"))
        (as @JSON @User)
      userName u @?= "alice"

      delRaw <- sendRaw api =<< prep (delete (compileTemplate "/users/0"))
      statusCode delRaw @?= S.status204

      notFound <- sendRaw api =<< prep (get (compileTemplate "/users/0"))
      statusCode notFound @?= S.status404
  ]

-- ---------------------------------------------------------------------------
-- Expectations
-- ---------------------------------------------------------------------------

expectationTests :: TestTree
expectationTests = testGroup "withExpectations"
  [ testCase "expected counts pass" $ do
      withExpectations
        [ expect_ (hasMethod M.mGet <> hasURIPath "/users")
                  (Exactly 1)
                  (json200 (User 1 "alice"))
        ]
        $ \t -> do
          _ <- sendIO t (get (compileTemplate "/users")) (as @JSON @User)
          pure ()

  , testCase "violated count throws ExpectationNotMet" $ do
      result <- try $ withExpectations
        [ expect_ (hasMethod M.mGet <> hasURIPath "/users")
                  (Exactly 2)
                  (json200 (User 1 "alice"))
        ]
        $ \t -> do
          _ <- sendIO t (get (compileTemplate "/users")) (as @JSON @User)
          pure ()
      case (result :: Either ExpectationNotMet ()) of
        Left _  -> pure ()
        Right _ -> assertFailure "expected ExpectationNotMet"

  , testCase "unexpected request throws UnexpectedRequest" $ do
      result <- try $ withExpectations
        [ expect_ (hasMethod M.mGet <> hasURIPath "/users")
                  AnyTimes
                  (json200 (User 1 "alice"))
        ]
        $ \t -> do
          _ <- sendIO t (get (compileTemplate "/admin")) (as @JSON @User)
          pure ()
      case (result :: Either SomeException ()) of
        Left _  -> pure ()
        Right _ -> assertFailure "expected an exception"
  ]

-- ---------------------------------------------------------------------------
-- State machines
-- ---------------------------------------------------------------------------

stateMachineTests :: TestTree
stateMachineTests = testGroup "stateMachine"
  [ testCase "polling: pending -> pending -> complete" $ do
      transport <- stateMachine StateMachine
        { initialState = 0 :: Int
        , transition = \n _ _ ->
            let body_ = if n < 2 then "{\"status\":\"pending\"}"
                                 else "{\"status\":\"complete\"}"
            in (,) (n + 1) <$> rawResponse S.status200
                  [(H.hContentType, "application/json")] body_
        }
      let req = get (compileTemplate "/jobs/1")
      r1 <- sendRaw transport =<< prep req
      r1Body <- rawResponseBytes r1
      BS.isInfixOf "pending" r1Body @?= True
      r2 <- sendRaw transport =<< prep req
      r2Body <- rawResponseBytes r2
      BS.isInfixOf "pending" r2Body @?= True
      r3 <- sendRaw transport =<< prep req
      r3Body <- rawResponseBytes r3
      BS.isInfixOf "complete" r3Body @?= True
  ]

-- ---------------------------------------------------------------------------
-- Decoders
-- ---------------------------------------------------------------------------

decoderTests :: TestTree
decoderTests = testGroup "Decoder"
  [ testCase "asEither @JSON @JSON returns Right on 2xx" $ do
      let transport = stubJSON S.status200 (User 1 "alice")
      Response { responseBody = r } <- sendIO transport
        (get (compileTemplate "/users/1"))
        (asEither @JSON @ErrorPayload @JSON @User)
      r @?= Right (User 1 "alice")

  , testCase "asEither returns Left on non-2xx" $ do
      let transport = stubJSON S.status404 (ErrorPayload "not found")
      Response { responseBody = r } <- sendIO transport
        (get (compileTemplate "/users/999"))
        (asEither @JSON @ErrorPayload @JSON @User)
      r @?= Left (ErrorPayload "not found")
  ]

data ErrorPayload = ErrorPayload { errMsg :: !Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

-- ---------------------------------------------------------------------------
-- Fault injection
-- ---------------------------------------------------------------------------

faultTests :: TestTree
faultTests = testGroup "Faults"
  [ testCase "withTruncation cuts the body" $ do
      let inner = stub S.status200 "abcdefghij"
          transport = withTruncation 4 inner
      raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
      bs <- rawResponseBytes raw
      bs @?= "abcd"
  ]

-- ---------------------------------------------------------------------------
-- Tracing (smoke test: middleware composes without error)
-- ---------------------------------------------------------------------------

tracingTests :: TestTree
tracingTests = testGroup "withTracing"
  [ testCase "no-op tracing middleware passes the response through" $ do
      let inner = stubJSON S.status200 (User 1 "alice")
          transport = withTracing defaultTracingConfig inner
      Response { responseBody = u } <-
        sendIO transport (get (compileTemplate "/users/1")) (as @JSON @User)
      u @?= User 1 "alice"

  , testCase "TracingDisabled short-circuits to the inner transport" $ do
      let inner = stubJSON S.status200 (User 2 "bob")
          transport = withTracing TracingDisabled inner
      Response { responseBody = u } <-
        sendIO transport (get (compileTemplate "/users/2")) (as @JSON @User)
      u @?= User 2 "bob"

  , testCase "span lifetime extends until the popper hits EOF" $ do
      -- Smoke test: the popper should still produce its chunk
      -- /after/ sendRaw returns, since the span (and the popper
      -- wrapping it) live past that boundary.
      let inner = stub S.status200 "streamed body"
          transport = withTracing defaultTracingConfig inner
      raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
      first <- bodyPopper raw
      first @?= "streamed body"
      eof <- bodyPopper raw
      eof @?= BS.empty
  ]

-- Build a Request BodyStream from any Body-bearing Request for use
-- with sendRaw.
prep :: Body body => Request body -> IO (Request BodyStream)
prep r = prepareRequest [] r

-- ---------------------------------------------------------------------------
-- Streaming VCR + Alt + path matcher
-- ---------------------------------------------------------------------------

streamingVcrTests :: TestTree
streamingVcrTests = testGroup "Streaming VCR"
  [ testCase "withChunkedRecording preserves chunk boundaries on replay" $ do
      let chunks = ["alpha", "beta", "gamma"] :: [BS.ByteString]
      innerRaw <- do
        p <- popperFromList chunks
        pure $ mockTransport $ \_ -> pure RawResponse
          { statusCode    = S.status200
          , Network.HTTP.Client.headers       = []
          , bodyPopper    = p
          , protocolInfo  = HTTP1_1
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
        replayed @?= chunks
  ]

pullChunks :: IO BS.ByteString -> IO [BS.ByteString]
pullChunks p = go []
  where
    go acc = do
      c <- p
      if BS.null c then pure (reverse acc) else go (c : acc)

decoderAltTests :: TestTree
decoderAltTests = testGroup "ResponseDecoder Alt"
  [ testCase "<!> defers to the second decoder when the first can't decode" $ do
      let transport = stubBytes S.status200
            [(H.hContentType, "text/plain; charset=utf-8")]
            "plain bytes"
          decoder :: ResponseDecoder Text
          decoder = as @JSON @Text <!> as @PlainText @Text
      Response { responseBody = t } <-
        sendIO transport (get (compileTemplate "/x")) decoder
      t @?= "plain bytes"
  ]

pathMatcherTests :: TestTree
pathMatcherTests = testGroup "hasPathMatches"
  [ testCase "captures simple :id segment" $ do
      let api = mockAPI MockAPI
            { routes =
                [ on (hasMethod M.mGet <> hasPathMatches "/users/:id")
                    (\_ _ -> ok200 "match")
                ]
            , fallback = \_ _ -> rawResponse S.status404 [] ""
            }
      raw <- sendRaw api =<< prep (get (compileTemplate "/users/42"))
      statusCode raw @?= S.status200

  , testCase "rejects extra segments" $ do
      let api = mockAPI MockAPI
            { routes =
                [ on (hasMethod M.mGet <> hasPathMatches "/users/:id")
                    (\_ _ -> ok200 "match")
                ]
            , fallback = \_ _ -> rawResponse S.status404 [] ""
            }
      raw <- sendRaw api =<< prep (get (compileTemplate "/users/42/posts"))
      statusCode raw @?= S.status404
  ]

-- ---------------------------------------------------------------------------
-- Compression
-- ---------------------------------------------------------------------------

poolTests :: TestTree
poolTests = testGroup "ConnectionPool"
  [ testCase "newPool / closePool round-trip is clean" $ do
      pool <- newPool defaultPoolConfig
      closePool pool

  , testCase "withPool runs the action and tears down" $ do
      ran <- newIORef False
      withPool defaultPoolConfig $ \_ -> writeIORef ran True
      readIORef ran >>= (@?= True)

  , testCase "ClientConfig.ccPoolConfig defaults to Just" $
      case ccPoolConfig defaultClientConfig of
        Just _  -> pure ()
        Nothing -> assertFailure "expected a default PoolConfig"
  ]

compressionTests :: TestTree
compressionTests = testGroup "withDecompression"
  [ testCase "decodes gzip-encoded response" $ do
      let payload = "the quick brown fox jumps over the lazy dog" :: BS.ByteString
          compressed = BSL.toStrict $ GZip.compress (BSL.fromStrict payload)
          inner = mockTransport $ \_ -> rawResponse S.status200
            [(H.hContentType, "text/plain"), ("Content-Encoding", "gzip")]
            compressed
          transport = withDecompression inner
      Response { responseBody = t } <-
        sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
      TE.encodeUtf8 t @?= payload

  , testCase "decodes deflate (zlib) response" $ do
      let payload = "deflate payload bytes here" :: BS.ByteString
          compressed = BSL.toStrict $ Zlib.compress (BSL.fromStrict payload)
          inner = mockTransport $ \_ -> rawResponse S.status200
            [(H.hContentType, "text/plain"), ("Content-Encoding", "deflate")]
            compressed
          transport = withDecompression inner
      Response { responseBody = t } <-
        sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
      TE.encodeUtf8 t @?= payload

  , testCase "decodes brotli response" $ do
      let payload = "brotli payload that is reasonably long for the encoder"
                      :: BS.ByteString
          compressed = BSL.toStrict $ Brotli.compress (BSL.fromStrict payload)
          inner = mockTransport $ \_ -> rawResponse S.status200
            [(H.hContentType, "text/plain"), ("Content-Encoding", "br")]
            compressed
          transport = withDecompression inner
      Response { responseBody = t } <-
        sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
      TE.encodeUtf8 t @?= payload

  , testCase "adds Accept-Encoding when absent" $ do
      (logged, log_) <- withRequestLog (stubStatus S.status200)
      let transport = withDecompression logged
      _ <- sendIO transport (get (compileTemplate "/x")) (as @PlainText @Text)
      assertLog log_ (anyRequest (hasHeaderEq H.hAcceptEncoding "br, gzip, deflate"))

  , testCase "leaves unknown Content-Encoding alone" $ do
      let payload = "raw bytes" :: BS.ByteString
          inner = mockTransport $ \_ -> rawResponse S.status200
            [("Content-Encoding", "x-magic")]
            payload
          transport = withDecompression inner
      raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
      bs <- rawResponseBytes raw
      bs @?= payload

  , testCase "strips Content-Encoding + Content-Length after decompression" $ do
      let payload = "hello" :: BS.ByteString
          compressed = BSL.toStrict $ GZip.compress (BSL.fromStrict payload)
          inner = mockTransport $ \_ -> rawResponse S.status200
            [ ("Content-Encoding", "gzip")
            , (H.hContentLength,
                BS8.pack (show (BS.length compressed)))
            ]
            compressed
          transport = withDecompression inner
      raw <- sendRaw transport =<< prep (get (compileTemplate "/x"))
      let RawResponse { headers = respHdrs } = raw
      H.lookupHeader "Content-Encoding" respHdrs @?= Nothing
      H.lookupHeader H.hContentLength    respHdrs @?= Nothing
  ]

vcrTests :: TestTree
vcrTests = testGroup "VCR"
  [ testCase "record then replay reproduces the response" $ do
      withSystemTempDirectory "wire-vcr" $ \dir -> do
        let cassettePath = dir <> "/login.yaml"
            real = stubJSON S.status201 (User 1 "alice")
            postUsers = post (compileTemplate "/users")
        Response { responseBody = u } <-
          recordSession real cassettePath $ \t ->
            sendIO t postUsers (as @JSON @User)
        u @?= User 1 "alice"

        cassette <- loadCassette cassettePath
        transport <- replayTransport cassette byMethodAndURI
        Response { responseBody = u2 } <-
          sendIO transport postUsers (as @JSON @User)
        u2 @?= User 1 "alice"
  ]
