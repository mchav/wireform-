{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the high-level wireform HTTP client (@Network.HTTP.Wire.*@).
--
-- These exercise the pieces that are testable without spinning up a
-- live server: media-type matching, request encoding, the assertion
-- library against a mock transport, VCR record/replay, and the
-- middleware combinators.
module Test.Wire (tests) where

import Control.Exception (try, SomeException)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO.Temp (withSystemTempDirectory)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Wire

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
tests = testGroup "Network.HTTP.Wire"
  [ mediaTypeTests
  , requestBuildingTests
  , sendTests
  , middlewareTests
  , vcrTests
  ]

-- ---------------------------------------------------------------------------
-- Media type parsing + matching
-- ---------------------------------------------------------------------------

mediaTypeTests :: TestTree
mediaTypeTests = testGroup "MediaType"
  [ testCase "parses type/subtype" $ do
      let Right m = parseMediaType "application/json"
      mtType m    @?= "application"
      mtSubType m @?= "json"

  , testCase "parses parameters and lowercases the name" $ do
      let Right m = parseMediaType "Application/JSON; charset=utf-8"
      mtType m    @?= "application"
      mtSubType m @?= "json"
      lookup "charset" (mtParameters m) @?= Just "utf-8"

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

seqApply :: a -> (a -> b) -> b
seqApply x f = f x

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
