{-# LANGUAGE PackageImports #-}
module Test.WAI (tests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified "http-types" Network.HTTP.Types as WAIHttp
import qualified Network.Wai as Wai
import Network.Wai.Internal (ResponseReceived(ResponseReceived))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Network.HTTP.Message as U
import qualified "wireform-http" Network.HTTP.Types.Body as U
import qualified "wireform-http" Network.HTTP.Types.Header as U
import qualified "wireform-http" Network.HTTP.Types.Method as U
import qualified "wireform-http" Network.HTTP.Types.Status as U
import qualified "wireform-http" Network.HTTP.Types.Version as U
import Network.HTTP.WAI

tests :: TestTree
tests = testGroup "WAI adapter"
  [ testGroup "Primitive conversions"
      [ testStatusRoundtrip
      , testVersionRoundtrip
      , testMethodRoundtrip
      ]
  , testGroup "waiToHandler"
      [ testWaiToHandlerBasic
      , testWaiToHandlerBody
      , testWaiToHandlerHeaders
      , testWaiToHandlerQueryString
      ]
  , testGroup "handlerToWai"
      [ testHandlerToWaiBasic
      , testHandlerToWaiStream
      ]
  , testGroup "Request conversions"
      [ testFromWaiRequestPreservesFields
      , testToWaiRequestPreservesFields
      ]
  ]

------------------------------------------------------------------------
-- Primitive conversion tests
------------------------------------------------------------------------

testStatusRoundtrip :: TestTree
testStatusRoundtrip = testCase "status roundtrip" $ do
  let statuses = [U.status200, U.status404, U.status500, U.Status 418]
  mapM_ (\s -> do
    let waiS = WAIHttp.mkStatus (fromIntegral (U.statusCode s)) ""
        back = U.Status (fromIntegral (WAIHttp.statusCode waiS))
    U.statusCode back @?= U.statusCode s
    ) statuses

testVersionRoundtrip :: TestTree
testVersionRoundtrip = testCase "version roundtrip" $ do
  let versions =
        [ (U.HTTP1_0, WAIHttp.HttpVersion 1 0)
        , (U.HTTP1_1, WAIHttp.HttpVersion 1 1)
        , (U.HTTP2,   WAIHttp.HttpVersion 2 0)
        ]
  mapM_ (\(wf, wai) -> do
    let wai' = WAIHttp.HttpVersion
                 (fromIntegral (U.versionMajor wf))
                 (fromIntegral (U.versionMinor wf))
    wai' @?= wai
    let wf' = U.mkVersion
                (fromIntegral (WAIHttp.httpMajor wai))
                (fromIntegral (WAIHttp.httpMinor wai))
    wf' @?= wf
    ) versions

testMethodRoundtrip :: TestTree
testMethodRoundtrip = testCase "method roundtrip" $ do
  let methods = [U.mGet, U.mPost, U.mPut, U.mDelete, U.mPatch]
  mapM_ (\m -> do
    let wai = U.fromMethod m
        back = U.methodFromBytes wai
    back @?= m
    ) methods

------------------------------------------------------------------------
-- waiToHandler tests
------------------------------------------------------------------------

echoApp :: Wai.Application
echoApp req respond = do
  body <- Wai.consumeRequestBodyStrict req
  respond $ Wai.responseLBS
    WAIHttp.status200
    [ ("X-Method", Wai.requestMethod req)
    , ("X-Path", Wai.rawPathInfo req)
    , ("X-Query", Wai.rawQueryString req)
    , ("Content-Type", "application/octet-stream")
    ]
    body

testWaiToHandlerBasic :: TestTree
testWaiToHandlerBasic = testCase "GET / returns 200" $ do
  let handler = waiToHandler echoApp
  resp <- handler U.Request
    { U.requestMethod    = U.mGet
    , U.requestTarget    = "/"
    , U.requestAuthority = Nothing
    , U.requestScheme    = U.SchemeHttp
    , U.requestHeaders   = []
    , U.requestBody      = U.BodyEmpty
    , U.requestVersion   = U.HTTP1_1
    , U.requestTrailers  = pure []
    }
  U.responseStatus resp @?= U.status200
  assertHeaderEq (U.responseHeaders resp) "X-Method" "GET"
  assertHeaderEq (U.responseHeaders resp) "X-Path" "/"

testWaiToHandlerBody :: TestTree
testWaiToHandlerBody = testCase "POST with body echoes it back" $ do
  let handler = waiToHandler echoApp
      payload = "hello wireform"
  resp <- handler U.Request
    { U.requestMethod    = U.mPost
    , U.requestTarget    = "/echo"
    , U.requestAuthority = Nothing
    , U.requestScheme    = U.SchemeHttp
    , U.requestHeaders   = [(U.hContentType, "text/plain")]
    , U.requestBody      = U.BodyBytes payload
    , U.requestVersion   = U.HTTP1_1
    , U.requestTrailers  = pure []
    }
  U.responseStatus resp @?= U.status200
  case U.responseBody resp of
    U.BodyBytes bs -> bs @?= payload
    other -> assertBool ("expected BodyBytes, got " <> show other) False

testWaiToHandlerHeaders :: TestTree
testWaiToHandlerHeaders = testCase "request headers forwarded to WAI app" $ do
  let app req respond = do
        let ua = maybe "" id $ lookup "User-Agent" (Wai.requestHeaders req)
        respond $ Wai.responseLBS WAIHttp.status200
          [("X-UA", ua)] ""
      handler = waiToHandler app
  resp <- handler U.Request
    { U.requestMethod    = U.mGet
    , U.requestTarget    = "/"
    , U.requestAuthority = Nothing
    , U.requestScheme    = U.SchemeHttp
    , U.requestHeaders   = [(U.hUserAgent, "wireform-test/1.0")]
    , U.requestBody      = U.BodyEmpty
    , U.requestVersion   = U.HTTP1_1
    , U.requestTrailers  = pure []
    }
  assertHeaderEq (U.responseHeaders resp) "X-UA" "wireform-test/1.0"

testWaiToHandlerQueryString :: TestTree
testWaiToHandlerQueryString = testCase "query string preserved" $ do
  let handler = waiToHandler echoApp
  resp <- handler U.Request
    { U.requestMethod    = U.mGet
    , U.requestTarget    = "/search?q=haskell&page=1"
    , U.requestAuthority = Nothing
    , U.requestScheme    = U.SchemeHttp
    , U.requestHeaders   = []
    , U.requestBody      = U.BodyEmpty
    , U.requestVersion   = U.HTTP1_1
    , U.requestTrailers  = pure []
    }
  assertHeaderEq (U.responseHeaders resp) "X-Path" "/search"
  assertHeaderEq (U.responseHeaders resp) "X-Query" "?q=haskell&page=1"

------------------------------------------------------------------------
-- handlerToWai tests
------------------------------------------------------------------------

testHandlerToWaiBasic :: TestTree
testHandlerToWaiBasic = testCase "200 response round-trips" $ do
  let handler _req = pure U.Response
        { U.responseStatus     = U.status200
        , U.responseVersion    = U.HTTP1_1
        , U.responseHeaders    = [(mk "X-Custom", "value")]
        , U.responseBody       = U.BodyBytes "ok"
        , U.responseTrailers   = pure []
        , U.responseH2StreamId = 0
        , U.responseCancel     = pure ()
        }
      app = handlerToWai handler
  resultRef <- newIORef Nothing
  _ <- app Wai.defaultRequest $ \resp -> do
    writeIORef resultRef (Just resp)
    pure ResponseReceived
  mResp <- readIORef resultRef
  case mResp of
    Nothing -> assertBool "WAI respond callback not called" False
    Just resp -> do
      WAIHttp.statusCode (Wai.responseStatus resp) @?= 200
      assertBool "has X-Custom header" $
        lookup "X-Custom" (Wai.responseHeaders resp) == Just "value"

testHandlerToWaiStream :: TestTree
testHandlerToWaiStream = testCase "streaming body round-trips" $ do
  chunksRef <- newIORef ["chunk1" :: ByteString, "chunk2", "chunk3"]
  let handler _req = pure U.Response
        { U.responseStatus     = U.status200
        , U.responseVersion    = U.HTTP1_1
        , U.responseHeaders    = []
        , U.responseBody       = U.BodyStream $ do
            chunks <- readIORef chunksRef
            case chunks of
              []     -> pure Nothing
              (c:cs) -> do
                writeIORef chunksRef cs
                pure (Just c)
        , U.responseTrailers   = pure []
        , U.responseH2StreamId = 0
        , U.responseCancel     = pure ()
        }
      app = handlerToWai handler
  resultRef <- newIORef Nothing
  _ <- app Wai.defaultRequest $ \resp -> do
    let (status, _hdrs, withBody) = Wai.responseToStream resp
    body <- withBody $ \streamBody -> do
      collectedRef <- newIORef []
      streamBody
        (\builder -> do
          let bs = LBS.toStrict (toLazyByteString builder)
          collected <- readIORef collectedRef
          writeIORef collectedRef (collected <> [bs]))
        (pure ())
      collected <- readIORef collectedRef
      pure (BS.concat collected)
    writeIORef resultRef (Just (WAIHttp.statusCode status, body))
    pure ResponseReceived
  mResult <- readIORef resultRef
  case mResult of
    Nothing -> assertBool "respond callback not called" False
    Just (code, body) -> do
      code @?= 200
      body @?= "chunk1chunk2chunk3"

------------------------------------------------------------------------
-- Request conversion tests
------------------------------------------------------------------------

testFromWaiRequestPreservesFields :: TestTree
testFromWaiRequestPreservesFields = testCase "fromWaiRequest preserves fields" $ do
  let waiReq = Wai.defaultRequest
        { Wai.requestMethod = "POST"
        , Wai.rawPathInfo = "/api/users"
        , Wai.rawQueryString = "?active=true"
        , Wai.httpVersion = WAIHttp.http11
        , Wai.requestHeaders = [("Content-Type", "application/json"), ("Host", "example.com")]
        , Wai.isSecure = True
        }
      req = fromWaiRequest waiReq
  U.requestMethod req @?= U.mPost
  U.requestTarget req @?= "/api/users?active=true"
  U.requestScheme req @?= U.SchemeHttps
  U.requestVersion req @?= U.HTTP1_1
  assertBool "has Content-Type header" $
    U.lookupHeader U.hContentType (U.requestHeaders req) == Just "application/json"

testToWaiRequestPreservesFields :: TestTree
testToWaiRequestPreservesFields = testCase "toWaiRequest preserves fields" $ do
  waiReq <- toWaiRequest U.Request
    { U.requestMethod    = U.mPut
    , U.requestTarget    = "/items/42?format=json"
    , U.requestAuthority = Just "api.example.com"
    , U.requestScheme    = U.SchemeHttps
    , U.requestHeaders   = [(U.hContentType, "application/json")]
    , U.requestBody      = U.BodyBytes "{\"name\":\"test\"}"
    , U.requestVersion   = U.HTTP1_1
    , U.requestTrailers  = pure []
    }
  Wai.requestMethod waiReq @?= "PUT"
  Wai.rawPathInfo waiReq @?= "/items/42"
  Wai.rawQueryString waiReq @?= "?format=json"
  Wai.isSecure waiReq @?= True
  assertBool "has Content-Type header" $
    lookup "Content-Type" (Wai.requestHeaders waiReq) == Just "application/json"
  assertBool "has host header" $
    Wai.requestHeaderHost waiReq == Just "api.example.com"
  case Wai.requestBodyLength waiReq of
    Wai.KnownLength n -> n @?= fromIntegral (BS.length "{\"name\":\"test\"}")
    Wai.ChunkedBody -> assertBool "expected KnownLength" False
  bodyChunk <- Wai.getRequestBodyChunk waiReq
  bodyChunk @?= "{\"name\":\"test\"}"
  eof <- Wai.getRequestBodyChunk waiReq
  eof @?= BS.empty

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

assertHeaderEq :: U.Headers -> BS.ByteString -> BS.ByteString -> IO ()
assertHeaderEq hdrs name expected =
  case U.lookupHeader (mk name) hdrs of
    Just v  -> v @?= expected
    Nothing -> assertBool ("missing header: " <> BS8.unpack name) False
