{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Schema Registry compatibility-mode probing
-- surface added to "Kafka.Streams.Serde.SchemaRegistry" and its
-- HTTP backing.
module Streams.SchemaRegistryCompatSpec (tests) where

import Data.IORef (newIORef)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Streams.Serde as Serde
import qualified Kafka.Streams.Serde.SchemaRegistry as SR
import qualified Kafka.Streams.Serde.SchemaRegistry.Http as SRHttp

tests :: TestTree
tests = testGroup "Kafka.Streams.Serde.SchemaRegistry.Compat"
  [ inMemory_compat_is_none
  , inMemory_checked_succeeds
  , http_request_shapes
  , http_compatible_reply
  , http_incompatible_reply
  ]

----------------------------------------------------------------------
-- In-memory client
----------------------------------------------------------------------

inMemory_compat_is_none :: TestTree
inMemory_compat_is_none =
  testCase "in-memory client reports CompatNone for every subject" $ do
    cli <- SR.inMemoryRegistry
    SR.srCompatibilityMode cli (SR.SchemaSubject "x") >>= \r ->
      r @?= Right SR.CompatNone

inMemory_checked_succeeds :: TestTree
inMemory_checked_succeeds =
  testCase "registrySerdeChecked degenerates to registrySerde for CompatNone" $ do
    cli <- SR.inMemoryRegistry
    r   <- SR.registrySerdeChecked SR.SchemaRegistrySerdeConfig
      { SR.srscClient  = cli
      , SR.srscSchema  = SR.SchemaPayload "{\"type\":\"string\"}"
      , SR.srscSubject = SR.SchemaSubject "topic-value"
      , SR.srscPayload = Serde.textSerde
      }
    case r of
      Right _ -> pure ()
      Left e  -> error ("expected Right, got Left " <> show e)

----------------------------------------------------------------------
-- HTTP shape
----------------------------------------------------------------------

http_request_shapes :: TestTree
http_request_shapes =
  testCase "HTTP request builders emit the expected paths" $ do
    let !modeReq =
          SRHttp.compatibilityModeRequest
            "http://reg.example"
            (SR.SchemaSubject "events-value")
        !testReq =
          SRHttp.testCompatibilityRequest
            "http://reg.example"
            (SR.SchemaSubject "events-value")
            (SR.SchemaPayload "{}")
    SRHttp.reqUrl modeReq @?= "http://reg.example/config/events-value"
    SRHttp.reqUrl testReq @?=
      "http://reg.example/compatibility/subjects/events-value/versions/latest"
    SRHttp.reqMethod modeReq @?= SRHttp.HttpGet
    SRHttp.reqMethod testReq @?= SRHttp.HttpPost

----------------------------------------------------------------------
-- HTTP-backed client (canned responses)
----------------------------------------------------------------------

cannedRequester
  :: SRHttp.HttpResponse                -- ^ response for compat-mode
  -> SRHttp.HttpResponse                -- ^ response for test-compat
  -> IO SRHttp.HttpRequester
cannedRequester modeResp testResp = do
  countRef <- newIORef (0 :: Int)
  let _ = countRef
  pure $ SRHttp.HttpRequester $ \req ->
    case SRHttp.reqMethod req of
      SRHttp.HttpGet  -> pure modeResp
      SRHttp.HttpPost -> pure testResp

http_compatible_reply :: TestTree
http_compatible_reply =
  testCase "200 + is_compatible:true ⇒ Compatible" $ do
    requester <- cannedRequester
      SRHttp.HttpResponse
        { SRHttp.respStatus = 200
        , SRHttp.respBody   = "{\"compatibilityLevel\":\"BACKWARD\"}"
        }
      SRHttp.HttpResponse
        { SRHttp.respStatus = 200
        , SRHttp.respBody   = "{\"is_compatible\": true}"
        }
    let cli = SRHttp.httpBackedRegistry "http://reg.example" requester
    mode <- SR.srCompatibilityMode cli (SR.SchemaSubject "s")
    mode @?= Right SR.CompatBackward
    compat <- SR.srTestCompatibility cli
                (SR.SchemaSubject "s")
                (SR.SchemaPayload "{}")
    compat @?= Right SR.Compatible

http_incompatible_reply :: TestTree
http_incompatible_reply =
  testCase "is_compatible:false ⇒ Incompatible carries the response body" $ do
    requester <- cannedRequester
      SRHttp.HttpResponse
        { SRHttp.respStatus = 200
        , SRHttp.respBody   = "{\"compatibilityLevel\":\"FULL\"}"
        }
      SRHttp.HttpResponse
        { SRHttp.respStatus = 200
        , SRHttp.respBody   = "{\"is_compatible\": false, \"messages\":[\"bad\"]}"
        }
    let cli = SRHttp.httpBackedRegistry "http://reg.example" requester
    r <- SR.srTestCompatibility cli
            (SR.SchemaSubject "s")
            (SR.SchemaPayload "{}")
    case r of
      Right (SR.Incompatible msg) ->
        assertBool ("expected 'bad' in: " <> T.unpack msg)
                   (T.isInfixOf "bad" msg)
      other -> error ("expected Incompatible, got " <> show other)
