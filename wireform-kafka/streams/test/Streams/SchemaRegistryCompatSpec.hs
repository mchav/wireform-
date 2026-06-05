{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Schema Registry compatibility-mode probing
-- surface added to "Kafka.Streams.Serde.SchemaRegistry" and its
-- HTTP backing.
module Streams.SchemaRegistryCompatSpec (tests) where

import Data.IORef (newIORef)
import qualified Data.Text as T
import Test.Syd

import qualified Kafka.Streams.Serde as Serde
import qualified Kafka.Streams.Serde.SchemaRegistry as SR
import qualified Kafka.Streams.Serde.SchemaRegistry.Http as SRHttp

tests :: Spec
tests = describe "Kafka.Streams.Serde.SchemaRegistry.Compat" $ sequence_
  [ inMemory_compat_is_none
  , inMemory_checked_succeeds
  , http_request_shapes
  , http_compatible_reply
  , http_incompatible_reply
  ]

----------------------------------------------------------------------
-- In-memory client
----------------------------------------------------------------------

inMemory_compat_is_none :: Spec
inMemory_compat_is_none =
  it "in-memory client reports CompatNone for every subject" $ do
    cli <- SR.inMemoryRegistry
    SR.srCompatibilityMode cli (SR.SchemaSubject "x") >>= \r ->
      r `shouldBe` Right SR.CompatNone

inMemory_checked_succeeds :: Spec
inMemory_checked_succeeds =
  it "registrySerdeChecked degenerates to registrySerde for CompatNone" $ do
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

http_request_shapes :: Spec
http_request_shapes =
  it "HTTP request builders emit the expected paths" $ do
    let !modeReq =
          SRHttp.compatibilityModeRequest
            "http://reg.example"
            (SR.SchemaSubject "events-value")
        !testReq =
          SRHttp.testCompatibilityRequest
            "http://reg.example"
            (SR.SchemaSubject "events-value")
            (SR.SchemaPayload "{}")
    SRHttp.reqUrl modeReq `shouldBe` "http://reg.example/config/events-value"
    SRHttp.reqUrl testReq `shouldBe`
      "http://reg.example/compatibility/subjects/events-value/versions/latest"
    SRHttp.reqMethod modeReq `shouldBe` SRHttp.HttpGet
    SRHttp.reqMethod testReq `shouldBe` SRHttp.HttpPost

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

http_compatible_reply :: Spec
http_compatible_reply =
  it "200 + is_compatible:true ⇒ Compatible" $ do
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
    mode `shouldBe` Right SR.CompatBackward
    compat <- SR.srTestCompatibility cli
                (SR.SchemaSubject "s")
                (SR.SchemaPayload "{}")
    compat `shouldBe` Right SR.Compatible

http_incompatible_reply :: Spec
http_incompatible_reply =
  it "is_compatible:false ⇒ Incompatible carries the response body" $ do
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
        (if (T.isInfixOf "bad" msg) then pure () else expectationFailure ("expected 'bad' in: " <> T.unpack msg))
      other -> error ("expected Incompatible, got " <> show other)
