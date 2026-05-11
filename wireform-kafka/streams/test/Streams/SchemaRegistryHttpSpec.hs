{-# LANGUAGE OverloadedStrings #-}

module Streams.SchemaRegistryHttpSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Streams.Serde.SchemaRegistry as SR
import qualified Kafka.Streams.Serde.SchemaRegistry.Http as H

tests :: TestTree
tests = testGroup "Schema Registry HTTP-backed client"
  [ testCase "registerSchemaRequest hits POST /subjects/<s>/versions"
      register_url
  , testCase "lookupSchemaRequest hits GET /schemas/ids/<id>"
      lookup_url
  , testCase "lookupBySubjectRequest hits GET /subjects/<s>/versions/latest"
      latest_url
  , testCase "register: 200 returns the parsed SchemaId"
      success_register
  , testCase "lookup: 404 returns SchemaNotFound"
      not_found_lookup
  , testCase "register: non-200 returns RegistryHttpError"
      http_error
  ]

register_url :: IO ()
register_url = do
  let r = H.registerSchemaRequest "http://schemas.example.com"
            (SR.SchemaSubject "events-value")
            (SR.SchemaPayload "{\"type\":\"string\"}")
  H.reqMethod r @?= H.HttpPost
  H.reqUrl r    @?= "http://schemas.example.com/subjects/events-value/versions"

lookup_url :: IO ()
lookup_url = do
  let r = H.lookupSchemaRequest "http://schemas.example.com" (SR.SchemaId 7)
  H.reqMethod r @?= H.HttpGet
  H.reqUrl r    @?= "http://schemas.example.com/schemas/ids/7"

latest_url :: IO ()
latest_url = do
  let r = H.lookupBySubjectRequest "http://schemas.example.com"
            (SR.SchemaSubject "events-value")
  H.reqMethod r @?= H.HttpGet
  H.reqUrl r    @?= "http://schemas.example.com/subjects/events-value/versions/latest"

stubRequester :: IORef [H.HttpRequest] -> H.HttpResponse -> H.HttpRequester
stubRequester ref resp = H.HttpRequester $ \req -> do
  modifyIORef' ref (req :)
  pure resp

success_register :: IO ()
success_register = do
  ref <- newIORef []
  let resp = H.HttpResponse 200 (BSC.pack "{\"id\":42}")
      cli  = H.httpBackedRegistry "http://x.example" (stubRequester ref resp)
  r <- SR.srRegister cli (SR.SchemaSubject "s") (SR.SchemaPayload "p")
  r @?= Right (SR.SchemaId 42)

not_found_lookup :: IO ()
not_found_lookup = do
  ref <- newIORef []
  let resp = H.HttpResponse 404 ""
      cli  = H.httpBackedRegistry "http://x.example" (stubRequester ref resp)
  r <- SR.srLookup cli (SR.SchemaId 7)
  r @?= Left (SR.SchemaNotFound (SR.SchemaId 7))

http_error :: IO ()
http_error = do
  ref <- newIORef []
  let resp = H.HttpResponse 500 (BSC.pack "internal error")
      cli  = H.httpBackedRegistry "http://x.example" (stubRequester ref resp)
  r <- SR.srRegister cli (SR.SchemaSubject "s") (SR.SchemaPayload "p")
  case r of
    Left (SR.RegistryHttpError 500 _) -> pure ()
    other -> error ("expected RegistryHttpError 500, got " <> show other)
