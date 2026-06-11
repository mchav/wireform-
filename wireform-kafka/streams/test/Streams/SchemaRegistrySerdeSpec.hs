{-# LANGUAGE OverloadedStrings #-}

module Streams.SchemaRegistrySerdeSpec (tests) where

import Data.ByteString qualified as BS
import Data.IORef
import Kafka.Streams.Serde.SchemaRegistry qualified as SR
import Test.Syd


tests :: Spec
tests =
  describe "Schema Registry serdes" $
    sequence_
      [ it
          "encodeEnvelope / decodeEnvelope round-trip"
          envelope_roundtrip
      , it
          "decodeEnvelope rejects bad magic byte"
          envelope_bad_magic
      , it
          "decodeEnvelope rejects truncated input"
          envelope_truncated
      , it
          "inMemoryRegistry assigns sequential ids"
          inmemory_ids
      , it
          "inMemoryRegistry returns the same id for the same payload"
          inmemory_same_payload
      , it
          "mockHttpRegistry records the HTTP exchanges"
          mock_records
      ]


envelope_roundtrip :: IO ()
envelope_roundtrip = do
  let bs = SR.encodeEnvelope (SR.SchemaId 12345) "payload"
  case SR.decodeEnvelope bs of
    Right (sid, payload) -> do
      sid `shouldBe` SR.SchemaId 12345
      payload `shouldBe` "payload"
    Left err -> error err


envelope_bad_magic :: IO ()
envelope_bad_magic =
  case SR.decodeEnvelope (BS.cons 99 (BS.replicate 8 0)) of
    Left _ -> pure ()
    Right _ -> error "expected bad-magic-byte rejection"


envelope_truncated :: IO ()
envelope_truncated =
  case SR.decodeEnvelope "" of
    Left _ -> pure ()
    Right _ -> error "expected truncated-envelope rejection"


inmemory_ids :: IO ()
inmemory_ids = do
  c <- SR.inMemoryRegistry
  Right (SR.SchemaId i1) <-
    SR.srRegister
      c
      (SR.SchemaSubject "t-value")
      (SR.SchemaPayload "schema1")
  Right (SR.SchemaId i2) <-
    SR.srRegister
      c
      (SR.SchemaSubject "t-value")
      (SR.SchemaPayload "schema2")
  (i2 > i1) `shouldBe` True


inmemory_same_payload :: IO ()
inmemory_same_payload = do
  c <- SR.inMemoryRegistry
  Right s1 <-
    SR.srRegister
      c
      (SR.SchemaSubject "t-value")
      (SR.SchemaPayload "schema")
  Right s2 <-
    SR.srRegister
      c
      (SR.SchemaSubject "t-value")
      (SR.SchemaPayload "schema")
  s1 `shouldBe` s2


mock_records :: IO ()
mock_records = do
  ref <- newIORef []
  let c = SR.mockHttpRegistry ref
  _ <-
    SR.srRegister
      c
      (SR.SchemaSubject "events-value")
      (SR.SchemaPayload "{...}")
  _ <- SR.srLookup c (SR.SchemaId 7)
  log_ <- readIORef ref
  length log_ `shouldBe` 2
