{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Kafka.Client.Telemetry
-- Description : KIP-714 client-instance-id derivation
--
-- The Java SDK ships
-- @KafkaConsumer.clientInstanceId(Duration)@ /
-- @KafkaProducer.clientInstanceId(Duration)@ /
-- @AdminClient.clientInstanceId(Duration)@ /
-- @KafkaStreams.clientInstanceIds(Duration)@ as getters for a
-- broker-assigned UUID the broker stamps on the
-- @GetTelemetrySubscriptions@ response (KIP-714). Until that
-- pipeline lands here, every client returns a /deterministic/
-- local id derived from its configured @client.id@ (or
-- @application.id@ for Streams) — the same client process
-- always reports the same id, which preserves the JVM contract
-- "this id is per-process and stable".
--
-- This module centralises the derivation in one place so the
-- consumer / producer / admin / streams call sites stay
-- consistent, and so the property "same input ⇒ same output"
-- can be tested in isolation.
--
-- When the broker-side telemetry assignment lands, the
-- per-client getters switch to the broker-assigned id and
-- this module shrinks to a fallback used pre-handshake.
module Kafka.Client.Telemetry
  ( clientInstanceIdFromText
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- | Derive a 16-byte deterministic client-instance id from a
-- text seed (typically the @client.id@ or @application.id@).
-- Pads short seeds with @\\0@ bytes and truncates long ones so
-- the result is always exactly 16 bytes. JVM telemetry expects
-- a UUID-shaped binary; we use the raw 16-byte payload
-- directly — broker-side assignment will replace this with a
-- proper RFC 4122 UUID once KIP-714's
-- @GetTelemetrySubscriptions@ pipeline is wired.
clientInstanceIdFromText :: Text -> ByteString
clientInstanceIdFromText t =
  let !bs = BS.append (TE.encodeUtf8 t) (BS.replicate 16 0)
   in BS.take 16 bs
