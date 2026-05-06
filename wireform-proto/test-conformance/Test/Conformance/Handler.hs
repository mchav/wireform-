{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The actual conformance handler: takes one
-- 'ConformanceRequest', decodes the inner payload as a
-- 'TestAllTypesProto3' (or 'FailureSet' for the runner's first
-- request), re-encodes it in the requested output format, and
-- returns one 'ConformanceResponse'.
--
-- Wire format coverage:
--
--   * @PROTOBUF@ in -> @PROTOBUF@ out: full round-trip via the
--     @loadProto@-generated codecs. Unknown fields are preserved
--     end-to-end through the message's unknown-fields slot, so
--     even tests carrying WKT-typed fields (which the schema in
--     "Test.Conformance.Schema" deliberately omits — see haddock
--     there) round-trip byte-identically.
--   * @PROTOBUF@ in -> @JSON@ out: encode via 'Aeson.encode'
--     (handler skips when the message has any unknown fields,
--     because the JSON shape isn't well-defined for them).
--   * @JSON@ in -> @PROTOBUF@ out: decode via 'Aeson.decode'.
--   * @JSON@ in -> @JSON@ out: same as above plus an
--     @encode . decode@ pass.
--   * Anything else (JSPB, TEXT_FORMAT) is reported as
--     @Skipped@; the upstream runner treats Skipped as a
--     non-failure for those categories.
--
-- The first request the upstream runner sends carries
-- @messageType = "conformance.FailureSet"@; the handler
-- responds with an empty 'FailureSet' (we don't pre-declare
-- expected failures here — the test-suite driver
-- "Test.Conformance.Driver" interprets the runner's overall
-- summary itself).
module Test.Conformance.Handler
  ( handleRequest
  ) where

import Control.Exception (SomeException, evaluate, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import qualified Data.Aeson as Aeson

import Data.Proxy (Proxy (..))

import qualified Proto.Decode as PD
import qualified Proto.Encode as PE
import qualified Proto.TextFormat as PTF

import Test.Conformance.Schema

-- | One request -> one response. Threaded through 'IO' so the
-- WKT range-error path (Timestamp/Duration too large/small)
-- can return @serialize_error@ via 'try' instead of escaping
-- as a process-level runtime exception.
handleRequest :: ConformanceRequest -> IO ConformanceResponse
handleRequest req
  | mt == "conformance.FailureSet"            = pure failureSetResponse
  | mt == "protobuf_test_messages.proto3.TestAllTypesProto3" =
      handleTestAllTypesProto3 req
  | otherwise = pure (skipped ("Unknown message type: " <> mt))
  where
    mt = conformanceRequestMessageType req

-- | First-request sentinel: the runner asks for the FailureSet
-- before any real test. We always return an empty set; the
-- driver does its own pass/fail accounting from the runner's
-- summary output.
failureSetResponse :: ConformanceResponse
failureSetResponse =
  let payload = PE.encodeMessage defaultFailureSet
  in defaultConformanceResponse
       { conformanceResponseResult =
           Just (ConformanceResponse'Result'ProtobufPayload payload)
       }

handleTestAllTypesProto3 :: ConformanceRequest -> IO ConformanceResponse
handleTestAllTypesProto3 req =
  case payloadInputFormat req of
    PayloadProtobuf bs -> case PD.decodeMessage bs of
      Left e   -> pure (parseErr (T.pack (show e)))
      Right tm -> serializeTAT outFmt tm
    PayloadJson js -> case Aeson.eitherDecodeStrictText js of
      Left e   -> pure (parseErr (T.pack e))
      Right tm -> serializeTAT outFmt tm
    PayloadText _ -> pure (skipped "TEXT_FORMAT input not supported")
    PayloadJspb _ -> pure (skipped "JSPB input not supported")
    PayloadNone   -> pure (skipped "no payload set")
  where
    outFmt = conformanceRequestRequestedOutputFormat req

-- | Encode a parsed TestAllTypesProto3 in the requested output
-- format and wrap the bytes / string in the appropriate
-- 'ConformanceResponse' arm. JSON / TEXT_FORMAT encoding is
-- done under 'try' so the WKT range-check 'error' calls
-- (Timestamp\/Duration ProtoInputTooLarge\/Small) surface as
-- @serialize_error@ rather than killing the process.
serializeTAT :: WireFormat -> TestAllTypesProto3 -> IO ConformanceResponse
serializeTAT fmt tm = case fmt of
  Protobuf -> pure defaultConformanceResponse
    { conformanceResponseResult = Just
        (ConformanceResponse'Result'ProtobufPayload (PE.encodeMessage tm)) }
  Json
    | hasUnknownFields tm -> pure (skipped
        "JSON output skipped: payload contains fields outside the spliced \
        \schema (e.g. WKT arms); their JSON shape isn't recoverable from \
        \the unknown-fields slot.")
    | otherwise -> trySerialize "JSON" $ do
        bs <- evaluate (Aeson.encode tm)
        evaluate (decodeUtf8Lazy bs)
        >>= \t -> pure defaultConformanceResponse
          { conformanceResponseResult = Just
              (ConformanceResponse'Result'JsonPayload t) }
  TextFormat  -> trySerialize "TEXT_FORMAT" $ do
    !pbtxt <- evaluate (PTF.typedToTextPretty (Proxy :: Proxy TestAllTypesProto3) tm)
    pure defaultConformanceResponse
      { conformanceResponseResult = Just
          (ConformanceResponse'Result'TextPayload pbtxt) }
  Jspb        -> pure (skipped "JSPB output not supported")
  Unspecified -> pure (serializeError "UNSPECIFIED requested_output_format")

-- | Wrap an IO action that builds a 'ConformanceResponse' so
-- any 'SomeException' (typically from a WKT canonical-range
-- check) becomes a @serialize_error@ response.
trySerialize :: T.Text -> IO ConformanceResponse -> IO ConformanceResponse
trySerialize tag act = do
  res <- try act
  case res of
    Left (e :: SomeException) ->
      pure (serializeError (tag <> ": " <> T.pack (show e)))
    Right r -> pure r

-- | Aeson.encode produces a lazy 'BL.ByteString' of UTF-8; the
-- 'ConformanceResponse' wants a 'Text' for the @json_payload@
-- arm. Round-trip via 'TLE.decodeUtf8'.
decodeUtf8Lazy :: BL.ByteString -> T.Text
decodeUtf8Lazy = TL.toStrict . TLE.decodeUtf8

-- | Did the wire-format decoder route any tags into the
-- record's unknown-fields slot? If so, JSON encoding loses
-- those fields, so the handler reports Skipped rather than a
-- partial success.
hasUnknownFields :: TestAllTypesProto3 -> Bool
hasUnknownFields = not . null . testAllTypesProto3UnknownFields

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

data PayloadInput
  = PayloadProtobuf !BS.ByteString
  | PayloadJson     !T.Text
  | PayloadText     !T.Text
  | PayloadJspb     !T.Text
  | PayloadNone

payloadInputFormat :: ConformanceRequest -> PayloadInput
payloadInputFormat r = case r.conformanceRequestPayload of
  Just (ConformanceRequest'Payload'ProtobufPayload bs) -> PayloadProtobuf bs
  Just (ConformanceRequest'Payload'JsonPayload     t)  -> PayloadJson t
  Just (ConformanceRequest'Payload'TextPayload     t)  -> PayloadText t
  Just (ConformanceRequest'Payload'JspbPayload     t)  -> PayloadJspb t
  Nothing                                              -> PayloadNone

skipped :: T.Text -> ConformanceResponse
skipped t = defaultConformanceResponse
  { conformanceResponseResult = Just (ConformanceResponse'Result'Skipped t) }

parseErr :: T.Text -> ConformanceResponse
parseErr t = defaultConformanceResponse
  { conformanceResponseResult = Just (ConformanceResponse'Result'ParseError t) }

serializeError :: T.Text -> ConformanceResponse
serializeError t = defaultConformanceResponse
  { conformanceResponseResult = Just (ConformanceResponse'Result'SerializeError t) }
