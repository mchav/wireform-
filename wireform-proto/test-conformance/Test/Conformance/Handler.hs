{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import qualified Data.Aeson as Aeson

import qualified Proto.Decode as PD
import qualified Proto.Encode as PE

import Test.Conformance.Schema

-- | One request -> one response. Pure (no IO needed); the
-- runner-binary entry point in "Runner" handles the stdin/stdout
-- length-prefixed framing.
handleRequest :: ConformanceRequest -> ConformanceResponse
handleRequest req
  | mt == "conformance.FailureSet"            = failureSetResponse
  | mt == "protobuf_test_messages.proto3.TestAllTypesProto3" =
      handleTestAllTypesProto3 req
  | otherwise = skipped ("Unknown message type: " <> mt)
  where
    mt = req.conformanceRequestMessageType

-- | First-request sentinel: the runner asks for the FailureSet
-- before any real test. We always return an empty set; the
-- driver does its own pass/fail accounting from the runner's
-- summary output.
failureSetResponse :: ConformanceResponse
failureSetResponse =
  let payload = PE.encodeMessage defaultFailureSet
  in defaultConformanceResponse
       { conformanceResponseProtobufPayload = payload
       }

handleTestAllTypesProto3 :: ConformanceRequest -> ConformanceResponse
handleTestAllTypesProto3 req =
  case payloadInputFormat req of
    PayloadProtobuf bs -> case PD.decodeMessage bs of
      Left e   -> parseErr (T.pack (show e))
      Right tm -> serializeTAT outFmt tm
    PayloadJson js -> case Aeson.eitherDecodeStrictText js of
      Left e   -> parseErr (T.pack e)
      Right tm -> serializeTAT outFmt tm
    PayloadText _ -> skipped "TEXT_FORMAT input not supported"
    PayloadJspb _ -> skipped "JSPB input not supported"
    PayloadNone   -> skipped "no payload set"
  where
    outFmt = req.conformanceRequestRequestedOutputFormat

-- | Encode a parsed TestAllTypesProto3 in the requested output
-- format and wrap the bytes / string in the appropriate
-- 'ConformanceResponse' arm.
serializeTAT :: WireFormat -> TestAllTypesProto3 -> ConformanceResponse
serializeTAT fmt tm = case fmt of
  Protobuf -> defaultConformanceResponse
    { conformanceResponseProtobufPayload = PE.encodeMessage tm }
  Json
    | hasUnknownFields tm -> skipped
        "JSON output skipped: payload contains fields outside the spliced \
        \schema (e.g. WKT arms); their JSON shape isn't recoverable from \
        \the unknown-fields slot."
    | otherwise -> defaultConformanceResponse
        { conformanceResponseJsonPayload =
            decodeUtf8Lazy (Aeson.encode tm) }
  TextFormat  -> skipped "TEXT_FORMAT output not implemented"
  Jspb        -> skipped "JSPB output not supported"
  Unspecified -> serializeError "UNSPECIFIED requested_output_format"

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
payloadInputFormat r
  | not (BS.null pb)   = PayloadProtobuf pb
  | not (T.null js)    = PayloadJson js
  | not (T.null tx)    = PayloadText tx
  | not (T.null jspb)  = PayloadJspb jspb
  | otherwise          = PayloadNone
  where
    pb   = r.conformanceRequestProtobufPayload
    js   = r.conformanceRequestJsonPayload
    tx   = r.conformanceRequestTextPayload
    jspb = r.conformanceRequestJspbPayload

skipped :: T.Text -> ConformanceResponse
skipped t = defaultConformanceResponse { conformanceResponseSkipped = t }

parseErr :: T.Text -> ConformanceResponse
parseErr t = defaultConformanceResponse { conformanceResponseParseError = t }

serializeError :: T.Text -> ConformanceResponse
serializeError t = defaultConformanceResponse
  { conformanceResponseSerializeError = t }
