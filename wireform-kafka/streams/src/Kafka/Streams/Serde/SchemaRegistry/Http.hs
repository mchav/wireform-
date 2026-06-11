{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Serde.SchemaRegistry.Http
Description : Confluent Schema-Registry HTTP wire shape

The actual @http-client@ dependency is intentionally /not/
pulled into the wireform-kafka tree — pinning a single HTTP
library is a one-way door for downstream users. Instead this
module exposes the request / response shape Confluent's
Schema-Registry REST API expects, plus a thin
'httpBackedRegistry' constructor that takes a
'HttpRequester' record-of-IO so callers wire whatever transport
their org standardises on.

If you want to use it with @http-client@:

@
import qualified Network.HTTP.Client as HC

myRequester :: HC.Manager -> HttpRequester
myRequester mgr = HttpRequester $ \req -> do
  let !httpReq = ...
  resp <- HC.httpLbs httpReq mgr
  pure HttpResponse
    { respStatus = HC.statusCode (HC.responseStatus resp)
    , reqBody   = LBS.toStrict (HC.responseBody resp)
    }

mkClient mgr =
  httpBackedRegistry "http://schemas.example.com" (myRequester mgr)
@

The wireform-kafka library never opens a TCP socket on the
caller's behalf for Schema Registry; callers stay in control of
TLS configuration / proxy setup / retry policy.
-}
module Kafka.Streams.Serde.SchemaRegistry.Http (
  HttpRequester (..),
  HttpRequest (..),
  HttpResponse (..),
  HttpMethod (..),
  httpBackedRegistry,

  -- * Request builders (exposed for testing)
  registerSchemaRequest,
  lookupSchemaRequest,
  lookupBySubjectRequest,
  compatibilityModeRequest,
  testCompatibilityRequest,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Kafka.Streams.Serde.SchemaRegistry qualified as SR


----------------------------------------------------------------------
-- HTTP shape
----------------------------------------------------------------------

data HttpMethod = HttpGet | HttpPost
  deriving stock (Eq, Show, Generic)


data HttpRequest = HttpRequest
  { reqMethod :: !HttpMethod
  , reqUrl :: !Text
  , reqHeaders :: ![(Text, Text)]
  , reqBody :: !(Maybe ByteString)
  }
  deriving stock (Eq, Show, Generic)


data HttpResponse = HttpResponse
  { respStatus :: !Int
  , respBody :: !ByteString
  }
  deriving stock (Eq, Show, Generic)


newtype HttpRequester = HttpRequester
  { runHttp :: HttpRequest -> IO HttpResponse
  }


----------------------------------------------------------------------
-- Request builders
----------------------------------------------------------------------

{- | @POST /subjects/<subject>/versions@ payload — a JSON
document with a single @"schema"@ field carrying the schema
text. Confluent expects the schema field to be a /string/
(the schema text itself is JSON, but the outer envelope
escapes it). For test / mock purposes we keep the raw bytes
because production users supply a real JSON encoder; the
wireform-kafka library doesn't pin one.
-}
registerSchemaRequest
  :: Text
  -- ^ base url, e.g. @http://schemas.example.com@
  -> SR.SchemaSubject
  -> SR.SchemaPayload
  -> HttpRequest
registerSchemaRequest baseUrl subj payload =
  HttpRequest
    { reqMethod = HttpPost
    , reqUrl =
        baseUrl
          <> "/subjects/"
          <> SR.unSchemaSubject subj
          <> "/versions"
    , reqHeaders = jsonHeaders
    , reqBody = Just (SR.unSchemaPayload payload)
    }


-- | @GET /schemas/ids/<id>@.
lookupSchemaRequest :: Text -> SR.SchemaId -> HttpRequest
lookupSchemaRequest baseUrl sid =
  HttpRequest
    { reqMethod = HttpGet
    , reqUrl =
        baseUrl
          <> "/schemas/ids/"
          <> T.pack (show (SR.unSchemaId sid))
    , reqHeaders = []
    , reqBody = Nothing
    }


-- | @GET /subjects/<subject>/versions/latest@.
lookupBySubjectRequest :: Text -> SR.SchemaSubject -> HttpRequest
lookupBySubjectRequest baseUrl subj =
  HttpRequest
    { reqMethod = HttpGet
    , reqUrl =
        baseUrl
          <> "/subjects/"
          <> SR.unSchemaSubject subj
          <> "/versions/latest"
    , reqHeaders = []
    , reqBody = Nothing
    }


{- | @GET /config/<subject>@ — fetches the compatibility-mode
override for a subject. Confluent returns a JSON document
of the shape @{"compatibilityLevel":"BACKWARD"}@; when the
subject has no per-subject override the registry replies
404 and we report 'SR.defaultCompatibilityMode'.
-}
compatibilityModeRequest :: Text -> SR.SchemaSubject -> HttpRequest
compatibilityModeRequest baseUrl subj =
  HttpRequest
    { reqMethod = HttpGet
    , reqUrl =
        baseUrl
          <> "/config/"
          <> SR.unSchemaSubject subj
    , reqHeaders = jsonHeaders
    , reqBody = Nothing
    }


{- | @POST /compatibility/subjects/<subject>/versions/latest@ — asks
the registry whether the supplied schema is compatible with
the subject's latest version /under the subject's configured
compatibility mode/. Confluent returns @{"is_compatible": true}@
on success.
-}
testCompatibilityRequest
  :: Text -> SR.SchemaSubject -> SR.SchemaPayload -> HttpRequest
testCompatibilityRequest baseUrl subj payload =
  HttpRequest
    { reqMethod = HttpPost
    , reqUrl =
        baseUrl
          <> "/compatibility/subjects/"
          <> SR.unSchemaSubject subj
          <> "/versions/latest"
    , reqHeaders = jsonHeaders
    , reqBody = Just (SR.unSchemaPayload payload)
    }


jsonHeaders :: [(Text, Text)]
jsonHeaders =
  [ ("Content-Type", "application/vnd.schemaregistry.v1+json")
  , ("Accept", "application/vnd.schemaregistry.v1+json")
  ]


----------------------------------------------------------------------
-- Client
----------------------------------------------------------------------

{- | Build a 'SR.SchemaRegistryClient' from an 'HttpRequester'.
Errors are surfaced as 'SR.RegistryHttpError' carrying the
HTTP status + the response body (truncated to 200 chars for
the message; full body is on the wire if the caller wants
to log it).
-}
httpBackedRegistry :: Text -> HttpRequester -> SR.SchemaRegistryClient
httpBackedRegistry baseUrl requester =
  SR.SchemaRegistryClient
    { SR.srRegister = \subj payload -> do
        let !req = registerSchemaRequest baseUrl subj payload
        resp <- runHttp requester req
        pure $ case respStatus resp of
          200 -> case parseSchemaId (respBody resp) of
            Just sid -> Right sid
            Nothing -> Left (SR.RegistryDecode "register: bad id payload")
          s -> Left (SR.RegistryHttpError s (truncBody (respBody resp)))
    , SR.srLookup = \sid -> do
        let !req = lookupSchemaRequest baseUrl sid
        resp <- runHttp requester req
        pure $ case respStatus resp of
          200 -> case parseSchemaText (respBody resp) of
            Just txt -> Right (SR.SchemaPayload txt)
            Nothing -> Left (SR.RegistryDecode "lookup: bad schema payload")
          404 -> Left (SR.SchemaNotFound sid)
          s -> Left (SR.RegistryHttpError s (truncBody (respBody resp)))
    , SR.srLookupBySubject = \subj -> do
        let !req = lookupBySubjectRequest baseUrl subj
        resp <- runHttp requester req
        pure $ case respStatus resp of
          200 -> case parseSubjectVersion (respBody resp) of
            Just (sid, txt) -> Right (sid, SR.SchemaPayload txt)
            Nothing -> Left (SR.RegistryDecode "lookupBySubject: bad payload")
          404 -> Left (SR.SubjectNotFound subj)
          s -> Left (SR.RegistryHttpError s (truncBody (respBody resp)))
    , SR.srCompatibilityMode = \subj -> do
        let !req = compatibilityModeRequest baseUrl subj
        resp <- runHttp requester req
        pure $ case respStatus resp of
          200 -> case parseCompatibilityLevel (respBody resp) of
            Just m -> Right m
            Nothing -> Left (SR.RegistryDecode "compatibilityMode: bad payload")
          -- Confluent replies 404 when the subject has no
          -- per-subject override: fall back to the global default.
          404 -> Right SR.defaultCompatibilityMode
          s -> Left (SR.RegistryHttpError s (truncBody (respBody resp)))
    , SR.srTestCompatibility = \subj payload -> do
        let !req = testCompatibilityRequest baseUrl subj payload
        resp <- runHttp requester req
        pure $ case respStatus resp of
          200 -> case parseIsCompatible (respBody resp) of
            Just True -> Right SR.Compatible
            Just False ->
              Right (SR.Incompatible (truncBody (respBody resp)))
            Nothing -> Left (SR.RegistryDecode "testCompatibility: bad payload")
          409 -> Right (SR.Incompatible (truncBody (respBody resp)))
          s -> Left (SR.RegistryHttpError s (truncBody (respBody resp)))
    }


truncBody :: ByteString -> Text
truncBody bs =
  let !short = BS.take 200 bs
  in TE.decodeUtf8With (\_ _ -> Just '?') short
  where


-- we'd prefer 'lenientDecode' but its lambda shape varies
-- across text versions; the local lambda here is portable.

----------------------------------------------------------------------
-- Tiny JSON-payload extractors
----------------------------------------------------------------------

-- We avoid pulling 'aeson' just for these three call sites. The
-- payloads we need are simple enough to cope with byte-level
-- scanning; production users with stricter validation can pre-
-- process the body in their HttpRequester wrapper.

parseSchemaId :: ByteString -> Maybe SR.SchemaId
parseSchemaId bs = do
  -- Look for @"id":<n>@.
  let !needle = "\"id\":"
  case findSuffix needle bs of
    Nothing -> Nothing
    Just rest ->
      case BSC.readInt (BSC.dropWhile (\c -> c == ' ') rest) of
        Just (n, _) -> Just (SR.SchemaId (fromIntegral n))
        Nothing -> Nothing


parseSchemaText :: ByteString -> Maybe ByteString
parseSchemaText bs =
  -- Confluent returns @{"schema":"<escaped json>"}@. We hand
  -- back the raw bytes between the first quoted schema field
  -- and the final closing brace.
  let !needle = "\"schema\":"
  in case findSuffix needle bs of
       Nothing -> Nothing
       Just rest ->
         -- rest starts with @"...something...@; strip the
         -- surrounding quotes only.
         let !stripped =
               BSC.dropWhile (\c -> c == ' ' || c == '"') rest
         in Just (BSC.takeWhile (/= '"') stripped)


parseSubjectVersion :: ByteString -> Maybe (SR.SchemaId, ByteString)
parseSubjectVersion bs = do
  sid <- parseSchemaId bs
  txt <- parseSchemaText bs
  pure (sid, txt)


{- | Parse @{"compatibilityLevel":"BACKWARD"}@ shaped bodies
(Confluent's @/config@ endpoint reply).
-}
parseCompatibilityLevel :: ByteString -> Maybe SR.CompatibilityMode
parseCompatibilityLevel bs =
  let !needle = "\"compatibilityLevel\":"
  in case findSuffix needle bs of
       Nothing -> Nothing
       Just rest ->
         let !stripped =
               BSC.dropWhile (\c -> c == ' ' || c == '"') rest
             !word = BSC.takeWhile (/= '"') stripped
         in compatModeFromText word


compatModeFromText :: ByteString -> Maybe SR.CompatibilityMode
compatModeFromText bs
  | bs == "NONE" = Just SR.CompatNone
  | bs == "BACKWARD" = Just SR.CompatBackward
  | bs == "BACKWARD_TRANSITIVE" = Just SR.CompatBackwardTransitive
  | bs == "FORWARD" = Just SR.CompatForward
  | bs == "FORWARD_TRANSITIVE" = Just SR.CompatForwardTransitive
  | bs == "FULL" = Just SR.CompatFull
  | bs == "FULL_TRANSITIVE" = Just SR.CompatFullTransitive
  | otherwise = Nothing


{- | Parse @{"is_compatible": true}@ / @{"is_compatible": false}@
bodies. We scan for the literal @true@ / @false@ token after
the field name; both Confluent and Karapace use the same
field name and value spelling.
-}
parseIsCompatible :: ByteString -> Maybe Bool
parseIsCompatible bs =
  let !needle = "\"is_compatible\":"
  in case findSuffix needle bs of
       Nothing -> Nothing
       Just rest ->
         let !skipped = BSC.dropWhile (== ' ') rest
         in case BSC.take 4 skipped of
              "true" -> Just True
              _ -> case BSC.take 5 skipped of
                "false" -> Just False
                _ -> Nothing


{- | Return the bytes /after/ the first occurrence of @needle@
in @hay@, or 'Nothing' if it isn't present.
-}
findSuffix :: ByteString -> ByteString -> Maybe ByteString
findSuffix needle hay = go hay
  where
    !nlen = BS.length needle
    go bs
      | BS.length bs < nlen = Nothing
      | BS.take nlen bs == needle = Just (BS.drop nlen bs)
      | otherwise = go (BS.tail bs)
