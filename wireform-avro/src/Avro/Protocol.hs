{-# LANGUAGE BangPatterns #-}

{- | Avro Protocol (IPC) definitions.

An Avro "Protocol" describes a set of RPC messages along with the named
types they reference. This module provides data types for protocols, their
JSON serialization, and the MD5 fingerprint used for handshakes.
-}
module Avro.Protocol (
  -- * Protocol types
  AvroProtocol (..),
  AvroMessage (..),
  AvroParam (..),

  -- * Protocol JSON encoding\/decoding
  protocolToJSON,
  protocolFromJSON,

  -- * Fingerprint
  avroProtocolFingerprint,

  -- * Handshake types
  HandshakeMatch (..),
  HandshakeRequest (..),
  HandshakeResponse (..),
  handshakeRequestToJSON,
  handshakeRequestFromJSON,
  handshakeResponseToJSON,
  handshakeResponseFromJSON,
) where

import Avro.JSON (avroSchemaFromJSON, avroSchemaToJSON)
import Avro.Schema (AvroType (..))
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V


-- | An Avro Protocol: a named collection of message definitions and their types.
data AvroProtocol = AvroProtocol
  { protoName :: !Text
  , protoNamespace :: !(Maybe Text)
  , protoDoc :: !(Maybe Text)
  , protoTypes :: ![AvroType]
  , protoMessages :: ![(Text, AvroMessage)]
  }
  deriving stock (Show, Eq)


-- | A single message in an Avro Protocol.
data AvroMessage = AvroMessage
  { msgRequest :: ![AvroParam]
  , msgResponse :: !AvroType
  , msgErrors :: !(Maybe AvroType)
  , msgOneWay :: !Bool
  }
  deriving stock (Show, Eq)


-- | A parameter in an Avro message request.
data AvroParam = AvroParam
  { paramName :: !Text
  , paramType :: !AvroType
  }
  deriving stock (Show, Eq)


-- | Encode an 'AvroProtocol' to its JSON representation.
protocolToJSON :: AvroProtocol -> Aeson.Value
protocolToJSON proto =
  Aeson.Object $
    KM.fromList $
      catMaybes
        [ Just ("protocol", Aeson.String (protoName proto))
        , fmap (\ns -> ("namespace", Aeson.String ns)) (protoNamespace proto)
        , fmap (\d -> ("doc", Aeson.String d)) (protoDoc proto)
        , if null (protoTypes proto)
            then Nothing
            else Just ("types", Aeson.Array $ V.fromList $ map avroSchemaToJSON (protoTypes proto))
        , if null (protoMessages proto)
            then Nothing
            else
              Just
                ( "messages"
                , Aeson.Object $
                    KM.fromList
                      [ (Key.fromText name, messageToJSON msg)
                      | (name, msg) <- protoMessages proto
                      ]
                )
        ]


messageToJSON :: AvroMessage -> Aeson.Value
messageToJSON msg =
  Aeson.Object $
    KM.fromList $
      catMaybes
        [ Just
            ( "request"
            , Aeson.Array $
                V.fromList
                  [ Aeson.Object $
                      KM.fromList
                        [ ("name", Aeson.String (paramName p))
                        , ("type", avroSchemaToJSON (paramType p))
                        ]
                  | p <- msgRequest msg
                  ]
            )
        , Just ("response", avroSchemaToJSON (msgResponse msg))
        , fmap (\e -> ("errors", avroSchemaToJSON e)) (msgErrors msg)
        , if msgOneWay msg then Just ("one-way", Aeson.Bool True) else Nothing
        ]


-- | Decode an 'AvroProtocol' from its JSON representation.
protocolFromJSON :: Aeson.Value -> Either String AvroProtocol
protocolFromJSON (Aeson.Object obj) = do
  name <- requireString "protocol" obj
  let ns = optString "namespace" obj
      doc = optString "doc" obj
  types <- case KM.lookup "types" obj of
    Nothing -> Right []
    Just (Aeson.Array arr) -> mapM avroSchemaFromJSON (V.toList arr)
    Just _ -> Left "protocol 'types' must be an array"
  messages <- case KM.lookup "messages" obj of
    Nothing -> Right []
    Just (Aeson.Object msgObj) ->
      mapM
        ( \(k, v) -> do
            msg <- messageFromJSON v
            Right (Key.toText k, msg)
        )
        (KM.toList msgObj)
    Just _ -> Left "protocol 'messages' must be an object"
  Right
    AvroProtocol
      { protoName = name
      , protoNamespace = ns
      , protoDoc = doc
      , protoTypes = types
      , protoMessages = messages
      }
protocolFromJSON _ = Left "protocol must be a JSON object"


messageFromJSON :: Aeson.Value -> Either String AvroMessage
messageFromJSON (Aeson.Object obj) = do
  request <- case KM.lookup "request" obj of
    Nothing -> Right []
    Just (Aeson.Array arr) -> mapM paramFromJSON (V.toList arr)
    Just _ -> Left "message 'request' must be an array"
  response <- case KM.lookup "response" obj of
    Just v -> avroSchemaFromJSON v
    Nothing -> Left "message missing 'response'"
  let errors = case KM.lookup "errors" obj of
        Just v -> case avroSchemaFromJSON v of
          Right ty -> Just ty
          Left _ -> Nothing
        Nothing -> Nothing
  let oneWay = case KM.lookup "one-way" obj of
        Just (Aeson.Bool True) -> True
        _ -> False
  Right
    AvroMessage
      { msgRequest = request
      , msgResponse = response
      , msgErrors = errors
      , msgOneWay = oneWay
      }
messageFromJSON _ = Left "message must be a JSON object"


paramFromJSON :: Aeson.Value -> Either String AvroParam
paramFromJSON (Aeson.Object obj) = do
  name <- requireString "name" obj
  ty <- case KM.lookup "type" obj of
    Just v -> avroSchemaFromJSON v
    Nothing -> Left "param missing 'type'"
  Right AvroParam {paramName = name, paramType = ty}
paramFromJSON _ = Left "param must be a JSON object"


{- | Compute the MD5 fingerprint of a protocol's canonical JSON form.
Used for Avro IPC handshake matching.
-}
avroProtocolFingerprint :: AvroProtocol -> ByteString
avroProtocolFingerprint proto =
  let !json = protocolToJSON proto
      !canonical = BL.toStrict $ Aeson.encode json
  in MD5.hash canonical


--------------------------------------------------------------------------------
-- Handshake types
--------------------------------------------------------------------------------

-- | Handshake match result.
data HandshakeMatch
  = -- | Both client and server hashes match
    MatchBoth
  | -- | Only client hash matches
    MatchClient
  | -- | Neither matches
    MatchNone
  deriving stock (Show, Eq, Ord, Enum, Bounded)


-- | Avro IPC handshake request.
data HandshakeRequest = HandshakeRequest
  { hsReqClientHash :: !ByteString
  , hsReqClientProtocol :: !(Maybe Text)
  , hsReqServerHash :: !ByteString
  , hsReqMeta :: !(Maybe [(Text, ByteString)])
  }
  deriving stock (Show, Eq)


-- | Avro IPC handshake response.
data HandshakeResponse = HandshakeResponse
  { hsRespMatch :: !HandshakeMatch
  , hsRespServerProtocol :: !(Maybe Text)
  , hsRespServerHash :: !(Maybe ByteString)
  , hsRespMeta :: !(Maybe [(Text, ByteString)])
  }
  deriving stock (Show, Eq)


handshakeMatchToText :: HandshakeMatch -> Text
handshakeMatchToText MatchBoth = "BOTH"
handshakeMatchToText MatchClient = "CLIENT"
handshakeMatchToText MatchNone = "NONE"


handshakeMatchFromText :: Text -> Either String HandshakeMatch
handshakeMatchFromText "BOTH" = Right MatchBoth
handshakeMatchFromText "CLIENT" = Right MatchClient
handshakeMatchFromText "NONE" = Right MatchNone
handshakeMatchFromText other = Left $ "unknown handshake match: " ++ T.unpack other


-- | Encode a handshake request to JSON.
handshakeRequestToJSON :: HandshakeRequest -> Aeson.Value
handshakeRequestToJSON req =
  Aeson.Object $
    KM.fromList $
      catMaybes
        [ Just ("clientHash", bytesToBase64JSON (hsReqClientHash req))
        , fmap (\p -> ("clientProtocol", Aeson.String p)) (hsReqClientProtocol req)
        , Just ("serverHash", bytesToBase64JSON (hsReqServerHash req))
        , fmap (\m -> ("meta", metaToJSON m)) (hsReqMeta req)
        ]


-- | Decode a handshake request from JSON.
handshakeRequestFromJSON :: Aeson.Value -> Either String HandshakeRequest
handshakeRequestFromJSON (Aeson.Object obj) = do
  clientHash <- requireBytes "clientHash" obj
  let clientProto = optString "clientProtocol" obj
  serverHash <- requireBytes "serverHash" obj
  let meta = case KM.lookup "meta" obj of
        Just v -> case metaFromJSON v of
          Right m -> Just m
          Left _ -> Nothing
        Nothing -> Nothing
  Right
    HandshakeRequest
      { hsReqClientHash = clientHash
      , hsReqClientProtocol = clientProto
      , hsReqServerHash = serverHash
      , hsReqMeta = meta
      }
handshakeRequestFromJSON _ = Left "handshake request must be a JSON object"


-- | Encode a handshake response to JSON.
handshakeResponseToJSON :: HandshakeResponse -> Aeson.Value
handshakeResponseToJSON resp =
  Aeson.Object $
    KM.fromList $
      catMaybes
        [ Just ("match", Aeson.String (handshakeMatchToText (hsRespMatch resp)))
        , fmap (\p -> ("serverProtocol", Aeson.String p)) (hsRespServerProtocol resp)
        , fmap (\h -> ("serverHash", bytesToBase64JSON h)) (hsRespServerHash resp)
        , fmap (\m -> ("meta", metaToJSON m)) (hsRespMeta resp)
        ]


-- | Decode a handshake response from JSON.
handshakeResponseFromJSON :: Aeson.Value -> Either String HandshakeResponse
handshakeResponseFromJSON (Aeson.Object obj) = do
  matchText <- requireString "match" obj
  match <- handshakeMatchFromText matchText
  let serverProto = optString "serverProtocol" obj
  let serverHash = case KM.lookup "serverHash" obj of
        Just v -> case bytesFromBase64JSON v of
          Right bs -> Just bs
          Left _ -> Nothing
        Nothing -> Nothing
  let meta = case KM.lookup "meta" obj of
        Just v -> case metaFromJSON v of
          Right m -> Just m
          Left _ -> Nothing
        Nothing -> Nothing
  Right
    HandshakeResponse
      { hsRespMatch = match
      , hsRespServerProtocol = serverProto
      , hsRespServerHash = serverHash
      , hsRespMeta = meta
      }
handshakeResponseFromJSON _ = Left "handshake response must be a JSON object"


--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

requireString :: Text -> KM.KeyMap Aeson.Value -> Either String Text
requireString k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.String s) -> Right s
  _ -> Left $ "missing or non-string field: " ++ T.unpack k


optString :: Text -> KM.KeyMap Aeson.Value -> Maybe Text
optString k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.String s) -> Just s
  _ -> Nothing


bytesToBase64JSON :: ByteString -> Aeson.Value
bytesToBase64JSON bs = Aeson.String $ T.pack $ concatMap byteToHex (BS.unpack bs)
  where
    byteToHex b = [hexDigit (b `div` 16), hexDigit (b `mod` 16)]
    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)


bytesFromBase64JSON :: Aeson.Value -> Either String ByteString
bytesFromBase64JSON (Aeson.String s) = hexDecode s
bytesFromBase64JSON _ = Left "expected string for bytes"


hexDecode :: Text -> Either String ByteString
hexDecode t = go (T.unpack t) []
  where
    go [] acc = Right (BS.pack (reverse acc))
    go [_] _ = Left "hex string has odd length"
    go (c1 : c2 : rest) acc = do
      h <- hexVal c1
      l <- hexVal c2
      go rest (h * 16 + l : acc)
    hexVal c
      | c >= '0' && c <= '9' = Right (fromIntegral (fromEnum c - fromEnum '0'))
      | c >= 'a' && c <= 'f' = Right (fromIntegral (fromEnum c - fromEnum 'a' + 10))
      | c >= 'A' && c <= 'F' = Right (fromIntegral (fromEnum c - fromEnum 'A' + 10))
      | otherwise = Left $ "invalid hex character: " ++ [c]


requireBytes :: Text -> KM.KeyMap Aeson.Value -> Either String ByteString
requireBytes k obj = case KM.lookup (Key.fromText k) obj of
  Just v -> bytesFromBase64JSON v
  Nothing -> Left $ "missing field: " ++ T.unpack k


metaToJSON :: [(Text, ByteString)] -> Aeson.Value
metaToJSON entries =
  Aeson.Object $
    KM.fromList
      [ (Key.fromText k, bytesToBase64JSON v)
      | (k, v) <- entries
      ]


metaFromJSON :: Aeson.Value -> Either String [(Text, ByteString)]
metaFromJSON (Aeson.Object obj) =
  mapM
    ( \(k, v) -> do
        bs <- bytesFromBase64JSON v
        Right (Key.toText k, bs)
    )
    (KM.toList obj)
metaFromJSON _ = Left "meta must be a JSON object"
