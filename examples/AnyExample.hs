{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Example: google.protobuf.Any pack/unpack.
--
-- Demonstrates:
-- * Packing arbitrary messages into Any
-- * Type-safe unpacking with compile-time type checking
-- * Dynamic unpacking with a TypeRegistry for runtime dispatch
-- * Any inside other messages
-- * Handling of mismatched type URLs
--
-- Run with: cabal run example-any
module Main where

import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldVarintSize, fieldTextSize)
import Proto.Google.Protobuf.Any
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Empty

-- A custom message type that contains an Any field.
data Event = Event
  { eventId      :: {-# UNPACK #-} !Word64
  , eventType    :: !Text
  , eventPayload :: !(Maybe Any)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

instance MessageEncode Event where
  buildMessage (Event eid etype payload) =
    (if eid == 0 then mempty else encodeFieldVarint 1 eid) <>
    (if etype == "" then mempty else encodeFieldString 2 etype) <>
    maybe mempty (encodeFieldMessage 3) payload

instance MessageSize Event where
  messageSize (Event eid etype payload) =
    (if eid == 0 then 0 else fieldVarintSize 1 eid) +
    (if etype == "" then 0 else fieldTextSize 2 etype) +
    maybe 0 (\p -> let sz = messageSize p
                   in fieldVarintSize 3 (fromIntegral sz) + sz) payload

instance MessageDecode Event where
  messageDecoder = loop 0 "" Nothing
    where
      loop !eid !etype !payload = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Event eid etype payload)
          Just (Tag fn wt) -> case fn of
            1 -> decodeFieldVarint >>= \v -> loop v etype payload
            2 -> decodeFieldString >>= \v -> loop eid v payload
            3 -> decodeFieldMessage >>= \v -> loop eid etype (Just v)
            _ -> skipField wt >> loop eid etype payload

instance IsMessage Event where
  messageTypeName _ = "example.Event"

main :: IO ()
main = do
  putStrLn "=== google.protobuf.Any Example ===\n"

  -- 1. Pack a Timestamp into an Any
  let ts = Timestamp 1708000000 123456789
  let anyTs = packAny ts
  putStrLn "--- Packing a Timestamp ---"
  putStrLn $ "Timestamp:  " <> show ts
  putStrLn $ "Any typeUrl: " <> show (typeUrl anyTs)
  putStrLn $ "Any value:   " <> show (BS.length (value anyTs)) <> " bytes"

  -- 2. Unpack it back (type-safe, compile-time checked)
  putStrLn "\n--- Type-safe unpack ---"
  case unpackAny anyTs of
    Just (Right (decoded :: Timestamp)) ->
      putStrLn $ "Unpacked:   " <> show decoded <> " (match: " <> show (decoded == ts) <> ")"
    Just (Left err) ->
      putStrLn $ "Decode error: " <> show err
    Nothing ->
      putStrLn "Type mismatch!"

  -- 3. Wrong type unpack returns Nothing
  putStrLn "\n--- Wrong type unpack ---"
  case (unpackAny anyTs :: Maybe (Either DecodeError Duration)) of
    Nothing -> putStrLn "Correctly rejected: Timestamp Any cannot unpack as Duration"
    Just _  -> putStrLn "BUG: should not match"

  -- 4. isMessageType check
  putStrLn "\n--- Type checking ---"
  putStrLn $ "Is Timestamp? " <> show (isMessageType (Proxy :: Proxy Timestamp) anyTs)
  putStrLn $ "Is Duration?  " <> show (isMessageType (Proxy :: Proxy Duration) anyTs)

  -- 5. Any inside another message
  putStrLn "\n--- Any inside Event ---"
  let event = Event 42 "user.login" (Just (packAny ts))
  let eventBytes = encodeMessage event
  putStrLn $ "Event encoded: " <> show (BS.length eventBytes) <> " bytes"

  case decodeMessage eventBytes of
    Left err -> putStrLn $ "Decode error: " <> show err
    Right (decoded :: Event) -> do
      putStrLn $ "Event id:    " <> show (eventId decoded)
      putStrLn $ "Event type:  " <> show (eventType decoded)
      case eventPayload decoded of
        Nothing -> putStrLn "No payload"
        Just anyPayload -> do
          putStrLn $ "Payload URL: " <> show (typeUrl anyPayload)
          case unpackAny anyPayload of
            Just (Right (innerTs :: Timestamp)) ->
              putStrLn $ "Payload:     " <> show innerTs
            _ -> putStrLn "Could not unpack payload"

  -- 6. Dynamic dispatch with TypeRegistry
  putStrLn "\n--- Dynamic dispatch (TypeRegistry) ---"
  let registry = registerType (Proxy :: Proxy Timestamp)
               . registerType (Proxy :: Proxy Duration)
               . registerType (Proxy :: Proxy Empty)
               $ emptyRegistry

  let messages =
        [ packAny (Timestamp 1000 0)
        , packAny (Duration 60 0)
        , packAny Empty
        ]

  mapM_ (\a -> do
    putStr $ "  " <> show (typeUrl a) <> " -> "
    case unpackAnyDynamic registry a of
      Just (Right (DynamicMessage msg)) -> putStrLn (show msg)
      Just (Left err) -> putStrLn $ "error: " <> show err
      Nothing -> putStrLn "unknown type"
    ) messages

  -- 7. Pack with custom prefix
  putStrLn "\n--- Custom URL prefix ---"
  let customAny = packAnyWithPrefix "mycompany.com/types/" ts
  putStrLn $ "Custom URL: " <> show (typeUrl customAny)

  putStrLn "\nDone."
