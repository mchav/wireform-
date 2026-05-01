{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Proto.Google.Protobuf.Any.Util
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Empty
import Proto.Message (IsMessage(..))

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

  let ts = defaultTimestamp { timestampSeconds = 1708000000, timestampNanos = 123456789 }
  let anyTs = packAny ts
  putStrLn "--- Packing a Timestamp ---"
  putStrLn $ "Timestamp:   " <> show ts
  putStrLn $ "Any typeUrl: " <> show (anyTypeUrl anyTs)
  putStrLn $ "Any value:   " <> show (BS.length (anyValue anyTs)) <> " bytes"

  putStrLn "\n--- Type-safe unpack ---"
  case unpackAny anyTs of
    Just (Right (decoded :: Timestamp)) ->
      putStrLn $ "Unpacked:    " <> show decoded <> " (match: " <> show (decoded == ts) <> ")"
    Just (Left err) ->
      putStrLn $ "Decode error: " <> show err
    Nothing ->
      putStrLn "Type mismatch!"

  putStrLn "\n--- Dynamic dispatch (TypeRegistry) ---"
  let registry = registerType (Proxy :: Proxy Timestamp)
               . registerType (Proxy :: Proxy Duration)
               . registerType (Proxy :: Proxy Empty)
               $ emptyRegistry

  let messages =
        [ packAny ts
        , packAny (defaultDuration { durationSeconds = 60 })
        , packAny defaultEmpty
        ]

  mapM_ (\a -> do
    putStr $ "  " <> show (anyTypeUrl a) <> " -> "
    case unpackAnyDynamic registry a of
      Just (Right (DynamicMessage msg)) -> putStrLn (show msg)
      Just (Left err) -> putStrLn $ "error: " <> show err
      Nothing -> putStrLn "unknown type"
    ) messages

  putStrLn "\nDone."
