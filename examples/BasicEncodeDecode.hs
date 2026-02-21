{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Example: basic message encoding and decoding.
--
-- Demonstrates defining a message type by hand, implementing the
-- encode/decode typeclasses, and doing a roundtrip.
--
-- Run with: cabal run example-basic
module Main where

import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Message (IsMessage(..))
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldVarintSize, fieldTextSize, fieldBoolSize)

-- Define a message type as a plain Haskell record.
-- Strict fields with UNPACK for primitives.
data Person = Person
  { personName  :: !Text
  , personAge   :: {-# UNPACK #-} !Word64
  , personEmail :: !Text
  , personActive :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

-- Implement encoding: fields skip default values per proto3 rules.
instance MessageEncode Person where
  buildMessage (Person name age email active) =
    (if name == "" then mempty else encodeFieldString 1 name) <>
    (if age == 0 then mempty else encodeFieldVarint 2 age) <>
    (if email == "" then mempty else encodeFieldString 3 email) <>
    (if not active then mempty else encodeFieldBool 4 active)

-- Implement size computation for exact-size ByteString allocation.
instance MessageSize Person where
  messageSize (Person name age email active) =
    (if name == "" then 0 else fieldTextSize 1 name) +
    (if age == 0 then 0 else fieldVarintSize 2 age) +
    (if email == "" then 0 else fieldTextSize 3 email) +
    (if not active then 0 else fieldBoolSize 4)

-- Implement decoding: CPS loop with accumulators, unknown field skipping.
instance MessageDecode Person where
  messageDecoder = loop "" 0 "" False
    where
      loop !name !age !email !active = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Person name age email active)
          Just (Tag fn wt) -> case fn of
            1 -> decodeFieldString >>= \v -> loop v age email active
            2 -> decodeFieldVarint >>= \v -> loop name v email active
            3 -> decodeFieldString >>= \v -> loop name age v active
            4 -> decodeFieldBool >>= \v -> loop name age email v
            _ -> skipField wt >> loop name age email active

-- Register type identity for Any support.
instance IsMessage Person where
  messageTypeName _ = "example.Person"

main :: IO ()
main = do
  putStrLn "=== Basic Encode/Decode Example ===\n"

  let alice = Person "Alice" 30 "alice@example.com" True
  putStrLn $ "Original: " <> show alice

  -- Encode to bytes
  let encoded = encodeMessage alice
  putStrLn $ "Encoded:  " <> show (BS.length encoded) <> " bytes"
  putStrLn $ "Hex:      " <> show encoded

  -- Exact-size encoding (single allocation, no intermediate chunks)
  let encodedSized = encodeMessageSized alice
  putStrLn $ "Sized:    " <> show (BS.length encodedSized) <> " bytes (identical: " <> show (encoded == encodedSized) <> ")"

  -- Decode back
  case decodeMessage encoded of
    Left err -> putStrLn $ "ERROR: " <> show err
    Right decoded -> do
      putStrLn $ "Decoded:  " <> show (decoded :: Person)
      putStrLn $ "Match:    " <> show (decoded == alice)

  -- Default values encode to empty
  let nobody = Person "" 0 "" False
  let nobodyBytes = encodeMessage nobody
  putStrLn $ "\nDefault person encodes to " <> show (BS.length nobodyBytes) <> " bytes"

  -- Unknown fields are silently skipped
  putStrLn "\nDone."
