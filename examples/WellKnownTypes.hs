{-# LANGUAGE OverloadedStrings #-}
-- | Example: working with well-known protobuf types.
--
-- Demonstrates Timestamp, Duration, Struct, Wrappers, and their
-- encode/decode roundtrips.
--
-- Run with: cabal run example-wellknown
module Main where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Proto.Encode
import Proto.Decode
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Empty
import Proto.Google.Protobuf.Wrappers
import Proto.Google.Protobuf.Struct
import Proto.Google.Protobuf.FieldMask

main :: IO ()
main = do
  putStrLn "=== Well-Known Types Example ===\n"

  -- Timestamp
  putStrLn "--- Timestamp ---"
  let ts = Timestamp 1708000000 500000000
  roundtrip "Timestamp" ts

  -- Duration
  putStrLn "--- Duration ---"
  let dur = Duration 3600 0
  roundtrip "Duration" dur

  -- Empty
  putStrLn "--- Empty ---"
  let emp = Empty
  let empBytes = encodeMessage emp
  putStrLn $ "Empty encodes to " <> show (BS.length empBytes) <> " bytes"
  roundtrip "Empty" emp

  -- Wrapper types (useful for distinguishing "field not set" from "field is zero")
  putStrLn "--- Wrappers ---"
  roundtrip "Int64Value(42)" (Int64Value 42)
  roundtrip "Int64Value(0)"  (Int64Value 0)
  roundtrip "BoolValue(True)" (BoolValue True)
  roundtrip "StringValue" (StringValue "hello, protobuf!")
  roundtrip "BytesValue" (BytesValue "\x00\x01\x02\xff")
  roundtrip "DoubleValue" (DoubleValue 3.14159265358979)

  -- FieldMask
  putStrLn "--- FieldMask ---"
  let fm = FieldMask (V.fromList ["user.name", "user.email", "user.settings.theme"])
  roundtrip "FieldMask" fm

  -- Struct (JSON-like dynamic values)
  putStrLn "--- Struct ---"
  let struct = Struct $ Map.fromList
        [ ("name", Value (Just (StringKind "Alice")))
        , ("age", Value (Just (NumberKind 30)))
        , ("active", Value (Just (BoolKind True)))
        , ("tags", Value (Just (ListKind (ListValue (V.fromList
            [ Value (Just (StringKind "admin"))
            , Value (Just (StringKind "user"))
            ])))))
        , ("metadata", Value (Just (StructKind (Struct (Map.fromList
            [ ("created", Value (Just (NumberKind 1708000000)))
            ])))))
        , ("deleted_at", Value (Just (NullKind NullValueNull)))
        ]

  let structBytes = encodeMessage struct
  putStrLn $ "Struct encoded: " <> show (BS.length structBytes) <> " bytes"
  case decodeMessage structBytes of
    Left err -> putStrLn $ "ERROR: " <> show err
    Right decoded -> do
      putStrLn $ "Fields: " <> show (Map.keys (structFields decoded))
      putStrLn $ "Match: " <> show (decoded == struct)

  putStrLn "\nDone."

roundtrip :: (MessageEncode a, MessageDecode a, Eq a) => String -> a -> IO ()
roundtrip label msg = do
  let encoded = encodeMessage msg
      decoded = decodeMessage encoded
  case decoded of
    Left err -> putStrLn $ "  " <> label <> ": ENCODE OK (" <> show (BS.length encoded) <> " bytes), DECODE FAILED: " <> show err
    Right d  -> putStrLn $ "  " <> label <> ": " <> show (BS.length encoded) <> " bytes, roundtrip=" <> show (d == msg)
