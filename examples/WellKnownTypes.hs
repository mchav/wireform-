{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Proto.Decode
import Proto.Encode
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Empty
import Proto.Google.Protobuf.FieldMask
import Proto.Google.Protobuf.Struct
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Wrappers


main :: IO ()
main = do
  putStrLn "=== Well-Known Types Example ===\n"

  putStrLn "--- Timestamp ---"
  roundtrip "Timestamp" (defaultTimestamp {timestampSeconds = 1708000000, timestampNanos = 500000000})

  putStrLn "--- Duration ---"
  roundtrip "Duration" (defaultDuration {durationSeconds = 3600})

  putStrLn "--- Empty ---"
  roundtrip "Empty" defaultEmpty

  putStrLn "--- Wrappers ---"
  roundtrip "Int64Value(42)" (defaultInt64Value {int64ValueValue = 42})
  roundtrip "BoolValue(True)" (defaultBoolValue {boolValueValue = True})
  roundtrip "StringValue" (defaultStringValue {stringValueValue = "hello, protobuf!"})
  roundtrip "DoubleValue" (defaultDoubleValue {doubleValueValue = 3.14159265358979})

  putStrLn "--- FieldMask ---"
  roundtrip "FieldMask" (defaultFieldMask {fieldMaskPaths = V.fromList ["user.name", "user.email"]})

  putStrLn "--- Struct ---"
  let struct =
        defaultStruct
          { structFields =
              Map.fromList
                [ ("name", defaultValue {valueKind = Just (Value'Kind'StringValue "Alice")})
                , ("age", defaultValue {valueKind = Just (Value'Kind'NumberValue 30)})
                , ("active", defaultValue {valueKind = Just (Value'Kind'BoolValue True)})
                ]
          }
  let structBytes = encodeMessage struct
  putStrLn $ "Struct encoded: " <> show (BS.length structBytes) <> " bytes"
  case decodeMessage structBytes of
    Left err -> putStrLn $ "ERROR: " <> show err
    Right decoded -> putStrLn $ "Fields: " <> show (Map.keys (structFields decoded))

  putStrLn "\nDone."


roundtrip :: (MessageEncode a, MessageDecode a, Eq a) => String -> a -> IO ()
roundtrip label msg = do
  let encoded = encodeMessage msg
      decoded = decodeMessage encoded
  case decoded of
    Left err -> putStrLn $ "  " <> label <> ": DECODE FAILED: " <> show err
    Right d -> putStrLn $ "  " <> label <> ": " <> show (BS.length encoded) <> " bytes, roundtrip=" <> show (d == msg)
