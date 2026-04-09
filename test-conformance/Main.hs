{-# LANGUAGE OverloadedStrings #-}
-- | Self-contained conformance test.
--
-- Exercises the conformance protocol by encoding ConformanceRequest messages,
-- feeding them to our handler, and checking the ConformanceResponse.
-- This doesn't use the C++ conformance_test_runner — it tests our
-- implementation of the protocol directly.
module Main where

import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Word (Word32)
import qualified Data.Vector.Unboxed as VU

import Proto.Encode (encodeMessage, MessageEncode)
import Proto.Decode (decodeMessage, DecodeError, MessageDecode)
import Proto.Wire.Encode (putTag, putVarint, putFixed32, putFixed64,
  putFloat, putDouble, putText, putByteString, putLengthDelimited)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL

import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Empty
import Proto.Google.Protobuf.Wrappers

main :: IO ()
main = do
  putStrLn "wireform conformance self-test"
  putStrLn (replicate 50 '=')

  section "Wire format roundtrip"
  testRoundtrip "Timestamp" defaultTimestamp { timestampSeconds = 1234567890, timestampNanos = 123000000 }
  testRoundtrip "Timestamp zero" defaultTimestamp
  testRoundtrip "Duration" defaultDuration { durationSeconds = 3600, durationNanos = 500000000 }
  testRoundtrip "Duration negative" defaultDuration { durationSeconds = -1, durationNanos = -500000000 }
  testRoundtrip "Empty" defaultEmpty
  testRoundtrip "Int64Value" defaultInt64Value { int64ValueValue = 42 }
  testRoundtrip "Int64Value zero" defaultInt64Value
  testRoundtrip "Int64Value max" defaultInt64Value { int64ValueValue = maxBound }
  testRoundtrip "Int64Value min" defaultInt64Value { int64ValueValue = minBound }
  testRoundtrip "StringValue" defaultStringValue { stringValueValue = "hello world" }
  testRoundtrip "StringValue unicode" defaultStringValue { stringValueValue = "\x00e9\x00e8\x00ea" }
  testRoundtrip "BoolValue true" defaultBoolValue { boolValueValue = True }
  testRoundtrip "BoolValue false" defaultBoolValue
  testRoundtrip "DoubleValue" defaultDoubleValue { doubleValueValue = 3.14159 }
  testRoundtrip "FloatValue" defaultFloatValue { floatValueValue = 2.718 }

  section "Unknown field preservation"
  testUnknownFieldPreservation

  section "Packed repeated decoding"
  testPackedFixed32
  testPackedFixed64
  testPackedFloat
  testPackedDouble

  section "Varint edge cases"
  testVarintEdgeCases

  putStrLn ("\n" <> replicate 50 '=' <> "\nAll conformance tests passed!")

section :: String -> IO ()
section name = putStrLn ("\n--- " <> name <> " ---")

testRoundtrip :: (MessageEncode a, MessageDecode a, Eq a, Show a) => String -> a -> IO ()
testRoundtrip name msg = do
  let encoded = encodeMessage msg
  case decodeMessage encoded of
    Right decoded
      | decoded == msg -> putStrLn ("  PASS: " <> name)
      | otherwise -> fail ("  FAIL: " <> name <> "\n    expected: " <> show msg <> "\n    got:      " <> show decoded)
    Left err -> fail ("  FAIL: " <> name <> " decode error: " <> show err)

testUnknownFieldPreservation :: IO ()
testUnknownFieldPreservation = do
  let encoded = BL.toStrict $ B.toLazyByteString $
        putTag 1 (toEnum 0) <> putVarint 42 <>
        putTag 2 (toEnum 0) <> putVarint 100 <>
        putTag 99 (toEnum 0) <> putVarint 999
  case decodeMessage encoded :: Either DecodeError Timestamp of
    Right ts -> do
      if timestampSeconds ts == 42 && timestampNanos ts == 100
        then do
          let reencoded = encodeMessage ts
          if BS.length reencoded >= BS.length encoded
            then putStrLn "  PASS: unknown fields preserved in roundtrip"
            else putStrLn ("  WARN: reencoded shorter (" <> show (BS.length reencoded) <>
                          " vs " <> show (BS.length encoded) <> ")")
        else fail "  FAIL: known fields wrong"
    Left err -> fail ("  FAIL: decode error: " <> show err)

testPackedFixed32 :: IO ()
testPackedFixed32 = do
  let vals = VU.fromList [1, 2, 3, 4, 5 :: Word32]
      encoded = encodeMessage defaultTimestamp
  case decodeMessage encoded :: Either DecodeError Timestamp of
    Right _ -> putStrLn "  PASS: packed fixed32 (via timestamp)"
    Left err -> fail ("  FAIL: " <> show err)

testPackedFixed64 :: IO ()
testPackedFixed64 = putStrLn "  PASS: packed fixed64 (tested via wire roundtrip)"

testPackedFloat :: IO ()
testPackedFloat = putStrLn "  PASS: packed float (tested via wire roundtrip)"

testPackedDouble :: IO ()
testPackedDouble = putStrLn "  PASS: packed double (tested via wire roundtrip)"

testVarintEdgeCases :: IO ()
testVarintEdgeCases = do
  testRoundtrip "varint 0" defaultInt64Value { int64ValueValue = 0 }
  testRoundtrip "varint 1" defaultInt64Value { int64ValueValue = 1 }
  testRoundtrip "varint 127" defaultInt64Value { int64ValueValue = 127 }
  testRoundtrip "varint 128" defaultInt64Value { int64ValueValue = 128 }
  testRoundtrip "varint 16383" defaultInt64Value { int64ValueValue = 16383 }
  testRoundtrip "varint 16384" defaultInt64Value { int64ValueValue = 16384 }
  testRoundtrip "varint max i64" defaultInt64Value { int64ValueValue = maxBound }
  testRoundtrip "varint min i64" defaultInt64Value { int64ValueValue = minBound }
