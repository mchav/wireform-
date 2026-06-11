{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString qualified as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import Thrift.Class (
  FromThrift,
  ToThrift,
  decodeThriftBinary,
  decodeThriftCompact,
  encodeThriftBinary,
  encodeThriftCompact,
 )


data LogEntry = LogEntry
  { level :: !Text
  , message :: !Text
  , code :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToThrift, FromThrift)


main :: IO ()
main = do
  let entry = LogEntry "ERROR" "disk full" 507

  let binBytes = encodeThriftBinary entry
  putStrLn $ "Binary: " ++ show (BS.length binBytes) ++ " bytes"

  case decodeThriftBinary binBytes of
    Right decoded -> putStrLn $ "Binary decoded: " ++ show (decoded :: LogEntry)
    Left err -> putStrLn $ "Binary error: " ++ err

  let compBytes = encodeThriftCompact entry
  putStrLn $ "Compact: " ++ show (BS.length compBytes) ++ " bytes"

  case decodeThriftCompact compBytes of
    Right decoded -> putStrLn $ "Compact decoded: " ++ show (decoded :: LogEntry)
    Left err -> putStrLn $ "Compact error: " ++ err

  putStrLn $ "Roundtrip (binary): " ++ show (decodeThriftBinary (encodeThriftBinary entry) == Right entry)
  putStrLn $ "Roundtrip (compact): " ++ show (decodeThriftCompact (encodeThriftCompact entry) == Right entry)

-- ---------------------------------------------------------------------------
-- Alternative approaches
-- ---------------------------------------------------------------------------

-- Approach 1: Generic deriving (as shown above)

-- Approach 2: TH from .thrift IDL
--   [thrift|
--     struct Person {
--       1: required string name
--       2: optional i32 age
--     }
--   |]

-- Approach 3: CLI codegen
--   wireform-gen thrift -i service.thrift -o src/Gen/
