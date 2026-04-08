{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.ByteString as BS
import Thrift.Class (ToThrift, FromThrift, encodeThriftBinary, decodeThriftBinary,
                     encodeThriftCompact, decodeThriftCompact)

data LogEntry = LogEntry
  { level   :: !Text
  , message :: !Text
  , code    :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToThrift, FromThrift)

main :: IO ()
main = do
  let entry = LogEntry "ERROR" "disk full" 507

  let binBytes = encodeThriftBinary entry
  putStrLn $ "Binary: " ++ show (BS.length binBytes) ++ " bytes"

  case decodeThriftBinary binBytes of
    Right decoded -> putStrLn $ "Binary decoded: " ++ show (decoded :: LogEntry)
    Left err      -> putStrLn $ "Binary error: " ++ err

  let compBytes = encodeThriftCompact entry
  putStrLn $ "Compact: " ++ show (BS.length compBytes) ++ " bytes"

  case decodeThriftCompact compBytes of
    Right decoded -> putStrLn $ "Compact decoded: " ++ show (decoded :: LogEntry)
    Left err      -> putStrLn $ "Compact error: " ++ err

  putStrLn $ "Roundtrip (binary): " ++ show (decodeThriftBinary (encodeThriftBinary entry) == Right entry)
  putStrLn $ "Roundtrip (compact): " ++ show (decodeThriftCompact (encodeThriftCompact entry) == Right entry)
