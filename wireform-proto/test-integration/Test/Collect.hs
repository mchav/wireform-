{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the error-accumulating decoder ('Proto.Decode.Collect').
module Test.Collect (collectTests) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Tasty
import Test.Tasty.HUnit

import Proto.Decode (DecodeError (..))
import Proto.Decode.Collect (DecodeIssue (..), decodeCollecting)
import Proto.Google.Protobuf.Wrappers (StringValue (..))

-- Encode a length-delimited field (wire type 2).
lenField :: Int -> [Word8] -> [Word8]
lenField fn bs = [fromIntegral (fn * 8 + 2), fromIntegral (length bs)] ++ bs

collectStringValue :: BS.ByteString -> ([DecodeIssue], Maybe StringValue)
collectStringValue = decodeCollecting

collectTests :: TestTree
collectTests =
  testGroup
    "Proto.Decode.Collect"
    [ testCase "clean message: no issues, value decoded" $
        collectStringValue (BS.pack (lenField 1 [0x6F, 0x6B]))
          @?= ([], Just (StringValue "ok" []))
    , testCase "invalid UTF-8 string field is reported with its path" $
        let (issues, val) = collectStringValue (BS.pack (lenField 1 [0xFF]))
         in do
              val @?= Nothing
              issues @?= [DecodeIssue ["value"] InvalidUtf8]
    , testCase "truncated top-level field is reported structurally" $
        let (issues, val) = collectStringValue (BS.pack [0x0A, 0x05, 0x61]) -- len=5 but 1 byte
         in do
              val @?= Nothing
              map issuePath issues @?= [[]]
    ]
