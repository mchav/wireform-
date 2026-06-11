{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the error-accumulating decoder ('Proto.Decode.Collect').
module Test.Collect (collectTests) where

import Data.ByteString qualified as BS
import Data.Word (Word8)
import Proto.Decode (DecodeError (..))
import Proto.Decode.Collect (DecodeIssue (..), decodeCollecting)
import Proto.Google.Protobuf.Wrappers (StringValue (..))
import Test.Syd


-- Encode a length-delimited field (wire type 2).
lenField :: Int -> [Word8] -> [Word8]
lenField fn bs = [fromIntegral (fn * 8 + 2), fromIntegral (length bs)] ++ bs


collectStringValue :: BS.ByteString -> ([DecodeIssue], Maybe StringValue)
collectStringValue = decodeCollecting


collectTests :: Spec
collectTests =
  describe
    "Proto.Decode.Collect"
    $ sequence_
      [ it "clean message: no issues, value decoded" $
          collectStringValue (BS.pack (lenField 1 [0x6F, 0x6B]))
            `shouldBe` ([], Just (StringValue "ok" []))
      , it "invalid UTF-8 string field is reported with its path" $
          let (issues, val) = collectStringValue (BS.pack (lenField 1 [0xFF]))
          in do
               val `shouldBe` Nothing
               issues `shouldBe` [DecodeIssue ["value"] InvalidUtf8]
      , it "truncated top-level field is reported structurally" $
          let (issues, val) = collectStringValue (BS.pack [0x0A, 0x05, 0x61]) -- len=5 but 1 byte
          in do
               val `shouldBe` Nothing
               map issuePath issues `shouldBe` [[]]
      ]
