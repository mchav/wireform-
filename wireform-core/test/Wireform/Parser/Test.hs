{-# LANGUAGE OverloadedStrings #-}

module Wireform.Parser.Test (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Test.Hspec

import Wireform.Parser
import Wireform.Parser.Driver (parseByteString)

spec :: Spec
spec = describe "Parser" $ do
  describe "parseByteString (non-streaming)" $ do
    it "parses a single byte" $ do
      parseByteString anyWord8 "\x42" `shouldBe` Right 0x42

    it "fails on empty input" $ do
      case parseByteString anyWord8 "" of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed"

    it "parses a 32-bit big-endian word" $ do
      parseByteString anyWord32be "\x00\x00\x01\x00"
        `shouldBe` Right 256

    it "parses a 32-bit little-endian word" $ do
      parseByteString anyWord32le "\x00\x01\x00\x00"
        `shouldBe` Right 256

    it "parses a 64-bit word" $ do
      parseByteString anyWord64le "\x01\x00\x00\x00\x00\x00\x00\x00"
        `shouldBe` Right 1

    it "sequences two byte reads" $ do
      let p = (,) <$> anyWord8 <*> anyWord8
      parseByteString p "\xAA\xBB" `shouldBe` Right (0xAA, 0xBB)

    it "fails on insufficient input for sequence" $ do
      let p = (,) <$> anyWord8 <*> anyWord8
      case parseByteString p "\xAA" of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed"

  describe "alternatives" $ do
    it "tries second branch on failure" $ do
      let p = (word8 0x01 *> pure "a") <|> (word8 0x02 *> pure "b")
      parseByteString p "\x02" `shouldBe` Right ("b" :: String)

    it "first branch succeeds" $ do
      let p = (word8 0x01 *> pure "a") <|> (word8 0x02 *> pure "b")
      parseByteString p "\x01" `shouldBe` Right ("a" :: String)

    it "both fail" $ do
      let p = (word8 0x01 *> pure "a") <|> (word8 0x02 *> pure "b")
      case parseByteString p "\x03" of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed"

  describe "cut/err" $ do
    it "cut converts Fail to Err" $ do
      let p = cut (word8 0x01 *> anyWord8 *> word8 0xFF) "expected 0xFF"
      case parseByteString p "\x01\x42\xAA" of
        Left (ParseErr _ e) -> e `shouldBe` "expected 0xFF"
        other -> expectationFailure ("unexpected: " <> show other)

    it "err produces unrecoverable error" $ do
      let p = err ("fatal" :: String)
      case parseByteString p "anything" of
        Left (ParseErr _ e) -> e `shouldBe` "fatal"
        other -> expectationFailure ("unexpected: " <> show other)

  describe "bytes matching" $ do
    it "matches exact bytes" $ do
      parseByteString (bytes "hello") "hello" `shouldBe` Right ()

    it "fails on mismatch" $ do
      case parseByteString (bytes "hello") "hxllo" of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed"

  describe "takeBs" $ do
    it "takes n bytes" $ do
      parseByteString (takeBs 3) "abcde" `shouldBe` Right "abc"

    it "fails if not enough bytes" $ do
      case parseByteString (takeBs 10) "abc" of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed"

  describe "skip" $ do
    it "skips n bytes and continues" $ do
      let p = skip 2 *> anyWord8
      parseByteString p "\x00\x00\x42" `shouldBe` Right 0x42

  describe "eof" $ do
    it "succeeds at end of input" $ do
      parseByteString eof "" `shouldBe` Right ()

    it "fails when input remains" $ do
      case parseByteString eof "x" of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed"

  describe "UTF-8 characters" $ do
    it "parses ASCII char" $ do
      parseByteString anyCharASCII "A" `shouldBe` Right 'A'

    it "fails on non-ASCII for anyCharASCII" $ do
      case parseByteString anyCharASCII "\xC3\xA9" of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed"

    it "parses multi-byte UTF-8" $ do
      parseByteString anyChar "\xC3\xA9" `shouldBe` Right '\x00E9'

    it "parses 3-byte UTF-8" $ do
      parseByteString anyChar "\xE2\x82\xAC" `shouldBe` Right '\x20AC'

    it "parses 4-byte UTF-8" $ do
      parseByteString anyChar "\xF0\x9F\x98\x80" `shouldBe` Right '\x1F600'

  describe "ASCII decimal" $ do
    it "parses a number" $ do
      parseByteString anyAsciiDecimalWord "12345x" `shouldBe` Right 12345

    it "fails on non-digit" $ do
      case parseByteString anyAsciiDecimalWord "abc" of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed"

  describe "lookahead and negative lookahead" $ do
    it "lookahead does not consume" $ do
      let p = lookahead anyWord8 *> anyWord8
      parseByteString p "\x42" `shouldBe` Right 0x42

    it "fails succeeds when inner fails" $ do
      let p = fails (word8 0x01)
      parseByteString p "\x02" `shouldBe` Right ()

  describe "many_/some_" $ do
    it "many_ accumulates" $ do
      let p = many_ (word8 0x41) *> anyWord8
      parseByteString p "\x41\x41\x41\x42" `shouldBe` Right 0x42

    it "many_ on empty succeeds" $ do
      let p = many_ (word8 0x41) *> eof
      parseByteString p "" `shouldBe` Right ()
