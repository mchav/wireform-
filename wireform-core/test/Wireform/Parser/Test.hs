{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Wireform.Parser.Test (spec) where

import Data.ByteString (ByteString)
import Data.Word
import Test.Hspec

import Wireform.Parser
import Wireform.Parser.Driver (parseByteString)
import Wireform.Parser.Error

type P = Parser String

ok :: (Show a, Eq a) => Either (ParseError String) a -> a -> Expectation
ok (Right a) expected = a `shouldBe` expected
ok (Left e) _         = expectationFailure ("parse failed: " <> show e)

bad :: (Show a) => Either (ParseError String) a -> Expectation
bad (Left _)  = pure ()
bad (Right a) = expectationFailure ("expected failure, got: " <> show a)

spec :: Spec
spec = describe "Parser" $ do
  describe "parseByteString (non-streaming)" $ do
    it "parses a single byte" $
      ok (parseByteString (anyWord8 :: P Word8) "\x42") 0x42

    it "fails on empty input" $
      bad (parseByteString (anyWord8 :: P Word8) "")

    it "parses a 32-bit big-endian word" $
      ok (parseByteString (anyWord32be :: P Word32) "\x00\x00\x01\x00") 256

    it "parses a 32-bit little-endian word" $
      ok (parseByteString (anyWord32le :: P Word32) "\x00\x01\x00\x00") 256

    it "parses a 64-bit word" $
      ok (parseByteString (anyWord64le :: P Word64) "\x01\x00\x00\x00\x00\x00\x00\x00") 1

    it "sequences two byte reads" $ do
      let p = (,) <$> anyWord8 <*> anyWord8 :: P (Word8, Word8)
      ok (parseByteString p "\xAA\xBB") (0xAA, 0xBB)

    it "fails on insufficient input for sequence" $ do
      let p = (,) <$> anyWord8 <*> anyWord8 :: P (Word8, Word8)
      bad (parseByteString p "\xAA")

  describe "alternatives" $ do
    it "tries second branch on failure" $ do
      let p = (word8 0x01 *> pure "a") <|> (word8 0x02 *> pure "b") :: P String
      ok (parseByteString p "\x02") "b"

    it "first branch succeeds" $ do
      let p = (word8 0x01 *> pure "a") <|> (word8 0x02 *> pure "b") :: P String
      ok (parseByteString p "\x01") "a"

    it "both fail" $ do
      let p = (word8 0x01 *> pure "a") <|> (word8 0x02 *> pure "b") :: P String
      bad (parseByteString p "\x03")

  describe "cut/err" $ do
    it "cut converts Fail to Err" $ do
      let p = cut (word8 0x01 *> anyWord8 *> word8 0xFF) "expected 0xFF" :: P ()
      case parseByteString p "\x01\x42\xAA" of
        Left (ParseErr _ e) -> e `shouldBe` "expected 0xFF"
        other -> expectationFailure ("unexpected: " <> show other)

    it "err produces unrecoverable error" $ do
      let p = err "fatal" :: P ()
      case parseByteString p "anything" of
        Left (ParseErr _ e) -> e `shouldBe` "fatal"
        other -> expectationFailure ("unexpected: " <> show other)

  describe "bytes matching" $ do
    it "matches exact bytes" $
      ok (parseByteString (byteString "hello" :: P ()) "hello") ()

    it "fails on mismatch" $
      bad (parseByteString (byteString "hello" :: P ()) "hxllo")

  describe "takeBs" $ do
    it "takes n bytes" $
      ok (parseByteString (takeBs 3 :: P ByteString) "abcde") "abc"

    it "fails if not enough bytes" $
      bad (parseByteString (takeBs 10 :: P ByteString) "abc")

  describe "skip" $ do
    it "skips n bytes and continues" $ do
      let p = skip 2 *> anyWord8 :: P Word8
      ok (parseByteString p "\x00\x00\x42") 0x42

  describe "eof" $ do
    it "succeeds at end of input" $
      ok (parseByteString (eof :: P ()) "") ()

    it "fails when input remains" $
      bad (parseByteString (eof :: P ()) "x")

  describe "UTF-8 characters" $ do
    it "parses ASCII char" $
      ok (parseByteString (satisfyAscii (const True) :: P Char) "A") 'A'

    it "fails on non-ASCII for satisfyAscii" $
      bad (parseByteString (satisfyAscii (const True) :: P Char) "\xC3\xA9")

    it "parses multi-byte UTF-8" $
      ok (parseByteString (anyChar :: P Char) "\xC3\xA9") '\x00E9'

    it "parses 3-byte UTF-8" $
      ok (parseByteString (anyChar :: P Char) "\xE2\x82\xAC") '\x20AC'

    it "parses 4-byte UTF-8" $
      ok (parseByteString (anyChar :: P Char) "\xF0\x9F\x98\x80") '\x1F600'

  describe "ASCII decimal" $ do
    it "parses a number" $
      ok (parseByteString (anyAsciiDecimalWord :: P Word) "12345x") 12345

    it "fails on non-digit" $
      bad (parseByteString (anyAsciiDecimalWord :: P Word) "abc")

  describe "lookahead and negative lookahead" $ do
    it "lookahead does not consume" $ do
      let p = lookahead anyWord8 *> anyWord8 :: P Word8
      ok (parseByteString p "\x42") 0x42

    it "fails succeeds when inner fails" $ do
      let p = fails (word8 0x01) :: P ()
      ok (parseByteString p "\x02") ()

  describe "many_/some_" $ do
    it "many_ accumulates" $ do
      let p = many_ (word8 0x41) *> anyWord8 :: P Word8
      ok (parseByteString p "\x41\x41\x41\x42") 0x42

    it "many_ on empty succeeds" $ do
      let p = many_ (word8 0x41) *> eof :: P ()
      ok (parseByteString p "") ()
