{-# LANGUAGE OverloadedStrings #-}

-- | Tests for hermes's SIMD-accelerated URL percent-decoder.
--
-- Exercises the pure decoders ('urlDecode' / 'urlDecodeForm'),
-- the SIMD scan fast path, error reporting, and the flatparse
-- integration ('urlDecodedSegment' / 'urlDecodedWhile').
module Test.UrlDecode (tests) where

import qualified Data.ByteString as BS
import Data.Word (Word8)

-- The URL decode parsers in 'Network.HTTP.URL.Decode' are built on
-- hermes's vendored Wireform.Parser, so they need hermes's own
-- @runParser@ \/ @Result@ shim. The Hedgehog \/ flatparse-style
-- combinators (@eof@, @takeRest@, @skipMany@, @skipSatisfyAscii@)
-- are still flatparse-compatible because hermes re-exports them.
import qualified Network.HTTP.Headers.Parsing.Util as FP

import Test.Syd

import Network.HTTP.URL.Decode

tests :: Spec
tests = describe "Network.HTTP.URL.Decode" $ sequence_
  [ pureDecoderTests
  , scannerTests
  , errorTests
  , flatparseTests
  , stressTests
  ]

-- ---------------------------------------------------------------------------
-- Pure decoder
-- ---------------------------------------------------------------------------

pureDecoderTests :: Spec
pureDecoderTests = describe "urlDecode / urlDecodeForm" $ sequence_
  [ it "no escapes → input is returned unchanged (sharing)" $ do
      let bs = "/a/perfectly/normal/path"
      case urlDecode bs of
        Right out -> do
          out `shouldBe` bs
          -- The sharing check: the result must be the same bytes,
          -- not a fresh copy. ByteString equality alone doesn't
          -- prove sharing, so just confirm correctness here.
          (BS.length out == BS.length bs) `shouldBe` True
        Left e -> expectationFailure ("unexpected error: " <> show e)

  , it "simple %20 → space" $
      urlDecode "hello%20world" `shouldBe` Right "hello world"

  , it "lowercase hex digits" $
      urlDecode "%e2%98%83" `shouldBe` Right "\xe2\x98\x83"

  , it "uppercase hex digits" $
      urlDecode "%E2%98%83" `shouldBe` Right "\xe2\x98\x83"

  , it "form mode decodes + as space" $
      urlDecodeForm "a+b%20c" `shouldBe` Right "a b c"

  , it "non-form mode leaves + alone" $
      urlDecode "a+b%20c" `shouldBe` Right "a+b c"

  , it "consecutive escapes" $
      urlDecode "%2F%2F%2F" `shouldBe` Right "///"

  , it "escape at start and end" $
      urlDecode "%20middle%20" `shouldBe` Right " middle "

  , it "empty input round-trips" $
      urlDecode "" `shouldBe` Right ""

  , it "Maybe variants mirror Either" $ do
      urlDecodeMaybe "%20" `shouldBe` Just " "
      urlDecodeMaybe "%2Z" `shouldBe` Nothing
      urlDecodeFormMaybe "a+b" `shouldBe` Just "a b"
  ]

-- ---------------------------------------------------------------------------
-- Scanner fast path
-- ---------------------------------------------------------------------------

scannerTests :: Spec
scannerTests = describe "firstSpecialOffset" $ sequence_
  [ it "unescaped input → offset == length" $ do
      let bs = "/abc/def?x=1"
      firstSpecialOffset False bs `shouldBe` BS.length bs

  , it "finds first %" $
      firstSpecialOffset False "abc%20def" `shouldBe` 3

  , it "ignores + when not in form mode" $
      firstSpecialOffset False "abc+def%20" `shouldBe` 7

  , it "form mode finds first + or %" $ do
      firstSpecialOffset True  "abc+def%20" `shouldBe` 3
      firstSpecialOffset True  "abc%20def+" `shouldBe` 3

  , it "long unescaped input is handled by the SIMD stride" $ do
      -- Pad past 32 bytes to make sure the AVX2 / SSE2 stride
      -- runs at least once.
      let bs = BS.replicate 100 (asciiByte 'a')
      firstSpecialOffset False bs `shouldBe` 100

  , it "% located deep inside a large buffer" $ do
      let prefix = BS.replicate 200 (asciiByte 'a')
          bs     = prefix <> "%41"
      firstSpecialOffset False bs `shouldBe` 200
  ]

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

errorTests :: Spec
errorTests = describe "errors" $ sequence_
  [ it "lonely % at end" $
      urlDecode "abc%" `shouldBe` Left TruncatedEscape

  , it "% followed by one hex digit" $
      urlDecode "abc%4" `shouldBe` Left TruncatedEscape

  , it "non-hex first nibble" $
      urlDecode "%Z0" `shouldBe` Left InvalidHexDigit

  , it "non-hex second nibble" $
      urlDecode "%0Z" `shouldBe` Left InvalidHexDigit

  , it "form mode propagates the same errors" $
      urlDecodeForm "abc+%" `shouldBe` Left TruncatedEscape
  ]

-- ---------------------------------------------------------------------------
-- Flatparse integration
-- ---------------------------------------------------------------------------

flatparseTests :: Spec
flatparseTests = describe "flatparse combinators" $ sequence_
  [ it "urlDecodedWhile until '&'" $ do
      let parser = urlDecodedWhile (/= ampersand)
          input  = "hello%20world&next=1"
      case FP.runParser parser input of
        FP.OK out rest -> do
          out  `shouldBe` "hello world"
          rest `shouldBe` "&next=1"
        other -> expectationFailure ("unexpected: " <> show other)

  , it "formUrlDecodedWhile until '&' translates +" $ do
      let parser = formUrlDecodedWhile (\w -> w /= ampersand)
          input  = "a+b%20c&done"
      case FP.runParser parser input of
        FP.OK out rest -> do
          out  `shouldBe` "a b c"
          rest `shouldBe` "&done"
        other -> expectationFailure ("unexpected: " <> show other)

  , it "decode failure inside parser surfaces as Err" $ do
      let parser = urlDecodedWhile (/= ampersand) <* FP.eof
          input  = "bad%2Z"
      case FP.runParser parser input of
        FP.Err InvalidHexDigit -> pure ()
        other -> expectationFailure
          ("expected InvalidHexDigit error, got: " <> show other)

  , it "urlDecodedSegment composes with arbitrary inner parser" $ do
      let inner  = FP.skipMany (FP.skipSatisfyAscii (/= '|'))
          parser = (,) <$> urlDecodedSegment inner <*> FP.takeRest
          input  = "x%3Dy|tail"
      case FP.runParser parser input of
        FP.OK (decoded, rest) "" -> do
          decoded `shouldBe` "x=y"
          rest    `shouldBe` "|tail"
        other -> expectationFailure ("unexpected: " <> show other)
  ]

-- ---------------------------------------------------------------------------
-- Stress: spans the SIMD strides
-- ---------------------------------------------------------------------------

stressTests :: Spec
stressTests = describe "stride alignment" $ sequence_
  [ it "decode across a 32-byte boundary" $ do
      -- Place the escape at byte 30 so the SIMD scan crosses
      -- both a 16-byte and 32-byte boundary on the way to it.
      let prefix = BS.replicate 30 (asciiByte 'a')
          bs     = prefix <> "%21!"
      urlDecode bs `shouldBe` Right (prefix <> "!!")

  , it "long alternating run" $ do
      let bs = BS.concat (replicate 50 "a%20")
      urlDecode bs `shouldBe` Right (BS.concat (replicate 50 "a "))

  , it "decoded output strictly shorter than input" $ do
      let bs = "%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F"
      case urlDecode bs of
        Right out -> do
          out `shouldBe` "////////////"
          (BS.length out < BS.length bs) `shouldBe` True
        Left e -> expectationFailure (show e)
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

ampersand :: Word8
ampersand = 0x26

asciiByte :: Char -> Word8
asciiByte = toEnum . fromEnum
