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

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP.URL.Decode

tests :: TestTree
tests = testGroup "Network.HTTP.URL.Decode"
  [ pureDecoderTests
  , scannerTests
  , errorTests
  , flatparseTests
  , stressTests
  ]

-- ---------------------------------------------------------------------------
-- Pure decoder
-- ---------------------------------------------------------------------------

pureDecoderTests :: TestTree
pureDecoderTests = testGroup "urlDecode / urlDecodeForm"
  [ testCase "no escapes → input is returned unchanged (sharing)" $ do
      let bs = "/a/perfectly/normal/path"
      case urlDecode bs of
        Right out -> do
          out @?= bs
          -- The sharing check: the result must be the same bytes,
          -- not a fresh copy. ByteString equality alone doesn't
          -- prove sharing, so just confirm correctness here.
          assertBool "len preserved" (BS.length out == BS.length bs)
        Left e -> assertFailure ("unexpected error: " <> show e)

  , testCase "simple %20 → space" $
      urlDecode "hello%20world" @?= Right "hello world"

  , testCase "lowercase hex digits" $
      urlDecode "%e2%98%83" @?= Right "\xe2\x98\x83"

  , testCase "uppercase hex digits" $
      urlDecode "%E2%98%83" @?= Right "\xe2\x98\x83"

  , testCase "form mode decodes + as space" $
      urlDecodeForm "a+b%20c" @?= Right "a b c"

  , testCase "non-form mode leaves + alone" $
      urlDecode "a+b%20c" @?= Right "a+b c"

  , testCase "consecutive escapes" $
      urlDecode "%2F%2F%2F" @?= Right "///"

  , testCase "escape at start and end" $
      urlDecode "%20middle%20" @?= Right " middle "

  , testCase "empty input round-trips" $
      urlDecode "" @?= Right ""

  , testCase "Maybe variants mirror Either" $ do
      urlDecodeMaybe "%20" @?= Just " "
      urlDecodeMaybe "%2Z" @?= Nothing
      urlDecodeFormMaybe "a+b" @?= Just "a b"
  ]

-- ---------------------------------------------------------------------------
-- Scanner fast path
-- ---------------------------------------------------------------------------

scannerTests :: TestTree
scannerTests = testGroup "firstSpecialOffset"
  [ testCase "unescaped input → offset == length" $ do
      let bs = "/abc/def?x=1"
      firstSpecialOffset False bs @?= BS.length bs

  , testCase "finds first %" $
      firstSpecialOffset False "abc%20def" @?= 3

  , testCase "ignores + when not in form mode" $
      firstSpecialOffset False "abc+def%20" @?= 7

  , testCase "form mode finds first + or %" $ do
      firstSpecialOffset True  "abc+def%20" @?= 3
      firstSpecialOffset True  "abc%20def+" @?= 3

  , testCase "long unescaped input is handled by the SIMD stride" $ do
      -- Pad past 32 bytes to make sure the AVX2 / SSE2 stride
      -- runs at least once.
      let bs = BS.replicate 100 (asciiByte 'a')
      firstSpecialOffset False bs @?= 100

  , testCase "% located deep inside a large buffer" $ do
      let prefix = BS.replicate 200 (asciiByte 'a')
          bs     = prefix <> "%41"
      firstSpecialOffset False bs @?= 200
  ]

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

errorTests :: TestTree
errorTests = testGroup "errors"
  [ testCase "lonely % at end" $
      urlDecode "abc%" @?= Left TruncatedEscape

  , testCase "% followed by one hex digit" $
      urlDecode "abc%4" @?= Left TruncatedEscape

  , testCase "non-hex first nibble" $
      urlDecode "%Z0" @?= Left InvalidHexDigit

  , testCase "non-hex second nibble" $
      urlDecode "%0Z" @?= Left InvalidHexDigit

  , testCase "form mode propagates the same errors" $
      urlDecodeForm "abc+%" @?= Left TruncatedEscape
  ]

-- ---------------------------------------------------------------------------
-- Flatparse integration
-- ---------------------------------------------------------------------------

flatparseTests :: TestTree
flatparseTests = testGroup "flatparse combinators"
  [ testCase "urlDecodedWhile until '&'" $ do
      let parser = urlDecodedWhile (/= ampersand)
          input  = "hello%20world&next=1"
      case FP.runParser parser input of
        FP.OK out rest -> do
          out  @?= "hello world"
          rest @?= "&next=1"
        other -> assertFailure ("unexpected: " <> show other)

  , testCase "formUrlDecodedWhile until '&' translates +" $ do
      let parser = formUrlDecodedWhile (\w -> w /= ampersand)
          input  = "a+b%20c&done"
      case FP.runParser parser input of
        FP.OK out rest -> do
          out  @?= "a b c"
          rest @?= "&done"
        other -> assertFailure ("unexpected: " <> show other)

  , testCase "decode failure inside parser surfaces as Err" $ do
      let parser = urlDecodedWhile (/= ampersand) <* FP.eof
          input  = "bad%2Z"
      case FP.runParser parser input of
        FP.Err InvalidHexDigit -> pure ()
        other -> assertFailure
          ("expected InvalidHexDigit error, got: " <> show other)

  , testCase "urlDecodedSegment composes with arbitrary inner parser" $ do
      let inner  = FP.skipMany (FP.skipSatisfyAscii (/= '|'))
          parser = (,) <$> urlDecodedSegment inner <*> FP.takeRest
          input  = "x%3Dy|tail"
      case FP.runParser parser input of
        FP.OK (decoded, rest) "" -> do
          decoded @?= "x=y"
          rest    @?= "|tail"
        other -> assertFailure ("unexpected: " <> show other)
  ]

-- ---------------------------------------------------------------------------
-- Stress: spans the SIMD strides
-- ---------------------------------------------------------------------------

stressTests :: TestTree
stressTests = testGroup "stride alignment"
  [ testCase "decode across a 32-byte boundary" $ do
      -- Place the escape at byte 30 so the SIMD scan crosses
      -- both a 16-byte and 32-byte boundary on the way to it.
      let prefix = BS.replicate 30 (asciiByte 'a')
          bs     = prefix <> "%21!"
      urlDecode bs @?= Right (prefix <> "!!")

  , testCase "long alternating run" $ do
      let bs = BS.concat (replicate 50 "a%20")
      urlDecode bs @?= Right (BS.concat (replicate 50 "a "))

  , testCase "decoded output strictly shorter than input" $ do
      let bs = "%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F"
      case urlDecode bs of
        Right out -> do
          out @?= "////////////"
          assertBool "shrank" (BS.length out < BS.length bs)
        Left e -> assertFailure (show e)
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

ampersand :: Word8
ampersand = 0x26

asciiByte :: Char -> Word8
asciiByte = toEnum . fromEnum
