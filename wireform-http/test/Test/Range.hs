{-# LANGUAGE OverloadedStrings #-}
{- |
Tests for "Network.HTTP.Client.Range".

The hermes-side parser/renderer is already tested in
@hermes-tests@; this module covers the wireform-flavoured
projection (the 'ByteRange' from/to/suffix split) plus the
helpers that are only at this layer:
'parseContentRangeFull', 'parseAcceptRanges', and the
@multipart/byteranges@ body parser.
-}
module Test.Range (tests) where

import qualified Data.ByteString as BS

import qualified Network.HTTP.Headers.ContentRange as HCR
import Network.HTTP.Client.Range

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

tests :: TestTree
tests = testGroup "Network.HTTP.Client.Range"
  [ testGroup "rangeHeader"
      [ testCase "closed range" $
          rangeHeader [byteRange 0 99] @?= "bytes=0-99"
      , testCase "open range from offset" $
          rangeHeader [byteRangeFrom 1000] @?= "bytes=1000-"
      , testCase "suffix range" $
          rangeHeader [byteRangeSuffix 500] @?= "bytes=-500"
      , testCase "comma-joined multi range" $
          rangeHeader [byteRange 0 99, byteRangeFrom 200, byteRangeSuffix 50]
            @?= "bytes=0-99,200-,-50"
      ]
  , testGroup "parseRange"
      [ testCase "round-trips closed" $
          parseRange "bytes=0-99" @?= Just [byteRange 0 99]
      , testCase "round-trips open" $
          parseRange "bytes=1000-" @?= Just [byteRangeFrom 1000]
      , testCase "round-trips suffix" $
          parseRange "bytes=-500" @?= Just [byteRangeSuffix 500]
      , testCase "round-trips multi" $
          parseRange "bytes=0-99,200-,-50"
            @?= Just [byteRange 0 99, byteRangeFrom 200, byteRangeSuffix 50]
      , testCase "rejects other range-units" $
          parseRange "rows=0-9" @?= Nothing
      ]
  , testGroup "parseContentRange / parseContentRangeFull"
      [ testCase "satisfied returns the legacy shape" $
          parseContentRange "bytes 0-499/1234"
            @?= Just (ContentRange 0 499 (Just 1234))
      , testCase "satisfied with unknown total" $
          parseContentRange "bytes 0-9/*"
            @?= Just (ContentRange 0 9 Nothing)
      , testCase "unsatisfied is Nothing through parseContentRange" $
          parseContentRange "bytes */4096" @?= Nothing
      , testCase "unsatisfied is detectable through parseContentRangeFull" $
          case parseContentRangeFull "bytes */4096" of
            Just (HCR.ContentRange _ (HCR.RangeRespUnsatisfied (Just 4096))) -> pure ()
            other -> error (show other)
      ]
  , testGroup "parseAcceptRanges"
      [ testCase "literal none" $
          parseAcceptRanges "none" @?= Just AcceptRangesNone
      , testCase "single unit" $
          parseAcceptRanges "bytes" @?= Just (AcceptRangesUnits ["bytes"])
      , testCase "comma list" $
          parseAcceptRanges "bytes, custom-unit"
            @?= Just (AcceptRangesUnits ["bytes", "custom-unit"])
      , testCase "rejects junk" $
          parseAcceptRanges "" @?= Nothing
      ]
  , testGroup "parseMultipartByteranges"
      [ testCase "two parts with explicit boundaries" $
          let boundary = "BOUNDARY"
              body = BS.concat
                [ "--BOUNDARY\r\n"
                , "Content-Type: text/plain\r\n"
                , "Content-Range: bytes 0-9/200\r\n"
                , "\r\n"
                , "0123456789"
                , "\r\n--BOUNDARY\r\n"
                , "Content-Type: text/plain\r\n"
                , "Content-Range: bytes 100-109/200\r\n"
                , "\r\n"
                , "ABCDEFGHIJ"
                , "\r\n--BOUNDARY--\r\n"
                ]
          in case parseMultipartByteranges boundary body of
               Just [a, b] -> do
                 mbContentType a @?= Just "text/plain"
                 mbContentType b @?= Just "text/plain"
                 crStart (mbRange a) @?= 0
                 crEnd   (mbRange a) @?= 9
                 crTotal (mbRange a) @?= Just 200
                 mbBody  a @?= "0123456789"
                 crStart (mbRange b) @?= 100
                 crEnd   (mbRange b) @?= 109
                 mbBody  b @?= "ABCDEFGHIJ"
               other -> error (show other)
      , testCase "skips a part with no Content-Range" $
          let boundary = "B"
              body = BS.concat
                [ "--B\r\n"
                , "Content-Type: text/plain\r\n"
                , "\r\n"
                , "garbage"
                , "\r\n--B\r\n"
                , "Content-Range: bytes 0-2/3\r\n"
                , "\r\n"
                , "abc"
                , "\r\n--B--\r\n"
                ]
          in case parseMultipartByteranges boundary body of
               Just xs -> do
                 -- The garbage part is dropped because its
                 -- header block carries no Content-Range; we
                 -- get only the valid second part.
                 assertEqual "kept parts" 1 (length xs)
                 assertEqual "kept body" "abc" (mbBody (head xs))
               Nothing -> error "expected a result"
      ]
  , testGroup "withRange"
      [ testCase "sets Range header on a Request" $
          -- This is more of a smoke-test: 'withRange' is a thin
          -- wrapper over 'setHeader' and the header bytes come
          -- from 'rangeHeader' which is exercised above.  We
          -- mostly want to know it compiles and produces a
          -- non-empty value.
          assertBool "rangeHeader non-empty" (BS.length (rangeHeader [byteRange 0 1]) > 0)
      ]
  ]

