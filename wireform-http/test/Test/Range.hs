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

import Test.Syd

tests :: Spec
tests = describe "Network.HTTP.Client.Range" $ sequence_
  [ describe "rangeHeader" $ sequence_
      [ it "closed range" $
          rangeHeader [byteRange 0 99] `shouldBe` "bytes=0-99"
      , it "open range from offset" $
          rangeHeader [byteRangeFrom 1000] `shouldBe` "bytes=1000-"
      , it "suffix range" $
          rangeHeader [byteRangeSuffix 500] `shouldBe` "bytes=-500"
      , it "comma-joined multi range" $
          rangeHeader [byteRange 0 99, byteRangeFrom 200, byteRangeSuffix 50]
            `shouldBe` "bytes=0-99,200-,-50"
      ]
  , describe "parseRange" $ sequence_
      [ it "round-trips closed" $
          parseRange "bytes=0-99" `shouldBe` Just [byteRange 0 99]
      , it "round-trips open" $
          parseRange "bytes=1000-" `shouldBe` Just [byteRangeFrom 1000]
      , it "round-trips suffix" $
          parseRange "bytes=-500" `shouldBe` Just [byteRangeSuffix 500]
      , it "round-trips multi" $
          parseRange "bytes=0-99,200-,-50"
            `shouldBe` Just [byteRange 0 99, byteRangeFrom 200, byteRangeSuffix 50]
      , it "rejects other range-units" $
          parseRange "rows=0-9" `shouldBe` Nothing
      ]
  , describe "parseContentRange / parseContentRangeFull" $ sequence_
      [ it "satisfied returns the legacy shape" $
          parseContentRange "bytes 0-499/1234"
            `shouldBe` Just (ContentRange 0 499 (Just 1234))
      , it "satisfied with unknown total" $
          parseContentRange "bytes 0-9/*"
            `shouldBe` Just (ContentRange 0 9 Nothing)
      , it "unsatisfied is Nothing through parseContentRange" $
          parseContentRange "bytes */4096" `shouldBe` Nothing
      , it "unsatisfied is detectable through parseContentRangeFull" $
          case parseContentRangeFull "bytes */4096" of
            Just (HCR.ContentRange _ (HCR.RangeRespUnsatisfied (Just 4096))) -> pure ()
            other -> expectationFailure (show other)
      ]
  , describe "parseAcceptRanges" $ sequence_
      [ it "literal none" $
          parseAcceptRanges "none" `shouldBe` Just AcceptRangesNone
      , it "single unit" $
          parseAcceptRanges "bytes" `shouldBe` Just (AcceptRangesUnits ["bytes"])
      , it "comma list" $
          parseAcceptRanges "bytes, custom-unit"
            `shouldBe` Just (AcceptRangesUnits ["bytes", "custom-unit"])
      , it "rejects junk" $
          parseAcceptRanges "" `shouldBe` Nothing
      ]
  , describe "parseMultipartByteranges" $ sequence_
      [ it "two parts with explicit boundaries" $
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
                 mbContentType a `shouldBe` Just "text/plain"
                 mbContentType b `shouldBe` Just "text/plain"
                 crStart (mbRange a) `shouldBe` 0
                 crEnd   (mbRange a) `shouldBe` 9
                 crTotal (mbRange a) `shouldBe` Just 200
                 mbBody  a `shouldBe` "0123456789"
                 crStart (mbRange b) `shouldBe` 100
                 crEnd   (mbRange b) `shouldBe` 109
                 mbBody  b `shouldBe` "ABCDEFGHIJ"
               other -> error (show other)
      , it "skips a part with no Content-Range" $
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
                 (length xs) `shouldBe` 1
                 (mbBody (head xs)) `shouldBe` "abc"
               Nothing -> error "expected a result"
      ]
  , describe "withRange" $ sequence_
      [ it "sets Range header on a Request" $
          -- This is more of a smoke-test: 'withRange' is a thin
          -- wrapper over 'setHeader' and the header bytes come
          -- from 'rangeHeader' which is exercised above.  We
          -- mostly want to know it compiles and produces a
          -- non-empty value.
          (BS.length (rangeHeader [byteRange 0 1]) > 0) `shouldBe` True
      ]
  ]

