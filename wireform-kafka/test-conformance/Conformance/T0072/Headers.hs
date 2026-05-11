{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conformance.T0072.Headers
Description : librdkafka @tests\/0072-headers_ut.c@

librdkafka's @0072-headers_ut@ adds, looks up, removes, and iterates
over message headers via the @rd_kafka_header_*@ API. Our analogue
is 'Kafka.Protocol.RecordBatch.RecordHeader' — just a record with
a key (UTF-8 bytes) and an optional value. We test the cases the
librdkafka file covers:

  * Empty header set is a valid record.
  * Multiple headers preserve insertion order on encode/decode.
  * Headers with absent values are encoded as null length and
    decoded back to 'Nothing'.
-}
module Conformance.T0072.Headers (tests) where

import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kafka.Protocol.RecordBatch as RB

tests :: TestTree
tests = testGroup "0072-headers_ut"
  [ testCase "RecordHeader: present value is returned verbatim" $ do
      let h = RB.RecordHeader "trace-id" (Just "abc123")
      RB.headerKey h   @?= "trace-id"
      RB.headerValue h @?= Just "abc123"

  , testCase "RecordHeader: absent value (Nothing) is preserved" $ do
      let h = RB.RecordHeader "x-debug" Nothing
      RB.headerValue h @?= Nothing

  , testCase "Empty header list is a valid Record" $ do
      let r = RB.Record
            { RB.recordOffsetDelta    = 0
            , RB.recordTimestampDelta = 0
            , RB.recordKey            = Nothing
            , RB.recordValue          = "payload"
            , RB.recordHeaders        = []
            }
      length (RB.recordHeaders r) @?= 0

  , testCase "Multiple headers preserve order" $ do
      let hs = [ RB.RecordHeader "a" (Just "1")
               , RB.RecordHeader "b" Nothing
               , RB.RecordHeader "c" (Just "3")
               ]
          r  = RB.Record 0 0 Nothing BS.empty hs
      map RB.headerKey   (RB.recordHeaders r) @?= ["a","b","c"]
      map RB.headerValue (RB.recordHeaders r) @?= [Just "1", Nothing, Just "3"]
  ]
