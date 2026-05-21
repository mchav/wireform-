{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PackageImports #-}

{-|
Module      : Benchmarks.Serialization
Description : Per-message Wire encode/decode benchmarks

Microbenchmarks for the Wire codec runtime as it dispatches on
specific Kafka protocol message classes.  Useful for tracking
per-message serialization cost over time and for comparing
producer- vs consumer-side overhead.

The benchmark works directly off the codegen-emitted
'Kafka.Protocol.Wire.Codec.WireCodec' instance for each message,
so adding coverage for a new request/response type is a one-line
edit.
-}
module Benchmarks.Serialization (benchmarks) where

import Criterion           (Benchmark, bench, bgroup, whnf)
import qualified Data.ByteString as BS
import qualified Data.Int as Int

import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ProduceRequest  as Produce
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.FetchRequest    as Fetch
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.MetadataRequest as Metadata
import qualified "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec                as WC
import           Benchmarks.Util                          ( createProduceRequest
                                                          , createFetchRequest
                                                          , createMetadataRequest
                                                          )

----------------------------------------------------------------------
-- Public group
----------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "Serialization"
  [ benchProduceRequest
  , benchFetchRequest
  , benchMetadataRequest
  ]

----------------------------------------------------------------------
-- ProduceRequest (apiKey 0, v9 flexible)
----------------------------------------------------------------------

benchProduceRequest :: Benchmark
benchProduceRequest = bgroup "ProduceRequest (v9, flexible)"
  [ produceRequestSized "small  (1 topic / 1 partition)"     1   1
  , produceRequestSized "medium (10 topics / 10 partitions)" 10  10
  , produceRequestSized "large  (100 topics / 100 partitions)" 100 100
  ]

produceRequestSized :: String -> Int -> Int -> Benchmark
produceRequestSized label topics partitions =
  let !msg     = createProduceRequest topics partitions
      !version = 9
      !encoded = WC.runEncodeVer @Produce.ProduceRequest version msg
  in bgroup label
       [ bench "encode" $ whnf (BS.length . WC.runEncodeVer @Produce.ProduceRequest version) msg
       , bench "decode" $ whnf (decodeOk @Produce.ProduceRequest version)            encoded
       , bench "roundtrip" $ whnf (\m ->
            case WC.runDecodeVer @Produce.ProduceRequest version
                   (WC.runEncodeVer @Produce.ProduceRequest version m) of
              Right _ -> ()
              Left  e -> error e) msg
       ]

----------------------------------------------------------------------
-- FetchRequest (apiKey 1, v12 flexible)
----------------------------------------------------------------------

benchFetchRequest :: Benchmark
benchFetchRequest = bgroup "FetchRequest (v12, flexible)"
  [ fetchRequestSized "small  (1 topic / 1 partition)"     1   1
  , fetchRequestSized "medium (10 topics / 10 partitions)" 10  10
  , fetchRequestSized "large  (100 topics / 100 partitions)" 100 100
  ]

fetchRequestSized :: String -> Int -> Int -> Benchmark
fetchRequestSized label topics partitions =
  let !msg     = createFetchRequest topics partitions
      !version = 12
      !encoded = WC.runEncodeVer @Fetch.FetchRequest version msg
  in bgroup label
       [ bench "encode" $ whnf (BS.length . WC.runEncodeVer @Fetch.FetchRequest version) msg
       , bench "decode" $ whnf (decodeOk @Fetch.FetchRequest version)            encoded
       ]

----------------------------------------------------------------------
-- MetadataRequest (apiKey 3, v9 flexible)
----------------------------------------------------------------------

benchMetadataRequest :: Benchmark
benchMetadataRequest = bgroup "MetadataRequest (v9, flexible)"
  [ metadataRequestSized "small  (1 topic)"   1
  , metadataRequestSized "medium (50 topics)" 50
  , metadataRequestSized "large  (500 topics)" 500
  ]

-- | Decode helper that forces the result to WHNF and discards
-- the typed value (criterion's 'whnf' won't otherwise inspect
-- the Either layer).
decodeOk :: forall a. WC.WireCodec a => Int16Like -> BS.ByteString -> ()
decodeOk version bs =
  case WC.runDecodeVer @a version bs of
    Right !_ -> ()
    Left  e  -> error ("decode failed: " ++ e)

-- | Local alias so callers don't need to import Data.Int just to
-- spell the type of an api version.
type Int16Like = Int.Int16

metadataRequestSized :: String -> Int -> Benchmark
metadataRequestSized label topics =
  let !msg     = createMetadataRequest topics
      !version = 9
      !encoded = WC.runEncodeVer @Metadata.MetadataRequest version msg
  in bgroup label
       [ bench "encode" $ whnf (BS.length . WC.runEncodeVer @Metadata.MetadataRequest version) msg
       , bench "decode" $ whnf (decodeOk @Metadata.MetadataRequest version)              encoded
       ]
