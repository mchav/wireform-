{-# LANGUAGE BangPatterns #-}

{-|
Module      : Benchmarks.Serialization
Description : Benchmarks for serialization libraries
Copyright   : (c) 2025
License     : BSD-3-Clause

This module benchmarks different serialization libraries (bytes, binary, cereal)
for encoding and decoding Kafka protocol messages to measure their relative
performance characteristics.

Note: Currently, we only benchmark the 'bytes' library that we actually use.
To fairly compare binary and cereal, we would need to implement equivalent
encoders/decoders for those libraries, which would be a significant undertaking.
For now, this module provides a framework and benchmarks for the bytes-based
serialization we currently use.
-}
module Benchmarks.Serialization (benchmarks) where

import Criterion (Benchmark, bench, bgroup, whnf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)

-- bytes library (what we currently use)
import Data.Bytes.Serial (serialize, deserialize)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)

-- binary library (for comparison)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as BinaryGet
import qualified Data.Binary.Put as BinaryPut

-- cereal library (for comparison)
import qualified Data.Serialize as Cereal

import qualified Kafka.Protocol.Generated.ProduceRequest as Produce
import qualified Kafka.Protocol.Generated.FetchRequest as Fetch
import qualified Kafka.Protocol.Generated.MetadataRequest as Metadata
import Benchmarks.Util (createProduceRequest, createFetchRequest, createMetadataRequest)

-- -----------------------------------------------------------------------------
-- Benchmarks
-- -----------------------------------------------------------------------------

-- | All serialization benchmarks
benchmarks :: Benchmark
benchmarks = bgroup "Serialization"
  [ benchProduceRequest
  , benchFetchRequest
  , benchMetadataRequest
  ]

-- -----------------------------------------------------------------------------
-- ProduceRequest Benchmarks
-- -----------------------------------------------------------------------------

benchProduceRequest :: Benchmark
benchProduceRequest = bgroup "ProduceRequest"
  [ benchProduceRequestSmall
  , benchProduceRequestMedium
  , benchProduceRequestLarge
  ]

benchProduceRequestSmall :: Benchmark
benchProduceRequestSmall = 
  let msg = createProduceRequest 1 1
      version = 9  -- Flexible version
      encoded = runPutS (Produce.encodeProduceRequest version msg)
  in bgroup "Small(1topic/1partition)"
    [ bench "encode/bytes" $ whnf (runPutS . Produce.encodeProduceRequest version) msg
    , bench "decode/bytes" $ whnf (runGetS (Produce.decodeProduceRequest version)) encoded
    , bench "roundtrip/bytes" $ whnf (\m -> runGetS (Produce.decodeProduceRequest version) (runPutS (Produce.encodeProduceRequest version m))) msg
    ]

benchProduceRequestMedium :: Benchmark
benchProduceRequestMedium = 
  let msg = createProduceRequest 10 10
      version = 9  -- Flexible version
      encoded = runPutS (Produce.encodeProduceRequest version msg)
  in bgroup "Medium(10topics/10partitions)"
    [ bench "encode/bytes" $ whnf (runPutS . Produce.encodeProduceRequest version) msg
    , bench "decode/bytes" $ whnf (runGetS (Produce.decodeProduceRequest version)) encoded
    , bench "roundtrip/bytes" $ whnf (\m -> runGetS (Produce.decodeProduceRequest version) (runPutS (Produce.encodeProduceRequest version m))) msg
    ]

benchProduceRequestLarge :: Benchmark
benchProduceRequestLarge = 
  let msg = createProduceRequest 100 100
      version = 9  -- Flexible version
      encoded = runPutS (Produce.encodeProduceRequest version msg)
  in bgroup "Large(100topics/100partitions)"
    [ bench "encode/bytes" $ whnf (runPutS . Produce.encodeProduceRequest version) msg
    , bench "decode/bytes" $ whnf (runGetS (Produce.decodeProduceRequest version)) encoded
    , bench "roundtrip/bytes" $ whnf (\m -> runGetS (Produce.decodeProduceRequest version) (runPutS (Produce.encodeProduceRequest version m))) msg
    ]

-- -----------------------------------------------------------------------------
-- FetchRequest Benchmarks
-- -----------------------------------------------------------------------------

benchFetchRequest :: Benchmark
benchFetchRequest = bgroup "FetchRequest"
  [ benchFetchRequestSmall
  , benchFetchRequestMedium
  , benchFetchRequestLarge
  ]

benchFetchRequestSmall :: Benchmark
benchFetchRequestSmall = 
  let msg = createFetchRequest 1 1
      version = 12  -- Flexible version
      encoded = runPutS (Fetch.encodeFetchRequest version msg)
  in bgroup "Small(1topic/1partition)"
    [ bench "encode/bytes" $ whnf (runPutS . Fetch.encodeFetchRequest version) msg
    , bench "decode/bytes" $ whnf (runGetS (Fetch.decodeFetchRequest version)) encoded
    , bench "roundtrip/bytes" $ whnf (\m -> runGetS (Fetch.decodeFetchRequest version) (runPutS (Fetch.encodeFetchRequest version m))) msg
    ]

benchFetchRequestMedium :: Benchmark
benchFetchRequestMedium = 
  let msg = createFetchRequest 10 10
      version = 12  -- Flexible version
      encoded = runPutS (Fetch.encodeFetchRequest version msg)
  in bgroup "Medium(10topics/10partitions)"
    [ bench "encode/bytes" $ whnf (runPutS . Fetch.encodeFetchRequest version) msg
    , bench "decode/bytes" $ whnf (runGetS (Fetch.decodeFetchRequest version)) encoded
    , bench "roundtrip/bytes" $ whnf (\m -> runGetS (Fetch.decodeFetchRequest version) (runPutS (Fetch.encodeFetchRequest version m))) msg
    ]

benchFetchRequestLarge :: Benchmark
benchFetchRequestLarge = 
  let msg = createFetchRequest 100 100
      version = 12  -- Flexible version
      encoded = runPutS (Fetch.encodeFetchRequest version msg)
  in bgroup "Large(100topics/100partitions)"
    [ bench "encode/bytes" $ whnf (runPutS . Fetch.encodeFetchRequest version) msg
    , bench "decode/bytes" $ whnf (runGetS (Fetch.decodeFetchRequest version)) encoded
    , bench "roundtrip/bytes" $ whnf (\m -> runGetS (Fetch.decodeFetchRequest version) (runPutS (Fetch.encodeFetchRequest version m))) msg
    ]

-- -----------------------------------------------------------------------------
-- MetadataRequest Benchmarks
-- -----------------------------------------------------------------------------

benchMetadataRequest :: Benchmark
benchMetadataRequest = bgroup "MetadataRequest"
  [ benchMetadataRequestSmall
  , benchMetadataRequestMedium
  , benchMetadataRequestLarge
  ]

benchMetadataRequestSmall :: Benchmark
benchMetadataRequestSmall = 
  let msg = createMetadataRequest 1
      version = 9  -- Flexible version
      encoded = runPutS (Metadata.encodeMetadataRequest version msg)
  in bgroup "Small(1topic)"
    [ bench "encode/bytes" $ whnf (runPutS . Metadata.encodeMetadataRequest version) msg
    , bench "decode/bytes" $ whnf (runGetS (Metadata.decodeMetadataRequest version)) encoded
    , bench "roundtrip/bytes" $ whnf (\m -> runGetS (Metadata.decodeMetadataRequest version) (runPutS (Metadata.encodeMetadataRequest version m))) msg
    ]

benchMetadataRequestMedium :: Benchmark
benchMetadataRequestMedium = 
  let msg = createMetadataRequest 50
      version = 9  -- Flexible version
      encoded = runPutS (Metadata.encodeMetadataRequest version msg)
  in bgroup "Medium(50topics)"
    [ bench "encode/bytes" $ whnf (runPutS . Metadata.encodeMetadataRequest version) msg
    , bench "decode/bytes" $ whnf (runGetS (Metadata.decodeMetadataRequest version)) encoded
    , bench "roundtrip/bytes" $ whnf (\m -> runGetS (Metadata.decodeMetadataRequest version) (runPutS (Metadata.encodeMetadataRequest version m))) msg
    ]

benchMetadataRequestLarge :: Benchmark
benchMetadataRequestLarge = 
  let msg = createMetadataRequest 500
      version = 9  -- Flexible version
      encoded = runPutS (Metadata.encodeMetadataRequest version msg)
  in bgroup "Large(500topics)"
    [ bench "encode/bytes" $ whnf (runPutS . Metadata.encodeMetadataRequest version) msg
    , bench "decode/bytes" $ whnf (runGetS (Metadata.decodeMetadataRequest version)) encoded
    , bench "roundtrip/bytes" $ whnf (\m -> runGetS (Metadata.decodeMetadataRequest version) (runPutS (Metadata.encodeMetadataRequest version m))) msg
    ]

{- NOTE: Binary and Cereal Comparison

To fairly compare the `binary` and `cereal` libraries against `bytes`, we would need to:

1. Implement complete Binary instances for all Kafka protocol types
2. Implement complete Serialize instances for all Kafka protocol types
3. Handle version-aware encoding/decoding for both
4. Handle flexible vs non-flexible versions
5. Handle all the Kafka-specific types (VarInt, CompactString, TaggedFields, etc.)

This is a significant undertaking (essentially reimplementing the entire protocol layer
for each library) and would only be worthwhile if we were seriously considering switching
serialization libraries.

For now, we benchmark the `bytes` library that we actually use. If performance becomes
a concern, we can implement comparison benchmarks at that time.

Example skeleton of what would be needed:

instance Binary ProduceRequest where
  put req = do
    -- Implement version-aware encoding matching Kafka protocol
    Binary.put (transactionalId req)
    Binary.put (acks req)
    ...
  
  get = do
    -- Implement version-aware decoding matching Kafka protocol
    tid <- Binary.get
    acks <- Binary.get
    ...
    return ProduceRequest{..}

Similarly for Cereal:

instance Serialize ProduceRequest where
  put = ... -- Similar to Binary
  get = ... -- Similar to Binary

Then we could add benchmarks like:

    , bench "encode/binary" $ whnf Binary.encode msg
    , bench "decode/binary" $ nf Binary.decode encoded
    , bench "encode/cereal" $ whnf Cereal.encode msg
    , bench "decode/cereal" $ nf Cereal.decode encoded
-}

