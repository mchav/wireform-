{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Benchmarks.SerLib
Description : Head-to-head benchmarks of the serialization primitive layer

The protocol encoders / decoders ultimately bottom out on a tight
loop that repeatedly emits (or parses) a handful of fixed-width
primitives plus a few length-prefixed @ByteString@ blobs. The exact
shape is approximately:

  * write Int32 (count)
  * for each element, write { Int32 partition_id, Int32 leader_epoch,
                              Int32 bytes_len, raw bytes }

This module isolates that loop and benchmarks it across four
implementations so we can see what the per-element overhead of the
current Data.Bytes.Put + Serial typeclass stack actually is, and how
much headroom is on the table if we reach for a different
serialization library.

Implementations under test:

  1. *bytes* — what the production codepath uses today
     (@Data.Bytes.Put.PutM@ + @Data.Bytes.Serial.serialize@).
  2. *cereal-direct* — same machinery, but bypassing the @Serial@
     typeclass and calling @putWord32be@ etc. directly. Tells us how
     much overhead the typeclass dictionary is adding.
  3. *binary* — @Data.Binary.Put@ as a control. Same shape, different
     library.
  4. *bytestring-builder* — @Data.ByteString.Builder@ direct. The
     idiomatic high-performance choice on Hackage today; this is the
     reference for "what librdkafka-style raw buffer writes look like
     in Haskell".

Use:

>  cabal bench wireform-kafka --benchmark-options='-m prefix SerLib'
-}
module Benchmarks.SerLib (benchmarks) where

import Control.Monad (replicateM_)
import qualified Data.Binary.Put as BinP
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize)
import qualified Data.Serialize.Put as Cereal
import Data.Int (Int32)
import Data.Word (Word32)

import Criterion (Benchmark, bench, bgroup, whnf)

import qualified Kafka.Protocol.Primitives as P

-- The shared input: a list of (partitionId, leaderEpoch, payload)
-- triples that approximates a single-topic ProduceRequest's inner
-- loop.
type Triple = (Int32, Int32, ByteString)

mkTriples :: Int -> [Triple]
mkTriples n =
  [ (fromIntegral i, fromIntegral (i `mod` 7), fixedPayload)
  | i <- [0 .. n - 1]
  ]

fixedPayload :: ByteString
fixedPayload = BS.replicate 64 0x41  -- 64 'A's

------------------------------------------------------------------------
-- 1. The current production stack: bytes + Serial typeclass
------------------------------------------------------------------------

encodeBytesSerial :: [Triple] -> ByteString
encodeBytesSerial xs = runPutS $ do
  serialize (fromIntegral (length xs) :: Int32)
  mapM_ encodeOne xs
  where
    encodeOne (pid, epoch, bs) = do
      serialize pid
      serialize epoch
      serialize (fromIntegral (BS.length bs) :: Int32)
      serialize (P.mkKafkaBytes bs)
{-# INLINE encodeBytesSerial #-}

------------------------------------------------------------------------
-- 2. Same stack, but bypass the Serial typeclass and call cereal's
--    Put primitives directly. Cuts the dictionary indirection that
--    `serialize` carries.
------------------------------------------------------------------------

encodeCerealDirect :: [Triple] -> ByteString
encodeCerealDirect xs = Cereal.runPut $ do
  Cereal.putWord32be (fromIntegral (length xs))
  mapM_ encodeOne xs
  where
    encodeOne (pid, epoch, bs) = do
      Cereal.putWord32be (fromIntegral pid)
      Cereal.putWord32be (fromIntegral epoch)
      Cereal.putWord32be (fromIntegral (BS.length bs))
      Cereal.putByteString bs
{-# INLINE encodeCerealDirect #-}

------------------------------------------------------------------------
-- 3. binary's Data.Binary.Put as a control.
------------------------------------------------------------------------

encodeBinaryDirect :: [Triple] -> ByteString
encodeBinaryDirect xs = BL.toStrict $ BinP.runPut $ do
  BinP.putWord32be (fromIntegral (length xs))
  mapM_ encodeOne xs
  where
    encodeOne (pid, epoch, bs) = do
      BinP.putWord32be (fromIntegral pid)
      BinP.putWord32be (fromIntegral epoch)
      BinP.putWord32be (fromIntegral (BS.length bs))
      BinP.putByteString bs
{-# INLINE encodeBinaryDirect #-}

------------------------------------------------------------------------
-- 4. Data.ByteString.Builder. This is the realistic ceiling for
--    "Haskell-native, lazy-bytestring builder in the inner loop".
--    librdkafka would beat this by another small constant by writing
--    into a pre-sized buffer with no allocation; we do not chase that
--    here.
------------------------------------------------------------------------

encodeBuilder :: [Triple] -> ByteString
encodeBuilder xs =
  BL.toStrict $ Builder.toLazyByteString $
       Builder.int32BE (fromIntegral (length xs))
    <> foldMap one xs
  where
    one (!pid, !epoch, !bs) =
         Builder.int32BE pid
      <> Builder.int32BE epoch
      <> Builder.int32BE (fromIntegral (BS.length bs))
      <> Builder.byteString bs
{-# INLINE encodeBuilder #-}

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "SerLib"
  [ benchAt 10
  , benchAt 100
  , benchAt 1000
  ]

benchAt :: Int -> Benchmark
benchAt n =
  let !triples = mkTriples n
  in bgroup (show n <> "-elements")
       [ bench "bytes+Serial"        $ whnf encodeBytesSerial   triples
       , bench "cereal direct"       $ whnf encodeCerealDirect  triples
       , bench "binary direct"       $ whnf encodeBinaryDirect  triples
       , bench "bytestring Builder"  $ whnf encodeBuilder       triples
       ]

-- Suppress unused-import warning if the codepath above stops touching
-- replicateM_ (used for sanity-prototyping).
_dummy :: IO ()
_dummy = replicateM_ 0 (return ())
