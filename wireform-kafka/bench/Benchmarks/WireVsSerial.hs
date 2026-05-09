{-# LANGUAGE OverloadedStrings #-}

-- | Direct comparison of the new direct-poke 'Kafka.Protocol.Wire'
-- codec vs the original 'Data.Bytes.Serial'-based path.
--
-- Each entry encodes the same value through both codecs and measures
-- the wall-clock time. Both surfaces produce byte-identical wire
-- output (proven by 'Protocol.WireSpec' cross-codec equivalence
-- properties) so the only thing the benchmark measures is the
-- per-byte overhead of the writer.
module Benchmarks.WireVsSerial (benchmarks) where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import qualified Data.ByteString as BS
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize)
import Data.Int (Int32, Int64)

import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Wire as W
import Kafka.Protocol.Wire.Primitives ()  -- Wire instances for KafkaString / KafkaBytes / VarInt

benchmarks :: Benchmark
benchmarks = bgroup "WireVsSerial"
  [ bgroup "Int32 (4-byte fixed)"
      [ bench "Serial"  $ nf (\x -> runPutS (serialize x))    int32Sample
      , bench "Wire"    $ nf W.runWirePut                     int32Sample
      ]
  , bgroup "Int64 (8-byte fixed)"
      [ bench "Serial"  $ nf (\x -> runPutS (serialize x))    int64Sample
      , bench "Wire"    $ nf W.runWirePut                     int64Sample
      ]
  , bgroup "VarInt (zigzag varint)"
      [ bench "Serial small (0..127)"  $ nf (\x -> runPutS (serialize x)) (P.VarInt 17)
      , bench "Wire   small (0..127)"  $ nf W.runWirePut                  (P.VarInt 17)
      , bench "Serial large (Int32 max)" $ nf (\x -> runPutS (serialize x)) (P.VarInt maxBound)
      , bench "Wire   large (Int32 max)" $ nf W.runWirePut                  (P.VarInt maxBound)
      ]
  , bgroup "KafkaString (Int16 length + UTF-8)"
      [ bench "Serial 'hello'"     $ nf (\x -> runPutS (serialize x)) helloString
      , bench "Wire   'hello'"     $ nf W.runWirePut                  helloString
      , bench "Serial 256 chars"   $ nf (\x -> runPutS (serialize x)) longString
      , bench "Wire   256 chars"   $ nf W.runWirePut                  longString
      ]
  , bgroup "KafkaBytes (Int32 length + payload)"
      [ bench "Serial 1 KiB"  $ nf (\x -> runPutS (serialize x)) bytes1k
      , bench "Wire   1 KiB"  $ nf W.runWirePut                  bytes1k
      , bench "Serial 16 KiB" $ nf (\x -> runPutS (serialize x)) bytes16k
      , bench "Wire   16 KiB" $ nf W.runWirePut                  bytes16k
      ]
  , bgroup "CompactString (UVarInt length + UTF-8)"
      [ bench "Serial 'hello'" $
          nf (\x -> runPutS (serialize x)) (P.toCompactString helloString)
      , bench "Wire   'hello'" $
          nf W.runWirePut                  (P.toCompactString helloString)
      ]
  ]
  where
    int32Sample :: Int32
    int32Sample = 1234567

    int64Sample :: Int64
    int64Sample = 1234567890123

    helloString :: P.KafkaString
    helloString = P.mkKafkaString "hello"

    longString :: P.KafkaString
    longString = P.mkKafkaString
                   (foldr (<>) "" (replicate 32 "abcdefgh"))

    bytes1k :: P.KafkaBytes
    bytes1k = P.mkKafkaBytes (BS.replicate 1024 0x41)

    bytes16k :: P.KafkaBytes
    bytes16k = P.mkKafkaBytes (BS.replicate (16 * 1024) 0x41)
