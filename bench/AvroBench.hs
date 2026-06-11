{-# LANGUAGE BangPatterns #-}

module Main where

import Avro.Decode (decodeAvro)
import Avro.Encode (encodeAvro)
import Avro.Schema (AvroField (..), AvroSchema (..), AvroType (..))
import Avro.Value qualified as AV
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import System.CPUTime
import Thrift.Decode (decodeBinary, decodeCompact)
import Thrift.Encode (encodeBinary, encodeCompact)
import Thrift.Value qualified as TV
import Thrift.Wire ()


main :: IO ()
main = do
  putStrLn "Avro / Thrift benchmarks"
  putStrLn (replicate 60 '=')

  benchAvroEncode
  benchAvroDecode
  benchThriftBinaryEncode
  benchThriftBinaryDecode
  benchThriftCompactEncode
  benchThriftCompactDecode


--------------------------------------------------------------------------------
-- Shared data
--------------------------------------------------------------------------------

personSchema :: AvroType
personSchema =
  AvroRecord
    { avroRecordName = "Person"
    , avroRecordNamespace = Nothing
    , avroRecordDoc = Nothing
    , avroRecordAliases = V.empty
    , avroRecordFields =
        V.fromList
          [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "age" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "email" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
          , AvroField "score" (AvroPrimitive AvroDouble) Nothing Nothing V.empty Nothing Map.empty
          ]
    , avroRecordProps = Map.empty
    }


personAvro :: AV.Value
personAvro =
  AV.Record $
    V.fromList
      [ AV.String "John Doe"
      , AV.Int 30
      , AV.String "john@example.com"
      , AV.Double 95.5
      ]


personThrift :: TV.Value
personThrift =
  TV.Struct $
    V.fromList
      [ (1, TV.String "John Doe")
      , (2, TV.I32 30)
      , (3, TV.String "john@example.com")
      , (4, TV.Double 95.5)
      ]


iterations :: Int
iterations = 100000


throughputMBs :: Int -> Int -> Integer -> String
throughputMBs iters msgSize picoSeconds =
  let totalBytes = fromIntegral iters * fromIntegral msgSize :: Double
      seconds = fromIntegral picoSeconds / 1e12 :: Double
      mbPerSec = (totalBytes / (1024 * 1024)) / seconds
  in show (round mbPerSec :: Int) ++ " MB/s"


--------------------------------------------------------------------------------
-- Avro benchmarks
--------------------------------------------------------------------------------

benchAvroEncode :: IO ()
benchAvroEncode = do
  let n = iterations
  putStrLn "\nAvro encode (100k iterations):"

  t1 <- getCPUTime
  let !totalSize = goEnc n 0
  t2 <- getCPUTime

  let !elapsed = t2 - t1
  putStrLn $ "  Time: " <> show (elapsed `div` 1000000000) <> " ms"
  putStrLn $ "  Total bytes: " <> show totalSize
  putStrLn $ "  Msg size: " <> show (totalSize `div` n) <> " bytes"
  putStrLn $ "  Throughput: " <> throughputMBs n (totalSize `div` n) elapsed
  where
    goEnc 0 !acc = acc
    goEnc !i !acc = goEnc (i - 1) (acc + BS.length (encodeAvro personSchema personAvro))


benchAvroDecode :: IO ()
benchAvroDecode = do
  let n = iterations
      encoded = encodeAvro personSchema personAvro

  putStrLn "\nAvro decode (100k iterations):"

  t1 <- getCPUTime
  let !count = goDec encoded n 0
  t2 <- getCPUTime

  let !elapsed = t2 - t1
  putStrLn $ "  Time: " <> show (elapsed `div` 1000000000) <> " ms"
  putStrLn $ "  Successful decodes: " <> show count
  putStrLn $ "  Msg size: " <> show (BS.length encoded) <> " bytes"
  putStrLn $ "  Throughput: " <> throughputMBs n (BS.length encoded) elapsed
  where
    goDec :: BS.ByteString -> Int -> Int -> Int
    goDec _ 0 !acc = acc
    goDec !enc !i !acc = case decodeAvro personSchema enc of
      Right _ -> goDec enc (i - 1) (acc + 1)
      Left _ -> goDec enc (i - 1) acc


--------------------------------------------------------------------------------
-- Thrift Binary benchmarks
--------------------------------------------------------------------------------

benchThriftBinaryEncode :: IO ()
benchThriftBinaryEncode = do
  let n = iterations
  putStrLn "\nThrift Binary encode (100k iterations):"

  t1 <- getCPUTime
  let !totalSize = goEnc n 0
  t2 <- getCPUTime

  let !elapsed = t2 - t1
  putStrLn $ "  Time: " <> show (elapsed `div` 1000000000) <> " ms"
  putStrLn $ "  Total bytes: " <> show totalSize
  putStrLn $ "  Msg size: " <> show (totalSize `div` n) <> " bytes"
  putStrLn $ "  Throughput: " <> throughputMBs n (totalSize `div` n) elapsed
  where
    goEnc 0 !acc = acc
    goEnc !i !acc = goEnc (i - 1) (acc + BS.length (encodeBinary personThrift))


benchThriftBinaryDecode :: IO ()
benchThriftBinaryDecode = do
  let n = iterations
      encoded = encodeBinary personThrift

  putStrLn "\nThrift Binary decode (100k iterations):"

  t1 <- getCPUTime
  let !count = goDec encoded n 0
  t2 <- getCPUTime

  let !elapsed = t2 - t1
  putStrLn $ "  Time: " <> show (elapsed `div` 1000000000) <> " ms"
  putStrLn $ "  Successful decodes: " <> show count
  putStrLn $ "  Msg size: " <> show (BS.length encoded) <> " bytes"
  putStrLn $ "  Throughput: " <> throughputMBs n (BS.length encoded) elapsed
  where
    goDec :: BS.ByteString -> Int -> Int -> Int
    goDec _ 0 !acc = acc
    goDec !enc !i !acc = case decodeBinary enc of
      Right _ -> goDec enc (i - 1) (acc + 1)
      Left _ -> goDec enc (i - 1) acc


--------------------------------------------------------------------------------
-- Thrift Compact benchmarks
--------------------------------------------------------------------------------

benchThriftCompactEncode :: IO ()
benchThriftCompactEncode = do
  let n = iterations
  putStrLn "\nThrift Compact encode (100k iterations):"

  t1 <- getCPUTime
  let !totalSize = goEnc n 0
  t2 <- getCPUTime

  let !elapsed = t2 - t1
  putStrLn $ "  Time: " <> show (elapsed `div` 1000000000) <> " ms"
  putStrLn $ "  Total bytes: " <> show totalSize
  putStrLn $ "  Msg size: " <> show (totalSize `div` n) <> " bytes"
  putStrLn $ "  Throughput: " <> throughputMBs n (totalSize `div` n) elapsed
  where
    goEnc 0 !acc = acc
    goEnc !i !acc = goEnc (i - 1) (acc + BS.length (encodeCompact personThrift))


benchThriftCompactDecode :: IO ()
benchThriftCompactDecode = do
  let n = iterations
      encoded = encodeCompact personThrift

  putStrLn "\nThrift Compact decode (100k iterations):"

  t1 <- getCPUTime
  let !count = goDec encoded n 0
  t2 <- getCPUTime

  let !elapsed = t2 - t1
  putStrLn $ "  Time: " <> show (elapsed `div` 1000000000) <> " ms"
  putStrLn $ "  Successful decodes: " <> show count
  putStrLn $ "  Msg size: " <> show (BS.length encoded) <> " bytes"
  putStrLn $ "  Throughput: " <> throughputMBs n (BS.length encoded) elapsed
  where
    goDec :: BS.ByteString -> Int -> Int -> Int
    goDec _ 0 !acc = acc
    goDec !enc !i !acc = case decodeCompact enc of
      Right _ -> goDec enc (i - 1) (acc + 1)
      Left _ -> goDec enc (i - 1) acc
