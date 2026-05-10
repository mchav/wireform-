{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pluggable @Kafka.Network.Transport@ surface
-- (specifically the in-memory pipe transport that lets tests
-- drive both ends of a wire conversation in-process).
module Network.TransportSpec (tests) where

import qualified Data.ByteString as BS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Network.Transport as T

tests :: TestTree
tests = testGroup "Kafka.Network.Transport"
  [ testCase "pipe transport: write on one side -> read on the other"
      pipe_round_trip
  , testCase "pipe transport: partial reads return only requested bytes"
      pipe_partial_reads
  , testCase "pipe transport: closing surfaces EOF on next read"
      pipe_close_eof
  , testCase "pipe transport: write after close returns Left"
      pipe_write_after_close
  , testCase "transportName labels are distinct for the two sides"
      pipe_names_distinct
  ]

pipe_round_trip :: IO ()
pipe_round_trip = do
  (cli, brk) <- T.mkPipeTransport
  Right () <- T.transportWrite cli "hello"
  Right got <- T.transportRead brk 5
  got @?= "hello"

pipe_partial_reads :: IO ()
pipe_partial_reads = do
  (cli, brk) <- T.mkPipeTransport
  Right () <- T.transportWrite cli "hello world"
  Right one <- T.transportRead brk 5
  one @?= "hello"
  Right two <- T.transportRead brk 6
  two @?= " world"

pipe_close_eof :: IO ()
pipe_close_eof = do
  (cli, brk) <- T.mkPipeTransport
  T.transportClose cli
  Right got <- T.transportRead brk 5
  -- After close + buffer drain, an empty bytestring signals EOF.
  -- (We didn't write anything before close.)
  assertBool "EOF surfaces as empty bytestring" (BS.null got)

pipe_write_after_close :: IO ()
pipe_write_after_close = do
  (cli, _brk) <- T.mkPipeTransport
  T.transportClose cli
  r <- T.transportWrite cli "ignored"
  case r of
    Left _ -> pure ()
    Right () -> error "expected Left after close"

pipe_names_distinct :: IO ()
pipe_names_distinct = do
  (cli, brk) <- T.mkPipeTransport
  let n1 = T.transportName cli
      n2 = T.transportName brk
  assertBool "names differ" (n1 /= n2)
