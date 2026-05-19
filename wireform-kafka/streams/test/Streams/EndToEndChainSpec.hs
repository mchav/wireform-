{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | End-to-end chain tests that thread several DSL operators
-- together: filter -> selectKey -> groupBy -> aggregate ->
-- toStream -> suppress -> toTopic.
module Streams.EndToEndChainSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Imperative

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

i64Bytes :: Int64 -> BSC.ByteString
i64Bytes = serialize int64Serde

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

tests :: TestTree
tests = testGroup "EndToEndChain"
  [ filter_select_groupby_count_chain
  , merge_then_groupby_aggregate
  ]

filter_select_groupby_count_chain :: TestTree
filter_select_groupby_count_chain =
  testCase "filter -> selectKey -> groupByKey -> count materialises per-bucket counts" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    -- Drop \"skip\" tokens, take everything else as a record.
    s1 <- filterStream (\r -> recordValue r /= "skip") src
    -- Re-key by the first character so we can bucket-count.
    s2 <- selectKey (\r -> T.take 1 (recordValue r)) s1
    let g = grouped textSerde textSerde
        kgs = groupByKey g s2
    table <- countStream (materializedAs (storeName "first-char-counts")) kgs
    topo <- buildTopology b
    driver <- newDriver topo "e2e-app"

    mapM_ (\v -> pipeInput driver (topicName "in") Nothing (bytes v) (t 0) 0)
      [ "alpha", "ant", "bravo", "skip", "barn", "skip", "abacus" ]

    Just kvs <- getKeyValueStore @Text @Int64 driver (ctlStore table)
    -- Three "a"s alpha/ant/abacus, two "b"s bravo/barn, "skip" filtered
    kvsGet kvs "a" >>= (@?= Just 3)
    kvsGet kvs "b" >>= (@?= Just 2)
    kvsGet kvs "s" >>= (@?= Nothing)
    closeDriver driver

merge_then_groupby_aggregate :: TestTree
merge_then_groupby_aggregate =
  testCase "mergeStreamsN -> groupByKey -> aggregate sums values across N inputs" $ do
    b <- newStreamsBuilder
    s1 <- streamFromTopic b (topicName "in1") (consumed textSerde int64Serde)
    s2 <- streamFromTopic b (topicName "in2") (consumed textSerde int64Serde)
    s3 <- streamFromTopic b (topicName "in3") (consumed textSerde int64Serde)
    merged <- mergeStreamsN [s1, s2, s3]
    let g = grouped textSerde int64Serde
        kgs = groupByKey g merged
    table <- aggregateStream (pure (0 :: Int64))
                              (\_ v acc -> acc + v)
                              materialized
                              kgs
    topo <- buildTopology b
    driver <- newDriver topo "e2e-app"

    pipeInput driver (topicName "in1") (Just (bytes "k")) (i64Bytes 1) (t 0) 0
    pipeInput driver (topicName "in2") (Just (bytes "k")) (i64Bytes 2) (t 1) 0
    pipeInput driver (topicName "in3") (Just (bytes "k")) (i64Bytes 4) (t 2) 0
    pipeInput driver (topicName "in1") (Just (bytes "k")) (i64Bytes 8) (t 3) 0

    Just kvs <- getKeyValueStore @Text @Int64 driver (ctlStore table)
    kvsGet kvs "k" >>= (@?= Just 15)
    closeDriver driver
