{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for KStream conversions: toTable, repartition, splitStream.
module Streams.StreamConvSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

tests :: TestTree
tests = testGroup "StreamConversions"
  [ to_table_basic
  , to_table_keeps_latest_per_key
  , repartition_passes_records_through
  , split_stream_routes_by_predicate
  , split_stream_default_branch_catches_residue
  , split_stream_no_default_drops_unmatched
  , merge_streams_n_combines_three
  , to_extracted_routes_per_record
  ]

to_table_basic :: TestTree
to_table_basic =
  testCase "toTable materialises into the named store" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    table <- toTable (materializedAs (storeName "tt-store")) src
    topo <- buildTopology b
    driver <- newDriver topo "tt-app"

    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v") (t 0) 0

    Just ro <- queryEngineStore @Text @Text (driverEngine driver)
                 (ktableStore table)
    ro.roKvGet "k" >>= (@?= Just "v")
    closeDriver driver

to_table_keeps_latest_per_key :: TestTree
to_table_keeps_latest_per_key =
  testCase "toTable retains only the latest value per key" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    table <- toTable (materializedAs (storeName "tt-store-2")) src
    topo <- buildTopology b
    driver <- newDriver topo "tt-app"

    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (t 1) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v3") (t 2) 0

    Just ro <- queryEngineStore @Text @Text (driverEngine driver)
                 (ktableStore table)
    ro.roKvGet "k" >>= (@?= Just "v3")
    closeDriver driver

repartition_passes_records_through :: TestTree
repartition_passes_records_through =
  testCase "repartition preserves record order in the test driver" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    rep <- repartition "by-key" src
    toTopic (topicName "out") (produced textSerde textSerde) rep
    topo <- buildTopology b
    driver <- newDriver topo "rp-app"

    pipeInput driver (topicName "in") Nothing (bytes "a") (t 0) 0
    pipeInput driver (topicName "in") Nothing (bytes "b") (t 0) 0
    pipeInput driver (topicName "in") Nothing (bytes "c") (t 0) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["a", "b", "c"]
    closeDriver driver

split_stream_routes_by_predicate :: TestTree
split_stream_routes_by_predicate =
  testCase "splitStream: first matching predicate wins" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    branches <- splitStream
      [ branchedFrom "a" (\r -> T.isPrefixOf "a" (recordValue r))
      , branchedFrom "b" (\r -> T.isPrefixOf "b" (recordValue r))
      ]
      Nothing
      src
    case (Map.lookup "a" branches, Map.lookup "b" branches) of
      (Just sa, Just sb) -> do
        toTopic (topicName "out-a") (produced textSerde textSerde) sa
        toTopic (topicName "out-b") (produced textSerde textSerde) sb
      _ -> error "missing branches"
    topo <- buildTopology b
    driver <- newDriver topo "sp-app"

    mapM_ (\v -> pipeInput driver (topicName "in") Nothing (bytes v) (t 0) 0)
      ["alpha", "bravo", "able", "banana"]

    outA <- readOutput driver (topicName "out-a")
    outB <- readOutput driver (topicName "out-b")
    map (unbytes . crValue) outA @?= ["alpha", "able"]
    map (unbytes . crValue) outB @?= ["bravo", "banana"]
    closeDriver driver

split_stream_default_branch_catches_residue :: TestTree
split_stream_default_branch_catches_residue =
  testCase "splitStream with a default branch catches everything else" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    branches <- splitStream
      [ branchedFrom "a" (\r -> T.isPrefixOf "a" (recordValue r))
      ]
      (Just "rest")
      src
    case (Map.lookup "a" branches, Map.lookup "rest" branches) of
      (Just sa, Just sr) -> do
        toTopic (topicName "out-a")    (produced textSerde textSerde) sa
        toTopic (topicName "out-rest") (produced textSerde textSerde) sr
      _ -> error "missing branches"
    topo <- buildTopology b
    driver <- newDriver topo "sp-app"

    mapM_ (\v -> pipeInput driver (topicName "in") Nothing (bytes v) (t 0) 0)
      ["alpha", "bravo", "able", "charlie"]

    outA <- readOutput driver (topicName "out-a")
    outR <- readOutput driver (topicName "out-rest")
    map (unbytes . crValue) outA @?= ["alpha", "able"]
    map (unbytes . crValue) outR @?= ["bravo", "charlie"]
    closeDriver driver

split_stream_no_default_drops_unmatched :: TestTree
split_stream_no_default_drops_unmatched =
  testCase "splitStream without default drops unmatched" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in")
             (consumed textSerde textSerde)
    branches <- splitStream
      [ branchedFrom "a" (\r -> T.isPrefixOf "a" (recordValue r))
      ]
      Nothing
      src
    case Map.lookup "a" branches of
      Just sa -> toTopic (topicName "out-a") (produced textSerde textSerde) sa
      Nothing -> error "missing branch a"
    topo <- buildTopology b
    driver <- newDriver topo "sp-app"

    mapM_ (\v -> pipeInput driver (topicName "in") Nothing (bytes v) (t 0) 0)
      ["alpha", "bravo", "able", "charlie"]

    out <- readOutput driver (topicName "out-a")
    map (unbytes . crValue) out @?= ["alpha", "able"]
    closeDriver driver

merge_streams_n_combines_three :: TestTree
merge_streams_n_combines_three =
  testCase "mergeStreamsN combines three streams in submission order" $ do
    b <- newStreamsBuilder
    s1 <- streamFromTopic b (topicName "in1") (consumed textSerde textSerde)
    s2 <- streamFromTopic b (topicName "in2") (consumed textSerde textSerde)
    s3 <- streamFromTopic b (topicName "in3") (consumed textSerde textSerde)
    merged <- mergeStreamsN [s1, s2, s3]
    toTopic (topicName "out") (produced textSerde textSerde) merged
    topo <- buildTopology b
    driver <- newDriver topo "mn-app"

    pipeInput driver (topicName "in1") Nothing (bytes "a") (t 0) 0
    pipeInput driver (topicName "in2") Nothing (bytes "b") (t 0) 0
    pipeInput driver (topicName "in3") Nothing (bytes "c") (t 0) 0
    pipeInput driver (topicName "in1") Nothing (bytes "d") (t 0) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["a", "b", "c", "d"]
    closeDriver driver

to_extracted_routes_per_record :: TestTree
to_extracted_routes_per_record =
  testCase "toExtracted routes each record to a topic chosen per-record" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let ext = TopicNameExtractor $ \r ->
          if T.isPrefixOf "a" (recordValue r)
            then pure (topicName "out-a")
            else pure (topicName "out-other")
    toExtracted ext (produced textSerde textSerde) s
    topo <- buildTopology b
    driver <- newDriver topo "ext-app"

    mapM_ (\v -> pipeInput driver (topicName "in") Nothing (bytes v) (t 0) 0)
      ["alpha", "bravo", "ant", "charlie"]

    outA <- readOutput driver (topicName "out-a")
    outO <- readOutput driver (topicName "out-other")
    map (unbytes . crValue) outA @?= ["alpha", "ant"]
    map (unbytes . crValue) outO @?= ["bravo", "charlie"]
    closeDriver driver
