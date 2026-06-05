{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | KTable-focused DSL tests.
module Streams.DSLSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Syd

import Kafka.Streams.Imperative

tests :: Spec
tests = describe "DSL (KTable)" $ sequence_
  [ table_from_topic_basic
  , table_filter_with_tombstone
  , table_mapvalues_updates_store
  , table_tombstone_via_null_value
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

table_from_topic_basic :: Spec
table_from_topic_basic =
  it "tableFromTopic materialises latest-per-key into store" $ do
    b <- newStreamsBuilder
    kt <- tableFromTopic b (topicName "in")
            (consumed textSerde textSerde)
            (materializedAs (storeName "ktab"))
    topo <- buildTopology b
    driver <- newDriver topo "ktab-app"

    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "1") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "b")) (bytes "1") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "2") (t 1) 0
    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "3") (t 2) 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "a" >>= (`shouldBe` Just "3")
        kvsGet kvs "b" >>= (`shouldBe` Just "1")
      Nothing -> error "store missing"
    closeDriver driver

table_filter_with_tombstone :: Spec
table_filter_with_tombstone =
  it "filterTable drops non-matching values" $ do
    b <- newStreamsBuilder
    kt <- tableFromTopic b (topicName "in")
            (consumed textSerde textSerde)
            (materializedAs (storeName "src-store"))
    kt2 <- filterTable
              (\r -> T.length (recordValue r) >= 3)
              (materializedAs (storeName "filt-store"))
              kt
    topo <- buildTopology b
    driver <- newDriver topo "ktab-app"

    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "ab")    (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "b")) (bytes "abcd")  (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "c")) (bytes "abcde") (t 0) 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt2)
    case mStore of
      Just kvs -> do
        kvsGet kvs "a" >>= (`shouldBe` Nothing)
        kvsGet kvs "b" >>= (`shouldBe` Just "abcd")
        kvsGet kvs "c" >>= (`shouldBe` Just "abcde")
      Nothing -> error "filtered store missing"
    closeDriver driver

table_mapvalues_updates_store :: Spec
table_mapvalues_updates_store =
  it "mapValuesTable derives a new store" $ do
    b <- newStreamsBuilder
    kt <- tableFromTopic b (topicName "in")
            (consumed textSerde textSerde)
            (materializedAs (storeName "src"))
    kt2 <- mapValuesTable T.toUpper
              (materializedAs (storeName "upper"))
              kt
    topo <- buildTopology b
    driver <- newDriver topo "ktab-app"

    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "hello") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "b")) (bytes "world") (t 0) 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt2)
    case mStore of
      Just kvs -> do
        kvsGet kvs "a" >>= (`shouldBe` Just "HELLO")
        kvsGet kvs "b" >>= (`shouldBe` Just "WORLD")
      Nothing -> error "mapped store missing"
    closeDriver driver

table_tombstone_via_null_value :: Spec
table_tombstone_via_null_value =
  it "filterTable produces tombstones when value drops out of filter" $ do
    b <- newStreamsBuilder
    kt <- tableFromTopic b (topicName "in")
            (consumed textSerde textSerde)
            (materializedAs (storeName "src2"))
    kt2 <- filterTable
              (\r -> T.length (recordValue r) >= 3)
              (materializedAs (storeName "filt2"))
              kt
    topo <- buildTopology b
    driver <- newDriver topo "ktab-app"

    -- "abcd" passes the filter (len>=3). Then "ab" fails — should tombstone.
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "abcd") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "ab")   (t 1) 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt2)
    case mStore of
      Just kvs -> kvsGet kvs "k" >>= (`shouldBe` Nothing)
      Nothing -> error "filt2 store missing"
    closeDriver driver
