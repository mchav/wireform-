{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.TopArticles
-- Description : Hopping-window count by industry
--
-- Mirror of @org.apache.kafka.streams.examples.wordcount.TopArticlesDemo@.
-- For each article-view event, count views per @(industry, article)@
-- in a hopping window of 1 hour advancing every 5 minutes. The output
-- is a stream of (industry, article, count) updates that downstream
-- consumers can rank.
--
-- Java (paraphrased):
--
-- @
-- KStream<String, Article> views = builder.stream("article-views");
-- views.groupBy((k, a) -> a.industry + "|" + a.article)
--      .windowedBy(TimeWindows.of(Duration.ofHours(1))
--                             .advanceBy(Duration.ofMinutes(5)))
--      .count(Materialized.as("article-counts"))
--      .toStream()
--      .to("article-counts-stream");
-- @
--
-- Haskell:
module Kafka.Streams.Examples.TopArticles
  ( runDemo
  , buildTopArticlesTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams

buildTopArticlesTopology :: IO Topology
buildTopArticlesTopology = do
  b <- newStreamsBuilder
  -- Source records are values like "tech|haskell-takeover".
  views <- streamFromTopic b
              (topicName "article-views")
              (consumed textSerde textSerde)
  -- groupByKey on the value (the "industry|article" handle).
  grouped_ <- groupByStream
                (\r -> recordValue r)
                (grouped textSerde textSerde)
                views
  -- 1-hour hopping window advancing every 5 minutes.
  let hopping = hoppingWindows (minutes 60) (minutes 5)
      tws     = windowedByTime hopping grouped_
  counts <- countWindowed
              (materializedAs (storeName "article-counts"))
              tws
  -- Tap the windowed table as a KStream<(WindowedKey k), Long>.
  windowedStream <- streamFromWindowedHandle
                      counts
                      textSerde
                      int64Serde
  -- Strip the window envelope back to the bare industry|article
  -- key so the sink topic stays Text-keyed.
  flat <- selectKey (\r -> case recordKey r of
                             Just (WindowedKey k _) -> k
                             Nothing                -> "")
                    windowedStream
  toTopic
    (topicName "article-counts-stream")
    (produced textSerde int64Serde)
    (flat { kstreamKeySerde = textSerde, kstreamValueSerde = int64Serde })
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== TopArticlesDemo ==="
  topo <- buildTopArticlesTopology
  driver <- newDriver topo "top-articles-app"

  let view value tsMs =
        pipeInput driver (topicName "article-views")
          Nothing
          (BSC.pack (T.unpack value))
          (Timestamp tsMs)
          0
      m :: Int64 -> Int64
      m n = n * 60 * 1000

  -- Within the same 1-hour window:
  view "tech|haskell-rules"     (m 0)
  view "tech|haskell-rules"     (m 10)
  view "tech|haskell-rules"     (m 30)
  view "tech|kafka-tips"        (m 5)
  view "tech|kafka-tips"        (m 25)
  view "finance|rates-explained"(m 15)
  view "finance|rates-explained"(m 50)

  -- Drain everything the windowed-as-stream emitted; keep only
  -- the highest count we've ever seen per key (i.e. the
  -- "current" hopping-window count after every input fed).
  out <- readOutput driver (topicName "article-counts-stream")
  let topByKey =
        foldr
          (\cr acc ->
             let k = case crKey cr of
                       Just b  -> BSC.unpack b
                       Nothing -> "<no-key>"
                 n = case deserialize int64Serde (crValue cr) :: Either Text Int64 of
                       Right x -> x
                       Left _  -> 0
             in case lookup k acc of
                  Just prev | prev >= n -> acc
                  _ -> (k, n) : filter ((/= k) . fst) acc)
          []
          out
  putStrLn ("Top counts per industry|article ("
           <> show (length topByKey) <> " keys, "
           <> show (length out) <> " raw updates):")
  mapM_ (\(k, n) -> putStrLn ("  " <> k <> " = " <> show n))
        (reverse topByKey)
  closeDriver driver
