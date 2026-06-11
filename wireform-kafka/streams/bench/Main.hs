{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Streams runtime micro-benchmark.

Measures end-to-end records/sec through the in-process
TopologyTestDriver for representative topology shapes:

  * passthrough  : source -> sink (the floor — pure dispatch
                   + record-collector cost).
  * filter+map   : source -> filter -> mapValues -> sink
                   (stateless transform pipeline).
  * count        : source -> groupByKey -> count -> sink
                   (stateful KV store update per record).
  * window-count : source -> groupByKey -> windowed-count
                   -> sink (windowed-store update per
                   record).

Results give the per-record CPU envelope of the runtime;
broker-side numbers are a separate concern (see the
top-level 'wireform-kafka' benchmark and its
'HwKafkaComparison' module).
-}
module Main (main) where

import Criterion.Main
import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative


main :: IO ()
main =
  defaultMain
    [ bgroup
        "streams-runtime"
        [ bench "passthrough/1000" $ nfIO (runPassthrough 1000)
        , bench "passthrough/10000" $ nfIO (runPassthrough 10000)
        , bench "filter-map/1000" $ nfIO (runFilterMap 1000)
        , bench "count/1000" $ nfIO (runCount 1000)
        , bench "window-count/1000" $ nfIO (runWindowCount 1000)
        ]
    ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


i64 :: Int64 -> BSC.ByteString
i64 = serialize int64Serde


ts :: Int64 -> Timestamp
ts = Timestamp


----------------------------------------------------------------------
-- Passthrough
----------------------------------------------------------------------

runPassthrough :: Int -> IO ()
runPassthrough n = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  driver <- newDriver topo "bench-passthrough"
  feed driver n
  closeDriver driver
  where
    feed driver !cnt =
      mapM_
        ( \i ->
            pipeInput
              driver
              (topicName "in")
              (Just (bytes (T.pack ("k" <> show i))))
              (bytes "value")
              (ts (fromIntegral i))
              0
        )
        [0 .. cnt - 1]


----------------------------------------------------------------------
-- Filter + map
----------------------------------------------------------------------

runFilterMap :: Int -> IO ()
runFilterMap n = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  s' <- filterStream (\r -> recordValue r /= "skip") s
  s'' <- mapValues T.toUpper s'
  toTopic (topicName "out") (produced textSerde textSerde) s''
  topo <- buildTopology b
  driver <- newDriver topo "bench-filtermap"
  mapM_
    ( \i ->
        pipeInput
          driver
          (topicName "in")
          (Just (bytes (T.pack ("k" <> show i))))
          (bytes "value")
          (ts (fromIntegral i))
          0
    )
    [0 .. n - 1]
  closeDriver driver


----------------------------------------------------------------------
-- Count
----------------------------------------------------------------------

runCount :: Int -> IO ()
runCount n = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = grouped textSerde textSerde
      ks = groupByKey g s
  _ <-
    countStream
      (materializedAs (storeName "counts"))
      ks
  topo <- buildTopology b
  driver <- newDriver topo "bench-count"
  mapM_
    ( \i ->
        pipeInput
          driver
          (topicName "in")
          (Just (bytes (T.pack ("k" <> show (i `mod` 100)))))
          (bytes "value")
          (ts (fromIntegral i))
          0
    )
    [0 .. n - 1]
  closeDriver driver


----------------------------------------------------------------------
-- Windowed count
----------------------------------------------------------------------

runWindowCount :: Int -> IO ()
runWindowCount n = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = grouped textSerde textSerde
      ks = groupByKey g s
      !ws = withGracePeriod (millis 60_000) (tumblingWindows (millis 1000))
      twks = windowedByTime ws ks
  _ <-
    countWindowed
      (materializedAs (storeName "win-counts"))
      twks
  topo <- buildTopology b
  driver <- newDriver topo "bench-windowed"
  mapM_
    ( \i ->
        pipeInput
          driver
          (topicName "in")
          (Just (bytes (T.pack ("k" <> show (i `mod` 100)))))
          (bytes "value")
          (ts (fromIntegral i * 10))
          0
    )
    [0 .. n - 1]
  closeDriver driver
