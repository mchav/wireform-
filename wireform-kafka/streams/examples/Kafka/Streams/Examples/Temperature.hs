{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.Temperature
-- Description : Tumbling-window max + suppress (KIP-328)
--
-- Mirror of @org.apache.kafka.streams.examples.temperature.TemperatureDemo@.
-- A tumbling 5-second window over a temperature stream; the windowed
-- maximum is suppressed until the window closes (KIP-328) so each
-- window emits exactly one final value.
--
-- Java:
--
-- @
-- KStream<String, Double> temps = builder.stream("temperatures");
-- temps.groupByKey()
--      .windowedBy(TimeWindows.of(Duration.ofSeconds(5)).grace(Duration.ZERO))
--      .reduce(Math::max)
--      .suppress(Suppressed.untilWindowCloses(BufferConfig.unbounded()))
--      .toStream()
--      .to("hot-temperatures");
-- @
--
-- Haskell:
module Kafka.Streams.Examples.Temperature
  ( runDemo
  , buildTemperatureTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

import Kafka.Streams

buildTemperatureTopology :: IO Topology
buildTemperatureTopology = do
  b <- newStreamsBuilder
  temps <- streamFromTopic b
              (topicName "temperatures")
              (consumed textSerde doubleSerde)
  -- groupByKey + windowedByTime(5s)
  let kgs = groupByKey (grouped textSerde doubleSerde) temps
      ws  = windowedByTime (tumblingWindows (seconds 5)) kgs
  -- reduce: max per (key, window). Mirrors JVM
  -- @TimeWindowedKStream.reduce(Math::max)@ — the first record
  -- per window seeds; subsequent records combine.
  windowed <- reduceWindowed max materialized ws
  -- Suppress every per-record update; emit one final value per
  -- window after the window closes (KIP-328 semantics).
  suppressed <- suppressWindowedHandle
                  (millis 0)
                  (durationMillis (seconds 5))
                  textSerde
                  doubleSerde
                  windowed
  -- Strip the window envelope before sinking back: we only care
  -- about the sensor key for the demo.
  flat <- selectKey (\r -> case recordKey r of
                             Just (WindowedKey k _) -> k
                             Nothing                -> "")
                    suppressed
  toTopic
    (topicName "hot-temperatures")
    (produced textSerde doubleSerde)
    (flat { kstreamKeySerde = textSerde, kstreamValueSerde = doubleSerde })
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== TemperatureDemo ==="
  topo <- buildTemperatureTopology
  driver <- newDriver topo "temperature-app"

  -- Three sensors, three windows of 5 seconds.
  let temp k t v =
        pipeInput driver (topicName "temperatures")
          (Just (BSC.pack (T.unpack k)))
          (serialize doubleSerde v)
          (Timestamp t)
          0
  -- Window [0..5000): max for kitchen = 22.1
  temp "kitchen" 100   18.5
  temp "kitchen" 1500  20.0
  temp "kitchen" 4500  22.1
  -- Window [5000..10000): max for kitchen = 24.5
  temp "kitchen" 5500  23.0
  temp "kitchen" 9999  24.5
  -- Window [0..5000) for living-room: max = 19.9
  temp "living"  100   19.9
  -- Push stream-time well past the second window so suppress flushes.
  advanceDriverStreamTime driver (Timestamp 10001)

  out <- readOutput driver (topicName "hot-temperatures")
  putStrLn ("Per-window max (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
          v = case deserialize doubleSerde (crValue cr) of
            Right d  -> show d
            Left err -> "?(" <> T.unpack err <> ")"
      in putStrLn ("  " <> k <> " = " <> v)
