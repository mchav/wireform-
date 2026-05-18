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
-- Haskell (free-arrow). 'F.liftIO_' bridges the
-- 'WindowedTableHandle' returned by 'F.reduceWindowed' into a
-- @KStream (WindowedKey k) v@ that 'F.suppressWindowed' can
-- consume — there's no first-class 'F.Topology' constructor for
-- that conversion because it has to thread caller-supplied serdes.
module Kafka.Streams.Examples.Temperature
  ( runDemo
  , temperatureTopology
  , buildTemperatureTopology
  ) where

import Control.Category ((>>>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)

import Kafka.Streams
import qualified Kafka.Streams.Materialized as Mat
import qualified Kafka.Streams.Suppress as Suppress
import qualified Kafka.Streams.Topology.Free as F

temperatureTopology :: F.Topology Void ()
temperatureTopology =
  F.source "temperatures" textSerde doubleSerde
    >>> F.groupByKey (grouped textSerde doubleSerde)
    >>> F.windowedByTime (tumblingWindows (seconds 5))
    >>> F.reduceWindowed max maxMat
    >>> F.liftIO_ "windowed-handle-to-stream"
          (\_b h -> Suppress.streamFromWindowedHandle
                      h textSerde doubleSerde)
    >>> F.suppressWindowed (millis 0) (durationMillis (seconds 5))
    >>> F.selectKey
          (\r -> case recordKey r of
                   Just (WindowedKey k _) -> k
                   Nothing                -> "")
    >>> F.sink "hot-temperatures" textSerde doubleSerde
  where
    maxMat :: Materialized Text Double
    maxMat =
      Mat.withValueSerde doubleSerde
        $ Mat.withKeySerde textSerde
        $ Mat.materialized

buildTemperatureTopology :: IO Topology
buildTemperatureTopology = F.buildTopologyFrom temperatureTopology

runDemo :: IO ()
runDemo = do
  putStrLn "=== TemperatureDemo ==="
  topo <- buildTemperatureTopology
  driver <- newDriver topo "temperature-app"

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
