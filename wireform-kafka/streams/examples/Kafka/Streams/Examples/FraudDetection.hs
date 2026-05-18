{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.FraudDetection
-- Description : Session windows — bursty-activity fraud detection
--
-- Mirror of the canonical "session window" demo: count user
-- activity per session (gap-based clustering of events) and flag
-- sessions with above-threshold counts.
--
-- Java (paraphrased):
--
-- @
-- KStream<String, String> activity = builder.stream("user-activity");
-- activity.groupByKey()
--         .windowedBy(SessionWindows.with(Duration.ofMinutes(5)))
--         .count()
--         .toStream()
--         .filter((k, v) -> v != null && v >= 10)
--         .to("suspicious-sessions");
-- @
--
-- Haskell:
module Kafka.Streams.Examples.FraudDetection
  ( runDemo
  , buildFraudTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams

buildFraudTopology :: IO Topology
buildFraudTopology = do
  b <- newStreamsBuilder
  activity <- streamFromTopic b
                (topicName "user-activity")
                (consumed textSerde textSerde)
  let kgs = groupByKey (grouped textSerde textSerde) activity
      sw  = sessionWindows (minutes 5)
      swks = windowedBySession sw kgs
  counts <- countSessionWindowed
              (materializedAs (storeName "session-counts"))
              swks
  -- Pin a KStream<Text, Long> at the session-aggregator's emit
  -- node. This is the equivalent of @counts.toStream()@ on the
  -- JVM; we re-stamp the value serde to the int64 count type.
  let stream = KStream
        { kstreamBuilder    = swthBuilder counts
        , kstreamParent     = swthNode counts
        , kstreamKeySerde   = textSerde
        , kstreamValueSerde = int64Serde
        }
  -- Filter to "suspicious" sessions (>= 10 events in a session).
  flagged <- filterStream
              (\r -> recordValue r >= 10)
              stream
  toTopic
    (topicName "suspicious-sessions")
    (produced textSerde int64Serde)
    flagged
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== FraudDetectionDemo ==="
  topo <- buildFraudTopology
  driver <- newDriver topo "fraud-app"

  let act u tsMs =
        pipeInput driver (topicName "user-activity")
          (Just (BSC.pack (T.unpack u)))
          (BSC.pack "click")
          (Timestamp tsMs)
          0
      m :: Int64 -> Int64
      m n = n * 60 * 1000

  -- alice: 12 clicks within 5 minutes -> single session, flagged.
  mapM_ (act "alice") (map (\i -> i * 5_000) [0 .. 11])
  -- bob: 3 clicks 10 minutes apart -> three single-event sessions,
  -- none flagged.
  act "bob" (m 0)
  act "bob" (m 10)
  act "bob" (m 20)

  -- Push stream time past the gap so the session windows close.
  advanceDriverStreamTime driver (Timestamp (m 30))

  out <- readOutput driver (topicName "suspicious-sessions")
  putStrLn ("Suspicious-session updates (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
          v = case deserialize int64Serde (crValue cr) :: Either Text Int64 of
            Right n  -> show n
            Left err -> "?(" <> T.unpack err <> ")"
      in putStrLn ("  " <> k <> " : " <> v <> " events")
