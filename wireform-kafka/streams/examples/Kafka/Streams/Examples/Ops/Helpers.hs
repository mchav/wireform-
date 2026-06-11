{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.Helpers
Description : Shared bits for the operational demos

Every @Ops@ demo runs a deliberately-trivial topology so the
interesting thing on screen is the /operational/ behaviour
(partition rebalance, crash failover, rolling upgrade, standby
warmup, etc.) and not whatever happens to be in the topology
DAG. This module exposes the shared topology builders and a
couple of formatting helpers so each demo stays short.
-}
module Kafka.Streams.Examples.Ops.Helpers (
  passthroughTopo,
  bytes,
  unbytes,
  ts0,
  section,
  bullet,
  printAssignments,
) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative


{- | One-source one-sink passthrough: @in -> out@. The Riffle docs
call this "the boring topology"; it's enough to make every
subscribed instance own at least one input partition.
-}
passthroughTopo :: IO TopologyValid
passthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


ts0 :: Timestamp
ts0 = Timestamp 0


-- | @=== title ===@ banner that mirrors the existing DSL demos.
section :: String -> IO ()
section t = putStrLn ("=== " <> t <> " ===")


-- | Indented bullet line used for human-readable progress output.
bullet :: String -> IO ()
bullet s = putStrLn ("  " <> s)


{- | Pretty-print @[(instanceLabel, [(topic, partition)])]@ as a
block of bullets. Used by every multi-instance demo.
-}
printAssignments
  :: String
  -- ^ heading (e.g. @"After crash"@)
  -> [(Text, [(TopicName, Int32)])]
  -> IO ()
printAssignments heading asg = do
  bullet (heading <> ":")
  mapM_
    ( \(label, parts) ->
        bullet
          ( "    "
              <> T.unpack label
              <> " -> "
              <> show
                ( map
                    ( \(t, p) ->
                        T.unpack (unTopicName t)
                          <> ":"
                          <> show p
                    )
                    parts
                )
          )
    )
    asg
