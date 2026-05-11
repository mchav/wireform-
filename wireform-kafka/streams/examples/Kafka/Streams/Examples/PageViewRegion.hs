{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.PageViewRegion
-- Description : KStream-KTable join — enrich page views with user region
--
-- Mirror of @org.apache.kafka.streams.examples.pageview.PageViewTypedDemo@.
-- The KTable holds @user -> region@; the KStream holds page views keyed
-- by user. Each view is enriched with the user's current region and
-- counted per region in a hopping window.
--
-- Java (paraphrased):
--
-- @
-- KStream<String,PageView>  views   = builder.stream("PageViews");
-- KTable <String,UserProfile> users = builder.table("UserProfiles");
-- KStream<String,String> enriched =
--     views.leftJoin(users, (pv, up) ->
--         up == null ? pv.page + ",unknown" : pv.page + "," + up.region);
-- enriched.to("EnrichedPageViews");
-- @
--
-- Haskell:
module Kafka.Streams.Examples.PageViewRegion
  ( runDemo
  , buildPageViewRegionTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams

buildPageViewRegionTopology :: IO Topology
buildPageViewRegionTopology = do
  b <- newStreamsBuilder
  -- KStream of "user -> page" page views.
  views <- streamFromTopic b
              (topicName "PageViews")
              (consumed textSerde textSerde)
  -- KTable of "user -> region".
  users <- tableFromTopic b
              (topicName "UserProfiles")
              (consumed textSerde textSerde)
              (materializedAs (storeName "user-profiles"))
  -- Inner join: each page view is enriched with the user's
  -- current region. Records arriving for users not in the
  -- KTable are dropped (use 'leftJoinKStreamKTable' to keep
  -- them with a Nothing on the table side).
  enriched <- joinKStreamKTable
                (\page region -> page <> "," <> region)
                (joined textSerde textSerde textSerde)
                views
                users
  toTopic
    (topicName "EnrichedPageViews")
    (produced textSerde textSerde)
    enriched
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== PageViewRegionDemo ==="
  topo <- buildPageViewRegionTopology
  driver <- newDriver topo "page-view-region-app"

  -- Populate the KTable first.
  let user u r =
        pipeInput driver (topicName "UserProfiles")
          (Just (BSC.pack (T.unpack u)))
          (BSC.pack (T.unpack r))
          (Timestamp 0)
          0
  user "alice" "us-east"
  user "bob"   "eu-west"
  user "carol" "ap-south"

  -- Stream a few page views.
  let view u page ts =
        pipeInput driver (topicName "PageViews")
          (Just (BSC.pack (T.unpack u)))
          (BSC.pack (T.unpack page))
          (Timestamp ts)
          0
  view "alice" "/home"     1
  view "bob"   "/products" 2
  view "carol" "/checkout" 3
  view "dave"  "/home"     4 -- not in KTable; dropped by inner join

  out <- readOutput driver (topicName "EnrichedPageViews")
  putStrLn ("Enriched page views (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let u = case crKey cr of
            Just k -> BSC.unpack k
            Nothing -> "<no-user>"
      in putStrLn ("  " <> u <> " -> " <> BSC.unpack (crValue cr))
