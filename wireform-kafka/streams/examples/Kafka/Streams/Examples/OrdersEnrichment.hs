{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.OrdersEnrichment
-- Description : Multi-stage enrichment — KStream-KTable join
--
-- Inspired by the @kafka-streams-examples@ microservices /Orders/
-- topology: an order event stream is enriched first with the
-- customer's profile (KStream-KTable join), then routed to the
-- regional fulfilment topic.
--
-- Java (paraphrased):
--
-- @
-- KStream<String, Order>     orders     = builder.stream("orders");
-- KTable<String, Customer>   customers  = builder.table("customers");
-- KStream<String, Enriched>  enriched   = orders.join(customers, ...);
-- enriched.to("enriched-orders");
-- @
--
-- Haskell:
module Kafka.Streams.Examples.OrdersEnrichment
  ( runDemo
  , buildOrdersEnrichmentTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams

buildOrdersEnrichmentTopology :: IO Topology
buildOrdersEnrichmentTopology = do
  b <- newStreamsBuilder
  -- Customer profiles ("customerId" -> "name|region").
  customers <- tableFromTopic b
                  (topicName "customers")
                  (consumed textSerde textSerde)
                  (materializedAs (storeName "customers-store"))
  -- Order events ("customerId" -> "orderId|amount").
  orders <- streamFromTopic b
              (topicName "orders")
              (consumed textSerde textSerde)
  -- Inner join: enrich each order with its customer's
  -- "name|region" record.
  enriched <- joinKStreamKTable
                (\order profile ->
                   profile <> "|" <> order)
                (joined textSerde textSerde textSerde)
                orders
                customers
  toTopic
    (topicName "enriched-orders")
    (produced textSerde textSerde)
    enriched
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== OrdersEnrichmentDemo ==="
  topo <- buildOrdersEnrichmentTopology
  driver <- newDriver topo "orders-app"

  let cust c v ts =
        pipeInput driver (topicName "customers")
          (Just (BSC.pack (T.unpack c)))
          (BSC.pack (T.unpack v))
          (Timestamp ts)
          0
      ord c v ts =
        pipeInput driver (topicName "orders")
          (Just (BSC.pack (T.unpack c)))
          (BSC.pack (T.unpack v))
          (Timestamp ts)
          0

  cust "c1" "alice|us-east" 0
  cust "c2" "bob|eu-west"   0
  ord  "c1" "o-100|199.95"  10
  ord  "c2" "o-101|49.50"   11
  -- Customer profile updates mid-stream — subsequent orders see
  -- the new region.
  cust "c1" "alice|us-west" 12
  ord  "c1" "o-102|14.99"   13

  out <- readOutput driver (topicName "enriched-orders")
  putStrLn ("Enriched orders (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b  -> BSC.unpack b
            Nothing -> "<no-key>"
      in putStrLn ("  " <> k <> " -> " <> BSC.unpack (crValue cr))
