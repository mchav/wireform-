{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Examples.InventoryFKJoin
-- Description : KIP-213 KTable-KTable foreign-key join
--
-- Demonstrates 'foreignKeyJoinKTable': product catalog @KTable@
-- joined into a per-warehouse inventory @KTable@ on a foreign
-- key extracted from the inventory value.
--
-- Java (paraphrased):
--
-- @
-- KTable<String, Inventory> inventory = builder.table("inventory");
-- KTable<String, Product>   products  = builder.table("products");
-- KTable<String, Stocked>   stocked   = inventory.join(
--   products,
--   (Inventory inv) -> inv.productId,         // FK extractor
--   (Inventory inv, Product p) -> new Stocked(inv, p)
-- );
-- stocked.toStream().to("stocked");
-- @
--
-- Haskell:
module Kafka.Streams.Examples.InventoryFKJoin
  ( runDemo
  , buildFKJoinTopology
  ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)

import Kafka.Streams

buildFKJoinTopology :: IO Topology
buildFKJoinTopology = do
  b <- newStreamsBuilder
  -- inventory: warehouseId -> "productId|qty"
  inventory <- tableFromTopic b
                  (topicName "inventory")
                  (consumed textSerde textSerde)
                  (materializedAs (storeName "inventory-store"))
  -- products: productId -> "name|category"
  products <- tableFromTopic b
                 (topicName "products")
                 (consumed textSerde textSerde)
                 (materializedAs (storeName "products-store"))
  -- KIP-213 inner FK join: extract the productId from the
  -- inventory value, look it up in the products table, emit
  -- "warehouse -> name|category|qty".
  stocked <- foreignKeyJoinKTable
                (\v -> T.takeWhile (/= '|') v)               -- FK extractor
                (\inv prod ->
                   prod <> "|" <> T.drop 1 (T.dropWhile (/= '|') inv))
                (materializedAs (storeName "stocked-store"))
                inventory
                products
  s <- toKStreamFromKTable stocked
  toTopic
    (topicName "stocked")
    (produced textSerde textSerde)
    (s { kstreamValueSerde = textSerde })
  buildTopology b

runDemo :: IO ()
runDemo = do
  putStrLn "=== InventoryFKJoinDemo ==="
  topo <- buildFKJoinTopology
  driver <- newDriver topo "fk-join-app"

  let inv w v =
        pipeInput driver (topicName "inventory")
          (Just (BSC.pack (T.unpack w)))
          (BSC.pack (T.unpack v))
          (Timestamp 0) 0
      prod p v =
        pipeInput driver (topicName "products")
          (Just (BSC.pack (T.unpack p)))
          (BSC.pack (T.unpack v))
          (Timestamp 0) 0

  prod "p-1" "Coffee Beans|grocery"
  prod "p-2" "USB Cable|electronics"
  inv "w-east" "p-1|120"
  inv "w-east" "p-2|45"
  inv "w-west" "p-1|80"

  -- Update product metadata mid-stream — every subscribing
  -- inventory row re-emits with the new product info.
  prod "p-1" "Single-Origin Coffee|grocery"

  -- Switch a warehouse from p-1 to p-3 (not yet in product table)
  -- — the inner join drops it until the product arrives.
  inv "w-east" "p-3|10"
  prod "p-3" "Stovetop Kettle|kitchenware"

  out <- readOutput driver (topicName "stocked")
  putStrLn ("Stocked records (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
      in putStrLn ("  " <> k <> " -> " <> BSC.unpack (crValue cr))
