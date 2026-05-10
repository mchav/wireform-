-- |
-- Module      : Main
-- Description : CLI dispatcher for the wireform-kafka-streams demos
--
-- One executable, many demos. Run with no args for the index, or
-- pass a demo name to run that demo against the in-process test
-- driver (no broker required).
--
-- The set of demos mirrors the canonical Apache Kafka Streams
-- examples shipped under @org.apache.kafka.streams.examples@:
--
--   * @pipe@        — copy from one topic to another
--   * @line-split@  — flatMap a line into words
--   * @word-count@  — flatMap + groupBy + count
--   * @page-views@  — KStream-KTable inner join
--   * @temperature@ — tumbling-window max + KIP-328 suppress
--   * @top-articles@— hopping-window count by industry
--   * @orders@      — multi-stage KStream-KTable enrichment
--   * @fraud@       — session windows
--   * @fk-join@     — KIP-213 KTable-KTable foreign-key join
--   * @iq@          — KIP-67 / KIP-796 interactive queries
--   * @processor@   — Processor API + Punctuator
--   * @branching@   — KIP-418 split on predicates
--   * @global@      — KStream-GlobalKTable join
--   * @cogroup@     — KIP-150 cogroup of streams with distinct value types
module Main (main) where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import qualified Kafka.Streams.Examples.Branching         as Branching
import qualified Kafka.Streams.Examples.Cogroup           as Cogroup
import qualified Kafka.Streams.Examples.FraudDetection    as FraudDetection
import qualified Kafka.Streams.Examples.GlobalTable       as GlobalTable
import qualified Kafka.Streams.Examples.InteractiveQueries as InteractiveQueries
import qualified Kafka.Streams.Examples.InventoryFKJoin   as InventoryFKJoin
import qualified Kafka.Streams.Examples.LineSplit         as LineSplit
import qualified Kafka.Streams.Examples.OrdersEnrichment  as OrdersEnrichment
import qualified Kafka.Streams.Examples.PageViewRegion    as PageViewRegion
import qualified Kafka.Streams.Examples.Pipe              as Pipe
import qualified Kafka.Streams.Examples.ProcessorAPI      as ProcessorAPI
import qualified Kafka.Streams.Examples.SideEffects       as SideEffects
import qualified Kafka.Streams.Examples.Temperature       as Temperature
import qualified Kafka.Streams.Examples.TopArticles       as TopArticles
import qualified Kafka.Streams.Examples.WordCount         as WordCount

demos :: [(String, IO ())]
demos =
  [ ("pipe",        Pipe.runDemo)
  , ("line-split",  LineSplit.runDemo)
  , ("word-count",  WordCount.runDemo)
  , ("page-views",  PageViewRegion.runDemo)
  , ("temperature", Temperature.runDemo)
  , ("top-articles", TopArticles.runDemo)
  , ("orders",      OrdersEnrichment.runDemo)
  , ("fraud",       FraudDetection.runDemo)
  , ("fk-join",     InventoryFKJoin.runDemo)
  , ("iq",          InteractiveQueries.runDemo)
  , ("processor",   ProcessorAPI.runDemo)
  , ("side-effects",SideEffects.runDemo)
  , ("branching",   Branching.runDemo)
  , ("global",      GlobalTable.runDemo)
  , ("cogroup",     Cogroup.runDemo)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["all"] -> mapM_ (\(_, r) -> r) demos
    [name] -> case lookup name demos of
      Just runner -> runner
      Nothing -> do
        hPutStrLn stderr $
          "unknown demo: " <> name
          <> "\nknown demos: " <> intercalate ", " (map fst demos)
    _ -> do
      putStrLn "wireform-kafka-streams-examples"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  wireform-kafka-streams-examples <demo>   run a single demo"
      putStrLn "  wireform-kafka-streams-examples all      run every demo in order"
      putStrLn ""
      putStrLn "Available demos:"
      mapM_ (\(n, _) -> putStrLn ("  " <> n)) demos
