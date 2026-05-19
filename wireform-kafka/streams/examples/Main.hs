-- |
-- Module      : Main
-- Description : CLI dispatcher for the wireform-kafka-streams demos
--
-- One executable, many demos. Run with no args for the index, or
-- pass a demo name to run that demo against the in-process test
-- driver (no broker required).
--
-- Pass @--broker host:port@ (or set @WIREFORM_KAFKA_BROKER@) to
-- run the demo against a real Kafka broker instead. Not every
-- demo supports broker mode — those that don't fall back to the
-- in-process driver with a clear stderr warning.
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
--
-- And a second set of /operational/ demos that exercise the
-- runtime — multi-instance cluster, deployments, crashes,
-- upgrades, warm failover, EOS:
--
--   * @ops-bringup@ — multi-instance assignment over a shared broker
--   * @ops-crash@   — one instance dies; peers inherit its partitions
--   * @ops-rolling@ — rolling deploy that recycles instances under load
--   * @ops-threads@ — KIP-663 in-process @num.stream.threads@ scaling
--   * @ops-standby@ — warm a standby off the changelog and decide
--                     when it's ready (KIP-441)
--   * @ops-eos@     — EOS commit visibility under read-committed
--                     and behaviour on commit fault
--   * @ops-revoke@  — KIP-869 soft-revocation grace decisions
module Main (main) where

import Data.List (intercalate)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

import qualified Kafka.Streams.Examples.Branching         as Branching
import qualified Kafka.Streams.Examples.Cogroup           as Cogroup
import qualified Kafka.Streams.Examples.FraudDetection    as FraudDetection
import qualified Kafka.Streams.Examples.GlobalTable       as GlobalTable
import qualified Kafka.Streams.Examples.IdiomaticPipeline as IdiomaticPipeline
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

import qualified Kafka.Streams.Examples.Ops.ClusterBringup   as OpsBringup
import qualified Kafka.Streams.Examples.Ops.CrashFailover    as OpsCrash
import qualified Kafka.Streams.Examples.Ops.DynamicThreads   as OpsThreads
import qualified Kafka.Streams.Examples.Ops.EOSCommit        as OpsEOS
import qualified Kafka.Streams.Examples.Ops.RevocationGrace  as OpsRevGrace
import qualified Kafka.Streams.Examples.Ops.RollingUpgrade   as OpsRolling
import qualified Kafka.Streams.Examples.Ops.StandbyWarmup    as OpsStandby

import Kafka.Streams.Examples.Runner
  ( RunMode (..)
  , brokerOnlyWarning
  , parseRunMode
  )

-- | A demo is either @RunMode@-aware (broker-friendly) or
-- in-memory-only. Both shapes are runnable; the @InMemoryOnly@
-- ones print a clear stderr warning if the user asked for
-- broker mode.
data Demo
  = ModeAware  !(RunMode -> IO ())
  | InMemoryOnly !(IO ())

runDemoEntry :: String -> RunMode -> Demo -> IO ()
runDemoEntry _ mode (ModeAware r) = r mode
runDemoEntry name mode (InMemoryOnly body) = do
  brokerOnlyWarning name mode
  body

demos :: [(String, Demo)]
demos =
  [ -- DSL feature demos. The simple stateless / hash-shuffle ones
    -- run identically in either mode; the rest are still test-
    -- driver-only.
    ("pipe",        ModeAware    Pipe.runDemo)
  , ("line-split",  ModeAware    LineSplit.runDemo)
  , ("word-count",  ModeAware    WordCount.runDemo)
  , ("page-views",  InMemoryOnly PageViewRegion.runDemo)
  , ("temperature", InMemoryOnly Temperature.runDemo)
  , ("top-articles",InMemoryOnly TopArticles.runDemo)
  , ("orders",      InMemoryOnly OrdersEnrichment.runDemo)
  , ("fraud",       InMemoryOnly FraudDetection.runDemo)
  , ("fk-join",     InMemoryOnly InventoryFKJoin.runDemo)
  , ("iq",          InMemoryOnly InteractiveQueries.runDemo)
  , ("processor",   InMemoryOnly ProcessorAPI.runDemo)
  , ("side-effects",InMemoryOnly SideEffects.runDemo)
  , ("branching",   InMemoryOnly Branching.runDemo)
  , ("global",      InMemoryOnly GlobalTable.runDemo)
  , ("cogroup",     InMemoryOnly Cogroup.runDemo)
  , ("idiomatic",   InMemoryOnly IdiomaticPipeline.runDemo)
    -- Operational demos: multi-instance cluster, deployments,
    -- crashes, upgrades, warm failover, EOS. All in-memory --
    -- the MockCluster / WorkerPool / Standby runtimes they
    -- exercise have no broker analogue at this layer.
  , ("ops-bringup", InMemoryOnly OpsBringup.runDemo)
  , ("ops-crash",   InMemoryOnly OpsCrash.runDemo)
  , ("ops-rolling", InMemoryOnly OpsRolling.runDemo)
  , ("ops-threads", InMemoryOnly OpsThreads.runDemo)
  , ("ops-standby", InMemoryOnly OpsStandby.runDemo)
  , ("ops-eos",     InMemoryOnly OpsEOS.runDemo)
  , ("ops-revoke",  InMemoryOnly OpsRevGrace.runDemo)
  ]

main :: IO ()
main = do
  rawArgs <- getArgs
  (mode, args) <- parseRunMode rawArgs
  case args of
    ["all"] -> do
      announceMode mode
      mapM_ (\(n, d) -> runDemoEntry n mode d) demos
    [name] -> case lookup name demos of
      Just d -> do
        announceMode mode
        runDemoEntry name mode d
      Nothing -> do
        hPutStrLn stderr $
          "unknown demo: " <> name
          <> "\nknown demos: " <> intercalate ", " (map fst demos)
    _ -> do
      putStrLn "wireform-kafka-streams-examples"
      putStrLn ""
      putStrLn "Usage:"
      putStrLn "  wireform-kafka-streams-examples [--broker host:port] <demo>"
      putStrLn "  wireform-kafka-streams-examples [--broker host:port] all"
      putStrLn ""
      putStrLn "Mode:"
      putStrLn "  Default mode runs every demo against the in-process"
      putStrLn "  TopologyTestDriver (no broker required)."
      putStrLn "  --broker host:port (or env WIREFORM_KAFKA_BROKER=host:port)"
      putStrLn "  runs broker-compatible demos against a real Kafka broker."
      putStrLn ""
      putStrLn "Available demos:"
      mapM_ (\(n, d) -> do
               let tag = case d of
                     ModeAware{}    -> "  (broker + in-memory)"
                     InMemoryOnly{} -> "  (in-memory only)"
               putStrLn ("  " <> n <> tag)) demos

announceMode :: RunMode -> IO ()
announceMode = \case
  InMemory ->
    putStrLn "[wireform-kafka-streams-examples] mode: in-memory (TopologyTestDriver)"
  Broker b ->
    putStrLn $ "[wireform-kafka-streams-examples] mode: broker " <> T.unpack b
