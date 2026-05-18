{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.Antithesis.ObservabilityTopologySpec
-- Description : Tests for the topology DAG JSON renderer
--
-- Covers:
--
--   * Source / processor / sink / store sections are present
--     and structurally well-formed.
--   * Edges agree with 'Topo.childrenOf' for every node.
--   * 'insertionOrder' tracks topology mutation order.
--   * 'applicationId' is stamped when supplied via 'RenderConfig'.
--   * Live metrics overlay surfaces non-zero counters.
module Streams.Antithesis.ObservabilityTopologySpec (tests) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Kafka.Streams
import qualified Kafka.Streams.Metrics as Met
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Observability.Topology as Obs

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

passthroughTopology :: IO Topo.Topology
passthroughTopology = do
  b <- newStreamsBuilder
  s  <- streamFromTopic b (topicName "in")
          (consumed textSerde textSerde)
  s' <- mapValues T.toUpper s
  toTopic (topicName "out") (produced textSerde textSerde) s'
  buildTopology b

-- | Read an object's field, asserting the parent is indeed an
-- object.
field :: T.Text -> A.Value -> A.Value
field nm = \case
  A.Object km -> KM.lookup (AK.fromText nm) km
                  `or'` error ("missing field: " <> T.unpack nm)
  _ -> error ("not an object when looking up " <> T.unpack nm)
  where
    or' Nothing x  = x
    or' (Just v) _ = v

arrayLen :: A.Value -> Int
arrayLen (A.Array v) = V.length v
arrayLen _           = -1

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Observability.Topology"
  [ structural_render_has_all_sections
  , insertion_order_tracks_addition
  , application_id_is_stamped
  , edges_agree_with_childrenOf
  , live_metrics_overlay_surfaces_counters
  ]

----------------------------------------------------------------------

structural_render_has_all_sections :: TestTree
structural_render_has_all_sections =
  testCase "topologyDescription emits version + sources/processors/sinks/stores/edges/insertionOrder" $ do
    topo <- passthroughTopology
    let v = Obs.topologyDescription topo
    field "version"        v @?= A.Number 1
    arrayLen (field "sources"        v) @?= 1   -- streamFromTopic
    -- processors: one for mapValues, one for `to` (sink-aware internal)
    assertBool "expected >= 1 processors"
      (arrayLen (field "processors" v) >= 1)
    arrayLen (field "sinks"          v) @?= 1
    -- stores: zero in this passthrough
    arrayLen (field "stores"         v) @?= 0
    assertBool "expected >= 1 edges"
      (arrayLen (field "edges"       v) >= 1)
    -- insertionOrder records every node
    assertBool "expected non-empty insertionOrder"
      (arrayLen (field "insertionOrder" v) >= 2)

insertion_order_tracks_addition :: TestTree
insertion_order_tracks_addition =
  testCase "insertionOrder reflects the order nodes were added" $ do
    topo <- passthroughTopology
    let v        = Obs.topologyDescription topo
        order    = field "insertionOrder" v
    case order of
      A.Array ns -> do
        -- First node is the source (whichever name was given);
        -- last node is the sink.
        let firstName =
              case V.head ns of
                A.String s -> s
                _          -> error "non-string in insertionOrder"
            lastName =
              case V.last ns of
                A.String s -> s
                _          -> error "non-string in insertionOrder"
        assertBool ("expected first node to be a KSTREAM-SOURCE; got "
                     <> T.unpack firstName)
          (T.isPrefixOf "KSTREAM-SOURCE" firstName)
        assertBool ("expected last node to be a KSTREAM-SINK; got "
                     <> T.unpack lastName)
          (T.isPrefixOf "KSTREAM-SINK" lastName)
      _ -> error "insertionOrder not an array"

application_id_is_stamped :: TestTree
application_id_is_stamped =
  testCase "applicationId from RenderConfig appears on the root" $ do
    topo <- passthroughTopology
    let v = Obs.topologyDescriptionWith
              (Obs.defaultRenderConfig
                { Obs.renderApplicationId = Just "my-app" })
              topo
    field "applicationId" v @?= A.String "my-app"

edges_agree_with_childrenOf :: TestTree
edges_agree_with_childrenOf =
  testCase "every JSON edge matches Topo.childrenOf" $ do
    topo <- passthroughTopology
    let v = Obs.topologyDescription topo
    case field "edges" v of
      A.Array es -> mapM_ check (V.toList es)
        where
          check edge =
            case edge of
              A.Object km -> do
                let fromV = KM.lookup "from" km
                    toV   = KM.lookup "to"   km
                case (fromV, toV) of
                  (Just (A.String f), Just (A.String t)) -> do
                    let fromNm   = Topo.nodeName f
                        children = Topo.childrenOf topo fromNm
                        expected = Topo.nodeName t `elem` children
                    assertBool
                      ("edge " <> T.unpack f <> " -> " <> T.unpack t
                        <> " not present in childrenOf")
                      expected
                  _ -> error "edge missing from/to"
              _ -> error "edge not an object"
      _ -> error "edges not an array"

live_metrics_overlay_surfaces_counters :: TestTree
live_metrics_overlay_surfaces_counters =
  testCase "liveTopologyDescription includes recorded counters" $ do
    topo    <- passthroughTopology
    metrics <- Met.newMetricsRegistry
    Met.incCounter metrics "test-counter"
    Met.incCounter metrics "test-counter"
    Met.addCounter metrics "another" 7
    v <- Obs.liveTopologyDescription topo metrics Obs.defaultRenderConfig
    case field "metrics" v of
      A.Object km -> do
        KM.lookup "test-counter" km @?= Just (A.Number 2)
        KM.lookup "another"      km @?= Just (A.Number 7)
      _ -> error "metrics not an object"
