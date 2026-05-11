{-# LANGUAGE OverloadedStrings #-}

module Streams.StableNamesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Streams.Topology.StableNames as N

tests :: TestTree
tests = testGroup "Topology stable names (KIP-307)"
  [ testCase "names use 10-digit zero-padded counters"
      pad10
  , testCase "two builds of the same operator sequence agree"
      same_sequence_same_names
  , testCase "different operator classes use independent counters"
      independent_counters
  , testCase "operatorPrefix uses the canonical Kafka strings"
      canonical_prefixes
  ]

pad10 :: IO ()
pad10 =
  N.generateNames [N.OpFilter, N.OpFilter]
    @?= ["KSTREAM-FILTER-0000000000", "KSTREAM-FILTER-0000000001"]

same_sequence_same_names :: IO ()
same_sequence_same_names = do
  let seq_ = [N.OpSource, N.OpFilter, N.OpMap, N.OpSink]
  N.generateNames seq_ @?= N.generateNames seq_

independent_counters :: IO ()
independent_counters =
  -- Filters share a counter; maps share a separate one.
  N.generateNames [N.OpFilter, N.OpMap, N.OpFilter, N.OpMap]
    @?=
      [ "KSTREAM-FILTER-0000000000"
      , "KSTREAM-MAP-0000000000"
      , "KSTREAM-FILTER-0000000001"
      , "KSTREAM-MAP-0000000001"
      ]

canonical_prefixes :: IO ()
canonical_prefixes = do
  N.operatorPrefix N.OpSource          @?= "KSTREAM-SOURCE-"
  N.operatorPrefix N.OpSink            @?= "KSTREAM-SINK-"
  N.operatorPrefix N.OpForeignKeyJoin  @?= "KTABLE-FK-JOIN-"
  N.operatorPrefix N.OpRepartition     @?= "KSTREAM-REPARTITION-"
