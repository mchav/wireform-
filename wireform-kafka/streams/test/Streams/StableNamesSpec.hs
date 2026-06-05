{-# LANGUAGE OverloadedStrings #-}

module Streams.StableNamesSpec (tests) where

import Test.Syd

import qualified Kafka.Streams.Topology.StableNames as N

tests :: Spec
tests = describe "Topology stable names (KIP-307)" $ sequence_
  [ it "names use 10-digit zero-padded counters"
      pad10
  , it "two builds of the same operator sequence agree"
      same_sequence_same_names
  , it "different operator classes use independent counters"
      independent_counters
  , it "operatorPrefix uses the canonical Kafka strings"
      canonical_prefixes
  ]

pad10 :: IO ()
pad10 =
  N.generateNames [N.OpFilter, N.OpFilter]
    `shouldBe` ["KSTREAM-FILTER-0000000000", "KSTREAM-FILTER-0000000001"]

same_sequence_same_names :: IO ()
same_sequence_same_names = do
  let seq_ = [N.OpSource, N.OpFilter, N.OpMap, N.OpSink]
  N.generateNames seq_ `shouldBe` N.generateNames seq_

independent_counters :: IO ()
independent_counters =
  -- Filters share a counter; maps share a separate one.
  N.generateNames [N.OpFilter, N.OpMap, N.OpFilter, N.OpMap]
    `shouldBe`
      [ "KSTREAM-FILTER-0000000000"
      , "KSTREAM-MAP-0000000000"
      , "KSTREAM-FILTER-0000000001"
      , "KSTREAM-MAP-0000000001"
      ]

canonical_prefixes :: IO ()
canonical_prefixes = do
  N.operatorPrefix N.OpSource          `shouldBe` "KSTREAM-SOURCE-"
  N.operatorPrefix N.OpSink            `shouldBe` "KSTREAM-SINK-"
  N.operatorPrefix N.OpForeignKeyJoin  `shouldBe` "KTABLE-FK-JOIN-"
  N.operatorPrefix N.OpRepartition     `shouldBe` "KSTREAM-REPARTITION-"
