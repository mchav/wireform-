{-# LANGUAGE OverloadedStrings #-}

module Streams.TopologyOptimizationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Streams.Topology.Optimization as TO

tests :: TestTree
tests = testGroup "TopologyOptimization toggles"
  [ testCase "OptimizeNone yields no flags"
      none
  , testCase "OptimizeAll yields every flag"
      all_
  , testCase "Single-toggle levels enable just one flag"
      singletons
  , testCase "parseOptimizationLevel round-trips"
      parse_round_trip
  , testCase "parseOptimizationLevel rejects unknown values"
      parse_unknown
  ]

none :: IO ()
none = TO.optimizationFlags TO.OptimizeNone @?= TO.noOptimizations

all_ :: IO ()
all_ = TO.optimizationFlags TO.OptimizeAll @?= TO.OptimizationFlags True True True

singletons :: IO ()
singletons = do
  TO.optimizationFlags TO.OptimizeReuseKtableSourceTopics
    @?= TO.OptimizationFlags True False False
  TO.optimizationFlags TO.OptimizeMergeRepartitionTopics
    @?= TO.OptimizationFlags False True False
  TO.optimizationFlags TO.OptimizeSingleStoreSelfJoin
    @?= TO.OptimizationFlags False False True

parse_round_trip :: IO ()
parse_round_trip = do
  let levels =
        [ TO.OptimizeNone
        , TO.OptimizeReuseKtableSourceTopics
        , TO.OptimizeMergeRepartitionTopics
        , TO.OptimizeSingleStoreSelfJoin
        , TO.OptimizeAll
        ]
  mapM_ (\l ->
    TO.parseOptimizationLevel (TO.optimizationLevelText l) @?= Just l)
    levels

parse_unknown :: IO ()
parse_unknown =
  TO.parseOptimizationLevel "garbage" @?= Nothing
