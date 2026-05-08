module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Streams.SerdeSpec
import qualified Streams.TimeSpec
import qualified Streams.StateStoreSpec
import qualified Streams.WindowSpec
import qualified Streams.TopologySpec
import qualified Streams.DriverSpec
import qualified Streams.DSLSpec
import qualified Streams.PunctuatorSpec
import qualified Streams.PersistentStoreSpec
import qualified Streams.JoinSpec
import qualified Streams.AggregationSpec
import qualified Streams.InteractiveQueriesSpec
import qualified Streams.CacheSpec
import qualified Streams.EOSSpec
import qualified Streams.MultiTaskSpec
import qualified Streams.StandbySpec
import qualified Streams.AssignorSpec
import qualified Streams.StreamConvSpec

main :: IO ()
main = defaultMain $ testGroup "kafka-streams"
  [ Streams.SerdeSpec.tests
  , Streams.TimeSpec.tests
  , Streams.StateStoreSpec.tests
  , Streams.WindowSpec.tests
  , Streams.TopologySpec.tests
  , Streams.DriverSpec.tests
  , Streams.DSLSpec.tests
  , Streams.PunctuatorSpec.tests
  , Streams.PersistentStoreSpec.tests
  , Streams.JoinSpec.tests
  , Streams.AggregationSpec.tests
  , Streams.InteractiveQueriesSpec.tests
  , Streams.CacheSpec.tests
  , Streams.EOSSpec.tests
  , Streams.MultiTaskSpec.tests
  , Streams.StandbySpec.tests
  , Streams.AssignorSpec.tests
  , Streams.StreamConvSpec.tests
  ]
