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
import qualified Streams.CogroupSpec
import qualified Streams.SuppressSpec
import qualified Streams.TimestampedSpec
import qualified Streams.VersionedSpec
import qualified Streams.TopologyDescriptionSpec
import qualified Streams.MetricsSpec
import qualified Streams.NamedSpec
import qualified Streams.WorkerPoolSpec
import qualified Streams.EOSRuntimeSpec
import qualified Streams.ExtensionsSpec
import qualified Streams.TestUtilsSpec
import qualified Streams.StateListenerSpec
import qualified Streams.QuerySpec
import qualified Streams.ParityBatchSpec
import qualified Streams.MoreParitySpec
import qualified Streams.EndToEndChainSpec
import qualified Streams.MorePartiyTwoSpec
import qualified Streams.MoreParityThreeSpec
import qualified Streams.MockClusterSpec
import qualified Streams.MockFailureModesSpec
import qualified Streams.MockAdvancedSpec
import qualified Streams.MockDriverModesSpec
import qualified Streams.ProbingRebalanceSpec
import qualified Streams.RevocationGraceSpec
import qualified Streams.StableNamesSpec
import qualified Streams.TopologyOptimizationSpec
import qualified Streams.SchemaRegistrySerdeSpec
import qualified Streams.TransactionalStoreSpec
import qualified Streams.StreamsConfigSurfaceSpec
import qualified Streams.ForeignKeyJoinDSLSpec
import qualified Streams.RuntimeDriverSpec
import qualified Streams.ExceptionHandlerSpec
import qualified Streams.KGroupedTableSpec
import qualified Streams.StoresExtraSpec
import qualified Streams.QueryAndDiscoverySpec
import qualified Streams.ProcessorAndStoreExtrasSpec
import qualified Streams.DynamicThreadsSpec
import qualified Streams.BoundedSuppressSpec
import qualified Streams.RackAwareAssignorSpec
import qualified Streams.ProbingRebalanceRuntimeSpec
import qualified Streams.StandbyTaskSpec
import qualified Streams.StandbyDriverSpec
import qualified Streams.RemoteIQSpec
import qualified Streams.DSLFacadeSpec
import qualified Streams.IdiomaticDSLSpec
import qualified Streams.MultiInstanceRebalanceSpec
import qualified Streams.RotatingFileSinkSpec
import qualified Streams.SchemaRegistryCompatSpec
import qualified Streams.SdkParitySpec
import qualified Streams.MultiInstanceHarnessSpec
import qualified Streams.SchemaRegistryHttpSpec
import qualified Streams.SchemaRegistryFormatsSpec
import qualified Streams.TopologyFreeSpec
import qualified Streams.TopologyFreeArrowSpec
import qualified Streams.AsyncIOSpec
import qualified Streams.Antithesis.KVStoreSMSpec
import qualified Streams.Antithesis.OptimizerEqSpec
import qualified Streams.Antithesis.WindowMathSpec
import qualified Streams.Antithesis.EOSChaosSpec
import qualified Streams.Antithesis.WorkerPoolSMSpec

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
  , Streams.CogroupSpec.tests
  , Streams.SuppressSpec.tests
  , Streams.TimestampedSpec.tests
  , Streams.VersionedSpec.tests
  , Streams.TopologyDescriptionSpec.tests
  , Streams.MetricsSpec.tests
  , Streams.NamedSpec.tests
  , Streams.WorkerPoolSpec.tests
  , Streams.EOSRuntimeSpec.tests
  , Streams.ExtensionsSpec.tests
  , Streams.TestUtilsSpec.tests
  , Streams.StateListenerSpec.tests
  , Streams.QuerySpec.tests
  , Streams.ParityBatchSpec.tests
  , Streams.MoreParitySpec.tests
  , Streams.EndToEndChainSpec.tests
  , Streams.MorePartiyTwoSpec.tests
  , Streams.MoreParityThreeSpec.tests
  , Streams.MockClusterSpec.tests
  , Streams.MockFailureModesSpec.tests
  , Streams.MockAdvancedSpec.tests
  , Streams.MockDriverModesSpec.tests
  , Streams.ProbingRebalanceSpec.tests
  , Streams.RevocationGraceSpec.tests
  , Streams.StableNamesSpec.tests
  , Streams.TopologyOptimizationSpec.tests
  , Streams.SchemaRegistrySerdeSpec.tests
  , Streams.TransactionalStoreSpec.tests
  , Streams.StreamsConfigSurfaceSpec.tests
  , Streams.ForeignKeyJoinDSLSpec.tests
  , Streams.RuntimeDriverSpec.tests
  , Streams.ExceptionHandlerSpec.tests
  , Streams.KGroupedTableSpec.tests
  , Streams.StoresExtraSpec.tests
  , Streams.QueryAndDiscoverySpec.tests
  , Streams.ProcessorAndStoreExtrasSpec.tests
  , Streams.DynamicThreadsSpec.tests
  , Streams.BoundedSuppressSpec.tests
  , Streams.RackAwareAssignorSpec.tests
  , Streams.ProbingRebalanceRuntimeSpec.tests
  , Streams.StandbyTaskSpec.tests
  , Streams.StandbyDriverSpec.tests
  , Streams.RemoteIQSpec.tests
  , Streams.IdiomaticDSLSpec.tests
  , Streams.DSLFacadeSpec.tests
  , Streams.MultiInstanceRebalanceSpec.tests
  , Streams.RotatingFileSinkSpec.tests
  , Streams.SchemaRegistryCompatSpec.tests
  , Streams.SdkParitySpec.tests
  , Streams.MultiInstanceHarnessSpec.tests
  , Streams.SchemaRegistryHttpSpec.tests
  , Streams.SchemaRegistryFormatsSpec.tests
  , Streams.TopologyFreeSpec.tests
  , Streams.TopologyFreeArrowSpec.tests
  , Streams.AsyncIOSpec.tests
  , testGroup "Antithesis"
      [ Streams.Antithesis.KVStoreSMSpec.tests
      , Streams.Antithesis.OptimizerEqSpec.tests
      , Streams.Antithesis.WindowMathSpec.tests
      , Streams.Antithesis.EOSChaosSpec.tests
      , Streams.Antithesis.WorkerPoolSMSpec.tests
      ]
  ]
