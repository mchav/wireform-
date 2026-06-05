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
import qualified Streams.WindowedSuppressIntegrationSpec
import qualified Streams.IdleHeartbeatSpec
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
import qualified Streams.LagSpec
import qualified Streams.HealthSpec
import qualified Streams.TopologyStatsSpec
import qualified Streams.ObservabilityOTelSpec
import qualified Streams.ReplaySpec
import qualified Streams.BackfillSpec
import qualified Streams.Properties.KVStoreSMSpec
import qualified Streams.Properties.OptimizerEqSpec
import qualified Streams.Properties.WindowMathSpec
import qualified Streams.Properties.EOSChaosSpec
import qualified Streams.Properties.WorkerPoolSMSpec
import qualified Streams.Properties.ObservabilityTopologySpec
import qualified Streams.Properties.OrphanTopicsSpec
import qualified Streams.Properties.ChangelogReplaySpec
import qualified Streams.Properties.WatermarkSpec
import qualified Streams.Properties.WorkerPoolConcurrentSpec
import qualified Streams.Properties.AtLeastOnceRedeliverySpec
import qualified Streams.Properties.TwoPhaseSinkSpec
import qualified Streams.Properties.WatermarkCoordSpec
import qualified Streams.Properties.TTLSpec
import qualified Streams.Properties.SchemaVersionedSpec
import qualified Streams.Properties.CDCSourceSpec
import qualified Streams.Properties.KeyGroupAssignorSpec
import qualified Streams.Properties.TieredStoreSpec
import qualified Streams.Properties.RemoteKVStoreSpec
import qualified Streams.Properties.StoreRefSpec
import qualified Streams.Properties.KeyGroupDispatchSpec
import qualified Streams.Properties.WatermarkWiringSpec
import qualified Streams.Properties.SnapshotSpec
import qualified Streams.Properties.OperatorWatermarkSpec
import qualified Streams.Properties.RebalanceBridgeSpec

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
  , Streams.WindowedSuppressIntegrationSpec.tests
  , Streams.IdleHeartbeatSpec.tests
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
  , Streams.LagSpec.tests
  , Streams.HealthSpec.tests
  , Streams.TopologyStatsSpec.tests
  , Streams.ObservabilityOTelSpec.tests
  , Streams.ReplaySpec.tests
  , Streams.BackfillSpec.tests
  , testGroup "Properties"
      [ Streams.Properties.KVStoreSMSpec.tests
      , Streams.Properties.OptimizerEqSpec.tests
      , Streams.Properties.WindowMathSpec.tests
      , Streams.Properties.EOSChaosSpec.tests
      , Streams.Properties.WorkerPoolSMSpec.tests
      , Streams.Properties.ObservabilityTopologySpec.tests
      , Streams.Properties.OrphanTopicsSpec.tests
      , Streams.Properties.ChangelogReplaySpec.tests
      , Streams.Properties.WatermarkSpec.tests
      , Streams.Properties.WorkerPoolConcurrentSpec.tests
      , Streams.Properties.AtLeastOnceRedeliverySpec.tests
      , Streams.Properties.TwoPhaseSinkSpec.tests
      , Streams.Properties.WatermarkCoordSpec.tests
      , Streams.Properties.TTLSpec.tests
      , Streams.Properties.SchemaVersionedSpec.tests
      , Streams.Properties.CDCSourceSpec.tests
      , Streams.Properties.KeyGroupAssignorSpec.tests
      , Streams.Properties.TieredStoreSpec.tests
      , Streams.Properties.RemoteKVStoreSpec.tests
      , Streams.Properties.StoreRefSpec.tests
      , Streams.Properties.KeyGroupDispatchSpec.tests
      , Streams.Properties.WatermarkWiringSpec.tests
      , Streams.Properties.SnapshotSpec.tests
      , Streams.Properties.OperatorWatermarkSpec.tests
      , Streams.Properties.RebalanceBridgeSpec.tests
      ]
  ]
