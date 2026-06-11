{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Observability.OrphanTopics
Description : Detect orphaned internal topics on the broker

Riffle Phase 1 §5: the framework owns the lifecycle of internal
changelog and repartition topics. Deployments that mutate the
topology between versions can leak these — a renamed store
creates a new changelog and abandons the old one on the broker,
where it silently consumes disk forever.

This module computes, for a given topology + applicationId:

  * the /expected/ set of internal-topic names the runtime would
    create on the broker, and
  * given a list of topics the broker actually has, which ones
    /look like/ they came from this application but aren't in
    the expected set — i.e. orphans from a previous deploy.

The detector is pure — runtime wiring (consulting an
@AdminClient@ for the actual broker topic list) lives elsewhere.
The result is intended for the runtime to log as a startup
diagnostic and for tools (CI, CLI) to consume.

== Naming convention

The detector uses the JVM Kafka Streams convention:

  * Changelog: @\<applicationId\>-\<storeName\>-changelog@
  * Repartition: @\<applicationId\>-\<repartitionNodeName\>-repartition@

Stores with @loggingEnabled = False@ or with an explicit
@loggingSourceTopic@ (KIP-295 reuse) are excluded from the
changelog set. Stores covered by the optimiser-derived
'Topo.topoChangelogPlan' are also excluded — they reuse an
external topic instead.
-}
module Kafka.Streams.Observability.OrphanTopics (
  -- * Detection
  OrphanInternalTopic (..),
  OrphanReason (..),
  detectOrphans,
  expectedInternalTopics,

  -- * Naming
  changelogTopic,
  repartitionTopic,
  isInternalTopicName,
) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.State.Store qualified as Store
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (
  TopicName,
  topicName,
  unNodeName,
  unTopicName,
 )


----------------------------------------------------------------------
-- Result
----------------------------------------------------------------------

data OrphanReason
  = -- | Topic name ends in @-changelog@.
    OrphanChangelog
  | -- | Topic name ends in @-repartition@.
    OrphanRepartition
  deriving stock (Eq, Show, Ord)


data OrphanInternalTopic = OrphanInternalTopic
  { orphanTopic :: !TopicName
  , orphanReason :: !OrphanReason
  , orphanApplicationId :: !Text
  }
  deriving stock (Eq, Show, Ord)


----------------------------------------------------------------------
-- Detection
----------------------------------------------------------------------

{- | Given the validated topology, the application id, and a list
of topics the broker reports, return the topics that
/look like/ they were created by this application but aren't
in the topology's expected set.

The broker topic list is the caller's responsibility — typically
pulled via an @AdminClient.listTopics@ call. The detector
doesn't decide /what to do/ with the orphans; that's the
runtime's policy (warn, fail, auto-delete on opt-in, ...).
-}
detectOrphans
  :: Topo.Topology
  -> Text
  -- ^ application id
  -> [TopicName]
  -- ^ topics the broker has
  -> [OrphanInternalTopic]
detectOrphans topo appId brokerTopics =
  let expected = expectedInternalTopics topo appId
      classify t
        | not (matchesPrefix appId t) = Nothing
        | otherwise =
            case suffixedReason t of
              Just r
                | not (Set.member t expected) ->
                    Just (OrphanInternalTopic t r appId)
              _ -> Nothing
  in List.sort
       [ o
       | t <- brokerTopics
       , Just o <- [classify t]
       ]


{- | Compute the full set of internal topic names the runtime would
create for a given topology + application id.
-}
expectedInternalTopics :: Topo.Topology -> Text -> Set TopicName
expectedInternalTopics topo appId =
  Set.fromList (changelogs <> repartitions)
  where
    changelogs =
      [ changelogTopic appId nm
      | (nm, builder) <- Map.toList (Topo.topoStores topo)
      , builderLoggingEnabled builder
      , not (isSourceReused topo nm builder)
      ]

    repartitions =
      [ repartitionTopic appId nm
      | nm <- foldr (:) [] (Topo.topoOrder topo)
      , isRepartitionNode nm
      ]


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{- | Conventional changelog topic name. Mirrors the JVM Kafka
Streams pattern: @\<applicationId\>-\<storeName\>-changelog@.
-}
changelogTopic :: Text -> Store.StoreName -> TopicName
changelogTopic appId sn =
  topicName (appId <> "-" <> Store.unStoreName sn <> "-changelog")


{- | Conventional repartition topic name:
@\<applicationId\>-\<nodeName\>-repartition@.
-}
repartitionTopic :: Text -> Topo.NodeName -> TopicName
repartitionTopic appId nm =
  topicName (appId <> "-" <> unNodeName nm <> "-repartition")


{- | Does this topic name follow the framework's internal-topic
naming scheme for the given application id?
-}
isInternalTopicName :: Text -> TopicName -> Bool
isInternalTopicName appId t =
  matchesPrefix appId t && case suffixedReason t of
    Just _ -> True
    Nothing -> False


matchesPrefix :: Text -> TopicName -> Bool
matchesPrefix appId t = (appId <> "-") `T.isPrefixOf` unTopicName t


suffixedReason :: TopicName -> Maybe OrphanReason
suffixedReason t
  | "-changelog" `T.isSuffixOf` unTopicName t = Just OrphanChangelog
  | "-repartition" `T.isSuffixOf` unTopicName t = Just OrphanRepartition
  | otherwise = Nothing


isRepartitionNode :: Topo.NodeName -> Bool
isRepartitionNode nm =
  "KSTREAM-REPARTITION" `T.isPrefixOf` unNodeName nm


builderLoggingEnabled :: Topo.AnyStoreBuilder -> Bool
builderLoggingEnabled = \case
  Topo.AsKeyValueBuilder b -> Store.loggingEnabled (Store.sbKvLogging b)
  Topo.AsWindowBuilder b -> Store.loggingEnabled (Store.sbWLogging b)
  Topo.AsSessionBuilder b -> Store.loggingEnabled (Store.sbSLogging b)
  Topo.AsRawBuilder b -> Store.loggingEnabled (Store.sbLogging b)


{- | Does this store's changelog reuse an external topic instead
of creating an internal one? Two ways:

  * The user explicitly set 'loggingSourceTopic' on the
    builder via 'Store.withSourceTopicChangelogKV' & friends.
  * The KIP-295 optimiser populated 'topoChangelogPlan'.
-}
isSourceReused :: Topo.Topology -> Store.StoreName -> Topo.AnyStoreBuilder -> Bool
isSourceReused topo nm builder =
  Map.member nm (Topo.topoChangelogPlan topo)
    || case builderSourceTopic builder of
      Just _ -> True
      Nothing -> False


builderSourceTopic :: Topo.AnyStoreBuilder -> Maybe TopicName
builderSourceTopic = \case
  Topo.AsKeyValueBuilder b -> Store.loggingSourceTopic (Store.sbKvLogging b)
  Topo.AsWindowBuilder b -> Store.loggingSourceTopic (Store.sbWLogging b)
  Topo.AsSessionBuilder b -> Store.loggingSourceTopic (Store.sbSLogging b)
  Topo.AsRawBuilder b -> Store.loggingSourceTopic (Store.sbLogging b)
----------------------------------------------------------------------
-- Pragmatics
--

{- ^ These functions are pure — runtime integration is the
caller's responsibility. A typical wiring inside
'Kafka.Streams.Runtime' on startup:

@
topics  <- AdminClient.listTopics admin
orphans <- pure (detectOrphans topo appId topics)
forM_ orphans $ \\o ->
  warn (\"orphan internal topic: \" <> unTopicName (orphanTopic o))
@
-}

----------------------------------------------------------------------
