{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Runtime.RebalanceBridge
-- Description : Bridge between Kafka.Client.ConsumerGroupV2
--               (KIP-848) and the streams reconciler
--
-- The streams runtime has its own
-- 'Kafka.Streams.Runtime.RebalanceProtocol' that models the
-- group as a KIP-848 reconciliation between
-- @GroupState.gsOwned@ and @gsTarget@. The broker-facing client
-- runs the same protocol at the wire level via
-- 'Kafka.Client.ConsumerGroupV2': its 'HeartbeatPlan' delivers
-- an 'AssignmentDelta' on each heartbeat round.
--
-- This module is the glue: it translates each
-- 'AssignmentDelta' into the corresponding 'Reconciliation' the
-- streams runtime applies to its 'GroupState', so the same
-- task-movement decisions surface in the same shape regardless
-- of whether the runtime is talking to a real broker or to the
-- in-process mock cluster.
--
-- The bridge is /pure/: callers wire it in by piping the output
-- of 'planHeartbeat' through 'applyAssignmentDelta' on every
-- heartbeat tick.
module Kafka.Streams.Runtime.RebalanceBridge
  ( -- * Conversion
    deltaToReconciliation
  , applyAssignmentDelta
    -- * Bridging tasks to (topic, partition) pairs
  , TaskAddress (..)
  , tpToTask
  ) where

import qualified Data.Set as Set
import Data.Text (Text)
import Data.Int (Int32)

import qualified Kafka.Client.ConsumerGroupV2 as CGV2
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Assignor (MemberId)
import qualified Kafka.Streams.Runtime.RebalanceProtocol as RP

----------------------------------------------------------------------
-- Task addressing
----------------------------------------------------------------------

-- | A streams-side task is identified by the @(subtopology,
-- partition)@ pair of the source topic it consumes. The bridge
-- carries the partition-side @(topic, partition)@ at the broker
-- protocol level and converts to 'TaskId' at the streams
-- runtime level.
newtype TaskAddress = TaskAddress { unTaskAddress :: (Text, Int32) }
  deriving stock (Eq, Ord, Show)

-- | The default mapping a single-source topology uses: the
-- partition number /is/ the task partition. Multi-subtopology
-- topologies override this with their own mapping.
tpToTask :: Int -> (Text, Int32) -> TaskId
tpToTask subtopology (_topic, partition) =
  TaskId
    { taskSubtopology = subtopology
    , taskPartition   = partition
    }

----------------------------------------------------------------------
-- Conversion
----------------------------------------------------------------------

-- | Build a 'Reconciliation' for the local member out of an
-- 'AssignmentDelta'. The local member observes:
--
--   * Its newly-assigned (topic, partition) pairs become
--     'rAdd' (translated to 'TaskId' via the supplied mapper).
--   * Its newly-revoked pairs become 'rRemove'.
--   * Lost partitions also flow into 'rRemove' — the runtime
--     treats lost and revoked identically at the reconciler
--     layer (the engine-side cleanup differs but the
--     ownership transition is the same).
deltaToReconciliation
  :: (Int -> (Text, Int32) -> TaskId)
    -- ^ partition mapping (see 'tpToTask')
  -> Int                                  -- ^ subtopology id
  -> CGV2.AssignmentDelta
  -> RP.Reconciliation
deltaToReconciliation toTaskId subtopology delta = RP.Reconciliation
  { RP.rAdd    = mapSet (toTaskId subtopology) (CGV2.adAssigned delta)
  , RP.rRemove = mapSet (toTaskId subtopology)
      (Set.union (CGV2.adRevoked delta) (CGV2.adLost delta))
  }
  where
    mapSet f = Set.fromList . map f . Set.toList

-- | Apply an 'AssignmentDelta' for one local member to the
-- runtime's 'GroupState'. Wraps 'deltaToReconciliation' +
-- 'applyReconciliation' for the common case the streams
-- runtime's heartbeat-handler calls every time a broker
-- response arrives.
--
-- Idempotent: re-applying a delta whose adds + removes are
-- already reflected in 'gsOwned' is a no-op.
applyAssignmentDelta
  :: (Int -> (Text, Int32) -> TaskId)
  -> Int                                  -- ^ subtopology id
  -> MemberId
  -> CGV2.AssignmentDelta
  -> RP.GroupState
  -> RP.GroupState
applyAssignmentDelta toTaskId subtopology mid delta gs =
  let !rec = deltaToReconciliation toTaskId subtopology delta
  in RP.applyReconciliation mid rec gs
