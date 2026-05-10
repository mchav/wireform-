{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.ShareGroupExtras
Description : KIP-1119 / KIP-1129 — share-group pause/resume + DLQ

Builds on 'Kafka.Client.ShareConsumer' (KIP-932) with the two
follow-up KIPs that fill in operationally-important gaps:

  * KIP-1119: pause / resume per-(topic, partition) on a share
    consumer. Useful for bounded back-pressure.
  * KIP-1129: dead-letter-queue (DLQ) support. Once a record
    has hit @max.delivery.count@, the consumer can route it to
    a DLQ topic with a typed reason instead of having the broker
    silently drop it.

Both surfaces are pure decision layers + a tiny stateful piece
on top of 'Kafka.Client.ShareConsumer.ShareConsumer'.
-}
module Kafka.Client.ShareGroupExtras
  ( -- * Pause / resume (KIP-1119)
    PauseSet
  , newPauseSet
  , pausePartitionsSG
  , resumePartitionsSG
  , isPausedSG
    -- * Dead-letter queue (KIP-1129)
  , DlqDecision (..)
  , decideDlq
  , DlqRoute (..)
  ) where

import Control.Concurrent.STM
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Kafka.Client.ShareConsumer as SC

----------------------------------------------------------------------
-- Pause / resume (KIP-1119)
----------------------------------------------------------------------

-- | Set of paused (topic, partition) pairs.
newtype PauseSet = PauseSet (TVar (HashSet (Text, Int32)))

newPauseSet :: IO PauseSet
newPauseSet = PauseSet <$> newTVarIO HashSet.empty

pausePartitionsSG :: PauseSet -> [(Text, Int32)] -> IO ()
pausePartitionsSG (PauseSet v) tps = atomically $
  modifyTVar' v (HashSet.union (HashSet.fromList tps))

resumePartitionsSG :: PauseSet -> [(Text, Int32)] -> IO ()
resumePartitionsSG (PauseSet v) tps = atomically $
  modifyTVar' v (\s -> HashSet.difference s (HashSet.fromList tps))

isPausedSG :: PauseSet -> Text -> Int32 -> IO Bool
isPausedSG (PauseSet v) topic part = do
  s <- readTVarIO v
  pure (HashSet.member (topic, part) s)

----------------------------------------------------------------------
-- DLQ (KIP-1129)
----------------------------------------------------------------------

data DlqRoute
  = DlqDrop                     -- ^ silently discard
  | DlqRouteTo !Text            -- ^ route to a specific DLQ topic
  | DlqDelegate                 -- ^ defer to a user-supplied callback
  deriving stock (Eq, Show, Generic)

data DlqDecision
  = DlqDecisionRetry            -- ^ keep retrying (delivery count below threshold)
  | DlqDecisionDeliver !DlqRoute
  deriving stock (Eq, Show, Generic)

-- | Pure DLQ decision: given the configured max-delivery-count
-- and a record's current delivery attempt count, return
-- whether the record should be retried or shoved into the DLQ.
decideDlq
  :: Int32          -- ^ max delivery count
  -> SC.ShareRecord
  -> DlqRoute
  -> DlqDecision
decideDlq maxDeliveries rec route
  | SC.srDeliveryCount rec >= maxDeliveries = DlqDecisionDeliver route
  | otherwise                                = DlqDecisionRetry
