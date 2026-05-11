{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.ShareGroupExtras
Description : Share-group consumer extensions: per-partition
              pause/resume + dead-letter-queue routing

Two operationally-important surfaces on top of
'Kafka.Client.ShareConsumer':

  * Per-(topic, partition) pause / resume — useful for bounded
    back-pressure.
  * Dead-letter-queue (DLQ) routing — once a record has hit
    @max.delivery.count@ the consumer can route it to a DLQ
    topic with a typed reason instead of letting the broker
    silently drop it.

Both surfaces are pure decision layers plus a small stateful
piece on top of 'Kafka.Client.ShareConsumer.ShareConsumer'.
-}
module Kafka.Client.ShareGroupExtras
  ( -- * Pause / resume
    PauseSet
  , newPauseSet
  , pausePartitions
  , resumePartitions
  , isPaused
    -- * Dead-letter queue
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
-- Pause / resume
----------------------------------------------------------------------

-- | Set of paused (topic, partition) pairs.
newtype PauseSet = PauseSet (TVar (HashSet (Text, Int32)))

newPauseSet :: IO PauseSet
newPauseSet = PauseSet <$> newTVarIO HashSet.empty

pausePartitions :: PauseSet -> [(Text, Int32)] -> IO ()
pausePartitions (PauseSet v) tps = atomically $
  modifyTVar' v (HashSet.union (HashSet.fromList tps))

resumePartitions :: PauseSet -> [(Text, Int32)] -> IO ()
resumePartitions (PauseSet v) tps = atomically $
  modifyTVar' v (\s -> HashSet.difference s (HashSet.fromList tps))

isPaused :: PauseSet -> Text -> Int32 -> IO Bool
isPaused (PauseSet v) topic part = do
  s <- readTVarIO v
  pure (HashSet.member (topic, part) s)

----------------------------------------------------------------------
-- Dead-letter queue
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
